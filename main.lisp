(ql:quickload '(:cl-ppcre :str))

(defun parse-text (text)
  (labels ((regexp-reader (name regexp &optional (fn #'identity))
             (lambda (text)
               (multiple-value-bind (start length) (cl-ppcre:scan regexp text)
                 (when start
                   (list (list name (funcall fn (str:substring 0 length text)))
                         (str:trim-left (str:substring length nil text)))))))
           (token-reader (token)
             (lambda (text)
               (when (str:starts-with? token text)
                 (list token
                       (str:trim-left (str:substring (length token) nil text))))))
           (apply-reads (text reader-fns &optional results)
             (if (not reader-fns)
                 results
                 (destructuring-bind (&optional result text) (funcall (first reader-fns) text)
                   (when result
                     (apply-reads text (rest reader-fns) (append results (list result))))))))
    (macrolet ((destruct-reads (lambda-list text reader-fns &body body)
                   `(destructuring-bind (&optional ,@lambda-list) (apply-reads ,text (list ,@reader-fns))
                      ,(when (member '_ lambda-list) '(declare (ignorable _)))
                      (when (and ,@lambda-list)
                        ,@body))))
      (let ((read-reg-lbl (regexp-reader :register "^[a-z]+"))
            (read-jmp-lbl (regexp-reader :address "^[A-Z]+[a-z]+[a-zA-Z]+"))
            (read-imm     (regexp-reader :immediate "^[0-9]+" #'parse-integer))
            (read-reg     (regexp-reader :register "^\\$[0-9]+" (lambda (reg) (parse-integer (str:substring 1 nil reg)))))
            (read-loc     (regexp-reader :location "^#[0-9]+" (lambda (reg) (parse-integer (str:substring 1 nil reg)))))
            (read-op      (regexp-reader :op "^[A-Z]+")))
        (labels ((read-register-label (text)
                   (destruct-reads (label _ register) text (read-reg-lbl (token-reader "IS") read-reg)
                     (list :alias label register)))
                 (read-location (text)  ;; read int
                   (destruct-reads (_ location) text ((token-reader "LOC") read-loc)
                     (list :location location)))
                 (read-expr (text &optional results)
                   (destructuring-bind (&optional result text)
                       (or (funcall read-reg-lbl text)
                           (funcall read-jmp-lbl text)
                           (funcall read-imm text)
                           (funcall read-reg text))
                     (when result
                       (let ((results (append results (list result))))
                         (if (str:starts-with? "," text)
                             (read-expr (str:substring 1 nil text) results)
                             (list results text))))))
                 (read-labeled-instruction (text)
                   (destruct-reads (label op expr) text (read-jmp-lbl read-op #'read-expr)
                     (list :instruction label op expr)))
                 (read-unlabeled-instruction (text)
                   (destruct-reads (op expr) text (read-op #'read-expr)
                     (list :instruction op expr)))
                 (parse-line (line)
                   (let* ((comment-idx (search ";" line))
                          (line        (str:trim (str:substring 0 comment-idx line))))
                     (when (< 0 (length line))
                       (or (read-register-label line)
                           (read-location line)
                           (read-labeled-instruction line)
                           (read-unlabeled-instruction line))))))
          (remove nil (mapcar #'parse-line (str:split #\Newline text))))))))

(defstruct machine general-registers special-registers memory location stack)

(defun make-empty-machine ()
  (make-machine :general-registers (make-array 256)
                :special-registers '((rR . 0))
                :memory (make-array 1000)
                :location 0
                :stack nil))

(defmacro instruction (op code params &body body)
  (let* ((body (reduce (lambda (sexp param)
                         (destructuring-bind (param type) param
                           (case type
                             (:positive-relative-address sexp)
                             (:negative-relative-address sexp)
                             (:register (subst `(aref (machine-general-registers machine)
                                                      ,param)
                                               param sexp))
                             (:immediate sexp))))
                       params
                       :initial-value body))
         (body (reduce (lambda (sexp special)
                         (destructuring-bind (to-replace . replacement) special
                           (subst replacement to-replace sexp :test #'equalp)))
                       (list '(@ .      (machine-location machine))
                             '(:rR .    (assoc (machine-special-registers machine) :rR))
                             '((m8 a) . (m8 machine a)))
                       :initial-value body)))
    `(list (list ,op ,@(mapcar #'second params))
           ,code
           (lambda (machine ,@(mapcar #'first params))
             ,@body))))

(defun s64 (x)
  "Convert signed or unsigned 64-bit integer to signed 64-bit integer"
  (logior x (- (mask-field (byte 1 63) x))))

(defun u64 (x)
  "Convert signed or unsigned 64-bit integer to unsigned 64-bit integer"
  (logand x #xFFFFFFFFFFFFFFFF))

(defun int-to-byte-array (x n-bytes)
  "Convert integer to big-endian byte array"
  (let ((x (logand x (1- (expt 2 (* 8 n-bytes))))))
    (do ((n n-bytes (1- n))
         (x x       (ash x -8))
         (c nil     (cons (mod x 256) c)))
        ((= n 0) c))))

(defun byte-array-to-int (bytes)
  "Convert big-endian byte array to unsigned integer"
  (reduce (lambda (acc x)
            (+ (ash acc 8) x))
          bytes))

(defun machine-get-memory (machine address size)
  ;; M1[x] -> returns x'th byte in machine-memory
  ;; M2[x] -> returns M1[y]<<8+M1[y+1] where y=2*floor(x/2)
  ;; M4[x] -> returns M1[y]<<24 + M1[y+1]<<16 + M1[y+2]<<8 + M1[y+3] where y=4*floor(x/4)
  (let ((address (- address (mod address size))))
    (byte-array-to-int (subseq (machine-memory machine) address (+ address size)))))

(defun machine-set-memory (machine address size value)
  (let ((address (- address (mod address size))))
    (setf (subseq (machine-memory machine) address (+ address size))
          (int-to-byte-array value size))))

(defun m8 (machine address)
  (machine-get-memory machine address 8))

(define-setf-expander m8 (machine address)
  (let ((store (gensym)))
    (values nil          ;; Temporary values
            nil          ;; Value forms
            (list store) ;; Store variables
            `(progn (machine-set-memory ,machine ,address 8 ,store)
                    ,store)
            `(m8 ,machine ,address))))

(defvar *instructions*)
(setq *instructions*
      (list
       ;; Subtract: s($X) ← s($Y) - s($Z)
       (instruction :SUB #x24 ((x :register) (y :register) (z :register))
         (setf x (u64 (- (s64 y) (s64 z)))
               ;; TODO: set rR
               @ (+ @ 4)))
       ;; Subtract: s($X) ← s($Y) - $Z
       (instruction :SUB #x25 ((x :register) (y :register) (z :immediate))
         (setf x (u64 (- (s64 y) (s64 z)))
               ;; TODO: set rR
               @ (+ @ 4)))
       ;; Compare: s($X) ← [s($Y) > s($Z)] - [s($Y) < s($Z)]
       (instruction :CMP #x30 ((x :register) (y :register) (z :register))
         ;; TODO: allow let to override y,z
         (setf x (u64 (cond
                        ((< (s64 z) (s64 y))  1)
                        ((< (s64 y) (s64 z)) -1)
                        (t        0)))
               @ (+ @ 4)))
       ;; Shift left: s($X) ← s($Y) * 2^u($Z)
       (instruction :SL #x38 ((x :register) (y :register) (z :register))
         (setf x (u64 (ash (s64 y) (u64 z)))
               @ (+ @ 4)))
       ;; Shift left: s($X) ← s($Y) * 2^u(Z)
       (instruction :SL #x39 ((x :register) (y :register) (z :immediate))
         (setf x (u64 (ash (s64 y) (u64 z)))
               @ (+ @ 4)))
       ;; Shift right: s($X) ← int(s($Y) / 2^u($Z))
       (instruction :SR #x3C ((x :register) (y :register) (z :register))
         (setf x (u64 (ash (s64 y) (- (u64 z))))
               @ (+ @ 4)))
       ;; Shift right: s($X) ← int(s($Y) / 2^u(Z))
       (instruction :SR #x3D ((x :register) (y :register) (z :immediate))
         (setf x (u64 (ash (s64 y) (- (u64 z))))
               @ (+ @ 4)))
       ;; Probable branch if nonpositive (forwards): if s($X) ≤ 0, set @ ← RA
       (instruction :PBNP #x5C ((x :register) (yz :positive-relative-address))
         (setf @ (if (<= (s64 x) 0)
                     (+ @ (* 4 yz))
                     (+ @ 4))))
       ;; Probable branch if nonpositive (backwards): if s($X) ≤ 0, set @ ← RA
       (instruction :PBNP #x5D ((x :register) (yz :negative-relative-address))
         (setf @ (if (<= (s64 x) 0)
                     (- @ (* 4 yz))
                     (+ @ 4))))
       ;; Probable branch if positive (forwards): if 0 < s($X), set @ ← RA
       (instruction :PBP #x54 ((x :register) (yz :positive-relative-address))
         (setf @ (if (< 0 (s64 x))
                     (+ @ (* 4 yz))
                     (+ @ 4))))
       ;; Probable branch if positive (backwards): if 0 < s($X), set @ ← RA
       (instruction :PBP #x55 ((x :register) (yz :negative-relative-address))
         (setf @ (if (< 0 (s64 x))
                     (- @ (* 4 yz))
                     (+ @ 4))))
       ;; Load octa s($X) ← s(M8[A])
       (instruction :LDO #x8C ((x :register) (y :register) (z :register))
         (let ((a (+ y z)))
           (setf x (s64 (m8 a))
                 @ (+ @ 4))))  
       ;; Bitwise or: v($X) ← v($Y) ∨ v($Z)
       (instruction :OR  #xC0 ((x :register) (y :register) (z :register))
         (setf x (logior y z)
               @ (+ @ 4)))
       ;; Bitwise or: v($X) ← v($Y) ∨ v(Z)
       (instruction :OR  #xC1 ((x :register) (y :register) (z :immediate))
         (setf x (logior y z)
               @ (+ @ 4)))
       ;; Jump (forwards): @ ← RA
       (instruction :JMP #xF0 ((xyz :positive-relative-address))
         (setf @ (+ @ (* 4 xyz))))
       ;; Jump (backwards): @ ← RA
       (instruction :JMP #xF1 ((xyz :negative-relative-address))
         (setf @ (+ @ (* -4 xyz))))
       (instruction :POP #xF8 ((x :immediate) (y :immediate))
         (declare (ignore x y))
         (setf @ (+ @ 4))
         ;; TODO: pop instruction
         )
       ))

(defun parse-mmix-file (filename)
  (let ((text (uiop:read-file-string (format nil "data/~a.mmix" filename))))
    (parse-text text)))

(defvar find-the-maximum-program)
(setq find-the-maximum-program
      #((:SL   (:register 2)   (:register 0) (:immediate 3))
        (:LDO  (:register 1)   (:register 4) (:register 2))
        (:JMP  (:positive-relative-address 6))
        (:LDO  (:register 3)   (:register 4) (:register 2))
        (:CMP  (:register 255) (:register 3) (:register 1))
        (:PBNP (:register 255) (:positive-relative-address 3))
        (:OR   (:register 1)   (:register 3) (:immediate 0))
        (:SR   (:register 0)   (:register 2) (:immediate 3))
        (:SUB  (:register 2)   (:register 2) (:immediate 8))
        (:PBP  (:register 2)   (:negative-relative-address 6))
        (:POP  (:immediate 2)  (:immediate 0))))

(defun execute-current-instruction (machine instructions)
  (let ((instruction (aref instructions (/ (machine-location machine) 4))))
    (format t "Executing instruction ~a~%" instruction)
    (let ((op           (first instruction))
          (param-types  (mapcar #'first (rest instruction)))
          (param-values (mapcar #'second (rest instruction))))
      (destructuring-bind (signature opcode fn) (assoc `(,op ,@param-types) *instructions* :test #'equal)
        (declare (ignore signature opcode))
        (apply fn machine param-values)))))

(defun execute-instructions (machine instructions)
  (loop while (< -1 (machine-location machine) (* 4 (length instructions)))
        do (execute-current-instruction machine instructions)
        finally (return machine)))

(defun prepare-machine (machine &rest configs)
  (reduce (lambda (machine config)
            (destructuring-bind (type dst val) config
              (case type
                (:register (setf (aref (machine-general-registers machine) dst) val))
                (:memory   (machine-set-memory machine dst 8 val))
                (:location (setf (machine-location machine) val))))
            machine)
          configs
          :initial-value machine))

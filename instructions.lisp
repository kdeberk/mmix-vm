(in-package #:instructions)

(defun s64 (x)
  "Convert signed or unsigned 64-bit integer to signed 64-bit integer"
  (logior x (- (mask-field (byte 1 63) x))))

(defun u64 (x)
  "Convert signed or unsigned 64-bit integer to unsigned 64-bit integer"
  (logand x #xFFFFFFFFFFFFFFFF))

(defun m8 (machine address)
  (machine:get-memory machine address 8))

(define-setf-expander m8 (machine address)
  (let ((store (gensym)))
    (values nil          ;; Temporary values
            nil          ;; Value forms
            (list store) ;; Store variables
            `(progn (machine:set-memory ,machine ,address 8 ,store)
                    ,store)
            `(m8 ,machine ,address))))

(defmacro definstruction (op code params &body body)
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
    `(setf (gethash '(,op ,@(mapcar #'second params)) *by-signature*)
           ,code
           (aref *by-op* ,code)
           (lambda (machine ,@(mapcar #'first params))
             ,@body))))

(defvar *by-signature*)
(defvar *by-op*)

(setq *by-signature* (make-hash-table :test #'equal)
      *by-op* (make-array 256))

(definstruction :SUB #x24 ((x :register) (y :register) (z :register))
  ;; Subtract: s($X) ← s($Y) - s($Z)
  (setf x (u64 (- (s64 y) (s64 z)))
        ;; TODO: set rR
        @ (+ @ 4)))

(parachute:define-test sub-24
  (let ((fn (aref *instructions-by-op* #x24))
        (regx 0) (regy 1) (valy 10) (regz 2) (valz 8))
    (let ((machine (machine:prepare (machine:new) `(:register ,regy ,valy) `(:register ,regz ,valz))))
      (is = (- valy valz)
          (progn (funcall fn machine regx regy regz)
                 (aref (machine-general-registers machine) regx)))
      (is = (mod (- valz valy) (expt 2 64))
          (progn (funcall fn machine regx regz regy)
                 (aref (machine-general-registers machine) regx))))))

(definstruction :SUB #x25 ((x :register) (y :register) (z :immediate))
  ;; Subtract: s($X) ← s($Y) - $Z
  (setf x (u64 (- (s64 y) (s64 z)))
        ;; TODO: set rR
        @ (+ @ 4)))

(parachute:define-test sub-25
  (let ((fn (aref *instructions-by-op* #x25))
        (regx 0) (regy 1) (valy 13) (smallval 8) (bigval 100))
    (let ((machine (machine:prepare (machine:new) `(:register ,regy ,valy))))
      (is = (- valy smallval)
          (progn (funcall fn machine regx regy smallval)
                 (aref (machine-general-registers machine) regx)))
      (is = (mod (- valy bigval) (expt 2 64))
          (progn (funcall fn machine regx regy bigval)
                 (aref (machine-general-registers machine) regx))))))

(definstruction :CMP #x30 ((x :register) (y :register) (z :register))
  ;; Compare: s($X) ← [s($Y) > s($Z)] - [s($Y) < s($Z)]
  ;; TODO: allow let to override y,z
  (setf x (u64 (cond
                 ((< (s64 z) (s64 y))  1)
                 ((< (s64 y) (s64 z)) -1)
                 (t        0)))
        @ (+ @ 4)))

(definstruction :SL #x38 ((x :register) (y :register) (z :register))
  ;; Shift left: s($X) ← s($Y) * 2^u($Z)
  (setf x (u64 (ash (s64 y) (u64 z)))
        @ (+ @ 4)))

(definstruction :SL #x39 ((x :register) (y :register) (z :immediate))
  ;; Shift left: s($X) ← s($Y) * 2^u(Z)
  (setf x (u64 (ash (s64 y) (u64 z)))
        @ (+ @ 4)))

(definstruction :SR #x3C ((x :register) (y :register) (z :register))
  ;; Shift right: s($X) ← int(s($Y) / 2^u($Z))
  (setf x (u64 (ash (s64 y) (- (u64 z))))
        @ (+ @ 4)))

(definstruction :SR #x3D ((x :register) (y :register) (z :immediate))
  ;; Shift right: s($X) ← int(s($Y) / 2^u(Z))
  (setf x (u64 (ash (s64 y) (- (u64 z))))
        @ (+ @ 4)))

(definstruction :PBNP #x5C ((x :register) (yz :positive-relative-address))
  ;; Probable branch if nonpositive (forwards): if s($X) ≤ 0, set @ ← RA
  (setf @ (if (<= (s64 x) 0)
              (+ @ (* 4 yz))
              (+ @ 4))))

(definstruction :PBNP #x5D ((x :register) (yz :negative-relative-address))
  ;; Probable branch if nonpositive (backwards): if s($X) ≤ 0, set @ ← RA
  (setf @ (if (<= (s64 x) 0)
              (- @ (* 4 yz))
              (+ @ 4))))

(definstruction :PBP #x54 ((x :register) (yz :positive-relative-address))
  ;; Probable branch if positive (forwards): if 0 < s($X), set @ ← RA
  (setf @ (if (< 0 (s64 x))
              (+ @ (* 4 yz))
              (+ @ 4))))

(definstruction :PBP #x55 ((x :register) (yz :negative-relative-address))
  ;; Probable branch if positive (backwards): if 0 < s($X), set @ ← RA
  (setf @ (if (< 0 (s64 x))
              (- @ (* 4 yz))
              (+ @ 4))))

(definstruction :LDO #x8C ((x :register) (y :register) (z :register))
  ;; Load octa s($X) ← s(M8[A])
  (let ((a (+ y z)))
    (setf x (s64 (m8 a))
          @ (+ @ 4))))

(definstruction :OR  #xC0 ((x :register) (y :register) (z :register))
  ;; Bitwise or: v($X) ← v($Y) ∨ v($Z)
  (setf x (logior y z)
        @ (+ @ 4)))

(definstruction :OR  #xC1 ((x :register) (y :register) (z :immediate))
  ;; Bitwise or: v($X) ← v($Y) ∨ v(Z)
  (setf x (logior y z)
        @ (+ @ 4)))

(definstruction :JMP #xF0 ((xyz :positive-relative-address))
  ;; Jump (forwards): @ ← RA
  (setf @ (+ @ (* 4 xyz))))

(definstruction :JMP #xF1 ((xyz :negative-relative-address))
  ;; Jump (backwards): @ ← RA
  (setf @ (+ @ (* -4 xyz))))

;; (definstruction :POP #xF8 ((x :immediate) (y :immediate))
;;   (declare (ignore x y))
;;   (setf @ (+ @ 4))
;;   ;; TODO: pop instruction
;;   )

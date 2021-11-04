(in-package #:parser)

(defun lexer (str)
  "lexer breaks up the string into lexemes.  The returned function can be repeatedly called until the result is nil"
  (let ((lexemes '(("^(IS|is)" . is)
                   ("^(LOC|loc)" . loc)
                   ("^," . comma)
                   ("^[A-Z]+(?=\\s)" . op)
                   ("^[a-zA-Z][a-zA-Z0-9]*" . id)
                   ("^\\$[0-9]+" . lit-reg)
                   ("^#[0-9]+" . lit-addr)
                   ("^[0-9]+" . lit-val))))
    #'(lambda ()
        (multiple-value-bind (start length) (cl-ppcre:scan "^(\\s+|;;.*)+" str)
          ;; skip any whitespace, comments at the start of the string.
          (when start
            (setf str (str:substring length t str))))
        (if (= 0 (length str))
            (values nil nil)
            (loop for (regex . type) in lexemes
                  for (start length) = (multiple-value-list (cl-ppcre:scan regex str))
                  if start
                    return (let ((substring (str:substring 0 length str)))
                             (setf str (str:substring length t str))
                             (values type substring)))))))

(yacc:define-parser *parser*
  (:start-symbol lines)
  (:terminals (op id lit-reg lit-addr lit-val comma is loc))
  (op-arg (id       (lambda (id)   `(:identifier ,id)))
          (lit-reg  (lambda (reg)  `(:register ,(parse-integer (str:substring 1 t reg)))))
          (lit-addr (lambda (addr) `(:address ,(parse-integer (str:substring 1 t addr)))))
          (lit-val  (lambda (val)  `(:immediate ,(parse-integer val)))))
  (op-args (op-arg comma op-args (lambda (x _ xs)
                                   (declare (ignore _))
                                   (append (list x) xs)))
           (op-arg #'list))
  (line (id is lit-reg (lambda (id _ reg)
                         (declare (ignore _))
                         `(:alias ,id ,(parse-integer (str:substring 1 t reg)))))
        (loc lit-addr (lambda (_ loc)
                        (declare (ignore _))
                        `(:location ,(parse-integer (str:substring 1 t loc)))))
        (id op op-args (lambda (lbl op op-args)
                         `(:op ,(intern op :keyword) :label ,lbl :args ,op-args)))
        (op op-args (lambda (op op-args)
                      `(:op ,(intern op :keyword) :args ,op-args))))
  (lines (line lines (lambda (x xs) (append (list x) xs)))
         (line #'list)))

(defun parse (str)
  "parse takes some MMIX code and converts it to an annotated list of instructions+metadata that the MMIX machine can handle."
  (yacc:parse-with-lexer (lexer str) *parser*))

(parachute:define-test parse
  (is equal (parse "j IS $0")
      '((:alias "j" 0)))
  (is equal (parse "j IS $0 LOC #100")
      '((:alias "j" 0)
        (:location 100)))
  (is equal  (parse "kk is $2 Decrk SUB kk,kk,8 PBP kk,Loop")
      '((:alias "kk" 2)
        (:op :SUB :label "Decrk" :args ((:identifier "kk") (:identifier "kk") (:immediate 8)))
        (:op :PBP :args ((:identifier "kk") (:identifier "Loop"))))))

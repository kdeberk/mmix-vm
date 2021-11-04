(asdf:defsystem "mmix-vm"
  :description "A small MMIX emulator"
  :components ((:file "parser")
               (:file "machine")
               (:file "instructions" :depends-on ("machine"))
               (:file "compiler" :depends-on ("instructions")))
  :depends-on (:parachute
               :cl-ppcre
               :str
               :yacc))

(defpackage #:main
  (:use :cl))

(defpackage #:parser
  (:use :cl :yacc :parachute)
  (:export #:parse))

(defpackage #:machine
  (:use :cl)
  (:export :new :prepare
           :set-memory :get-memory))

(defpackage #:instructions
  (:use :cl :parachute)
  (:import-from :machine :machine-general-registers
                :machine-special-registers :machine-location)
  (:export :*by-signature* :*by-op*))

(defpackage #:compiler
  (:use :cl)
  (:export :compile-lines))

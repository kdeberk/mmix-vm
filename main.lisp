(in-package #:main)

(defun parse-mmix-file (filename)
  (let ((text (uiop:read-file-string (format nil "data/~a.mmix" filename))))
    (parser:parse text)))

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
)

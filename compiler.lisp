(in-package #:compiler)

(defun compile-lines (lines)
  (let ((labels (gather-labels lines))
        (op-lines (annotate-locations lines)))
    (mapcar (lambda (op-line)
              (compile-op-line op-line labels))
            op-lines)))

(defun gather-labels (lines)
  "process-labels iterates over the lines and gathers the labels"
  (let ((result
          (reduce (lambda (acc line)
                    ;; Store op and register labels in assoc lists labels and aliases.
                    (destructuring-bind (loc labels) acc
                      (case (first line)
                        (:location (list (cadr line) labels))
                        (:op       (let ((label (cadr (member :label line))))
                                     (list (+ 1 loc)
                                           (if label
                                               (acons label (list :address loc) labels)
                                               labels))))
                        (:alias    (list loc (acons (second line) (list :register (third line)) labels)))
                        (otherwise acc))))
                  lines
                  :initial-value (list 0 nil))))
    (destructuring-bind (loc labels) result
      (declare (ignore loc))
      labels)))

(defun annotate-locations (lines)
  "annotate-location iterates over the lines and adds a :loc field to every :op line"
  (let ((result
          (reduce (lambda (acc line)
                    (destructuring-bind (loc lines) acc
                      (case (first line)
                        (:location (list (cadr line) lines))
                        (:op       (list (+ 1 loc)
                                         (cons (append line `(:loc ,loc))
                                               lines)))
                        (otherwise acc))))
                  lines
                  :initial-value (list 0 nil))))
    (destructuring-bind (loc lines) result
      (declare (ignore loc))
      (reverse lines))))

(defun compile-op-line (op-line labels)
  ;; TODO: always produce 4 bytes
  (let ((op (cadr (member :op op-line)))
        (args (cadr (member :args op-line)))
        (loc (cadr (member :loc op-line))))
    (let ((args (mapcar (lambda (arg)
                          (case (first arg)
                            (:identifier (let ((found (assoc (second arg) labels :test #'equal)))
                                           (if (not found)
                                               (progn (format t "Could not find ~a~%" (second arg))
                                                      nil)
                                               ;; TODO: raise error
                                               (destructuring-bind (name type val) found
                                                 (declare (ignore name))
                                                 (cond ((not (eq :address type)) (list type val))
                                                       ((> loc val) (list :positive-relative-address (- loc val)))
                                                       (t (list :negative-relative-address (- val loc))))))))
                            (otherwise arg)))
                        args)))
      (let ((signature (append (list op) (mapcar #'first args))))
        (let ((opcode (gethash signature instructions:*by-signature*)))
          (append (list opcode) (mapcar #'second args)))))))

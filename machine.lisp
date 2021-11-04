(in-package #:machine)

(defstruct machine general-registers special-registers memory location stack)

(defun new ()
  (make-machine :general-registers (make-array 256)
                :special-registers '((rR . 0))
                :memory (make-array 1000)
                :location 0
                :stack nil))

(defmethod prepare ((machine machine) &rest loads)
  (reduce (lambda (machine load)
            (destructuring-bind (type dst val) load
              (case type
                (:register (setf (aref (machine-general-registers machine) dst) val))
                (:memory   (machine-set-memory machine dst 8 val))
                (:location (setf (machine-location machine) val))))
            machine)
          loads
          :initial-value machine))

;; TODO: perhaps use defmethod for the following functions?

(defun machine-run (machine)
  )

(defun get-memory (machine address size)
  ;; M1[x] -> returns x'th byte in machine-memory
  ;; M2[x] -> returns M1[y]<<8+M1[y+1] where y=2*floor(x/2)
  ;; M4[x] -> returns M1[y]<<24 + M1[y+1]<<16 + M1[y+2]<<8 + M1[y+3] where y=4*floor(x/4)
  (let ((address (- address (mod address size))))
    (byte-array-to-int (subseq (machine-memory machine) address (+ address size)))))

(defun set-memory (machine address size value)
  (let ((address (- address (mod address size))))
    (setf (subseq (machine-memory machine) address (+ address size))
          (int-to-byte-array value size))))

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

(defun run-next-instruction (machine)
  ;; check location
  ;; get 4 bytes
  ;; find the entry for that byte and convert the parameters from the given
  )

(in-package :cl-user)

(defpackage prbs.util
  (:use :cl)
  (:export :num->bv
           :bytes->bits
           :bv->num
           :bitbv))

(in-package :prbs.util)

(defun num->bv (val &optional (size 8))
  "Convert an integer into a bitvector. Specify `SIZE` if you want something other than 8 bits per value."
  (coerce (loop for i from (1- size) downto 0
             collect (ldb (byte 1 i) val)) 'bit-vector))

(defun bytes->bits (b)
  "Convert a list of integer values into a single bytevector."
  (when b
    (concatenate 'bit-vector (num->bv (car b)) (bytes->bits (cdr b)))))

(defun bv->num (b &optional (m 1) (a 0))
  "Convert a bitvector into an integer."
  (if (equal #* b)
      a
      (let ((e (- (length b) 1)))
        (bv->num (subseq b 0 e) (* 2 m) (+ a (* m (elt b e)))))))

(defun bitbv (n)
  "Turn a 1 or 0 into a single-bit bitvector."
  (if (eq 0 n)
      #*0
      #*1))


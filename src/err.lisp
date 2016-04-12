(in-package :cl-user)

(defpackage prbs.err
  (:use :cl :prbs :bit-wise)
  (:export :sfind :sfind-all :lock))

(in-package :prbs.err)

(declaim (optimize (debug 0) (safety 0) (speed 3)))

(defun sfind (p n)
  "=> bit offset where `P` occurs in PRBS-`N`, or nil if not found"
  (let ((gen (bit-gen n)))
    (labels ((rec (p rem n o need)
               (if (zerop n)
                   nil
                   (let ((v (concatenate 'bit-vector rem (funcall gen need))))
                     (if (equal p v)
                         o
                         (rec p (subseq v 1) (1- n) (1+ o) 1))))))
      (rec p #* (seq-length n) 0 (length p)))))

(defun sfind-all (p n)
  "=> list of all bit offsets where `P` occurs in PRBS-`N`"
  (let ((gen (bit-gen n)))
    (labels ((rec (p rem n o c need)
               (if (zerop n)
                   c
                   (let ((v (concatenate 'bit-vector rem (funcall gen need))))
                     (if (equal p v)
                         (rec p (subseq v 1) (1- n) (1+ o) (cons o c) 1)
                         (rec p (subseq v 1) (1- n) (1+ o) c 1))))))
      (rec p #* (seq-length n) 0 nil (length p)))))

(defun lock (p n &optional (rem n))
  "lock on a prbs pattern for p. return a function that tracks bit errors for subsequent bits"
  (unless (or
           (zerop rem)
           (< (length p) n))
    (let ((gen (bit-gen n :seed (bv->num (subseq p 0 n)))))
      (if (mismatch (funcall gen (length p)) p)
          (lock (subseq p 1) n (1- rem))
          (make-tracker gen (length p))))))

(defun make-tracker (gen total-bits &optional (error-bits 0))
  (lambda (bits)
    (setq total-bits (+ total-bits (length bits)))
    (setq error-bits (+ error-bits (reduce #'+ (bit-xor bits (funcall gen (length bits))))))
    (values error-bits total-bits)))


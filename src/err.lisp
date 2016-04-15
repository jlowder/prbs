(in-package :cl-user)

(defpackage prbs.err
  (:use :cl
        :prbs
        :prbs.util)
  (:export :sfind
           :sfind-all
           :lock))

(in-package :prbs.err)

(defun sfind (p n)
  "=> bit offset where `P` occurs in PRBS-`N`, or nil if not found

Brute-force search a PRBS-`N` sequence for an arbitrary bit pattern."
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
  "=> list of all bit offsets where `P` occurs in PRBS-`N`

Brute-force search a PRBS-`N` sequence for all matches of an arbitrary bit pattern."
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
  "=> lambda (x)

Attempt to lock on a PRBS-`N` sequence using the bit pattern `P`. If
successful, return a lambda that tracks bit errors for subsequent data
in the sequence, or nil if unable to lock. The lambda can be
repeatedly called with a bitvector containing subsequent data, and it
will return the total number of bit errors detected and the total
number of bits it has seen."
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


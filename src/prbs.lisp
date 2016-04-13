(in-package :cl-user)

(defpackage prbs
  (:use :cl :rmatch :bit-wise)
  (:import-from :prbs.taps
                :taps)
  (:export :make-prbs
           :num-gen
           :bvlist-gen
           :bit-gen
           :byte-gen
           :take
           :seq-length))

(in-package :prbs)

(declaim (optimize (debug 0) (safety 0) (speed 3)))

(defun prbs-n (bv taps)
  "=> bit-vector

left-shift `BV` by one bit and apply `TAPS` to generate a new right-side bit."
  (concatenate 'bit-vector
               (subseq bv 1)
               (bitbv (match taps
                        ((t1 t2) (logxor (elt bv t1) (elt bv t2)))
                        ((t1 t2 t3 t4) (logxor (elt bv t1) (elt bv t2)
                                               (elt bv t3) (elt bv t4)))))))

(defun make-prbs (n &key (seed 2))
  "=> lambda ()

Create a closure representing PRBS-`N`. Each call to the lambda will
return the next bitvector of length `N` in the prbs-`N` sequence.
`SEED` can be provided as the integer value of the initial vector."
  (let ((taps (taps n))
        (v (num->bv seed n)))
    (lambda ()
      (let ((r v))
        (setq v (prbs-n v taps))
        r))))

(defun bvlist-gen (n &key (seed 2))
  "=> lambda (x)

Create a closure representing PRBS-`N`. The lambda takes a single
argument which is the number of the next `N`-bit bitvectors to
generate from the sequence (default 1 value), which will be returned
in a list. `SEED` can be provided as the integer value of the initial
vector."
  (let ((v (make-prbs n :seed seed)))
    (lambda (&optional (c 1))
      (loop repeat c collecting (funcall v)))))

(defun bit-gen (n &key (seed 2) (start 0))
  "=> lambda (x)

Create a closure representing PRBS-`N`. The lambda takes a single
argument which is the number of the next bits to generate from the
sequence (default 1 bit). The bits will be returned as a bitvector.
`SEED` can be provided as the integer value of the initial vector. If
`START` is provided, the generator will be initialized to that bit
offset."
  (let ((gen (bvlist-gen n :seed seed))
        (res #*))
    (labels ((rec (&optional (c 1))
               (let* ((len (length res))
                      (num (if (>= len c)
                               0
                               (ceiling (/ (- c len) n))))
                      (v (apply #'concatenate 'bit-vector res (funcall gen num))))
                 (if (> (length v) c)
                     (setq res (subseq v c))
                     (setq res #*))
                 (if (> (length v) c)
                     (subseq v 0 c)
                     v))))
      (let ((n (floor (/ start n)))
            (m (mod start n)))
        (loop repeat n do (funcall gen))
        (rec m))
      #'rec)))

(defun num-gen (n &key (seed 2))
  "=> lambda (x)

Create a closure representing PRBS-`N`. The lambda takes a single
argument which is the number of the next integer values to generate
from the sequence (default 1 value), which will be returned in a
list. `SEED` can be provided as the integer value of the initial
vector."
  (let ((gen (bit-gen n :seed seed)))
    (lambda (&optional (c 1))
      (loop repeat c
           collect (bv->num (funcall gen n))))))

(defun byte-gen (n &key (seed 2))
  "=> lambda (x)

Create a closure representing PRBS-`N`. The lambda takes a single
argument which is the number of the next 8-bit unsigned integer values
to generate from the sequence (default 1 byte), which will be returned
in a list. `SEED` can be provided as the integer value of the initial
vector."
  (let ((gen (bit-gen n :seed seed)))
    (lambda (&optional (c 1))
      (loop repeat c
           collect (bv->num (funcall gen 8))))))

(defun take (n gen)
  "=> `N` values from `GEN`

`GEN` can be a lambda returned from any of the *-gen functions."
  (funcall gen n))
                              
(defun seq-length (n)
  "=> number of bits in PRBS-`N`"
  (* n (1- (expt 2 n))))

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
           :seq-length
           :sfind
           :sfind-all))

(in-package :prbs)

(declaim (optimize (debug 0) (safety 0) (speed 3)))

(defun prbs-n (bv taps)
  "=> bit-vector

left-shift `BV` by one bit and apply `TAPS` to generate a new right-side bit."
  (flet ((newbit (bv)
           (apply #'logxor (match taps
                             ((t1 t2) (list (elt bv t1) (elt bv t2)))
                             ((t1 t2 t3 t4) (list (elt bv t1) (elt bv t2)
                                                  (elt bv t3) (elt bv t4)))))))
    (concatenate 'bit-vector
                 (subseq bv 1)
                 (bitbv (newbit bv)))))
  
(defun make-prbs (n &key (seed 2))
  "=> lambda ()

Create a lambda representing PRBS-`N`. Each call to the lambda will
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

Create a lambda representing PRBS-`N`. The lambda takes a single
argument which is the number of the next `N`-bit bitvectors to
generate from the sequence (default 1 value), which will be returned
in a list. `SEED` can be provided as the integer value of the initial
vector."
  (let ((v (make-prbs n :seed seed)))
    (lambda (&optional (c 1))
      (loop repeat c collecting (funcall v)))))

(defun bit-gen (n &key (seed 2) (start 0))
  "=> lambda (x)

Create a lambda representing PRBS-`N`. The lambda takes a single
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

(defun num-gen (n &key (seed 2) (start 0))
  "=> lambda (x)

Create a lambda representing PRBS-`N`. The lambda takes a single
argument which is the number of the next integer values to generate
from the sequence (default 1 value), which will be returned in a
list. `SEED` can be provided as the integer value of the initial
vector. If `START` is provided, the generator will be initialized to
that bit offset."
  (let ((gen (bit-gen n :seed seed :start start)))
    (lambda (&optional (c 1))
      (loop repeat c
           collect (bv->num (funcall gen n))))))

(defun byte-gen (n &key (seed 2) (start 0))
  "=> lambda (x)

Create a lambda representing PRBS-`N`. The lambda takes a single
argument which is the number of the next 8-bit unsigned integer values
to generate from the sequence (default 1 byte), which will be returned
in a list. `SEED` can be provided as the integer value of the initial
vector. If `START` is provided, the generator will be initialized to
that bit offset."
  (let ((gen (bit-gen n :seed seed :start start)))
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
  "=> list of all bit-offsets where `P` occurs in PRBS-`N`"
  (let ((gen (bit-gen n)))
    (labels ((rec (p rem n o c need)
               (if (zerop n)
                   c
                   (let ((v (concatenate 'bit-vector rem (funcall gen need))))
                     (if (equal p v)
                         (rec p (subseq v 1) (1- n) (1+ o) (cons o c) 1)
                         (rec p (subseq v 1) (1- n) (1+ o) c 1))))))
      (rec p #* (seq-length n) 0 nil (length p)))))

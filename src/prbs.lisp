(in-package :cl-user)

(defpackage prbs
  (:use :cl :rmatch :bit-wise)
  (:import-from :prbs.delay
                :delay
                :dcons
                :dcar
                :dcdr
                :dnull)
  (:import-from :prbs.taps
                :taps)
  (:export :make-prbs
           :num-gen
           :bvlist-gen
           :bit-gen
           :byte-gen
           :dseq
           :take
           :dtake
           ;; re-export from delay
           :dcons
           :dcar
           :dcdr
           :dnull))

(in-package :prbs)

(declaim (optimize (debug 0) (safety 0) (speed 3)))

(defun prbs-n (bv taps)
  "=> bit-vector

left-shift `BV` by one bit and apply `TAPS` to generate a new right-side bit"
  (flet ((newbit (bv)
           (apply #'logxor (match taps
                             ((t1 t2) (list (elt bv t1) (elt bv t2)))
                             ((t1 t2 t3 t4) (list (elt bv t1) (elt bv t2)
                                                  (elt bv t3) (elt bv t4)))))))
    (concatenate 'bit-vector
                 (subseq bv 1)
                 (bitbv (newbit bv)))))
  
(defun make-prbs (n &optional (iv #*10))
  "=>  lazy list of repeating `N`-bit bitvectors

`IV` can be provided as an initial bit-vector."
  (let ((taps (taps n)))
    (labels ((rec (v)
               (dcons v
                      (let ((next (prbs-n v taps)))
                        (rec next)))))
      (rec (num->bv (bv->num iv) n)))))

(defun bvlist-gen (n &optional (init #*10))
  "=> lambda (x)

Create a prbs-`N` function. The function takes a single argument which is the number of the next `N`-bit bit vectors to generate from the sequence (default 1 value).

`IV` can be provided as an initial bit-vector."
  (let ((v (make-prbs n init)))
    (lambda (&optional (c 1))
      (loop repeat c collecting
           (let ((b (dcar v)))
             (setq v (dcdr v))
             b)))))
      
(defun bit-gen (n &optional (init #*10) (skip 0))
  "return a function representing prbs-N. The returned function takes a single argument which is the number of the next bits to generate from the sequence (default 1 bit)."
  (let ((gen (bvlist-gen n init))
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
      (let ((n (floor (/ skip n)))
            (m (mod skip n)))
        (loop repeat n do (funcall gen))
        (rec m))
      #'rec)))

(defun num-gen (n &optional (init #*10))
  "return a function representing prbs-N. The returned function takes a single argument which is the number of the next n-bit integer values to generate from the sequence (default 1 value)."
  (let ((v (make-prbs n init)))
    (labels ((rec (&optional (c 1))
                  (when (> c 0)
                    (let ((b (dcar v)))
                      (when b
                        (setq v (dcdr v))
                        (cons (bv->num b) (rec (1- c))))))))
      #'rec)))

(defun byte-gen (n &optional (init #*10))
  "return a function representing prbs-N. The returned function takes a single argument which is the number of the next unsigned integer values to generate from the sequence (default 1 value)."
  (let ((gen (bit-gen n init)))
    (lambda (&optional (c 1))
      (loop repeat c
           collect (bv->num (funcall gen 8))))))

(defun take (n gen)
  "Use on any of the returned 'gen' functions to get 'n' values from it."
  (funcall gen n))
                              
(defun dseq (n &optional (init #*10))
  "return a lazy list of ones and zeroes representing a PRBS sequence."
  (let ((gen (bit-gen n init)))
    (labels ((rec ()
               (dcons (bv->num (funcall gen)) (rec))))
      (rec))))

(defun dtake (n l)
  "Use to take 'n' values from the lazy list returned from dseq"
  (unless (or (zerop n)
              (dnull l))
    (cons (dcar l) (dtake (1- n) (dcdr l)))))

(defun beginsp (p d)
  (if (null p)
      t
      (if (equal (car p) (dcar d))
          (beginsp (cdr p) (dcdr d))
          nil)))
                 
(defun dfind (p d n &optional (o 0))
  (if (zerop n)
      nil
      (if (beginsp p d)
          o
          (dfind p (dcdr d) (1- n) (1+ o)))))

(defun seq-length (n)
  (* n (1- (expt 2 n))))

(defun dfind-all (p d n &optional (o 0) (c nil))
  (if (zerop n)
      c
      (dfind-all p (dcdr d) (1- n) (1+ o) 
                 (if (beginsp p d)
                     (cons o c)
                     c))))

(defun sfind (p n)
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
  (let ((gen (bit-gen n)))
    (labels ((rec (p rem n o c need)
               (if (zerop n)
                   c
                   (let ((v (concatenate 'bit-vector rem (funcall gen need))))
                     (if (equal p v)
                         (rec p (subseq v 1) (1- n) (1+ o) (cons o c) 1)
                         (rec p (subseq v 1) (1- n) (1+ o) c 1))))))
      (rec p #* (seq-length n) 0 nil (length p)))))

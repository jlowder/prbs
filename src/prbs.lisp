(in-package :cl-user)

(defpackage prbs
  (:use :cl :rmatch :bit-wise)
  (:import-from :prbs.delay
                :delay
                :dcons
                :dcar
                :dcdr
                :dnull)
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

(defparameter *lfsr2*
  (list
   6 '(6 5)
   3 '(3 2)
   2 '(2 1)
   4 '(4 3)
   5 '(5 3)
   7 '(7 6)
   23 '(23 18)))

(defparameter *lfsr4*
  (list
   8 '(8 6 5 4)))

(defun taps (n)
  (mapcar #'(lambda (x) (- n x))
          (or
           (getf *lfsr2* n)
           (getf *lfsr4* n))))

(defun prbs-n (bv taps)
  "shift a bit vector by one bit and use the register taps to generate a new bit"
  (flet ((newbit (bv)
           (apply #'logxor (match taps
                             ((t1 t2) (list (elt bv t1) (elt bv t2)))
                             ((t1 t2 t3 t4) (list (elt bv t1) (elt bv t2)
                                                  (elt bv t3) (elt bv t4)))))))
    (concatenate 'bit-vector
                 (subseq bv 1)
                 (bitbv (newbit bv)))))

(defun make-prbs (n &optional (iv #*10))
  "return a lazy list of repeating n-bit bitvectors"
  (let ((taps (taps n)))
    (labels ((rec (v)
               (dcons v
                      (let ((next (prbs-n v taps)))
                        (rec next)))))
      (rec (num->bv (bv->num iv) n)))))

(defun bvlist-gen (n &optional (init #*10))
  "return a function representing prbs-N. The returned function takes a single argument which is the number of the next n-bit bit vectors to generate from the sequence (default 1 value)."
  (let ((v (make-prbs n init)))
    (lambda (&optional (c 1))
      (loop for i from 1 to c collecting
           (let ((b (dcar v)))
             (setq v (dcdr v))
             b)))))
      
(defun bit-gen (n &optional (init #*10))
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

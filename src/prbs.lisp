(in-package :cl-user)

(defpackage prbs
  (:use :cl :rmatch)
  (:import-from :prbs.delay
                :dcons
                :dcar
                :dcdr
                :take)
  (:export :make-prbs
           :num-gen
           :bvlist-gen))

(in-package :prbs)

(defun bitbv (n)
  (if (eq 0 n)
      #*0
      #*1))

(defun num->bits (val &optional (size 8))
  (coerce (loop for i from (1- size) downto 0
             collect (ldb (byte 1 i) val)) 'bit-vector))

(defun prbs-n (bv taps)
  (flet ((newbit (bv)
           (match taps
             ((t1 t2) (logxor (elt bv t1) (elt bv t2)))
             ((t1 t2 t3 t4) (logxor (elt bv t1) (elt bv t2)
                                    (elt bv t3) (elt bv t4))))))
    (concatenate 'bit-vector
                 (subseq bv 1)
                 (bitbv (newbit bv)))))

(defun make-prbs (n &optional (iv 2))
  (let ((taps '(0 1))
        (init (num->bits iv n)))
    (labels ((rec (v)
               (dcons v
                      (let ((next (prbs-n v taps)))
                        (if (equal init next)
                            nil
                            (rec next))))))
      (rec init))))

(defun bvlist-gen (n)
  (let ((v (make-prbs n)))
    (lambda (&optional (c 1))
      (loop for i from 1 to c collecting
           (let ((b (dcar v)))
             (setq v (dcdr v))
             b)))))
      
(defun num-gen (n)
  (let ((v (make-prbs n)))
    (labels ((rec (&optional (c 1))
                  (when (> c 0)
                    (let ((b (dcar v)))
                      (when b
                        (setq v (dcdr v))
                        (cons (bv2int b) (rec (1- c))))))))
      #'rec)))
                              
(defun bv2int (b &optional (m 1) (a 0))
  (when b
    (if (equal #* b)
        a
        (let ((e (- (length b) 1)))
          (bv2int (subseq b 0 e) (* 2 m) (+ a (* m (elt b e))))))))


(in-package :cl-user)

(defpackage prbs.delay
  (:use :cl)
  (:export :delay
           :force
           :take
           :take-all
           :dcons
           :dlist
           :dcar
           :dcdr))

(in-package :prbs.delay)

(defmacro delay (expr)
  (let ((gg (gensym)))
    `(let ((,gg #'(lambda () ,expr)))
       (setf (get 'forcable ,gg) t)
       ,gg)))

(defmacro force (x)
  (let ((gg (gensym)))
  `(if (get 'forcable ,x)
       (let ((,gg (get 'forced ,x)))
         (when (null ,gg)
           (setf (get 'forced ,x) t)
           (setf (get 'forced-value ,x) (funcall ,x)))
         (get 'forced-value ,x))
       ,x)))

(defmacro dcons (a b)
  `(delay (cons ,a ,b)))

(defun dcdr (a)
  (cdr (force a)))

(defun dcar (a)
  (car (force a)))
  
(defun dlist (l)
  (delay (when l
           (cons (car l) (dlist (cdr l))))))

(defun dnil ()
  (delay nil))

(defun dnull (x)
  (not (force x)))

(defun take (n l)
  (unless (or (zerop n)
              (dnull l))
    (cons (dcar l) (take (1- n) (dcdr l)))))

(defun take-all (l)
  (unless (dnull l)
    (cons (dcar l) (take-all (dcdr l)))))



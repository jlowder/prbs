(in-package :cl-user)

(defpackage prbs.delay
  (:use :cl)
  (:export :delay
           :dtake
           :dcons
           :dcar
           :dnull
           :dcdr))

(in-package :prbs.delay)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These are mostly based on the lazy functions from "Land Of Lisp" 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro delay (expr)
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nil))
       (lambda ()
         (unless ,forced
           (setq ,value ,expr)
           (setq ,forced t))
         ,value))))

(defun force (x)
  (funcall x))

(defmacro dcons (a b)
  `(delay (cons ,a ,b)))

(defun dcdr (a)
  (cdr (force a)))

(defun dcar (a)
  (car (force a)))
  
(defun dnull (x)
  (not (force x)))



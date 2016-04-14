(defsystem prbs
    :name "prbs"
    :version "0.1.0"
    :author "Jason Lowdermilk <jlowdermilk@gmail.com>"
    :description "A library of higher-order functions that generate Pseudo-Random Binary Sequences of (practically) any degree"
    :licence "MIT"
    :serial t
    :depends-on (:rmatch :bit-wise)
    :components
    ((:module "src"
              :components
              ((:file "taps")
               (:file "prbs")
               (:file "err")))))

(defsystem :prbs-docs
  :depends-on (:cl-gendoc)
  :pathname "doc"
  :serial t
  :components
  ((:file "prbs-docs")))

(defmethod perform :after ((o load-op) (c (eql (find-system :prbs-docs))))
  (let ((fn (find-symbol (symbol-name 'generate) (find-package :prbs-docs))))
    (funcall fn)))

(defmethod operation-done-p ((o load-op) (c (eql (find-system :prbs-docs))))
  nil)

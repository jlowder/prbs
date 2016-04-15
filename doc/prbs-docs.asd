(defsystem :prbs-docs
  :version "0.1.0"
  :author "Jason Lowdermilk <jlowdermilk@gmail.com>"
  :description "Document generation for prbs"
  :depends-on (:cl-gendoc :prbs)
  :license "MIT"
  :serial t
  :components
  ((:file "prbs-docs")))

(defmethod perform :after ((o load-op) (c (eql (find-system :prbs-docs))))
  (let ((fn (find-symbol (symbol-name 'generate) (find-package :prbs-docs))))
    (funcall fn)))

(defmethod operation-done-p ((o load-op) (c (eql (find-system :prbs-docs))))
  nil)

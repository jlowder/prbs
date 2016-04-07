(defsystem prbs
    :name "prbs"
    :version "0.1.0"
    :author "Jason Lowdermilk <jlowdermilk@gmail.com>"
    :licence "MIT"
    :serial t
    :depends-on (:rmatch)
    :components
    ((:module "src"
              :components
              ((:file "delay")
               (:file "prbs")))))

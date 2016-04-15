(defsystem prbs
    :name "prbs"
    :version "0.1.0"
    :author "Jason Lowdermilk <jlowdermilk@gmail.com>"
    :description "A library of higher-order functions for generating Pseudo-Random Binary Sequences of (practically) any degree"
    :license "MIT"
    :serial t
    :components
    ((:module "src"
              :components
              ((:file "taps")
               (:file "util")
               (:file "prbs")
               (:file "err")))))

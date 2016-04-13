(defpackage :prbs-docs
  (:use #:cl #:gendoc :prbs :prbs.err)
  (:export #:generate))

(in-package :prbs-docs)

(defun generate ()
  (gendoc (:output-filename "ref.html"
           :css "simple.css")
    (:apiref :prbs :prbs.err)))

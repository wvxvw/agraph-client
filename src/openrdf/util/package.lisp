(in-package :cl)
(defpackage :openrdf.utils (:use :cl :iterate)
  (:export :encode-ntriple-string
           :local-name-index
           :ensure-uri-string
           :local-name
           :namespace))

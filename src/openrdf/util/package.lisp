(in-package :cl)
(defpackage :openrdf.utils (:use :cl :iterate :cl-ppcre)
  (:export :encode-ntriple-string
           :local-name-index
           :ensure-uri-string
           :local-name
           :namespace
           :parse-uriref
           :parse-nodeid
           :parse-literal
           :ensure-array))

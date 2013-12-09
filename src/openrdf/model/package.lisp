(in-package :cl)
(defpackage :openrdf.model (:use :cl :local-time)
            (:export :value
                     :resource
                     :uri
                     :bnode
                     :namespace
                     :ntriples
                     :value=
                     :get-uri
                     :get-value
                     :get-local-name
                     :get-namespace))

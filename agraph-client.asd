(in-package :cl)
(defpackage agraph-client-asd (:use :cl :asdf))
(in-package :agraph-client-asd)

(defsystem agraph-client
  :version "0.1"
  :author "Oleg Sivokon <olegsivokon@gmail.com>,
           derived from Python source written by Franz Inc.
           <http://www.franz.com/agraph/allegrograph/>"
  :license "EPL"
  :depends-on (:alexandria :cl-ppcre :iterate :local-time :cl-containers)
  :components ((:module
                "src" :serial t
                :components
                ((:module
                  "openrdf" :serial t
                  :components
                  ((:module
                    "util" :serial t
                    :components
                    ((:file "package")
                     (:file "utils" :depends-on ("package"))))
                   (:module
                    "query" :serial t
                    :components
                    ((:file "package")
                     (:file "dataset" :depends-on ("package"))))
                   (:module
                    "model" :serial t :depends-on ("util")
                    :components
                    ((:file "package")
                     (:file "value" :depends-on ("package"))
                     (:file "literal" :depends-on ("value"))
                     (:file "statement" :depends-on ("value"))
                     (:file "valuefactory" :depends-on ("statement"))))
                   (:module
                    "vocabulary" :serial t :depends-on ("model")
                    :components
                    ((:file "package")
                     (:file "xmlschema" :depends-on ("package")))))))))
  :description "Port of Franz' client for AllegroGraph database"
  :long-description
  #.(with-open-file
        (stream (merge-pathnames
                 #p"README.org" (or *load-pathname* *compile-file-pathname*))
                :if-does-not-exist nil :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream)) seq)))
  :in-order-to ((test-op (load-op :agraph-client-test)))
  :perform (test-op :after (op c)
                    (funcall (intern (string '#:run!) :agraph-client.test)
                             :agraph-client.test)))

(defsystem :agraph-client-test
  :author "Oleg Sivokon <olegsivokon@gmail.com>"
  :description "Minimal test suite for testing agraph-client"
  :license "EPL"
  :depends-on (:agraph-client :fiveam)
  :components ((:module "tests"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "suite" :depends-on ("package"))
                         (:file "test-client" :depends-on ("suite"))))))

(in-package :cl)
(defpackage agraph-client-asd (:use :cl :asdf))
(in-package :agraph-client-asd)

(defsystem agraph-client
  :version "0.1"
  :author "Oleg Sivokon <olegsivokon@gmail.com>"
  :license "MIT"
  :depends-on (:alexandria :split-sequence :iterate)
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "solver" :depends-on ("package")))))
  :description "A toy project for solving Sudoku 9x9 puzzles"
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
  :license "MIT"
  :depends-on (:agraph-client :fiveam)
  :components ((:module "tests"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "suite" :depends-on ("package"))
                         (:file "test-client" :depends-on ("suite"))))))

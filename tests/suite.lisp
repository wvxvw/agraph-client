(in-package :agraph-client.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (get-test :agraph-client)
    (def-suite :agraph-client)))

(def-suite :agraph-client.test :in :agraph-client)

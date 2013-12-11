;; -*- coding: utf-8 -*-

(in-package :openrdf.model)

;; this is that rare case, which calls for funcallable object
;; but let us not decide hastily, maybe we don't need this at all.
(defclass value-factory ()
  ((blank-node-amount :initform 10 :initarg :blank-node-amount
          :accessor value-factory-blank-node-amount)
   (store :initform nil :initarg :store
          :accessor value-factory-store)
   (unused-bnode-ids :initform nil :initarg :unused-bnode-ids
          :accessor value-factory-unused-bnode-ids)))

(defmethod value-factory-unused-bnode-id ((this value-factory))
  (unless (value-factory-unused-bnode-ids this)
    (setf (value-factory-unused-bnode-ids this)
          (get-blank-nodes (mini-repository (value-factory-store this))
                           (value-factory-blank-node-amount this))))
  (subseq (pop (value-factory-unused-bnode-ids this)) 2))

(defmethod create-bnode ((this value-factory) &optional node-id)
  (make-instance 'bnode :id (or node-id (value-factory-unused-bnode-id this))))

(defmethod create-literal ((this value-factory) value &key datatype language)
  (if (and (consp value) (cdr value))
      (create-range this (first value) (second value))
      (make-instance 'literal :label value :datatype datatype :language language)))

(defmethod create-statement
    ((this value-factory) subject predicate object &optional context)
  (declare (ignore this))
  (make-instance 'statement
                 :subject subject :predicate predicate
                 :object object :context context))

;; This is something really odd... not sure what happens, if there's no
;; local-name, but there is a namespace
(defmethod create-uri ((this value-factory) &key uri namespace local-name)
  (declare (ignore this))
  (when (and namespace local-name)
    (setf uri (concatenate 'string namespace local-name)))
  (make-instance 'uri :value uri))

;; This all looks too odd, why do I need predicate here?
(defmethod validate-range-constant ((this value-factory) term predicate)
  (declare (ignore predicate))
  (let ((datatype (get-datatype term)))
    (unless datatype (error "~a must have datatype" (get-value term)))))

;; Same as above + there was a to-do for `geo-term'
(defmethod validate-compound-literal ((this value-factory) term predicate)
  (typecase term
    (range-literal
     (validate-range-constant this (lower-bound term) predicate)
     (validate-range-constant this (upper-bound term) predicate))
    (geo-term )))

(defmethod position->term ((this value-factory) term &optional predicate)
  (typecase term
    (compound-literal (validate-compound-literal this term predicate))
    (value (setf term (clreate-literal this term))))
  term)

(defmethod create-range ((this value-factory) lower-bound upper-bound)
  ;; `range-literal' needs some defaults for `:label' slot
  (make-instance 'range-literal
                 :upper-bound (position->term this upper-bound)
                 :lower-bound (position->term this lower-bound)))

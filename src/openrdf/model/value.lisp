;; -*- coding: utf-8 -*-

(in-package :openrdf.model)

(defclass value () ())

(defclass resource (value) ())

;; All this string parsing nonsense looks bad, why not have
;; a uri struct to encode all parts?
(defclass uri (resource)
  ((value :initform nil :initarg :value
          :accessor uri-value :type (or null string)))
  (:default-initargs :uri nil))

(defclass bnode (resource)
  ((id :initform nil :initarg :id :accessor bnode-id)))

(defclass namespace ()
  ((prefix :initarg :prefix :accessor namespace-prefix)
   (name :initarg :name :accessor namespace-name)))

(defgeneric ntriples (rdf-term)
  (:documentation "Converts RDF-TERM to ntriples. You MUST implement it.")
  (:method ((this value)) (error "Not implemented"))
  (:method ((this uri)) (openrdf.utils:encode-ntriple-string (uri-value this)))
  (:method ((this bnode)) (format nil "_:~a" (bnode-id this))))

(defgeneric value= (term-a term-b)
  (:documentation "Compares two RDF term. You MUST implement it.")
  (:method ((this value) (that value)) (error "Not implemented"))
  (:method ((this uri) (that uri)) (string= (uri-value this) (uri-value that)))
  (:method ((this bnode) (that bnode)) (string= (bnode-id this) (bnode-id that))))

(defmethod print-object ((this value) stream)
  (print (ntriples this) stream))

(defmethod print-object ((this namespace) stream)
  (format stream "~a :: ~a" (namespace-prefix this) (namespace-name this)))

(defgeneric get-uri (uri)
  (:documentation "One more accessor for value slot?")
  (:method ((this uri)) (uri-value this)))

(defgeneric get-value (uri)
  (:documentation "One more accessor for value slot?")
  (:method ((this uri)) (uri-value this)))

(defgeneric get-local-name (uri)
  (:documentation "")
  (:method ((this uri)) (openrdf.utils:local-name (uri-value this))))

(defgeneric get-namespace (uri)
  (:documentation "")
  (:method ((this uri)) (openrdf.utils:namespace (uri-value this))))

(defmethod initialize-instance :after
    ((this uri) &rest initargs
     &key lambda-list argument-precence-order &allow-other-keys)
  (declare (ignore lambda-list argument-precence-order))
  (destructuring-bind (&key value &allow-other-keys) initargs
    (when (and value
               (char= (char value 0) #\<)
               (char= (char value (1- (length value))) #\>))
      (setf (uri-value this) (subseq value 1 (1- (length value)))))))

(defun ensure-uri (maybe-uri)
  (if (typep maybe-uri 'uri) maybe-uri
      (make-instance 'uri :value maybe-uri)))

;; -*- coding: utf-8 -*-

(in-package :openrdf.query)

(defstruct binding name value)

(defclass query-result () ())

(defclass graph-query-result (query-result repository-result) ())

;; TODO: This is begging to implement some container interface
(defclass tuple-query-result (query-result)
  ((variable-names :accessor tuple-query-result-variable-names)
   (string-tuples :accessor tuple-query-result-string-tuples)
   (cursor :initform 0 :accessor tuple-query-result-cursor)
   (tuple-count :initform 0 :accessor tuple-query-result-tuple-count)
   (binding-set :accessor tuple-query-result-binding-set)))

(defmethod initialize-instance :after
    ((this tuple-query-result) &rest initargs
     &key lambda-list argument-precence-order &allow-other-keys)
  (declare (ignore lambda-list argument-precence-order))
  (destructuring-bind (&key variable-names string-tuples &allow-other-keys)
      initargs
    (setf (tuple-query-result-variable-names this)
          (openrdf.utils:ensure-array variable-names)
          (tuple-query-result-string-tuples this)
          (openrdf.utils:ensure-array string-tuples)
          (tuple-query-result-tuple-count this)
          (length (tuple-query-result-string-tuples this))
          (tuple-query-result-binding-set this)
          (make-instance 'list-binding-set
                         :names (tuple-query-result-variable-names this)))))

(defmethod row-count ((this tuple-query-result))
  (tuple-query-result-tuple-count this))

(defmethod close-query ((this tuple-query-result)))

;; This must be the iterator implementation, must look into how
;; cl-containers handles this
(defmethod next-item ((this tuple-query-result))
  (unless (>= (tuple-query-result-cursor this)
              (tuple-query-result-tuple-count this))
    (let ((bset (tuple-query-result-binding-set this)))
      (reset bset (aref (tuple-query-result-string-tuples this)
                        (tuple-query-result-cursor this)))
      (incf (tuple-query-result-cursor this))
      bset)))

;; We inherit two more slots, which we could reuse: `key' and `test'
;; not sure what `key' does, but it could be a hash function?
;; actually, seems like a lot more then that...
(defclass list-binding-set (cl-containers:set-container)
  ((variable-names :accessor list-binding-set-variable-names)
   (string-tuple :accessor list-binding-set-string-tuple)
   (value-cache :initform nil :accessor list-binding-set-value-cache)))

(defmethod reset ((this list-binding-set) string-tuple)
  (setf (list-binding-set-string-tuple this) string-tuple)
  ;; I've no idea what this does
  (iter
    (for i :from 0 :below (length (list-binding-set-variable-names this)))
    (setf (aref (list-binding-set-value-cache this) i) nil)))

(defmethod validate-index ((this list-binding-set) index)
  (if (and (>= index 0) (< index (length (list-binding-set-string-tuple this))))
      index
      (error "index out of bounds, must be 0..~d"
             (1- (length (list-binding-set-string-tuple this))))))

(defmethod ith-value ((this list-binding-set) i)
  (labels ((%convert (x)
             (if (consp x) (mapcar #'%convert x)
                 (string->term x))))
    (or (aref (list-binding-set-value-cache this) i)
        (setf (list-binding-set-value-cache this)
              (%convert (aref (list-binding-set-string-tuple this) i))))))

(defmethod cl-containers:find-item ((this list-binding-set) item)
  (typecase item
    (integer (ith-value this (validate-index this item)))
    (otherwise
     (ith-value this (position (list-binding-set-variable-names this) item)))))

(defmethod cl-containers:iterate-elements ((this list-binding-set) func)
  ;; Maybe...
  (mapc func (list-binding-set-variable-names this)))

(defmethod get-binding ((this list-binding-set) name)
  (make-binding :name name :value (cl-containers:find-item self name)))

(defmethod get-row ((this list-binding-set))
  (list-binding-set-string-tuple this))

(defmethod size ((this list-binding-set))
  (length (list-binding-set-value-cache this)))

(defmethod list-binding->hash-table ((this list-binding-set) &optional string-dict)
  (let ((result (make-hash-table :test #'equal)))
    (cl-containers:iterate-elements
     (lambda (key)
       (setf (gethash key result)
             (let ((value (cl-containers:find-item this key)))
               (if string-dict (write-to-string value) value)))))
    result))
                  
(defmethod print-object ((this list-binding-set) stream)
  (format stream "~a" (list-binding->hash-table this t)))

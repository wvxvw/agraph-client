;; -*- coding: utf-8 -*-

(in-package :openrdf.model)

(defclass statement ()
  ((subject :initarg :subject :accessor statement-subject)
   (predicate :initarg :predicate :accessor statement-predicate)
   (object :initarg :object :accessor statement-object)
   (context :initarg :context :accessor statement-context)
   (string-tuple :accessor statement-tuple)))

(defmethod initialize-instance :after
    ((this statement) &rest initargs
     &key lambda-list argument-precence-order &allow-other-keys)
  (declare (ignore initargs lambda-list argument-precence-order))
  (setf (statment-tuple this) nil))

(defmethod value= ((this statement) (that statement))
  (with-slots ((subject-a subject) (predicate-a predicate)
               (object-a object) (context-a context))
      this
    (with-slots ((subject-b subject) (predicate-b predicate)
               (object-b object) (context-b context))
        that
      (and (equal subject-a subject-b)
           (equal predicate-a predicate-b)
           (equal object-a object-b)
           (equal context-a context-b)))))

(defmethod print-object ((this literal) stream)
  (format stream "~a" (statement-tuple this)))

(defmethod value-length ((this statement))
  (length (statement-tuple this)))

(defmethod statement-object ((this statement))
  (unless (slot-value this 'statement-object)
    (setf (statement-object this) (first (statement-tuple this))))
  (slot-value this 'statement-object))

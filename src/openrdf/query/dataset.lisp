;; -*- coding: utf-8 -*-

(in-package :openrdf.query)

(defparameter all-contexts "ALL_CONTEXTS")

(defclass dataset ()
  ((contexts :initform nil :initarg :contexts :accessor dataset-contexts)
   (default-graphs :initform (make-instance 'cl-containers:set-container)
     :accessor dataset-default-graphs)
   (named-graphs :initform (make-instance 'cl-containers:set-container)
     :accessor dataset-named-graphs)))

(defmethod initialize-instance :after
    ((this dataset) &rest initargs
     &key lambda-list argument-precence-order &allow-other-keys)
  (declare (ignore lambda-list argument-precence-order))
  (destructuring-bind (&key contexts &allow-other-keys) initargs
    (when contexts
      (iter
        (with ngraphs := (dataset-named-graphs this))
        (for ctx :in contexts)
        (cl-containers:insert-item ngraphs ctx)))))

(defmethod list-default-graphs ((this dataset))
  (let ((graphs (dataset-default-graphs this)))
    (if (zerop (cl-containers:size graphs)) all-contexts
        (cl-containers:collect-nodes graphs))))

(defmethod add-default-graph ((this dataset) uri)
  (cl-containers:insert-item (dataset-default-graphs this) uri))

(defmethod remove-default-graph ((this dataset) uri)
  (cl-containers:delete-item (dataset-default-graphs this) uri))

(defmethod list-named-graphs ((this dataset))
  (let ((graphs (dataset-named-graphs this)))
    (if (zerop (cl-containers:size graphs)) all-contexts
        (cl-containers:collect-nodes graphs))))

(defmethod add-named-graph ((this dataset) uri)
  (cl-containers:insert-item (dataset-named-graphs this) uri))

(defmethod remove-named-graph ((this dataset) uri)
  (cl-containers:delete-item (dataset-named-graphs this) uri))

(defmethod clear ((this dataset))
  (cl-containers:empty! (dataset-default-graphs this))
  (cl-containers:empty! (dataset-named-graphs this)))

(defmethod as-query ((this dataset) exclude-null-context-p)
  (if (not (or (dataset-named-graphs this)
               (dataset-default-graphs this)))
      (if exclude-null-context-p "" "## empty dataset ##")
      ;; this begs for iterate macro
      (with-output-to-string (stream)
        (cl-containers:iterate-nodes
         (dataset-default-graphs this)
         (lambda (item)
           (unless (and (not item) exclude-null-context-p)
             ;; this can probably be replaced by format
             (princ "FROM " stream)
             (append-uri this stream item)
             (princ #\Space))))
        ;; this block is the same as the block above
        ;; needs refactoring
        (cl-containers:iterate-nodes
         (dataset-named-graphs this)
         (lambda (item)
           (unless (and (not item) exclude-null-context-p)
             ;; this can probably be replaced by format
             (princ "FROM NAMED " stream)
             (append-uri this stream item)
             (princ #\Space)))))))

(defmethod print-object ((this dataset) stream)
  (print (as-query this) stream))

(defmethod append-uri ((this dataset) stream uri)
  ;; this needs to be made more generic
  (let ((uri-string
         (if (some
              (lambda (slot)
                (string= (symbol-name
                          (sb-mop:slot-definition-name slot)) "value"))
              (sb-mop:class-slots (class-of uri)))
             (slot-value uri 'value)
             (coerce uri 'string))))
    ;; magic!
    (if (> (length uri-string) 50)
        (format stream "<~a..~a>"
                (subseq uri-string 0 19)
                (subseq uri-string (- (length uri-string) 29)))
        (format stream "<~a>" uri-string))))

;; -*- coding: utf-8 -*-

(in-package :openrdf.query)

(defclass query-language ()
  ((registered-languages :initform nil
          :accessor query-language-registered-languages :allocation :class)
   (sparql :initform nil :accessor query-language-sparql :allocation :class)
   (prolog :initform nil :accessor query-language-prolog :allocation :class)
   (name :initarg :name :accessor query-language-name)))

(defmethod print-object ((this query-language) stream)
  (print (query-language-name this) stream))

(defmethod query-values ((this query-language))
  (copy-list (query-language-registered-languages this)))

(defmethod query-value-of ((this query-language) name)
  (iter
    (with lname := (string-downcase name))
    (for ql :in (query-language-registered-languages this))
    (when (string= (string-downcase (query-language-name ql)) lname)
      (return ql))))

;; Maybe there's a better way to do this
(defparameter *query-language*
  (let ((lang (make-instance 'query-language :name "SPARQL")))
    (setf (query-language-sparql lang) lang
          (query-language-prolog lang) (make-instance 'query-language :name "PROLOG"))))

(defclass query ()
  ((query-language :initform nil :accessor query-query-language)
   (query-string :initform nil :accessor query-query-string)
   (base-uri :initform nil :accessor query-base-uri)
   (dataset :initform nil :accessor query-dataset)
   (include-infered-p :initform nil :accessor query-include-inferred-p)
   (bindings :initform nil :accessor query-bindings)
   (connection :initform nil :accessor query-connection)
   (check-variables-p :initform nil :accessor query-check-variables-p)
   (preferred-exectuion-language :initform nil :accessor query-preferred-execution-language)
   (actual-exectuion-language :initform nil :accessor query-actual-execution-language)
   (subject-comes-first-p :initform nil :accessor query-subject-comes-first-p)))

(defparameter *trace-query* nil)

;; No idea why this should be a method...
(defmethod set-trace-query ((this query) settings)
  (declare (ignore this))
  (setf *trace-query* settings))

(defmethod set-binding ((this query) name value)
  (when (stringp value)
    (setf value (create-literal (query-connection this) value)))
  (setf (gethash name (query-bindings this)) value))

(defmethod remove-binding ((this query) name)
  (remhash name (query-bindings this)))

(defmethod set-contexts ((this query) contexts)
  (when contexts
    (let ((ds (make-instance 'dataset)))
      (iter
        (for ctx :in contexts)
        (add-named-graph
         ds
         (if (stringp ctx)
             (create-uri (query-connection this) ctx) ctx)))
      (setf (query-dataset this) ds))))

(defmethod evaluate-generic-query
    ((this query) &key countp acceptp analyzep analysis-technique analysis-timeout updatep)
  (let* ((conn (query-connection this))
         (named-contexts (contexts->ntriple-contexts
                          (when (query-dataset this)
                            (named-graphs (query-dataset this)))))
         (regular-contexts (contexts->ntriple-contexts
                          (if (query-dataset this)
                              (default-graphs (query-dataset this))
                              +all-contexts+)))
         bindings)
    (when (query-bindings this)
      (setf bindings (make-hash-table))
      (iter
        (for (key value) :in-hashtable (query-bindings this))
        (setf (gethash key bindings) (term->mini-term conn value))))
    (let ((mini (mini-repository conn)) response)
      (if (eql (query-language this)
               (query-language-sparql *query-language*))
          (setf response               ; this is ugly
                (eval-sparql-query mini (query-string this)
                                   :context regular-contexts
                                   :named-context named-contexts
                                   :inferp (query-include-infered-p this)
                                   :bindings bindings
                                   :check-variables-p (query-check-variables-p this)
                                   :countp countp
                                   :acceptp acceptp
                                   :analyzep analyzep
                                   :analysis-technique analysis-technique
                                   :analysis-timeout analysis-timeout
                                   :updatep updatep))
          (when (eql (query-language this)
                     (query-language-prolog *query-language*))
            (when named-contexts (error "Prolog queries don't support datasets"))
            (when analyzep (error "Prolog queries don't support analysis"))
            (setf response
                  (eval-prolog-query mini (query-string this)
                                     :inferp (query-include-infered-p this)
                                     :countp countp
                                     :acceptp acceptp))))
      response)))

(defun check-language (language)
  (when (stringp language)
      (setf language
            (cond
              ((string= language "SPARQL")
               (query-language-sparql *query-language*))
              ((string= language "PROLOG")
               (query-language-prolog *query-language*)))))
  (if (or (eql (query-language-sparql *query-language*) language)
          (eql (query-language-prolog *query-language*) language))
      language
      (error "Language ~a not recognized as query language" language)))

(defclass tuple-query (query) ())

(defclass update-query (query) ())

(defclass graph-query (query) ())

(defclass boolean-query (query) ())

(defgeneric evaluate (query &rest more-args)
  (:method  ((this tuple-query) &rest countp)
    (let ((response (evaluate-generic-query this :countp (first countp))))
      (if countp response
          (make-instance 'tuple-query-result
                         :names (gethash "names" response)
                         :values (gethash "values" response)))))
  (:method ((this update-query) &rest unused)
    (declare (ignore unused))
    (evaluate-generic-query this :updatep t))
  (:method ((this graph-query) &rest unused)
    (declare (ignore unused))
    (make-instance 'graph-query-result
                   :value (evaluate-generic-query this :updatep t)))
  (:method ((this boolean-query) &rest unused)
    (declare (ignore unused))
    (evaluate-generic-query this)))

(defmethod analyze ((this tuple-query) &key analysis-technique analysis-timeout)
  (evaluate-generic-query this :analysis-technique analysis-technique
                          :analysis-timeout analysis-timeout))

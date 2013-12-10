;; -*- coding: utf-8 -*-

(in-package :openrdf.model)

;; This is ugly, maybe there's some way to sidestep this with `eval-when'
(defun voc (symbol)
  (symbol-value (find-symbol symbol :openrdf.vocabulary)))

(defun voc-uri (symbol)
  (uri-value (symbol-value (find-symbol symbol :openrdf.vocabulary))))

(defun datatype-from-python (value datatype)
  ;; this translation is all messed up, but will do for the time being
  ;; some date-related noise has been dropped, I don't understand how it
  ;; can be relevant.
  (typecase value
    (string (values value datatype))
    (boolean (values (if value "true" "false") (voc '+boolean+)))
    (bignum (values (write-to-string value) (voc '+long+)))
    (fixnum (values (write-to-string value) (voc '+integer+)))
    (float (values (write-to-string value) (voc '+double+)))
    (timestamp (values (format-timestring nil value) (voc '+datetime+)))
    (otherwise (values (write-to-string value) datatype))))

(defclass literal (value)
  ((label :initarg :label :accessor literal-label)
   (datatype :initform nil :initarg :datatype :accessor literal-datatype)
   (language :initform nil :initarg :language :accessor literal-language)))

(defmethod convert-to-python ((this literal))
  "This is probably the reverse of `datatype-from-python'"
  ;; Why is value called label?
  (funcall
   (let ((datatype-uri (uri-value (literal-datatype this))))
     (cond
       ((string= datatype-uri (voc-uri +string+)) #'literal-value)
       ((string= datatype-uri (voc-uri +boolean+)) #'boolean-value)
       ((string= datatype-uri (voc-uri +long+)) #'long-value)
       ((string= datatype-uri (voc-uri +integer+)) #'int-value)
       ((string= datatype-uri (voc-uri +double+)) #'float-value)
       ((string= datatype-uri (voc-uri +datetime+)) #'date-value)) this)))

(defmethod (setf literal-datatype) (value (this literal))
  (setf (slot-value this 'datatype) (ensure-uri value)))

(defmethod value= ((this literal) (that literal))
  (with-slots ((label-a label) (datatype-a datatype) (language-a language)) this
    (with-slots ((label-b label) (datatype-b datatype) (language-b language)) this
      (and (eqal label-a label-b)
           (eqal datatype-a datatype-b)
           (eqal language-a language-b)))))

(defmethod int-value ((this literal))
  (truncate (coerce (literal-label this) 'integer) #xffffffff))

(defmethod long-value ((this literal))
  (truncate (coerce (literal-label this) 'integer) #xffffffffffffffff))

(defmethod float-value ((this literal))
  (coerce (literal-label this) 'float))

(defmethod boolean-value ((this literal))
  (coerce (literal-label this) 'boolean))

(defmethod date-value ((this literal))
  (parse-timestring (coerce (literal-label this) 'string)))

(defmethod ntriples ((this literal))
  (format nil "\"~a\"~@[@~a~]~@[~@*^^~a~]"
          (openrdf.utils:encode-ntriple-string (literal-label this))
          (literal-language this)
          (ntriples (literal-datatype this))))

;; no idea why, this should be the choice of the compound-literal...
(defparameter +range-literal+ "rangeLiteral")

(defclass compound-literal (literal)
  ((choice :initarg :choice :accessor compound-literal-choice :type string)
   (lower-bound :initform nil :initarg :lower-bound :type literal-impl
                :accessor compound-literal-lower-bound)
   (upper-bound :initform nil :initarg :upper-bound :type literal-impl
                :accessor compound-literal-upper-bound)))

(defmethod range-literal-p ((this compound-literal))
  (string= (compound-literal-choice this) +range-literal+))

;; The original code extends this from compound-literal, but it doesn
;; make sense to do so...
(defclass range-literal (literal)
  ((lower-bound :initform nil :initarg :lower-bound :type literal-impl
                :accessor range-literal-lower-bound)
   (upper-bound :initform nil :initarg :upper-bound :type literal-impl
                :accessor range-literal-upper-bound)))

(defun geounitp (value) (member value '(:km :mile :radian :degree)))

(deftype geounit () '(satisfies geounitp))

(defclass geo-coordinate (compound-literal)
  ((x :initarg :x :accessor geo-x)
   (y :initarg :y :accessor geo-y)
   (unit :initform nil :initarg :unit :type geounit
                :accessor geo-unit)
   (geo-type :initform nil :initarg :geo-type
                :accessor geo-type)))

(defclass geo-spatial-region (geo-coordinate) ())

(defclass geo-box (geo-spatial-region)
  ((x-min :initarg :x-min :accessor geo-x-min)
   (x-max :initarg :x-max :accessor geo-x-max)
   (y-max :initarg :y-max :accessor geo-y-max)
   (y-min :initarg :y-min :accessor geo-y-min)))

(defclass geo-circle (geo-spatial-region)
  ((radius :initarg :radius :accessor geo-radius)))

(defclass geo-polygon (geo-spatial-region)
  ((vertices :initarg :vertices :accessor geo-vertices)
   (resource :initarg :resource :accessor geo-resource)))

(defmethod print-object ((this geo-circle) stream)
  (with-slots (x y radius) this
    (format stream "|Circle|~d,~d radius=~d" x y radius)))

(defmethod print-object ((this geo-coordinate) stream)
  (with-slots (x y) this (format stream "|COOR|(~d, ~d)" % x y)))

(defmethod print-object ((this geo-box) stream)
  (with-slots (x-min x-max y-min y-max) this
    (format stream "|Box|~d,~d ~d,~d" % x-min x-max y-min y-max)))

(defmethod print-object ((this geo-polygon) stream)
  (format stream "|Polygon|~s" (geo-vertices this)))

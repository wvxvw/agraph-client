;; -*- coding: utf-8 -*-

(in-package :openrdf.utils)

(defun encode-ntriple-string (string)
  (with-output-to-string (stream)
    (iter
     (for c :in-string string)
     (princ 
      (case c
        (#\Tab "\\t")
        (#\Rubout "\\r")
        (#\Newline "\\n")
        (#\Space " ")
        (#\" "\\\"")
        (#\\ "\\")
        (otherwise
         (if (and (char>= c #\#)
                  (char<= c #\~)
                  (not (char= c #\')))
             (make-string 1 :initial-element c)
             (format nil "\\u~4,'0x" (char-code char)))))
      stream))))

(defun local-name-index (uri)
  (iter
    (initially (setq index -1))
    (for needle :in '(#\# #\/ #\:))
    (for index :next (when (< index 0) (position needle uri)))
    (finally
     (if (< index 0)
         (error "No separator found in URI '%s'" uri)
         (return (1+ index))))))

(defun ensure-uri-string (value)
  (if (char= (char value 0) #\<) value (format nil "<~a>" value)))

(defun local-name (uri)
  (subseq uri (local-name-index uri)))

(defun namespace (uri)
  (subseq uri 0 (local-name-index uri)))

(defparameter *uri-scanner*
  (create-scanner "^<([^:]+:[^\s\"<>]+)>$" :single-line-mode t))

(defparameter *nodeid-scanner*
  (create-scanner "^_:([A-Za-z][A-Za-z0-9]*)$" :single-line-mode t))

;; Not sure if this is intended: you cannot specify both language and uri
(defparameter *literal-scanner*
  (create-scanner
   "^\"([^\"\\\\]*(?:\\.[^\"\\\\]*)*)\"(?:@([a-z]+(?:-[a-z0-9]+)*)|\\^\\^<([^:]+:[^\\s\"<>]+)>)?$"
   :single-line-mode t))

(defun parse-uriref (maybe-uri)
  (nth-value 1 (scan-to-strings *uri-scanner* maybe-uri)))

(defun parse-nodeid (maybe-nodeid)
  (nth-value 1 (scan-to-strings *nodeid-scanner* maybe-nodeid)))

(defun parse-literal (maybe-literal)
  (let ((groups (nth-value 1 (scan-to-strings *literal-scanner* maybe-literal))))
    (and groups (values (aref groups 0) (aref groups 1) (aref groups 2)))))

(defun ensure-array (value)
  (typecase value
    (array value)
    (cons (make-array (length value) :initial-contents value))
    (null nil)
    (otherwise (make-array 1 :element-type (type-of value)
                           :initial-element value))))

#!/usr/local/bin/sbcl --script
;; The MIT License

;; Copyright (c) 2009-2010, MMER Foundation. All right reserved.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(defpackage :manager
  (:use :cl))

(in-package :manager)

(defparameter *root*
  (make-pathname :directory
		 (append (pathname-directory sb-ext::*load-pathname*) '("pbxconfig")))
  "Root directory for the configuration of the PBX node.")

(defparameter *agi-global* '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilitites

(defun directory-p (directory)
  "Returns true if dir is a dirrectory"
  (and (null (pathname-name directory))
       (null (pathname-type directory))))

(defun directory-ls (pathname &optional &key (recursively nil))
  (loop for element in (directory (format nil "~a/*.*" pathname))
     if  (directory-p element) append
       (if recursively
	   (cons element (directory-ls element))
	   (list  element))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AGI

(defun agi-command (command &rest args)
  (format *standard-output* "~A ~{~S~#[~:; ~]~}~%" (string-upcase command) args)
  (multiple-value-bind (code result data) (values-list (string-split #\Space (read-line t nil "")))
    (string-left-trim "(" (string-right-trim ")" data))))

(defun agi-verbose (message &optional (level 3))
  (agi-command "verbose" message level))

(defun agi-set-variable (variable value)
  (agi-command "set variable" variable value))

(defun agi-get-variable (variable)
  (agi-command "get variable" variable))

(defun agi-database-put (family key value)
  (agi-command "database put"
	       (princ-to-string family)
	       (princ-to-string key)
	       (princ-to-string value)))

(defun agi-database-get (family key)
  (agi-command "database get"
	       (princ-to-string family)
	       (princ-to-string key)))

(defun agi-database-del (family key)
  (agi-command "database del"
	       (princ-to-string family)
	       (princ-to-string key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read/Write Structure primitives

(defun object-read (pathname)
  "Read INI key/value pairs in CONFIG file of one structure."
  (let ((result))
    (with-open-file (object (.config pathname) :external-format :utf8)
      (handler-case
	  (progn (do ((line (read-line object nil)
			    (read-line object nil)))
		     ((null line) result)
		   (handler-case
		       (multiple-value-bind (key value) (values-list (string-split #\= line))
			 (push (list key (string-split #\, value)) result)))))
	(sb-int:stream-decoding-error ())))))

(defun string-split (devider string)
  (if (null (position devider string))
      (list string)
      (append  (list (subseq string 0 (position devider string)))
	       (string-split devider (subseq string (+ 1 (position devider string)))))))

(defun .config (pathname)
  (make-pathname :directory (pathname-directory pathname)
		 :name ".config"))

(defun family-read-by-name (family name)
  "Read FAMILY object by its name."
  (let ((pathname (subdir name (subdir family))))
    (append (list (append (list "name") (last (pathname-directory pathname))))
	    (object-read pathname))))

(defun subdir (folder-name &optional (current *root*))
  "Build pathname for sub-directory of the current folder."
  (make-pathname :directory (append (pathname-directory current) (list folder-name))))

(defun object-get-field-value (object field)
  (let ((value (second (find-if (lambda (key/pair)
	    (equal key/pair field)) object :key #'car))))
    value))

(defun family-list (family-name)
  "List all find FAMILY objects."
  (mapcar (lambda (element)
	    (car (last (pathname-directory element))))
	  (directory-ls (subdir family-name))))

(defun family-find-by-field (field-name value &optional &key (family "members") (test #'equal))
  "Find any valid OBJECT of FAMILY where  FIELD-NAME value equal to VALUE."
  (do ((elements (family-list family) (cdr elements)))
      ((null (car elements)))
    (let ((object (family-read-by-name family (car elements))))
      (when (member value (object-get-field-value object field-name) :test test)
	(return (values t object))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PBX specific 
(defun pbx-find-report (result object)
  (agi-verbose (format nil "Name: ~s" (object-get-field-value object "name")))
  (agi-verbose (format nil "Result: ~s" result)))

(defun pbx-member-find-by-pin (pin)
  (multiple-value-bind (found member)
      (family-find-by-field "pin" (princ-to-string pin))
    (pbx-find-report found member)
    (agi-set-variable 'mfound found)
    (agi-set-variable 'mname (object-get-field-value member "name"))))

(defun pbx-queue-find-by-id (id)
  (multiple-value-bind (found queue)
      (family-find-by-field "id" (princ-to-string id) :family "queues")
    (pbx-find-report found queue)
    (when found
      (agi-set-variable 'qname (object-get-field-value queue "name")))))

(defun pbx-acl-member-join-to-queue? (member-name queue-name)
  "Check if MEMBER allowed to connect to QUEUE"
  (let ((member (family-read-by-name "members" member-name))
	(queue (family-read-by-name "queues" queue-name)))
    (let ((qacl (object-get-field-value member "qacl")))
      (if (member (car (object-get-field-value queue "id")) qacl :test #'equal)
	  (progn
	    (agi-verbose (format nil "Result: ~s" t))
	    (agi-set-variable 'maccess t)	    
	    (agi-database-put "members"
	    		      (agi-get-variable "CALLERID(number)")
			      queue-name) t) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(defun read-agi-global ()
  (do ((line (read-line t nil "")
	     (read-line t nil "")))
      ((string-equal "" line) t)
    (multiple-value-bind (key value) (values-list (string-split #\: line))
      (push (list (subseq key 4)
		  (string-trim " " value)) *agi-global* ))))

(defun exec-function (argv)
  (handler-case
      (progn
	(let ((func (symbol-function (find-symbol (string-upcase (car argv))))))
	  (agi-verbose "--------------------------------------------------")
	  (agi-verbose (format nil " >>> ~A ~A"  (car argv) (cdr argv)))
	  (agi-verbose "--------------------------------------------------")
	  ;; (mapcar (lambda (pair)
	  ;; 	    (agi-verbose (format nil "~a=~S" (car pair) (cadr pair))))  *agi-global*)
	  (apply func (cdr argv))))
    (undefined-function ()
      (warn "The function \"~a\" is undefined." (car argv)))))


(if (> (length sb-ext:*posix-argv*) 1)
    (progn
      (read-agi-global)
      (exec-function (cdr sb-ext:*posix-argv*)))
    (agi-verbose "script.lisp <funcall> <&args>"))

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

(defparameter *root* (make-pathname :directory '(:relative "pbxconfig"))
  "Root directory for the configuration of the PBX node.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilitites

(defun directory-p (directory)
  "Returns true if dir is a dirrectory"
  (and (null (pathname-name directory))
       (null (pathname-type directory))))

(defun directory-ls (pathname &optional &key (recursively nil))
  (loop for element in (directory (format nil "~a/*.*" pathname))
     append (if (and (directory-p element)
					 recursively)
				(cons element (directory-ls element))
				(list element))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AGI

(defun agi-command (command &rest args)
  (format *standard-output* "~A ~{~S~#[~:; ~]~}~%" (string-upcase command) args)
  (sleep 0.00005))

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
    (with-open-file (object (.config pathname))
      (do ((line (read-line object nil)
				 (read-line object nil)))
		  ((null line) result)
		(handler-case
			(multiple-value-bind (key value) (values-list (string-split #\= line))
			  (push (list key (string-split #\, value)) result)))))))

(defun string-split (devider string)
  (if (null (position devider string))
      string
      (list  (subseq string 0 (position devider string))
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
  (cadr (find-if (lambda (key/pair)
				   (equal key/pair field)) object :key #'car)))

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
	  (when (funcall test value (object-get-field-value object field-name))
		(return (values t object ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PBX specific 

(defun pbx-member-find-by-pin (pin)
  (multiple-value-bind (access member)
	  (family-find-by-field "pin" (princ-to-string pin))
	(progn
	  (if access
		  (progn
			(agi-verbose (format nil "Name: ~s" (object-get-field-value member "name")))
			(agi-verbose (format nil "Access: ~s" access)))
		  (agi-verbose (format nil "No any member with pin ~a found!" pin)))
	  (agi-set-variable 'maccess (princ-to-string access))
	  (agi-set-variable 'mname   (object-get-field-value member "name")))))

(defun pbx-queue-find-by-id (id)
  (multiple-value-bind (find queue)
	  (family-find-by-field "id" (princ-to-string id) :family "queues")
	(if find
		(progn
		  (agi-verbose (format nil "Name: ~s" (object-get-field-value queue "name")))
		  (agi-set-variable 'qname (object-get-field-value queue "name")))
		(agi-verbose (format nil "Queue with id ~a not found!" id) ))))

(defun pbx-acl-member-join-to-queue? (member queue)
  "Check if MEMBER allowed to connect to QUEUE"
  (let ((member (family-read-by-name "members" member))
		(queue (family-read-by-name "queues" queue)))
	(let ((qacl (object-get-field-value member "qacl")))
	  (if (member (object-get-field-value queue "id" ) (if (listp qacl) qacl (list qacl) ) :test #'equal)
		  t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(defun exec-function (argv)
  (handler-case
	  (progn
		(let ((func (symbol-function (find-symbol (string-upcase (car argv))))))
		  (agi-verbose (format nil "[ Function: ~A  argv: ~A ]"  (car argv) (cdr argv)))
		  (agi-verbose "==================================================")
		  (apply func (cdr argv))
		  (agi-verbose "==================================================")))
	(undefined-function ()
	  (warn "The function \"~a\" is undefined." (car argv)))))

(if (> (length sb-ext:*posix-argv*) 1)
	(exec-function (cdr sb-ext:*posix-argv*))
	(agi-verbose "script.lisp <funcall> <&args>"))

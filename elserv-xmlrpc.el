;;; elserv-xmlrpc.el -- Elserv interface for XML-RPC.

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: HTTP, XML-RPC

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;
;; Example:
;;
;; (defun hello (p)
;;   (format "Hello, %s." p))
;; To register above function hello as "example.hello", evaluate following.
;; (elserv-xmlrpc-register "example.hello" 'hello '("string" "string") "ISO-2022-JP")
;; 1st argument is the method name to register.
;; 2nd argument is the function that corresponds.
;; 3rd argument is the signature for the method.
;; 4th argument is the encoding for the returned xml(optional).

;;; Code:

(require 'elserv)
(require 'xml-rpc)
(require 'mcharset)
(eval-when-compile (require 'cl))

(put 'elserv-xmlrpc-exception 'error-conditions
     '(elserv-xmlrpc-exception error))

(defmacro elserv-xmlrpc-define-fault (name code string)
  "Define fault code with NAME, CODE, and STRING."
  `(progn
     (put ',name 'error-conditions '(,name elserv-xmlrpc-exception error))
     (put ',name 'elserv-xmlrpc-code ,code)
     (put ',name 'elserv-xmlrpc-string ,string)))

(elserv-xmlrpc-define-fault elserv-xmlrpc-fault-method-missing      1
			    "Method missing.")
(elserv-xmlrpc-define-fault elserv-xmlrpc-fault-uncaught-exception  2
			    "Uncaught exception.")
(elserv-xmlrpc-define-fault elserv-xmlrpc-fault-wrong-parameter     3
			    "Wrong parameter.")
(elserv-xmlrpc-define-fault elserv-xmlrpc-fault-wrong-number-params 4
			    "Wrong number of parameters.")
(elserv-xmlrpc-define-fault elserv-xmlrpc-fault-missing-method-name 5
			    "Missing method name.")
(elserv-xmlrpc-define-fault elserv-xmlrpc-fault-recursive-call      6
			    "Recursive call.")
(elserv-xmlrpc-define-fault elserv-xmlrpc-fault-expected-struct     8
			    "Struct expected.")

(put 'with-elserv-xmlrpc-fault-handler 'edebug-form-spec '(body))
(defmacro with-elserv-xmlrpc-fault-handler (&rest forms)
  "Evaluate FORMS like progn with elserv xmlrpc fault handler."
  `(condition-case why
       (progn ,@forms)
     (wrong-number-of-arguments
      (elserv-xmlrpc-exception 'elserv-xmlrpc-fault-wrong-number-params))
     (wrong-type-argument
      (elserv-xmlrpc-exception 'elserv-xmlrpc-fault-wrong-parameter))
     (void-function
      (elserv-xmlrpc-exception 'elserv-xmlrpc-fault-method-missing))
     (elserv-xmlrpc-exception (elserv-xmlrpc-exception (car why)))
     (error (elserv-xmlrpc-exception 'elserv-xmlrpc-uncaught-exception
				     (format "Uncaught exception: %s\n" why)))))

(defun elserv-xmlrpc-exception (why &optional msg)
  "Make a fault response from WHY.
If optional MSG is specified, it is used as response body."
  `((methodResponse nil
		    (fault
		     nil
		     ,(car
		       (xml-rpc-value-to-xml-list
			`(("faultCode" . ,(get why
					       'elserv-xmlrpc-code))
			  ("faultString" . ,(or
					     msg
					     (get why
						  'elserv-xmlrpc-string))))))))))

(defun elserv-xmlrpc-register (method-name function signature &optional encoding)
  "Add METHOD-NAME entry for FUNCTION with SIGNATURE.
Optional ENCODING specifies the encoding of the response."
  (put 'elserv-xmlrpc-method
       (intern method-name)
       function)
  (put 'elserv-xmlrpc-method-signature
       (intern method-name)
       signature)
  (when encoding
    (put 'elserv-xmlrpc-method-encoding
	 (intern method-name)
	 encoding)))

(defun elserv-xmlrpc-unregister (method-name)
  "Remove METHOD-NAME entry."
  (put 'elserv-xmlrpc-method
       (intern method-name)
       nil)
  (put 'elserv-xmlrpc-method-signature
       (intern method-name)
       nil)
  (when (get 'elserv-xmlrpc-method-encoding
	     (intern method-name))
    (put 'elserv-xmlrpc-method-encoding
	 (intern method-name)
	 nil)))

(defun elserv-xmlrpc-get-function (method-name)
  "Get fuction for METHOD-NAME."
  (get 'elserv-xmlrpc-method
       (intern method-name)))

(defun elserv-xmlrpc-get-signature (method-name)
  "Get signature for METHOD-NAME."
  (get 'elserv-xmlrpc-method-signature
       (intern method-name)))

(defun elserv-xmlrpc-get-encoding (method-name)
  "Get encoding for METHOD-NAME."
  (get 'elserv-xmlrpc-method-encoding
       (intern method-name)))

(defun elserv-xmlrpc-get-method-help (method-name)
  "Get documentation of METHOD-NAME."
  (let ((func (get 'elserv-xmlrpc-method (intern method-name))))
    (documentation func)))

(defun elserv-xmlrpc-list-methods ()
  "Return List of method-names"
  (let ((plist (symbol-plist 'elserv-xmlrpc-method))
	methods)
    (while plist
      (if (cadr plist)
	  (setq methods (cons (symbol-name (car plist)) methods)))
      (setq plist (cddr plist)))
    methods))

(defun elserv-xmlrpc-process-request (request)
  (let ((xml (car 
	      (with-temp-buffer
		(insert request)
		(xml-parse-region (point-min) (point-max)))))
	method)
    (setq method 
	  (car (xml-node-children (assq 'methodName (xml-node-children xml)))))
    (setq params
	  (xml-node-children (assq 'params (xml-node-children xml)))
	  params
	  (unless (equal params '("")) ; empty params
	    (mapcar
	     (lambda (node)
	       (xml-rpc-xml-list-to-value (xml-node-children node)))
	     params)))
    (elserv-xmlrpc-method-call method params)))

(defun elserv-xmlrpc-method-call (method params)
  "XML-RPC method call for METHOD and PARAMS."
  (let ((encoding (elserv-xmlrpc-get-encoding method)))
    (concat
     "<?xml version=\"1.0\""
     (if encoding
	 (format "encoding=\"%s\"" encoding))
     "?>"
     (xml-rpc-xml-to-string
      (car
       (with-elserv-xmlrpc-fault-handler
	(unless method (signal
			'elserv-xmlrpc-fault-missing-method-name nil))
	(let ((func (elserv-xmlrpc-get-function method)))
	  (if (and func
		   (functionp func))
	      `((methodResponse
		 nil
		 (params nil
			 (param nil
				,(car
				  (xml-rpc-value-to-xml-list
				   (apply func params)))))))
	    (signal 'elserv-xmlrpc-fault-method-missing nil)))))
      (if encoding
	  (mime-charset-to-coding-system encoding))))))

(defun elserv-xmlrpc-register-defaults ()
  "Register default methods."
  ;; standard methods.
  (elserv-xmlrpc-register "system.listMethods" 
			  'elserv-xmlrpc-list-methods
			  '(("array")))
  (elserv-xmlrpc-register "system.methodSignature"
			  'elserv-xmlrpc-get-signature
			  '(("array" "string")))
  (elserv-xmlrpc-register "system.methodHelp"
			  'elserv-xmlrpc-get-method-help
			  '(("string" "string")))
  ;; search method.
  (elserv-xmlrpc-register "system.search"
			  'elserv-search
			  '(("array" "string"))))


(require 'product)
(product-provide (provide 'elserv-xmlrpc) (require 'elserv))

;;; elserv-xmlrpc.el ends here

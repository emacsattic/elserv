;;; es-xmlrpc.el -- Elserv interface for XML-RPC.

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

;;; Code:

(require 'elserv)
(require 'elserv-xmlrpc)

(defun elserv-xmlrpc-function (result path ppath request)
  (elserv-set-result-header
   result 
   (list 'content-type "text/xml"))
  (elserv-set-result-body
   result
   (elserv-xmlrpc-process-request (plist-get request 'body))))

(defun elserv-xmlrpc-publish (process path)
  "Publish XML-RPC service.
PROCESS is the elserv server process.
PATH is the path to publish XML-RPC content."
  (elserv-xmlrpc-register-defaults)
  (elserv-publish process path
		  :function 'elserv-xmlrpc-function
		  :description "XML-RPC service"))

(require 'product)
(product-provide (provide 'es-xmlrpc) (require 'elserv))

;;; es-xmlrpc.el ends here

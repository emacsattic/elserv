;;; elserv-negotiation.el --  Handles the content negotiation.

;; Copyright (C) 2001 OHASHI Akira <bg66@koka-in.org>

;; Author: OHASHI Akira <bg66@koka-in.org>
;; Keywords: HTTP

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

;;; Code:

(eval-when-compile (require 'cl))
(require 'elserv)

(defvar elserv-negotiation-http-header
  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<HTML><HEAD>
<TITLE>406 Not Acceptable</TITLE>
</HEAD><BODY>
<H1>Not Acceptable</H1>
An appropriate representation of the requested resource %s could not be found on this server.<P>
Available variants:
<ul>\n")

(defvar elserv-negotiation-list-format
  "<li><a href=\"%s\">%s</a> , %s, language %s\n")

(defvar elserv-negotiation-http-footer
  "</ul>
<HR>
<ADDRESS>%s Server at %s Port %s</ADDRESS>
</BODY></HTML>\n")

(defvar elserv-negotiation-language-list
  '("en" "ja" "da" "nl" "et" "fr" "de" "el" "it" "pt" "ltz" "ca" "es" "sv"))

(defun elserv-negotiation (filename language)
  "Return a filename which matches to FILENAME and LANGUAGE exists."
  (let (realfile)
    (if language
	(let ((langs (elserv-negotiation-language language)))
 	  (setq realfile
		(catch 'done
		  (dolist (lang langs)
		    (if (file-readable-p
			 (setq realfile (concat filename "." lang)))
			(throw 'done realfile)))))))
    (if (stringp realfile)
	realfile
      (if (file-readable-p filename)
	  filename
	(let ((files
	       (directory-files (file-name-directory filename) nil
				(concat "^" (file-name-nondirectory filename)
					"\\.[A-Za-z]+$"))))
	  (dolist (file files)
	    (catch 'found
	      (dolist (list elserv-negotiation-language-list)
		(if (string-match (concat "\\." list "$") file)
		    (throw 'found t)))
		(setq files (delete file files))))
	  files)))))

(defun elserv-negotiation-language (string)
  "Parse Accept-Language field body and return language candidate list."
  (let (candidates)
    (while (string-match "^\\([A-Za-z-]+\\)\\(\\(; *q=[0-9.]+\\)?, *\\)?"
			 string)
      (setq candidates (cons 
			(substring string (match-beginning 1)(match-end 1))
			candidates))
      (setq string (substring string (match-end 0))))
    (nreverse candidates)))

(defun elserv-negotiation-make-result (result host path files)
  "Make a result of content negotioation."
  (let (port string)
    (if (or (string-match "^\\[\\([^]]+\\)\\]:?\\([0-9]*\\)" host)
	    (string-match "^\\([^:]+\\):?\\([0-9]*\\)" host))
	(progn
	  (setq port (match-string 2 host))
	  (setq host (match-string 1 host)))
      (setq port "80"))
    (dolist (file files)
       (setq string (concat string
			    (format elserv-negotiation-list-format
				    file file
				    (elserv-mime-type file)
				    (file-name-extension file)))))
    (elserv-set-result-code result 'elserv-ok)
    (elserv-set-result-header result `(content-type "text/html"))
    (elserv-set-result-body
     result
     (concat
      (format elserv-negotiation-http-header path)
      string
      (format elserv-negotiation-http-footer (elserv-version) host port)))
    result))

(provide 'elserv-negotiation)

;;; elserv-negotiation.el ends here

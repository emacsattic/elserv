;;; elserv-autoindex.el -- Handles the on-the-fly html index generation

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

(defvar elserv-autoindex-http-header
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">
<HTML>
 <HEAD>
  <TITLE>Index of %s</TITLE>
 </HEAD>
 <BODY>
<H1>Index of %s</H1>
<PRE><IMG SRC=\"/icons/blank.gif\" ALT=\"     \"> <A HREF=\"?N=D\">Name</A>                    <A HREF=\"?M=A\">Last modified</A>       <A HREF=\"?S=A\">Size</A>  <A HREF=\"?D=A\">Description</A>
<HR>\n")

(defvar elserv-autoindex-list-format
  "<IMG SRC=\"%s\" ALT=\"[%s]\"> <A HREF=\"%s\">%s</A>%s%s  \n")

(defvar elserv-autoindex-http-footer
  "</PRE><HR>
<ADDRESS>%s Server at %s Port %s</ADDRESS>
</BODY></HTML>\n")

(defvar elserv-autoindex-ignore-list '("." ".htaccess" "CVS" "RCS"))

(defvar elserv-autoindex-icons-alist
  '(("\\.css$"    . (:label "TXT" :icon "/icons/text.gif"))
    ("\\.html?$"  . (:label "TXT" :icon "/icons/text.gif"))
    ("\\.txt$"    . (:label "TXT" :icon "/icons/text.gif"))
    ("\\.jpe?g$"  . (:label "IMG" :icon "/icons/image2.gif"))
    ("\\.gif$"    . (:label "IMG" :icon "/icons/image2.gif"))
    ("\\.png$"    . (:label "IMG" :icon "/icons/image2.gif"))
    ("\\.tiff?$"  . (:label "IMG" :icon "/icons/image2.gif"))
    ("\\.x[bp]m$" . (:label "IMG" :icon "/icons/image2.gif"))
    ("\\.gz$"     . (:label "   " :icon "/icons/compressed.gif"))
    ("\\.z$"      . (:label "CMP" :icon "/icons/compressed.gif"))
    ("\\.e?ps$"   . (:label "   " :icon "/icons/a.gif"))
    ("\\.tex$"    . (:label "   " :icon "/icons/tex.gif"))
    ("\\.dvi$"    . (:label "   " :icon "/icons/dvi.gif"))
    ("\\.pdf$"    . (:label "   " :icon "/icons/layout.gif"))
    ("\\.tar$"    . (:label "   " :icon "/icons/tar.gif"))
    ("\\.zip$"    . (:label "   " :icon "/icons/compressed.gif"))
    ("\\.lzh$"    . (:label "   " :icon "/icons/compressed.gif"))
    ("\\.mp[23]$" . (:label "SND" :icon "/icons/sound2.gif"))
    ("\\.midi?$"  . (:label "SND" :icon "/icons/sound2.gif"))
    ("\\.wav$"    . (:label "SND" :icon "/icons/sound2.gif"))
    ("\\.au$"     . (:label "SND" :icon "/icons/sound2.gif"))
    ("\\.ram$"    . (:label "SND" :icon "/icons/sound2.gif"))
    ("\\.r[am]$"  . (:label "SND" :icon "/icons/sound2.gif"))
    ("\\.mpe?g$"  . (:label "VID" :icon "/icons/movie.gif"))
    ("\\.qt$"     . (:label "VID" :icon "/icons/movie.gif"))
    ("\\.mov$"    . (:label "VID" :icon "/icons/movie.gif"))
    ("\\.avi$"    . (:label "VID" :icon "/icons/movie.gif"))
    ("^core$"     . (:label "   " :icon "/icons/bomb.gif"))
    ("\\.c$"      . (:label "   " :icon "/icons/c.gif"))
    ("\\.p[ly]$"  . (:label "   " :icon "/icons/p.gif"))
    ("\\.sh$"     . (:label "   " :icon "/icons/script.gif"))
    ("\\.bin$"    . (:label "   " :icon "/icons/binary.gif"))
    ("\\.exe$"    . (:label "   " :icon "/icons/binary.gif"))))

(defun elserv-autoindex (host path directory)
  "Handles the on-the-fly html index generation."
  (let (port files string)
    (if (or (string-match "^\\[\\([^]]+\\)\\]:?\\([0-9]*\\)" host)
	    (string-match "^\\([^:]+\\):?\\([0-9]*\\)" host))
	(progn
	  (setq port (match-string 2 host))
	  (setq host (match-string 1 host)))
      (setq port "80"))
    (setq path (substring path 0 (string-match "/$" path)))
    (setq files (directory-files directory))
    (dolist (list elserv-autoindex-ignore-list)
      (setq files (delete list files)))
    (dolist (filename files)
      (let ((icon (elserv-autoindex-get-attr directory filename 'icon))
	    (label (elserv-autoindex-get-attr directory filename 'label))
	    (name (elserv-autoindex-get-attr directory filename 'name))
	    (lastmodified (elserv-autoindex-get-attr
			   directory filename 'lastmodified))
	    (size (elserv-autoindex-get-attr directory filename 'size)))
	(setq string
	      (concat string
		      (format elserv-autoindex-list-format
			      icon label filename name
			      (format (concat "%" (number-to-string
						   (- 41 (length name))) "s")
				      lastmodified)
			      (format "%7s" size))))))
    (elserv-set-result-code result 'elserv-ok)
    (elserv-set-result-header result `(content-type "text/html"))
    (elserv-set-result-body
     result
     (concat
      (format elserv-autoindex-http-header path path)
      string
      (format elserv-autoindex-http-footer (elserv-version) host port)))
    result))

(defun elserv-autoindex-get-attr (directory filename type)
  "Return attribute of FILENAME."
  (let ((realfile (expand-file-name filename directory)))
    (cond
     ((eq type 'icon)
      (if (string= filename "..")
	  "/icons/back.gif"
	(elserv-autoindex-get-icon realfile ':icon)))
     ((eq type 'label)
      (elserv-autoindex-get-icon realfile ':label))
     ((eq type 'name)
      (if (string= filename "..")
	  "Parent Directory"
	filename))
     ((eq type 'lastmodified)
      (format-time-string "%d-%b-%Y %R" (nth 5 (file-attributes realfile))))
     ((eq type 'size)
      (if (file-directory-p realfile)
	  "-"
	(let ((size (nth 7 (file-attributes realfile))))
	  (cond
	   ((<= 1 (/ size 1048576))
	  (format "%3.1fM" (/ size 1048576.0)))
	   ((<= 1 (/ size 1024))
	    (format "%4.0fk" (/ size 1024.0)))
	   (t size))))))))

(defmacro make-match-function (string)
  `(lambda (regexp)
     (save-match-data (string-match regexp ,string))))

(defun elserv-autoindex-get-icon (filename &optional type)
  "Return icon's filename or lable for FILENAME."
  (let ((alist (or
		(assoc-if
		 (make-match-function (file-name-nondirectory filename))
		 elserv-autoindex-icons-alist)
		(if (file-directory-p filename)
		    '("directory" .
		      (:label "DIR" :icon "/icons/folder.gif"))
		  '("unknown" . (:label "   " :icon "/icons/unknown.gif"))))))
    (plist-get (cdr alist) type)))

(provide 'elserv-autoindex)

;;; elserv-autoindex.el ends here

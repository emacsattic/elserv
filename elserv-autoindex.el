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
<PRE><IMG SRC=\"%s/blank.gif\" ALT=\"     \"> <A HREF=\"?N=D\">Name</A>                     <A HREF=\"?M=A\">Last modified</A>       <A HREF=\"?S=A\">Size</A>  <A HREF=\"?D=A\">Description</A>
<HR>\n")

(defvar elserv-autoindex-list-format
  "<IMG SRC=\"%s\" ALT=\"[%s]\"> <A HREF=\"%s\">%s</A> %s%s  \n")

(defvar elserv-autoindex-http-footer
  "</PRE><HR>
<ADDRESS>%s Server at %s Port %s</ADDRESS>
</BODY></HTML>\n")

(defvar elserv-autoindex-ignore-list '("^\\." "~$" "#$" "^HEADER" "^README"
				              "RCS" "CVS" ",v$" ",t$"))

(defvar elserv-autoindex-icon-alist
  '(("\\.css$"    . (:label "TXT" :icon "text.gif"))
    ("\\.html?$"  . (:label "TXT" :icon "text.gif"))
    ("\\.txt$"    . (:label "TXT" :icon "text.gif"))
    ("\\.jpe?g$"  . (:label "IMG" :icon "image2.gif"))
    ("\\.gif$"    . (:label "IMG" :icon "image2.gif"))
    ("\\.png$"    . (:label "IMG" :icon "image2.gif"))
    ("\\.tiff?$"  . (:label "IMG" :icon "image2.gif"))
    ("\\.x[bp]m$" . (:label "IMG" :icon "image2.gif"))
    ("\\.gz$"     . (:label "   " :icon "compressed.gif"))
    ("\\.z$"      . (:label "CMP" :icon "compressed.gif"))
    ("\\.e?ps$"   . (:label "   " :icon "a.gif"))
    ("\\.tex$"    . (:label "   " :icon "tex.gif"))
    ("\\.dvi$"    . (:label "   " :icon "dvi.gif"))
    ("\\.pdf$"    . (:label "   " :icon "layout.gif"))
    ("\\.tar$"    . (:label "   " :icon "tar.gif"))
    ("\\.zip$"    . (:label "   " :icon "compressed.gif"))
    ("\\.lzh$"    . (:label "   " :icon "compressed.gif"))
    ("\\.mp[23]$" . (:label "SND" :icon "sound2.gif"))
    ("\\.midi?$"  . (:label "SND" :icon "sound2.gif"))
    ("\\.wav$"    . (:label "SND" :icon "sound2.gif"))
    ("\\.au$"     . (:label "SND" :icon "sound2.gif"))
    ("\\.ram$"    . (:label "SND" :icon "sound2.gif"))
    ("\\.r[am]$"  . (:label "SND" :icon "sound2.gif"))
    ("\\.mpe?g$"  . (:label "VID" :icon "movie.gif"))
    ("\\.qt$"     . (:label "VID" :icon "movie.gif"))
    ("\\.mov$"    . (:label "VID" :icon "movie.gif"))
    ("\\.avi$"    . (:label "VID" :icon "movie.gif"))
    ("^core$"     . (:label "   " :icon "bomb.gif"))
    ("\\.c$"      . (:label "   " :icon "c.gif"))
    ("\\.p[ly]$"  . (:label "   " :icon "p.gif"))
    ("\\.sh$"     . (:label "   " :icon "script.gif"))
    ("\\.bin$"    . (:label "   " :icon "binary.gif"))
    ("\\.exe$"    . (:label "   " :icon "binary.gif"))))

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
      (setq files (delete-if (lambda (string)
			       (save-match-data (string-match list string)))
			     files)))
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
      (format elserv-autoindex-http-header path path
	      elserv-icon-publish-path)
      string
      (format elserv-autoindex-http-footer (elserv-version) host port)))
    result))

(defun elserv-autoindex-get-attr (directory filename type)
  "Return attribute of FILENAME."
  (let ((realfile (expand-file-name filename directory)))
    (cond
     ((eq type 'icon)
      (concat elserv-icon-publish-path "/"
	      (if (string= filename "..")
		  "back.gif"
		(elserv-autoindex-get-icon realfile ':icon))))
     ((eq type 'label)
      (elserv-autoindex-get-icon realfile ':label))
     ((eq type 'name)
      (if (string= filename "..")
	  "Parent Directory"
	filename))
     ((eq type 'lastmodified)
      (let ((system-time-locale "C"))
	(format-time-string "%d-%b-%Y %R" (nth 5 (file-attributes realfile)))))
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

(defun elserv-autoindex-get-icon (filename &optional type)
  "Return icon's filename or lable for FILENAME."
  (let ((alist (or
		(assoc-if
		 (lambda (regexp)
		   (save-match-data
		     (string-match regexp (file-name-nondirectory filename))))
		 elserv-autoindex-icon-alist)
		(if (file-directory-p filename)
		    '("directory" .
		      (:label "DIR" :icon "folder.gif"))
		  '("unknown" . (:label "   " :icon "unknown.gif"))))))
    (plist-get (cdr alist) type)))

(provide 'elserv-autoindex)

;;; elserv-autoindex.el ends here

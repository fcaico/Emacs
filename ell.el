;;; ell.el --- Browse the Emacs Lisp List
;; Author: Jean-Philippe Theberge (yesod@mercere.net)
;; Created: 22/05/2000 - update: 23/05/2000
;; Version: 0.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; Copyright (c) 1998 - 1999 Free Software Foundation, Inc.
;;
;; This file is not part of GNU Emacs. :-(
;;
;; GNU Emacs is free software; you can redistribute it and/or modify 
;; it under the terms of the GNU General Public License as published by 
;; the Free Software Foundation; either version 2, or (at your option) 
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License 
;; along with GNU Emacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
 
;;; Commentary:
;; 
;;  The Emasc Lisp Lisp is available at http://anc.ed.ac.uk/~stephen/emacs/ell.html
;;  
;;  If Shephen change the layout of his web page, this package may stop to work correcly 
;;  You may then need to upgrade this package.
 
;;; Code:
(defvar ell-host "anc.ed.ac.uk")
(defvar ell-path "~stephen/emacs/ell.html")
 
(defun ell-packages-list ()
  "Insert the contents of URL at point."
  (ignore-errors (kill-buffer "*ell-temp-buffer*"))
  (with-temp-buffer
    (let* ((host ell-host)
    (path ell-path)
    (coding-system-for-read 'binary)
    (coding-system-for-write 'binary)
    (http (open-network-stream
    "ell-retrieval-process"
    "*ell-temp-buffer*"
    host
    80))
    (pbuf (process-buffer http))
    (packagesL nil))
      (process-send-string
	   http (concat "GET /" path " HTTP/1.0\r\n\r\n"))       
	  (while (eq (process-status http) 'open)
		(sleep-for 1))
      (insert-buffer pbuf)
      (kill-buffer pbuf)
      (goto-char (point-min))
      (while
		  (re-search-forward
		   "<a href=\"\\(.*\\)\">\\(.*\\.el\\)</a> *--- *\\(.*\\)<br>\nContact: *\\(.*\\)<br>" nil t) 
		(add-to-list 'packagesL (list (buffer-substring (match-beginning 1)(match-end 1))           
									  (buffer-substring (match-beginning 2)(match-end 2))
									  (buffer-substring (match-beginning 3)(match-end 3))
									  (buffer-substring (match-beginning 4)(match-end 4)))))       
	  packagesL)))
 
(define-derived-mode ell-mode text-mode "Ell"
  "Major mode to display the Emacs lisp list.
Special commands:
\\{ellmode-map}"
  (setq ell-font-lock-keywords
		(list
		 '("^\\*\\(.*\\.el\\) -" 1 font-lock-comment-face)     
		 '("^\\(.*\\.el\\) -" 1 font-lock-keyword-face)
		 )
		)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ell-font-lock-keywords nil t)))
 
(defun ell-packages ()
  "Display the Emacs Lisp list in a Emacs buffer."
  (interactive)
  (ignore-errors (kill-buffer "*ell-packages*"))
  (switch-to-buffer "*ell-packages*")
  (insert "==========================================")
  (center-line)
  (insert "\n")   
  (insert "The Emacs Lisp List")
  (center-line)
  (insert "\n")   
  (insert "by Stephen Eglen: stephen@anc.ed.ac.uk")
  (center-line)
  (insert "\n")   
  (insert "==========================================")
  (center-line)
  (insert "\n\n")   
  (insert "Note: File with an asterisk (*) are already installed on your system.\n\n")   
  (mapcar (lambda (x)
			(insert (format "%s - %s (by %s)\n%s\n\n"
							(let ((name (cadr x)))
							  (if (locate-library name)
								  (concat "*" name)
								name))
							(caddr x)
							(cadddr x)
							(car x))))
		  (reverse (ell-packages-list)))
  (ell-mode)
  (goto-char (point-min)))

(provide 'ell)
 
;;; ell.el ends here


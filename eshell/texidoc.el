;;; texidoc --- process embedded texinfo docs in source files

;; Copyright (C) 1999, 2000 Free Sofware Foundation

;; Author: John Wiegley <johnw@gnu.org>
;; Created: 20 Jul 1999
;; Version: 1.0
;; Keywords: maint, tex, docs
;; X-URL: http://www.emacs.org/~johnw/emacs.html

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; History:

;;; Code:

(defconst texidoc-version "1.0"
  "This version of texidoc.")

(defgroup texidoc nil
  "Emebbed Texinfo keeps the documentation closer to the code, and
allows the comments to serve as both."
  :tag "Embedded Texinfo"
  :group 'texinfo)

;;; User Variables:

(defcustom texidoc-load-hook nil
  "*A hook that gets run after \"texidoc.el\" has been loaded."
  :type 'hook
  :group 'texidoc)

;;; Internal Variables:

(defvar texidoc-last-output-begin)

;;; User Functions:

(defun texidoc-convert-text (&optional fill-p)
  "Convert textual notations from point onward.
Must leave point beyond the modified text.
The conversions are:

   \"foo\"  ->  ``foo''
   `foo'  ->  @code{foo}
   'foo'  ->  @samp{foo}
   _foo_  ->  @emph{foo}
   *foo*  ->  @strong{foo}
   <foo>  ->  @file{foo}
   [foo]  ->  @xref{foo}
   {foo}  ->  @footnote{foo}
    --    ->  ---
   {      ->  @{
   }      ->  @}
   @      ->  @@

   indent by two spaces produces an example
	  by three spaces produces a quotation"
  (let ((begin (point)) start intable)
    (while (re-search-forward "^@ " nil t)
      (forward-line 1)
      (if intable
	  (replace-match "@item\n")
	(replace-match "@itemize @bullet\n@item\n")
	(setq intable t))
      (forward-line 1)
      (save-excursion
	(setq
	 intable
	 (catch 'found
	   (while t
	     (cond
	      ((looking-at "@ ")
	       (throw 'found t))
	      ((or (eobp)
		   (looking-at "\\(@[^ ]\\|\\S-\\)"))
	       (and (not (eobp))
		    (forward-char -1))
	       (insert "@end itemize\n\n")
	       (throw 'found nil))
	      ((looking-at " +")
	       (replace-match "")))
	     (forward-line 1))))))
    (goto-char begin)
    (while (not (eobp))
      (when (looking-at "^  ")
	(setq start (point))
	(while (not (or (eobp)
			(looking-at "^\\(\\S-\\|$\\)")))
	  (delete-char 2)
	  (if (= start (point))
	      (insert "@example\n"))
	  (while (re-search-forward "[@{}]" (line-end-position) t)
	    (save-excursion
	      (goto-char (match-beginning 0))
	      (insert "@")))
	  (forward-line 1))
	(insert "@end example\n")
	(put-text-property start (- (point) 2) 'example-text t))
      (forward-line 1))
    (goto-char begin)
    (while (and (re-search-forward
		 ;; Backslash Fest '99
		 "\\\\\\\\\\[\\([^]]+\\)\\]" nil t)
		(not (get-text-property (match-beginning 0)
					'example-text)))
      (let* ((name (match-string 1))
	     (sym (intern-soft name))
	     (keys (and sym (where-is-internal sym)))
	     (desc (and keys (mapconcat 'key-description keys " "))))
	(replace-match
	 (concat "<" (or desc (concat "M-x " name)) ">"))))
    (while (and (re-search-forward
		 "\\\\{\\([^}]+\\)}" nil t)
		(not (get-text-property (match-beginning 0)
					'example-text)))
      (replace-match ""))
    (goto-char begin)
    (while (and (re-search-forward "\\(^\\| \\)--\\( \\|$\\)" nil t)
		(not (get-text-property (match-beginning 0)
					'example-text)))
      (replace-match "---"))
    (goto-char begin)
    (while (and (re-search-forward "\\.\\.\\.\\( \\|$\\)" nil t)
		(not (get-text-property (match-beginning 0)
					'example-text)))
      (replace-match
       (concat "@" (and (= (length (match-string 1)) 0) "end")
	       "dots{}\\1")))
    (goto-char begin)
    (while (and (re-search-forward "`\\([^']+\\)'" nil t)
		(not (get-text-property (match-beginning 0)
					'example-text)))
      (replace-match "@code{\\1}" t))
    (goto-char begin)
    (while (re-search-forward "\\(^\\|[ \t.]\\)\\([['_*^<{]\\)" nil t)
      (unless (get-text-property (match-beginning 0)
				 'example-text)
	(let ((char (string-to-char (match-string 2)))
	      (here (point-marker)) there)
	  (replace-match "\\1")
	  (re-search-forward
	   (cond ((eq char ?\') "'")
		 ((eq char ?_) "_")
		 ((eq char ?*) "*")
		 ((eq char ?^) "\\^")
		 ((eq char ?\<) ">")
		 ((eq char ?\{) "}")
		 ((eq char ?\[) "]")))
	  (replace-match "}")
	  (setq there (point-marker))
	  (goto-char here)
	  (insert
	   (cond ((eq char ?\') "@samp{")
		 ((eq char ?_) "@emph{")
		 ((eq char ?*) "@strong{")
		 ((eq char ?^) "@uref{")
		 ((eq char ?\<) "@kbd{")
		 ((eq char ?\{) "@footnote{")
		 ((eq char ?\[) "@ref{"))))))
    (goto-char begin)
    (let (odd)
      (while (and (re-search-forward "\"" nil t)
		  (not (get-text-property (match-beginning 0)
					  'example-text)))
	(replace-match (if odd "''" "``"))
	(setq odd (not odd))))
    (goto-char (point-max))
    (and fill-p (fill-region begin (point)))))

(defun texidoc-current-buffer (buffer &optional initial-p)
  "Process embedded Texinfo text in the current buffer.
The extracted code will be appended to BUFFER."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (cond
       ((looking-at ";;;\\(_.*\\| Commentary:\\|\\)\n\n?;")
	(if (string= (match-string 1) " Commentary:")
	    (forward-line 1))
	(let ((begin (point)))
	  (forward-paragraph 1)
	  (append-to-buffer buffer begin (point))
	  (with-current-buffer buffer
	    (goto-char texidoc-last-output-begin)
	    (while (and (re-search-forward
			 "\\(^;;;_\\s-*[*+-]\\|^;+\\( \\|$\\)\\)" nil t)
			(not (get-text-property (match-beginning 0)
						'example-text)))
	      (replace-match
	       (cond ((string= (match-string 1) ";;;_*")
		      "\n@section")
		     ((string= (match-string 1) ";;;_ +")
		      "\n@subsection")
		     ((string= (match-string 1) ";;;_ -")
		      "\n\@subsubsection")
		     (t ""))))
	    (goto-char texidoc-last-output-begin)
	    (texidoc-convert-text); t)
	    (setq texidoc-last-output-begin (point)))))
       ((looking-at (concat ";;;\\(###autoload\\|doc\\)\n"
			    "(defun\\s-*\\(\\S-+\\)\\s-*"))
	(let ((text (match-string 2)))
	  (with-current-buffer buffer
	    (insert "\n@deffn {Command} " text " "))
	  (goto-char (match-end 0))
	  (while (not (looking-at ")"))
	    (forward-char 1)
	    (unless (looking-at ")")
	      (let ((beg (point)) end-p)
		(forward-sexp 1)
		(setq text
		      (buffer-substring beg (point))
		      end-p (looking-at ")"))
		(with-current-buffer buffer
		  (if (or (string= text "&optional")
			  (string= text "&rest"))
		      (insert text " ")
		    (insert "@var{" text "}"
			    (if end-p "" " ")))))))
	  (with-current-buffer buffer
	    (insert "\n"))
	  (re-search-forward "^\\s-+\"")
	  (forward-char -1)
	  (let ((beg (1+ (point)))
		(text (read (current-buffer))))
	    (with-current-buffer buffer
	      (setq texidoc-last-output-begin (point))
	      (insert text "\n@end deffn\n\n")
	      (goto-char texidoc-last-output-begin)
	      (texidoc-convert-text)
	      (setq texidoc-last-output-begin (point))))))
       ((looking-at "(defcustom\\s-*\\([^ \n]+\\)")
	(let ((text (match-string 1)))
	  (unless (save-match-data
		    (string-match "-load-hook\\'" text))
	    (with-current-buffer buffer
	      (insert "\n@defvr {User Option} " text "\n")))
	  (read (current-buffer))
	  (unless (save-match-data
		    (string-match "-load-hook\\'" text))
	    (with-current-buffer buffer
	      (setq texidoc-last-output-begin (point))
	      (insert (substring (get (intern-soft text)
				      'variable-documentation) 1)
		      "\n@end defvr\n\n")
	      (goto-char texidoc-last-output-begin)
	      (texidoc-convert-text)
	      (setq texidoc-last-output-begin (point))))))
       ((looking-at "(defgroup\\s-*\\([^ \n]+\\)")
	(let ((group-sym (intern-soft (match-string 1))))
	  (read (current-buffer))
	  (if (get group-sym 'custom-tag)
	      (with-current-buffer buffer
		(setq texidoc-last-output-begin (point))
		(if initial-p
		    (progn
		      (insert (get group-sym 'group-documentation) "\n\n")
		      (insert "@chapter " (get group-sym 'custom-tag) "\n\n"))
		  (insert "@chapter " (get group-sym 'custom-tag) "\n\n")
		  (insert (get group-sym 'group-documentation) "\n"))
		(goto-char texidoc-last-output-begin)
		(texidoc-convert-text)
		(setq texidoc-last-output-begin (point))))))
       ((looking-at ";;; Code:")
	(goto-char (point-max))))
      (forward-line 1))))

(defun texidoc-file (infile buffer &optional initial-p)
  "Process embedded Texinfo from INFILE, appending it to BUFFER."
  (let ((inbuf (find-file-noselect infile)))
    (save-excursion
      (set-buffer buffer)
      (texinfo-mode)
      (goto-char (point-max))
      (set-buffer inbuf)
      (eval-buffer)
      (goto-char (point-min))
      (texidoc-current-buffer buffer initial-p))))

(defun texidoc-files (produce-menu-func file &rest files)
  "Process Texinfo docs from FILES, append to the last entry."
  (let* ((target (find-file-noselect (car (last files)))))
    (save-excursion
      (set-buffer target)
      (erase-buffer)
      (insert-file-contents file)
      (re-search-forward "%%")
      (narrow-to-region (match-beginning 0) (match-end 0))
      (backward-delete-char 2)
      (set (make-local-variable 'texidoc-last-output-begin) (point))
      (insert "@c auto-extracted text follows, scanned by texidoc.el\n\n")
      (message "Starting production of Texinfo documentation")
      (let ((initial-p t))
	(while (cdr files)
	  (unless (string= (car files) "texidoc.el")
	    (message "Processing file: %s" (car files))
	    (texidoc-file (car files) target initial-p))
	  (setq files (cdr files)
		initial-p nil)))
      (widen)
      (when produce-menu-func
	(message "Formatting Texinfo")
	(texinfo-insert-node-lines (point-min) (point-max) t)
	(texinfo-every-node-update)
	(texinfo-all-menus-update)
	(texinfo-every-node-update)
	(texinfo-all-menus-update)
	(goto-char (point-min))
	(re-search-forward "^@menu$")
	(let ((begin (line-beginning-position)))
	  (re-search-forward "^@end menu$")
	  (delete-region begin (point))
	  (funcall produce-menu-func)
	  (narrow-to-region begin (point))
	  (goto-char (point-min))
	  (while (re-search-forward "^\\* " nil t)
	    (add-text-properties (match-beginning 0)
				 (match-end 0)
				 '(example-text t)))
	  (goto-char (point-min))
	  (texidoc-convert-text)
	  (widen)))
      (message "Finished")
      (save-buffer))))

(defun batch-texidoc-file ()
  "usage: emacs -batch -f batch-texidoc-file FILE... OUTPUT-FILE"
  (texidoc-files nil command-line-args-left))

;;; Internal Functions:

(provide 'texidoc)

(run-hooks 'texidoc-load-hook)

;;; texidoc.el ends here

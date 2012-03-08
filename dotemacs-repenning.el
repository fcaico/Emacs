;; -*- Mode: Emacs-Lisp -*-

;; Time-stamp: <97/09/09 11:55:44 jackr>

;; (setq debug-on-error t)

(setq byte-compile-warnings (list ;; 'free-vars
				  'unresolved
				  'callargs
				  'redefine
				  'obsolete
				  ))

;;{{{ User options settings

(setq load-path (append
		 (list (expand-file-name "~/emacs"))
		 load-path))

(if (and (string-match "19.34" emacs-version)
	 (string-lessp emacs-version "19.34.5"))
    (progn
      (setq file-name-buffer-file-type-alist
	    ;; Only specify those that are likely to fool auto-translate
	    ;; ". t"  means binary
	    ;; ". nil" or no "." means text
	    ;; List copied (with mods) from winnt.el
	    '(
	      ("[:/].*config.sys$" . nil) ; config.sys text
	      ("\\.elc$" . t)		; emacs stuff
	      ("\\.\\(obj\\|exe\\|com\\|lib\\|sys\\|chk\\|bin\\|ico\\|pif\\)$" . t)
	      ;;("\\.\\(obj\\|exe\\|com\\|lib\\|sys\\|chk\\|out\\|bin\\|ico\\|pif\\)$" . t)
					; MS-Dos stuff
	      ("\\.\\(arc\\|zip\\|pak\\|lzh\\|zoo\\)$" . t)
					; Packers
	      ("\\.\\(a\\|o\\|tar\\|z\\|gz\\|taz\\)$" . t)
					; Unix stuff
	      ("\\.tp[ulpw]$" . t)
					; Borland Pascal stuff
	      )
	    )
      (if nil				; What auto-translate then does:
	  (setq file-name-buffer-file-type-alist
		(append file-name-buffer-file-type-alist
			'(("" . check-buffer-file-type))))
	)
      (require 'auto-translate)))

(require 'gnuserv)
(gnuserv-start)

(require 'info)

(setq
 Info-enable-edit 			t
 Manual-buffer-view-mode		'other
 Manual-unique-man-sections-only	nil
 Manual-match-topic-exactly		t
 ange-ftp-tmp-name-template		(if (and window-system
						 (eq window-system 'win32))
					    "c:/temp/ange-ftp"
					  "/tmp/ange-ftp")
 backup-by-copying-when-linked		t
 completion-ignore-case 		nil
 default-major-mode 			'text-mode
 display-time-day-and-date 		t
 ediff-window-setup-function		'ediff-setup-windows-plain
 enable-local-eval			t
 enable-recursive-minibuffers		t
 fill-column				70
 find-file-compare-truenames		t
 goal-column 				nil
 inhibit-startup-message 		t
 ispell-silently-savep			t
 kept-new-versions 			5
 kept-old-versions 			5
 line-number-mode			t
 minibuffer-confirm-incomplete		t
 minibuffer-max-depth 			nil
 parens-require-spaces			nil ; I don't like this
 require-final-newline			t
 server-kill-quietly			t
 sh-mode-indent 			8
 shell-cd-regexp 			"cdl?"
 shell-multiple-shells			t
 shell-prompt-pattern			"^[^ ]+[>%#:][ \t]*"
 tags-build-completion-table		t
 toolbar-mail-command			'identity;; 'my-mh-rmail
 toolbar-use-separate-mail-frame	t
 track-eol 				t
 delete-old-versions	 		t
 win32-recognize-altgr			nil ;let alt be alt
 )

(setq auto-mode-alist
    (append '(
	      ("\\.yum$"	.	c++-mode)
	      ("\\.c\\+\\+$"	.	c++-mode)
	      ("\\.cxx$"	.	c++-mode)
	      ("\\.cc$"		.	c++-mode)
	      ("\\.C$"		.	c++-mode)
	      ("\\.in$"		.	c++-mode)
	      ("\\.i$"		.	c++-mode)
	      ("\\.pdl$"	.	c++-mode)
	      ("\\.yuk$"	.	c-mode)
	      ("\\.rpc$"	.	c-mode)
	      ("\\.uil$"	.	c-mode)
	      ("\\.c$"  	. 	c-mode)
	      ("\\.h$"  	. 	c++-mode)
	      ("\\.a$"		.	ada-mode)
	      ("\\.pl$"		.	perl-mode)
	      ("\\.ms$"		.	nroff-mode)
	      ("\\.mm$"		.	nroff-mode)
	      ("\\.dm$"		.	nroff-mode)
	      ("\\.man$"	.	nroff-mode)
	      ("\\.nr$"		.	nroff-mode)
	      ("\\.tr$"		.	nroff-mode)
	      ("\\.emacs$"	.	emacs-lisp-mode)
	      ("\\.sh$"		.	ksh-mode)
	      ("make_[a-z]*$"	.	ksh-mode)
	      ("makeclient$"	.	ksh-mode)
	      ("makelibdirs$"	.	ksh-mode)
	      ("makeoptical$"	.	ksh-mode)
	      ("makesql$"	.	ksh-mode)
	      ("/setup\\."	.	ksh-mode)
	      ("\\.tmpl$"	.	makefile-mode)
	      ;;("\\.html$"       .	html-helper-mode)
	      )
	    auto-mode-alist))

(setq Info-directory-list (append
			   (list "/usr/src/gnu/info/")
			   Info-directory-list))

(display-time)

;;}}}
;;{{{ Function definitions

;;{{{ Hooks
;;{{{ my-cc-c++-hook

(defun my-cc-c++-hook ()

  (auto-fill-mode 1)

  (if (and buffer-file-name
	   (string-match ".*/var/tmp/cvintp[0-9]+/"
			 (file-name-directory buffer-file-name))
	   (save-excursion
	     (goto-char (point-min))
	     (looking-at "{")))
      (save-excursion
	(add-hook 'local-write-file-hooks 'cream-f+c-marker)
	(add-hook (make-local-variable 'after-save-hook) 'stuff-f+c-marker))))

;;}}}
;;{{{ my-dired-mode-hook

(defun my-dired-mode-hook ()
  "Bind some private functions."
  (setq case-fold-search t)		;because the mode overrides
  )

;;}}}
;;{{{ my-folder-mode-hook

(defconst my-mh-folder-bindings
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-s" 'mh-sort-folder)
    (define-key m "\M-\C-s" 'mh-search-folder)
    m)
  "Keybindings that I set when in mh-folder-mode")

(defun my-folder-mode-hook ()
  "Hook for entry to MH folder mode."
  (cond
   ((string-match "XEmacs" (emacs-version))
    (set-keymap-parents my-mh-folder-bindings mh-folder-mode-map)
    (use-local-map my-mh-folder-bindings)
    (set-specifier scrollbar-height (cons (current-buffer) 16))

    )
   (t
    (local-set-key "\M-s" 'mh-sort-folder)
    (local-set-key "\M-\C-s" 'mh-search-folder)))
  (local-set-key "!" 'jr-mh-refile-or-write-appropriately))

;;}}}
;;{{{ my-ksh-mode-hook

(defun my-ksh-mode-hook nil
  (setq compile-command (concat (file-name-nondirectory
				 (or (buffer-file-name)
				     "/bin/false"))
				" -n"))
  (font-lock-mode 1)             ;; font-lock the buffer
  (setq ksh-indent 4)
  (setq ksh-group-offset -4)
  (setq ksh-brace-offset -4)   
  (setq ksh-tab-always-indent t)
  (setq ksh-match-and-tell t)
  (setq ksh-align-to-keyword t)	;; Turn on keyword alignment
)

;;}}}
;;{{{ my-lisp-hook

(defun my-lisp-hook ()
  ;;(local-set-key "\033q" 'indent-sexp)
  ;;(local-set-key "\033j" 'indent-sexp)
  (local-set-key "\M-\C-d" 'edebug-defun)
  ;;(local-set-key "\M-\C-r" 'eval-region)
  (local-set-key "\C-x\C-e" 'byte-compile-file)
  (make-local-variable 'edebug-all-defuns) ; when set, eval-region edebugs
  (auto-fill-mode 1))

;;}}}
;;{{{ my-lisp-interaction-mode-hook

(defun my-lisp-interaction-mode-hook ()
  (local-set-key "\r" 'newline-and-indent))

;;}}}
;;{{{ my-makefile-mode-hook

(defvar makefile-mkdep nil
  "Buffer-local stack of mkdep data in makefiles.")
(make-variable-buffer-local 'makefile-mkdep)
;;(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)
(defun my-makefile-mode-hook ()
  "Hide mkdep stuff"
  (if (save-excursion
	(goto-char (point-min))
	(re-search-forward "^# DO NOT DELETE THIS LINE" nil t))
      (save-excursion
	(goto-char (point-min))
	(re-search-forward "^# DO NOT DELETE THIS LINE" nil t)
	(forward-line 2)
	(setq makefile-mkdep
	      (buffer-substring (point) (point-max)))
	(let (buffer-read-only)
	  (setq buffer-read-only nil)
	  (delete-region (point) (point-max))
	  (set-buffer-modified-p nil)
	  )

	(add-hook 'local-write-file-hooks 'my-makefile-write-hook)

	;; No, it's not redundant.  The purpose is to move it to the
	;; front, if it's not there already, so it can run soon enough
	;; to prevent poor func-menu from indexing all those mkdep
	;; entries.
	(if (not (eq (car makefile-mode-hook) 'my-makefile-hook))
	    (add-hook 'makefile-mode-hook 'my-makefile-mode-hook))))
  )

(defun my-makefile-write-hook ()
  "Restore mkdep stuff."
  (if makefile-mkdep
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "^# DO NOT DELETE THIS LINE" nil t)
	    (let ((bm (buffer-modified-p))
		  font-lock-mode)

	      (forward-line 2)
	      (if (or (= (point) (point-max))
		      (y-or-n-p "Replace with original mkdep stuff? "))
		  (progn
		    (delete-region (point) (point-max))
		    (insert makefile-mkdep)

		    (set-buffer-modified-p bm)
		    )
		)
	      )
	  )
	(add-hook 'after-save-hook 'my-makefile-mode-hook)
	)
    )
  nil)

;;}}}
;;{{{ my-nroff-mode-hook

(defun my-nroff-mode-hook ()
  "Sets outline minor mode and outline-regexp."
  ;;(outline-minor-mode 1)
  (setq outline-regexp "\\.H\\s +[0-9]"))

;;}}}
;;{{{ my-perl-mode-hook

(defun my-perl-mode-hook ()
  (interactive)
  (modify-syntax-entry ?' "_")
  (local-set-key '(meta backspace) 'backward-kill-word)
  (set (make-local-variable 'compile-command) (file-name-nondirectory
					       buffer-file-name))
  (set (make-local-variable 'auto-fill-inhibit-regexp)
       "[^#\n]*\\(\".*\"[^#\n]*\\)*\"")
  (auto-fill-mode 1))

;;}}}
;;{{{ my-sh-mode-hook
(defun my-sh-mode-hook nil
  "Shell mode?  Who cares about *shell* mode???"
  (ksh-mode))

;;}}}
;;{{{ my-text-hook

(defun my-text-hook ()
  (auto-fill-mode 1)
  (make-local-variable 'indent-line-function)
  (local-set-key "\C-?" 'backward-delete-char-untabify)
  (local-set-key "\M-\C-q" 'quote-paragraph)
  (local-set-key "\C-i" 'indent-relative)
  (local-set-key "\M-\C-r" 'append-rtn-addr-to-paragraph)
  (setq indent-line-function 'indent-relative-maybe)
  (setq paragraph-separate "^$\\|^\\|^ *$\\|^-+$"
	paragraph-start paragraph-separate)
  (setq fill-column 64)
  (modify-syntax-entry ?\" "\""))

;;}}}
;;}}}

;;{{{ Hook settings

(add-hook 'ada-mode-hook 'ada-mode-hook)
(add-hook 'archie-display-hook 'server-make-window-visible)
(add-hook 'archie-load-hook 'archie-load-hook)
(add-hook 'c++-mode-hook 'my-cc-c++-hook)
(add-hook 'c-mode-hook 'my-cc-c++-hook)
(add-hook 'dired-mode-hook 'my-dired-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
(add-hook 'hide-ifdef-mode-hook 'my-hide-ifdef-mode-hook)
(add-hook 'ksh-mode-hook 'my-ksh-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-interaction-mode-hook)
(add-hook 'lisp-mode-hook 'my-lisp-hook)
(add-hook 'mh-folder-mode-hook 'my-folder-mode-hook)
(add-hook 'nroff-mode-hook 'my-nroff-mode-hook)
(add-hook 'perl-mode-hook 'my-perl-mode-hook)
(add-hook 'sh-mode-hook 'my-sh-mode-hook)
(add-hook 'text-mode-hook 'my-text-hook)
(add-hook 'write-file-hooks 'time-stamp)

;;}}}

;;{{{ Source code editing

;;{{{ c-comment-ident

(defun c-comment-ident (arg)
  "Comment out a c identifier (or, arg contiguous ones)."
  (interactive "*P")
  (or (looking-at "\\b")
      (backward-sexp))
  (insert "/*")
  (forward-sexp arg)
  (insert "*/"))

;;}}}
;;{{{ clearmake-filter

(defun clearmake-filter (process output)
  "Insert and clean up clearmake output into *compilation*"
  (if (and process (process-status process))
      (save-excursion
	(set-buffer "*compilation*")
	(let* ((start (if (= (point) (point-max)) -1 (point))))
	  (unwind-protect
	      (progn
		(toggle-read-only -1)
		(goto-char (point-max))
		(save-excursion (insert output))
		(while (re-search-forward "\e\\[[0-9]*m" nil t)
		  (delete-region (match-beginning 0) (match-end 0))))
	    (goto-char (if (= -1 start) (point-max) start))
	    (toggle-read-only 1))))))

;;}}}
;;{{{ indent-includes

(defun indent-includes ()
  "Indents ``list-matching-lines ^#'' lines from *.in files."
  (interactive)
  (beginning-of-line)
  (re-search-forward "line 1\\s +\"")
  (beginning-of-line)
  (let ((including-file
	 (save-excursion
	   (re-search-backward "line [0-9]+ \"" nil t)
	   (goto-char (match-end 0))
	   (buffer-substring
	    (point)
	    (1- (progn (search-forward "\"") (point)))))))
    (save-restriction
      (save-excursion
	(narrow-to-region
	 (point)
	 (save-excursion
	   (search-forward including-file) (end-of-line 0) (point)))
	(replace-regexp "^" " ")))
    (forward-line 1)))

;;}}}
;;{{{ insert-braces

;;; Dave Brennan <brennan@hal.com> has a copy; might as well forward
;;; him any improvements as well!
(defun insert-braces ()
  "Insert matched braces, leave point inside."
  (interactive "*")
  (let (blink-paren-function) ;nil it temporarily
    (execute-kbd-macro
     (if (and (eq major-mode 'cc-c++-mode) (not (looking-at ";")))
	 "{};" "{}")))
  (forward-char (if (and (eq major-mode 'cc-c++-mode) (not (looking-at ";")))
		    -2 -1))
  (if (save-excursion
	(forward-char -2)
	(looking-at "\\$"))
      nil
    (save-excursion (reindent-then-newline-and-indent))
    (newline-and-indent)))

;;}}}

;;}}}

;;{{{ Display property fiddling

;;{{{ my-mh-highlight-headers

;; added "mailto"
(defconst highlight-headers-url-pattern
  (concat "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|mailto\\):"
	  "\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?"
	  "[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]+"
	  ))

(defun my-mh-highlight-headers ()
  "Hook function"
  (interactive)
  (save-window-excursion
    (if (eq major-mode 'mh-folder-mode)
	(other-window 1))
    (highlight-headers (point-min) (point-max) nil)
    (setq truncate-lines (setq truncate-partial-width-windows nil))))

;;}}}
;;{{{ toggle-line-wrap

(defun toggle-line-wrap ()
  "Toggles the line-wrap (or line-truncate, depending on your
perspective) function.  Covers (and equates) both horizontal and
vertical splits."
  (interactive)
  (setq truncate-lines (setq truncate-partial-width-windows (not
							     truncate-lines)))
  (recenter (- (count-lines (window-start) (point))
	       (if (bolp) 0 1)))
  )

;;}}}

;;}}}

;;{{{ Script creation and debugging

;;{{{ make-perl-script

(defun make-perl-script (arg)
  "Make the current buffer into a perl script.  With arg, nukes first."
  (interactive "*P")
  (cond
   ((or arg (= (- (point-min) (point-max)) 0))
    (erase-buffer)
    (insert-file "~/bin/debug.pl")))
  (save-excursion
    (shell-command (concat "chmod +x " (buffer-file-name))))
  (if (search-forward "gud-perldb-history: " nil t)
      (insert (concat "(\"perl "
		      (file-name-nondirectory (buffer-file-name))
		      "\")")))
  (save-buffer)
  (shell-command (concat "chmod a+x " (buffer-file-name)))
  (find-alternate-file (buffer-file-name)) ; set mode, fontification, etc.
  (beginning-of-buffer)
  (search-forward "usage=\"Usage: $0 \[-$flags\]" nil t))

;;}}}
;;{{{ make-shell-script

(defun make-shell-script (arg)
  "Make the current buffer into a ksh script.  With arg, nukes first."
  (interactive "*P")
  (cond
   ((or arg (= (- (point-min) (point-max)) 0))
    (erase-buffer)
    (insert-file "~/bin/debug.ksh")
    (if (search-forward "#	eval: (if (fboundp 'sh-mode) (sh-mode))\n" nil t)
	(insert
	 "#	compile-command: \""
	 (file-name-nondirectory (buffer-file-name))
	 " -n \"\n"))))
  (save-buffer)
  (shell-command (concat "chmod a+x " (buffer-file-name)))
  (find-alternate-file (buffer-file-name)) ; set mode, fontification, etc.
  (beginning-of-buffer)
  (search-forward "usage=\"Usage: $0 \[-$flags\]" nil t))

;;}}}
;;{{{ pdb-down-to-here

(defun pdb-down-to-here (command-line)
  "Launch perl debugger, proceeding down to current line."
  (interactive
   (list (read-from-minibuffer "Run perldb (like this): "
			       (if (and (require 'gud)
					(consp gud-perldb-history))
				   (car gud-perldb-history)
				 "perl ") 
			       nil nil
			       '(gud-perldb-history . 1))))
  (require 'gud)
  (make-local-variable 'gud-perldb-history)
  (let ((buf (current-buffer))
	(line (+ (if (bolp) 1 0)
		 (count-lines (point-min) (point)))))
    (save-some-buffers t)
    (let ((binary-process-output (memq system-type '(windows-nt dos))))
      (perldb command-line))		; changes current buf
    (while (save-excursion
	     (goto-char (point-max))
	     (beginning-of-line)
	     (not (looking-at " *DB<1>")))
      (sit-for 1)
      (accept-process-output)
      (if (save-excursion
	    (goto-char (point-max))
	    (beginning-of-line 0)
	    (looking-at "Debugger exited abnormally.*"))
	  (progn
	    (pop-to-buffer buf)
	    (error (buffer-substring (match-beginning 0) (match-end 0))))))
    (goto-char (point-max))		; ... but doesn't do this, oddly
    (insert "c " (number-to-string (+ line (if (bolp) 1 0))))
    (comint-send-input)))

;;}}}

;;}}}

;;{{{ Misc

;;{{{ insert-time-stamp

(defun insert-time-stamp ()
  "Insert current date and time."
  (interactive "*")
  (insert (current-time-string)))

;;}}}
;;{{{ kill-buffer-other-window

(defun kill-buffer-other-window (arg)
  "Kill the buffer in the ARGth other window, or the current buffer if no
other window."
  (interactive "p")
  (let ((buf (save-window-excursion
	       (other-window arg)
	       (current-buffer))))
    (delete-windows-on buf)
    (kill-buffer buf))
  )

;;}}}
;;{{{ my-save-buffer-kill-frame

(defun my-save-buffer-kill-frame (arg)
  "Saves buffer, if necessary (with ARG, w/o asking), and then kills
it and its frame."
  (interactive "P")
  (let ((buf (current-buffer))
	(delete-frame nil)
	(kill-buffer nil))
    (if (and (buffer-file-name buf)
	     (buffer-modified-p)
	     (or arg (y-or-n-p (format "Save buffer %s? " (buffer-name)))))
	(save-buffer nil))
    (setq kill-buffer (or (not (buffer-modified-p buf))
			  (not (buffer-file-name buf))
			  (yes-or-no-p (concat "Buffer "
					       (buffer-name buf)
					       " modified; kill anyway? "))))
    (setq delete-frame (if (and (one-window-p)
				(or arg
				    (unwind-protect
					(y-or-n-p "Delete frame as well? ")
				      (message ""))))
			   (selected-frame)
			 nil))
    (delete-windows-on buf)
    (if kill-buffer (progn (if (string-match "XEmacs" (emacs-version))
			       (set-buffer-modified-p nil buf)
			     (save-excursion
			       (set-buffer buf)
			       (set-buffer-modified-p nil)))
			   (kill-buffer buf)))
    (and delete-frame (delete-frame))))

;;}}}
;;{{{ my-set-sound-defaults

(defun my-set-sound-defaults (vol)
  (interactive "nSet sounds to what volume? ")
  (mapcar
   (function (lambda (entry)
	       (let ((e (memq ':volume entry)))
		 (if e
		     (setcar (cdr (memq ':volume entry)) vol)
		   (nconc entry (list ':volume vol)))
		 (list (car entry) ':volume (car (cdr (memq ':volume entry)))))))
   sound-alist))

;;}}}
;;{{{ my-spell-word

(defun my-spell-word (arg)
  "Calls ispell-word, or with ARG, ispell-buffer."
  (interactive "P")
  (require 'ispell)
  (if arg
      (ispell-message)
    (ispell-word))
  )

;;}}}
;;{{{ my-substitute-key-definition

;; substitute-key-definition doesn't work in Xemacs
;;(defun my-substitute-key-definition (old new map)
;;  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
;;In other words, OLDDEF is replaced with NEWDEF where ever it appears.
;;Done by hand, because Xemacs pre-19.12 doesn't do the whole job. 
;;"
;;  (mapcar
;;   '(lambda (key) (define-key map key new))
;;   (where-is-internal old map)))

;;}}}
;;{{{ pp-window-configuration

(defvar pp-window-configuration ()
  "Holds the stack of former window configurations.  
Form: (list (list window-configuration point-in-current)
            ...)")

(defvar pp-max 10
  "*Maximum number of window configurations stored by pp-window-configuration.")


(defun pp-window-configuration (arg)
  "Save current window config (with ARG, restore; with ARG>=16,
flush)"
  (interactive "p")
  (cond
   ((and (numberp arg) (>= arg 16))
    (setq pp-window-configuration nil)
    (message "Window configuration stack cleared."))
   ((or (not (numberp arg)) (and (numberp arg) (> arg 1)))
    (setq pp-window-configuration
	  (append (list 
		   (list (current-window-configuration) (point)))
		  (if (>= (length pp-window-configuration) pp-max)
		      (reverse (cdr (reverse pp-window-configuration)))
		    pp-window-configuration))))
   (t
    (if (= 0 (length pp-window-configuration))
	(message "Window configuration stack is empty.")
      (let* ((ppwc (car pp-window-configuration))
	     (wc (nth 0 ppwc))
	     (p (nth 1 ppwc)))
	(setq pp-window-configuration (cdr pp-window-configuration))
	(set-window-configuration wc)
	(goto-char p))))))

;;}}}
;;{{{ remove-alist-name

(defun remove-alist-name (name alist)
  "Removes element whose car is NAME from ALIST."
  (cond ((equal name (car (car alist)))
	 (cdr alist))
	((null alist)
	 nil)
	(t
	 (cons (car alist)
	       (remove-alist-name name (cdr alist))))))

;;}}}
;;{{{ set-vectra-keys

(defun set-vectra-keys ()
  "Set up key bindings needed when using PCTPC (or whoever's sending
the wrong key codes."
  (interactive)
  ;; key labelled "Insert"
  (global-set-key [ find ] 'fkey-overwrite-mode)
  ;; key labelled "Delete"
  (global-set-key [ select ] 'backward-delete-char-untabify)
  ;; key labelled "Home"
  (global-set-key [ insert ] 'beginning-of-buffer)
  ;; key labelled "End"
  (global-set-key [ prior ] 'end-of-buffer)
  ;; key labelled "Page Up" 
  (global-set-key "\C-[[3~" 'fkey-scroll-down)
  ;; key labelled "Page Down"
  (global-set-key [ next ] 'fkey-scroll-up)
  )

;;}}}
;;{{{ sum-column

(defun sum-column (start end arg)
  "Add up (presumed) numbers in the column defined by START and END.
Insert if ARG."
  (interactive "r\nP")
  (if (< end start) (let (tmp)
		      (setq tmp start)
		      (setq start end)
		      (setq end tmp)))
  (save-excursion
    (goto-char start)
    (let ((numcol (current-column))
	  (numend (save-excursion (goto-char end) (current-column)))
	  (sum 0))
      (while (< (point) end)
	(setq sum (+ sum (string-to-number
			  (buffer-substring (point)
					    (progn
					      (move-to-column numend t)
					      (point))))))
	(beginning-of-line 2)
	(move-to-column numcol t))
      (if arg (insert (number-to-string sum)))
      (message "Total: %.2f" sum))))

;;}}}
;;{{{ save-buffer-kill-buffer

(defun save-buffer-kill-buffer (arg)
  "Saves buffer, if necessary (with ARG, w/o asking), and then kills it."
  (interactive "P")
  (let ((buf (current-buffer)))
    (if (and (buffer-file-name buf)
	     (buffer-modified-p)
	     (or arg (y-or-n-p (format "Save buffer %s? " (buffer-name)))))
	(save-buffer nil)
      )
    (delete-windows-on buf)
    (kill-buffer buf)))

;;}}}
;;{{{ window-width
(defvar mh-scanline-length 200
  "*Allows mh-folder-mode scan lines to be long, scrollabe.")

(defadvice window-width (around ww-cheat-for-mh-folders activate)
  "If called within an mh-e folder window, claims very 
wide window."
  (if (eq major-mode 'mh-folder-mode)
      (setq ad-return-value mh-scanline-length)
    ad-do-it
    ))

;;}}}
;;{{{ xrdb

(defun xrdb ()
  "Display for edit the current X resources, saving into ~/.Xresources."
  (interactive)
  (pp-window-configuration t)
  (find-file "~/.Xresources")
  (delete-other-windows)
  (local-set-key "\C-c\C-c" 'reload-xrdb)

  ;; Set up comments appropriate for cpp
  ;; Note that white space can't be allowed between resource values
  ;; and the comment delimiter, or it will be included in the value
  ;; (producing things like "unrecognizable color name 'red       '")
  (modify-syntax-entry ?/ ". 14")
  (modify-syntax-entry ?* ". 23")
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-column)
  (setq comment-column 0) 
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")

  (message (substitute-command-keys "\\[reload-xrdb] to feed changes to xrdb"))
  )

;;}}}

;;}}}

;;}}}
;;{{{ Advice

(setq  ad-start-advice-on-load		t
       ad-activate-on-definition	t)

(require 'advice)

;;{{{ crontab

(defadvice crontab-get (before pp-for-crontab activate)
  "Stack window configuration"
  (pp-window-configuration t))

(defadvice crontab-save (after pp-for-crontab activate)
  "Unstack window configuration"
  (pp-window-configuration nil))

;;}}}
;;{{{ find-tag

(defadvice find-tag (before pp-for-tags activate)
  "Stack window configuration, as with function calls."
  (pp-window-configuration t))

;;}}}
;;{{{ mh-execute-commands

(defadvice mh-execute-commands (around mhec-show-protect activate)
  "Preserve showing state (should be fixed in next mh-e)"
  (let ((show-state mh-showing))
    ad-do-it
    (if show-state
	(mh-show)
      (mh-set-scan-mode))))

;;}}}
;;{{{ mh-get-new-mail

(cond
 ((string-match "XEmacs" (emacs-version))
  (defadvice mh-get-new-mail (after mgnm-clear-modeline activate)
    "Clear the ``Mail'' indication in the mode line."
    (require 'time)
    (display-time-function)))
 (t
  (defadvice mh-get-new-mail (after mgnm-clear-modeline activate)
    "Clear the ``Mail'' indication in the mode line."
    (autoload 'display-time-filter "time")
    (display-time-filter nil nil))
  ))

;;}}}
;;{{{ mh-maybe-show

(defun highlight-current-line (cache new-face)
  (interactive "SCache variable? 
SFace? ")
  (make-local-variable cache)
  (if (and cache (extentp (eval cache)))
      (set-extent-property (eval cache) 'face 'default))
  (save-excursion
    (set-extent-property
     (set cache (make-extent (progn (beginning-of-line)(point))
			     (progn (end-of-line) (point))))
     'face
     new-face)))

(defadvice mh-maybe-show (after highlight-mh-folder-line activate)
  "Select line for current message in folder mode window, so (if
  display is capapble) it's visually distinguished"
  (if mh-showing
      nil				; mh-show advice will get it
    (add-spec-list-to-specifier
     (face-property 'message-headers 'font)
     '((global
	(nil
	 .
	 "-adobe-courier-medium-r-normal--*-130-75-75-m-0-iso8859-1"))))
    (highlight-current-line 'highlighted-mh-folder-line 'message-headers)))

;;}}}
;;{{{ mh-rmail
(defadvice mh-rmail (before pp-for-mh-rmail activate)
  "Stack window configuration."
  (pp-window-configuration t))
;;}}}
;;{{{ mh-show

(defadvice mh-show (after highlight-mh-folder-line activate)
  "Select line for current message in folder mode window, so (if
  display is capapble) it's visually distinguished"
  (add-spec-list-to-specifier
   (face-property 'message-headers 'font)
   '((global
      (nil
       .
       "-adobe-courier-medium-r-normal--*-130-75-75-m-0-iso8859-1"))))
  (highlight-current-line 'highlighted-mh-folder-line 'message-headers))

;;}}}

;;}}}

;;}}}
;;{{{ Major packages and tweeks

;;{{{ auto-save
(cond
 ((string-match "XEmacs" (emacs-version))
  (require 'auto-save)
  (setq auto-save-directory (if (file-directory-p "/work/jackr/autosave")
				"/work/jackr/autosave"
			      "~/autosave"))))

;;}}}
;;{{{ autoinsert

(require 'autoinsert)			;needs my version
(setq auto-insert-directory "~/emacs-templates/")
(setq auto-insert-alist '(("\\.tex$" . "tex-insert.tex")
			  ("admin/weeklies/" . "weeklies-insert")))
(add-hook 'auto-insert-hooks 'my-auto-insert-hook)

(defun my-auto-insert-hook nil
  (cond
   ((string-match "admin/weeklies/[0-9]+$" (buffer-file-name))
    (goto-char (point-min))
    (search-forward "Weekly Status, ")
    (insert-time-stamp)
    (insert "\n\n")
    (insert-file-contents (car (last (directory-files "~/admin/weeklies"
						      t
						      "[0-9]+$"
						      nil
						      t))))
    (kill-region (point)
		 (progn
		   (search-forward "- Goals")
		   (beginning-of-line)
		   (point))))))

(defconst month-name-to-num-alist
  (list (cons "Jan" "01")
	(cons "Feb" "02")
	(cons "Mar" "03")
	(cons "Apr" "04")
	(cons "May" "05")
	(cons "Jun" "06")
	(cons "Jul" "07")
	(cons "Aug" "08")
	(cons "Sep" "09")
	(cons "Oct" "10")
	(cons "Nov" "11")
	(cons "Dec" "12"))
  "Alist mapping month name strings to month number.")

(defun make-weekly-status-report nil
  "Generate a weekly status report, based on last."
  (interactive)
  (let* ((date (current-time-string))	; Mon Apr 29 11:24:53 1996
	 (report-file-name (concat (substring date 22) ; year
				   (cdr-safe (assoc (substring date 4 7) ; mon
						    month-name-to-num-alist))
				   (substring date 8 10))) ;day
	 where)
    (while (setq where (string-match " " report-file-name))
      (aset report-file-name where ?0))
    (find-file-other-frame (concat "~/admin/weeklies/"
				   report-file-name))
    ))

;;}}}
;;{{{ c-mode, c++-mode

(autoload 'c-mode "cc-mode")
(autoload 'c++-mode "cc-mode")

;;}}}
;;{{{ crypt
(cond
 ((string-match "XEmacs" (emacs-version))
  (setq crypt-encryption-type 'pgp
	crypt-confirm-password nil
	)
  (require 'crypt)))

;;}}}
;;{{{ dabbrev

(setq dabbrev-case-replace nil)

;;}}}
;;{{{ efs
;;; (require 'efs)
;;; (setq
;;;  efs-binary-file-name-regexp ".*"
;;;  efs-default-user "ftp"
;;;  efs-ftp-program-name "rftp"
;;;  efs-pty-check-threshold nil
;;;  efs-cmd-ok-msgs  "^200 \\|^227 "
;;;  efs-cmd-ok-cmds (concat
;;; 		  "^quote port \\|^type \\|^quote site \\|^chmod \\|"
;;; 		  "^quote noop\\|^quote pasv")
;;;  efs-failed-msgs (concat
;;; 		  efs-failed-msgs
;;; 		  "[Nn]ot a plain file")
;;;  )
;;}}}
;;{{{ ehelp

(require 'ehelp)

;;(define-key global-map [delete] 'ehelp-command)

(substitute-key-definition 'help-command 'ehelp-command (current-global-map))
(substitute-key-definition 'command-apropos 'apropos (current-global-map))

(substitute-key-definition 'save-buffer-kill-buffer
			      'my-save-buffer-kill-frame
			      (current-global-map))

(substitute-key-definition 'save-buffer-kill-buffer
			      'save-buffers-kill-emacs
			      (current-global-map))

(substitute-key-definition 'save-buffers-kill-emacs
			      'my-save-buffer-kill-frame
			      (current-global-map))

;;}}}
;;{{{ filladapt

(cond
 ((string-match "XEmacs" (emacs-version))
  (require 'filladapt)
  (setq-default filladapt-mode t)))

;;}}}
;;{{{ folding

(require 'folding)

(fold-add-to-marks-list 'ksh "# {{{" "# }}}")

(defadvice fume-rescan-buffer (around unfold-around protect activate)
  "Unfold and refold around fume scans, so all functions are seen."
  (if (not folding-mode)
      ad-do-it
    (folding-mode)
    ad-do-it
    (folding-mode)))

;;}}}
;;{{{ func-menu

(cond
 ((string-match "XEmacs" (emacs-version))
  (require 'func-menu)
  ;;(define-key global-map 'f8 'function-menu)
  (add-hook 'find-file-hooks 'fume-add-menubar-entry)
  (define-key global-map "\C-cl" 'fume-list-functions)
  (define-key global-map "\C-cg" 'fume-prompt-function-goto)

  ;; For descriptions of the following user-customizable variables,
  ;; type C-h v <variable>
  (setq fume-max-items 25
	fume-fn-window-position 3
	fume-auto-position-popup t
	fume-display-in-modeline-p t
	fume-menubar-menu-location "Apps"
	fume-buffer-name " *Function List*"
	fume-no-prompt-on-valid-default nil)

  (defconst fume-function-name-regexp-lisp
    (concat
     "\\(^(defun+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
     "\\|"
     "\\(^(defsubst+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
     "\\|"
     "\\(^(defmacro+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
     "\\|"
     "\\(^(defadvice+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
     "\\|"
     "\\(^(de+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
     "\\|"
     "\\(^(dmd+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
     )
    "Expression to get lisp function names")))

;;}}}
;;{{{ ibuff-menu

(setq ibuff-initial-sublist-modes "fdpk"
      ibuff-bury-buffers-regexp (concat
				 "^\\("
				 (mapconcat
				  'identity
				  (list " .*"
					"sent .*"
					"\\*ftp .*"
					"\\*vc.*"
					"\\.ibuff-.*"
					"\\+.*"
					"\\*Help\\*"
					"\\*Directory\\*"
					"\\*Completions\\*"
					"\\*Dired log\\*"
					"\\*Compile-Log\\*"
					"\\.infonotes"
					"\\.newsrc")
				  "\\|")
				 "\\)$")
      ibuff-hide-buffers-regexp "^\\( \\|show-\\+\\|\\*info\\*\\|\\.bbdb\\|diary\\)"
      ibuff-preserve-match-and-mode t)
(autoload 'ibuff-menu "ibuff-menu" "Edit the buffer list." t)
(global-set-key "\C-x\C-b" 'ibuff-menu)

;;}}}
;;{{{ tabs-are-spaces

(defvar tabs-are-spaces nil
  "*If set, replace all tabs with equivalent spaces.  Automatically
buffer-local when set.")
(make-variable-buffer-local 'tabs-are-spaces)

(defadvice tab-to-tab-stop (around untabify-tab activate)
  "Advised to obey tabs-are-spaces"
  (if (not tabs-are-spaces)
      ad-do-it
    (let ((start (save-excursion
		   (skip-chars-backward "\\s ")
		   (point)))
	  end)
      ad-do-it
      (setq end (set-marker (make-marker) (1+ (point-marker))))
      (untabify start end)
      (goto-char (1- end)))))

(defadvice indent-relative (around untabify-tab activate)
  "Advised to obey tabs-are-spaces"
  (if (not tabs-are-spaces)
      ad-do-it
    (let ((start (save-excursion
		   (skip-chars-backward "\\s ")
		   (point)))
	  end)
      ad-do-it
      (setq end (set-marker (make-marker) (1+ (point-marker))))
      (untabify start end)
      (goto-char (1- end)))))

;;}}}
;;{{{ mh-e

(defun mh-smail-other-frame ()
  "mh-smail in a new frame.  Duh."
  (interactive)
  (save-window-excursion
    (mh-smail)
    (switch-to-buffer-other-frame (current-buffer))))

;;;; ------------------------------------------------------------
;;;; auto refile folder support.
;;;; ------------------------------------------------------------

(defvar mh-auto-folder-alist nil
  "*Non-nil value should be an alist that will be used to choose a default
folder name when messages are refiled.  The alist should be of the form
\((HEADER-NAME
   (REGEXP . FOLDER-NAME) ...
  ...))
where HEADER-NAME, REGEXP and FOLDER-NAME are strings.

If any part of the contents of the message header named by HEADER-NAME
is matched by the regular expression REGEXP, the corresponding FOLDER-NAME
will be used as the default when prompting for a folder to refile the message in.

Matching is case sensitive.")


(defun ange-mh-auto-select-folder ()
  "Return the name of a folder to refile the current message in."
  (require 'mh-comp)
  (let (case-fold-search
	header)
    (save-excursion
      (catch 'match
	(mapcar
	 (function (lambda (entry)
		     (setq header (mh-get-field (car entry)))
		     (if (zerop (length header))
			 ()
		       (mapcar
			(function (lambda (tuple)
				    (if (string-match (car tuple) header)
					(throw 'match (cdr tuple)))))
			(cdr entry)))))
	 mh-auto-folder-alist)
	(if (eq 'refile (car mh-last-destination))
	    (symbol-name (cdr mh-last-destination))
	    "")))))

(setq mh-msg-folder-hook 'ange-mh-auto-select-folder)
(setq mh-default-folder-for-message-function 'ange-mh-auto-select-folder
      my-auto-folder-list '(;; *first* match wins!
			    ("iml@" . "+magic")
			    ("NETRUNNER-L@ORACLE.WIZARDS.COM" . "+netrunner")
			    ("pgp-mime@" . "+pgp/mime")
			    ("@qualcomm" . "+eudora")
			    ("@cdnow.com" . "+cdnow")
			    ("reporter@" . "+emacs/reporter")
			    ("mcip@" . "+mcip")
			    ("patch-list@home.msr.hp.com" . "+hp-patches")
			    ("Majordomo@GreatCircle" . "+majordomo")
			    ("majordomo-users@GreatCircle" . "+majordomo")
			    ("mh-e@x.org" . "+emacs/mh-e")
			    ("PGP Key Service" . "+pgp/mitring")
			    ("pgp-public-keys@" . "+pgp/mitring")
			    ("URL-master" . "+URL-minder")
			    ("cciug" . "+cciug")
			    ("x-pgp" . "+pgp/emacs")
			    ("cm-beta" . "+cm/clearmake")
			    )
      mh-auto-folder-alist (list
			    (list "Subject"
				  (cons "Standings" "+magic")
				  (cons "Results" "+magic")
				  (cons "ClearCase license usage summary" 
					"+cclur")
				  (cons "backup" "+backups")
				  (cons "baseline/Menlo" "+builds")
				  (cons "BUILD FAILED" "+builds")
				  (cons "too slow" "+cm/oakland/tooslow")
				  )
			    (append (list "To")
				    my-auto-folder-list)
			    (append (list "Resent-To")
				    my-auto-folder-list)
			    (append (list "Cc")
				    my-auto-folder-list)
			    (append (list "From")
				    my-auto-folder-list)
			    (append (list "Sender")
				    my-auto-folder-list))

      )
;;;
;;; MH variables
;;;
(setq 
 mh-delete-yanked-msg-window		t
 mh-progs
     (if (file-executable-p "/usr/src/bin/")
	 "/usr/src/bin/"
       "/usr/local/bin")
 mh-lib
     (if (file-executable-p "/usr/src/lib/mh/")
	 "/usr/src/lib/mh/"
       "/usr/local/lib/mh/")
 mh-lpr-command-format			
     (if (file-executable-p "/usr/local/bin/a2ps")
	 "/usr/local/bin/a2ps -2 -l -s -f -i -r | lp -t'%s'"
       "lp -t'%s'")   
 mh-recursive-folders			t
 mh-redist-full-contents 		nil
 mh-summary-height			8 ; make tty-smart?
 mh-yank-from-start-of-msg		'body
 )

;;}}}
;;{{{ minibuf

(load "minibuf" 'missing-ok 'no-messages)

;;}}}
;;{{{ vc (the cc-vc one)

(load "vc-hooks")
;;(load"vc")
(setq vc-cleartool-binary "D:/atria/bin/cleartool.exe")
(setq vc-diff-on-checkin nil)

;;(defun vc-cleartool-blocking-call (arg)
;;  "Disable vc-cc until you can fix it."
;;  (interactive)
;;  (message "Skipping cleartool stuff.")
;;  "")

;;}}}
;;{{{ zyzzyva (miscellany)

(autoload 'crontab-edit "crontab" "" t)

(autoload 'electric-command-history "echistory" t)

(autoload 'fast-apropos "fast-apr" "\
Show all symbols whose names contain matches for REGEXP.  If
optional argument DO-ALL is non-nil, does more (time-consuming) work
such as showing key bindings.  Returns list of symbols and
documentation found."
t)

;;(autoload 'html-mode "html-mode" "HTML major mode." t)
;;(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)

(autoload 'ksh-mode "ksh-mode" nil t)

(autoload 'format-lisp-code-directory "lispdir" nil t)
(autoload 'lisp-dir-apropos "lispdir" nil t)
(autoload 'lisp-dir-retrieve "lispdir" nil t)
(autoload 'lisp-dir-verify "lispdir" nil t)

(autoload 'metamail-buffer "metamail" nil t)
(autoload 'metamail-interpret-body "metamail" nil t)
(autoload 'metamail-interpret-header "metamail" nil t)
(autoload 'metamail-region "metamail" nil t)

(autoload 'perldb "gud" "Not yet loaded." t)

(autoload 'super-apropos "fast-apr" "\
Show symbols whose names/documentation contain matches for REGEXP.
If optional argument DO-ALL is non-nil, does more (time-consuming)
work such as showing key bindings and documentation that is not stored
in the documentation file.  Returns list of symbols and documentation
found."
t)

;;}}}

;;}}}
;;{{{ Version-specific initializations

(require 'saveplace)
(setq-default save-place t)

(substitute-key-definition 'find-file
			   (if  (fboundp 'find-file-other-frame)
			       'find-file-other-frame
			     'find-file-other-screen)
			   (current-global-map))

(global-set-key "\M-\C-x\M-\C-f" 'find-file)

;;}}}
;;{{{ Global key bindings

(global-unset-key "\M-z")		; Remove zap-to-char!!!!
(global-unset-key "\C-z")		; Remove iconify-or-deiconify-frame

(global-set-key "\M-#" 'bbdb-complete-name)
(global-set-key "\M-\C-z"  'pp-window-configuration)
(global-set-key "\C-\\" 'compare-windows)
(global-set-key "\C-c\`" 'find-this-error)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
;;(global-set-key "\C-x4k" 'kill-buffer-other-window)
(global-set-key "\C-x4m" 'mh-smail-other-window)
(global-set-key "\C-x5m" 'mh-smail-other-frame)
(global-set-key "\C-x\C-e" 'compile)
(global-set-key "\C-x\C-k" 'kill-compilation)
(global-set-key "\C-x\M-\e" 'electric-command-history)
(global-unset-key "\C-xm")		;normally "mail"
(global-set-key "\C-xms" 'mh-smail)
(global-set-key "\C-xmr" 'mh-rmail)
(global-set-key "\C-xw" 'toggle-line-wrap) ; Overrides widen
(global-set-key "\M-$" 'my-spell-word)
(global-set-key "\M-%" 'query-replace-regexp)
(global-set-key "\M-{" 'insert-braces)
(global-set-key "\M-G" 'goto-line)
(global-set-key "\M-\C-\\" 'two-window-command)
(global-set-key "\M-\C-g" 'what-line)
(global-set-key "\M-\C-r" 'append-rtn-addr-to-paragraph)
(global-set-key "\M-\C-s" 'isearch-forward)
(global-set-key "\M-\C-t" 'insert-time-stamp)
(global-set-key "\M-\C-u" 'up-list) ; instead of backward-up-list
(global-set-key "\M-\\" 'fixup-whitespace)
(global-set-key "\M-`" 'next-error)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-s" 'center-line)

(require 'pc-select)
(pc-bindings-mode)
(pc-selection-mode);; which does: S-delete (cut), S-insert (paste) and C-insert (copy)

;; some other stuff I want pc-ized
(setq transient-mark-mode (not (memq system-type '(windows-nt dos)))
      mark-even-if-inactive nil		; may be more trouble than
					; it's worth
      )
(define-key global-map [delete] 'kill-region)
(define-key global-map [mouse-2] 'yank)
(define-key global-map [C-delete] 'copy-region-as-kill)

;;(global-set-key [f1] 'advertised-undo) ; help
(global-set-key [f2] 'kill-region)
(global-set-key [f3] 'copy-region-as-kill)
(global-set-key [f4] 'yank)

(global-set-key [M-delete] 'backward-kill-word)
(global-set-key [M-backspace] 'backward-kill-word)

;; Colorization of my choice, or nearly so
(setq default-frame-alist '((background-color . "gray60"))
      font-lock-face-attributes
      '((font-lock-comment-face "Sienna")
	(font-lock-string-face "ForestGreen")
	(font-lock-keyword-face "red")
	;;(font-lock-function-name-face "OrangeRed")
	(font-lock-function-name-face "MediumVioletRed")
	(font-lock-variable-name-face "BlueViolet")
	(font-lock-type-face "VioletRed")
	(font-lock-reference-face "IndianRed"))
      )
; (font-lock-make-faces t) ; how to make the changes happen

;; Turn on font-lock in all modes that support it
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode t))

;; Maximum colors
(setq font-lock-maximum-decoration t)

(show-paren-mode 1)

(require 'saveconf)
(if (null (cdr command-line-args))
    (recover-context))
(setq auto-save-and-recover-context t)


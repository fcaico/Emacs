
;; -*- Mode: Emacs-Lisp -*-
(setq load-path (cons (expand-file-name "~/elisp") load-path))
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
(setq completion-ignored-extensions (cons ".class" completion-ignored-extensions))

(setq shell-file-name "C:/WINNT/system32/cmd.exe")
(setq inhibit-startup-message t)
(standard-display-european t)

(cond (window-system
   (setq default-frame-alist
      '((width             . 95)
	(height            . 46)
	(font 	           . "-*-Courier New-normal-r-*-*-13-97-*-*-c-*-*-ansi-")
	(foreground-color  . "Black")
	(background-color  . "WhiteSmoke")
	(cursor-color      . "MediumBlue")
	(icon-type         . t)		; gnu picture as Emacs icon
	(icon-name         . nil)	; use frame title
   ))
   ;(setq frame-title-format "%b")	; set frame title to buffer name
   ;(setq icon-title-format "%b")	; set  icon title to buffer name
   (setq initial-frame-alist
      '((top               . 1)
        (left              . 1)
   ))
))
(require 'emacs-vers)
(setq running-xemacs (eq emacs-type 'lucid))
(setq running-emacs-19 (emacs-version>= 19))
(setq running-fsf-emacs-19 (and running-emacs-19 (not running-xemacs)))
(setq running-emacs-18 (emacs-version<  19))
(setq running-x (or (eq window-system 'x)
		    (eq window-system 'win32)))

;; Global key maps and such
(global-set-key [home]   'beginning-of-line)
(global-set-key [end]    'end-of-line)
(global-set-key [C-home] 'beginning-of-buffer)
(global-set-key [C-end]  'end-of-buffer)
(global-set-key [S-delete] 'clipboard-kill-region)
(global-set-key [S-insert] 'clipboard-yank)
(global-set-key [C-insert] 'clipboard-kill-ring-save)
(global-set-key [?\C-z] 'undo)		; added here because I usually do kills by mistake

(put 'narrow-to-region 'disabled nil)	; Enable `narrow-to-region' ("C-x n n").
(put 'eval-expression  'disabled nil)	; Enable `eval-expression'  ("M-ESC").
(put 'upcase-region    'disabled nil)	; Enable `upcase-region'    ("C-x C-u").
(put 'downcase-region  'disabled nil)	; Enable `downcase-region'  ("C-x C-l").
(global-set-key "\C-x\C-j" 'goto-line)	; Command prompts for linenumber to go to.
(global-set-key "\e " 'set-mark-command) ; Easier to type than C-@.

;; Customize in case we're running Lucid Emacs.
(cond (running-xemacs
       (setq find-file-use-truenames nil     ; `buffer-file-name' is symbolic link's name
	     find-file-compare-truenames t   ; Find true file's existing buffer.
	     minibuffer-confirm-incomplete t ; Helpful for catching typos, etc.
	     complex-buffers-menu-p t)	     ; Buffers menu will contain submenus.
       (defun current-time-zone ()
             '(60 nil "MET" "MET DST"))

       (cond (running-x			; Change top window title bar format (unless -wn)
	      (if (equal screen-title-format "%S: %b")
		  (setq screen-title-format
			(concat "%S [" emacs-version "]"
				(if nil ; (getenv "NCD")
				    ""
				  "   %b"))))))
       ; Make mouse button 2 insert at point, instead of at the position clicked.
       ;(define-key global-map 'button2 'x-insert-selection)
       (load "big-menu")		; Now that's a menu!
       (load "blink-paren")		; Nicely blinking parentheses.
       (require 'completer)		; For a smart minibuffer.
))

;; Final newline handling
(setq require-final-newline t)		; Silently ensure newline at end of file

;; Prevent Emacs from extending file when pressing down arrow at end of buffer.
(if running-xemacs
    (setq next-line-extends-end-of-buffer nil)
  (setq next-line-add-newlines nil))

;; Get rid of the silly default ChangeLog file "changelo" when running NTemacs
(if (and running-emacs-19 (eq system-type 'windows-nt))
    (setq change-log-default-name "ChangeLog"))

;; Start folding-mode when needed
(cond (running-emacs-19
      (autoload 'folding-mode "folding" 
		"Minor mode that simulates a folding editor" t)
      (defun folding-mode-find-file-hook ()
	"One of the hooks called whenever a `find-file' is successful."
	(and (assq 'folded-file (buffer-local-variables))
	     folded-file
	     (folding-mode 1)
	     (kill-local-variable 'folded-file)))
      (or (memq 'folding-mode-find-file-hook find-file-hooks)
	  (setq find-file-hooks (append find-file-hooks
				  '(folding-mode-find-file-hook))))))

;; imenu stuff
(autoload 'imenu "imenu" 
  "Jump to places in buffer using a completion or mouse menu." t)
(setq imenu-always-use-completion-buffer-p t) ; Always use completion buffer for index
(define-key global-map [M-S-down-mouse-3] 'imenu)

;; calendar stuff
(cond (running-emacs-19
      (require 'calendar)
      (require 'timezone)
      (setq european-calendar-style t)))
(setq calendar-longitude 52.21
      calendar-latitude 4.54
      calendar-location-name "Amsterdam"
      calendar-time-display-form '(24-hours 
				   ":" minutes 
				   (if time-zone " (") 
				   time-zone 
				   (if time-zone ")")))

;; Display time in mode-line (including day, date, and time zone)
(if running-fsf-emacs-19
    (progn (setq display-time-day-and-date t)
	   (setq display-time-string-forms  
		 '((format "%s %s %s " dayname monthname day)
		   (format "%s:%s" 24-hours minutes) 
		   (format " (%s) " (nth 1 (current-time-zone)))
		   load 
		   (if mail " Mail" ""))))
  (progn (require 'time)
	 (setq time-zone (nth 2 (current-time-zone)))
	 (setq display-time-day-and-date t)))
(display-time)

;; Autosave stuff
(require 'auto-save)			; Activate auto-save
(setq auto-save-directory nil)		; Save in same directory as visited file

;; font-lock, face-lock, and fast-lock setup
(defun turn-on-font-lock ()
  (font-lock-mode 1))
(cond (running-x
       (require 'font-lock)
;;       (make-face           'my-comment-face)
;;       (set-face-foreground 'my-comment-face    "Red")
;;       (make-face           'my-doc-string-face)
;;       (set-face-foreground 'my-doc-string-face "SpringGreen")
;;       (make-face           'my-string-face)
;;       (set-face-foreground 'my-string-face     "ForestGreen")
;;       (make-face           'my-keyword-face)
;;       (set-face-foreground 'my-keyword-face    "Purple")
;;       (make-face           'my-function-face)
;;       (set-face-foreground 'my-function-face   "Blue")
;;       (make-face           'my-type-face)
;;       (set-face-foreground 'my-type-face       "Firebrick")
;;       (make-face           'my-other-type-face)
;;       (set-face-foreground 'my-other-type-face "OrangeRed")
;;       (setq font-lock-function-name-face 'my-function-face
;;	     font-lock-comment-face       'my-comment-face
;;	     font-lock-string-face        'my-string-face
;;	     font-lock-keyword-face       'my-blue-face
;;	     font-lock-doc-string-face    'my-doc-string-face
;;	     font-lock-type-face          'my-type-face
;;	     font-lock-other-type-face    'my-other-type-face)
       ;; Turn on font-lock in all modes that support it
       (if (fboundp 'global-font-lock-mode)
	   (global-font-lock-mode t))
       (add-hook 'c++-mode-hook	        'turn-on-font-lock)
       (add-hook 'c-mode-hook		'turn-on-font-lock)
       (add-hook 'html-helper-mode-hook 'turn-on-font-lock)
       (add-hook 'lisp-mode-hook	'turn-on-font-lock)
       (add-hook 'perl-mode-hook	'turn-on-font-lock)
       (add-hook 'pls-mode-hook	        'turn-on-font-lock)
       (add-hook 'plsql-mode-hook	'turn-on-font-lock)
       (add-hook 'sql-mode-hook	        'turn-on-font-lock)
       (add-hook 'sqlforms-mode-hook    'turn-on-font-lock)
       (add-hook 'tex-mode-hook	        'turn-on-font-lock)
       (add-hook 'texinfo-mode-hook	'turn-on-font-lock)
     ))
(if (featurep 'font-lock)
    (require 'face-lock)
  (if running-fsf-emacs-19
      (eval-after-load "font-lock" '(load "face-lock"))
    (add-hook 'font-lock-mode-hook '(lambda () (require 'face-lock)))))
(cond (running-fsf-emacs-19
       (setq font-lock-maximum-decoration t)
       (autoload 'turn-on-fast-lock "fast-lock" "Turn on Fast Lock mode." t)
       (add-hook 'font-lock-mode-hook 'turn-on-fast-lock)
       (eval-after-load "font-lock" '(require 'choose-color))))
(defun my-java-mode-hook ()
  (cond (window-system
	 (require 'andersl-java-font-lock)
	 (turn-on-font-lock))))
(add-hook 'java-mode-hook        'my-java-mode-hook)
(load "java-imenu")


(transient-mark-mode t)
(defadvice vc-toggle-read-only (after refontify-buffer activate)
  (font-lock-fontify-buffer) ;; Or whatever the appropriate function is
)
(defadvice vc-register (after refontify-buffer activate)
  (font-lock-fontify-buffer) ;; Or whatever the appropriate function is
)

;;  html-helper-mode 
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.[Hh][Tt][Mm][Ll]?$" . html-helper-mode) auto-mode-alist))
(setq html-helper-do-write-file-hooks t) ; Set not nil to get timestamps updating
(setq html-helper-build-new-buffer t)	 ; Set not nil to get template when new buffer
(setq html-helper-address-string "Karel Sprenger <a href=\"mailto:ks@ic.uva.nl\"> &lt;ks@ic.uva.nl&gt;</a>")
(setq tempo-interactive t)
(setq html-helper-use-expert-menu t)     ; Set not nil to get the full HTML menu
(setq html-helper-htmldtd-version "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 3.0//EN\">\n"); Select HTML 3.0 style as default
(add-hook 'html-helper-load-hook '(lambda () (progn (load "hhm-extra")
						    (load "hhm-table")
						    (load "hhm-netscape"))))
(add-hook 'html-helper-mode-hook '(lambda () (auto-fill-mode 1)))

;; perl-mode
(autoload 'perl-mode "cperl-mode" 
  "alternate mode for editing Perl programs" t)
(setq auto-mode-alist (cons '("\\.[Pp][Llm]$" . perl-mode) auto-mode-alist))
(setq cperl-hairy t)
(add-hook 'perl-mode-hook '(lambda () (setq cperl-indent-level 3
					    cperl-continued-statement-offset 3
					    cperl-continued-brace-offset -3
					    cperl-brace-offset 0
					    cperl-brace-imaginary-offset 0
					    cperl-label-offset -2
					    comment-column 50)))
(setq interpreter-mode-alist (append interpreter-mode-alist
				     '(("miniperl" . perl-mode))))

;; (autoload 'perl-mode "perl-mode"
;;   "Mode for editing Perl programs" t)
;; (add-hook 'perl-mode-hook '(lambda () (setq perl-indent-level 3
;; 					    perl-continued-statement-offset 3
;; 					    perl-continued-brace-offset -3 
;; 					    perl-brace-offset 0
;; 					    perl-brace-imaginary-offset 0 
;; 					    perl-label-offset -2
;; 					    comment-column 50)))

;; sql-mode & plsql-mode
(autoload 'sql-mode "sql-mode" "Major mode for editing SQL*plus batch files." t)
(autoload 'sqlplus  "sql-mode" "Run interactive SQL*Plus session in separate buffer." t)
;(setq sqlplus-username-password "icg@T:mail.uva.nl:inbel")
(setq auto-mode-alist (cons '("\\.[Ss][Qq][Ll]$" . sql-mode) auto-mode-alist))
(autoload 'plsql-mode "plsql-mode" "Major mode for PL/SQL code" t)
(add-hook 'plsql-mode-hook '(lambda () (setq plsql-indent 3)))
(autoload 'sqlforms-mode "sqlforms-mode" "SQL*Forms Mode" t)
(setq auto-mode-alist (cons '("\\.[Ii][Nn][Pp]$" . sqlforms-mode) auto-mode-alist))
(autoload 'pls-mode  "pls-mode" "PL/SQL Editing Mode" t)
(autoload 'diana-mode  "pls-mode" "DIANA for PL/SQL Browsing Mode" t)
(setq auto-mode-alist
  (append '(("\\.[Pp][Ll][Ss]$"  . plsql-mode)
;;             ("\\.[Ss][Qq][Ll]$"  . plsql-mode)
	    ("\\.[Ss][Pp][SsBbPp]$"  . plsql-mode) ; Steve Feuerstein's package extensions
           ) auto-mode-alist))
;; (setq auto-mode-alist
;;   (append '(("\\.[Pp][Ll][Ss]$"  . pls-mode)
;;             ("\\.[Ss][Qq][Ll]$"  . pls-mode)
;;             ("\\.[Pp][[Ll][Dd]$"  . diana-mode)
;;            ) auto-mode-alist))

;; text-mode
(setq default-major-mode 'indented-text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 78)

;; Ange-ftp
(setq ange-ftp-ftp-program-name "c:/emacs-19.34/bin/ftp.exe")
(setq ange-ftp-tmp-name-template 
           (concat (expand-file-name (getenv "TEMP")) "/ange-ftp"))
(setq ange-ftp-gateway-tmp-name-template 
           (concat (expand-file-name (getenv "TEMP")) "/ange-ftp"))

;; print-NT
(setq lpr-command "print"
      lpr-destination '("/D:\"\\\\ICARUS\\o&a\"") ;; for normal printer
      ps-lpr-destination '("/D:\"\\\\ICARUS\\o&a\"")) ;; for postscript printer
(setq  ps-paper-type 'ps-a4 ) ; the type of paper (if needed)
(setq ps-lpr-buffer "c:\\temp\\psspool.ps")
(setq txt-lpr-buffer "c:\\temp\\psspool.txt")
(setq ps-psnup-buffer "c:\\temp\\psnup.ps")
(setq gs-print-command "c:\\gstools\\gs4.03\\gswin32.exe")
(setq gs-view-command  "c:\\gstools\\gsview\\gsview32.exe")
(setq ps-print-use-faces t)		; always print using faces
(require  'print-nt)

;; Miscellaneous
(require 'paren)
(show-paren-mode 1)
(set-message-beep 'ok)

;;(setq Man-awk-command "gawk")
(setq Man-awk-command "awk")
(setq Info-fontify t)
(setq Man-fontify-manpage-flag t)

;; mail stuff
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-default-smtp-server "mail.ic.uva.nl")
(setq smtpmail-smtp-service "smtp")
(setq smtpmail-local-domain "ic.uva.nl")
(setq smtpmail-code-conv-from nil)
(load-library "smtpmail")
(setq user-full-name "Karel Sprenger")
(setq user-mail-address "ks@ic.uva.nl")
(setenv "MAILHOST" "mail.ic.uva.nl")
(setq rmail-primary-inbox-list '("po:ks@ic.uva.nl") rmail-pop-password-required t)

;; gnus stuff
(setq gnus-default-nntp-server "mail.ic.uva.nl")
(setq gnus-local-domain "ic.uva.nl")
(setq gnus-local-organization "Informatiseringscentrum, University of Amsterdam")
(setq message-send-mail-function 'smtpmail-send-it)
(setq gnus-secondary-select-methods '((nnml "")))
(setq nnmail-spool-file "po:ks@ic.uva.nl")
(setq nnmail-pop-password-required t)
(require 'generic-mode)
(require 'generic-extras)

;; lisp-dir
(require 'crypt++)
(autoload 'format-lisp-code-directory "lispdir" nil t)
(autoload 'lisp-dir-apropos "lispdir" nil t)
(autoload 'lisp-dir-retrieve "lispdir" nil t)
(autoload 'lisp-dir-verify "lispdir" nil t)
(setq lisp-code-directory "~/elisp/LCD-datafile.gz")
(setq elisp-archive-host "sunsite.cnlab-switch.ch")
(setq elisp-archive-directory "/mirror/elisp-archive/")

;;; Commands added by calc-private-autoloads on Fri Feb 28 16:27:48 1997.
(autoload 'calc-dispatch	   "calc" "Calculator Options" t)
(autoload 'full-calc		   "calc" "Full-screen Calculator" t)
(autoload 'full-calc-keypad	   "calc" "Full-screen X Calculator" t)
(autoload 'calc-eval		   "calc" "Use Calculator from Lisp")
(autoload 'defmath		   "calc" nil t t)
(autoload 'calc			   "calc" "Calculator Mode" t)
(autoload 'quick-calc		   "calc" "Quick Calculator" t)
(autoload 'calc-keypad		   "calc" "X windows Calculator" t)
(autoload 'calc-embedded	   "calc" "Use Calc inside any buffer" t)
(autoload 'calc-embedded-activate  "calc" "Activate =>'s in buffer" t)
(autoload 'calc-grab-region	   "calc" "Grab region of Calc data" t)
(autoload 'calc-grab-rectangle	   "calc" "Grab rectangle of data" t)
(setq load-path (append load-path (list "h:/ks/elisp/calc-2.02d")))
(global-set-key "\e#" 'calc-dispatch)
;;; End of Calc autoloads.

;; start gnuserv
(if (eq window-system 'win32)	;; Windows NT/95
    (progn
      (require 'gnuserv)
      (setq server-done-function 'bury-buffer)
      (setq gnuserv-frame (car (frame-list)))
      (gnuserv-start)
      (message "gnuserv started.")
      (beep)))

--=====================_874040676==_
Content-Type: text/plain; charset="us-ascii"


Karel Sprenger <ks@ic.uva.nl>               | phone: +31-20-525 2302
Informatiseringscentrum                     |        +31-20-525 2741
Universiteit van Amsterdam                  | fax  : +31-20-525 2084
Turfdraagsterpad 9, NL-1012 XT  AMSTERDAM   | home : +31-20-670 0942
*** PGP Public Key available on servers *** | email: <cjas@xs4all.nl>

--=====================_874040676==_--



--------------34597E6D404C--



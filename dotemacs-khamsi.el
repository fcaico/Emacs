;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; S. Khamsi
;;; 13 May 1997
;;;
;;; $Id: .emacs 1.44 1997/09/09 13:07:16 khamsi Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set up some initial things
(cond ((file-exists-p "~/.backups")
       (setq auto-save-list-file-prefix "~/.backups/.saves-")))
(setq c-brace-offset -3)
(setq enable-local-variables 1)
(setq perl-indent-level 2)
(setq tab-width 2)
(setq tab-interval 2)
(setq make-backup-files t)
(setq next-line-add-newlines nil)
(setq set-fill-column 80)
(setq version-control t) ; Allow numbered backups
(setq visible-bell t)
(setq-default indent-tabs-mode nil)
;(modify-syntax-entry "_" "w")
(let ((process-connection-type nil))	; try not to hog a pty for this.
  (display-time))
(setq frame-title-format "Emacs - %f")
(setq icon-title-format "Emacs - %b")

;;; Resize the minibuffer so its entire contents are visible.
(setq resize-minibuffer-mode t)
(resize-minibuffer-mode)

;;; OS specific stuff
(global-set-key [mouse-wheel] 'mouse-wheel-handler)
(setq shell-command-switch "-c")
(setq win32-quote-process-args t)
(setq win32-num-mouse-buttons 2)
(using-unix-filesystems t)
(setenv "SHELL" "c:/usr/local/gnuwin32/b18/H-i386-cygwin32/bin/bash.exe")
(setq shell-file-name "c:/usr/local/gnuwin32/b18/H-i386-cygwin32/bin/bash.exe")
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
(setq default-frame-alist
      '((cursor-color . "blue")
        (foreground-color . "black")
        (background-color . "white")
        (top . 5) (left . 600)
        (width . 80) (height . 87)
        (font . "-*-6X13-medium-r-*-*-13-97-*-*-p-60-*-ansi-")))
(setq initial-frame-alist '((top . 5) (left . 70)))

;(set-default-font "-*-Lucida Console-normal-r-*-*-13-97-*-*-p-70-*-ansi-")
(add-hook 'shell-mode-hook
          '(lambda () (setq comint-completion-addsuffix '("/" . ""))) t)

;; Use my own compile.el
(cond ((file-exists-p "~/lisp/compile.elc")
       (load-file "~/lisp/compile.elc")))

;;; Append ~/lisp to the load-path if it exist.
(cond ((file-exists-p "~/lisp")
       (setq load-path (cons (expand-file-name "~/lisp") load-path))))
; Add my util file.
(cond ((or (file-exists-p "~/lisp/util.el")
           (file-exists-p "~/lisp/util.elc"))
           (load "util")))

;;; Set frame size and position
(set-frame-position (selected-frame) 70 5)
(set-frame-height (selected-frame) 87)
(set-frame-width (selected-frame) 80)

;;; Colorized fonts
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)

(setq query-replace-highlight t)	;highlight during query
(setq search-highlight t)		;incremental search highlights

;;; Setup C/C++/Java indentation
(require 'cc-mode)
;; This will work after redumping
(setq c-site-default-style "local")

;; Use this until redumping
(add-hook 'c-mode-hook
	  (function (lambda ()
		      (setq c-file-style "local"))))
(add-hook 'c++-mode-hook
	  (function (lambda ()
		      (setq c-file-style "local"))))

;;; Setup Java mode stuff
(cond ((file-exists-p "~/lisp")
       (setq c-brace-offset -2)
       (autoload 'java-mode "lisp/java-mode" "java mode" t nil)
       (setq auto-mode-alist
             (append '(("\\.java$" . java-mode)) auto-mode-alist))))

(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun my-java-mode-hook ()
  (cond (window-system
         (require 'andersl-java-font-lock)
         (turn-on-font-lock))))

(add-hook 'font-lock-mode-hook
          (function
           (lambda ()
             (if (eq major-mode 'java-mode)
                 (setq font-lock-keywords java-font-lock-keywords)))))

;(cond ((file-exists-p "~/lisp/hilit-java.elc")
;       (load "~/lisp/hilit-java")))

(c-add-style "local"
             '((c-basic-offset . 2)
               (c-comment-only-line-offset . 0)
               (c-hanging-braces-alist . ((substatement-open before after)))
               (c-offsets-alist . ((topmost-intro        . 0)
                                   (topmost-intro-cont   . 0)
                                   (substatement         . 2)
                                   (substatement-open    . 0)
                                   (statement-case-open  . 2)
                                   (statement-cont       . 2)
                                   (access-label         . -2)
                                   (inclass              . 2)
                                   (inline-open          . 2)
                                   ))))
(c-add-style "tabs"
             '((c-basic-offset . 2)
               (c-comment-only-line-offset . 0)
               (c-offsets-alist . ((statement-block-intro . +)
                                  (knr-argdecl-intro . 0)
                                  (substatement-open . 0)
                                  (label . -)
                                  (statement-cont . 0)
                                  ))))

;;; Go into c++-mode or perl-mode depending on file extention
(setq auto-mode-alist
      (append '(("\\.C$"   . c++-mode)
		("\\.cc$"  . c++-mode)
		("\\.cpp$" . c++-mode)
		("\\.cxx$" . c++-mode)
		("\\.hxx$" . c++-mode)
		("\\.h$"   . c++-mode)
		("\\.hh$"  . c++-mode)
		("\\.idl$" . c++-mode)
		("\\.c$"   . c-mode)
                ("\\.pl$" . perl-mode)
                ("\\.pm$" . perl-mode)
                ("\\.java$" . java-mode)
                ("\\.?[Ff][Aa][Qq]$" . faq-mode)
                ("\\.txt$" . text-mode))
	      auto-mode-alist))

;;; Used for switching frames (fix for C-x 5 o being "fixed")
;(defun raise-and-focus-on-frame (frame)
;  "Raise frame and give it input focus if in foreground."
;  (raise-frame frame)
;  (make-frame-invisible frame)
;  (make-frame-visible frame))

;;; Re-bind some keys
;(global-unset-key "\C-x5o")
;(global-set-key "\C-x5o" 'raise-and-focus-on-frame)
(global-set-key "\eg" 'fill-region)
;(global-set-key "\C-cg" 'gnus)
;(global-set-key "\C-cu" 'gnus-uudecode-marked-articles)
;(global-set-key "\C-cl" 'recenter)
(global-set-key [C-up]   'enlarge-window)
(global-set-key [C-down]  'shrink-window)

;;; Set up some function keys
(global-set-key (quote [f1]) (quote enlarge-window 1))
(global-set-key [f2] 'shrink-window)
(global-set-key [f3] 'ispell-word)
(global-set-key [f4] 'call-last-kbd-macro)
(global-set-key [f5] 'next-error)
(global-set-key [f6] 'compile)
(global-set-key [f7] 'ediff-files)
(global-set-key [f8] 'hilit-highlight-buffer)
(global-set-key [f9] 'undo)
(global-set-key [f10] 'other-window)
(global-set-key [f11] 'goto-line)
(global-set-key [f12] 'bury-buffer)

;;; Setup ange-ftp
(cond ((file-exists-p "e:/bin/ftp.exe")
       (setq ange-ftp-ftp-program-name "e:/bin/ftp.exe")))
(setq ange-ftp-tmp-name-template 
      (concat (expand-file-name (getenv "TEMP")) "/ange-ftp"))
(setq ange-ftp-gateway-tmp-name-template 
      (concat (expand-file-name (getenv "TEMP")) "/ange-ftp"))

;;; Mail related stuff
;;; Some place to put copies for me
(setq mail-archive-file-name "~/Mail/my_messages_at_raytheon")
(setq rmail-delete-after-output t)

;;; Set up rmail headers to ignore
(setq rmail-ignored-headers 
      "^distribution:\\|^errors-to:\\|^message-id:\\|^originating-client:\\|^path:\\|^precedence:\\|^received:\\|^repository:\\|^status:\\|^summary-line:\\|^x-envelope-to:\\|^x-vms-to:\\|^x-mailer:\\|^x-listserver:\\|^x-listserver-version:\\|^content-length:\\|^posted-date:\\|^content-type:\\|^mime-version:\\|^x-mailing-list:\\|^x-mailing-list-archive:\\|^sender:\\|^X-Listprocessor-Version:\\|^contect-transfer-encoding:\\|^X-Loop:\\|^X400:\\|^X-:\\|^Resent-From:\\|^Resent-Sender:\\|^Encoding:\\|^Return-Path:\\|^Mr-Received:\\|^Autoforward:\\|^Importance:\\|^Priority:\\|^Sensetivity:\\|^Ua-Content-Id:\\|^X400-Mts-Identifier:\\|^Hop-Count:\\|^References:\\|^Lines:")

;;; Set up fill mode in a few places.
(setq set-fill-column 80)
(setq mail-mode-hook			; do auto fill mode in mail mode
      '(lambda () (auto-fill-mode 1)))

(setq text-mode-hook			; do auto fill mode in text mode
      '(lambda () (auto-fill-mode 1)))

;;;	Supercite-2.3
(setq sc-nested-citation-p t)
(setq sc-fixup-whitespace-p nil)
(setq sc-auto-fill-region-p nil)
(setq sc-electric-references-p nil)
(setq sc-citation-leader "")
(setq mail-yank-hooks 'sc-cite-original)

;;; Other stuff
(setq news-reply-mode-hook
      '(lambda ()
	 (auto-fill-mode 1)
	 (abbrev-mode 1)))
(setq news-post-mode-hook
      '(lambda ()
	 (auto-fill-mode 1)
	 (abbrev-mode 1)))

(setq news-reply-followup-to-mode-hook
      '(lambda ()
	 (auto-fill-mode 1)
	 (abbrev-mode 1)))

;;; GNUS defaults
;(setq tcp-program-name "nttcp")
(setq gnus-use-generic-from "kmrnews")
(setq gnus-nntp-server "kmrnews")
(setq gnus-your-organization "Raytheon")
(setq gnus-user-full-name "Sarir Khamsi")
(setq gnus-article-maybe-hightlight t)
(setq gnus-user-from-line "khamsi@kmrmail.kmr.ll.mit.edu")

;; Some stuff to speed up GNUS startup
(setq gnus-check-new-newsgroups 'ask-server)
(setq gnus-read-active-file t)  ;; was 'some
(setq gnus-fetch-old-headers nil) ;even "some" setting takes too long
(setq gnus-asynchronous t)
(setq gnus-keep-backlog 10)

;;; Set up abbreviation mode
(setq-default abbrev-mode t)
(cond ((file-exists-p "~/.abbrev_defs")
       (read-abbrev-file "~/.abbrev_defs")))
(setq save-abbrevs t)

(defun shell-mode-settings ()
  (add-hook 'comint-output-filter-functions 
            'comint-watch-for-password-prompt nil t)
  (add-hook 'comint-output-filter-functions 
            'comint-strip-ctrl-m nil t)
  (setq tab-width 8))

(add-hook 'shell-mode-hook 'shell-mode-settings)

;;; Set up POP3 stuff
(setq user-full-name "Sarir Khamsi")
(setq user-mail-address "khamsi@kmrmail.kmr.ll.mit.edu")
;(setq rmail-primary-inbox-list '("po:khamsi") rmail-pop-password-required t)
(setq smtpmail-default-smtp-server "kmrmail")
(setq smtpmail-local-domain "kmrmail")
(setq send-mail-function 'smtpmail-send-it)
(setenv "MAILHOST" "kmrmail")
(load-library "smtpmail")

;;; Setup functions to allow toggling between quoting args and not
(defun sk-quote-process-args (state)
  (interactive "sState: ")
  (cond ((equal state "t")
         (setq win32-quote-process-args t))
        (not equal (state "t")
             (setq win32-quote-process-args t))))

(global-set-key "\C-cV" 'get-clipboard)
(global-set-key "\C-cC" 'set-clipboard)

;;; Setup ispell4 stuff
(cond ((file-exists-p "c:/bin/ispell.exe")
       (autoload 'ispell-word "ispell4" 
         "Check spelling of word at or before point" t)
       (autoload 'ispell-complete-word "ispell4" 
         "Complete word at or before point" t)
       (autoload 'ispell-region "ispell4" 
         "Check spelling of every word in the region" t)
       (autoload 'ispell-buffer "ispell4" 
         "Check spelling of every word in the buffer" t)
       (setq ispell-command "c:/bin/ispell.exe"
             ispell-look-dictionary
             "c:/bin/ispell.words"
             ispell-look-command "c:/bin/look.exe"
             ispell-command-options (list "-d" "c:/bin/ispell.dict"))))

;;; Setup the desktop package.
(load "desktop")
(desktop-load-default)
(desktop-read)

(put 'eval-expression 'disabled nil)

;;; Setup MIME stuff
;(load "mime-setup")

;;; User specific stuff

;;; Bag the scrollbars
;(scroll-bar-mode nil)
;(menu-bar-mode nil)

;(setq load-path (cons "c:/usr/local/emacs-19.34/site-lisp" load-path))

;;; Load VM
(cond ((file-exists-p "~/lisp/.vm")
       (load-file "~/lisp/.vm")))

;;; WoMan stuff for reading man pages in emacs
(autoload 'woman "woman" "Decode and browse a UN*X man page." t)
(autoload 'woman-find-file "woman"
  "Find, decode and browse a specific UN*X man-page file." t)

;;; Microsoft IntelliPoint support
(defvar mouse-wheel-scroll-amount 20
  "*Number of lines to scroll per click of the mouse wheel.")
      
(defun mouse-wheel-handler (event)
  "Scroll the current buffer by `mouse-wheel-scroll-amount'."
  (interactive "e")
  (condition-case nil
      (if (< (car (cdr (cdr event))) 0)
	  (scroll-up mouse-wheel-scroll-amount)
	(scroll-down mouse-wheel-scroll-amount))
    (error nil)))

;;; Start off in "home" dir.
(cd (getenv "HOME"))

;;; Fix wierd shell problem.
(add-hook 'sh-mode-hook
          (defun my-shell-mode-hook ()
            (sh-set-shell 
             "c:/usr/local/gnuwin32/b18/H-i386-cygwin32/bin/bash")))

;;; Show matching parens
(require 'paren)

;;; Set up faq-mode
(load "faq-mode")




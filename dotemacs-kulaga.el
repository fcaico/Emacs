;;  Set initial position of Emacs window ;
(setq initial-frame-alist '((top . 10) (left . 30)
			    (width . 130) (height . 60)))

;;  Cyrillic support			
(standard-display-european 1)

;;  Frame appearence			
(set-default-font 
 "-*-Lucida Console-normal-r-*-*-11-82-*-*-c-*-*-#204-")

(setq default-frame-alist
      '((top . 20) (left . 40)
	(width . 130) (height . 60)
	(cursor-color . "black")
	(foreground-color . "black")
	(background-color . "white")
	(font . "-*-Lucida Console-normal-r-*-*-11-82-*-*-c-*-*-#204-")))

	; set the format for the title bars (and thus the icon text) for 
	; each window			
(setq frame-title-format "Emacs - %f")
(setq icon-title-format "Emacs - %b")

;;;  Key Bindings			
(global-set-key [C-f4]  'kill-this-buffer)
(global-set-key [M-f5]  'goto-line)
(global-set-key [S-f8]  'compile)
    

;; Mouse Bindings			

;; Text mode				
(add-hook 'text-mode 'auto-fill-mode)
(setq fill-column 70)

;; Bookmarks
(setq bookmark-save-flag 1)

;; Fontification			
(global-font-lock-mode t)
(setq font-lock-support-mode 'lazy-lock-mode)
(setq font-lock-maximum-decoration t )
(show-paren-mode 1)

;; Mail support

(setq user-full-name "Andrey A. Kulaga")
(setq user-mail-address "kulaga@rp3.menatep.ru")
(setq smtpmail-default-smtp-server "rp3.menatep.ru")
(setq smtpmail-local-domain nil)
(setq send-mail-function 'smtpmail-send-it)
(setq mail-interactive nil)
(load-library "smtpmail")

(setenv "MAILHOST" "rp3.menatep.ru")
(setq rmail-primary-inbox-list '("po:kulaga") rmail-pop-password-required t)

(setq mail-yank-prefix "> ")
(setq rmail-ignored-headers "^X.*\\|Received.*\\|Status.*\\|Return-Path\\|Message-Id\\|Organization\\|^MIME.*\\|Content\\|References\\|Precedence\\|Lines\\|In-Reply-To\\|Sender\\|Priority\\|Encoding" )
(setq rmail-file-name "~/mail/RMAIL")
(setq mail-self-blind t)

(setq rmail-delete-after-output 't)
(setq rmail-output-file-alist '( ("ntemacs-users" . "~/mail/emacs" ) 
				 ("gnu-win32" . "~/mail/gnu")
				 ("Andrey A. Kulaga" . "~/mail/copy")
				 ("rp3.menatep.ru" . "~/mail/otdel")))


;; You are need rmime package to read mime-formatted mail. See FAQ 
;(add-hook 'rmail-show-message-hook 'rmime-format)
;(add-hook 'rmail-edit-mode-hook    'rmime-cancel)
;(autoload 'rmime-format "rmime" "" nil)


(global-set-key [f5]    'my-get-new-mail)
(global-set-key [S-f5]  'my-send-mail)

;; Diary support

(setq european-calendar-style 't)
(setq calendar-week-start-day 1)
(display-time)
(add-hook 'diary-hook 'appt-make-list)
           

;; C support				
(load "cc-mode")
(add-hook 'c-mode 'auto-fill-mode)
(add-hook 'c++-mode 'auto-fill-mode)
(setq auto-mode-alist
      (append
       '(("\\.C$"    . c++-mode)
	 ("\\.H$"    . c++-mode)
	 ("\\.cc$"   . c++-mode)
	 ("\\.hh$"   . c++-mode)
	 ("\\.c$"    . c-mode)
	 ("\\.sc$"   . c-mode)
	 ("\\.h$"    . c-mode)
	 ("\\.m$"    . objc-mode)
	 ("\\.java$" . java-mode)
	 ) auto-mode-alist))

;;  Shell Support - uncomment this if you use bash as shell		
;(setq sh-shell-file '"/gnuwin32/b18/H-i386-cygwin32/bin/bash")
;(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
;(setq win32-quote-process-args t)
;(setq shell-command-switch "-c")
;(add-hook 'shell-mode-hook
;               '(lambda () (setq comint-completion-addsuffix t))
;               t)


;;  Man support	- you are need Woman package to read Unix man pages. See FAQ		
;(autoload 'woman "woman"
;  "Decode and browse a UN*X man page." t)
;(autoload 'woman-find-file "woman"
;  "Find, decode and browse a specific UN*X man-page file." t)

;(define-key-after
;      ;; Repeated calls do not seem to matter! 
;  (lookup-key global-map [menu-bar help-menu])
;  [woman] '("WoMan..." . woman) 'man)

;;  Initially loaded files		
;(shell)
;(calendar)

;; Desktop saving

;(load "desktop")
;(desktop-load-default)
;(desktop-read)


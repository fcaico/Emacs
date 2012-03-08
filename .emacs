;; ======================================================================
;; EMACS Customization file
;     ftp://ftp.cis.ohio-state.edu/pub/emacs-lisp/
;     ftp://ftp.uni-mainz.de/pub/gnu/elisp-archive/
;     ftp://gatekeeper.dec.com/pub/GNU/elisp-archive/
;     ftp://nic.funet.fi/pub/gnu/emacs/elisp-archive/
;     ftp://sunsite.cnlab-switch.ch/mirror/elisp-archive/
;     ftp://src.doc.ic.ac.uk/public/Mirrors/ftp.cis.ohio-state.edu/pub/emacs-lisp/
;; /anonymous@ftp.cis.ohio-state.edu:/pub/emacs-lisp
;;
;;
;; This file contains all my current emacs customizations.
;; For the list of emacs lisp files available try this URL:
;;       http://www.anc.ed.ac.uk/~stephen/emacs/ell.html
;;
;; for more goodies and examples goto this URL:
;;       http://www.cs.washington.edu/homes/voelker/ntemacs/contrib/
;; 
;; for printable (postscript) versions of the emacs manuals goto this URL:
;;       ftp://ftp.cs.ubc.ca/pub/archive/gnu/manuals_ps
;;
;; you can make emacs "understand" URLs and email addresses by using
;; goto-address.el.  Try it now by typing M-x goto-address.  The above
;; URL should then highlight.
;;
;; Permanent redirecting link to emacs FAQ and resources:
;; http://poboxes.com/jari.aalto/emacs-elisp.html
;;
;; send mail to Frank for more information: fcaico@ufosys.com
;;
;; ======================================================================

;; =================================================================
;; Begin Customizations
;; =================================================================
;; Stuff that has to be first according to notes
;; =================================================================

;; SET THIS FIRST!! EVERYTHING ELSE DEPENDS ON THIS VARIABLE!

;; add private lisp directory to load-path
(add-to-list 'load-path "c:/program files/emacs-21.1/site-lisp/eshell")
(add-to-list 'load-path "c:/program files/emacs-21.1/site-lisp/pcomplete")
(add-to-list 'load-path "c:/program files/emacs-21.1/site-lisp/jde/lisp")
(add-to-list 'load-path "c:/program files/emacs-21.1/site-lisp/semantic")
(add-to-list 'load-path "c:/program files/emacs-21.1/site-lisp/speedbar")
(add-to-list 'load-path "c:/program files/emacs-21.1/site-lisp/elib")
(add-to-list 'load-path "c:/program files/emacs-21.1/site-lisp/eieio")
(add-to-list 'load-path "~/emacs")

;; =================================================================
;; Emacs auto customize section
;; =================================================================

;; Start custom section. emacs will  add to this section when
;; you use the customize stuff on the help menu



(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(auto-show-mode nil)
 '(c-comment-only-line-offset (quote (0 . 0)))
 '(c-hanging-braces-alist (quote ((brace-list-open before after) (brace-entry-open before after) (substatement-open before after) (block-close . c-snug-do-while) (extern-lang-open after) (inexpr-class-open before after) (inexpr-class-close before after))))
 '(c-indent-comments-syntactically-p t)
 '(c-tab-always-indent nil)
 '(delete-old-versions t)
 '(eshell-modules-list (quote (eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-script eshell-smart eshell-term eshell-unix)))
 '(font-lock-maximum-decoration (quote ((t . t))))
 '(global-font-lock-mode t nil (font-lock))
 '(hscroll-global-mode t nil (hscroll))
 '(inhibit-startup-message t)
 '(jde-bug-jdk-directory "d:/java/jdk1.3.1/")
 '(jde-bug-vm-includes-jpda-p t)
;; '(html-helper-mode-uses-bold-italic t nil (html-helper-mode))
 '(jde-compile-option-directory "d:\\java\\dev\\classes")
 '(jde-compile-option-sourcepath (quote ("d:\\java\\dev\\source")))
 '(jde-db-debugger (quote ("JDEbug" "" . "Executable")))
 '(jde-db-source-directories (quote ("d:\\java\\dev\\source")))
 '(jde-global-classpath (quote ("d:\\java\\dev\\source" "d:\\java\\dev\\classes")))
 '(next-line-add-newlines nil)
 '(show-paren-delay 0.5)
 '(show-paren-mode t nil (paren))
 '(show-paren-ring-bell-on-mismatch t)
 '(show-paren-style (quote mixed))
 '(speedbar-use-images nil)
 '(tab-width 4)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(version-control t)
 '(cursor-type 'bar)
 '(cursor-color red))
(set-cursor-color "red")

(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(font-lock-builtin-face ((((class color) (background light)) (:foreground "DarkBlue"))))
 '(font-lock-keyword-face ((((class color) (background light)) (:foreground "Blue"))))
 '(html-tag-face ((((class color) (background light)) (:bold t :foreground "DarkGreen"))))
 '(vhdl-font-lock-attribute-face ((((class color) (background light)) (:foreground "SlateBlue")))))

;; =================================================================
;; Frame settings
;; =================================================================

;; To obtain new font string, execute eval-expression, and eval this:
; (insert(prin1-to-string(w32-select-font)))
;; This will show the required string in the scratch buffer.

(setq default-frame-alist
      '((width . 80) 
		(height . 45)
		(font . "-outline-consolas-normal-r-normal-normal-12-120-96-96-c-*-iso8859-15")))
;;		(font . "-outline-Andale Mono-normal-r-normal-normal-12-90-96-96-c-*-iso8859-15")))
;;        (font . "-*-Courier New-normal-r-*-*-12-82-96-96-c-*-iso8859-1")))
;;(setq initial-frame-alist '((top . 0) (left . 0)))

;; Uncomment this to make a minibuffer only frame!
;;(add-to-list 'default-frame-alist
;;	'(minibuffer . nil))


;; =================================================================
;; EMACS general look and feel 
;; =================================================================

(require 'fc-buffers)
(require 'fc-frames)
(require 'fc-misc)
(require 'fc-VSS)

;; turn on delete-selection-mode. This makes the selection behave
;; in a more windows-like manner.
(delete-selection-mode)

;; Shut off annoying sound
(set-message-beep 'silent)

;; Set the icon and frame titles %f file name, %b buffer name
(setq frame-title-format "%b") 
(setq icon-title-format "%f")

;; show column number in status bar
(setq column-number-mode t)

;; make searches case-INsensitive
(set-default 'case-fold-search t)

; Enable `narrow-to-region' ("C-x n n").
(put 'narrow-to-region 'disabled nil)	

; Enable `eval-expression'  ("M-ESC").
(put 'eval-expression  'disabled nil)	

;; Final newline handling - Silently ensure newline at end of file
(setq require-final-newline t)		

;; Get rid of old versions of files
(setq delete-old-versions t)

;; Shut off warning messages when using system shell
(setq w32-allow-system-shell t)

;; Stop ^M's from displaying in system shell window
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;; Automatically turn on auto-fill-mode when editing text files
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; If you want to change the word wrap column, change this number
;; (setq-default fill-column 65)

;;======================================================================
;; Load gnuserv. This is required for things such as running emacs
;; as the default MSVC editor. I usually start emacs once in the
;; morning, then keep it running all day as my only editor
;;
;; visit http://www.wyrdrune.com/gnuserv.htm for help with this mode.
;;======================================================================
(require 'gnuserv)
;;(setenv "GNUSERV_SHOW_EMACS" "1")
(gnuserv-start)
(setq gnuserv-frame (selected-frame))

;;======================================================================
;; This maps edit keys to standard Windows keystokes. It requires the
;; library cua-mode.el from Kim Storm at the following URL:
;; http://eolicom.olicom.dk/~storm/cua-mode.el
;;(load "cua-mode")
;;(CUA-mode t)


;;======================================================================
;; This lib makes for nice buffer handling when many files are open
;; To invoke menu, hit C + mouse 1
(load "msb")


;;======================================================================
;; imenu stuff
(autoload 'imenu "imenu" 
  "Jump to places in buffer using a completion or mouse menu." t)
; Always use completion buffer for index
(setq imenu-always-use-completion-buffer-p t)

(define-key global-map [M-S-down-mouse-3] 'imenu)

;; =================================================================
;; Compiling with MS Visual Studio
;; =================================================================

(require 'fc-compile)


;; =================================================================
;; Grep Stuff
;; =================================================================

;; Note - requires latest grep, find, and xargs to be in
;; emacs/bin directory. It is available from the follwing URL:
;; http://www.cygnus.com/misc/gnu-win32

;; Once grep is completed, you can visit each hit in order
;; with C-x` ( that's a back tic )

;; this is required for igrep.el version 2.7 and later.
(defvar grep-null-device null-device)

(setq igrep-expression-quote-char ?')
(setq igrep-parenthesis-escape-char ?\\)

(autoload (function igrep) "igrep"
  "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)

(autoload (function igrep-find) "igrep"
  "*Run `grep` via `find`..." t)

(autoload (function dired-do-igrep) "igrep"
  "*Run `grep` on the marked (or next prefix ARG) files." t)

(autoload (function dired-do-igrep-find) "igrep"
  "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)

;; Ignore case by default:
(setq igrep-options "-i")
;; To search subdirectories by default:
(setq igrep-find t)

;; =================================================================
;; mode settings
;; =================================================================
(setq auto-mode-alist
	  (append (list '("\\.cc$"    . c++-mode)
					'("\\.hh$"    . c++-mode)
					'("\\.hpp$"   . c++-mode)
					'("\\.c$"     . c++-mode)
					'("\\.h$"     . c++-mode)
					'("\\.s?html?\\'" . html-helper-mode)
					'("\\.html$" . html-helper-mode)
					'("\\.htm$" . html-helper-mode)
					'("\\.asp$" . html-helper-mode)
					'("\\.aspx$" . html-helper-mode)
					'("\\.cs$" . csharp-mode)
					'("\\.vbs"    . visual-basic-mode)
					'("\\.js"     . java-mode)
					'("\\.inc"    . visual-basic-mode)
					'("\\.cs$" . csharp-mode)
					) auto-mode-alist))

;; =================================================================
;; HTML Helper Mode
;; =================================================================

;; We dont *always* need html helper mode...
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)

(defun html-switch-modes ()
  (interactive)
  (if (eq major-mode 'html-helper-mode)
	  (visual-basic-mode)
	(if (eq major-mode 'visual-basic-mode)
		(java-mode)
	  (if (eq major-mode 'java-mode)
		  (html-helper-mode)))))


(defun html-install-my-keys ()
  (local-set-key [f3] 'html-switch-modes))

(defun html-helper-settings ()
  (setq html-helper-verbose nil)
  (html-install-my-keys)
  (local-set-key "\C-m" 'newline)
  (local-set-key "\C-i" 'self-insert-command))

(add-hook 'html-helper-mode-hook 'html-helper-settings)
(add-hook 'visual-basic-mode-hook 'html-install-my-keys)
(add-hook 'java-mode-hook 'html-install-my-keys)

;;======================================================================
;; Stuff for IDL mode 
;;======================================================================

;; turn on font-lock-mode for IDL files.  Dont know why this isnt
;; on by default...
(add-hook 'idl-mode-hook 'font-lock-mode)

;;(define-key idl-mode-map [f7] 'fc-compile-prompt-only-if-needed)
;;(define-key idl-mode-map [S-f7] 'fc-compile-prompt-always)

;;======================================================================
;; Stuff for C++/C mode 
;;
;; Use c-show-syntactic-information on a line of c code to determine
;; which variables you want to customize for indentation purposes.
;;======================================================================
(require 'fc-headers)

;; These setting are written in the order as described in the Emacs
;; info pages. ( Hit C-hi, then go to Emacs | Programs | Program
;; Indent | Custom C Indent | Syntactic Symbols for a description of
;; each. I found it easier to open one of my own source files, and
;; hit tab on a particular line to find the name of the syntactic
;; symbol. This assumes that the setting for 
;; c-echo-syntactic-information-p is not nil. )

(setq my-c-style
	  '((c-auto-newline . t)
		(c-cleanup-list . (scope-operator empty-defun-braces defun-close-semi))
		(c-offsets-alist . ((arglist-close . c-lineup-arglist)
							(substatement-open . 0)))))

(add-hook 'c-mode-common-hook
		  (function (lambda () 
					  (c-add-style "my-style" my-c-style t))))

;;(define-key c-mode-map [f7] 'fc-compile-prompt-only-if-needed)
;;(define-key c++-mode-map [f7] 'fc-compile-prompt-only-if-needed)
;;(define-key c-mode-map [S-f7] 'fc-compile-prompt-always)
;;(define-key c++-mode-map [S-f7] 'fc-compile-prompt-always)


;;(define-key c-mode-map [M-C-return] 'insert-function-header)
;;(define-key c++-mode-map [M-C-return] 'insert-function-header)
;;(define-key c-mode-map "\e\C-c" 'fc-insert-separator)
;(define-key c++-mode-map "\e\C-c" 'fc-insert-separator)

;; =================================================================
;; PC-Buffer switching...
;; This allows c-tab and c-s-tab switching
;; Author: Igor Boukanov <boukanov@fi.uib.no>
;; =================================================================
(require 'pc-bufsw)
(pc-bufsw::bind-keys [C-tab] [C-S-tab])


;; =================================================================
;; Lisp Dir Appropos stuff
;; =================================================================
(autoload 'format-lisp-code-directory "lispdir" nil t)
(autoload 'lisp-dir-apropos "lispdir" nil t)
(autoload 'lisp-dir-retrieve "lispdir" nil t)
(autoload 'lisp-dir-verify "lispdir" nil t)

(setq elisp-archive-host "ftp.cis.ohio-state.edu")
(setq elisp-archive-dirctory "/pub/emacs-lisp")
(setq lisp-code-directory "c:/program files/emacs-20.6/site-lisp/LCD-datafile.Z")

;; =================================================================
;; Mini-functions, macros, and other personal hacks
;; =================================================================

;; Good for opening header file under cursor
(global-set-key "\C-cf" 'open-file-under-cursor)

(fset 'open-file-under-cursor
   [?\C-\M-b ?\C-  ?\C-\M-f ?\C-\M-f ?\M-w ?\C-x ?\C-f ?\C-y return])

;; =================================================================
; make emacs come up iconified
;;(iconify-frame (selected-frame))

;;======================================================================
;; Stuff to make backup files always go to C:/BACKUPS 
;;======================================================================
(require 'backup-dir)
(setq bkup-backup-directory-info  '((t "/backups/"   ok-create )))

;;======================================================================
;; Autorevert Makes sure what we are editing doesnt get out of sync 
;; with the file system.
;;======================================================================

(require 'autorevert)
;(global-auto-revert-mode 1)
(add-hook 'c-mode-common-hook 'turn-on-auto-revert-mode)

;;======================================================================
;; Stuff for compressing/decompressing files
;;======================================================================
(require 'crypt++)



;;(load "eshell-auto")



;; =================================================================
;; Useful notes and other stuff
;; =================================================================


;; How to record and display a keyboard macro

;; Just open a buffer and type C-x (   Then start typing in your macro.
;; Once you are finished defining your macro type C-x ) 
;; Then type M-x name-last-kbd-macro. This will allow you to call your
;; macro whatever you want. Next open up your .emacs file and position
;; your cursor where you want the code for the macro to appear.  
;; Type M-x insert-kbd-macro and type in the name.  The code will
;; automatically be generated.

;; =================================================================
;; My preferred key bindings
;; =================================================================

;; go to specific line in current buffer
(global-set-key "\C-cg" 'goto-line)

;; Eat space at point up to non-space
(global-set-key "\C-ce" 'fixup-whitespace)

;; Remap Home and End keys to move within current line, and
;; C-Home and C-End keys to beginning and end of buffer
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [\C-home] 'beginning-of-buffer)
(global-set-key [\C-end] 'end-of-buffer)

;; Other misc keybindings
(global-set-key "\C-t" 'scroll-cursor-to-top)
(global-set-key "\C-ct" 'scroll-cursor-to-bottom)
(global-set-key "\C-x5k" 'kill-buffer-delete-frame)

;; Make the % key jump to the matching {}[]() if on another, like VI
;;(global-set-key "%" 'match-paren)

(global-set-key [f2] 'undo)
(global-set-key [f4] 'next-error)
(global-set-key "\M-i" 'indent-region)

;;(define-key dired-mode-map "O" 'dired-find-file-other-frame)



;; =================================================================
;; I HATE IT When you're scrolling with the wheel and accidentally
;; insert a copy region.  Yuck - you dont even know you did it!!!!
;; =================================================================
(global-unset-key [mouse-2])


;; =================================================================
;;
;; =================================================================
;;(global-set-key "\M-\C-f" 'insert-function-header)

;;(define-key c++-mode-map "\M-\C-m" 'insert-function-header)
;;(define-key c++-mode-map "\M-\C-c" 'fc-insert-separator)

;; =================================================================
;; 
;; =================================================================
;;(setq printer-name "LPT2:")                      ; non-standard port
(setq printer-name "//HALEY/4M-BackHall")  ; network printer

;; =================================================================
;; Java mode stuff
;; =================================================================

(defun my-java-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0) ; this is the one you care about
  (c-set-offset 'statement-case-open '-)
  (c-set-offset 'case-label '+)
;;  (c-set-offset 'block-open '-)
;;  (c-set-offset 'inclass '-)
  (c-set-offset 'inline-open 0)
  (setq tab-width 4
  		;; make sure spaces are used instead of tabs
  		indent-tabs-mode nil)
  (message "my-java-mode-hook function executed"))

(add-hook 'java-mode-hook 'my-java-mode-hook)


;; =================================================================
;; JDE Mode stuff
;; =================================================================

;; Set the debug option to enable a backtrace when a
;; problem occurs.
(setq debug-on-error nil)


;; Defer loading the JDE until I open a Java file
(setq defer-loading-jde t)

(if defer-loading-jde
	(progn
	  (autoload 'jde-mode "jde" "JDE mode." t)
	  (setq auto-mode-alist
 			(append
 			 '(("\\.java\\'" . jde-mode))
 			 auto-mode-alist)))
  (require 'jde))

(defun my-jde-mode-hook ()
   (setq c-basic-offset 4)
   (c-set-offset 'substatement-open 0) ; this is the one you care about
   (c-set-offset 'statement-case-open '-)
   (c-set-offset 'case-label '+)
   (setq tab-width 4
  		;; make sure spaces are used instead of tabs
  		indent-tabs-mode nil)
   (message "my-java-mode-hook function executed"))

(add-hook 'jde-mode-hook 'my-jde-mode-hook)


;; ; The path to PowerShell
;; (setq exec-path (cons "C:/WINDOWS/system32/windowspowershell/v1.0" exec-path))

;; ; Filename of the PowerShell shell
;; (setq explicit-shell-file-name "powershell")

;; ; Tell Emacs to use PowerShell
;; (setq shell-file-name explicit-shell-file-name)

;; ; Argument to use when executing a single command
;; (setq shell-command-switch "-Command")

;; ; Arguments when starting an interactive shell
;; (setq explicit-powershell-args '("-Command" "-"))


; ;; Include the following only if you want to run
; ;; bash as your shell.

;; Setup Emacs to run bash as its primary shell.
(setq shell-file-name "bash")
(setq shell-command-switch "-c")
(setq explicit-shell-file-name shell-file-name)
(setenv "SHELL" shell-file-name)
(setq explicit-sh-args '("-login" "-i"))
(if (boundp 'w32-quote-process-args)
	(setq w32-quote-process-args ?\")) ;; Include only for MS Windows.


;; =================================================================
;; C# Mode Stuff
;; =================================================================
(autoload 'csharp-mode "cc-mode")

;; (c-add-style "myC#Style"
;;   '("C#"
;;   (c-basic-offset . 2)
;;   (c-comment-only-line-offset . (0 . 0))
;;   (c-offsets-alist . (
;;     (c                     . c-lineup-C-comments)
;;     (inclass		   . 0)
;;     (namespace-open	   . +)
;;     (namespace-close	   . +)
;;     (innamespace	   . 0)
;;     (class-open		   . +)
;;     (class-close	   . 0)
;;     (inclass		   . 0)
;;     (defun-open		   . +)
;;     (defun-block-intro     . 0)
;;     (inline-open	   . ++)
;;     (statement-block-intro . 0)
;;     (brace-list-intro      . +)
;;     ))
;;   ))

 (setq my-csharp-style
 	  '((c-auto-newline . t)
 		(c-cleanup-list . (scope-operator empty-defun-braces defun-close-semi))
 		(c-offsets-alist . ((arglist-close . c-lineup-arglist)
 							(block-open . -)
 							(substatement-open . 0)))))

(add-hook 'csharp-mode-hook 
		  (function (lambda () 
					  (c-add-style "my-cs-style" my-csharp-style t))))

(require 'cc-mode)
(c-initialize-cc-mode)


(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" . 
                                 visual-basic-mode)) auto-mode-alist))

;; (c-add-style "myC#Style"
;;   '("C#"
;;   (c-basic-offset . 2)
;;   (c-comment-only-line-offset . (0 . 0))
;;   (c-offsets-alist . (
;;     (c                     . c-lineup-C-comments)
;;     (inclass		   . 0)
;;     (namespace-open	   . +)
;;     (namespace-close	   . +)
;;     (innamespace	   . 0)
;;     (class-open		   . +)
;;     (class-close	   . 0)
;;     (inclass		   . 0)
;;     (defun-open		   . +)
;;     (defun-block-intro     . 0)
;;     (inline-open	   . ++)
;;     (statement-block-intro . 0)
;;     (brace-list-intro      . +)
;;     ))
;;   ))


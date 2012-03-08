;/* .emacs (-*-Emacs-Lisp-*-)
;**============================================================================
;*/

(setq max-specpdl-size 1000)


;
; setup to load the jde stuff from one directory
;
(setq load-path (cons "../../../lisp/progmodes" load-path))
(setq load-path (cons "../../../lisp/emacs-lisp" load-path))

;
; This works with the latest version of cc-mode (5.21).
; Verify with M-X c-version
;
(load "cc-mode")
(c-add-style "myjavastyle"
  '("java"
  (c-basic-offset . 2)
  (c-comment-only-line-offset . (0 . 0))
  (c-offsets-alist . (
    (c                     . c-lineup-C-comments)
    (inclass		   . 0)
    (class-open		   . +)
    (class-close	   . +)
    (inline-open	   . ++)
    (defun-block-intro     . 0)
    (statement-block-intro . 0)
    ))
  ))

(c-add-style "myCstyle"
  '("gnu"
  (c-basic-offset . 2)
  (c-comment-only-line-offset . (0 . 0))
  (c-offsets-alist . (
    (c                     . c-lineup-C-comments)
    (brace-list-open . 2)
    (defun-open . 2)
    (inclass		   . 0)
    (class-open		   . +)
    (class-close	   . +)
    (inline-open	   . ++)
    (defun-block-intro     . 0)
    (statement-block-intro . 0)
    ))
  ))

(c-add-style "MyC#Style"
  '("java"
  (c-basic-offset . 2)
  (c-comment-only-line-offset . (0 . 0))
  (c-offsets-alist . (
    (c                     . c-lineup-C-comments)
    (inclass		   . 0)
    (class-open		   . +)
    (class-close	   . +)
    (inline-open	   . ++)
    (defun-block-intro     . 0)
    (statement-block-intro . 0)
    ))
  ))







;
; html helper
;
;;(setq load-path (cons "c:/emacs/site-lisp/html-helper-mode" load-path))
(setq load-path (cons "../../../site-lisp/html-helper-mode" load-path))
(load "html-helper-mode")


;
; gnuserv
;
;;(setq load-path (cons "c:/emacs/site-lisp/gnuserv" load-path))
(setq load-path (cons "../../../site-lisp/gnuserv" load-path))
(load "gnuserv")

;;(setq load-path (cons "c:/emacs/site-lisp/init" load-path))
(setq load-path (cons "../../../site-lisp/init" load-path))
(load "jinit")


(defun my-java-mode-hook ()
  (cond (window-system
;	 (setq imenu-generic-expression java-imenu-regexp)
	 (turn-on-font-lock)
	 (c-set-style "myjavastyle")
	 )))
(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun my-c-mode-hook ()
  (cond (window-system
	 (c-set-style "myCstyle")
	 )))

(defun my-csharp-mode-hook ()
  (cond (window-system
;;	 (setq imenu-generic-expression csharp-imenu-regexp)
	 (turn-on-font-lock)
	 (c-set-style "MyC#Style")
	 )))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'java-mode-hook 'my-c-mode-hook)
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(setq font-lock-maximum-decoration t)
(global-font-lock-mode)

(put 'eval-expression 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;
; Key Bindings
;

;
; most of these strange key bindings are from the original Tops-20 Emacs
; my fingers are still wired the old way
;
(fset 'kill-to-beg-of-line "0")
(global-set-key "\C-u" 'kill-to-beg-of-line)		; old ^U behavior
;(global-unset-key "\C-x\C-c")				; no exiting accidently
(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Really kill Emacs? "))
	    kill-emacs-query-functions))

(global-set-key "\C-w" 'backward-kill-word)		; old ^W behavior
(define-key esc-map "w" 'kill-region)			; old meta-w
(define-key esc-map "?" 'command-apropos)
(define-key esc-map "s" 'center-line)
(define-key esc-map "." 'set-mark-command)

(define-key ctl-x-map "s" 'save-buffer)
(define-key ctl-x-map "w" 'copy-region-as-kill)
(define-key ctl-x-map "c" 'compile)
(define-key ctl-x-map "g" 'goto-line)

(global-set-key "\C-h" 'quoted-insert)    

;(global-set-key "\C-_" 'help-command)
;(setq help-char "?C-_)

(define-key isearch-mode-map "\C-h" 'isearch-quote-char)
(define-key isearch-mode-map "\C-\\" 'isearch-repeat-forward)
(global-set-key "\C-r" 'isearch-backward)
(global-set-key "\C-\\" 'isearch-forward)

;(global-unset-key "\C-q")				; unbind xoff
;(global-unset-key "\C-s")				; unbind xon

;(global-unset-key "\C-x\C-q")				; unbind xoff
;(global-unset-key "\C-x\C-s")				; unbind xon

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; HTML mode
;
(setq html-helper-do-write-file-hooks t)
(setq html-helper-build-new-buffer t)
(setq html-helper-address-string "
<a href=\"http://drgdna/bmerrill\">Brad Merrill</a>
<a href=\"mailto:bmerrill@microsoft.com\">&lt;bmerrill@microsoft.com&gt;</a>
")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons "c:/emacs/site-lisp/vb" load-path))

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
                                  visual-basic-mode)) auto-mode-alist))
(autoload 'vbp-mode "visual-basic-mode" "VBP mode." t)
(setq auto-mode-alist (append '(("\\.\\(vbg\\|vbg\\)$" .
                                  vbp-mode)) auto-mode-alist))
(setq visual-basic-ide-pathname "d:/msvs/VB98/VB6.EXE")

(autoload 'vbp-mode "vbp-mode" "VBP mode." t)
(setq auto-mode-alist (append '(("\\.vbp$" .
                     vbp-mode)) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; extension to mode mapping
;
(setq auto-mode-alist
      (append '(
		("\\.s?html?\\'" . html-helper-mode)
		("\\.asp$" . html-helper-mode)
		("\\.aspx$" . html-helper-mode)
		("\\.html$" . html-helper-mode)
		("\\.htm$" . html-helper-mode)
                ("\\.md$" . emacs-lisp-mode)
		("\\.txt$" . text-mode)
		("\\.cs$" . csharp-mode)
		) auto-mode-alist ))


;
; Compiler error matcher (compil.el)
;
(setq-default compilation-error-regexp-alist
 '(
 ; Microsoft JVC:
 ;sample.java(6,1) : error J0020: Expected 'class' or 'interface'
 ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)) : \\(error\\|warning\\) J[0-9]+:" 1 3 4)

 ;C# Compiler
 ;t.cs(6,18): error SC1006: Name of constructor must match name of class
 ;
 ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): \\(error\\|warning\\) SC[0-9]+:" 1 3 4)

 ; Microsoft C/C++:
 ;  keyboard.c(537) : warning C4005: 'min' : macro redefinition
 ;  d:\tmp\test.c(23) : error C2143: syntax error : missing ';' before 'if'
 ;VC EEi
 ;e:\projects\myce40\tok.h(85) : error C2236: unexpected 'class' '$S1'
 ;myc.cpp(14) : error C3149: 'class System::String' : illegal use of managed type 'String'; did you forget a '*'?
    ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \
: \\(error\\|warning\\) C[0-9]+:" 1 3)
 ))


(setq-default compile-command "nmake")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; bash setup
;
;(setq binary-process-input t) 
;(setq w32-quote-process-args ?\") 
;(setq shell-file-name "bash") ;; or sh if you rename your bash executable to sh. 
;(setenv "SHELL" shell-file-name) 
;(setq explicit-shell-file-name shell-file-name) 
;(setq explicit-sh-args '("-login" "-i"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Misc
;

(setq auto-save-interval 200)
(setq-default case-fold-search t)
(setq-default comment-column 40)
(setq completion-auto-help nil)
(setq enable-recursive-minibuffers t)
(setq-default fill-column 64)
(setq inhibit-startup-message t)
(setq insert-default-directory nil)



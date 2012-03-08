;; =================================================================
;; =================================================================
;; Filename: .emacs
;; Emacs initialization file
;; Dave Gallucci
;; work email galluccd@nasd.com
;; home email dgall812@concentric.net
;; =================================================================
;; This .emacs initialization file has evolved over many months,
;; and you'll often see code or even comments that I stole from
;; other sample .emacs files, dejanews posts, lisp files, or the
;; GNU pages. I am convinced that Emacs can be made to do anything
;; I will ever need. ( or any other programmer for that matter )
;; If you think Emacs is just an editor, you don't yet understand
;; what Emacs is all about. At one time, I used to get caught up
;; in the Emacs vs. VI thing, then decided to really learn Emacs.
;; Though it edits, it is a total coding environment, fully
;; extensible and able to do practically anything, include amusing
;; diversions we all need after banging out a few thousand lines of
;; code. If you see anything in this file that I can improve on,
;; please let me know. Learning Emacs is a journey that doesn't end.
;; =================================================================
;; =================================================================

;; =================================================================
;; Begin Customizations
;; =================================================================
;; Stuff that has to be first according to notes
;; =================================================================

;; SET THIS FIRST!! EVERYTHING ELSE DEPENDS ON THIS VARIABLE!
(setenv "HOME" "c:/emacs")

;; add private lisp directory to load-path
(add-to-list 'load-path "~/lisp/elisp")

;; which-func-mode ( From gnu emacs FAQ )
;; If you set which-func-mode-global via customize, which-func-mode
;; will not turn on automatically. You need to add the following to
;; your startup file BEFORE the call to custom-set-variables: 
(which-func-mode 1)

;; If you want to have comments displayed in italics,
;; uncomment the following line. Note that this must
;; be done before font settings! (Emacs 20)
;; (setq w32-enable-italics t)

;; =================================================================
;; Emacs auto customize section
;; =================================================================

;; Start custom section. emacs will  add to this section when
;; you use the customize stuff on the help menu

(custom-set-variables
 '(rmail-mail-new-frame t t)
 '(inhibit-startup-message t)
 '(transient-mark-mode t)
 '(which-func-mode-global t nil (which-func))
 '(scroll-step 1)
 '(scroll-conservatively 1 t)
 '(find-file-run-dired t)
 '(font-lock-support-mode (quote fast-lock-mode))
 '(global-font-lock-mode t nil (font-lock))
 '(global-auto-revert-mode t nil (autorevert))
 '(make-backup-files nil)
 '(font-lock-global-modes t))

;; =================================================================
;; EMACS general look and feel 
;; =================================================================

;; I like to know what time it is. These lines show the clock in
;; the status bar. Comment out first line if you prefer to show
;; time in 12 hour format
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; Shut off annoying sound
(set-message-beep 'silent)

;; Set the icon and frame titles %f file name, %b buffer name
(setq frame-title-format "%b")
(setq icon-title-format "%f")

;; show column number in status bar
(setq column-number-mode t)

;; make searches case-INsensitive
(set-default 'case-fold-search t)

;; Load gnuserv. This is required for things such as running emacs
;; as the default MSVC editor. I usually start emacs once in the
;; morning, then keep it running all day as my only editor
(require 'gnuserv)
(gnuserv-start)
(setq gnuserv-frame (selected-frame))

;; Set colors like this, if desired
;;(set-background-color "wheat")
;;(set-foreground-color "black")

;; This maps edit keys to standard Windows keystokes. It requires the
;; library cua-mode.el from Kim Storm at the following URL:
;; http://eolicom.olicom.dk/~storm/cua-mode.el
;; (load "cua-mode")
;; (CUA-mode t)

;; Enable uppercase or lowercase conversions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; This lib makes for nice buffer handling when many files are open
;; To invoke menu, hit C + mouse 1
(load "msb")

;; If you change font sizes, you'll probably have to tweak these

;; Settings for work: 17 inch monitor
;; running 1024 x 768 resolution
(set-frame-height (selected-frame) 48)
(set-frame-width (selected-frame) 130) 

;; Settings for home: 15 inch monitor
;; running 600 x 800 resolution
;; (set-frame-height (selected-frame) 38)
;; (set-frame-width (selected-frame) 108)

;; To obtain new font string, execute eval-expression, and eval this:
;; (insert(prin1-to-string(w32-select-font)))
;; This will show the required string in the scratch buffer.
(set-default-font  "-*-Courier New-normal-r-*-*-11-82-96-96-c-*-iso8859-1")
;; (make-face-italic 'font-lock-comment-face)

;; Add Emacs close confirmation
(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Really kill Emacs? "))
     kill-emacs-query-functions))

;; Get rid of old versions of files
(setq delete-old-versions t)

;; Shut off warning messages when using system shell
(setq w32-allow-system-shell t)

;; Stop ^M's from displaying in system shell window
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;; This prevents shell commands from being echoed
(defun my-comint-init ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'my-comint-init)

;; Shut off message buffer. Note - if you need to debug emacs,
;; comment these out so you can see what's going on.
;; (setq message-log-max nil)
;; (kill-buffer "*Messages*")

;; Automatically turn on auto-fill-mode when editing text files

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; If you want to change the word wrap column, change this number
;; (setq-default fill-column 65)

;; =================================================================
;; Print Configuration
;; =================================================================

;; This requires GhostScript which is available at this URL:
;; http://www.cs.wisc.edu/~ghost/aladdin
;; A note on the gs options:
;; -q                      quiet
;; -sDEVICE=mswinpr2       uses default windows printer
;; -dNOPAUSE               don't wait for user intervention
;; Path to gstools
;; -IC:/GSTOOLS/gs5.50;c:/GSTOOLS/gs5.50/fonts
;; -c quit                 this is added at the end to terminate gs

(require 'ps-print)
;; (setq ps-paper-type 'ps-letter)
(setq ps-paper-type 'letter)
(setq ps-lpr-command "c:/gstools/gs5.50/gswin32")
;; the GhosScript options
(setq ps-lpr-switches '("-q -sDEVICE=mswinpr2 -dNOPAUSE -IC:/GSTOOLS/gs5.50;c:/GSTOOLS/gs5.50/fonts")) 
;; temporary spool
(setq ps-lpr-buffer "~/spool/psspool.ps")      

(defun win32-ps-print-buffer ()
  (interactive)
  (ps-print-buffer ps-lpr-buffer)
  (shell-command
   (apply 'concat (append (list ps-lpr-command " ")
                          ps-lpr-switches
                          (list " " ps-lpr-buffer " -c quit"))))
  )

(define-key global-map "\C-cp" 'win32-ps-print-buffer)

;; =================================================================
;; Compiling with MS Visual Studio
;; =================================================================

;; Note that the lib and include environment variables must be
;; set, and they must point to all directories containing headers
;; and static libs required for your build

;; (setq compile-command "NMAKE /f \"SCTS.mak\" \"SCTS\" - Win32 Debug\"")
;; (setq compile-command "NMAKE /f \"SCTS.mak\" \"SCTS\" - Win32 Release\"")
;; (setq compile-command "NMAKE /f \"nqds_compare.mak\" \"nqds_compare\" - Win32 Debug\"")
(setq compile-command "NMAKE /f \"nqds_compare.mak\" \"nqds_compare\" - Win32 Release\"")

;; =================================================================
;; Jari Aalto's Tiny Library
;; =================================================================

;; Defines everything, publishes interface for tiny library. This
;; library has a LOT of useful things and is available at
;; ftp://cs.uta.fi/pub/ssjaaa/ema-tiny.html
(require 'tinylibm)

;; search fwd or bkwd for word under cursor
(autoload 'tisw-search-word-forward "tinysword" t t)
(autoload 'tisw-search-word-backward "tinysword" t t)
(global-set-key [f2]   'tisw-search-word-forward)
(global-set-key [S-f2] 'tisw-search-word-backward)

(autoload 'c-comment-edit "c-comment-edit2"
  "No information available. See lisp file c-comment-edit2 for full documentation" t)
(autoload 'c-comment-edit-end "c-comment-edit2"
  "No information available. See lisp file c-comment-edit2 for full documentation" t)

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
;; Code Related Settings
;; =================================================================

;; These setting are written in the order as described in the Emacs
;; info pages. ( Hit C-hi, then go to Emacs | Programs | Program
;; Indent | Custom C Indent | Syntactic Symbols for a description of
;; each. I found it easier to open one of my own source files, and
;; hit tab on a particular line to find the name of the syntactic
;; symbol. This assumes that the setting for 
;; c-echo-syntactic-information-p is not nil. )

(defconst my-c-style
  '(
    (c-echo-syntactic-information-p . t)
    (c-basic-offset . 4)
    (c-toggle-auto-state . t)
    (c-offsets-alist .
                     ((string                . +)
                      (c                     . +)
                      (defun-open            . 0)
                      (defun-close           . 0)
                      (defun-block-intro     . +)
                      (class-open            . 0)
                      (class-close           . 0)
                      (inline-open           . 0)
                      (inline-close          . 0)
                      (extern-lang-open      . 0)
                      (extern-lang-close     . 0)
                      (func-decl-cont        . 0)
                      (knr-argdecl-intro     . +)
                      (knr-argdecl           . +)
                      (topmost-intro         . 0)
                      (topmost-intro-cont    . +)
                      (member-init-intro     . +)
                      (member-init-cont      . +)
                      (inher-intro           . +)
                      (inher-cont            . +)
                      (block-open            . 0)
                      (block-close           . 0)
                      (brace-list-open       . 0)
                      (brace-list-close      . 0)
                      (brace-list-intro      . +)
                      (brace-list-entry      . +)
                      (statement             . 0)
                      (statement-cont        . ++)
                      (statement-block-intro . +)
                      (statement-case-intro  . +)
                      (statement-case-open   . 0)
                      (substatement          . +)
                      (substatement-open     . 0)
                      (case-label            . +)
                      (access-label          . -)
                      (label                 . 0)
                      (do-while-closure      . 0)
                      (else-clause           . 0)
                      (catch-clause          . 0)
                      (comment-intro         . 0)
                      (arglist-intro         . +)
                      (arglist-cont          . 0)
                      (arglist-cont-nonempty . +)
                      (arglist-close         . +)
                      (stream-op             . +)
                      (inclass               . +)
                      (inextern-lang         . +)
                      (cpp-macro             . 0)
                      (friend                . 0)
                      (objc-method-intro     . +)
                      (objc-method-args-cont . +)
                      (objc-method-call-cont . +)
                      ))


    (c-comment-only-line-offset . (0 . -1000))
    (c-hanging-braces-alist        . ((substatement-open after)
                                      (brace-list-open)))
    (c-hanging-colons-alist . ((member-init-intro before)
                               (inher-intro)
                               (case-label after)
                               (label after)
                               (access-label after)))
    (c-cleanup-list . (scope-operator
                       empty-defun-braces
                       defun-close-semi))
    )
  "Gallucci C++ Programming Style")

(defun my-c-mode-common-hook ()
  (c-add-style "gallucci" my-c-style t)
  (c-set-offset 'member-init-intro '+)
  (setq tab-width 4
        indent-tabs-mode nil
        font-lock-maximum-decoration t)
  (c-toggle-auto-hungry-state 1)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; So I always have the proper number of parenthesis
(show-paren-mode 1)

;; Make the % key jump to the matching {}[]() if on another, like VI
(global-set-key "%" 'match-paren)      
  
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


;; This allows you to keep a tags stack and push and pop your
;; way around the source code. This feature is present in both
;; XEmacs and VI. Pop tag is mapped to M-*.

(global-set-key (read-kbd-macro "M-*") 'tags-return)

(defvar tags-stack nil)

(defun tags-stack-push (el)
  (setq tags-stack (cons el tags-stack)))

(defun tags-stack-pop ()
  (let ((el (car tags-stack)))    
    (setq tags-stack (cdr tags-stack))
    el))

(defadvice find-tag (before push-tag activate)  
  (or (ad-get-arg 1)
      (tags-stack-push (cons (current-buffer) (point)))))

(defadvice tags-search (before push-tag activate)
  (tags-stack-push (cons (current-buffer) (point))))

(defun tags-return ()
  (interactive) 
  (let* ((el (tags-stack-pop))  
         (buffer (car el))
         (point  (cdr el)))
    (if buffer (switch-to-buffer buffer))
    (if point (goto-char point))))

;; =================================================================
;; Version Control Systems
;; =================================================================

;; PVCS. You will need pvcs.el.
;; (require 'pvcs)
;; (setq vc-default-back-end (quote RCS))
;; (setq vc-consult-headers nil)
;; (setq vc-mistrust-permissions t)

; =================================================================
;; SMTP Outgoing Mail Settings
;; =================================================================

(setq user-full-name "David Gallucci")
(setq user-mail-address "dgall812@concentric.net")

;; Use signature file. Defaults to ~/.signature
;; (setq mail-signature t)

(setq smtpmail-default-smtp-server "smtp.concentric.net")
(setq smtpmail-local-domain nil)
(setq send-mail-function 'smtpmail-send-it)

(load-library "smtpmail")
(load-library "supercite")
(setq sc-nested-citation-p t)

;; =================================================================
;; POP3 Incoming Mail Settings
;; =================================================================

;; Requires epop3.el by Franklin Lee. You can get it at:
;; http://www.cs.washington.edu/homes/voelker/ntemacs.html#mail-leave-rmail

;; Uncomment these lines if NOT using epop3...
;; (setenv "MAILHOST" "pop3.concentric.net")
;; (setq rmail-pop-password-required t)


;; Sample for checking numerous pop3 servers
;;    (setq rmail-primary-inbox-list
;;          '("po:me@mypopserver.domain.com"
;;            "po:m3@anotherserver.elsewhere.com"
;;            . . . ))

;; (setq rmail-primary-inbox-list
;;       '("po:dgall812@pop3.concentric.net"
;;  ))

;; ... and comment out all this stuff
;; up to and including the require line

;; Tell me when there's new mail. epop3-mail also allows you to leave
;; mail on server
;; (autoload 'epop3-mail "epop3mail"
;;   "Get mail from pop server for PO:USER@HOST and put it in TOFILE." t)

;; (autoload 'start-biff "epop3mail" "pop3 biff, unleashed" t)
;; (autoload 'stop-biff "epop3mail" "pop3 biff, muzzled" t)
;; (autoload 'restart-biff "epop3mail" "pop3 biff, RE-unleashed" t)
;; (autoload 'flush-pop-passwords "epop3mail" "flush passwords" t)
;; (autoload 'biffs-current-language "epop3mail" "what is biff talking?" t)
;; (autoload 'biffs-last-check "epop3mail" "when did biff last check?" t)
;; (autoload 'speak-biff! "biff-mode" "make biff speak" t)

;; (require 'epop3mail)

;; epop3 options

;;  Set this to non-nil for 'biff' to display the # of unread messages.
;; (setq epop3-biff-show-numbers t)

;; Set this to non-nil for 'biff' to show '[Mail:  ]' in the modeline
;; (setq epop3-biff-show-mail-string t)

;; Tell biff to start checking mail now, and do so every 15 minutes
;; (start-biff 15 t)

;; =================================================================
;; GNUS Stuff
;; =================================================================

(setq gnus-cache-directory "~/news/cache/")
(setq gnus-home-directory "~/news/")
(setq gnus-startup-file "~/news/.newsrc")
(setq gnus-default-nntp-server "news.concentric.net")
(setq gnus-user-full-name "David Gallucci")
(setq gnus-user-from-line "dgall812@concentric.net")
(setq gnus-use-generic-from "David Gallucci")
(setq mail-host-address "concentric.net")
(setq gnus-local-organization "Not Listed")
(setq gnus-secondary-select-methods '((nnml "")))
(setq nnmail-pop-password-required t)
(setq gnus-use-cache t)
(setq message-yank-prefix ">")

;; =================================================================
;; My preferred key bindings
;; =================================================================

;; This allows c-tab and c-s-tab switching
;; Author: Igor Boukanov <boukanov@fi.uib.no>
(require 'pc-bufsw)
(pc-bufsw::bind-keys [C-tab] [C-S-tab])

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

;; Use F4 key to toggle speedbar. Speedbar URL is
;; http://www.ultranet.com/~zappo/speedbar.shtml
(global-set-key [(f4)] 'speedbar-get-focus)

;; =================================================================
;; Mini-functions, macros, and other personal hacks
;; =================================================================

;; kill current buffer without confirmation
(global-set-key "\C-xk" 'kill-current-buffer)

(defun kill-current-buffer ()
  "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

;; =================================================================

;; kill-buffer-other-window
(global-set-key [f12] 'kill-buffer-other-window)

(defun kill-buffer-other-window (arg)
  "Kill the buffer in the other window, and make the current buffer full size. If no
other window, kills current buffer."
  (interactive "p")
  (let ((buf (save-window-excursion
        (other-window arg)
        (current-buffer))))
    (delete-windows-on buf)
    (kill-buffer buf))  )

;; =================================================================

;; kill-buffer-jump-other-window

;; uncomment to activate
(global-set-key [f11] 'kill-buffer-jump-other-window)

(defun kill-buffer-jump-other-window (arg)
  "Kill this buffer and jump to other window."
  (interactive "p")
    (other-window arg)
    (kill-buffer-other-window arg)  )

;; =================================================================

;; Good for opening header file under cursor
(global-set-key "\C-cf" 'open-file-under-cursor)

(fset 'open-file-under-cursor
   [?\C-\M-b ?\C-  ?\C-\M-f ?\C-\M-f ?\M-w ?\C-x ?\C-f ?\C-y return])

;; =================================================================

;; Set line with cursor to top of window
(global-set-key "\C-ct" 'scroll-cursor-to-top)

(fset 'scroll-cursor-to-top
   "\C-u1\C-l")

;; =================================================================

;; Copy sexp under cursor into register. Somewhat of a hack since
;; marked region remains active following copy. Macro will call
;; unmark-region. If there's a better way, show me!
(global-set-key "\C-cc" 'copy-word-under-cursor)

(fset 'copy-word-under-cursor
   [?\C-\M-b ?\C-  ?\C-\M-f ?\C-x ?r ?s ?l ?\M-x ?t ?i ?: ?: ?t ?- ?u ?n ?m ?a ?r ?k ?- ?r ?e ?g ?i ?o ?n return])

;; =================================================================

;; Replace sexp under cursor with sexp previously copied into register
;; with above function "copy-word-under-cursor". I needed this so I
;; could do same paste operation without having first kill replace
;; what I wanted to paste
(global-set-key "\C-cv" 'replace-word-under-cursor)

(fset 'replace-word-under-cursor
   [?\C-\M-b ?\C-  ?\C-\M-f ?\C-w ?\C-x ?r ?i ?l])

;; =================================================================

;; Set line with cursor to bottom of window
(global-set-key "\C-cb" 'scroll-cursor-to-bottom)

(fset 'scroll-cursor-to-bottom
   "\C-u-1\C-l")

;; =================================================================

;; Insert header file template from
;; ~/templates directory
(defun insert-header-skeleton ()  
  "Insert skeleton header file"
  (interactive)  
  (insert-file-contents "~/templates/header.h"))
(global-set-key [f5] 'insert-header-skeleton)

;; =================================================================

;; Insert implementation file template from
;; ~/templates directory
(defun insert-cpp-skeleton ()  
  "Insert skeleton cpp file"
  (interactive)  
  (insert-file-contents "~/templates/cpp.cpp"))
(global-set-key [f6] 'insert-cpp-skeleton)

;; =================================================================

;; Insert function comment file template from
;; ~/templates directory
(defun insert-function-comment ()  
  "Insert function comment"
  (interactive)  
  (insert-file-contents "~/templates/function.cpp"))
(global-set-key [f7] 'insert-function-comment)

;; =================================================================

;; Insert function prototype in current header file and matching
;; function body in implementation file. Unfortunately, this does
;; not yet insert the class name and scope resolution operator.
(defun insert-new-method (proto)
  "Insert a method into the current header file at point. Insert
implementation, too.  This function expects the implementation file to
be named foo.cpp and in the same directory as the current file, foo.h."
  (interactive "MPrototype: ")
  (insert proto ";\n")
  (save-window-excursion
    (find-file (concat (file-name-sans-extension (buffer-file-name))
                       ".cpp"))
    (end-of-buffer)
    (insert"\n\n")
    (insert-function-comment))
  (save-window-excursion
    (find-file (concat (file-name-sans-extension (buffer-file-name))
                       ".cpp"))
    (end-of-buffer)
    (insert proto "\n{\n}\n")))

(global-set-key "\C-ci" 'insert-new-method)

;; =================================================================
;; Saving Emacs Sessions - Useful when you have a bunch of source
;; files open and you don't want to go and manually open each
;; one, especially when they are in various directories. Page 377
;; of the GNU Emacs Manual says: "The first time you save the state
;; of the Emacs session, you must do it manually, with the command
;; M-x desktop-save. Once you have dome that, exiting Emacs will
;; save the state again -- not only the present Emacs session, but
;; also subsequent sessions. You can also save the state at any
;; time, without exiting Emacs, by typing M-x desktop-save again.
;; =================================================================

(load "desktop")
(desktop-load-default)
(desktop-read)

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
;; =================================================================



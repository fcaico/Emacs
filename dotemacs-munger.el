;; Setup file called from _emacs
(progn 

  ;;; winNT specific
  (global-set-key [delete] "\C-d")
  (global-set-key [M-delete] "\M-d")
  (global-set-key [M-backspace] [?\M-\177]) 
;;  (load-library "stdname")
  (set-default-font "-*-Fixedsys-normal-r-*-*-12-90-*-*-c-*-*-ansi-")

  (if (not window-system)
      nil
    (setq default-frame-alist
	  (append default-frame-alist
		  '((top . 1)
		    (left . 20)
		    (width . 100)
		    (height . 45)
		    ;;(background-color . "Gray75")
		    (font .  "-*-Fixedsys-normal-r-*-*-12-90-*-*-c-*-*-ansi-")
		    ))))

  ;; set the icon and frame titles %f file name, %b buffer name
  (setq frame-title-format "%b")
  (setq icon-title-format "%f")


  ;;;; Bash support
  ;(setq shell-file-name "d:/unix/cygnus/H-i386-cygwin32/bin/bash")
  ;(setq shell-command-switch "-c")
  ;(setq win32-quote-process-args t)


  ;; wo-man support
  (autoload 'woman "woman"
    "Decode and browse a UN*X man page." t)
  (autoload 'woman-find-file "woman"
    "Find, decode and browse a specific UN*X man-page file." t)
  (setq woman-path '("d:\unix\cygnus\man" "e:\usr\man"))
  

  ;; tell emacs where to find TAGS files
  ;;(setq tags-table-list
  ;;   '("~/src/elisp" "f:/oak2/source"))
  (setq tags-file-name "f:/aw/classes/COM/OpT/agent/interfaceAgent")

  (setq load-path (append load-path (list "d:\\unix\\emacs\\local"
					  "e:\\home\\greg\\src\\elisp")))
  (load "cc-mode")

  
  ;; setup for diary and appointments
  (display-time)
  (add-hook 'diary-hook 'appt-make-list)

  ;; It is suggested that this will greatly improve performance, don't do it now.
  ;; Remove the menu bar and scroll bars (for better performance)
  (defun my-after-init-hook ()
    (menu-bar-mode nil)
    (scroll-bar-mode nil))
;;  (add-hook 'after-init-hook 'my-after-init-hook)


  ;;; end Windows specific ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (global-set-key [f3] 'kill-region)
  (global-set-key [f4] 'yank)
  (global-set-key [f1]  'bury-buffer)
  (global-set-key [f2]  'next-buffer)
  (global-set-key [f5]  'kill-this-buffer)
  (global-set-key [f6]  'kill-frame-and-buffer)
  (global-set-key [f7]  'advertised-undo)

  
  (load "gnuserv")
  (setq gnuserv-frame t)
  (gnuserv-start)

   ;;; C++ stuff
   (defconst my-c-style
    '("PERSONAL"
      (setq  c-basic-offset 3)
      (c-tab-always-indent           . t)
      (c-comment-only-line-offset    . 0)
      
      ;; I can't get this to stop putting a newline before open braces..
      (c-hanging-braces-alist        . ((block-open after)
					(brace-list-open)
					(defun-open)))
      (c-hanging-colons-alist        . ((case-label after)
					(inher-intro)
					(label after)))

      (c-cleanup-list                . (defun-close-semi))

      (c-echo-semantic-information-p . t)            
      (c-set-offset 'topmost-intro-cont (* -1 2))
      (c-set-offset 'block-open 0)	; line up braces with above statements
      (c-set-offset 'inclass 0)		; No extras spaces when in a class def
      ;; a fix for java
      (c-set-offset 'access-label 0)
;;      (c-set-offset 'access-label -2)
      
      (c-toggle-auto-state -1)		;turn off auto newlines (at braces, etc.)
      )
    "My C Programming Style"
    )

   ;; Customizations for both c-mode and c++-mode
   (defun my-c-mode-common-hook ()
     ;; set up for my perferred indentation style, but  only do it once
     (let ((my-style "PERSONAL"))
       (or (assoc my-style c-style-alist)
	   (setq c-style-alist (cons my-c-style c-style-alist)))
       (c-set-style my-style))

     (setq  c-basic-offset 3)
  
     ;; other customizations
     (setq tab-width 3
	   comment-column 70
	   indent-tabs-mode nil)		; use spaces are used instead of tabs
  
     (c-set-offset 'block-open '-)	; line up braces with above statements
     ;; a fix for java
     (c-set-offset 'access-label 0)
     ;; keybindings for both C and C++.  We can put these in c-mode-map
     ;; because c++-mode-map inherits it
     (define-key c-mode-map "\C-m" 'newline-and-indent) ; make RET indent
     )

   (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

   

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (cond (window-system
	  (setq hilit-mode-enable-list  '(not text-mode)
		hilit-background-mode   'light
		hilit-inhibit-hooks     nil
		hilit-inhibit-rebinding nil)
 
	  (require 'hilit19)
	  ))
   )

(defun kill-frame-and-buffer ()
  "kill the buffer and the frame it's in"
  (interactive)
  (kill-this-buffer)
  (delete-frame))

;; oppisite of previous buffer
(defun next-buffer ()
  "Go to the next buffer in the buffer list - GJM"
  (interactive)
  (switch-to-buffer (car (reverse (buffer-list)))))

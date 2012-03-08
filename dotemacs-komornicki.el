; .emacs file      Wed Sep 23 17:56:12 1992  Wojciech Komornicki
; 
(message "Loading .emacs")
(setq initial-frame-alist '((minibuffer . t) (name . "Wojciech Komornicki") (width . 100) (height . 35) (left . 5) (top . 5)))
(setq default-frame-alist '((minibuffer . t) (menu-bar-lines . 1) (width. 75) (height . 23)))
(setq-default inhibit-startup-message t)
(setq-default truncate-lines t)
(setq-default selective-display t)
(setq change-log-default-name "~/changelog")
(setq auto-save-list-file-prefix nil)
(setenv "LOGNAME" "wnk")

(add-hook 'run-after-load-hooks
  (function (lambda ()
    (message "Loading wnk customization")
    (setq 
      version-control t
      text-mode-hook (function (lambda ()
		     (setq fill-column 72)
		     (auto-fill-mode 1))))
     (message "done")
)))

(add-hook 'timecard-mode-hook
  (function (lambda ()
     (make-local-variable 'version-control)
     (setq version-control 'never))))

(defvar SIG "Wojciech Komornicki")

(defun sys()
  (interactive)
  (insert SIG)
)

(cond (window-system
       (require 'hilit19)
       (setq hilit-mode-enable-list  '(not text-mode)
             hilit-background-mode   'light
             hilit-inhibit-hooks     nil
             hilit-inhibit-rebinding nil)
 
        ))

(defun set-w-percentage (size)
  (let ((curr-w (get-buffer-window (current-buffer)))
    (n (- (window-height (get-buffer-window (current-buffer)))
          (/ (* size
	    (- (screen-height)
	       (window-height (minibuffer-window))))
	 100))))
    (if curr-w
    (save-excursion
      (select-window curr-w)
      (shrink-window n)))))
  
(defun adress()
  (interactive)
  (rolo "~/address")
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list '(28 48 52 55))
  (rolo-index-sort 0)
  (set-w-percentage 75)
)

(defvar wnk-window-configuration nil)

(defun save-win-conf ()
  (interactive)
  (setq wnk-window-configuration (current-window-configuration)))

(defun restore-win-conf ()
  (interactive)
  (set-window-configuration wnk-window-configuration))

(global-set-key [M-f12] 'save-win-conf)
(global-set-key [S-M-f12] 'restore-win-conf)

(defun vc-init-log ()
  "Insert an initial log entry at the end of a file."
  (interactive)
  (goto-char (point-max))
  (insert "/*
* RCS HISTORY
* $Header: $
* $Log: $
*/
"))

(put 'downcase-region 'disabled nil)

(defun indent-file ()
  (interactive)
  (c++-mode)
  (read-msdos nil)
  (while (search-forward "{" nil t)
    (backward-char 1)
    (c-indent-exp t)
    (forward-sexp 1)
    (next-line 1)
    ))

(defun trunc()
  (interactive)
  (let ((here (point)))
    (if (eq truncate-lines nil)
    (setq truncate-lines t)
      (setq truncate-lines nil))
    (recenter t)
    (goto-char here)
))

(modify-frame-parameters (selected-frame) '((name . "Wojciech Komornicki")))

(global-set-key "\C-x\C-t"     (make-sparse-keymap))
(global-set-key "\C-x\C-t\C-i" 'timecard-checkin)
(global-set-key "\C-x\C-t\C-o" 'timecard-checkout)
(global-set-key "\C-x\C-t\C-t" 'timecard-find-timecard)

(setq timecard-define-menus t)    ; only if you want menus

(autoload 'timecard-find-timecard "timecard" nil t)
(autoload 'timecard-checkin       "timecard" nil t)
(autoload 'timecard-checkout      "timecard" nil t)
(autoload 'timecard-mode          "timecard" nil t)

(defun dosify ()
  (interactive)
  (let (end)
    (end-of-line)
    (setq end (point))
    (beginning-of-line)
    (replace-string "/" "\\")
    (comint-send-input)
))

(setenv "SHELL" "c:\\4nt251\\4nt.exe")

(add-hook 'shell-mode-hook
  (function (lambda ()
    (define-key shell-mode-map "\M-\C-m" 'dosify))))

(defun sort-headers (beg end)
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (re-search-forward "\\s +$" nil t) 
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward ";\n" nil t)
      (replace-match "\n\f"))
    (sort-pages nil (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "\n\f" nil t)
      (replace-match ";\n"))
))

(defun do-nothing()
  (interactive)
)

(setq tex-default-mode 'latex-mode)
(setq shell-file-name "cmdproxy.exe")
(setq shell-command-switch "-c")
(setq win32-quote-process-args t)
(setq vc-default-back-end 'RCS)

(put 'eval-expression 'disabled nil)
(load "desktop")
(desktop-load-default)
(desktop-read)

;;======================================================================
;; fc-misc.el
;; This file contains miscellaneous functions for customizing
;; emacs' behavior.
;;
;; send mail to frank.caico@east.aht.com for more help or support.
;;======================================================================

(defun my-comint-init ()
  "prevents shell commands from being echoed"
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'my-comint-init)

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(provide 'fc-misc)


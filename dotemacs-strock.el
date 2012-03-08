(setq frame-title-format "%f - Emacs")
(setq icon-title-format  "%f - Emacs")

(setq message-log-max nil)
(kill-buffer "*Messages*")

(setq display-time-24hr-format t)


(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [\C-home] 'beginning-of-buffer)
(global-set-key [\C-end] 'end-of-buffer)

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist
(append (list '("\\.cc$"    .c++-mode)
              '("\\.hh$"    .c++-mode)
              '("\\.hpp$"   .c++-mode)
              '("\\.c$"     .c++-mode)
              '("\\.h$"     .c++-mode)
              '("\\.html$"  .html-helper-mode)
              '("\\.htm$"   .html-helper-mode))
          auto-mode-alist))

(setq win32-pass-alt-to-system t)
(setq default-frame-alist
      (append default-frame-alist
              '((top . 10)
                (left . 35)
                (width . 150)
                (height . 50)
                (background-color . "Wheat")
                (font .  "-*-6X13-medium-r-*-*-13-97-*-*-p-60-*-ansi-"))
              ))

(setq win32-pass-alt-to-system t)
(global-set-key [f5] 'replace-string)
(global-set-key [f6] 'goto-line)
(autoload 'follow-mode "follow"
    "Synchronize windows showing the same buffer, minor mode." t)

;; Turn on font-lock in all modes that support it
  (if (fboundp 'global-font-lock-mode)
       (global-font-lock-mode t))

  ;; Maximum colors
  (setq font-lock-maximum-decoration t)



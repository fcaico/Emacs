;;======================================================================
;; fc-buffers.el
;; This file contains functions which pertain to generic manipulation
;; of emacs buffers. Include it in your .emacs file to customize
;; emacs' behavior.
;;
;; send mail to frank.caico@east.aht.com for more help or support.
;;======================================================================

(defun kill-all-buffers()
  "Kills all current buffers, without confirmation."
  (interactive)
  (mapcar 'kill-buffer (buffer-list)))

;;----------------------------------------------------------------------
(defun kill-current-buffer-quietly ()
   "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))


;;----------------------------------------------------------------------
(defun kill-buffer-other-window (arg)
  "Kill the buffer in the other window, and make the current buffer full size. 
if no other window, kills current buffer."
  (interactive "p")
  (let ((buf (save-window-excursion
        (other-window arg)
        (current-buffer))))
    (delete-windows-on buf)
    (kill-buffer buf)))


; ;;----------------------------------------------------------------------
; (defadvice switch-to-buffer (before existing-buffer
;                                     activate compile)
;   "When interactive, switch to existing buffer only.,
; unless given a prefix argument."
;   (interactive
;    (list (read-buffer "Switch to buffer: "
;                       (other-buffer)
;                       (null current-prefix-arg)))))

; ;;----------------------------------------------------------------------
; (defadvice switch-to-buffer-other-window (before existing-buffer
;                                                  activate compile)
;   "When interactive, switch to existing buffer only.,
; unless given a prefix argument."
;   (interactive
;    (list (read-buffer "Switch to buffer in other window: "
;                       (other-buffer)
;                       (null current-prefix-arg)))))

; ;;----------------------------------------------------------------------
; (defadvice switch-to-buffer-other-frame (before existing-buffer
;                                                 activate compile)
;   "When interactive, switch to existing buffer only.,
; unless given a prefix argument."
;   (interactive
;    (list (read-buffer "Switch to buffer in other frame: "
;                       (other-buffer)
;                       (null current-prefix-arg)))))

;;----------------------------------------------------------------------
(defun kill-buffer-jump-other-window (arg)
  "Kill this buffer and jump to other window."
  (interactive "p")
    (other-window arg)
    (kill-buffer-other-window arg)  )

;;----------------------------------------------------------------------
(fset 'scroll-cursor-to-top 
   "\C-u0\C-l")

;;----------------------------------------------------------------------
(fset 'scroll-cursor-to-bottom
   "\C-u-1\C-l")

(provide 'fc-buffers)

;;======================================================================
;; fc-frames.el
;; This file contains functions which pertain to generic manipulation
;; of emacs frames/windows. Include it in your .emacs file to customize
;; emacs' behavior.
;;
;; send mail to frank.caico@east.aht.com for more help or support.
;;======================================================================

(require 'fc-buffers)

(defun dired-find-file-other-frame ()
  "Does what it says!"
  (interactive)
  (find-file-other-frame (file-name-sans-versions (dired-get-filename) t)))

(defun kill-buffer-delete-frame ()
  "This function kills the current buffer, deletes the current window 
and then switches focus to the next frame - making it visible if it is
not already."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-frame )
  (make-frame-visible (next-frame))
  (other-window 1))


(provide 'fc-frames)

;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pc-keys.el -- 
;; ITIID           : $ITI$ $Header $__Header$
;; Author          : System Administrator Account
;; Created On      : Wed Dec 22 19:47:52 1993
;; Last Modified By: System Administrator Account
;; Last Modified On: Tue Dec 13 10:29:32 1994
;; Update Count    : 18
;; Status          : Unknown, Use with caution!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This package provides some key rebindings to make the keys be more
;; PC software like. The name is actually misleading because there are
;; some other keybindings, too.
;; Oh, well ...

;;
;; Variables
;;

(defvar pc-keys-rebind-delete t

  "Set this to t if you want the delete key to delete the character
under the point. Set this to nil if you want the delete key to retain
its previous binding.

ATTENTION: This variable must be set *before* loading pc-keys!"

)

;;
;; Functions
;;

;;
;; for the home key
;;

(defun pc-keys-home ()

  "You need to bind this to a key, say the home key. Then, pressing
the home key once means go to beginning of line, pressing it twice in
a row means go to beginning of window, pressing it thrice in a row
means go to beginning of buffer. The time that elapses between the key
presses is irrelevant."

  (interactive)
  (let* ((keys (recent-keys))
         (len (length keys))
         (key1 (if (> len 0) (elt keys (- len 1)) nil))
         (key2 (if (> len 1) (elt keys (- len 2)) nil))
         (key3 (if (> len 2) (elt keys (- len 3)) nil))
         (key-equal-1 (eq key1 key2))
         (key-equal-2 (and key-equal-1 (eq key2 key3))))
    (cond (key-equal-2 (if mark-active
                           (goto-char (point-min))
                         (beginning-of-buffer)))
          (key-equal-1 (if mark-active () (push-mark))
		       (move-to-window-line 0))
          (t (beginning-of-line)))))

;;
;; for the end key
;;

(defun pc-keys-end ()

  "You need to bind this to a key, say the end key. Then, pressing the
end key once means go to end of line, pressing it twice in a row means
go to beginning of window, pressing it thrice in a row means go to
beginning of buffer. The time that elapses between the key presses is
irrelevant."

  (interactive)
  (let* ((keys (recent-keys))
         (len (length keys))
         (key1 (if (> len 0) (elt keys (- len 1)) nil))
         (key2 (if (> len 1) (elt keys (- len 2)) nil))
         (key3 (if (> len 2) (elt keys (- len 3)) nil))
         (key-equal-1 (eq key1 key2))
         (key-equal-2 (and key-equal-1 (eq key2 key3))))
    (cond (key-equal-2 (if mark-active
                           (goto-char (point-max))
                         (end-of-buffer)))
          (key-equal-1 (if mark-active () (push-mark))
		       (move-to-window-line -1)
		       (end-of-line))
          (t (end-of-line)))))

;;
;; for shift-control-cursor-down key
;;

(defun pc-keys-scroll-other-window-one-line-up ()

  "As the name implies this is similar to scroll-other-window."

  (interactive)
  (let ((scroll-in-place nil))
    (scroll-other-window 1)))

;;
;; for shift-control-cursor-up key
;;

(defun pc-keys-scroll-other-window-one-line-down ()

  "As the name implies this is similar to scroll-other-window."

  (interactive)
  (let ((scroll-in-place nil))
    (scroll-other-window -1)))

;;
;; for shift-control-page-down key
;;

(defun pc-keys-scroll-other-window-up ()

  "As the name implies this is similar to scroll-other-window."

  (interactive)

  (other-window 1)
  (scroll-up)
  (other-window -1))

;;
;; for shift-control-page-up key
;;

(defun pc-keys-scroll-other-window-down ()

  "As the name implies this is similar to scroll-other-window."

  (interactive)

  (other-window 1)
  (scroll-down)
  (other-window -1))

;;
;; for control-backspace key
;;

(defun pc-keys-kill-whole-line ()

  "Like kill-line but does not kill from end of line but from
beginning of line."

  (interactive)
  (beginning-of-line)
  (kill-line))

;;
;; for control-cursor-up key
;;

(defun pc-keys-scroll-down-one-line ()

  "Scrolls the visible part of the buffer one line down."

  (interactive)
  (let ((scroll-in-place nil))
    (scroll-down 1))
  )

;;
;; for control-cursor-down key
;;

(defun pc-keys-scroll-up-one-line ()

  "Scrolls the visible part of the buffer one line up."

  (interactive)
  (let ((scroll-in-place nil))
    (scroll-up 1))
  )

;;
;; for shift-cursor-up
;;

(defun pc-keys-previous-line ()

  "Scrolls one line but does not move the cursor relative to
window. Instead, the beginning of the window is changed."

  (interactive)
  (let ((scroll-in-place nil))
    (scroll-down 1)
    (previous-line 1)))

;;
;; for shift-cursor-down
;;

(defun pc-keys-next-line ()

  "Scrolls one line but does not move the cursor relative to
window. Instead, the beginning of the window is changed."

  (interactive)
  (let ((scroll-in-place nil))
    (scroll-up 1)
    (previous-line -1)))

;;
;; Key bindings
;;

(global-set-key [home] 'pc-keys-home)
(global-set-key [end] 'pc-keys-end)
(global-set-key [C-up] 'pc-keys-scroll-down-one-line)
(global-set-key [C-down] 'pc-keys-scroll-up-one-line)
(global-set-key [S-up] 'pc-keys-previous-line)
(global-set-key [S-down] 'pc-keys-next-line)
(global-set-key [M-up] 'backward-paragraph)
(global-set-key [M-down] 'forward-paragraph)
(global-set-key [C-S-up] 'pc-keys-scroll-other-window-one-line-down)
(global-set-key [C-S-down] 'pc-keys-scroll-other-window-one-line-up)
(global-set-key [C-S-prior] 'pc-keys-scroll-other-window-down)
(global-set-key [C-S-next] 'pc-keys-scroll-other-window-up)
(global-set-key [C-backspace] 'pc-keys-kill-whole-line)

(if pc-keys-rebind-delete
    (progn
      (global-set-key [delete] 'delete-char)
      (global-set-key [M-delete] 'kill-word)))

(provide 'pc-keys)


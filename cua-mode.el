;;; cua-mode.el --- emulate CUA key bindings

;; Copyright (C) 1997-2000 Free Software Foundation, Inc.

;; Author: Kim F. Storm <stormware@get2net.dk>
;; Maintainer: Kim F. Storm <stormware@get2net.dk>
;; Location: http://hjem.get2net.dk/storm/emacs/
;; Keywords: keyboard CUA
;; Version: 1.8-b1

;; This file is [not yet] part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is the CUA-mode package which provides a complete emulatation of
;; the standard CUA key bindings (Motif/Windows/Mac GUI) for selecting
;; and manipulating the region where S-<movement> is used to
;; highlight & extend the region.
;;
;; This package allow the C-z, C-x, C-c, and C-v keys to be
;; bound appropriately according to the Motif/Windows GUI, i.e.
;;	C-z	-> undo
;;	C-x	-> cut
;;	C-c	-> copy
;;	C-v	-> paste
;;
;; The tricky part is the handling of the C-x and C-c keys which
;; are normally used as prefix keys for most of emacs' built-in
;; commands.  With CUA-mode they still do!!!
;;
;; Only when the region is currently active (and highlighted since
;; transient-mark-mode is used), the C-x and C-c keys will work as CUA
;; keys
;; 	C-x -> cut
;; 	C-c -> copy
;; When the region is not active, C-x and C-c works as prefix keys!

;; This has a few drawbacks (such as not being able to copy the region
;; into a register using C-x r x), but CUA-mode automatically mirrors all
;; region commands using the [C-x r] prefix to use the [M-r] prefix as
;; well, so you can use M-r x to copy to a register.

;; ... and in the few cases where you make a mistake and delete the
;; region - you just undo the mistake (with C-z).

;; CUA-mode is based on "the best of" pc-selection-mode, s-region, and
;; delete-selection-mode packages with some extra features which I think
;; are unique to this package.

;; It works in a very homogeneous way (via a versatile pre-command-hook)
;; and without rebinding any of the cursor movement or scrolling
;; keys. The interpretation of C-x and C-c as either emacs prefix keys
;; or CUA cut/copy keys is handled via emacs' key-translation-map
;; feature.

;; A few more details:
;; 
;; * To activate CUA-mode, turn on CUA-mode via customize, or place this
;;   in your .emacs:
;; 	(CUA-mode t)
;; 
;; * When the region is highlighted, TAB and S-TAB will indent the entire
;;   region by the normal tab-width (or the given prefix arg).
;; 
;; * C-x C-x (exchange point and mark) no longer activates the mark (i.e. 
;;   highlights the region).  I found that to be confusing since the
;;   sequence C-x C-x (exchange once) followed by C-x C-x (change back)
;;   would then cut the region!  To activate the region in this way,
;;   use C-u C-x C-x.
;; 
;; * [delete] will delete (not copy) the highlighted region.
;; 
;; * The highlighted region is automatically deleted if other text is
;;   typed or inserted.
;; 
;; * Use M-r as a prefix for the region commands instead of C-x r.
;;   The original binding of M-r (move-to-window-line) is now on
;;   M-r M-r.


;;; Code:

;; Other configuration options

(defvar CUA-overriding-prefix-keys
  '((?\C-x "\C-x@\C-x" nil kill-region)
    (?\C-c "\C-x@\C-c" t   copy-region-as-kill))
  "List of prefix keys which are remapped via key-translation-map.
Each element in the list is a tuple with three elements:
The prefix key, the corresponding binding in key-translation-map, 
whether the binding is valid in read-only buffers, and
the function to call for that key if the region is active.")

(defvar CUA-movement-keys
  '((forward-char	right)
    (backward-char	left)
    (next-line		down)
    (previous-line	up)
    (forward-word	C-right)
    (backward-word	C-left)
    (end-of-line	end)
    (beginning-of-line	home)
    (end-of-buffer	C-end)
    (beginning-of-buffer C-home)
    (scroll-up		next)
    (scroll-down	prior)
    (forward-paragraph	C-down)
    (backward-paragraph	C-up))
  "List of cursor movement functions for which to create CUA key mappings.
Each element in the list is a list where the first element is the name of
the cursor movement function, and the rest of the list are keys to which
the function shall be bound.  For each key listed, both the key itself and
the shifted version S-key are bound to the specified function.")

(defvar CUA-region-commands
  '((del	; delete current region before command
     self-insert-command self-insert-iso insert-register
     newline-and-indent newline open-line)
    (del-ign	; delete current region and ignore command
     delete-backward-char backward-delete-char 
     backward-delete-char-untabify delete-char)
    (kill	; kill region before command
     )
    (kill-ign	; kill region and ignore command
     kill-region)
    (copy	; copy region before command
     )
    (copy-ign	; copy region and ignore command
     copy-region-as-kill)
    (yank	; replace region with element on kill ring
     yank clipboard-yank)
    (toggle	; explicitly toggle region active
     set-mark-command)
    (cancel	; cancel current region
     keyboard-quit)
    (ind-left	; indent all lines in region by same amount
     indent-for-tab-command tab-to-tab-stop c-indent-command)
    (ind-right	; unindent all lines in region by same amount
     CUA-indent-region-right))
  "Specifies how various editing functions behaves in CUA mode.
The value is a list of lists. For each element in the list, the
first element is an action and the rest of the list are names of
editing commands which shall perform the specified action if the
region is active and CUA mode is enabled.

The following actions operates on the region before executing the command:
  move       deactivate region if unshifted key, expand if shifted,
  del        delete the region
  kill       kill (copy and delete) the region
  copy       copy the region

The following actions operates on the region and ignores the command:
  del-ign    delete the region
  kill-ign   kill the region
  copy-ign   copy the region
  yank       replace region with element on kill ring,
  ind-left   move all lines in region to the left by same amount,
  ind-right  move all lines in region to the right by same amount,
  inhibit    inhibit next CUA command on region")

;; Misc variables

(defvar CUA-inhibit-next-command nil
  "Internally used variable set to inhibit the CUA-binding of the next key.")

(defvar CUA-explicit-region-start nil
  "Current region was started using set-mark-command.")

;;; User functions.

;;;###autoload
(defun CUA-exchange-point-and-mark (arg)
  "Exchanges point and mark, but don't activate the mark.
Activates the mark if a prefix argument is given."
  (interactive "P")
  (if arg
      (setq mark-active t)
    (exchange-point-and-mark)
    (setq mark-active nil)))

(defun CUA-indent-region-right (arg)
  "Move region to the right by the same amount."
  (interactive "P")
  ;; This is performed via the ind-right hook in CUA-pre-hook
  ;; Dummy if region is not active.
)

;;; Aux functions

(defun CUA-delete-active-region (op keep)
  (cond 
   ((eq op 'kill)
    (kill-region (point) (mark)))
   ((eq op 'copy)
    (copy-region-as-kill (point) (mark)))
   (t ; 'delete
    (delete-region (point) (mark))))
  (if keep
      (setq mark-active t
	    deactivate-mark nil)
    (setq mark-active nil)
    (run-hooks 'deactivate-mark-hook))
  t)

(defun CUA-indent-selection (arg backw)
  (message "Indenting...")
  (let ((a (point)) (b (mark)) c amount)
    (if (> a b) (setq c a a b b c))
    (save-excursion
      (goto-char a)
      (beginning-of-line)
      (setq a (point)))
    (if (equal arg '(4))
	(indent-region a b nil)
      (setq amount (if arg (prefix-numeric-value arg) tab-width))
      (indent-rigidly a b (if backw (- amount) amount))))
  (setq deactivate-mark t))


(defun CUA-prefix-override (prompt)
  (let (map)
    (if (and CUA-mode mark-active transient-mark-mode (not CUA-inhibit-next-command)
	     (= (length (this-command-keys)) 1))
	(setq map (assq last-input-char CUA-overriding-prefix-keys)))
    (if (and map (or (not buffer-read-only) (car (cdr (cdr map)))))
	(car (cdr map))
      (char-to-string last-input-char))))

(defun CUA-pre-hook ()
  "Function run prior to command to check for special region handling.
If current command is a movement and the key is shifted, set or expand
the region." 
  (let ((action (and CUA-mode transient-mark-mode
		     (symbolp this-command) (get this-command 'CUA)))
	(action2 (and CUA-mode transient-mark-mode
		      (symbolp this-command) (get this-command 'CUA2)))
	(inhibit-this CUA-inhibit-next-command)
	(ro buffer-read-only)
	(ignore nil))

    (setq CUA-inhibit-next-command nil)

    ;; If current command has the inhibit action, then we want the following behaviour:
    ;; - If the region is not active, execute the command
    ;; - If the region is active, inhibit next CUA command, and ignore command
    ;; - If the region is active, and we just did inhibit, then execute the command

    (if (eq action2 'inhibit)
	(if (and mark-active (not inhibit-this))
	    (setq CUA-inhibit-next-command t
		  action nil ; and this
		  ignore t)))
    (cond
     ((not action) nil)
     ((eq action 'toggle) 
      (if CUA-mode-enable-normal-region
	  (if mark-active
	      (setq mark-active nil
		    CUA-explicit-region-start nil)
	    (set-mark-command nil)
	    (setq CUA-explicit-region-start t))
	(setq ignore t)))
     ((eq action 'cancel) 
      (setq mark-active nil
	    CUA-explicit-region-start nil))
     ((eq action 'move)
      (if (or CUA-explicit-region-start
	      (memq 'shift (event-modifiers (aref (this-single-command-keys) 0))))
	  (and (not mark-active) (set-mark-command nil))
	(setq mark-active nil)))
     (mark-active
      (setq CUA-explicit-region-start nil)
      (if (not ro)
	  (cond
	   ((eq action 'kill)
	    (CUA-delete-active-region 'kill nil))
	   ((eq action 'kill-ign)
	    (setq ignore (CUA-delete-active-region 'kill nil)))
	   ((eq action 'yank)
	    ;; Before a yank command, make sure we don't yank
	    ;; the same region that we are going to delete.
	    ;; That would make yank a no-op.
	    (if (string= (buffer-substring (point) (mark))
			 (car kill-ring))
		(current-kill 1))
	    (CUA-delete-active-region 'delete nil))
	   ((eq action 'del-ign)
	    (setq ignore (CUA-delete-active-region 'delete nil)))
	   ((eq action 'del)
	    (CUA-delete-active-region 'delete nil))
	   ((eq action 'ind-left)
	    (setq ignore (CUA-indent-selection current-prefix-arg nil)))
	   ((eq action 'ind-right)
	    (setq ignore (CUA-indent-selection current-prefix-arg t)))
	   (t
	    (setq ro t))))
      (if ro ; or if not handled above
	  (cond
	   ((eq action 'inhibit)
	    (if (not inhibit-this)
		(setq CUA-inhibit-next-command t
		      ignore t)))
	   ((eq action 'copy)
	    (CUA-delete-active-region 'copy nil))
	   ((eq action 'copy-ign)
	    (setq ignore (CUA-delete-active-region
			  'copy CUA-mode-keep-region-after-copy)))
	   (t
	    (setq this-command (lookup-key global-map (this-command-keys)))
	    ))))
     (t
      (setq CUA-explicit-region-start nil)))
    (if ignore
	(setq this-command '(lambda () (interactive))))))


;;;###autoload
(defun CUA-mode (arg)
  "Toggle CUA keybinding mode.
When ON, C-x and C-c will cut and copy the selection if the selection
is active (i.e. the region is highlighted), and typed text replaces
the active selection. When OFF, typed text is just inserted at point."
  (interactive "P")
  (setq CUA-mode
	(if (null arg) (not CUA-mode)
	  (> (prefix-numeric-value arg) 0)))
  (when (and CUA-mode (not (get 'forward-char 'CUA)))
    (let ((list CUA-region-commands) type l)
      (while list
	(setq l (car list)
	      type (car l)
	      l (cdr l)
	      list (cdr list))
	(while l
	  (put (car l) 'CUA type)
	  (setq l (cdr l)))))
    (let ((list CUA-movement-keys) cmd l)
      (while list
	(setq l (car list)
	      cmd (car l)
	      l (cdr l)
	      list (cdr list))
	(while l
	  (put cmd 'CUA 'move)
	  (define-key global-map (vector (car l)) cmd)
	  (define-key global-map
	    (vector (intern (concat "S-" (symbol-name (car l))))) cmd)
	  (setq l (cdr l)))))

    ;; Map the C-zxcv keys according to CUA.

    (define-key global-map [?\C-z] 'advertised-undo)
    (define-key global-map [?\C-v] 'yank)
    (define-key ctl-x-map  [?\C-x] 'CUA-exchange-point-and-mark)

    (or key-translation-map
	(setq key-translation-map (make-sparse-keymap)))
    (let ((map CUA-overriding-prefix-keys))
      (while map
	(define-key key-translation-map
	  (vector (nth 0 (car map))) 'CUA-prefix-override)
	(define-key global-map (nth 1 (car map)) (nth 3 (car map)))
	(setq map (cdr map))))

    ;; Compatibility mappings

    (define-key global-map [S-insert]  'yank)
    (define-key global-map [M-insert]  'yank-pop)
    (define-key global-map [C-insert]  'copy-region-as-kill)
    (define-key global-map [S-delete]  'kill-region)

    ;; The following bindings are useful on Sun Type 3 keyboards
    ;; They implement the Get-Delete-Put (copy-cut-paste)
    ;; functions from sunview on the L6, L8 and L10 keys
    ;;  (define-key global-map [f16]  'yank)
    ;;  (define-key global-map [f18]  'copy-region-as-kill)
    ;;  (define-key global-map [f20]  'kill-region)

    ;; The following bindings are from Pete Forman and RMS.
    ;; I have disabled them because I prefer to map my own
    ;; function keys and I don't like M-bs to undo.  ++KFS

    ;;  (global-set-key [f1] 'help)		; KHelp         F1
    ;;  (global-set-key [f6] 'other-window)	; KNextPane     F6
    ;;  (global-set-key [delete] 'delete-char)  ; KDelete       Del
    ;;  (global-set-key [M-backspace] 'undo)	; KUndo         aBS

    (global-set-key [C-delete] 'kill-line)      ; KEraseEndLine cDel

    (define-key global-map [S-tab]     'CUA-indent-region-right)
    (setq mark-even-if-inactive t)
    (setq highlight-nonselected-windows nil)

    (add-hook 'pre-command-hook 'CUA-pre-hook))

  (setq transient-mark-mode CUA-mode))

;;; Register commands prefix remapping [C-x r ...]

(defun CUA-remap-ctl-x-commands (ctl-x-key prefix &optional no-orig)
  "Remap ctl-x commands [C-x r ...] onto [PREFIX ...].
Unless the optional third arguments NO-ORIG is non-nil, the original
binding of [PREFIX] is remapped to [PREFIX PREFIX]."
  (let ((org-prefix-cmd	(lookup-key global-map prefix))
	(new-prefix-cmd (lookup-key ctl-x-map ctl-x-key)))
    (if new-prefix-cmd
	(global-set-key prefix new-prefix-cmd))
    (if (and (not no-orig) 
	     new-prefix-cmd org-prefix-cmd
	     (not (eq new-prefix-cmd org-prefix-cmd)))
	(global-set-key (concat prefix prefix) org-prefix-cmd))))

;;; KEYPAD REMAPPING

(defun CUA-keypad-bind (kp bind)
  "Bind the keys in KP list to BIND list in function-key-map.
If BIND is 'unbind, all bindings for the keys are removed."
  (if (not (boundp 'function-key-map))
      (setq function-key-map (make-sparse-keymap)))
  (if (eq bind 'unbind)
      (while kp
	(define-key function-key-map (vector (car kp)) nil)
	(setq kp (cdr kp)))
    (while (and kp bind)
      (define-key function-key-map (vector (car kp)) (vector (car bind)))
      (setq kp (cdr kp)
	    bind (cdr bind)))))

  
;;;###autoload
(defun CUA-keypad-mode (mode &optional numlock decimal)
  "Set keypad bindings in function-key-map according to MODE.
If optional second argument NUMLOCK is non-nil, the NumLock On bindings
are changed. Otherwise, the NumLock Off binding are changed.

 Mode      Binding
 -------------------------------------------------------------
 'prefix   Command prefix argument, i.e.  M-0 .. M-9 and M--
 'cursor   Bind keypad keys to the cursor movement keys.
 'numeric  Plain numeric, i.e. 0 .. 9 and .  (or DECIMAL arg)
 'none     Removes all bindings for keypad keys in function-key-map.

If mode is 'numeric and the optional third argument DECIMAL is non-nil,
the decimal key on the keypad is mapped to DECIMAL instead of [.]."
  (let ((kp (if numlock
		'(kp-decimal kp-0 kp-1 kp-2 kp-3 kp-4 kp-5 kp-6 kp-7 kp-8 kp-9)
	      '(kp-delete kp-insert kp-end kp-down kp-next kp-left
			  kp-space kp-right kp-home kp-up kp-prior))))
    (CUA-keypad-bind
     kp 
     (cond
      ((eq mode 'none)
       'unbind)
      ((eq mode 'prefix)
       '(?\M-- ?\M-0 ?\M-1 ?\M-2 ?\M-3 ?\M-4 ?\M-5 ?\M-6 ?\M-7 ?\M-8 ?\M-9))
      ((eq mode 'cursor)
       '(delete insert end down next left space right home up prior))
      ((eq mode 'numeric)
       (cons (or decimal ?.) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
      (t
       (signal 'error (list "Unknown keypad mode: " mode)))))))


	   
;;;; Customization:

;; Set up the custom library.
;; taken from http://www.dina.kvl.dk/~abraham/custom/
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args) 
      (` (defvar (, var) (, value) (, doc))))))

(defgroup CUA-mode nil
  "Emulate CUA key bindings including C-x and C-c."
  :prefix "CUA-mode"
  :group 'editing-basics
  :link '(emacs-commentary-link :tag "Commentary" "CUA-mode.el")
  :link '(emacs-library-link :tag "Lisp File" "CUA-mode.el"))

;;;###autoload
(defcustom CUA-mode nil
  "Non-nil means that CUA emulation mode is enabled.
In CUA mode, shifted movement keys highlight and extend the region.
When a region is highlighted, the binding of the C-x and C-c keys are
temporarily changed to work as Motif, MAC or MS-Windows cut and paste.
Also, insertion commands first delete the region and then insert.
This mode enables Transient Mark mode and it provides a superset of the
PC Selection Mode and Delete Selection Modes.

Setting this variable directly does not take effect;
use either \\[customize] or the function `CUA-mode'."
  :set (lambda (symbol value) (CUA-mode (or value 0)))
  :require 'CUA-mode
  :link '(emacs-commentary-link "CUA-mode.el")
  :version "20.5"
  :type 'boolean
  :group 'CUA-mode)

(defcustom CUA-mode-disable-CUA-command 'set-mark-command
  "Prefix command used for disabling the following CUA-key."
  :set (lambda (symbol value)
	 (if (and value (commandp value))
	     (put value 'CUA2 'inhibit)))
  :type 'symbol
  :group 'CUA-mode)

(defcustom CUA-mode-register-commands-prefix nil
  "Remap register commands [C-x r ...] onto this prefix.
E.g. to use M-r as register command prefix, use the value [?\\M-r].
If set to nil, register commands are not remapped.
Other C-x ? commands can be remapped using CUA-remap-ctl-x-commands"
  :set (lambda (symbol value)
	 (if value
	     (CUA-remap-ctl-x-commands "r" value)))
  :type 'sexp
  :group 'CUA-mode)

(defcustom CUA-mode-keep-region-after-copy nil
  "If non-nil, don't deselect the region after copying."
  :type 'boolean
  :group 'CUA-mode)

(defcustom CUA-mode-enable-normal-region t
  "If non-nil, the region can be activated using set-mark-command.
This means that the region can be activated using [\\[set-mark-command]] and
the region extended using the normal (unshifted) movement keys, but still
use C-x and C-c for cut and copy when the region is active.
If the region is already active, using [\\[set-mark-command]] will cancel the region.

If the set-mark-command is also used for inhibiting the next CUA command, 
then the first [\\[set-mark-command]] will start the region, then next [\\[set-mark-command]] will
inhibit the next CUA command, but if this next CUA command is also [\\[set-mark-command]],
it will cancel the region."
  :type 'boolean
  :group 'CUA-mode)


(provide 'CUA-mode)

;;; cua-mode.el ends here

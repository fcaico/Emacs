;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;
;;;; module:  generate.el
;;;; version:  2.0
;;;; author:  Ciaran A Byrne ciaran@gec-rl-hrc.co.uk
;;;; date: 2:Sept:87
 
;;;  02.05.2000: added a patch in command-loop-3 (christoph.conrad@gmx.de)
 
;;;;;;;;;;;;;;;;;;;; macro lisp expansion ;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;
;;;; user commands:
;;;;  start-generating - replaces start-kbd-macro ^X(
;;;;  stop-generating  -    "    end-kbd-macro  ^X)
;;;;  expand-macro  - produces REAL emacs lisp code
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
 
;; (global-set-key "\^X(" 'start-generating)
;; (global-set-key "\^X)" 'stop-generating)
 
(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))
 
(defun caadr (x) (car (cadr x)))
(defun caddr (x) (car (cddr x)))
(defun cadar (x) (car (cdar x)))
(defun cdadr (x) (cdr (cadr x)))
 
 
(defvar gen-history '(first . last) "command-history subsection pair") 
(defvar generate-on nil "true if recording commands")
 
 
(defun start-generating ()
  "Records commands issued until the command stop-generating is invoked. 
The recorded commands can be turned into emacs lisp using
the command expand-macro.
 
Keystrokes are echoed in the minibuffer to remind you that
you are doing something weird"
  (interactive)
  (if generate-on (message "Already generating !")
    (progn
      (setq generate-on t)
      (message "Started generating")
      (setcar gen-history command-history) ; note beginning of macro       
	  (unwind-protect
		  (command-loop-3)  ; run soft command loop
		(stop-generating))
      (run-hooks 'generate-hook))))
 
(defun stop-generating ()
  "Ends command recording. See also: start-generating and expand-macro" 
 
  (interactive)
  (setcdr gen-history  command-history) ; note end of macro
  (message "Stopped generating")
  (setq generate-on nil))
 
 
(defun expand-macro (buffer fname doc)
  "Expands the most recently recorded command sequence into emacs lisp. 
Outputs into BUFFER and calls the function NAME with DOC string. 
 
See also: start-generating, stop-generating"
 
  (interactive
  "BBuffer for expansion : \nSNew function name : \nsDoc string : ")   
  (if generate-on (stop-generating))
  (let ((macro (rev-sub-list gen-history))) ; chop macro out
    (get-buffer-create buffer)
    (set-buffer buffer)
    (goto-char (point-max))
    (set-mark (point))
    (insert "\n(defun " (symbol-name fname) " () " ) ; function header     
	(insert "\"" doc)
    (insert "\nmacroised by " (user-full-name))
    (insert " @ " (current-time-string)  "\"\n")     
	(insert "\n(interactive)\n")
    (setq standard-output (get-buffer buffer))
    (mapcar 'print macro)
    (exchange-point-and-mark)
    ;; -cc- should always go back to buffer start
    (mapcar 'delete-matching-lines ; zap useless stuff
			'(  "^$"
				"start-generating"
				"stop-generating"
				"expand-macro"
				"execute-extended-command nil"
										; etc ?
				))
    (narrow-to-region (point) (point-max))
    (emacs-lisp-mode)
    (indent-region (point) (point-max) nil) ; neaten it all up
    ;; merge-multiple-numeric-args only can sum up functions with     
	;; interactive "p" (Prefix arg converted to NUMBER)     
	(mapcar 'merge-multiple-numeric-args
			'(
			  previous-line
			  next-line
			  delete-backward-char
			  backward-delete-char-untabify
			  backward-kill-word
			  kill-word
			  forward-char
			  backward-char
 
			  ;; -cc- added
			  my-scroll-up-in-place
			  my-scroll-down-in-place
			  next-line-mark
			  previous-line-mark
			  ;;
 
			  forward-char-nomark
			  backward-char-nomark
			  ))
    (goto-char (point-max))
    (insert "\n)\n")
    (widen)
    (run-hooks 'expand-macro-hook)))


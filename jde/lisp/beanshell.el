;;; beanshell.el
;; $Revision: 1.27 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2001 Paul Kinnucan.

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


(defgroup bsh nil
  "Customizations for the Emacs inteface to Pat Neimeyer's Java
interpreter, the Beanshell."
  :group 'tools
  :prefix "bsh-")

(defcustom bsh-startup-timeout 10
  "*Length of time Emacs waits for the Beanshell to startup.
Increase the value of this variable if you get Lisp errors
on BeanShell startup on Unix."
  :group 'bsh
  :type 'integer)

(defcustom bsh-eval-timeout 30
  "*Length of time in seconds Emacs waits for the Beanshell to evaluate
a Java expression before giving up and signaling an error."
  :group 'bsh
  :type 'integer)

(defcustom bsh-vm-args nil
  "*Specify arguments to be passed to the Java vm.
This option allows you to specify one or more arguments to be passed
to the Java vm that runs the BeanShell. Note that the value of this
variable should be a list of strings, each of which represents an
argument. When customizing this variable, use a separate text field
for each argument."
  :group 'bsh
  :type '(repeat (string :tag "Argument")))

(defcustom bsh-startup-directory ""
  "Path of directory in which to start the beanshell.
The path may start with a tilde (~) indication your
home directory and may include environment variables.
If this variable is the null string (the default), 
the beanshell starts in the directory of the current
buffer."
  :group 'bsh
  :type 'directory)


(defun bsh()
  "*Starts BeanShell, a Java interpreter developed by Pat Niemeyer."
  (interactive)
  (bsh-internal t))


(defun bsh-running-p ()
  "Returns t if the beanshell is running."
  (if (comint-check-proc "*bsh*") t))

(defun bsh-internal (&optional display-buffer) 
  (let ((bsh-buffer-name "*bsh*"))
    (if (not (comint-check-proc bsh-buffer-name))
	(let* ((bsh-buffer (get-buffer-create bsh-buffer-name))
	       (jde-java-directory
		(concat
		 (jde-find-jde-data-directory)
		 "java/"))
	       (vm (if (eq system-type 'windows-nt)
		       jde-run-java-vm-w
		     jde-run-java-vm))
	       (vm-args
		(list
		 "-classpath"
		 (jde-build-classpath
		  (append
		   (if jde-global-classpath 
		       jde-global-classpath
		     (let ((cp (getenv "CLASSPATH")))
		       (if (stringp cp)
			   (split-string cp jde-classpath-separator))))
		   (list
		    (expand-file-name "bsh-commands" jde-java-directory)
		    (expand-file-name "lib/jde.jar" jde-java-directory)
		    (expand-file-name "lib/bsh.jar" jde-java-directory)))
		  'jde-global-classpath)))
	       (dir (cond
		     ((not (string= bsh-startup-directory ""))
		      (jde-normalize-path 'bsh-startup-directory))
		     ((buffer-file-name)
		      (file-name-directory (buffer-file-name)))
		     (t
		      default-directory))))

	  (setq vm-args (append vm-args bsh-vm-args))
	  (setq vm-args (append vm-args (list "bsh.Interpreter")))

	  (save-excursion
	    (set-buffer bsh-buffer)
	    (erase-buffer)

	    (cd dir)
	    (insert (concat "cd " dir "\n"))
	    (insert 
	     (concat vm " "
		     (mapconcat (lambda (x) x) vm-args " ") "\n\n"))

	    (comint-mode)
	    (setq comint-prompt-regexp "bsh % "))

	  (message "%s" "Starting the BeanShell. Please wait...")

	  (let ((win32-start-process-show-window t)
		(w32-start-process-show-window t)
		(windowed-process-io t))		 
	    (comint-exec bsh-buffer "bsh" vm nil vm-args))

	  (let ((bsh-process (get-buffer-process bsh-buffer)))
	    (process-kill-without-query bsh-process))

	  (if display-buffer
	      (pop-to-buffer bsh-buffer-name)))
      (when display-buffer
	(message "The Java interpreter is already running.")
	(pop-to-buffer bsh-buffer-name)))))


(setq bsh-tq-reply nil)

(defun bsh-eval-filter (process result)
  (let ((end-of-result (string-match ".*bsh % " result)))
    ;; Check for case
    ;;   %bsh\n...eval output...%bsh\n
    ;; This can happen because the beanshell outputs two or more
    ;; prompts after evaluating some expressions.
    ;; Thanks to Stephane Nicolas.
    ;; (if (eq end-of-result 0)
    ;; (accept-process-output process 0 5))
    (if end-of-result
	(setq bsh-tq-reply (concat bsh-tq-reply (substring result 0 end-of-result)))
      (setq bsh-tq-reply (concat bsh-tq-reply result))
      (accept-process-output process bsh-eval-timeout 5))))

(defun bsh-eval (expr &optional eval-return)
  "Uses the BeanShell Java interpreter to evaluate a Java statement.
If the interpreter is not already running, this function starts
the interpreter. This function returns any text output by the
Java interpreter's standard out or standard error pipes.
If the optional argument eval-return is non-nil, this function
returns the result of evaluating the Java output as a Lisp
expression."
  (let* ((bsh-process
	  (if (get-process "bsh")
	      (get-process "bsh")
	    (let (proc)
	      (bsh-internal)
	      (setq proc (get-process "bsh"))
	      (if (eq system-type 'windows-nt)
		  (accept-process-output proc)
		(while (accept-process-output proc bsh-startup-timeout 0)))
	      proc)))
	 (comint-filter (if bsh-process (process-filter bsh-process))))
    (when bsh-process
      (setq bsh-tq-reply nil)
      (set-process-filter bsh-process 'bsh-eval-filter)
      ;; (message "Evaluating: %s" expr)
      (process-send-string bsh-process (concat expr "\n"))
      (if (not (accept-process-output bsh-process bsh-eval-timeout 100))
	  (error "No reply from BeanShell"))
      (set-process-filter bsh-process comint-filter)
      (if (string-match "// Error:" bsh-tq-reply)
	  (progn
	    (message 
	     "Beanshell expression evaluation error.\n  Expression: %s\n  Error: %s"
	     expr bsh-tq-reply)
	    (error "Beanshell eval error. See messages buffer for details.")))
      ;; (if eval-return (message "Evaluating reply: %s" bsh-tq-reply))
      (if eval-return
	  (if bsh-tq-reply
	      (condition-case eval-error
		  (eval (read bsh-tq-reply))
		(error
		 (message "Error evaluating Lisp result of Java expression evaluation.")
		 (message "  Java expression: %s." expr)
		 (message "  Java evaluation result: %s." bsh-tq-reply)
		 (error "Error evaluating Java expresson. See *Messages* buffer.")))
	    (progn
	      (message "bsh-eval-r error: Beanshell result is null. Cannot evaluate.")
	      (message "  Expression: %s" expr)))
	bsh-tq-reply))))

(defun bsh-eval-r(java-statement) 
  "Convenience function for evaluating Java statements
that return Lisp expressions as output. This function 
invokes bsh-eval with the evaluate-return option set to
t."
  (bsh-eval java-statement t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beanshell commands    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun bsh-exit ()
  "Closes the existing beanshell process"
  (interactive)
  (if (get-process "bsh")
      (process-send-string (get-process "bsh") "exit();\n")
    (message "The beanshell is not running")))

(defun bsh-open-class-browser ()
  "Opens the beanshell class browser"
  (interactive)
  (bsh-eval "browseClass(\"\");"))

(defun bsh-open-desktop ()
  "Opens the beanshell desktop"
  (interactive)
  (bsh-eval "desktop();"))

(provide 'beanshell)			;

;; $Log: beanshell.el,v $
;; Revision 1.27  2001/08/14 06:11:35  paulk
;; Add bsh-exit, bsh-open-class-browser, and bsh-open-desktop. Thanks to Javier Lopez.
;;
;; Revision 1.26  2001/07/31 05:11:48  paulk
;; ReleaseNotes.txt
;;
;; Revision 1.25  2001/06/13 03:51:44  paulk
;; Now defines bsh customization group.
;;
;; Revision 1.24  2001/05/31 05:14:38  paulk
;; Provide support for per-project caching of class data in the Beanshell. Thanks to Matt Conway.
;;
;; Revision 1.23  2001/05/19 02:39:18  paulk
;; Put jde-global-classpath first on the classpath to facilitate debugging of Java code run in hthe Beanshell.
;;
;; Revision 1.22  2001/04/16 05:33:20  paulk
;; Normalized paths. Thanks to Nick Sieger.
;;
;; Revision 1.21  2001/03/21 20:46:34  paulk
;; Updated bsh-internal to handle case where both jde-global-classpath and CLASSPATH environment variable are nil. Thanks to Toru TAKAHASHI <torutk@alles.or.jp> for reporting this bug and supply an initial version of a fix.
;;
;; Revision 1.20  2001/03/01 05:01:28  paulk
;; Adds the customization variable bsh-startup-directory.
;;
;; Revision 1.19  2001/02/25 04:23:12  paulk
;; Fixed bug in processing CLASSPATH environment variable.
;;
;; Revision 1.18  2001/02/03 07:44:26  paulk
;; Now uses jde-build-classpath to build BeanShell classpath. This allows enviromnent variables in the classpath.
;;
;; Revision 1.17  2000/10/08 12:55:38  paulk
;; *** empty log message ***
;;
;; Revision 1.16  2000/08/10 09:09:47  paulk
;; Now handles Lisp eval errors gracefully.
;;
;; Revision 1.15  2000/08/07 05:11:38  paulk
;; Adds bsh-vm-args variable.
;;
;; Revision 1.14  2000/08/04 02:51:19  paulk
;; Added bsh-eval-timeout variable.
;;
;; Revision 1.13  2000/02/16 04:39:28  paulk
;; Implemented Cygwin/XEmacs compatiblity fix provided by Fred Hart
;; <cfhart@Z-TEL.com> in bsh-internal.
;;
;; Revision 1.12  2000/02/02 05:51:00  paulk
;; Expanded doc string.
;;
;; Revision 1.11  2000/01/28 04:28:00  paulk
;; Fixed startup timing bug that cause commands that use the beanshell to
;; failt the first time on Unix systems.
;;
;; Revision 1.10  2000/01/15 08:00:03  paulk
;; Corrected typo.
;;
;; Revision 1.9  1999/11/01 03:13:07  paulk
;; No change.
;;
;; Revision 1.8  1999/09/17 06:55:26  paulk
;; Set comint-prompt-regexp to the beanshell prompt.
;; Fixed bug where Emacs was querying user whether to kill the beanshell
;; buffer on exit from Emacs.
;;
;; Revision 1.7  1999/01/15 22:18:41  paulk
;; Added Andy Piper's NT/XEmacs compatibility changes.
;;
;; Revision 1.6  1998/12/13 22:10:04  paulk
;; Add check for chunked traffic between Emacs and the BeanShell.
;;
;; Revision 1.5  1998/12/09 00:59:43  paulk
;; Added a startup message for beanshell.
;;
;; Revision 1.4  1998/11/27 10:07:57  paulk
;; Use CLASSPATH environment variable if jde-global-classpath is nil.
;;
;; Revision 1.3  1998/11/22 23:14:28  paulk
;; Fixed path separator bug.
;;
;; Revision 1.2  1998/11/22 18:11:56  paulk
;; Changed path to use jde.jar.
;;
;; Revision 1.1  1998/10/22 00:07:56  paulk
;; Initial revision
;;


;; End of beanshell.el
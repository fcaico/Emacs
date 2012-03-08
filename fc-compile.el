;;======================================================================
;; fc-compile.el
;; This file is the emacs equivalent of vcvars32.bat it defines the 
;; environment variables needed to do compiles with visual studio.
;; send mail to frank.caico@east.aht.com for more help or support.
;;======================================================================

(defvar fc-current-project)
;; The project which is currently being worked on. 
;; This is used to specify the make file to compile with.

(defvar fc-current-config)
;; The Current target to compile.  If none is specified, then ALL will be used.

;;======================================================================
;; environment variables needed for compilation to work properly
;;======================================================================

(setenv "VSCommonDir" "C:\\PROGRA~1\\MICROS~3\\Common")
(setenv "MSDevDir" "C:\\PROGRA~1\\MICROS~3\\Common\\msdev98")
(setenv "MSVCDir" "C:\\PROGRA~1\\MICROS~3\\VC98")
(setenv "VcOsDir" "WINNT")

(setenv "PATH" 
		(format "%s\\BIN;%s\\BIN;%s\\TOOLS\\%s;%s\\TOOLS;%s"
				(getenv "MSDevDir")
				(getenv "MSVCDir")
				(getenv "VSCommonDir")
				(getenv "VcOsDir")
				(getenv "VSCommonDir")
				(getenv "PATH")))

(setenv "INCLUDE"
		(format "%s\\ATL\\INCLUDE;%s\\INCLUDE;%s\\MFC\\INCLUDE;%s"
				(getenv "MSVCDir")
				(getenv "MSVCDir")
				(getenv "MSVCDir")
				(getenv "INCLUDE")))

(setenv "LIB"
		(format "%s\\LIB;%s\\MFC\\LIB;%s"
					 (getenv "MSVCDir")
					 (getenv "MSVCDir")
					 (getenv "LIB")))


;;======================================================================
;; Functions to do the work
;;======================================================================

(defun reset-compile-settings ()
  (interactive)
  (makunbound 'fc-current-project)
  (makunbound 'fc-current-config)
  (makunbound 'compile-command))

(defun fc-compile (project config &optional target call-interactive)
  "Workhorse for compile functions in fc-compile."
  (setq fc-current-project project)
  (setq fc-current-config config)
  (setq compile-command
		(format 
		 "NMAKE /f \"%s.mak\"%s %s"
		 project
		 (if (not (string= config "")) 
			 (format " CFG=\"%s\"" config) "")
		 (if (not target)
			 "ALL"
		   target)))
  (if call-interactive
	  (call-interactively 'compile)
	(compile compile-command)))
  

(defun fc-compile-prompt-only-if-needed (project config)
  "Frank's compile function which prompts for input only when needed."
  (interactive 
   (list 
	(if (not (boundp 'fc-current-project))
		(read-from-minibuffer 
		 "Enter Project Name: ")
	  fc-current-project)
	(if (not (boundp 'fc-current-config))
		(read-from-minibuffer 
		 "Enter Desired Configuration: ")
	  fc-current-config)))
  (fc-compile project config nil nil))

(defun fc-compile-prompt-always (project config target)
  "Frank's compile function which prompts for input allways."
  (interactive
   (list 
	(read-from-minibuffer
	 "Enter Project Name: "
	 (if (boundp 'fc-current-project)
	   fc-current-project))
	(read-from-minibuffer
	 "Enter Desired Configuration: "
	 (if (boundp 'fc-current-config)
		 fc-current-config))
	(read-from-minibuffer
	 "Enter Desired Target: ")))
  (fc-compile project config target t))

(provide 'fc-compile)

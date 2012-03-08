;;; jde.el -- Integrated Development Environment for Java.
;; $Revision: 1.187 $ $Date: 2001/08/24 06:01:04 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 1999, 2000, 2001 Paul Kinnucan.

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

;; This is one of a set of packages that make up the 
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; The latest version of the JDE is available at
;; <URL:http://jde.sunsite.dk>.
;; <URL:http://www.geocities.com/SiliconValley/Lakes/1506/>

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

;;; Code:


;;;###autoload
(defconst jde-version "2.2.8"
  "JDE version number.")

(defconst jde-xemacsp (string-match "XEmacs" (emacs-version))
  "Non-nil if we are running in the XEmacs environment.")

(defconst jde-xemacs20p (and jde-xemacsp (>= emacs-major-version 20)))

;; (eval-when (eval load compile)		;-- sgr 5-Sept-2000
(eval-when-compile
  (defconst jde-xemacsp (string-match "XEmacs" (emacs-version))
    "Non-nil if we are running in the XEmacs environment.")

  (defconst jde-xemacs20p (and jde-xemacsp (>= emacs-major-version 20))))


(require 'semantic-load)
(require 'easymenu)
(require 'cl)
(require 'font-lock)
(require 'cc-mode)
(require 'cus-edit)
(require 'jde-compile)
(require 'jde-db)
(require 'jde-run)
(require 'jde-make)
(require 'jde-gen)
(require 'compile)
(require 'imenu)
(require 'speedbar)
(require 'browse-url)
(require 'beanshell)
(require 'jde-wiz)
(require 'jde-parse)
(require 'jde-help)
(require 'jde-bug)
(require 'jde-complete)
(require 'jde-javadoc)
(require 'jde-javadoc-gen)
(require 'jde-stat)
(require 'jde-which-method)
(require 'jde-java-font-lock)
(require 'jde-import)
(require 'jde-package)
(require 'executable)  ;; in XEmacs' sh-script package

;; (require 'jde-project)

;; From custom web page for compatibility between versions of custom:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defface (var values doc &rest args)
      (` (progn
	   (defvar (, var) (quote (, var)))
	   ;; To make colors for your faces you need to set your .Xdefaults
	   ;; or set them up ahead of time in your .emacs file.
	   (make-face (, var))
	   )))
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

(if (not (fboundp 'custom-set-default))
    (defalias 'custom-set-default 'set-default))

(defgroup jde nil
  "Java Development Environment"
  :group 'tools
  :prefix "jde-")

(defgroup jde-project nil
  "JDE Project Options"
  :group 'jde
  :prefix "jde-")


;; (makunbound 'jde-key-bindings)
(defcustom jde-key-bindings
  (list 
   (cons "[?\C-c ?\C-v ?\C-a]" 'jde-run-menu-run-applet)
   (cons "[?\C-c ?\C-v ?\C-b]" 'jde-build)
   (cons "[?\C-c ?\C-v ?\C-c]" 'jde-compile)
   (cons "[?\C-c ?\C-v ?\C-d]" 'jde-debug)
   (cons "[?\C-c ?\C-v ?\C-f]" 'jde-find)
   (cons "[?\C-c ?\C-v ?i]"    'jde-wiz-implement-interface)
   (cons "[?\C-c ?\C-v ?j]"     'jde-javadoc-generate-javadoc-template)
   (cons "[?\C-c ?\C-v ?\C-k]" 'bsh)
   (cons "[?\C-c ?\C-v ?\C-l]" 'jde-gen-println)
   (cons "[?\C-c ?\C-v ?\C-n]" 'jde-browse-jdk-doc)
   (cons "[?\C-c ?\C-v ?\C-p]" 'jde-save-project)
   (cons "[?\C-c ?\C-v ?\C-q]" 'jde-wiz-update-class-list)
   (cons "[?\C-c ?\C-v ?\C-r]" 'jde-run)
   (cons "[?\C-c ?\C-v ?\C-s]" 'speedbar-frame-mode)
   (cons "[?\C-c ?\C-v ?\C-t]" 'jde-db-menu-debug-applet)
   (cons "[?\C-c ?\C-v ?\C-w]" 'jde-help-symbol)
   (cons "[?\C-c ?\C-v ?\C-y]" 'jde-open-class-source)
   (cons "[?\C-c ?\C-v ?\C-z]" 'jde-import-find-and-import)
   (cons "[?\C-c ?\C-v ?\C-[]" 'jde-run-etrace-prev)
   (cons "[?\C-c ?\C-v ?\C-]]" 'jde-run-etrace-next)
   (cons "[(control c) (control v) (control ?.)]" 'jde-complete-at-point-menu)
   (cons "[(control c) (control v) ?.]" 'jde-complete-at-point)
   )
  "*Specifies key bindings for the JDE.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies 
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer.
You can use the notation [f1], [f2], etc., to specify function keys."
  :group 'jde-project
  :type '(repeat
	  (cons :tag "Key binding"
		(string :tag "Key")
		(function :tag "Command")))
  :set '(lambda (sym val)
	  (mapc
	   (lambda (buf)
	     (save-excursion
	       (set-buffer buf)
	       (when (boundp 'jde-mode-map)
		 ;; Unmap existing key bindings
		 (if (and (boundp 'jde-key-bindings)
			  jde-key-bindings)
		     (mapc 
		      (lambda (binding)
			(let ((key (car binding)))
			  (if (string-match "\\[.+]"key)
			      (setq key (car (read-from-string key))))
			  (local-unset-key key)))
		      jde-key-bindings))
		 ;; Map new key bindings.
		 (mapc 
		  (lambda (binding)
		    (let ((key (car binding))
			  (fcn (cdr binding)))
		      (if (string-match "\\[.+]" key)
			  (setq key (car (read-from-string key))))
		      (define-key (current-local-map) key fcn)))
		  val))))
	   (jde-get-java-source-buffers))
	  (set-default sym val)))

(defcustom jde-launch-beanshell-on-demand-p t
  "If non-nil, the JDE launches the Beanshell the first time it is needed.
Otherwise, the JDE launches the Beanshell, if it is not already running,
whenever you open a Java source file."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-project-context-switching-enabled-p t
  "*Enable project context switching.
If non-nil, the JDE reloads a buffer's project file when you switch to the buffer from
another buffer belonging to another project. You can disable this feature if you prefer
to load project files manually. The debugger uses this variable to disable context-switching
temporarily when stepping through code."
  :group 'jde-project
  :type 'boolean)

(defun jde-toggle-project-switching ()
  "Toggles project switching on or off."
  (interactive)
  (setq jde-project-context-switching-enabled-p 
	(not jde-project-context-switching-enabled-p)))

;;(makunbound 'jde-jdk-doc-url)
(defcustom jde-jdk-doc-url "http://www.javasoft.com/j2se/1.3/docs/index.html"
  "*URL of JDK documentation. 
This can point to a remote or local copy of the documentation. By
default, this variable points to the copy stored at JavaSoft's
website."
  :group 'jde-project
  :type 'file)

;;(makunbound 'jde-global-classpath)
(defcustom jde-global-classpath nil
  "Specify a common classpath for compile, run, and debug commands.
Use this variable if you want to the JDE to use the same classpath for
compiling, running,and debugging an application. Note that the value
of this variable is a list of strings, each of which specifies a
path. The JDE converts this list to a colon- or semicolon-separated
list before inserting in the compiler or vm command line. The paths
may start with a tilde (~) and may include environment variables. The
JDE replaces the ~ with your home directory and replaces each instance
of an environment variable with its value before inserting it into
a command line.

You can specify different classpaths for compiling, running and
debugging applicaitons. Use `jde-compile-option-classpath' to specify
the compilation classpath, `jde-run-option-classpath' to specify the
run classpath, and/or `jde-db-option-classpath' to specify the debug
classpath. You can use these variables together. For example, suppose
that you need to use one classpath for compilation and other for
running and debugging. You could do this by setting
`jde-compile-option-classpath' to the compile classpath and
`jde-global-classpath' to the run and debug classpath. If you set
`jde-global-classpath', the JDE uses it to construct the classpath for
any operation for which you do not set the operation-specific
classpath variable (e.g., `jde-compile-option-classpath'). 

If you do not set `jde-global-classpath', the JDE uses the operation-specific
classpath if it is set. If neither the global nor the
operation-specific classpath is set, the JDE does not generate a
-classpath argument for the operation, e.g., compile or run a Java
class. In this case, the operation uses the value of the CLASSPATH variable
if specified.

Note: do not use environment variables in the paths that you set via
jde-global-classpath."
  :group 'jde-project
  :type '(repeat (file :tag "Path")))

(defcustom jde-quote-classpath t
  "*Quote the classpath argument.
Set this option on when using the bash shell with Windows 95 or NT.
The semicolons in the classpath confuse the shell."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-expand-classpath-p t
  "Replace each occurence of a directory named `jde-lib-directory-name'
 in the classpath with paths to the jar and zip files in that directory."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-lib-directory-name "lib" 
  "Regular expression that matches name of lib directories for 
the current project. See `jde-expand-classpath-p' and
`jde-expand-classpath' for more information"
  :group 'jde-project
  :type 'string)

(defcustom jde-project-name "default"
"Specifies name of project to which the current buffer belongs."
  :group 'jde-project
  :type 'string)

(defcustom jde-project-file-name "prj.el"
  "*Specify name of JDE project file.
When it loads a Java source file, the JDE looks for a lisp file of
this name (the default is prj.el in the source file hierarchy. If it
finds such a file, it loads the file. You can use this file to set the
classpath, compile options, and other JDE options on a
project-by-project basis."
  :group 'jde-project
  :type 'string)

(defcustom jde-compiler "javac"
  "*Java compiler.
Specifies the path to the compiler to be used to compile the source
in the current buffer. The default is the JDK compiler (javac)."
  :group 'jde-project
  :type 'file)

(defcustom jde-read-compile-args nil
"*Specify whether to prompt for additional compiler arguments.
If this variable is non-nil, the jde-compile command prompts
you to enter additional compiler arguments in the minibuffer.
These arguments are appended to those specified by customization
variables. The JDE maintains a history list of arguments 
entered in the minibuffer."
  :group 'jde-project
  :type 'boolean
)

(defvar jde-interactive-compile-args ""
"String of compiler arguments entered in the minibuffer.")

(defvar jde-interactive-compile-arg-history nil
"History of compiler arguments entered in the minibuffer.")

(defcustom jde-compile-finish-hook 
  '(jde-compile-finish-refresh-speedbar jde-compile-finish-flush-completion-cache)
  "List of functions to be invoked when compilation of a 
Java source file terminates. Each function should accept
two arguments: the compilation buffer and a string 
describing how the compilation finished."
  :group 'jde
  :type 'hook)


(defun jde-compile-finish-flush-completion-cache (buf msg) 
  "Flush the classinfo cache at the end of compilation.
Flush the entire cache as we don't know which classes
were recompiled."
  ;;Setting the last java buffer as the current buffer
  (condition-case nil
      (progn
	(if jde-xemacsp
	    (set-buffer (cadr (buffer-list)))
	  (set-buffer (car (buffer-list))))
	(if (eq major-mode 'jde-mode)
	    (progn
	      (setq jde-complete-last-compiled-class (jde-complete-get-name-of-this-class))
	      (jde-complete-flush-classes-in-cache (list jde-complete-last-compiled-class))
	      (setq jde-complete-last-compiled-class nil)
	      (message "Flushed completion cache."))))
    (error nil)))

(defun jde-compile-finish-refresh-speedbar (buf msg) 
  "Refresh speedbar at the end of a compilation."
  (if (and (frame-live-p speedbar-frame)
	    (frame-visible-p speedbar-frame))
       (speedbar-refresh)))

(defcustom jde-build-function '(jde-java-build)
  "*Function that will be invoked by the jde-build command.
The `jde-java-build' function uses javac's built-in make 
facility to rebuild a project. This function invokes javac 
on the source file specified by `jde-run-app-class', with 
the -depend option. This causes javac to recompile all 
missing or out-of-date files required to run the application's 
main class. The `java-make' function uses a user-specified make 
program to rebuild the project. JDE configuration variables 
control which mode is used. This command invokes the make program 
specified by the variable `jde-make-program'. If the variable 
`jde-make-args' is a non-empty string, this function uses its 
contents to invoke make; otherwise, it prompts you to enter 
command-line arguments for make. The `jde-ant-build' function
uses the Apache Ant program to build the project. You may also
specify a custom function to use."
  :group 'jde-project
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Function: "
	   :entry-format " %b %v"
	   (const jde-java-build)
	   (const jde-ant-build)
	   (const jde-make)
	   (function my-custom-build-function)))
  :set '(lambda (sym val)
	  (if (eq (car val) 'jde-ant-build)
	      (require 'jde-ant))
	  (set-default sym val)))

(defcustom jde-enable-senator t
  "Enable senator mode. This mode provides Java-aware buffer navigation
and searching commands."
  :group 'jde-project
  :type 'boolean
  :set '(lambda (sym val)
	  (mapc 
	   (lambda (buff)
	     (save-excursion	       
	       (set-buffer buff)
	       (senator-minor-mode (if val 1 -1))))
	     (jde-get-java-source-buffers))
	  (set-default sym val)))

(defcustom jde-enable-abbrev-mode nil
"*Enable expansion of abbreviations in jde-mode.
See `jde-mode-abbreviations' for more information."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-mode-abbreviations
  (list 
   (cons "ab" "abstract")
   (cons "bo" "boolean")
   (cons "br" "break")
   (cons "by" "byte")
   (cons "byv" "byvalue")
   (cons "cas" "cast")
   (cons "ca" "catch")
   (cons "ch" "char")
   (cons "cl" "class")
   (cons "co" "const")
   (cons "con" "continue")
   (cons "de" "default")
   (cons "dou" "double")
   (cons "el" "else")
   (cons "ex" "extends")
   (cons "fa" "false")
   (cons "fi" "final")
   (cons "fin" "finally")
   (cons "fl" "float")
   (cons "fo" "for")
   (cons "fu" "future")
   (cons "ge" "generic")
   (cons "go" "goto")
   (cons "impl" "implements")
   (cons "impo" "import")
   (cons "ins" "instanceof")
   (cons "in" "int")
   (cons "inte" "interface")
   (cons "lo" "long")
   (cons "na" "native")
   (cons "ne" "new")
   (cons "nu" "null")
   (cons "pa" "package")
   (cons "pri" "private")
   (cons "pro" "protected")
   (cons "pu" "public")
   (cons "re" "return")
   (cons "sh" "short")
   (cons "st" "static")
   (cons "su" "super")
   (cons "sw" "switch")
   (cons "sy" "synchronized")
   (cons "th" "this")
   (cons "thr" "throw")
   (cons "throw" "throws")
   (cons "tra" "transient")
   (cons "tr" "true")
   (cons "vo" "void")
   (cons "vol" "volatile")
   (cons "wh" "while")
   )
"*Abbreviations used for Java keywords.
To use these abbreviations, you must enable abbrev-mode (see
`jde-enable-abbrev-mode'). To use an abbreviation, enter the
abbreviation followed by a white-space character. To suppress
expansion, enter C-q white-space."
   :group 'jde-project
  :type '(repeat 
	  (cons :tag "jde-mode abbreviation"
		(string :tag "Abbreviation")
		(string :tag "Expansion"))))

(defvar jde-mode-abbrev-table (make-abbrev-table)
  "Abbrev table for use in JDE-mode buffers.")


(defun jde-init-abbrev-table ()
  "Load the abbrev table with a set of abbrevs that invoke an anonymous
function that  does the expansion only if point is not in a quoted string 
or a comment."

  ;; Note the use of lexical-let - must have the common lisp packages
  ;; around, since the anonymous function needs the closure provided by
  ;; lexical-let.
  (interactive)
  ;; (setq local-abbrev-table (make-abbrev-table))
  (mapc 
   (lambda (x)
     (lexical-let
	 ((abbrev (car x))		; this is the abbrev, lexically scoped
	  (expansion (cdr x)))		; this is the expansion
       (define-abbrev 
	 local-abbrev-table
	 abbrev
	 ""
	 (lambda ()
	   (if (jde-parse-comment-or-quoted-p)
	       (insert abbrev)		; insert the abbrev in quote/comment
	     (insert expansion)))       ; proceed with expansion elsewhere
	 0)))
   jde-mode-abbreviations)

  (if jde-gen-cflow-enable
      (jde-gen-load-abbrev-templates))

  (setq abbrevs-changed nil))

;; The next two functions contributed by s.nicolas@videotron.ca
(defun jde-abbrev-mode ()
"*Activates  or deactivates the abbreviation mode in JDE
without altering the project settings.
See `jde-mode-abbreviations' for more information."
 (interactive)
  (setq jde-enable-abbrev-mode (not jde-enable-abbrev-mode))
  (setq abbrev-mode jde-enable-abbrev-mode)
  (when jde-enable-abbrev-mode
     ;; Define abbreviations.a
    (jde-init-abbrev-table))
  (if jde-enable-abbrev-mode
    (message "abbreviation mode on")
    (message "abbreviation mode off")))

(defun jde-show-abbrevs ()
"*Shows a popup menu containing all available expansions.
See `jde-mode-abbreviations' for more information."
  (interactive)
   (let* ((expansions
          (mapcar
            (lambda(x) (cons (cdr x) (car x)))
              jde-mode-abbreviations))
         (expansion (car (imenu--mouse-menu expansions (if jde-xemacsp nil
t) "Abbreviations"))))
  (insert expansion)))


;;;###autoload
(defun jde-set-compiler (compiler)
  "Specify the pathname of the compiler to be used to compile the
current buffer. Default is javac."
  (interactive
   "sEnter compiler (javac): ")
   (if (string= compiler "")
       (setq jde-compiler "javac")
     (setq jde-compiler compiler)))

(defvar jde-classpath-separator (if (eq system-type 'cygwin32) 
				    ";" path-separator)
  "The separator to use in a classpath.
This is usually the same as `path-separator'")

;;;###autoload
(defun jde-set-global-classpath (classpath)
  "Specify the value of the -classpath argument for the Java compiler and
interpreter."
  (interactive 
   "sEnter classpath: ")
  (setq jde-global-classpath (split-string classpath jde-classpath-separator)))

;;;###autoload
(defun jde-browse-jdk-doc ()
  "Displays the JDK doc in a web browser. This function uses the URL
stored in the variable jde-jdk-doc-url to locate the JDK documentation."
  (interactive)
  (let ((expanded-url (jde-normalize-path 'jde-jdk-doc-url)))
    (if (or (string-match "http:" jde-jdk-doc-url)
	    (string-match "file:" jde-jdk-doc-url))
	(browse-url jde-jdk-doc-url browse-url-new-window-p)
      (if (file-exists-p expanded-url)
	  (browse-url expanded-url browse-url-new-window-p)
	(error "The JDK documentation file, %s, does not exist." jde-jdk-doc-url)))))

(defun jde-make-compile-command (more-args)
  "Constructs the java compile command as: jde-compiler + options + buffer file name."
  (concat jde-compiler " " 
	  (jde-get-compile-options) 
	  (if (not (string= more-args ""))
	      (concat " " more-args))
	  " "
	  (file-name-nondirectory buffer-file-name)))

(defun jde-show-compile-options ()
  "Show the JDE Compile Options panel."
  (interactive)
  (customize-apropos "jde-compile-options" 'groups))

(defun jde-show-run-options ()
  "Show the JDE Run Options panel."
  (interactive)
  (customize-apropos "jde-run-options" 'groups))

(defun jde-show-debug-options ()
  "Show the JDE Debug Options panel."
  (interactive)
  (customize-apropos "jde-db-options" 'groups))

(defun jde-show-project-options ()
  "Show the JDE Debug Options panel."
  (interactive)
  (customize-apropos "jde-project" 'groups))

(defun jde-show-autocode-options ()
  "Show the JDE Autocode panel."
  (interactive)
  (customize-apropos "jde-gen" 'groups))


;;;###autoload
(defun jde-java-build ()
  "Use javac -depend to build the application whose main class is
specified by `jde-run-application-class'."
 (interactive)
  (cond 
   ((string= jde-run-application-class "")
    (message "No application main class specified."))
   (t
    (string-match "\\(\\(\\w*\\.\\)*\\)\\(\\w*\\b\\)"
		jde-run-application-class)
    (let* ((b1 (match-beginning 1))
	   (e1 (match-end 1))
	   (b2 (match-beginning 3))
	   (e2 (match-end 3))
	   (file (concat
		  (substring jde-run-application-class b2 e2)
		  ".java"))
	   (package (if e1
			(substring jde-run-application-class b1 e1)))
	   (directory (jde-db-search-src-dirs file package)))
      (cond
       (directory
	(let ((file-path 
	       (concat directory 
		       file))
	      (save-depend jde-compile-option-depend))
	  (find-file file-path)
	  (setq jde-compile-option-depend t)
	  (jde-compile)
	  (setq jde-compile-option-depend save-depend)))
       (t
	(message (concat "Could not find source for "
			 jde-run-application-class))))))))
    
;;;###autoload
(defun jde-build ()
  "Rebuild the entire project.
This command invokes the function defined by `jde-build-function'."
  (interactive)
  (funcall (car jde-build-function)))
      

(defun jde-mode-internal () 
 ;; Define buffer-local variables.
  (make-local-variable 'jde-project-name)
  (make-local-variable 'jde-run-applet-document)

  (setq jde-current-project 
	(jde-find-project-file default-directory))

  ;; Load the project file for this buffer. The project file
  ;; defines JDE options for a project.
  (if (not (jde-debugger-running-p))
	   (jde-load-project-file))

  ;; Enable support for automatic project switching.
  ;; This feature loads the appropriate project settings whenever
  ;; a user switches from a Java buffer belonging to one project
  ;; to a buffer belonging to another.
  (let ((pch post-command-hook))
    (make-local-hook 'post-command-hook)
    (setq post-command-hook pch)
    (unless (find 'jde-detect-java-buffer-activation post-command-hook)
      (add-hook 'post-command-hook 'jde-detect-java-buffer-activation nil t)))

  (let ((acf after-change-functions))
    (make-local-hook 'after-change-functions)
    (setq after-change-functions acf)
    (add-hook 'after-change-functions
	    'jde-parse-buffer-changed-hook nil t))


  (if jde-xemacsp
      (jde-insert-menu-in-XEmacs-menubar))

  ;; Define underscore as a word constituent. This is needed
  ;; to support coding styles the begin fields with an underscore.
  (modify-syntax-entry ?_ "w")

  (when jde-enable-abbrev-mode
     ;; Define abbreviations.
    (jde-init-abbrev-table)
    (abbrev-mode 1))

  ;; Reset the key bindings in case jde-mode-keymap
  ;; was not bound at startup.
  (custom-initialize-reset 'jde-key-bindings nil)

  (if (and
       jde-setnu-mode-enable
       (< (point-max) jde-setnu-mode-threshold))
      (setnu-mode 1))

  (if (string= (car jde-db-debugger) "JDEbug")
      (jde-bug-install-jdebug-menu))

;;; david@dponce.com
;; (make-local-variable 'semantic-toplevel-bovine-table)
  (jde-parse-semantic-default-setup)
 
  (make-local-variable 'mode-line-format)
  (setq mode-line-format jde-mode-line-format)

  (if (and
       jde-which-method-mode
       (not jde-which-method-idle-timer))
    (setq jde-which-method-idle-timer
	  (run-with-idle-timer .25 t 'jde-which-method-update)))

  (make-local-hook 'semantic-after-toplevel-bovinate-hook)
  (add-hook 'semantic-after-toplevel-bovinate-hook 
	    'jde-parse-update-after-parse nil t)

  (if jde-enable-senator
      (progn
	(require 'senator)
	(senator-minor-mode 1)))

  ; When looking for a tag that has multiple matches
  ; in the TAGS file, prefer (find first) the
  ; occurrence in the _current_ buffer.
  ; Contributed by Charles Rich, Mitsubishi Electric Research Laboratories,
  ; Cambridge, MA>
  (make-local-variable 'tags-table-format-hooks)
  (setq tags-table-format-hooks '(jde-etags-recognize-tags-table
				  recognize-empty-tags-table))

  (if (and
       (not jde-launch-beanshell-on-demand-p)
       (not (bsh-running-p)))
      (bsh-internal))

  (jde-wiz-set-bsh-project))

;; This is actually a no-op to get jde auto-loaded.
;;;###autoload
(defun jde-mode ()
  "Major mode for developing Java applications and applets."
  nil)

(define-derived-mode 
  jde-mode java-mode "JDE"
  "Major mode for developing Java applications and applets.
  \\{jde-mode-map}"

  (jde-mode-internal)
)

(defcustom jde-log-max 500
  "*Maximum number of lines to keep in the JDE log buffer.  If nil,
disable logging.  If t, don't truncate the buffer."
  :group 'jde-project
  :type '(choice (integer :tag "Number of lines to keep")
		 (boolean :tag "Disable/Unlimited")))

(defun jde-log-msg (msg &rest args)
  "Log a message to the *jde-log* buffer.  Does nothing if
`jde-log-max' is nil."
  (if jde-log-max
      (save-match-data
	(save-excursion
	  (set-buffer (get-buffer-create "*jde-log*"))
	  (goto-char (point-max))
	  (insert (apply 'format msg args))
	  (insert "\n")
	  (if (integerp jde-log-max)
	      (let ((line-cnt 0))
		(while (search-backward "\n" nil t)
		  (setq line-cnt (1+ line-cnt)))
		(goto-char (point-min))
		(while (> line-cnt jde-log-max)
		  (delete-region (point) (search-forward "\n" nil t))
		  (setq line-cnt (1- line-cnt)))))))))

(defun jde-log-msg-t (msg &rest args)
  "Log a message to the *jde-log* buffer, and return t.  Does nothing
but return t if `jde-log-max' is nil."
  (jde-log-msg msg args)
  t)

(defun jde-log-msg-nil (msg &rest args)
  "Log a message to the *jde-log* buffer, and return nil.  Does nothing
but return nil if `jde-log-max' is nil."
  (jde-log-msg msg args)
  nil)

;; Make jde-mode the default mode for Java source code buffers.
;; Prepend the jde-mode entry so that it shadows the java-mode
;; entry already in the list.
;;;###autoload
(setq auto-mode-alist
  (append
   '(("\\.java\\'" . jde-mode))
	auto-mode-alist))

(defvar jde-menu-definition
  (list "JDE"
	["Compile"           jde-compile t]
	;; ["Run App"           jde-run (not (jde-run-application-running-p))]
	["Run App"           jde-run t]
	["Debug App"         jde-debug t]
	"-"
	;;["-"                 ignore nil]
	["Run Applet"        jde-run-menu-run-applet t]
	["Debug Applet"      jde-db-menu-debug-applet t]
	"-"  
	["Build"             jde-build t]
	(list "Find"
	      ["Expression..."    jde-find (and 
					 (executable-find
					  (if (eq system-type 'windows-nt) "find.exe" "find"))
					 (executable-find
					  (if (eq system-type 'windows-nt) "grep.exe" "grep")))]
	      ["Class Source..."  jde-open-class-source t]
	      )
	(list "Interpreter"
	      ["Start"         bsh t]
	      ["Exit"          bsh-exit t]
	 )
        (list "Documentation"
	      ["Add"             jde-javadoc-autodoc-at-line (jde-javadoc-enable-menu-p)]
	      ["Check This"      jde-javadoc-checkdoc-at-line (jde-javadoc-enable-menu-p)]
	      ["Check All"       jde-javadoc-checkdoc t]
	      ["Generate"        jde-javadoc-make t]
	)
        "-" 
	(list "Code Generation"
	      (list "Templates"
		    ["Get/Set Pair..."  jde-gen-get-set t]
		    ["Println..."       jde-gen-println t]
		    (list "Listener"
			  ["Action"          jde-gen-action-listener t]
			  ["Window"          jde-gen-window-listener t]
			  ["Mouse"           jde-gen-mouse-listener t]
			  )
		    ["Other..."        jde-gen-code t]
		    )
	      (list "Wizards"
		    ["Import class..."             jde-import-find-and-import t]
		    ["Override Method"             jde-wiz-override-method t]
		    ["Implement Interface..."      jde-wiz-implement-interface t]
		    ["Extend Abstract Class..."    jde-wiz-extend-abstract-class t]
		    ["Delegate Methods..."         jde-wiz-delegate t]
		    ["Generate Get/Set Methods"    jde-wiz-get-set-methods t]
		    "-"
		    ["Update Class List"   jde-wiz-update-class-list t]
		    )
	      )
	["Speedbar"          speedbar-frame-mode t]
	(list "Project"
	      (vector "Auto Switch" 
                      'jde-toggle-project-switching
                      (if jde-xemacsp :active :enable) t 
                      :style 'radio 
                      :selected 'jde-project-context-switching-enabled-p)
	      (list "Options"
		    ["General"         jde-show-project-options t]
		    ["Compile"         jde-show-compile-options t]
		    ["Run"             jde-show-run-options t]
		    ["Debug"           jde-show-debug-options t]
		    ["Autocode"        jde-show-autocode-options t]
		    ["Javadoc"         jde-javadoc-customize t]
		    )
	      (list "Project File"
		    ["Create New" jde-create-new-project t]
		    ["Save"     jde-save-project t]
		    ["Load"     jde-load-project-file t]
		    ["Load All" jde-load-all-project-files t]
		    )
	      )
	(list "Help"
	      ["JDE Users Guide"       jde-show-help t]
	      ["JDK"                   jde-browse-jdk-doc t]
	      "-"
	      ["Class..."              jde-help-class t]
	      ["Class Member..."       jde-help-class-member t]
	      ["Symbol at Point"       jde-help-symbol t]
	      "-"
	      ["Submit problem report" jde-submit-problem-report t]
	      "-"
	      (concat "JDE " jde-version)
	      )
	)
  "Menu for JDE.")

;; Define JDE menu for FSF Emacs.
(if (or (not jde-xemacsp) (featurep 'infodock))
    (easy-menu-define jde-menu 
		      jde-mode-map
		      "Menu for JDE."
		      jde-menu-definition))

(defun jde-insert-menu-in-XEmacs-menubar ()
  "Insert JDE menu in the XEmacs menu bar."
  (if (and 
       (not (featurep 'infodock))
       (not (memq 'infodock c-emacs-features))
       (boundp 'current-menubar)
       current-menubar)
      (if (fboundp 'add-submenu)
	  (add-submenu nil jde-menu-definition)
	(add-menu nil "JDE" (cdr jde-menu-definition)))))


(defvar jde-new-buffer-menu
  (list
   "JDE New"
   ["Class..."         jde-gen-class-buffer t]
   ["Console..."       jde-gen-console-buffer t]
   ["Other..."         jde-gen-buffer t]
   )
  "Menu for creating new Java buffers.")

;; Add JDE New menu to Emacs Files menu.
(if (not jde-xemacsp)
    (let* ((mb (assq 'menu-bar global-map))
	   (files (assq 'files mb))
	   (menu (if (fboundp 'easy-menu-create-menu)
		     (easy-menu-create-menu 
		      (car jde-new-buffer-menu) (cdr jde-new-buffer-menu))
		   (easy-menu-create-keymaps 
		    (car jde-new-buffer-menu) (cdr jde-new-buffer-menu))))     
	   (menu-name (car jde-new-buffer-menu)))
      (define-key-after (cdr (cdr files)) [jde-new]
	(cons menu-name menu)
	'open-file))
  (unless (featurep 'infodock)
    (add-submenu '("File") jde-new-buffer-menu "Insert File...")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Classpaths                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-cygpath (path &optional direction)
  "Converts a path from cygwin to DOS form if DIRECTION is nil.
Otherwise, it converts the path to cygwin form.  Requires that cygpath
be in your path."
  (interactive "sPath: ")
  (if (executable-find "cygpath")
      (save-excursion
	(let ((buf-name "*cygwin-output*")
	      (output-type (if direction "-u" "-w")))
	  (shell-command 
	   (concat "cygpath " output-type " -p '" path "'") buf-name)
	  (set-buffer buf-name)
	  (let ((output (buffer-substring (point-min) (point-max))))
	    (kill-buffer buf-name)
	    (substitute ?\/ ?\\ (remove ?\n output)))))
    (error "Cannot find cygpath executable.")))

(defun jde-cygwin-path-converter-cygpath (path)
  (interactive "sPath: ")
  (if (string-match "^[a-zA-Z]:" path)
      path
    (jde-cygpath path)))

(defun jde-cygwin-path-converter-internal (path)
  "Convert cygwin style PATH to a form acceptable to java vm.  Basically
converts paths of the form: '//C/dir/file' or '/cygdrive/c/dir/file' to
'c:/dir/file'.  This function will not modify standard unix style paths
unless they begin with '//[a-z]/' or '/cygdrive/[a-z]/'."
  (interactive "sPath: ")
  (let* ((path-re "/\\(cygdrive\\)?/\\([a-zA-Z]\\)/")
         (subexpr 2)
         (index1 (* 2 subexpr))
         (index2 (1+ index1)))
    (if (string-match (concat "^" path-re) path)
	(let ((new-path
	       (concat (substring path 
				  (nth index1 (match-data)) 
				  (nth index2 (match-data)))
		       ":/" 
		       (substring path (match-end 0)))))
	  (while (string-match (concat ":" path-re) new-path)
	    (setq new-path
		  (concat
		   (substring new-path 0 (match-beginning 0))
		   ";"
		   (substring new-path 
				  (nth index1 (match-data)) 
				  (nth index2 (match-data)))
		   ":/" 
		   (substring new-path (match-end 0)))))
	  (substitute ?\\ ?\/ new-path))
      path)))


(defcustom jde-cygwin-path-converter '(jde-cygwin-path-converter-internal)
  "Function to use to convert cygwin paths to DOS paths.  
Choose jde-cygwin-path-converter-internal, jde-cygwin-path-converter-cygpath,
or \"custom-function.\" jde-cygwin-path-converter-cygpath handles all
cygwin-style paths, including mount points, e.g.,/bin. 
jde-cygwin-path-converter-internal does not handle mount
paths. However, it is much faster as it does not require running a
subprocess every time the JDE needs to convert a path. Choose
\"custom-function\" if you want the JDE to use a function that you
supply. Replace \"custom-function\" with the name of the function that
you want to use."
  :group 'jde-project
  :type  '(list
	   (radio-button-choice :format "%t \n%v"
			       :tag "Converter: "
			       :entry-format "  %b %v"
			       (const jde-cygwin-path-converter-internal)
			       (const jde-cygwin-path-converter-cygpath)
			       (function custom-function))))
		       

(defun jde-convert-cygwin-path (path &optional separator)
  "Convert cygwin style PATH to a form acceptable to java vm, using
the conversion function specified by `jde-cygwin-path-converter'."
  (interactive "sPath: ")
  (funcall (car jde-cygwin-path-converter) 
	   (if separator (substitute ?\: (string-to-char separator) path) path)))

(defcustom jde-resolve-relative-paths-p t
  "If this variable is non-nil, the JDE converts relative paths to 
absolute paths. The JDE does this by appending the relative path to the path
of the project file for the current source buffer, if such
a file exists. Otherwise, the JDE appends the relative path to the path
of the current directory."
  :group 'jde-project
  :type 'boolean)

(defun jde-normalize-path (path &optional symbol) 
  "This function performs the following transformation on PATH:

  * Replaces environment variables of the form $VAR or ${VAR} with
    their values. Note that you must use the Unix notation for
    environment variables on the native Windows versions of Emacs and
    XEmacs.

  * Replaces the tilde character with the value of the home directory,
    typically specified by the HOME environment variable.

  * Converts Cygwin style paths to DOS notation on Windows.

  * Converts relative paths to absolute paths if
    `jde-resolve-relative-paths-p' is non-nil.  Paths are resolved
    according to the location deepest project file found, or if
    optional SYMBOL is non-nil, paths are resolved to the location of
    the deepest project file found that defines SYMBOL.

Note: PATH can either be a path string or a symbol corresponding to a
variable that holds a path string, in which case the optional arg
SYMBOL is unnecessary."
  (if (symbolp path)
      (setq symbol path
	    path (symbol-value symbol)))
  (let* ((directory-sep-char ?/)
	 (p (substitute-in-file-name path))
	(len (length p)))
    (if (and
	 jde-resolve-relative-paths-p
	 (> len 0)
	 (eq (aref p 0) ?.))
	(let* (prj-file-path
	       (dir (file-name-directory (or (buffer-file-name)
					     default-directory))))
	  ;; find the deepest originating project for the symbol
	  ;; based on the current directory, and resolve to that
	  ;; project's directory
	  (if symbol
	      (let ((prjs (get symbol 'jde-project))
		    (sort-fn
		     (lambda (x1 x2) 
		       (let* ((dir1 (file-name-directory (car x1)))
			      (dir2 (file-name-directory (car x2)))
			      match1 match2)
			 (if (null dir1)
			     (null dir2)
			   (if (null dir2)
			       t
			     (setq match1 (compare-strings
					   dir1 0 (length dir1)
					   dir 0 (length dir1)))
			     (setq match2 (compare-strings
					   dir2 0 (length dir2)
					   dir 0 (length dir2))))
			   (cond
			    ((not (eq match1 t))
			     (if (eq match2 t)
				 nil
			       (> (length dir1) (length dir2))))
			    ((not (eq match2 t))
			     t)
			    ((> (length dir1) (length dir2)))))))))
		(setq prjs (sort prjs sort-fn))
		(setq prj-file-path (caar prjs)))
	    (setq prj-file-path 
		  (jde-find-project-file dir)))
	  (if prj-file-path
	      (setq dir (file-name-directory prj-file-path))
	    (setq dir default-directory))
	  (if (and (> len 1)
		   (eq (aref p 1) ?.))
	      ;; path actually begins with `..', so normalize to one
	      ;; directory up
	      (save-match-data
		(string-match "\\.+/?" p)
		(setq p (expand-file-name (substring p (match-end 0))
					  (expand-file-name (concat dir "../")))))
	    (setq p (expand-file-name p dir))))
      (setq p (expand-file-name p)))
    (jde-convert-cygwin-path p)))

(defun jde-expand-classpath (classpath)
  "Replaces paths to directories named lib with paths
to jar or zip files in those directories."
  (if jde-expand-classpath-p
      (let (paths)
	(loop for path in classpath do
	      (if (and 
		   (file-exists-p path)
		   (file-directory-p path)
		   (string-match jde-lib-directory-name
				 (file-name-nondirectory path)))
		  (progn
		    (setq paths 
			  (append paths 
				  (directory-files path t "\\.jar$")))
		    (setq paths
			  (append paths 
				  (directory-files path t "\\.zip$"))))
		(setq paths (append paths (list path)))))
	paths)
    classpath))


(defun jde-build-classpath (paths &optional symbol)
  "Builds a classpath from PATHS.  PATHS is a either list of paths or
a symbol whose value is a list of paths, in which case the optional
arg SYMBOL is unnecessary."
  (if (symbolp paths)
      (setq symbol paths
	    paths (symbol-value symbol)))
  (mapconcat
   (lambda (path)
     (jde-normalize-path path symbol))
   (jde-expand-classpath 
    (mapcar
     (lambda (path)
       (jde-normalize-path path symbol))
     paths))       
   jde-classpath-separator))

(defun jde-global-classpath ()
  "Builds a classpath string from the path entries in
`jde-global-classpath'."
  (jde-build-classpath 'jde-global-classpath))


(defun jde-build-path-arg (arg path-list &optional quote symbol)
"Build a command-line path argument from a list of paths."
  (let ((path (jde-build-classpath path-list symbol)))
    (if quote
        (setq path (concat "\"" path "\"")))
    (setq path (concat arg " " path))))


(defun jde-build-classpath-arg (path-list &optional quote symbol)
"Build a classpath from a list of paths."
 (jde-build-path-arg "-classpath" path-list quote symbol))

(defun jde-root-dir-p (dir)
  (let ((parent (expand-file-name  "../" dir)))
    (cond 
     ((and
       (fboundp 'ange-ftp-ftp-name)
       (ange-ftp-ftp-name dir))
      (ange-ftp-get-file-entry parent))
     ((eq system-type 'windows-nt)
      ;; If the current directory tree is on a 
      ;; virtual drive created by the subst command
      ;;
      ;;  (not (file-exists-p parent)) 
      ;;  
      ;; fails. Hence, the following hack contributed
      ;; by Nat Goodspeed.
      (or 
       (string= parent "//") ; for paths like //host/d/prj/src
       (string= parent "\\\\") ; for paths like \\host\d\prj\src
       (string= (substring parent -3) "../"))) ; for paths like d:/prj/src
     ((eq system-type 'cygwin32)
      (or (string= (file-truename dir) "/") 
	  (not (file-exists-p (file-truename dir)))))
     (t
      (or (or (not (file-readable-p dir))
	      (not (file-readable-p parent)))
	  (and 
	   (string= (file-truename dir) "/")
	   (string= (file-truename parent) "/")))))))

(defun jde-get-global-classpath () 
  "Return the value of `jde-global-classpath', if defined, otherwise
the value of the CLASSPATH environment variable converted to a list."
  (if jde-global-classpath 
      jde-global-classpath
    (let ((cp (getenv "CLASSPATH")))
      (if (stringp cp)
	  (split-string cp jde-classpath-separator)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Project Files                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst jde-project-file-version "1.0"
  "*The current JDE project file version number.")

(defvar jde-loaded-project-file-version nil
  "*Temporary var that holds the project file version of the project
being loaded.")

(defun jde-project-file-version (ver)
  (setq jde-loaded-project-file-version ver))

(defun jde-find-project-file (dir)
  "Finds the next project file upwards in the directory tree
from DIR. Returns nil if it cannot find a project file in DIR
or an ascendant directory."
  (let ((directory-sep-char ?/) ;; Override NT/XEmacs setting
	(file (find jde-project-file-name
		    (directory-files dir) :test 'string=)))
    (if file
	(expand-file-name file dir)
      (if (not (jde-root-dir-p dir))
	  (jde-find-project-file (expand-file-name "../" dir))))))

(defun jde-find-project-files (dir)
  "Return all the project files in the current directory tree,
starting with the topmost."
  (let ((file (jde-find-project-file dir))
	current-dir files)
    (while file
      (setq files (append (list file) files))
      (setq current-dir (file-name-directory file))
      (setq 
       file
       (if (not (jde-root-dir-p current-dir))
	   (jde-find-project-file
	    (expand-file-name "../" current-dir)))))
    files))
      

(defun jde-load-project-file ()
  "Load the project file(s) for the Java source file in the current
buffer. Search for all the project file first in the directory
tree containing the current source buffer. If any files are found,
first reset all variables to their startup values. Then load
the project files starting with the topmost in the tree.
If no project files are found, set the JDE variables to their
Emacs startup values."
  (interactive)
  (let ((prj-files (jde-find-project-files default-directory)))
    (if prj-files
	(progn
	  (jde-set-variables-init-value)
	  (loop for file in prj-files do
		(let ((jde-loading-project file))
		  (jde-log-msg "jde-load-project-file: Loading %s" file)
		  ;; reset project file version
		  (setq jde-loaded-project-file-version nil)
		  (load-file file))))
      (jde-set-variables-init-value t))))


(defun jde-load-all-project-files ()
  (interactive)
  "Loads the project file associated with each Java source buffer."
  (mapc
   (lambda (java-buffer)
     (save-excursion
       (set-buffer java-buffer)
       (message "Loading project file for %s ..." 
		(buffer-file-name java-buffer))
       (jde-load-project-file)))
   (jde-get-java-source-buffers)))

;;;###autoload
(defun jde-open-project-file ()
  "Opens the project file for the Java source file in the
current buffer."
  (interactive)
  (let ((prj-file (jde-find-project-file default-directory)))
    (if prj-file
	(find-file prj-file)
      (message "%s" "Project file not found."))))


(defun jde-save-delete (symbol buffer)
  "Delete the call to SYMBOL from project file in BUFFER.
Leave point at the location of the call, or after the last expression."
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (catch 'found
      (while t
	(let ((sexp (condition-case nil
			(read (current-buffer))
		      (end-of-file (throw 'found nil)))))
	  (when (and (listp sexp)
		     (eq (car sexp) symbol))
	    (delete-region (save-excursion
			     (backward-sexp)
			     (point))
			   (point))
	    (throw 'found nil)))))
    (unless (bolp)
      (princ "\n"))))

(defun jde-symbol-p (symbol)
  "Returns non-nil if SYMBOL is a JDE variable."
  (and (get symbol 'custom-type)
       (or (string-match "^bsh-" (symbol-name symbol))
	   (string-match "^jde-" (symbol-name symbol)))))

(defvar jde-symbol-list nil
  "*A list of jde variables which are processed by `jde-save-project'.")

(defun jde-symbol-list ()
  "Return a list of variables processed by `jde-save-project'.
The first time this is called, the list is saved in jde-symbol-list."
  (or jde-symbol-list
      (mapatoms
       (lambda (symbol)
         (if (jde-symbol-p symbol)
             (setq jde-symbol-list (cons symbol jde-symbol-list))))))
  jde-symbol-list)

(defun jde-set-project-name (name)
  (put 'jde-project-name 'customized-value (list name))
  (setq jde-project-name name))

(defun jde-put-project (symbol project value)
  "Stores a new value for SYMBOL in PROJECT, or overwrites any
existing value."
  (let ((proj-alist (get symbol 'jde-project)))
    (if (null proj-alist)
	(put symbol 'jde-project (list (cons project (list value))))
      (if (assoc project proj-alist)
	  (setcdr (assoc project proj-alist) (list value))
	(put symbol 'jde-project (pushnew (cons project (list value)) proj-alist))))))

(defun jde-get-project (symbol project)
  "Gets the value for SYMBOL that is associated with PROJECT, or nil
if none.  To test if SYMBOL has any value for PROJECT, use
`jde-project-present-p'."
  (car-safe (cdr-safe (assoc project (get symbol 'jde-project)))))

(defun jde-project-present-p (symbol project)
  "Returns non-nil if SYMBOL has a value for PROJECT."
  (assoc project (get symbol 'jde-project)))

(defun jde-save-open-buffer (project)
  "Creates a new buffer or opens an existing buffer for PROJECT."
  (let ((auto-insert nil)		; turn off auto-insert when
	buffer standard-output)		; creating a new file
    (setq buffer (find-file-noselect project))
    (setq standard-output buffer)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (jde-save-delete 'jde-project-file-version buffer)
      (delete-blank-lines)
      (jde-save-delete 'jde-set-variables buffer)
      (delete-blank-lines)
      (jde-save-delete 'jde-set-project-name buffer)
      (delete-blank-lines))
    (princ "(jde-project-file-version ")
    (prin1 jde-project-file-version)
    (princ ")\n")
    (princ "(jde-set-variables")
    (jde-log-msg "jde-save-open-buffer: Opening buffer for %s" project)
    buffer))

(defun jde-save-close-buffer (project)
  "Saves and closes the buffer associated with PROJECT."
  (let* ((buffer (find-if (lambda (buf)
			    (string= project (buffer-file-name buf)))
			  (buffer-list)))
	 (standard-output buffer))
    (if buffer
      (progn
	(princ ")\n")
	(save-excursion
	  (set-buffer buffer)
	  (save-buffer))
	(jde-log-msg "jde-save-close-buffer: Closing buffer for %s" project)
	(kill-buffer buffer))
      (jde-log-msg "jde-save-close-buffer: Unable to find buffer for %s" project))))

(defun jde-save-variable (symbol projects)
  "Saves all of the values of SYMBOL for each project file mentioned
in PROJECTS."
  (mapc
   (lambda (project)
     (if (and (not (string= (car project) "default"))
	      (member (car project) projects))
	 (let ((buffer (find-if (lambda (buf)
				  (string= (car project) (buffer-file-name buf)))
				(buffer-list)))
	       standard-output)
	   (if (null buffer)
	       (setq standard-output (setq buffer (jde-save-open-buffer (car project))))
	     (setq standard-output buffer))
	   (jde-log-msg "jde-save-variable: Saving %S in %s" symbol (car project))
	   (princ "\n '(")
	   (princ symbol)
	   (princ " ")
	   (prin1 (custom-quote (car (cdr project))))
	   (princ ")"))))
   (get symbol 'jde-project)))

(defun jde-save-needs-saving-p (symbol projects)
  "Function used internally by the project saving mechanism to
determine whether or not to save a symbol in a project file.  If there
are settings to be saved, this function also resolves which project
should receive the customized values."
  (unless (= (length projects) 0)
    (let ((value (symbol-value symbol))
	  val-to-save
	  current-proj proj-iter)
      (setq current-proj (car projects))
      (cond
       ;; CASE: current value changed from saved value in current
       ;; project
       ((and (jde-project-present-p symbol current-proj)
	     (not (equal value (jde-get-project symbol current-proj))))
	(jde-log-msg "jde-save-needs-saving-p: changed value for %S in project `%s'"
		     symbol current-proj)
	(jde-put-project symbol current-proj value)
	t)
       ;; CASE: no value for symbol in current project - check all
       ;; parent projects (plus default) to see if value has changed
       ((and (not (jde-project-present-p symbol current-proj))
	     (progn
	       (setq val-to-save value)
	       (setq proj-iter (cdr projects))
	       (while (and proj-iter
			   (not (jde-project-present-p symbol (car proj-iter))))
		 (setq proj-iter (cdr proj-iter)))
	       (if proj-iter
		   (not (equal value
			       (jde-get-project symbol (car proj-iter))))
		 (setq proj-iter (list "default"))
		 (setq val-to-save (eval (car (get symbol 'customized-value))))
		 (and (not (null (get symbol 'customized-value)))
		      (not (equal val-to-save
				  (and (jde-project-present-p symbol (car proj-iter))
				       (jde-get-project symbol (car proj-iter)))))))))
	(jde-log-msg "jde-save-needs-saving-p: override value %S from parent `%s' in project `%s'"
		     symbol (car proj-iter) current-proj)
	(jde-put-project symbol current-proj val-to-save)
	t)
       ;; CASE: current value same as value in the deepest project that
       ;; holds that value - re-save it
       ((progn
	  (setq proj-iter projects)
	  (while (and proj-iter
		      (not (jde-project-present-p symbol (car proj-iter))))
	    (setq proj-iter (cdr proj-iter)))
	  (if proj-iter
	      (equal value (jde-get-project symbol (car proj-iter)))))
	(jde-log-msg "jde-save-needs-saving-p: original value for %S in project `%s'"
		     symbol (car proj-iter))
	t)))))

(defun jde-save-project-internal (projects)
  (let ((projects-reversed (nreverse projects)))
    (jde-log-msg "jde-save-project-internal: projects: %S" projects-reversed)
    (mapc 'jde-save-open-buffer projects-reversed)
    (mapc (lambda (symbol)
	    (if (jde-save-needs-saving-p symbol projects-reversed)
		(jde-save-variable symbol projects-reversed)))
	  (jde-symbol-list))
    (mapc 'jde-save-close-buffer projects-reversed)))

;;;###autoload
(defun jde-save-project ()
  "Saves source file buffer options in one or more project files.
This command provides an easy way to create and update a project file
for a Java project. Simply open a source file, set the desired
options, using the JDE Options menu, then save the settings in the
project file, using this command.  Now, whenever you open a source
file from the same directory tree, the saved settings will be restored
for that file."
  (interactive)
  (let ((project-file-paths (jde-find-project-files default-directory)))
    (if (not project-file-paths)
	(setq project-file-paths
	      (list (expand-file-name jde-project-file-name
				      (read-file-name "Save in directory: "
						      default-directory
						      default-directory)))))
    (jde-save-project-internal project-file-paths)))

;;;###autoload
(defun jde-create-new-project (new-dir)
  "Creates a new JDE project file in directory NEW-DIR, saving any
current customized variables.  If a project file already exists in the
given directory, the project is simply re-saved.  This functions the
same as `jde-save-project' when no project files can be found for the
current source file.  But, if there already exist projects somewhere
along the path, this command unconditionally creates a project file in
the directory specified, thus allowing the user to create and maintain
hierarchical projects."
  (interactive "DCreate new project in directory: ")
  (let ((prj-file (expand-file-name jde-project-file-name new-dir))
	(projects (jde-find-project-files new-dir)))
    (if (not (member prj-file projects))
	;; create empty project file if none found
	(let* ((auto-insert nil)	; disable auto-insert
	       (standard-output (find-file-noselect prj-file))	
	       (message-log-max nil))	; disable message log
	  (princ "(jde-project-file-version ")
	  (prin1 jde-project-file-version)
	  (princ ")\n(jde-set-variables)\n")
	  (save-excursion
	    (set-buffer standard-output)
	    (save-buffer))
	  (kill-buffer standard-output)
	  (setq projects (nconc projects (list prj-file)))))
    (jde-save-project-internal projects)))

(defun jde-set-variables (&rest args)
  "Initialize JDE customization variables.  

Takes a variable number of arguments. Each argument 
should be of the form:

  (SYMBOL VALUE)

The value of SYMBOL is set to VALUE.
"
  (while args 
    (let ((entry (car args)))
      (if (listp entry)
	  (let* ((symbol (nth 0 entry))
		 (value (nth 1 entry))
		 (customized (nth 2 entry))
		 (set (or (and (local-variable-if-set-p symbol nil) 'set)
			  (get symbol 'custom-set)
			  'set-default)))
	    (if (or customized
		    jde-loaded-project-file-version)
		(put symbol 'customized-value (list value)))
	    (if (boundp 'jde-loading-project)
		(progn
		  (jde-log-msg "jde-set-variables: Loading %S from project %s" symbol
			       (symbol-value 'jde-loading-project))
		  (jde-put-project symbol
				   (symbol-value 'jde-loading-project)
				   (eval value)))
	      (jde-log-msg "jde-set-variables: Loading %S from unknown project" symbol))
	    (when (default-boundp symbol)
	      ;; Something already set this, overwrite it
	      (funcall set symbol (eval value)))
	    (setq args (cdr args)))))))

(defun jde-set-variables-init-value (&optional msg)
  "Set each JDE variable to the value it has at Emacs startup."
  (interactive)
  (if (or (interactive-p) msg)
      (message "Setting JDE variables to startup values..."))
  (mapcar 
   (lambda (symbol) 
     (let ((val-to-set (eval (car (or (get symbol 'saved-value)
				      (get symbol 'standard-value)))))
	   (set (or (get symbol 'custom-set) 'set-default)))
       (if (or (get symbol 'customized-value)
	       (get symbol 'jde-project))
	   (funcall set symbol val-to-set))
       (put symbol 'customized-value nil)
       (put symbol 'jde-project nil)
       (jde-put-project symbol "default" val-to-set)))
   (jde-symbol-list)))

;; Code to update JDE customization variables when a user switches
;; from a Java source buffer belonging to one project to a buffer
;; belonging to another.

(setq jde-current-project "")

(defun jde-reload-project-file ()
  "If project context-switching is enabled (see
`jde-project-context-switching-enabled-p'), reloads the project file
for a newly activated Java buffer when the new buffer's project
differs from the old buffer's."
  (interactive)
  (let ((project-file-path (jde-find-project-file default-directory)))
    (if (not project-file-path) (setq project-file-path ""))
    (if (and 
	 jde-project-context-switching-enabled-p
	 (not (jde-debugger-running-p))
	 (not (string= jde-current-project project-file-path)))
	(progn
	  (setq jde-current-project project-file-path)
	  (jde-load-project-file)
	  (jde-wiz-set-bsh-project)))))

(defun jde-debugger-running-p () 
  (and 
   (jde-dbs-debugger-running-p)
   (jde-dbs-get-target-process)))

(defcustom jde-entering-java-buffer-hook 
  '(jde-reload-project-file 
    jde-which-method-update-on-entering-buffer)
  "*Lists functions to run when entering a Java source buffer."
  :group 'jde-project
  :type 'hook)

(defvar jde-current-buffer (current-buffer)
  "*Internal JDE variable that holds the current active buffer.")

(defun jde-detect-java-buffer-activation ()
  "Detects when a user activates a buffer.
If the activated buffer is a Java buffer, runs the 
`jde-entering-java-buffer-hook' hooks."
  (let ((curr-buff (current-buffer)))
    (if (not (equal curr-buff jde-current-buffer))
	(progn
	  (setq jde-current-buffer curr-buff)
	  (if (eq major-mode 'jde-mode)
	      (run-hooks 'jde-entering-java-buffer-hook))))))

(defun jde-count-open-java-buffers ()
  "Returns non-nil if any java buffers are open."
  (count 
   ".java"
   (buffer-list)
   :test
   (lambda (file-type buffer)
     (let ((file-name (buffer-file-name buffer)))
       (if file-name
	   (string-match file-type file-name))))))
	 

(defun jde-remove-jde-hook ()
  "Removes `jde-detect-java-buffer-activation-hook' when
all Java source buffers have been closed."
  (unless (> (jde-count-open-java-buffers) 1)
  (remove-hook 'post-command-hook 'jde-detect-java-buffer-activation)))

(add-hook 'kill-buffer-hook 'jde-remove-jde-hook)


;; JDE help

(defun jde-find-jde-data-directory ()
  "Return the path of the JDE data directory.
Returns the path of the directory containing the
JDE java and documentation directories;  nil if the 
directory cannot be found. If XEmacs, returns the location of
the data directory in the XEmacs distribution hierarchy. On all other Emacs versions, 
the JDE expects to find the documentation and Java class directories
in the same directory that contains the JDE lisp directory."
  (let (dir)
    (if jde-xemacsp
	(progn
	  (setq dir (locate-data-directory "jde"))
	  (when (not dir)
	      (setq dir (file-name-directory (locate-library "jde")))
	      (setq dir (substring dir 0 (- (length dir) 5)))))
      (setq dir (file-name-directory (locate-library "jde"))))
    (if dir
	(nsubstitute ?/ ?\\ dir))
    (if (not jde-xemacsp)
	(setq dir (substring dir 0 (- (length dir) 5))))
    dir))

(defun jde-find-jde-doc-directory ()
  "Return the path of the JDE documentation directory.
Returns  nil if the directory cannot be found. At some
point, XEmacs will include the JDE. Versions of XEmacs
that include JDE will store the JDE doc in a data
directory called jde. On all other Emacs versions, the JDE
expects to find the documentation in a subdirectory 
named doc of the directory that contains the file
jde.el."
  (jde-find-jde-data-directory))

;;;###autoload
(defun jde-show-help ()
  "Displays the JDE User's Guide in a browser."
  (interactive)
  (let* ((jde-dir (jde-find-jde-doc-directory))
         (jde-help
          (if jde-dir
	      (expand-file-name "doc/html/jde-ug/jde-ug.html" jde-dir))))      
    (if (and
         jde-help
         (file-exists-p jde-help))
        (browse-url (concat "file://" (jde-convert-cygwin-path jde-help))
                    browse-url-new-window-p)
      (signal 'error '("Cannot find JDE help file.")))))

(defun jde-debug ()
"*Runs the debugger specified by `jde-db-debugger'."
  (interactive)
  (if (string= (car jde-db-debugger) "JDEbug")
      (jde-bug-debug-app)
    (jde-db)))

;;
;; Problem reporting functions contributed by Phillip Lord <plord@hgmp.mrc.ac.uk>.
;;
(defvar jde-problem-report-mail-address "pkinnucan@mediaone.net" )

(defun jde-submit-problem-report()
  "Submit a problem report for the JDE" 
  (interactive)
  (require 'reporter)
  (and 
   (y-or-n-p "Do you want to submit a problem report on the JDE? ")
   (progn
     (message "Preparing problem report...")
     ;;prepare the basic buffer
     (reporter-submit-bug-report
      jde-problem-report-mail-address
      (concat "JDE version " jde-version)
      (jde-problem-report-list-all-variables)
      nil
      'jde-problem-report-post-hooks
      "Please enter the details of your bug report here" )
     (message "Preparing bug report...done"))))


(defun jde-problem-report-post-hooks()
  "Function run the reporter package done its work.
It looks for a JDEBug buffer and inserts the contents of that, and then prompts 
for insertion of the .emacs file"
  (save-excursion 
    (goto-char (point-max))
    (let* ((debug-buffer (get-buffer "*JDEbug*"))
	   (messages-buffer 
	    (get-buffer
	     (if jde-xemacsp " *Message-Log*" "*Messages*")))
	   (backtrace-buffer (get-buffer "*Backtrace*"))
	   (jde-log-buffer (get-buffer "*jde-log*"))
	   (process 
	    (let ((proc (jde-dbs-get-target-process)))
	      (if (not proc)
		  (let ((dead-proc-alist 
			 (oref jde-dbs-the-process-morgue proc-alist)))
		    (if dead-proc-alist
			(setq proc (cdr (car dead-proc-alist))))))
	      proc))
	   (cli-buffer (if (and process (slot-boundp process 'cli-buf))
			   (oref process cli-buf)))
	   (locals-buffer (if (and process (slot-boundp process 'locals-buf))
			      (oref process locals-buf)))
	   )

      ;;insert the contents of the debug buffer if it is there. 
      (if debug-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the *JDEBug* buffer were\n\n")
	    (insert-buffer debug-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert *JDEbug* buffer" ))
	(insert-string "\n\n\nThere was no *JDEBug* buffer" ))

      ;;insert the contents of the CLI buffer if it exists.
      (if cli-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the CLI buffer are\n\n")
	    (insert-buffer cli-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert CLI buffer" ))
	(insert-string "\n\n\nThere is no CLI buffer" ))


      ;;insert the contents of the locals buffer if it exists.
      (if locals-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the locals buffer are\n\n")
	    (insert-buffer locals-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert locals buffer" ))
	(insert-string "\n\n\nThere is no locals buffer" ))

      ;;insert the contents of the backtrace buffer if it is there. 
      (if backtrace-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the *Backtrace* buffer were\n\n")
	    (insert-buffer backtrace-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert *Backtrace* buffer" ))
	(insert-string "\n\n\nThere was no *Backtrace* buffer" ))


      ;;insert the contents of the messages buffer if it is there. 
      (if messages-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the *Messages* buffer were\n\n")
	    (insert-buffer messages-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert *Messages* buffer" ))
	(insert-string "\n\n\nThere was no *Messages* buffer" ))

      ;;insert the contents of the jde-log buffer if it is there. 
      (if jde-log-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the *jde-log* buffer were\n\n")
	    (insert-buffer jde-log-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert *jde-log* buffer" ))
	(insert-string "\n\n\nThere was no *jde-log* buffer" )))

    (when process-environment
      (insert-string "\n\n\nProcess environment: \n\n")
      (insert-string (mapconcat (lambda (var) var) process-environment "\n")))

    (let ((buf (get-buffer-create "*Insert .emacs*"))
	  (mail-buf (current-buffer)))
      
      (set-buffer buf)
      (widget-insert "It is requested that you send the entire contents of your .emacs file.\n")
      (widget-insert "This is because it has been found that those parts of the .emacs file\n" )
      (widget-insert "which appear not to be JDE related often do in fact contain the cause of\n")
      (widget-insert "reported bugs.\n\n")
      (widget-insert "If you do not want to send the contents of your .emacs or you load a large\n" )
      (widget-insert "large number of files from your full .emacs file, then please attempt to\n" )
      (widget-insert "replicate the bug using the minimal .emacs file suggested in the JDE\n" )
      (widget-insert "documentation, and note that you have done this in this bug report\n" )
      (widget-insert "If you choose to do neither of these things we may not be able to\n" )
      (widget-insert "or necessarily want to help determine the cause of the problem!\n" )
      (switch-to-buffer "*Insert .emacs*")
      
      (set-buffer mail-buf)
      (goto-char (point-max))
      (if (y-or-n-p "Insert your .emacs file into the problem report? " )
	  (progn
	    (insert-string "\n\n\nThe contents of the .emacs file was\n\n\n")
	    (insert-file "~/.emacs")
	    (goto-char (point-max))
	    (insert-string "\n\n\n=====end inserted .emacs file"))
	(insert-string "\n\n\nThe user choose not to insert their .emacs file\n" ))
      ;;clean up the prompt buffer
      (kill-buffer buf))))

(defun jde-problem-report-list-all-variables()
  "List all variables starting with `jde' or `bsh'."
  (let ((vars))
    (mapatoms
     (lambda (symbol)
       (when 
	   (and (or 
		 (string-match "bsh-" (symbol-name symbol))
		 (string-match "jde-" (symbol-name symbol)))
		(get symbol 'custom-type))
	 (setq vars (cons symbol vars)))))
    vars))


;; Line numbering support.
(eval-when (compile)
  (require 'setnu))

(defvar jde-setnu-deletion-check t "deletion check")
(make-variable-buffer-local 'jde-setnu-deletion-check)

(defun jde-setnu-after-change (start end length)
 "When in setnu-mode, toggles setnu-mode off and on."
   (if setnu-mode
       (if (or
	    (and
	     (> length 0)
	     jde-setnu-deletion-check)
	    (string-match 
		  "[\n\r]" 
		  (buffer-substring-no-properties start end)))
	   (run-with-timer 
	    0.001 nil
	    ;; setnu toggler      
	   (lambda () (setnu-mode) (setnu-mode))))
     (setq jde-setnu-deletion-check nil)))

(defun jde-setnu-before-change (start end) 
  "Determines whether any newlines were deleted."
   (if setnu-mode
       (if (> end start) 
	   (setq jde-setnu-deletion-check 
		 (string-match "[\n\r]" (buffer-substring-no-properties start end))))))


(defcustom jde-setnu-mode-threshold 20000
 "Maximum number of bytes in a file (buffer) that can result in
automatic line numbering."
 :group 'jde-project
 :type 'integer)

(defcustom jde-setnu-mode-enable nil
 "Enable numbering of lines in Java source buffers."
 :group 'jde-project
 :type 'boolean
 :set '(lambda (sym val)
	 (if val
	     (progn
	       (require 'setnu)
	       (add-hook 
		'after-change-functions 
		'jde-setnu-after-change)
	       (add-hook 
		'before-change-functions 
		'jde-setnu-before-change)
	       (mapc
		(lambda (buf)
		  (save-excursion
		    (set-buffer buf)
		    (if (and
			 (not setnu-mode)
			 (< (point-max) jde-setnu-mode-threshold))
			(setnu-mode 1))))
		  (jde-get-java-source-buffers)))
	   (progn
	     (mapc 
	      (lambda (buf)
		(save-excursion
		  (set-buffer buf)
		  (if (and (boundp 'setnu-mode)
			   setnu-mode)
		      (setnu-mode))))
	      (jde-get-java-source-buffers))))	 
	 (set-default sym val)))

;; jde-describe-map is Ehud Karni's describe map with jde prepended.
(defun jde-keymap-test (var)           ; internal function for keymap checking
       (and (boundp var)
            (keymapp (symbol-value var))))

(defun jde-describe-map (map)          ; display map binding
 "Display binding for MAP which must be a quoted keymap variable"
  (interactive
       (let ((map (intern (completing-read "Key map: " obarray 'jde-keymap-test 1))))
           (list map)))
       (let ((val (symbol-value map)))
           (or (keymapp val)
               (error "%s is not a keymap !" (symbol-name map)))
           (with-output-to-temp-buffer "*Help*"
               (princ (format "Binding for keymap %s is:\n" (symbol-name map)))
               (princ (substitute-command-keys "\\{val}" ))
               (print-help-return-message))))

(defun jde-keys ()
  "Displays JDE key bindings. Use `jde-bug-keys' to display JDEbug keybindings ."
  (interactive)
  (jde-describe-map 'jde-mode-map))


;; Contributed by John Ciolfi, jciolfi@mathworks.com.
(defun jde-compile-file-if-necessary (file)
  "Compile the JDE file FILE if necessary.
This is done if FILE.el is newer than FILE.elc or if FILE.elc doesn't exist."
  (if (string= (file-name-extension file) "el")
      (let* ((root (file-name-sans-extension file))
	     (elc-file (concat root ".elc")))
	(if (or (not (file-exists-p elc-file))
		(file-newer-than-file-p file  elc-file))
	    (progn
	      (message (format "Byte-compiling %s..." 
			       (file-name-nondirectory file)))
	      (byte-compile-file file))))))

;;;###autoload
(defun jde-compile-jde ()
  "Byte-compile all uncompiled files of jde."

  ;; Be sure to have . in load-path since a number of files in jde
  ;; depend on other files and we always want the newer one even if
  ;; a previous version of jde exists.

  (interactive)
  (let ((load-path (append '(".") load-path))
	(jde-lisp-directory (expand-file-name "lisp" (jde-find-jde-data-directory))))
    (save-excursion 
      (mapcar 
       (function jde-compile-file-if-necessary)
       (directory-files jde-lisp-directory t)))))

;; Provided for XEmacs compatibility.
(if (not (fboundp 'subst-char-in-string))
    (defun subst-char-in-string (fromchar tochar string &optional inplace)
      "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
      (let ((i (length string))
	    (newstr (if inplace string (copy-sequence string))))
	(while (> i 0)
	  (setq i (1- i))
	  (if (eq (aref newstr i) fromchar)
	      (aset newstr i tochar)))
	newstr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Find command                                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom jde-find-case-sensitive nil
  "*Specifies whether recursive file search are case-sensitive.
If non-nil, the search made with 'jde-find' would be case-sensitive, otherwise
case difference would be ignored."
  :group 'jde-project
  :type 'boolean
)

(defcustom jde-find-file-regexp '("*.java")
  "*Specifies the file pattern to select files to be searched.
This regular expression will be applied to the find command used to select the
files on which the grep will be applied. By default it selects all java files,
but one could modify it to also search other files such as jsp files. The
syntaxt should follow the one supported by GNU find in its -name option."
  :group 'jde-project
  :type '(repeat (string :tag "Find regexg"))
)

(defvar jde-find-root-history nil
  "History of directory trees searched in this session.")

(defvar jde-find-regexp-history nil
  "History of search expressions used in this session.")

(defun jde-find (&optional regexp)
  "Find a regular expression REGEXP in all of the current JDE project files.
It suggests the path to use to search file. By default this is the first defined
value of `jde-db-source-directories', `jde-compile-option-sourcepath', 
`jde-compile-option-classpath', or `jde-global-classpath'
This command can be customized using the `jde-find-case-sensitive' variable to
control case sensitivity and `jde-find-file-regexp' to choose the type of files
to be searched. This command requires that the Unix grep and find utilities be
installed on your system in the Emacs command path. The Cygwin package contains
Windows versions of both utilities."
  (interactive)
  (if (not (executable-find 
	    ;; Hack required by faulty XEmacs implementation of executable-find.
	    (if (eq system-type 'windows-nt) "grep.exe" "grep")))
      (error "This command requires the Unix grep utility."))
  (if (not (executable-find 
	    (if (eq system-type 'windows-nt) "find.exe" "find")))
      (error (list "This command requires the Unix find utility.")))
  (let* ((grep-roots 
	  (read-from-minibuffer 
	   "Recursive grep from directories: "
	   (cons 
	    (mapconcat 
	     'jde-normalize-path 
	     (or
	      jde-db-source-directories
	      jde-compile-option-sourcepath 
	      jde-compile-option-classpath 
	      jde-global-classpath) 
	     " ")
	    0)
	   nil nil 'jde-find-root-history))
	 (case-sensitive-option 
	  (if jde-find-case-sensitive 
	      ""
	    "-i"))
	 (regexp 
	  (if (and (boundp 'regexp) regexp) 
	      regexp
	    (read-from-minibuffer 
	     "Search for regexp: "
	     (if (boundp 'jde-find-regexp-history) 
		 (car jde-find-regexp-history)
	       nil)
	     nil nil 'jde-find-regexp-history)))
	 ;;file option would be like -name "regexp1" -or -name "regexp2" ...
	 (file-regexp 
	  (mapconcat 
	   (lambda (x) 
	     (format "-name \"%s\"" x)) 
	   jde-find-file-regexp 
	   " -or "))
	 (whole-cmd 
	  (format "find %s %s -type f | xargs grep %s -n \"%s\" /dev/null" 
		  grep-roots 
		  file-regexp 
		  case-sensitive-option 
		  regexp)))
    (grep whole-cmd)))

(defun jde-create-prj-values-str ()
  (let ((directory-sep-char ?/)) ;; Override NT/XEmacs setting
    (format "jde.util.JdeUtilities.setProjectValues(\"%s\", %s);"
	    jde-current-project 
	    (jde-build-path-arg nil jde-global-classpath t))))


(defun jde-jeval (java-statement &optional eval-return)
  (if (not (bsh-running-p))
      (bsh-eval (jde-create-prj-values-str)))
  (bsh-eval java-statement eval-return))

(defun jde-jeval-r (java-statement)
  (jde-jeval java-statement t))

(provide 'jde)

;; Change History 

;;
;; $Log: jde.el,v $
;; Revision 1.187  2001/08/24 06:01:04  paulk
;; Made jde-compile-finish-flush-completion-cache more robust.
;;
;; Revision 1.186  2001/08/14 05:45:42  paulk
;; Updated version number.
;;
;; Revision 1.185  2001/08/08 06:00:00  paulk
;; * Added some more API help commands to the JDE menu.
;; * XEmacs compatibility fix for error that occurs when the JDE tries to erase the completion cache after compiling a file.
;;
;; Revision 1.184  2001/08/04 05:34:18  paulk
;; Reorganized JDE menu somewhat.
;;
;; Revision 1.183  2001/08/04 03:46:25  paulk
;; Adds JDE->Wizards->Extend Abstract Class menu item.
;;
;; Revision 1.182  2001/07/31 05:07:40  paulk
;; Adds JDE->Wizards->Generate Get/Set Methods. Thanks to Javier Lopez and Sandip Chitale.
;;
;; Revision 1.181  2001/07/29 07:01:38  paulk
;; Fixes JDE->Help->JDE User Guide function so that it works on XEmacs.
;;
;; Revision 1.180  2001/07/18 02:41:27  paulk
;; NT/XEmacs compatibility fix: jde-find-project-file now returns paths with forward slashes as directory sep characters on NT/XEmacs.
;;
;; Revision 1.179  2001/07/17 05:42:15  paulk
;; Bump version number.
;;
;; Revision 1.178  2001/07/16 13:36:30  paulk
;; Adds jde-enable-senator variable.
;;
;; Revision 1.177  2001/07/16 13:23:48  paulk
;; Fixed bug in jde-save-needs-saving-p that caused the JDE not to save variables whose customized value is nil.
;;
;; Revision 1.176  2001/07/08 04:37:28  paulk
;; Added compatibility fixes for NT/XEmacs use of backslash as the
;; default directory sep character. Thanks to William Griswold <wgg@cs.ucsd.edu>
;;
;; Revision 1.175  2001/07/07 04:47:22  paulk
;; Adds the customization variable, jde-launch-beanshell-on-demand.
;;
;; Revision 1.174  2001/07/06 02:13:20  paulk
;; Provide support for user-supplied completion templates.
;;
;; Revision 1.173  2001/06/27 04:41:32  paulk
;; Adds the customization variable, jde-build-function, which allows selection of one a standard set of functions (jde-java-build, jde-ant-build or jde-make) or specification of a custom function.
;;
;; Removes the customization variable, jde-build-use-make.
;;
;; Thanks to Jason Stell jason.stell@globalone.net.
;;
;; Revision 1.172  2001/06/07 04:08:12  paulk
;; Fixed bug in jde-jeval.
;;
;; Revision 1.171  2001/05/31 05:14:39  paulk
;; Provide support for per-project caching of class data in the Beanshell. Thanks to Matt Conway.
;;
;; Revision 1.170  2001/05/23 03:33:56  paulk
;; Added require for semantic-load. Thanks to David Ponce.
;;
;; Revision 1.169  2001/05/22 02:54:14  paulk
;; Adds jde-find command. Thanks to "Guillaume Berche" <guillaume.berche@eloquant.com>.
;;
;; Revision 1.168  2001/05/19 02:36:01  paulk
;; Updated to support semantic 1.4. Thanks to David Ponce.
;;
;; Revision 1.167  2001/04/27 04:52:10  paulk
;; Fixes bug in jde-normalize-path's handling of paths that begin with ../. Thanks to "Nick Sieger" <nsieger@bitstream.net> for this fix.
;;
;; Revision 1.166  2001/04/26 08:46:27  paulk
;; jde-lib-directory-name can now be a regular expression that matches the name of directories that contain zip or jar files.
;;
;; Revision 1.165  2001/04/25 03:23:26  paulk
;; Fix Project->Auto Switch menu to work on XEmacs.
;;
;; Revision 1.164  2001/04/24 08:13:48  paulk
;; Adds a new menu item, JDE->Project->Auto Switch, that toggles project context switching on or off.
;;
;; Revision 1.163  2001/04/24 08:01:21  paulk
;; Define new function jde-toggle-project-switching.
;;
;; Revision 1.162  2001/04/20 20:52:13  paulk
;; Bug fix: JDE no longer overwrites local version of after-change-functions. Thanks to David Ponce.
;;
;; Revision 1.161  2001/04/19 04:40:57  paulk
;; Updated version number.
;;
;; Revision 1.160  2001/04/17 04:18:03  paulk
;; XEmacs compatibility fix: pass nil third argument to local-variable-if-set-p. Thanks to Nick Sieger.
;;
;; Revision 1.159  2001/04/16 05:41:27  paulk
;; - jde-build-classpath now optionally takes a symbol representing a string of paths.
;; - Normalized JDK doc path.
;;   Thanks to Nick Sieger for both enhancements.
;;
;; Revision 1.158  2001/04/11 03:13:55  paulk
;; This is a major update to the JDE project file system to support hierarchical
;; project files. The enhanced  code remembers which project files set
;; options and will save them back to the appropriate project
;; file when the project is saved.
;;
;; When the values of variables are newly customized, the new value is saved in
;; the deepest project file found relative to the buffer where
;; `jde-save-project' is invoked.  Thus, the buffer where you save the project
;; controls in whic project file newly customized values are saved.
;;
;; The implementation boils down to associating a new property `jde-project'
;; with each symbol that has been saved to a project file.   The property's
;; content is an alist of cons cells that look like ("<project file path>"
;; <saved value>).
;;
;; A  side-effect of the changes is where do relative paths resolve to now
;; that path-related options can be set by multiple project files?  The project
;; system solves that problem by resolving relative paths relative to the project
;; file that defines the option.
;;
;; Thanks to "Nick Sieger" <nsieger@bitstream.net> for contributing this
;; enhancement.
;;
;; Revision 1.157  2001/04/11 02:33:42  paulk
;; The JDE now replaces occurrences of lib directories in  classpaths
;; with the jar and zip files that those directories
;; contain. This means that you no longer have to include
;; jar and zip files explicitly in classpaths. See
;; jde-expand-classpath-p and jde-lib-directory-name for details.
;;
;; Revision 1.156  2001/03/30 18:04:28  paulk
;; Fix jde-save-variables to save overrides of variables customized in the user's .emacs file. Thanks to Nick Sieger <nsieger@bitstream.net> for providing this fix.
;;
;; Revision 1.155  2001/03/30 08:46:29  paulk
;; Implements hierarchical project files. The JDE saves only variables that have been customized for the current session in project files. The JDE now loads project files in the following manner. It first sets all the JDE variables to their Emacs startup values (i.e., the default value or the value saved in your .emacs file). It then loads all the project files in the directory tree containing the current source buffer, starting with the topmost file. Thanks to Nick Sieger for contributing to this enhancement.
;;
;; Revision 1.154  2001/03/29 06:19:15  paulk
;; - jde-save-project now prompts you to enter the directory in which to save a new project file.
;;
;; - jde-save-project-in prompts you to enter the directory in which to save a new or existing project file.
;;
;; Revision 1.153  2001/03/29 04:16:16  paulk
;; Adds support for relative paths. The JDE now converts relative paths to absolute paths by appending the relative path to the path of the project file for the current source buffer.
;;
;; Revision 1.152  2001/03/29 02:44:32  paulk
;; Replaced jde-find-exec with executable-find, which is defined by executable.el available with both the Emacs and XEmacs distributions.
;;
;; Revision 1.151  2001/03/27 17:47:45  paulk
;; Eliminate dependency on which package by including the function jde-find-exec and replacing references to the which command with jde-find-exec. Thanks to klaus.berndl@sdm.de for suggesting this change and providing the implementation of jde-find-exec.
;;
;; Revision 1.150  2001/03/23 09:04:06  paulk
;; Now update the method in the mode line during idle times instead of after every keystroke. Thanks to Steven Monnier for suggesting this enhancement.
;;
;; Revision 1.149  2001/02/27 12:53:09  paulk
;; Now initializes jde-mode-abbrev-table to ensure compatability with cc-mode 5.28. Thanks to David Ponce for this fix.
;;
;; Revision 1.148  2001/02/25 04:14:55  paulk
;; - Update docstring for jde-global-classpath.
;; - Removed jde-path-to-string function because split-string does the same job.
;;
;; Revision 1.147  2001/02/21 05:53:13  paulk
;; Added require for new jde-package package.
;;
;; Revision 1.146  2001/02/20 05:09:04  paulk
;; The JDE now expands paths that begin with a tilde but not a period (.).
;;
;; Revision 1.145  2001/02/17 07:27:01  paulk
;; - Fixed regression bug in context-switching that resulted from setting
;;   jde-current-project to the path of the current prj.el file.
;;
;; - No longer expand classpaths to avoid expanding relative path (.) notation.
;;
;; Revision 1.144  2001/02/03 08:14:26  paulk
;; Changed declarations for all path variables from string to file. This means that you can now use completion (M tab) to complete paths in customization buffers.
;;
;; Revision 1.143  2001/02/03 07:32:29  paulk
;; Made quote argument optional in jde-build-path-arg and jde-build-classpath-arg.
;;
;; Revision 1.142  2001/01/16 04:25:30  paulk
;; Adds jde-abbrev-mode and jde-show-abbrev commands. Thanks to s.nicolas@videotron.ca.
;;
;; Revision 1.141  2000/12/25 09:32:27  paulk
;; * Adds jde-cygwin-path-converter variable.
;;
;; * Adds support for environment variable substitution in class and sourcepaths.
;;
;; Revision 1.140  2000/12/18 05:22:46  paulk
;; *** empty log message ***
;;
;; Revision 1.139  2000/12/10 06:52:58  paulk
;; Added jde-compile-jde.
;;
;; Revision 1.138  2000/11/27 06:18:41  paulk
;; Miscellaneous bug fixes and minor enhancements.
;;
;; Revision 1.137  2000/11/20 05:15:16  paulk
;; Added jde-import-organize command. Moved all import-related code from
;; jde-wiz.el to a new package named jde-import.el.
;;
;; Revision 1.136  2000/10/22 07:56:25  paulk
;; Add a Documentation submenu.
;;
;; Revision 1.135  2000/10/20 04:12:13  paulk
;; *** empty log message ***
;;
;; Revision 1.134  2000/10/12 02:31:55  paulk
;; *** empty log message ***
;;
;; Revision 1.133  2000/10/08 12:55:40  paulk
;; *** empty log message ***
;;
;; Revision 1.132  2000/09/05 04:59:57  paulk
;; Bug fixes.
;;
;; Revision 1.131  2000/08/19 06:48:36  paulk
;; Control flow abbreviations now optional.
;;
;; Revision 1.130  2000/08/11 05:02:52  paulk
;; Now refreshes speedbar at the end of a compilation.
;;
;; Revision 1.129  2000/07/29 03:18:41  paulk
;; Add support for line numbering via the setnu package.
;;
;; Revision 1.128  2000/07/28 06:27:47  paulk
;; Committing all modified files.
;;
;; Revision 1.127  2000/07/13 05:22:49  paulk
;; *** empty log message ***
;;
;; Revision 1.126  2000/06/12 08:20:19  paulk
;; Integrated David Ponce's jdok package.
;;
;; Revision 1.125  2000/05/10 05:38:41  paulk
;; The JDEbug menu now appears or disappears when you select or deselect JDEbug as the current debugger.
;;
;; Revision 1.124  2000/04/20 04:32:09  paulk
;; User can now supply customized imenu regular expressions. See `jde-imenu-regex-function'.
;;
;; Revision 1.123  2000/04/14 07:23:30  paulk
;; Added option jde-imenu-recognize-tag-comments-p. When on, this option causes the imenu symbol declaration indexer to recognize variables and method declarations witn prefixed tag comments.
;;
;; Revision 1.122  2000/03/16 05:08:25  paulk
;; Added JDEbug option to jde-db-debugger.
;;
;; Revision 1.121  2000/03/03 06:52:20  paulk
;; Moved Browse JDK doc item to Help menu. Other cosmetic changes to the
;; Help menu.
;;
;; Revision 1.120  2000/02/17 06:40:15  paulk
;; Fixed key bindings to show function keys on menu labels.
;;
;; Revision 1.119  2000/02/16 04:40:33  paulk
;; Implemented Cygwin/XEmacs compatiblity fixes provided by Fred Hart
;; <cfhart@Z-TEL.com> in bsh-internal.
;;
;; Revision 1.118  2000/02/16 03:11:50  paulk
;; *** empty log message ***
;;
;; Revision 1.117  2000/02/14 06:19:38  paulk
;; Implemented up and down stack commands.
;;
;; Revision 1.116  2000/01/29 02:25:15  paulk
;; You can now use the notation [f1], [f2], etc., to specify function
;; keys when customizing jde-key-bindings.
;;
;; Revision 1.115  2000/01/18 07:11:26  paulk
;; Added jde-show-class-source. Thanks to Phil Lord for the initial
;; implementation of this command.
;;
;; Revision 1.114  2000/01/02 08:07:55  paulk
;; Added attach process commands.
;;
;; .
;; .
;; .
;; Old entries deleted to save space.
;;
;; Revision 1.8  1997/06/18 17:20:00  paulk
;; Initial checkin.
;;

;; End of jde.el
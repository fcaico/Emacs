;;; jde-compile.el -- Integrated Development Environment for Java.
;; $Revision: 1.14 $ $Date: 2001/04/16 05:47:33 $ 

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

;;; Commentary:

;; This is one of a set of packages that make up the 
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; The latest version of the JDE is available at
;; <URL:http://sunsite.auc.dk/jde/>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

;;; Code:


(defgroup jde-compile-options nil
  "JDE Compiler Options"
  :group 'jde
  :prefix "jde-compile-option-")

(defcustom jde-compile-option-command-line-args ""
  "*Specify options as a string of command-line arguments.
The value of this variable should be a string of switches understood
by the compiler, for example, -depend -g. This variable is intended to
be used to set compile options not otherwise defined by the JDE, in
particular, options not defined by javac but used by another compiler
that you might want to use with the JDE."
  :group 'jde-compile-options
  :type 'string)

(defcustom jde-compile-option-classpath nil
"*Specify paths of classes required to compile this project.
The JDE uses the specified paths to construct a -classpath
argument to pass to the compiler. This option overrides the
`jde-global-classpath' option."
  :group 'jde-compile-options
  :type '(repeat (file :tag "Path")))

(defcustom jde-compile-option-sourcepath nil
"*Specify the source code path to search for class or interface definitions.

As with the user class path, source path entries  can be directories, JAR 
archives, or ZIP archives. If packages are used, the local path name within 
the directory or archive must reflect the package name. 

Note that classes found through the classpath are subject to automatic 
recompilation if their sources are found."
  :group 'jde-compile-options
  :type '(repeat (file :tag "Path")))

(defcustom jde-compile-option-directory ""
  "*Specifies the root directory of the class file hierarchy.
The compiler places compiled classes in the specified
directory. For example, specifying the class
directory as: 
  
  C:\\users\\dac\\classes

causes the class files for the classes in the MyProgram.java source
file to be saved in the directory C:\\users\\dac\\classes. If your class 
is in the package demos\\awt, the class files would be placed in directory
C:\\users\\dac\\classes\\demos\\awt."
  :group 'jde-compile-options
  :type 'directory)

(defcustom jde-compile-option-deprecation nil
  "*Warn use or override of a deprecated member or class. 
A member or class is deprecated if its documentation comment contains
the @deprecated tag. The compiler will emit a warning at the end of
compilation whether or not the deprecation option is on; this option
causes the location of each individual use or override to be noted.

Deprecated members or classes are deliberately not mentioned if the
source file containing the deprecation is being recompiled.  This can
happen because the file is on the command line or because the depend
option is on and the source file is out of date.
"
  :group 'jde-compile-options
  :type 'boolean)


(defcustom jde-compile-option-debug 
  (list "selected" (list t nil nil))
  "*Include debug information in classes.
The compiler includes line number information by default.

Before JDK 1.2, the the debug and optimize options were
mutually exclusive. In JDK 1.2, it is possible to combine debug and
optimize, but the shortcuts taken by optimized code may occasionally
produce surprising debugging results. For example, declared variables
may not exist and code may appear to move or not be executed at all."
  :group 'jde-compile-options
  :type '(list 
	  (radio-button-choice 
	   :format "%t \n%v"
	   :tag "Debug info to include in class:"
	   (const "all")
	   (const "none")
	   (const "selected"))
	  (list
	   :tag "    info"
	   :indent 4
	   (checkbox :format "%[%v%] %t \n"
		     :tag "Line Numbers")
	   (checkbox :format "%[%v%] %t \n"
		     :tag "Variables")
	   (checkbox :format "%[%v%] %t \n"
		     :tag "Source")))
	   
)


(defcustom jde-compile-option-optimize nil
"*Directs the compiler to try to generate faster code. 
This may slow down compilation, make larger class files, and/or make
it difficult to debug.

Prior to 1.2, the optimize option tried to inline methods across
classes. This created compatibility problems and sometimes generated
illegal bytecode. The optimize option also implicitly turned on the
depend option and implicitly turned off the debug option.

In JDK 1.2, the optimize option no longer inlines across classes and
so may safely be used for any java compilation. Optimize no longer
implicitly turns on depend or implicitly turns off debug."
  :group 'jde-compile-options
  :type 'boolean)


(defcustom jde-compile-option-depend nil
"*Analyze dependencies.
Causes recompilation of class files on which the source files given as
command line arguments recursively depend. Without this option, only
files that are directly depended on and missing or out-of-date will be
recompiled. Recompilation does not extend to missing or out-of-date
files only depended on by already up-to-date class files.

Note: if you are using a compiler other than post JDK 1.1.6 versions
of javac, you may need to specify the command-line switch used by
the compiler to specify dependency checking. See 
`jde-compile-option-depend-switch' for more information."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-option-depend-switch (list "-Xdepend")
"*Specify command line switch for depend option.
This option is necessary because the command-line switch for
dependency checking differs among Java compilers. Choose
from the following options:

  -Xdepend  Full dependency checking (post JDK 1.1.6)
  -depend   Full dependency checking (jikes and pre-JDK 1.1.6)
  +F        Check everything except jar and zip files (jikes only)
  +U        Check everything including jar and zip files (jikes only)"
  :group 'jde-compile-options
  :type '(list 
	  (radio-button-choice 
	   :format "%t \n%v"
	   :tag "Select -Xdepend (javac) or -depend (jikes):"
	   (const "-Xdepend")
	   (const "-depend")
	   (const "+F")
	   (const "+U"))))

(defcustom jde-compile-option-vm-args nil
"*Specify command-line arguments for Java interpreter.
Passes the specified arguments to the Java interpreter that runs the
compiler. The argument should not contain spaces. This is useful for
adjusting the compiler's execution environment or memory usage."
  :group 'jde-compile-options
  :type '(repeat (string :tag "Option")))

(defcustom jde-compile-option-verbose nil
"*Print verbose messages.
Causes the compiler and linker to print out messages about what source
files are being compiled and what class files are being loaded."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-option-nowarn nil
"*Turn off warnings.
If this option is specified, the compiler does not print out any
warnings."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-option-encoding nil
"*Specify the source file encoding name, such as EUCJIS\\SJIS.
If this option is not specified, then the platform default converter
is used."
  :group 'jde-compile-options
  :type 'boolean)

;;(makunbound 'jde-compile-option-target)
(defcustom jde-compile-option-target (list "1.1")
"*Generate class files that will work on VMs with the specified version.
 
The default is to generate class files to be compatible with both
1.1 and 1.2 VMs. The versions supported by javac in JDK1.2 are: 

  1.1     Ensure that generated class files will be compatible 
          with 1.1 and 1.2 VMs. This is the default.
  
  1.2     Generate class files that will run on 1.2 VMs, but 
          not on 1.1 VMs.

  1.3     Generate class files that will run on VMs in the 
          Java 2 SDK, v 1.3 and later, but will not run 
          on 1.1 or 1.2 VMs

By default, classes are compiled against the bootstrap and extension classes
of the JDK that javac shipped with. But javac also supports cross-compiling, 
where classes are compiled against a bootstrap and extension classes of a 
different Java platform implementation. It is important to use 
`jde-compile-option-bootclasspath' and `jde-compile-option-extdirs' when 
cross-compiling."
  :group 'jde-compile-options
  :type '(list
	  (radio-button-choice 
	   :format "%t \n%v"
	   :tag "Target VM:"
	   (const "1.1")
	   (const "1.2")
	   (const "1.3"))))

(defcustom jde-compile-option-bootclasspath nil
"*Cross-compile against the specified set of boot classes.
As with the user class path, boot class path entries can be 
directories, JAR archives, or ZIP archives."
  :group 'jde-compile-options
  :type '(repeat (file :tag "Path")))

(defcustom jde-compile-option-extdirs nil
"*Cross-compile against the specified extension directories. 
Each JAR archive in the specified directories is searched for class files."
  :group 'jde-compile-options
  :type '(repeat (file :tag "Path")))

;;(makunbound 'jde-compile-option-verbose-path)
(defcustom jde-compile-option-verbose-path nil
"*Describe how paths and standard extensions were searched to find
source and class files.

   ***NOTE***

   This option is supported only by the versions of javac shipped
   with JDK 1.1.x and 1.2.x and oldjavac in JDK 1.3."

  :group 'jde-compile-options
  :type 'boolean)


(defun jde-get-compile-options ()
"Constructs a command-line argument string for compiler.
The string consists of the contents of the jde-compile-options
variable concatenated with the various jde-compile-option
settings.
"
  (let (options)

    (if jde-compile-option-classpath
	(setq options 
	      (jde-build-classpath-arg
	       jde-compile-option-classpath
	       jde-quote-classpath
	       'jde-compile-option-classpath))
      (if jde-global-classpath
	  (setq options
		(jde-build-classpath-arg
		 jde-global-classpath
		 jde-quote-classpath
		 'jde-global-classpath))))

    (if jde-compile-option-sourcepath
	(setq options 
	      (concat options " "
	      (jde-build-path-arg
	       "-sourcepath"
	       'jde-compile-option-sourcepath 
	       jde-quote-classpath))))

    (if jde-compile-option-bootclasspath
	(setq options 
	      (concat options " "
	      (jde-build-path-arg
	       "-bootclasspath"
	       'jde-compile-option-bootclasspath 
	       jde-quote-classpath))))

    (if jde-compile-option-extdirs
	(setq options 
	      (concat options " "
	      (jde-build-path-arg
	       "-extdirs"
	       'jde-compile-option-extdirs
	       jde-quote-classpath))))

    ;; Debug option.
    (let* ((include-option (nth 0 jde-compile-option-debug))
	   (selected (nth 1 jde-compile-option-debug))
	   (lines (nth 0 selected))
	   (vars (nth 1 selected))
	   (src (nth 2 selected)))
      (cond
       ((and
	 (string= include-option "selected")
	 lines
	 (not vars)
	 (not src)))
       ((string= include-option "all")
	(setq options (concat options " -g")))
       ((string= include-option "none")
	(setq options (concat options " -g:none")))
       ((and
	 (string= include-option "selected")
	 (or lines vars src))
	(setq options 
	      (concat options 
		      " -g:"
		      (if lines
			  (if (or vars src) "lines,"
			    "lines"))
		      (if vars
			  (if vars
			      (if src "vars," "vars")))
		      (if src "source"))))))      

    (if (not (string= jde-compile-option-directory ""))
	(setq options
	      (concat options 
		" -d "
		(jde-normalize-path 'jde-compile-option-directory))))

    (if jde-compile-option-deprecation
	(setq options (concat options " -deprecation")))

    (if jde-compile-option-optimize
	(setq options (concat options " -O")))

    (if jde-compile-option-depend
	(setq options 
	      (concat options " " (car jde-compile-option-depend-switch))))

    (if jde-compile-option-vm-args
	(setq options
	      (concat 
	       options	      
	       (mapconcat
		(lambda (arg)
		  (concat " -J" arg))
		jde-compile-option-vm-args
		""))))

    (if jde-compile-option-verbose
	(setq options (concat options " -verbose")))

    (if jde-compile-option-verbose-path
	(setq options (concat options " -Xverbosepath")))

    (if jde-compile-option-nowarn
	(setq options (concat options " -nowarn")))

    (if (not (string= jde-compile-option-command-line-args ""))
	(setq options (concat options " " 
			      jde-compile-option-command-line-args)))

    (let ((target (car jde-compile-option-target)))
      (if (not (string= target "1.1"))
	  (setq options (concat options " -target " target))))
	
    options))

;;;###autoload
(defun jde-set-compile-options (options)
  "Sets the compile options.
Enter the options as you would on the command line, e.g.,
-depend -verbose."
  (interactive
   "sEnter options: ")
  (setq jde-compile-option-command-line-args options))


;;;###autoload
(defun jde-compile ()
  "Compile the Java program in the current buffer.
This command invokes the compiler specified by `jde-compiler'
with the options specified by the JDE customization variables
that begin with `jde-compile'. If the variable
`jde-read-compile-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled."
  (interactive)

  (if jde-read-compile-args
      (setq jde-interactive-compile-args
	      (read-from-minibuffer 
	       "Compile args: "
	       jde-interactive-compile-args
	       nil nil
	       '(jde-interactive-compile-arg-history . 1))))

  (let ((compile-command
	 (jde-make-compile-command 
	  jde-interactive-compile-args)))  

    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jde-compile from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (and (eq system-type 'windows-nt)
	     (not jde-xemacsp))	
	(let ((temp last-nonmenu-event))
	  ;; The next line makes emacs think that jde-compile
	  ;; was invoked from the minibuffer, even when it
	  ;; is actually invoked from the menu-bar.
	  (setq last-nonmenu-event t)
	  (save-some-buffers (not compilation-ask-about-save) nil)
	  (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))

    (setq compilation-finish-function 
      (lambda (buf msg) 
	(run-hook-with-args 'jde-compile-finish-hook buf msg)
	(setq compilation-finish-function nil)))

    (compile-internal compile-command "No more errors")))

(provide 'jde-compile)

;; Change History
;; $Log: jde-compile.el,v $
;; Revision 1.14  2001/04/16 05:47:33  paulk
;; Normalized paths. Thanks to Nick Sieger.
;;
;; Revision 1.13  2001/04/11 03:23:18  paulk
;; Updated to resolve relative paths relative to the project file that defines them. Thanks to Nick Sieger.
;;
;; Revision 1.12  2001/04/02 02:42:58  paulk
;; Remove extraneous definition of jde-build-classpath-arg.
;;
;; Revision 1.11  2001/03/13 04:03:47  paulk
;; Changed the type of jde-compile-option-directory from string to directory to permit path completion.
;;
;; Revision 1.10  2001/02/20 05:15:10  paulk
;; You can now use environment variables, tilde notation, and cygwin syntax in jde-compile-option-directory path.
;;
;; Revision 1.9  2001/02/17 17:43:34  paulk
;; Added support for JDK 1.3 targets.
;;
;; Revision 1.8  2001/02/03 08:18:44  paulk
;; Changed declarations of customized variables so that you can now use completion (M tab) to complete path names.
;;
;; Revision 1.7  2000/09/21 02:05:11  paulk
;; Fixes bug in formatting jde-compile-option-vm-args for the command line.
;;
;; Revision 1.6  2000/08/19 07:04:28  paulk
;; Adds compile finish hook.
;;
;; Revision 1.5  2000/08/11 05:04:45  paulk
;; Added jde-compile-finish-hook variable.
;;
;; Revision 1.4  2000/04/10 05:27:30  paulk
;; Compile command now supports Cygwin-style class paths.
;;
;; Revision 1.3  1999/01/15 22:04:15  paulk
;; Added Andy Piper's NT/XEmacs compatibility changes.
;;
;; Revision 1.2  1998/12/07 01:35:28  paulk
;; Updated compile options to reflect changes in command-line options
;; accepted by javac.
;;
;; Revision 1.1  1998/12/06 02:37:54  paulk
;; Initial revision
;;

;; End of jde-compile.el

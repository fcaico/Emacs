;; jde-ant.el --- Use Apache Ant to build your JDE projects

;;
;; Author: Jason Stell | jason.stell@globalone.net
;; Created: 19 Oct 2000
;; Version 1.3   19 June 2001
;;

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Commentary:
;; This file defines jde-ant-build and some helper functions.
;; jde-ant-build uses the specified ant program/shell script to 
;; execute a specified build file (in the project root). 
;; 
;;
;; Installation:
;; -- Make sure this file is accessible via your emacs load-path
;; -- Add the following to your .emacs
;;        (require 'jde-ant)
;; -- Customize the ant variables, which can be found in the
;;    jde-project group
;; -- Optionally create a keybinding for jde-ant-build
;;
;;
;; To do: 
;; -- allow interactive ant target/argument specification. 
;;    {DONE - version 1.1}
;;
;; -- consider using jde-run-java-vm to execute Ant instead of
;;    requiring a shell script.
;;
;;
;; Notes:
;; -- The JDE (Java Development Environment for Emacs) can be
;;    downloaded at http://sunsite.auc.dk/jde/
;;
;; -- Apache Ant is a Java & XML build system that can be downloaded 
;;    at http://jakarta.apache.org/ant/
;;
;; Version History:
;;
;; -- Version 1.3 (19 June 2001)
;;    : Addition of jde-ant-projecthelp to display list of targets for
;;      the current buildfile.
;; -- Version 1.2 (4 June 2001) 
;;    : Addition of jde-ant-read-buildfile option to prompt for the buildfile 
;;      name -- contributed by Rob Shaw <shaw@servidium.com>
;;    : Various Bug fixes contributed by Rob Shaw <shaw@servidium.com> 
;;        - The setting of the system property in a format other
;;          than -Dname=value had the side effect of negating the -emacs 
;;          command line argument.
;;        - The setting of the current directory to the location of the 
;;          JDE project file is now taking place when
;;          jde-ant-enable-find is nil.
;;        - The ant target is now the last thing to be appended to
;;          ant command to avoid any possible confusion for ANT
;;          as to what is the desired target.
;; -- Version 1.2b2 (25 May 2001) 
;;    : Fix to properly use the -find <buildfile> Ant switch--contributed
;;      by Rob Shaw <shaw@servidium.com>.
;; -- Version 1.2b1 (23 May 2001) 
;;    : Added jde-ant-enable-find custom flag to use the -find switch
;;      available in Ant. This overrides the requirement for a JDE
;;      project file
;;    : Fixed minor bug missing whitespace before -buildfile switch
;;      when building the ant compile command
;; -- Version 1.1 (20 October 2000)
;;    : Added interactive prompts (optional, based on customizable
;;      toggles) for Ant target and additional args. Removed the 
;;      jde-ant-target custom variable, since this is really
;;      represented by the default target in the build file.
;;    : The -f switch seems to be causing problems. Removed it from
;;      the default jde-ant-args.
;;    : Basic changes to the way the ant command is assembled.
;;
;; -- Version 1.0 (19 October 2000)
;;    Initial Version


(defcustom jde-ant-program "ant"
  "*Specifies name of ant program/script."
 :group 'jde-project
 :type 'string)

(defcustom jde-ant-args "-emacs"
  "*Specifies arguments to be passed to make program."
  :group 'jde-project
  :type 'string)

(defcustom jde-ant-buildfile "build.xml"
  "*Specifies the default buildfile to use."
  :group 'jde-project
  :type 'string)

(defcustom jde-ant-read-buildfile nil
"*Specify whether to prompt for a buildfile. If non-nil, the 
jde-ant-build command prompts you for an ant buildfile."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-ant-read-target nil
"*Specify whether to prompt for a build target. If non-nil, the 
jde-ant-build command prompts you for an ant target."
  :group 'jde-project
  :type 'boolean)

(defvar jde-ant-interactive-target ""
"Target to use for the build in place of the default target.")

(defvar jde-ant-interactive-target-arg-history nil
"History of targets entered in the minibuffer.")

(defcustom jde-ant-read-args nil
"*Specify whether to prompt for additional arguments to pass to ant. If non-nil, the 
jde-ant-build command prompts you for the additional arguments."
  :group 'jde-project
  :type 'boolean)

(defvar jde-ant-interactive-args ""
"Additional arguments for the ant command.")

(defvar jde-ant-interactive-args-arg-history nil
"History of targets entered in the minibuffer.")

(defcustom jde-ant-enable-find nil
"*Specify whether the -find option should be specified in Ant. If non-nil, Ant
will search up the directory hierarchy from the current directory for the build
definition file. Also note that, if non-nil, this will relax the requirement for
an explicit jde project file."
   :group 'jde-project
   :type 'boolean)


(defun jde-build-ant-command (target more-args)
  "Constructs the java ant command."
  (let (ant-command)
    (setq ant-command
	  (concat jde-ant-program " -Dbuffer=" buffer-file-name " "))
    
    (if (not jde-ant-enable-find)
	(if (not (string= jde-ant-buildfile ""))
	    (setq ant-command (concat ant-command " -buildfile " jde-ant-buildfile " "))))
    
    (if (not (string= jde-ant-args ""))
	(setq ant-command (concat ant-command jde-ant-args " ")))

    (if (not (string= more-args ""))
	(setq ant-command (concat ant-command more-args " ")))

    (if jde-ant-enable-find
	(setq ant-command (concat ant-command " -find " jde-ant-buildfile " ")))
    
    (if (not (string= target ""))
	(setq ant-command (concat ant-command target " ")))


    ant-command))

;;;###autoload
(defun jde-ant-build ()
  "Build the current project using Ant."
  (interactive)

  (if jde-ant-read-buildfile
      (setq jde-ant-buildfile
	      (read-from-minibuffer 
	       "Buildfile: "
	       jde-ant-buildfile
	       nil nil
	       '(jde-ant-buildfile-arg-history . 1))))

  (if jde-ant-read-target
      (setq jde-ant-interactive-target
	      (read-from-minibuffer 
	       "Target to build: "
	       jde-ant-interactive-target
	       nil nil
	       '(jde-ant-interactive-target-arg-history . 1))))

  (if jde-ant-read-args
      (setq jde-ant-interactive-args
	      (read-from-minibuffer 
	       "Additional build args: "
	       jde-ant-interactive-args
	       nil nil
	       '(jde-ant-interactive-args-arg-history . 1))))


  (let ((compile-command (jde-build-ant-command jde-ant-interactive-target jde-ant-interactive-args)))
    (when compile-command
      ;; Force save-some-buffers to use the minibuffer
      ;; to query user about whether to save modified buffers.
      ;; Otherwise, when user invokes the command from
      ;; menu, save-some-buffers tries to popup a menu
      ;; which seems not to be supported--at least on
      ;; the PC.
      (if (and (eq system-type 'windows-nt)
	       (not jde-xemacsp))
          (let ((temp last-nonmenu-event))
            ;; The next line makes emacs think that the command
            ;; was invoked from the minibuffer, even when it
            ;; is actually invoked from the menu-bar.
            (setq last-nonmenu-event t)
            (save-some-buffers (not compilation-ask-about-save) nil)
            (setq last-nonmenu-event temp))
        (save-some-buffers (not compilation-ask-about-save) nil))

      (message "%s" compile-command)
      (if (not jde-ant-enable-find)
	  (let ((default-directory (jde-find-project-file default-directory)))
	    (compile-internal compile-command "No more errors")))

      (if jde-ant-enable-find
	    (compile-internal compile-command "No more errors")))))


;;;###autoload
(defun jde-ant-projecthelp ()
  "Display Ant project help for the current project.
This will execute the Ant program with the `-projecthelp'
switch to output available targets with their descriptions
for the current buildfile. This function uses the same 
rules as `jde-ant-build' for finding the buildfile."
  (interactive)

  (if jde-ant-read-buildfile
      (setq jde-ant-buildfile
	      (read-from-minibuffer 
	       "Buildfile: "
	       jde-ant-buildfile
	       nil nil
	       '(jde-ant-buildfile-arg-history . 1))))

  (let ((projecthelp-command (jde-build-ant-command nil " -projecthelp")))
    (when projecthelp-command
      (message "%s" projecthelp-command)
      (if (not jde-ant-enable-find)
	  (let ((default-directory (jde-find-project-file default-directory)))
	    (shell-command projecthelp-command "*Ant Project Help*"))
	(shell-command projecthelp-command "*Ant Project Help*")))))



(provide 'jde-ant)

;; End of jde-ant.el

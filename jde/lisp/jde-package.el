;; jde-package.el -- Java package statement generator
;; $Id: jde-package.el,v 1.6 2001/06/07 03:31:00 paulk Exp $

;; Copyright (C) 1998, 2000, 2001 by David Ponce

;; Author:       David Ponce <david@dponce.com>
;; Maintainer:   David Ponce <david@dponce.com>
;;               Paul Kinnucan <paulk@mediaone.net>
;; Created:      September 28 1998

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

;;; Commentary:
;;
;; This package automatically generates a Java package statement.  The
;; package name is deducted from the current classpath setting of
;; JDE.  When a directory found in classpath is a root of the current
;; buffer default directory, the relative path of the default
;; directory from the classpath one becomes the package name by
;; substituting directory separators by '.'.
;;
;; For example:
;;
;;   The JDE classpath setting is:
;;
;;    ("~/java" "/jdk1.1.6/lib/classes.zip")
;;
;;   For the file "~/java/FR/test/MyClass.java", the package name
;;   generated will be "FR.test".
;;
;;   The JDE classpath setting is:
;;
;;    ("~/java" "~/java/test" "/jdk1.1.6/lib/classes.zip")
;;
;;   For the file "~/java/test/MyClass.java",
;;   `jde-package-default-package-comment' will be generated because
;;   the default package can be used.
;;
;;   The JDE classpath setting is:
;;
;;    ("~/java" "/jdk1.1.6/lib/classes.zip")
;;
;;   For the file "/usr/java/MyClass.java", no package name will be
;;   generated because the directory "/usr/java" is not accessible
;;   from classpath.
;;
;; Usage:
;;
;; M-x `jde-package-update' to update the Java package statement or
;; insert a new one at beginning of current buffer.
;;
;; The function `jde-package-generate-package-statement' should be
;; used in Java template to automatically generate the package
;; statement.
;;
;; Customization:
;;
;; The following variables could be set:
;;
;; - `jde-package-load-hook' hook run when package has been loaded.
;;
;; - `jde-package-package-comment' Java line comment appended to the
;;   generated package statement.  Default is " // Generated package
;;   name"
;;
;; - `jde-package-default-package-comment' Java line comment generated
;;   when the default package is used.  Default is "// Default package
;;   used"
;;
;; - `jde-package-search-classpath-variables' list of variables where
;;   to search the current classpath list.  Default is
;;   '(`jde-compile-option-classpath' `jde-global-classpath')

;;; History:
;;
;; See at end of this file.

;;; Code:
(defconst jde-package-unknown-package-name
  "*unknown*"
  "The string returned when a package name can't be generated.")

(defconst jde-package-package-regexp
  "package .*;.*$"
  "The regexp used to find the Java package statement.")

(defgroup jde-package nil
  "jde-package package customization"
  :group 'jde
  :prefix "jde-package-")

(defcustom jde-package-load-hook nil
  "*Hook run when package has been loaded."
  :group 'jde-package
  :type 'hook)

(defcustom jde-package-package-comment
  " // Generated package name"
  "*Java line comment appended to the generated package statement.
An empty string suppress the generation of this comment."
  :group 'jde-package
  :type 'string)

(defcustom jde-package-default-package-comment
  "// Default package used"
  "*Java line comment generated when the default package is used.
An empty string suppress the generation of this comment."
  :group 'jde-package
  :type 'string)

(defcustom jde-package-search-classpath-variables
  '(jde-compile-option-classpath jde-global-classpath)
  "*Specify the variables where to search the current classpath list.
The first one which has a non nil value will be used by jde-package."
  :group 'jde-package
  :type '(repeat variable))

(defalias 'jde-package-fullpath 'jde-normalize-path)

(defun jde-package-get-classpath ()
  "Return the current classpath list.
That is to say the first non-nil value found in the variables given by
`jde-package-search-classpath-variables'."
  (let ((search-in jde-package-search-classpath-variables)
        (classpath))
    (while (and search-in (not classpath))
      (setq classpath (symbol-value (car search-in))
            search-in (cdr search-in)))
    classpath))

(defun jde-package-get-directories-in-classpath ()
  "Return the list of directories found in classpath."
  (mapcan
   #'(lambda (path)
       (if (not (string= path ".")) ; "." is ignored in classpath
           (let ((path (jde-package-fullpath path)))
             (if (file-directory-p path)
                 (list (file-name-as-directory path))))))
   (jde-package-get-classpath)))

(defun jde-package-get-current-directory ()
  "Return the full path of the current directory."
  (jde-package-fullpath default-directory))

(defun jde-package-seach-package-directories ()
  "Return a list of package directory candidates or nil if not found."
  (let ((dir (jde-package-get-current-directory))
        ;; case-insensitive for Windows
        (case-fold-search (eq system-type 'windows-nt)))
    (mapcan
     #'(lambda (root)
         (let ((root (regexp-quote root)))
           (message "Seaching %S in %S..." root dir)
           (and (string-match root dir)
                (list (substring dir (match-end 0))))))
     (jde-package-get-directories-in-classpath))))

(defun jde-package-get-package-directory ()
  "Return the package directory found.
Or `jde-package-unknown-package-name' if not found."
  (or (jde-package-best-package-candidate
       (jde-package-seach-package-directories))
      jde-package-unknown-package-name))

(defun jde-package-best-package-candidate (candidates)
  "Return the best package directory candidate from CANDIDATES.
The best is the shortest one that could be found."
  (car (sort candidates
             #'(lambda (dir1 dir2)
                 (string-match (regexp-quote dir1) dir2)))))

(defun jde-package-convert-directory-to-package (dir)
  "Convert directory path DIR to a valid Java package name.
Replace ?/ by ?. and remove extra ?/ at end."
  (if (string= dir "")
      ""
    (subst-char-in-string
     ?/ ?.
     (substring (file-name-as-directory dir) 0 -1)
     t)))

;;;###autoload
(defun jde-package-generate-package-statement ()
  "Generate a Java package statement.
If the package name cant be generated this function returns an empty
string."
  (let* (
         ;; The JDE always uses ?/ as directory separator so ensure
         ;; [X]Emacs uses the same one when running on Windows!
         (directory-sep-char ?/)
         (package-name (jde-package-get-package-directory)))
    (cond
     ((string= package-name jde-package-unknown-package-name)
      (message
       "The current directory is not accessible from classpath.")
      "")
     ((string= package-name "")
      (message "Default package used.")
      jde-package-default-package-comment)
     (t
      (message "package %s;%s"
               (jde-package-convert-directory-to-package package-name)
               jde-package-package-comment)))))

;;;###autoload
(defun jde-package-update ()
  "Update or replace the Java package statement in the current buffer.
If the package statement does not exist a new one is inserted at the
top of the buffer.  If the package name cant be generated nothing is
done.  The major mode of current buffer must be `jde-mode'."
  (interactive)
  (or (eq major-mode 'jde-mode)
      (error "Invalid major mode found.  Must be `jde-mode'"))
  (let ((package (jde-package-generate-package-statement)))
    (unless (string= package "")
      (goto-char (point-min))
      (if (re-search-forward jde-package-package-regexp nil t)
          (replace-match package)
        (insert package)
        (newline)))))

(provide 'jde-package)
(run-hooks 'jde-package-load-hook)

;;; Change History:

;; $Log: jde-package.el,v $
;; Revision 1.6  2001/06/07 03:31:00  paulk
;; Fixed NT/XEmacs incompatibility caused by XEmacs' use of back slash as the directory path separator. Thanks to David Ponce.
;;
;; Revision 1.5  2001/04/09 05:01:03  paulk
;; Fixed bug in jde-package-get-directories-in-classpath that caused it to return t instead of a list when the classpath contained ".".
;;
;; Revision 1.4  2001/03/30 07:10:31  paulk
;; Minor code improvement and cleanup and many checkdoc related
;; cosmetic changes contributed by David Ponce.
;;
;; Revision 1.3  2001/03/13 04:23:58  paulk
;; Cosmetic changes.
;;
;; Revision 1.2  2001/02/22 03:36:30  paulk
;; Removed require for jde to fix infinite recursion loading bug.
;;
;; Revision 1.1  2001/02/21 05:52:02  paulk
;; Initial revision.
;;

;;
;; $Log: jde-package.el,v $
;; Revision 1.6  2001/06/07 03:31:00  paulk
;; Fixed NT/XEmacs incompatibility caused by XEmacs' use of back slash as the directory path separator. Thanks to David Ponce.
;;
;; Revision 1.5  2001/04/09 05:01:03  paulk
;; Fixed bug in jde-package-get-directories-in-classpath that caused it to return t instead of a list when the classpath contained ".".
;;
;; Revision 1.4  2001/03/30 07:10:31  paulk
;; Minor code improvement and cleanup and many checkdoc related
;; cosmetic changes contributed by David Ponce.
;;
;; Revision 1.3  2001/03/13 04:23:58  paulk
;; Cosmetic changes.
;;
;; Revision 1.2  2001/02/22 03:36:30  paulk
;; Removed require for jde to fix infinite recursion loading bug.
;;
;; Revision 1.1  2001/02/21 05:52:02  paulk
;; Initial revision.
;;

;;; jde-package.el ends here.

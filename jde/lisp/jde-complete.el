;; jde-complete.el -- Smart completion for the JDE
;;
;; $Revision: 1.34 $
;;
;; Author: Rodrigo Reyes <reyes@chez.com>
;; Maintainers: Rodrigo Reyes
;;              Paul Kinnucan <pkinnucan@mediaone.net>
;;              David Ponce
;;              Howard Spector 
;;              Stephane Nicolas <s.nicolas@videotron.ca>,
;;              Javier Lopez <jlopez@cellexchange.com> 

;; Keywords: java, intellisense, completion

;; Copyright (C) 1999, 2000, 2001 Rodrigo Reyes, Paul Kinnucan, David Ponce,
;;                          Stephane Nicolas, David Ponce, Javier Lopez

;; This package follows the GNU General Public Licence (GPL), see the
;; COPYING file that comes along with GNU Emacs. This is free software,
;; you can redistribute it and/or modify it under the GNU GPL terms.
;;
;; Java is a registered trademark of Sun Microsystem, Inc.
;;
;;; Commentary:

;; This is one of a set of packages that make up the
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;;
;; This package adds smart completion to the JDE. How it works is
;; simple : put the cursor at the end of a statement "under
;; construction", eg. "myVariable.rem<CURSOR HERE> and call the
;; jde-complete-at-point emacs-lisp function (this is by default
;; C-.). A completion is then inserted. If multiple completions are
;; possible, calling the completion function again will cycle through
;; all the possibilities (as dabbrev-mode does).

;; To retrieve all the possible completions, it uses the java code in
;; jde.util.Completion.getClassInfo(), called by beanshell. That
;; need the class to be compiled (but that's not worst than an etag
;; call).

;; Known bugs/problems :

;; - Due to the way the JVM works, it is not possible to explicitly
;; unload a class. So, if major changes are done in a class, the
;; beanshell must be restarted in order to reload the class.

;; The latest version of the JDE is available at
;; <URL:http://jde.sunsite.dk>.
;; <URL:http://www.geocities.com/SiliconValley/Lakes/1506/>

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at pkinnucan@mediaone.net

;;jde-eldoc for completion signatures
(eval-when-compile (require 'eldoc)
                   (require 'senator))

(defvar jde-complete-temp-process "*Temp*"
  "Used as the name of a temporary process")

(defvar jde-complete-last-compiled-class nil
  "Contains the name of the class that was compiled last")

(defvar jde-complete-current-signature nil
  "Contains the signature of the last method inserted by 
either `jde-complete-at-point' or `jde-complete-at-point-menu'")

(defvar jde-complete-display-signature nil
  "If non nil it displays the `jde-complete-current-signaure' in
the minibuffer")

(defcustom jde-complete-signature-display-time 60
  "Amount of time is seconds to display the method signature
in the minibuffer after a completion")

(defcustom jde-complete-add-space-after-method nil
  "*If non nil it will add a space between the method
name and the first parenthesis. i.e foo () instead of foo() when using
the completion methods `jde-complete-at-point' and `jde-complete-at-point-menu'
After customizing this variable, be sure to use `jde-complete-flush-classinfo-cache',
any class information that was previously cache is not going to be affected by
this setting"
  :group 'jde-project
  :type 'boolean)

(defcustom jde-complete-unique-method-names nil
  "If non nil it will display methods with the same name
 but with different signature only once, the signature for ths methods
 will be the signature of the first method in the list of completions
The list of completion is sorted in alphabetical order.
This variable modifies the completion for `jde-complete-at-point-menu'
and `jde-complete-at-point'. After customizing this variable,
be sure to use `jde-complete-flush-classinfo-cache',
any class information that was previously cache is not going to be affected by
this setting"
  :group 'jde-project
  :type 'boolean)

(defcustom jde-complete-insert-method-signature t
  "If non nil it will insert the method signature when using
`jde-complete-at-point' and `jde-complete-at-point-menu'
i.e. v.get(int, java.lang.String). If
`jde-complete-unique-method-names' is non nil methods with the same
name will get the signature of the first one in the completion list.
After customizing this variable, be sure to use `jde-complete-flush-classinfo-cache',
any class information that was previously cache is not going to be affected by
this setting"
  :group 'jde-project
  :type 'boolean)

(defvar jde-complete-current-list nil
  "The list of all the completion. Each element of the list is a list
which car is the possible completion, and the cdr is an additional
information about this completion.")

(defvar jde-complete-current-list-index nil
  "An index to an element in jde-complete-current-list. This is used to
cycle the list.")

(defvar jde-complete-current-beginning (make-marker)
  "The beginning of the region where the last completion was inserted.")

(defvar jde-complete-current-end (make-marker)
  "The end of the region where the last completion was inserted.")

;; Modified `jde-complete-import-list' to use semantic parser table
(defun jde-split-import-token (token)
  "Helper function used by `jde-complete-import-list' which return a
list (PACKAGE-DOT CLASS-OR-STAR) from given semantic 'include (that
is Java import) TOKEN.
For example:
  : (jde-split-import-token \"java.util.Hashtable\")
  > (\"java.util.\" . \"Hashtable\")
  : (jde-split-import-token \"java.lang.*\")
  > (\"java.lang.\" . \"*\")
  : (jde-split-import-token \"test\")
  > (\"test.\" . \"*\")"
  (let* ((import      (semantic-token-name token))
         (match-point (string-match "\\." import))
	 split-point)
    (while match-point
      (setq split-point (1+ match-point)
            match-point (string-match "\\." import split-point)))
    (if split-point
        (list (substring import 0 split-point)
              (substring import split-point))
      (list (concat import ".")
            "*"))))

(defun jde-complete-import-list ()
  "Return the list of Java packages declared in the current buffer.
It uses the semantic parser table to find the 'package' and 'import'
statements. It implicitly adds the java.lang.* package. See also
`jde-split-import-token'."
  (let* ((tokens   (semantic-bovinate-toplevel t))
         (packages (semantic-find-nonterminal-by-token 'package tokens))
         (imports  (semantic-find-nonterminal-by-token 'include tokens))
         lst)
    (setq lst (append
               (mapcar (function
                        (lambda (token)
                          (list
                           (concat (semantic-token-name token) ".")
                           "*")))
                       packages)
               (mapcar 'jde-split-import-token
                       imports)))
    (or (member "java.lang.*" lst)
        (setq lst (append lst '(("java.lang." "*")))))
    lst))

(defun jde-complete-valid-java-declaration-at (point varname)
  "Verify that a POINT starts a valid java declaration
for the VARNAME variable."
  (save-excursion
    (goto-char point)
    (if (looking-at 
	 (concat "\\([A-Za-z0-9_.\177-\377]+\\)[ \t\n\r]+" 
		 (jde-complete-double-backquotes varname) 
		 "[ \t\n\r]*[;=]"))
	(match-string 1)
      nil)))
  
(defun jde-complete-double-backquotes (varname)
  "Build a new string identical to VARNAME, except that every backquote
`\' is doubled, so that it can be used in a regex expression"
  (let (result (idx 0) (len (length varname)) curcar)
    (while (< idx len)
      (setq curcar (elt varname idx))
      (setq result (concat result (if (eq curcar ?\\)
				      "\\\\"
				    (make-string 1 curcar))))
      (setq idx (1+ idx)))
    result))

(defun jde-complete-declared-type-of (name)
  "Find in the current buffer the java type of the variable NAME.  The
function returns a string containing the name of the class, or nil
otherwise. This function does not give the fully-qualified java class
name, it just returns the type as it is declared."
  (save-excursion
    (let (found res pos orgpt resname)
      (while (and (not found)
		  (search-backward name nil t))
	(setq pos (point))
	(backward-word 1)
	(setq resname (jde-complete-valid-java-declaration-at (point) name))
	(goto-char pos)
	(forward-char -1)
	(if resname
	    (progn (setq res resname)
		   (setq found t))))
      res)))

(defun jde-complete-filter-fqn (importlist)
  "Filter all the fully-qualified classnames in the import list. It uses
the knowledge that those classnames are at the beginning of the list,
so that it can stops at the first package import (with a star `*' at
the end of the declaration)."
  (if importlist
      (if (string= "*" (car (cdr (car importlist))))
	  importlist
	(jde-complete-filter-fqn (cdr importlist)))))

(defun jde-complete-guess-type-of (name)
  "Guess the fully qualified name of the class NAME, using the import
list. It returns a string if the fqn was found, or a list of possible
packages otherwise."
  (let ((importlist (jde-complete-import-list)) shortname fullname tmp result)
    (while (and importlist (null result))
      (setq tmp (car importlist))
      (setq shortname (car (cdr tmp)))
      (setq fullname (concat (car tmp) name))
      (cond 
       ((string= "*" shortname)
	(setq result importlist))
       ((string= name shortname)
	(setq result fullname))
       (t 
	(setq importlist (cdr importlist)))))
    result))

;; Contributed by Charles Hart <cfhart@Z-TEL.com>
;; Returns t if the fully qualified class name can be found in the
;; classpath, nil otherwise
(defun jde-complete-class-exists (name)
  ;;replacing double quotes by empty strings double quotes seems to break the beanshell
  (while (string-match "\"" name)
    (setq name (replace-match "" nil nil name))) 
  
  ;;replacing back slaches by empty strings backlashes causes beanshell problem
  (while (string-match "\\\\" name)
    (setq name (replace-match "" nil nil name))) 
  (jde-jeval-r (concat "jde.util.JdeUtilities.classExists(\"" name "\");")))

;; Contributed by Charles Hart <cfhart@Z-TEL.com>
;; Get the fully qualified name of the class NAME, using the import
;; list. It returns a string if the fqn was found, or null otherwise.
;; This is more capable than jde-complete-guess-type-of because it
;; uses the beanshell to determine if an import statement with a
;; wildcard contains the unqualified class name passed to this
;; function.
(defun jde-complete-get-qualified-name (name)
  "Guess the fully qualified name of the class NAME, using the import
list. It returns a string if the fqn was found, or null otherwise."
  (if (jde-complete-class-exists name)
      name
    (let ((importlist (jde-complete-import-list)) shortname fullname tmp result)
      (while (and importlist (null result))
	(setq tmp (car importlist))
	(setq shortname (car (cdr tmp)))
	(setq fullname (concat (car tmp) name))
	(cond 
	 ((and (string= "*" shortname) (jde-complete-class-exists fullname))
	  (setq result fullname))
	 ((string= name shortname)
	  (setq result fullname))
	 (t 
	  (setq importlist (cdr importlist)))))
      result)))

(defun jde-complete-get-name-of-this-class ()
  "Gets the fully qualified name of this class name of this"
  (let* (answer
         (tokens (semantic-bovinate-toplevel t))
         (pos (string-match ".java" (buffer-name)))
         (package (car (semantic-find-nonterminal-by-token 'package
							   tokens)))
         (package-name (semantic-token-name package)))
    (if pos
        (setq answer (concat (if package-name (concat package-name "."))
                             (substring (buffer-name) 0 pos)))
      (error "Wrong type of file %s" (buffer-name)))
    answer))

(defvar jde-complete-classinfo-cache nil)

(defcustom jde-complete-classinfo-cache-size 50
  "The max size of completion's cache.")

(defun jde-complete-flush-classinfo-cache ()
  "Flushes all entries in the completion cache"
  (interactive)
  (setq jde-complete-classinfo-cache nil))

(defun jde-complete-flush-classes-in-cache (class-list)
  "Flushes all the classes in CLASS-LIST as entries of cache."
  (let ((temp (nth 0 jde-complete-classinfo-cache))
	(index -1) 
	(found nil)
	(class (car class-list)))
    (while class
      (while (and temp (not found))
	(setq index (1+ index))
	(setq temp (nth index jde-complete-classinfo-cache))
	(if (string= (car temp) class)
	    (setq found t)))
      (if found 
	  (setq jde-complete-classinfo-cache
		(nthcdr (1+ index) jde-complete-classinfo-cache)))
      (setq class-list (cdr class-list))
      (setq class (car class-list))
      (setq found nil))))

(defun jde-complete-add-to-classinfo-cache (name classinfo)
  (let (new-entry new-list)
    (if (nth jde-complete-classinfo-cache-size jde-complete-classinfo-cache)
        (progn
            (setq new-entry (list name classinfo))
            (setq new-list (list new-entry nil))
            (setcdr new-list (cdr jde-complete-classinfo-cache))
            (setq jde-complete-classinfo-cache new-list)  
            (message "cache is full"))
        ;;else
        (setq jde-complete-classinfo-cache 
              (append 
               jde-complete-classinfo-cache 
               (list (list name classinfo)))))))

(defun jde-complete-get-from-cache (name)
  (let ((temp (nth 0 jde-complete-classinfo-cache)) (index -1) (found nil))
    (while (and temp (not found))
      (setq index (1+ index))
      (setq temp (nth index jde-complete-classinfo-cache))
      (if (string= (car temp) name)
	  (setq found t)))
    (if found
	(nth 1 temp)
      nil)))

(defun jde-complete-get-classinfo (name)
  "Return the class info list for the class NAME. This function first
checks to see if the class info is cached. If so, it returns the
cached class info. Otherwise, it creates the class info list. Each
element of the list returned by this function is itself a list whose
car is a possible completion and whose cdr gives additional
informations on the completion."
  ;;replacing double quotes by empty strings double quotes seems to break the beanshell
  (while (string-match "\"" name)
    (setq name (replace-match "" nil nil name))) 
  
  ;;replacing back slaches by empty strings backlashes causes beanshell problem
  (while (string-match "\\\\" name)
    (setq name (replace-match "" nil nil name))) 
  
  (let ((class-info (jde-complete-get-from-cache name)))
    (when (not class-info)
      (setq class-info 
	    (jde-jeval-r (concat "jde.util.Completion.getClassInfo(\"" name "\");")))
      (setq class-info (sort (jde-complete-build-completion-list class-info) 
                             'jde-complete-sort-comparison))
      (if class-info
          (jde-complete-add-to-classinfo-cache name class-info)))
    class-info))
 
(defun jde-complete-get-classinfo-with-access-level (name access-level)
  "Return the class info list for the class NAME and the ACCESS-LEVEL.
Allowed values for access level are 0 for protected 1 for private. This function first
checks to see if the class info is cached. If so, it returns the
cached class info. Otherwise, it creates the class info list. Each
element of the list returned by this function is itself a list whose
car is a possible completion and whose cdr gives additional
informations on the completion."
  ;;replacing double quotes by empty strings double quotes seems to break the beanshell
  (while (string-match "\"" name)
    (setq name (replace-match "" nil nil name))) 
  
  ;;replacing back slaches by empty strings backlashes causes beanshell problem
   (while (string-match "\\\\" name)
     (setq name (replace-match "" nil nil name))) 
 
  (let ((class-info (jde-complete-get-from-cache name)))
    (when (not class-info)
      (setq class-info 
	    (bsh-eval-r (concat "jde.util.Completion.getClassInfo(\"" name "\"," 
                                (number-to-string access-level) ");")))
      (setq class-info (sort (jde-complete-build-completion-list class-info) 
                             'jde-complete-sort-comparison))
      (if class-info
	  (jde-complete-add-to-classinfo-cache name class-info)))
    class-info))

(defun jde-complete-get-classinfo-javacode (name import access-level)
  "Return the java code that calls the
jde.util.Completion.getClassInfo function with the short java class
name NAME and the package list IMPORT where to look at."
  (save-excursion
    (concat 
     "{ " 
     "String[] lst = new String[" (number-to-string (length import)) "];\n"
     (let ((count -1))
       (mapconcat 
	(function 
	 (lambda (x) 
	   (setq count (+ 1 count))
	   (concat "lst[" (int-to-string count) "]=\"" 
		   (car (nth count import)) "\";\n")))
	import
	" "))
     "jde.util.Completion.getClassInfo(\"" name "\",lst," (number-to-string access-level) ");\n"
     "}")))

(defun jde-complete-isolate-to-complete (s)
  "Returns the right expression that needs completion in S." 
  (let* ((index (length s)) stop (paren 0) curcar final-string (inside-quotes nil))
    (while (and (> index 0)
		(not stop))     
      (setq index (- index 1))
      (setq curcar (aref s index))
      (cond
       ((eq ?\" curcar) ;;Checking if we are inside double quotes
        (if (not (eq ?\\ (aref s (- index 1)))) ;;if the quote is not escape
            (setq inside-quotes (not inside-quotes))))
       ((eq ?\) curcar)
        (if (not inside-quotes)
            (setq paren (1+ paren))))
       ((eq ?\( curcar)
        (if (not inside-quotes)
            (setq paren (1- paren)))))
      (if (or (< paren 0)
	      (and (eq curcar ?\,) (<= paren 0)))
	  (setq stop t)))
    (if stop
	(setq index (1+ index)))
    (setq final-string (substring s index))
    (if (and (not (string= final-string ""))
             (string= "(" (substring final-string 0 1)))
        (let* ((closing-paren (string-match ")" final-string))
               (closing (string-match ")." final-string (+ 1 closing-paren))))
          (setq final-string
                (concat (substring final-string 2 closing-paren)
                        "."
                        (substring final-string (+ closing 2))))))
    final-string))

;; Can this function be reimplemented to use regular expressions?
(defun jde-complete-isolate-before-matching-of-last-car (s)
  "Returns the right expression that needs completion in S." 
  (let* (final-string (index (length s)) stop (paren 0) (bracket 0) curcar (inside-quotes nil))
    (while (and (> index 0)
		(not stop))     
      (setq index (- index 1))
      (setq curcar (aref s index))
      (cond
       ((eq ?\" curcar) ;;Checking if we are inside double quotes
        (if (not (eq ?\\ (aref s (- index 1)))) ;;if the quote is not escape
            (setq inside-quotes (not inside-quotes))))
       ((eq ?\) curcar)
        (if (not inside-quotes)
            (setq paren (1+ paren))))
       ((eq ?\( curcar)
        (if (not inside-quotes)
            (setq paren (1- paren))))
       ((eq ?\] curcar)
        (if (not inside-quotes)
            (setq bracket (1+ bracket))))
       ((eq ?\[ curcar)
        (if (not inside-quotes)
	(setq bracket (1- bracket)))))
      (if (and (= paren 0)
	       (= bracket 0)) 
	  (setq stop t)))
    (setq final-string (substring s 0 index))
    (if (and (string= "" final-string)
             (string= "((" (substring s 0 2)))
        (let ((closing-paren (string-match ")" s)))
          (setq final-string (substring s 2 closing-paren))))
    final-string))

(defun jde-complete-match-paren-position ()
  "Returns the position of the parenthesis match"
  (let ((current (point))
        match)
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (backward-list 1)))
    (setq match (point))
    (goto-char current)
    match))

(defun jde-complete-convert-args-to-types (args) 
  "Converts something like this (10, 10) to (int, int)"
  (let* ((answer "(")
         (first-time t)
         (temp (substring args 1 (- (length args) 1))) ;;Striping the parens
         (lst (split-string temp ", ?"))
         tmp)
    (while lst
      (setq tmp (car lst))
      (if (not first-time)
          (setq answer (concat answer ", "))
        (setq first-time nil))
      (setq answer (concat answer 
                           (jde-complete-eval-type-of tmp)))
      (setq lst (cdr lst)))
    (setq answer (concat answer ")"))
    answer))

;; Can this function be reimplemented to use regular expressions?
(defun jde-complete-java-variable-at-point ()
  "Returns a list (VAR PARTIAL) where VAR.PARTIAL is the partially completed method or field
name at point. For example, suppose obj.f1.ge were the name at point. This function would return
the list (obj.f1 ge)."
  (save-excursion
    (let (start middle-point varname curcar dot (dot-offset 0) found (original-point (point)) 
	  intermediate-point beg-point first-part second-part first-paren cast-type second-paren
          args (offset 0) (bracket-count 0) (paren-count 0))

      ;; Move cursor to the beginning of the partially
      ;; completed part of the expression, e.g., move point
      ;; from
      ;;
      ;;   obj.f1.ge   to obj.f1.ge
      ;;            ^           ^
      ;;   obj.f1.get(int,int)  to obj.f1.get(int,int)
      ;;                      ^              ^
      (setq curcar (char-before))
      (while (null found)
	(cond 
	 ((or (and (>= curcar ?a) (<= curcar ?z)) ; a-z
	      (and (>= curcar ?A) (<= curcar ?Z)) ; A-z
	      (and (>= curcar ?0) (<= curcar ?9))
	      (>= curcar 127)
	      (member curcar '(?_ ?\\ ))) ;; _ \
	  (forward-char -1))
	 ((eq ?. curcar)
          (setq dot-offset 1)
          (if (eq ?\) (char-before (- (point) 1)))
              (progn
                (forward-char -2)
                (setq first-paren (point))
                (setq second-paren (jde-complete-match-paren-position))
                (setq offset (+ (- first-paren second-paren) 1))
                (forward-char 2)
                (setq found (point)))
            (setq found (point))))
	 (t
          (setq found (point))))
	(setq curcar (char-before)))
      (setq intermediate-point found)
      ;; Now move point to the beginning of the expression, e.g.,
      ;; from
      ;;
      ;;  obj.f1.ge
      ;;        ^
      ;; to
      ;;
      ;;  obj.f1.ge
      ;; ^
      ;;
      (progn 
        (setq curcar (char-before))
        (while (or (and (>= curcar ?a) (<= curcar ?z))
                   (and (>= curcar ?A) (<= curcar ?Z))
                   (and (>= curcar ?0) (<= curcar ?9))
                   (>= curcar 127)
                   (and (eq curcar ? ) (or (< 0 paren-count) (< 0 bracket-count)))
                   (member curcar '(?\" ?\. ?\_ ?\\ ?\( ?\) ?\, ?\[ ?\])))
          (cond 
           ((eq curcar ?\))
            (progn
              (forward-char -1)
              (goto-char (jde-complete-match-paren-position))))
           ((eq curcar ?\( )
            (setq paren-count (1- paren-count)))
           ((eq curcar ?\] )
            (setq paren-count (1+ bracket-count)))
           ((eq curcar ?\[ )
            (setq paren-count (1- bracket-count))))
          (forward-char -1)
          (setq curcar (char-before)))
        
        (setq beg-point (point))
        (set-marker jde-complete-current-beginning intermediate-point)
        (set-marker jde-complete-current-end original-point)
        (setq middle-point (- intermediate-point dot-offset offset))
        (setq first-part (buffer-substring-no-properties beg-point middle-point))
        (setq first-part (jde-complete-isolate-to-complete first-part))
        
        ;;replacing newline by empty strings  new lines seems to break the beanshell
        (while (string-match "\n" first-part)
          (setq first-part (replace-match "" nil nil first-part))) 
        
        ;;replacing extra spaces for "". This done to reduce the space
        ;;that the completion title takes
        (while (string-match " " first-part)
          (setq first-part (replace-match "" nil nil first-part))) 
        
        (setq second-part (buffer-substring-no-properties intermediate-point original-point))

        ;;Checking for casting
        ;; ((Object) obj).ge
        (if (and (not cast-type)
                 (string= first-part "")
                 (eq (char-before (+ 1 middle-point)) ?\()
                 (eq (char-before (+ 2 middle-point)) ?\())
            (save-excursion 
              (goto-char (+ middle-point 1))
              (setq first-paren (point))
              (setq second-paren (jde-complete-match-paren-position))
              (setq cast-type (buffer-substring-no-properties (+ 1 first-paren) second-paren))))
        
        (if cast-type
            (progn
              (setq jde-complete-casting t)
              (list cast-type second-part))
          (progn
            (setq jde-complete-casting nil)
            (list first-part second-part))))
      )))

(defun jde-complete-sort-comparison (first second) 
  (string< (car first) (car second)))

(defun jde-complete-get-variables (variables) 
  "Transform a list of the type (\"var\" \"java.lang.String\")
into (\"var\" \"java.lang.String\ var\")"
  (let (result current prev)
    (if (null jde-complete-unique-method-names) 
        (while variables
          (setq current (car (car variables))) 
          (setq result
                (append
                 (list (cons (concat current  
                                     " : " 
                                     (nth 1 (car variables)))
                             current))
                 result))
          (setq variables (cdr variables)))
      (while variables
        (if (not (string= prev current))
            (progn 
              (setq prev current)
              (setq result
                    (append
                     (list (cons (concat current 
                                         " : " 
                                         (nth 1 (car variables)))
                                 current))
                     result))))
        (setq variables (cdr variables))))
    result))

(defun jde-complete-build-completion-list (classinfo)
  "Build a completion list from the CLASSINFO list, as returned by the
jde.util.Completion.getClassInfo function."
  (let (result tmp current prev end-paren end-parens)
    ;; get the variable fields
    (setq tmp (car classinfo))
    (setq result (jde-complete-get-variables tmp))

    ;; get inner classes
    (setq tmp (nth 3 classinfo))
    (if tmp 
        (while tmp
          (let* ((fullname (caar tmp))
                 (pos (string-match "\\$" fullname))
                 (name (substring fullname (+ 1 pos))))
            (setq result
                  (append 
                   (list (cons (concat name " : " fullname)
                               name))
                   result)))
          (setq tmp (cdr tmp))))

    ;; get the methods 
    ;;Setting the end paren
    (setq end-paren (if (null jde-complete-add-space-after-method)
              "("
            " ("))

    ;;Setting the end parenthesis
    (setq end-parens
          (if (null jde-complete-insert-method-signature)
              (if (null jde-complete-add-space-after-method)
                  "()"
                " ()")
            ""))
    (setq tmp (nth 2 classinfo))
    (setq prev nil)
    (if (null jde-complete-unique-method-names)
        (while tmp
          (let* ((type (car (cdr (car tmp))));;method type i.e. boolean
                 (method (jde-complete-build-information-for-completion (car tmp) end-paren)))
            (setq current (car (car tmp))) 
            (setq result
                  (append
                   (list (cons
                          (concat method " : " type)
                          (concat
                           (if (null jde-complete-insert-method-signature)
                               current
                             method)
                           end-parens)))
                   result)))
          (setq tmp (cdr tmp)))
      (while tmp
        (let* ((type (car (cdr (car tmp))))
               (method (jde-complete-build-information-for-completion (car tmp) end-paren)))
          (setq current (car (car tmp))) 
          (if (not (string= prev current))
              (progn
                (setq prev current)
                (setq result
                      (append
                       (list (cons
                              (concat method " : " type)
                              (concat
                               (if (null jde-complete-insert-method-signature)
                                   current
                                 method)
                               end-parens)))
                       result)))))
        (setq tmp (cdr tmp))))
    result))

(defun jde-complete-build-information-for-completion (lst end-parens)
  (let ((result (concat
                 (car lst)
                 end-parens)))
    (setq lst (cdr (cdr lst)))
    (while lst
      (setq result (concat result (car lst)))
      (setq lst (cdr lst))
      (if lst
	  (setq result (concat result ", "))))
    (setq result (concat result ")"))
    result))

(defun jde-complete-complete-cycle ()
  "Replace the previous completion by the next one in the list."
  (let (elem tmp)
    (setq jde-complete-current-list-index (1+ jde-complete-current-list-index))
    (if (>= jde-complete-current-list-index (length jde-complete-current-list))
	(setq jde-complete-current-list-index 0))
    (setq elem (nth jde-complete-current-list-index jde-complete-current-list))
    (setq tmp (cdr elem))
    (if tmp
	(progn
	  (delete-region jde-complete-current-beginning jde-complete-current-end)
	  (insert tmp)
	  (set-marker jde-complete-current-end 
		      (+ (marker-position jde-complete-current-beginning) (length tmp)))
          (setq jde-complete-current-signature (car elem))
          (setq jde-complete-display-signature t)
          (run-at-time jde-complete-signature-display-time
                       nil `set-variable `jde-complete-display-signature nil)
	  (eldoc-message jde-complete-current-signature))
      (message (format "No completion at this point!(cycle)")))
    ;;  (goto-char (marker-position jde-complete-current-end))
    ))

(defun jde-complete-insert-completion (item)
  (if item 
      (progn
        (delete-region jde-complete-current-beginning jde-complete-current-end)
        (insert item)
        (eldoc-message jde-complete-current-signature)
        (setq jde-complete-display-signature t)
        (run-at-time jde-complete-signature-display-time nil `set-variable `jde-complete-display-signature nil)
	(set-marker jde-complete-current-end 
		    (+ (marker-position jde-complete-current-beginning) 
		       (length item))))))

(defun jde-complete-find-all-completions (pair lst &optional exact-match)
 (let* (tmp
        chop-pos
        (args (nth 2 pair))
        (pat (nth 1 pair))
        (result nil)
        (first-char (substring pat 0 1)))
   (if (null args)
       (setq exact-match nil)
     (setq pat (concat pat args))) 
       
    (while lst
      (setq tmp (car (car lst)))
      (setq chop-pos (string-match " : " tmp))
      (setq tmp (substring tmp 0 chop-pos))
      (if (if exact-match 
	      (string= pat tmp)
    (equal 0 (string-match pat tmp)))
	  (setq result (append result (list (car lst)))))
      (setq lst (cdr lst)))
    result))

(defun jde-complete-split-by-dots (s)
  "Return a list containing the longest substring of S that ends with a dot, and the rest.But removes the intermediate(=last) dot."
  ;;we now isolate the last atom after a dot and the beginning
  (if (string-match "\\(.*\\)\\.\\(.*\\)" s)
      (list (match-string 1 s) (match-string 2 s))
    nil))

(defun jde-complete-get-component-type-of-array-class (name)
  (let (result)
    (setq result (jde-jeval (concat "System.out.println( Class.forName(\"" name "\").getComponentType().getName()) ;")));;removed \n
    (substring result 0 (- (length result) 1))))

(defvar jde-complete-attempted-to-import nil
  "Variable use to avoid looping in jde-complete-eval-type-of when
the type of a class could not be found an it tried to import it")

(defvar jde-complete-casting nil
  "Variable use to determined when the variable was casted")

(defun jde-complete-eval-type-of (expr)
  "Eval type of EXPR and returns either a java class name or a java type name."
					;(debug)
  (let (qualified-name chop-pos temp answer)
    (setq answer
          (cond
           ;;If it's number returns an int
           ((integerp expr)
            "int")
           ;;If it's 001L or 134l return long
           ((if (and (integerp (substring expr 0 (- (length expr) 1)))
                     (or (string= "L" (substring expr (- (length expr) 1) (length expr)))
                         (string= "l" (substring expr (- (length expr) 1) (length expr)))))
                "long"))
           ;;If it's a floating point return float
           ((floatp expr)
            "double")
           ;;If it's 000F or 1234f return float
           ((if (and (floatp (substring expr 0 (- (length expr) 1)))
                     (or (string= "F" (substring expr (- (length expr) 1) (length expr)))
                         (string= "f" (substring expr (- (length expr) 1) (length expr)))))
                "float"))
           ((string-match "false\\|true" expr)
            "boolean")
           ;;Checking if it's a character
           ((string= "'" (substring expr 0 1))
            "char")
           ;; If it's "this", we return the class name of the class we code in
           ((string= "this" expr)
            (jde-complete-get-qualified-name (jde-parse-get-class-at-point)))
           
           ;; If it's "super", we return the super class name of the class we code in
           ((string= "super" expr)
            (jde-complete-get-qualified-name (jde-parse-get-super-class-at-point)))
           ;;if it's a class name, done
           ((setq qualified-name (jde-complete-get-qualified-name expr))
            qualified-name)

           ;;check if it's an inner class
           ((and (string= "this.this" expr)
                 (setq qualified-name (jde-complete-get-inner-class (jde-parse-get-class-at-point))))
            qualified-name)
                           
           (t
            (let (to-complete
                  (last-char 
                   (aref expr (- (length expr) 1 ))))
              ;; If it ends with a parenthesis
              (cond
               ((eq last-char ?\))
                (let* ((result (jde-complete-isolate-before-matching-of-last-car expr))
                       (temp (jde-complete-split-by-dots result))
                       to-complete)
                  (if temp
                      (jde-complete-find-completion-for-pair temp)
                    ;;we need exact completion here
                    (jde-complete-find-completion-for-pair (list "this" result) nil 1))

                  ;;if the previous did not work try only result
                  (if (not jde-complete-current-list)
                      (jde-complete-find-completion-for-pair (list result "")))
                 
                  ;;if the previous did not work try again 
                  (setq qualified-name (jde-complete-get-qualified-name result))
                  (if qualified-name
                      qualified-name
                    (if jde-complete-current-list
                        (progn
                          (setq to-complete (car (car jde-complete-current-list)))
                          (setq chop-pos (string-match " : " to-complete))
                          (substring to-complete (+ chop-pos 3)))
                      (error "Could not find type of %s." expr)))))

               ;;if it's an array
               ((eq last-char ?\])
                (let ((temp (jde-complete-eval-type-of 
                             (jde-complete-isolate-before-matching-of-last-car expr))))
                  (jde-complete-get-component-type-of-array-class temp)))

               ;;we look for atoms if expr is splittable by dots
               ((setq temp (jde-complete-split-by-dots expr))
                ;;we need exact completion here
                (jde-complete-find-completion-for-pair temp t)
                (if jde-complete-current-list
                    (progn
                      (setq to-complete (car (car jde-complete-current-list)))
                      (setq chop-pos (string-match " : " to-complete))
                      (substring to-complete (+ chop-pos 3)))
                  nil))
               (t
                ;; See if it's declared somewhere in this buffer.
                (let (parsed-type result result-qualifier)
                  (setq parsed-type (jde-parse-declared-type-of expr))
                  (setq result (car parsed-type))
                  (setq result-qualifier (cdr parsed-type))

                  (if result
                      (let ((count 0) type)
                        (while (string-match ".*\\[\\]" result)
                          (setq result (substring result 0 (- (length result) 2 )))
                          (setq count (1+ count)))

                        (let (work)
                          (setq type
                                (cond
                                 ;; handle primitive types, e.g., int
                                 ((member result jde-complete-primitive-types)
                                  result)
                                 ;; quickly make sure fully qualified name doesn't exist
                                 ((and result-qualifier 
                                       (jde-complete-class-exists 
                                        (setq work (concat result-qualifier "." result))))
                                  work)
                                 ;; then check for inner classes
                                 ((setq work (jde-complete-get-inner-class-name result result-qualifier))
                                  work)
                                 ;; otherwise use unqualified class name
                                 (t
                                  (jde-complete-get-qualified-name result)))))

                        (if type
                            (progn
                              (while (> count 0)
                                (setq type (concat type "[]"))
                                (setq count (1- count)))
                              (jde-complete-transform-array-classes-names type))
                          (if (y-or-n-p (format "Could not find type of %s. Attempt to import %s? " 
                                                expr result))
                              (progn
                                ;; import
                                (jde-import-find-and-import result)
                                ;; recursive call of eval-type-of
                                (jde-complete-eval-type-of expr))
                            (error "Could not find type of %s" result))))
                    (if (and jde-complete-casting 
                             (null jde-complete-attempted-to-import)
                             (y-or-n-p 
			      (format "Could not find type of %s. Attempt to import %s? " expr expr)))
                        (progn
                          (setq jde-complete-attempted-to-import t)
                          (setq jde-complete-casting nil)
                          (jde-import-find-and-import expr)
                          (jde-complete-eval-type-of expr))
                      (progn
                        (setq jde-complete-attempted-to-import nil)
                        nil))))))))))
    answer))

(defun jde-complete-find-type (pair) 
  "Tries to find the type of complex pairs"
  (list "Object" ""))

(defun jde-complete-get-inner-class (expr) 
  "Takes a single argument like B.A and split it up in a name
and qualifer. The name and the qualifier are then use as arguments 
to the function `jde-complete-get-inner-class-name'"
  (let (name qualifier (pos (string-match "\\." expr)))
    (if pos
        (progn
          (setq name (substring expr (+ 1 pos)))
          (setq qualifier (substring expr 0 pos))))
    (jde-complete-get-inner-class-name name qualifier)))

;; Tries to divine inner class via the following algorithm:
;; if no qualifier (e.g. A), then try
;;   <this-class>$A
;; if qualifier (e.g. D.C.B.A), then try the following in order:
;;   <this-class>$D$C$B$A
;;   <super-types>$D$C$B$A (NOT YET IMPLEMENTED)
;;   D$C$B$A           i.e  D$C$B$A in default, current, or imported package
;;   D + C$B$A         i.e  D in imports or local/default package with a C$B$A inner class
;;   D.C.B$A           i.e. B$A in package D.C
;;   D.C$B$A           i.e. C$B$A in package D
(defun jde-complete-get-inner-class-name (name qualifier)
  "Tries to find an inner class with the class name of name.  If qualifier
is nil, the inner class is only searched on current class at point.  If
qualifier is not nil, a wider array of possible inner classes will
searched.  Returns nil if not found; a fully qualified inner class name
otherwise."
  (let (class-name this-full-class-name result)
    (setq this-full-class-name (jde-complete-get-qualified-name (jde-parse-get-class-at-point)))
    (cond
     (qualifier
      (let ((work (concat qualifier "$" name)))
	(setq work (subst-char-in-string ?. ?\$ work))

	;; check against this$D$C$B$A
	(setq class-name (concat this-full-class-name "$" work))
	(if (not (jde-complete-class-exists class-name))
	    (let (dot-count index first-part remaining-part)
	      (setq class-name nil)
             
	      ;; check against D$C$B$A (default and imported)
	      (print "Zero dots" (get-buffer "*scratch*"))
	      (setq class-name (jde-complete-get-qualified-name work))

	      ;; check remaining semi-qualified variants
	      (setq dot-count 1)
	      (while (and (null class-name)
			  (setq index (string-match "\\$" work)))

		(setq first-part (substring work 0 index))
		(setq remaining-part (if (< index (length work))
					 (substring work (+ 1 index))
				       nil))
		(cond
		 ;; check against C$B$A on class D
		 ((= 1 dot-count)
		  (setq class-name (jde-complete-get-qualified-name first-part))
		  (if (not (null class-name))
		      (progn
			(setq class-name (concat class-name "$" remaining-part))
			(if (null (jde-complete-class-exists class-name))
			    (setq class-name nil)))))

		 ;; just check if it exists (ignoring imports)
		 (t
		  (setq class-name (concat first-part "." remaining-part))
		  (if (null (jde-complete-class-exists class-name))
		      ;; lastly check if it exists in this package
		      (progn
			(setq class-name (concat (jde-parse-get-package-name) "." work))
			(if (not (jde-complete-class-exists class-name))
			    (setq class-name nil))))))

		(setq work (concat first-part "." remaining-part))
		(setq dot-count (1+ dot-count)))

	      class-name))))

     ;; otherwise, no qualifier...check against this$D
     ((jde-complete-class-exists (setq class-name (concat this-full-class-name "$" name)))
      class-name))))
		 

(defvar jde-complete-primitive-types '("byte" "char" "double" "float" 
				       "int" "long" "short" "boolean")
  "Primitive Java types.")

(defun jde-complete-find-completion-for-pair (pair &optional exact-completion &optional access-level)
  (let ((type (jde-complete-eval-type-of (car pair))))
    (if type
	(cond ((member type jde-complete-primitive-types)
               (error "Cannot complete primitive type: %s." type))
              ((string= type "void")
               (error "Cannot complete return type of %s is void." (car pair)))
              (access-level
               (let ((classinfo (jde-complete-get-classinfo-with-access-level type access-level)))
                 (if classinfo
                     (if (and (string= (nth 1 pair) "")
                              (not exact-completion))
                         (setq jde-complete-current-list classinfo) 
                       (setq jde-complete-current-list 
                             (jde-complete-find-all-completions pair classinfo exact-completion))))))
              (t
               (let ((classinfo (jde-complete-get-classinfo type)))
                 (if classinfo
                    (if (and (string= (nth 1 pair) "")
                             (not exact-completion))
                        (setq jde-complete-current-list classinfo) 
                      (setq jde-complete-current-list 
                            (jde-complete-find-all-completions pair classinfo exact-completion)))))))
      nil)))

(defun jde-complete-transform-array-classes-names (name)
  (let (result)
    (while (string-match ".*\\[\\]" name)
      (setq name (substring name 0 (- (length name) 2 ))) 
      (setq result (concat "[" result)))
    (if result
	(progn
	  (cond
	   ((string= name "byte")
	    (setq result (concat result "B")))
	   ((string= name "char")
	    (setq result (concat result "C")))
	   ((string= name "double")
	    (setq result (concat result "D")))
	   ((string= name "float")
	    (setq result (concat result "F")))
	   ((string= name "int")
	    (setq result (concat result "I")))
	   ((string= name "long")
	    (setq result (concat result "J")))
	   ((string= name "short")
	    (setq result (concat result "S")))
	   ((string= name "boolean")
	    (setq result (concat result "Z")))
	   (t
	    (setq result (concat result "L" name ";"))))
	  result)
      name)))
 
(defun jde-complete-at-point ()
  "Completes the method or field name at point.
Repeating the command cycles through all potential completions for the name.
This function displays the signature of a method completion in the minibuffer.
This command uses the Beanshell to run Java code that in turn uses Java
reflection to determine the methods and fields defined by the class of the
object at point. This command starts the Beanshell if necessary. Hence, you
may experience a slight delay when using this command for the first time in
a session or when completing a field or method of an object that has many
methods and fields. See `jde-complete-at-point-menu' for a version of this 
command that lets you select the desired completion from a popup menu."
  (interactive)
  (if (and
       jde-complete-current-list
       (markerp jde-complete-current-beginning)
       (markerp jde-complete-current-end)
       (marker-position jde-complete-current-beginning)
       (marker-position jde-complete-current-end)
       (>= (point) (marker-position jde-complete-current-beginning))
       (<= (point) (marker-position jde-complete-current-end))
       (eq last-command this-command))
      (jde-complete-complete-cycle) 
    ;;else
    (progn 
      (let (completions
            jde-complete-attempted-to-import
            (pair (jde-complete-java-variable-at-point))
            (access nil))
        (if (string= (car pair) "" )
	    (progn
	      (setcar pair "this")))
        (if (null pair)
	    (progn
	      (setq pair (list "this" ""))
	      (set-marker jde-complete-current-beginning (point) ) 
	      (insert-before-markers "this.")
	      (set-marker jde-complete-current-end (point))))
        (if (string= (car pair) "this")
            (setq access 1)
          (if (string= (car pair) "super")
              (setq access 0)))
        (if access
            (setq completions (jde-complete-find-completion-for-pair pair nil access))
          (setq completions (jde-complete-find-completion-for-pair pair)))
        ;;if the jde-complete-current-list  is nil check if the method is in the current class(this)
        (if (null completions)
            (setq completions 
                  (jde-complete-find-completion-for-pair (list (concat "this." (car pair)) "")
                                                   nil 1)))
        ;;if completions is still null check if the method is in the super class
        (if (null completions)
            (setq completions 
                  (jde-complete-find-completion-for-pair (list (concat "super." (car pair)) "")
                                                   nil 0)))
        (if completions 
            (progn
              (setq jde-complete-current-list-index -1)
              (jde-complete-complete-cycle))
          (message "Could not fint type of %s." (car pair)))))))

(defun jde-complete-popup-completion-menu (&optional title)
  "Popup a completion menu for the object at point.
The popup menu displays all of the possible completions for the object
it was invoked on.  To automatically split large menus this function
use `imenu--mouse-menu' to handle the popup menu."
  (let (index-alist pair name)
    (setq index-alist jde-complete-current-list)
    (setq pair
          (if (= (length index-alist) 1)
              ;; if only one item match, return it 
              (car index-alist)
            ;; delegates menu handling to imenu :-)
            (imenu--mouse-menu index-alist   
                               (jde-cursor-posn-as-event) ; Popup window at text cursor
                               (or title "Completion"))))
    (setq name (cdr pair))
    (setq jde-complete-current-signature (car pair))
    (jde-complete-insert-completion name)))

(defun jde-cursor-posn-as-event()
  "Returns the text cursor position as an EVENT on Emacs and the
mouse cursor position on XEmacs."
  (if jde-xemacsp
      (let* ((mouse-pos (mouse-pixel-position))
	     (x (car (cdr mouse-pos)))
	     (y (cdr (cdr mouse-pos))))
	(make-event 'button-press `(button 1 modifiers nil x ,x y ,y)))
    (let ((x (* (if jde-xemacsp (frame-width) (frame-char-width))
		(if (and
		     (boundp 'hscroll-mode)
		     hscroll-mode)
		    (hscroll-window-column)
		  (mod (current-column) (window-width)))))
	  (y  (* (if jde-xemacsp (frame-height) (frame-char-height)) 
		 (- (count-lines (point-min) (point))
		    (count-lines (point-min) (window-start)))))
	  (window (get-buffer-window (current-buffer))))
      (list (list x y) window))))
  
(defun jde-complete-at-point-menu ()
  "Completes the method or field name at point.
This command displays a popup menu listing the potential completions for the name
at point. Selecting a completion causes the command to use the completion to complete
the name at point. See `jde-complete-at-point' for a version of this 
command that lets you cycle throught the potential completions at point."
  (interactive)
  (let* ((pair (jde-complete-java-variable-at-point))
         jde-complete-attempted-to-import
         completion-list
         access)
    (if (string= (car pair) "" )
	(progn
          (setcar pair "this")))
    (if (null pair)
	(progn
          (setq pair (list "this" "" ))
	  (set-marker jde-complete-current-beginning (point) ) 
	  (insert-before-markers "this.")
	  (set-marker jde-complete-current-end (point))))
    (if (string= (car pair) "this")
        (setq access 1)
      (if (string= (car pair) "super")
          (setq access 0)))
    (if access 
        (setq completion-list (jde-complete-find-completion-for-pair pair nil access))
      (setq completion-list (jde-complete-find-completion-for-pair pair)))
    ;;if the completion list is nil check if the method is in the current class(this)
    (if (null completion-list)
        (setq completion-list (jde-complete-find-completion-for-pair
                               (list (concat "this." (car pair)) "")
                               nil 1)))
    ;;if completions is still null check if the method is in the super class
    (if (null completion-list)
        (setq completion-list (jde-complete-find-completion-for-pair
                               (list (concat "super." (car pair)) "")
                               nil 0)))
    
    (if completion-list
        (let ((title (concat (car pair) "."
                             (car (cdr pair)) "[...]")))
          (jde-complete-popup-completion-menu title))
      (message "Could not find type of %s." (car pair)))))

(defun jde-complete-eldoc-print-current-symbol-info ()
  "Print information using `eldoc-message' while in function `eldoc-mode'.
You can override the info collecting part with `eldoc-current-symbol-info'."
  ;;resetting the jde-complete-display-signature
  (if jde-complete-display-signature
      (eldoc-message jde-complete-current-signature)
    (if (semantic-active-p)
	(progn
	  (require 'eldoc)
	  (require 'senator)
	  (senator-eldoc-print-current-symbol-info)))))

(defadvice eldoc-print-current-symbol-info (around jde activate)
  "Enable ELDOC in non Emacs Lisp, but jde mode."
  (if (eq major-mode 'emacs-lisp-mode)
      ad-do-it
    (if (eq major-mode 'jde-mode)
        (if (eldoc-display-message-p)
            (jde-complete-eldoc-print-current-symbol-info))
      (eldoc-mode -1))))

(provide 'jde-complete)

;; $Log: jde-complete.el,v $
;; Revision 1.34  2001/08/15 05:15:38  paulk
;; Fixed bug that cause completion to return the wrong name when the class does not have
;; a package, e.g., .Test for Test.java. Thanks to Javier Lopez.
;;
;; Revision 1.33  2001/07/31 05:09:03  paulk
;; Fixes bug that prevented completion of variables in inner classes. Thanks to Javier Lopez.
;;
;; Revision 1.32  2001/07/21 03:49:17  paulk
;; Fixed missing first line. Fix for handling static inner classes was contributed by Javier Lopez.
;;
;; Revision 1.31  2001/07/21 03:46:12  paulk
;; Now handles completion for methods and fields of static inner classes.
;;
;; Revision 1.30  2001/07/18 01:45:38  paulk
;; Fixes bug in completion of inner classes. Thanks to Javier Lopez.
;;
;; Revision 1.29  2001/07/17 05:37:46  paulk
;; Bug fixes from Javier Lopez.
;;
;; Revision 1.28  2001/07/13 04:59:06  paulk
;; Bug fixes from Javier Lopez.
;;
;; Revision 1.27  2001/07/06 02:09:33  paulk
;; Many improvements. Thanks to Javier Lopez.
;;
;; Revision 1.26  2001/06/12 07:22:05  paulk
;; Completion now works for instances of inner classes in many cases.
;; The inner class must be declared before it is referenced for completion to work.
;; Future releases will fix this and other deficiencies in inner class handling.
;; Thanks to "Evan Easton" <evan@eeaston.com>.
;;
;; Revision 1.25  2001/06/12 06:03:14  paulk
;; * Completion list now includes private methods of the current class and protected methods of
;;   the super class.
;;
;; * Now sorts the possible completions in alphabetical order.
;;
;; * Fixes bug that prevented completion of  super class methods, i.e. super.ge...
;;
;; Thanks to Javier Lopez <jlopez@cellexchange.com>
;;
;; Revision 1.24  2001/05/31 05:14:39  paulk
;; Provide support for per-project caching of class data in the Beanshell. Thanks to Matt Conway.
;;
;; Revision 1.23  2001/04/02 02:45:04  paulk
;; Add some comments.
;;
;; Revision 1.22  2001/03/15 19:47:07  paulk
;; Now pops up the completion menu at the text cursor position on Emacs and at the mouse cursor position on XEmacs. I will change this to popup at the text cursor position on XEmacs as well as soon as I can figure out how to do it. Thanks to Matt_Conway@i2.com for providing the Emacs version of this enhancement.
;;
;; Revision 1.21  2001/01/25 04:31:01  paulk
;; Completion now asks user whether to import a class that it cannot find. Thanks to Phillip Lord.
;;
;; Revision 1.20  2000/12/19 04:33:34  paulk
;; Fixed popup completion menu to work on XEmacs. Thanks to David Ponce for providing this fix.
;;
;; Revision 1.19  2000/10/25 02:52:16  paulk
;; Fixed bug where the completion function was completing symbols that it could not find with the results of the previous completion.
;;
;; Revision 1.18  2000/10/20 04:02:10  paulk
;; Now uses semantic for some functions. Thanks to David Ponce.
;;
;; Revision 1.17  2000/10/08 12:55:39  paulk
;; *** empty log message ***
;;
;; Revision 1.16  2000/09/30 17:00:20  paulk
;; Use imenu to display completion choices. Thanks to David Ponce.
;;
;; Revision 1.15  2000/08/19 07:07:05  paulk
;; Flushes cache at end of compilation.
;;
;; Revision 1.14  2000/08/11 05:15:05  paulk
;; Now flushes the classinfo cache at the end of a compilation.
;;
;; Revision 1.13  2000/08/10 08:48:49  paulk
;; Now handles primitive arrays correctly.
;;
;; Revision 1.12  2000/08/09 02:04:26  paulk
;; Adds support for completion of array instances. Thanks to Steff.
;;
;; Revision 1.11  2000/08/01 07:37:40  paulk
;; Now caches methods and fields for each class referenced in a session. Now completes private and protected methods and fields. Thanks to Stephane <s.nicolas@videotron.ca>.
;;
;; Revision 1.10  2000/07/30 20:06:12  paulk
;; Updated doc for jde-complete-at-point and jde-complete-at-point-menu commands.
;;
;; Revision 1.9  2000/07/27 04:54:00  paulk
;; Now completes object fields to any depth and completes variables declared in method argument lists. Thanks to Stephane Nicolas <s.nicolas@videotron.ca>.
;;
;; Revision 1.8  2000/07/26 14:42:23  paulk
;; Adds support for static fields and methods and completion of fields and methods of this
;; and super objects. Thanks to  Stephane Nicolas <s.nicolas@videotron.ca> for this enhancement.
;;
;; Revision 1.7  2000/06/01 05:52:25  paulk
;; Completion menu now works on XEmacs.
;;
;; Revision 1.6  2000/05/16 04:41:28  paulk
;; *** empty log message ***
;;

;; end of jde-complete.el

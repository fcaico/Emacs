;;; jde-wiz.el
;; $Revision: 1.57 $
;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2000, 2001 Paul Kinnucan.

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

(require 'beanshell)
(require 'jde-complete)
(require 'jde-widgets)
(require 'jde-parse)
(require 'jde-gen)

(defgroup jde-wiz nil
  "JDE Wizards"
  :group 'jde
  :prefix "jde-wiz-")

(defun jde-wiz-escape-backslashes-in-path (path) 
  (mapconcat
   (lambda (char) 
     (if (eq char ?\\) "\\\\" (char-to-string char)))
   path ""))

(defun jde-wiz-update-class-list()
  "Update the class list used to resolve class names.
The first time you invoke a JDE wizard, the JDE builds a list of all classes on the classpath
defined by jde-global-classpath. Wizards use this list to resolve unqualified class names. If you add any classes to the classpath after invoking a wizard, you should update the class list."
  (interactive)
  (if (bsh-running-p)
      (progn
	(message "Rescanning classes...")
	(jde-jeval (jde-create-prj-values-str))
	(jde-jeval "jde.util.JdeUtilities.buildClassList(true);")
	(message "Rescanning classes...Complete"))))

(defun jde-wiz-set-bsh-project()
  "Update the beanshell's concept of the current project and the
classpath associated with it.  This may cause an update scan of the
class list the next time a wizard uses the class list for a lookup.
The scanning only occurs if the project is newly opened or its
classpath has been changed since the last scan, and switching between
projects does not necessarily force a rescan as the scan information
is cached in the beanshell.  You can force a rescan for a project by
calling `jde-wiz-update-class-list'."
  (interactive)
  (when (bsh-running-p)
    (jde-jeval (jde-create-prj-values-str))))

(defun jde-wiz-get-package-name ()
  (let ((package-re "^[ \t]*package[ \t]+\\(.*\\)[ \t]*;"))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward package-re (point-min) t)
	(looking-at package-re)
	(buffer-substring-no-properties
	 (match-beginning 1)
	 (match-end 1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Interface Implementation wizard                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-wiz-get-unqualified-name (name)
  (string-match "[^.]+$" name)
  (substring name (match-beginning 0) (match-end 0)))

(defun jde-wiz-update-implements-clause (interface-name &optional extends)
  "Updates the implements clause unless extends is non-nil. In that case the 
extends clause is updated"
  (interactive
   "sEnter interface: ")
  (let (keyword
        (interface 
	 (jde-wiz-get-unqualified-name interface-name)))
    (if extends 
        (setq keyword "extends")
      (setq keyword "implements"))
    (save-excursion
      (let* ((class-re "class[ \t]+\\([a-zA-z]+[a-zA-Z0-9._]*\\).*[ \n]*")
	     (open-brace-pos
	      (scan-lists (point) -1 1))
	     (class-name-end-pos
	      (when open-brace-pos
		(goto-char open-brace-pos)
		(when (re-search-backward class-re (point-min) t)
		  (looking-at class-re)
		  (match-end 1))))
	     (implements-keyword-end-pos
	      (when (and open-brace-pos class-name-end-pos)
		(goto-char open-brace-pos)
		(if (re-search-backward keyword class-name-end-pos t)
		    (match-end 0)))))
	(if implements-keyword-end-pos
	    (progn
	      (goto-char implements-keyword-end-pos)
	      (insert (concat " " interface ", ")))
	  (when class-name-end-pos
	    (goto-char (- open-brace-pos 1))
	    (insert (concat " " keyword " " interface " "))))))))

(defun jde-wiz-generate-interface (interface-name)
  "*Generate a skeleton implementation of a specified interface."
  (condition-case err
      (let* ((javadoc (if jde-wiz-include-javadoc
                          "true"
                        "false"))
             (new-line (if jde-gen-k&r
                           "true"
                         "false"))
	     (code
	      (jde-jeval-r
	       (concat
		"jde.wizards.InterfaceFactory.makeInterface(\""
		interface-name "\", " javadoc ", true, "
                new-line ");"))))
	(if code 
	    (let ((required-imports
		   (jde-jeval-r
		    "jde.wizards.InterfaceFactory.getImportedClasses();")))
	      (insert code)
	      (if required-imports
		  (jde-import-insert-imports-into-buffer required-imports))
	      (jde-wiz-update-implements-clause interface-name))))	  
    (error
     (message "%s" (error-message-string err)))))

(defun jde-wiz-implement-interface-internal (interface-name)
  "*Generate a skeleton implementation of a specified interface.
This command works only for interfaces defined by `jde-global-classpath', if
set, otherwise the CLASSPATH environment variable."
  (interactive
   "sInterface name: ")
  (condition-case err
      (let ((names 
	     (jde-jeval-r
	      (format "jde.util.JdeUtilities.getQualifiedName(\"%s\");" interface-name ))))
	(if names
	    (if (> (length names) 1)
		(let ((dialog
		       (jde-option-dialog
			"class name dialog"
			:options names
			:text "Select interface to implement."
			:ok-action 'jde-wiz-generate-interface)))
		  (jde-dialog-show dialog))
	      (jde-wiz-generate-interface (car names)))
	  (error "Cannot find interface %s on the current classpath." interface-name)))
    (error
     (message "%s" (error-message-string err)))))

(defun jde-wiz-implement-interface (interface-name)
  "*Generate a skeleton implementation of a specified interface.
This command works only for interfaces that exist on the classpath
defined by `jde-global-classpath', if set, otherwise 
by the CLASSPATH environment variable."
  (interactive
   "sInterface name: ")
  (jde-wiz-implement-interface-internal interface-name)
  (goto-char (scan-lists (point) -1 1))
  (c-indent-exp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method override wizard                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-wiz-override-method (method-name) 
  "Overrides a method whose name you specify.
This command creates a skeleton implementation of the
overridden method at point. This command infers the
qualified name of the class of the overriden method by 
prepending the package name of the current buffer to
the class containing point. If the class defines
more than one method of the same name, this command
prompts you to select the desired method from a list
of method prototypes.

This command also generates import statements for 
the parameter and return types of the overridden method.
The import statements are inserted after the last 
existing import statement or the package statement
or the first blank line in the source file. Import
statements are generated only for types for which an
import statement does not already exist in the file.

NOTE: this command works only if the overriding class 
      has been previously compiled."
  (interactive
   "sMethod name: " )
  (condition-case err
      (let* ((class-name 
	      (jde-parse-get-super-class-at-point))
	     (qualified-class-name
	      (jde-complete-get-qualified-name class-name)))
	(if qualified-class-name
	    (let ((signatures
		   (jde-jeval
		    (concat 
		     "jde.wizards.MethodOverrideFactory.getCandidateSignatures(\""
		     qualified-class-name "\",\"" method-name "\");") t)))
	      (if signatures
		  (if (> (length signatures) 1)
		      (let ((dialog
			     (jde-option-dialog
			      "Select Override Method"
			      :options signatures
			      :text "Select method to override."
			      :ok-action 'jde-wiz-override-method-internal
			      :ok-action-args (list signatures))))
			(jde-dialog-show dialog))
		    (jde-wiz-override-method-internal (car signatures)  signatures))))
	  (error "Cannot find parent class %s" class-name)))
    (error
     (message "%s" (error-message-string err)))))


(defun jde-wiz-override-method-internal (selected-method methods)
  (let* ((variant
	  (position selected-method methods :test 'string=))
	 (javadoc (if jde-wiz-include-javadoc
                          "true"
                        "false"))
         (new-line (if jde-gen-k&r
                           "true"
                         "false"))
	 (skeleton
	  (jde-jeval-r
	   (concat
	    "jde.wizards.MethodOverrideFactory.getMethodSkeleton("
	    (int-to-string variant) ", " javadoc ", " new-line ");")))
	 (required-imports
	  (jde-jeval-r
	   "jde.wizards.MethodOverrideFactory.getImportedClasses();")))
    (insert skeleton)
    (goto-char (scan-lists (point) -1 1))
    (c-indent-exp)
    (if required-imports
	(jde-import-insert-imports required-imports))))

;;
;; Contributed by Javier Lopez and Paul Kinnucan
;;
(defun jde-browse-class(&optional class-name) 
  "Browse class in the beanshell class browser"
  (interactive)
  (let ((class
	 (or class-name
	     (read-from-minibuffer "Class: " (thing-at-point 'symbol)))))
	(lexical-let (browse-class)
	  (fset 'browse-class
		(lambda (class)
		  (bsh-eval (format "browseClass(\"%s\");" class))))
	  (jde-parse-select-qualified-class-name
	   class 'browse-class))))

(defun jde-wiz-delegate (attribute-name)
  "*Generate delegation methods for a given attribute.
This method generates methods in the current buffer that delegate
calls to methods of the given ATTRIBUTE of the current class.  For
example, if the current buffer contains Java class A and there is
an attribute in A named b of class type B, this method will
generate in A, all the public methods of class B and delegate
handling of those methods via attribute B."
  (interactive
   "sAttribute name: ")
  (condition-case err
      (let* ((start nil)
             (javadoc (if jde-wiz-include-javadoc
                          "true"
                        "false"))
             (new-line (if jde-gen-k&r
                           "true"
                         "false"))
             (class-name (or (jde-complete-get-qualified-name
                              (car (jde-parse-declared-type-of attribute-name)))
                             (read-string (concat "Enter fully qualified class name of " 
                                                  attribute-name ": "))))
	     (code
	      (jde-jeval-r
	       (concat
		"jde.wizards.DelegateFactory.makeDelegates(\""
		attribute-name "\", \"" class-name "\", " javadoc ", true, "
		new-line ");"))))
	(if code 
	    (let ((required-imports
		   (jde-jeval-r
		    "jde.wizards.DelegateFactory.getImportedClasses();")))
              (font-lock-mode -1)
              (setq start (point))
	      (insert code)
              ;;indenting the new code
              (goto-char (scan-lists (point) -1 1))
              (c-indent-exp)
	      (if required-imports
		  (jde-import-insert-imports-into-buffer required-imports))
              (font-lock-mode)
              )))	  
    (error
     (message "%s" (error-message-string err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Abstract Implementation wizard                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-wiz-generate-abstract-class (class-name)
  "*Generate a skeleton implementation of a specified abstract class."
  (condition-case err
      (let* ((javadoc (if jde-wiz-include-javadoc
                          "true"
                        "false"))
             (new-line (if jde-gen-k&r
                           "true"
                         "false"))
	     (code
	      (jde-jeval-r
	       (concat
		"jde.wizards.AbstractClassFactory.makeAbstractClass(\""
		class-name "\", " javadoc ", true, "
		new-line ");"))))
	(if code 
	    (let ((required-imports
		   (jde-jeval-r
		    "jde.wizards.AbstractClassFactory.getImportedClasses();")))
	      (insert code)
	      (if required-imports
		  (jde-import-insert-imports-into-buffer required-imports))
	      (jde-wiz-update-implements-clause class-name t))))	  
    (message "%s" (error-message-string err))))

(defun jde-wiz-extend-abstract-class-internal (class-name)
  "*Generate a skeleton implementation of the specified abstract class.
This command works only for interfaces defined by `jde-global-classpath', if
set, otherwise the CLASSPATH environment variable."
  (interactive
   "sAbstract classname: ")
  (condition-case err
      (let ((names 
	     (jde-jeval-r
	      (format "jde.util.JdeUtilities.getQualifiedName(\"%s\");" class-name ))))
	(if names
	    (if (> (length names) 1)
		(let ((dialog
		       (jde-option-dialog
			"class name dialog"
			:options names
			:text "Select abstract class to extend."
			:ok-action 'jde-wiz-generate-abstract-class)))
		  (jde-dialog-show dialog))
	      (jde-wiz-generate-abstract-class (car names)))
	  (message "Cannot find abstract class %s on the current classpath." class-name)))
    (message "%s" (error-message-string err)))
  (goto-char (scan-lists (point) -1 1))
  (c-indent-exp))

(defun jde-wiz-extend-abstract-class (class-name)
  "*Generate a skeleton implementation of the abstract methods of the
a specified class. This command works only for abstract classes that exist
on the classpath defined by `jde-global-classpath', if set, otherwise 
by the CLASSPATH environment variable."
  (interactive
   "sAbstract class name: ")
  (jde-wiz-extend-abstract-class-internal class-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get Set Method Implementation wizard                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom jde-wiz-include-javadoc t
  "This variables indicates if javadoc comments should be included in
skeletons created by wizards. This variable modifies the output
of the functions `jde-wiz-get-set-methods', 
`jde-wiz-generate-interface', `jde-wiz-extend-abstract-class' and
`jde-wiz-delegate'"
  :group 'jde-wiz
  :type 'boolean)

(defcustom jde-wiz-get-set-variable-prefix "arg"
  "This variable defines a prefix to be added to argmument names
for the funtion `jde-wiz-get-set-methods'. For example if this
variable is set to \"arg\" the following variable
String name; will produce this method signature:
public String get(String argName)"
  :group 'jde-wiz
  :type 'string)

(defcustom jde-wiz-get-javadoc-template
  (list "/**"
        "* Gets the value of %n"
        "*"
        "* @return the value of %n"
        "*/")
  "Template used by `jde-wiz-get-set-method' to add the javadoc
to a get method. The %n are replaced by the variable name and
%t by the variable."
  :group 'jde-wiz
  :type '(repeat string))

(defcustom jde-wiz-set-javadoc-template
  (list "/**"
        "* Sets the value of %n"
        "*"
        "* @param %p Value to assign to this.%n"
        "*/")
  "Template used by `jde-wiz-get-set-method' to add the javadoc to a
set method. The %n are replaced by the variable name, %t by the variable
type and %p for the argument name. In most cases %n and %p are the same 
except when `jde-wiz-get-set-variable-prefix' is non nil; in this case the
%p will pick up those changes as well."
  :group 'jde-wiz
  :type '(repeat string))

(defcustom jde-wiz-show-report t
  "A non nil value will show a report of the existing get set methods
and the ones added"
  :group 'jde-wiz
  :type 'boolean)

(defcustom jde-wiz-get-set-variable-convention (cons "" nil)
  "Use this variable to specify your coding conventions. This variable is used
by the `jde-wiz-get-set-method' to filter the convention from the variable declared
in the buffer from the name of the method. The are 3 options a prefix, a postfix, and
the first uppercase letter. For example, choosing a prefix and setting
it to '_' means that the '_' will be filtered from all the variable containing it.
So this variable private String _name;  will produce this get method:
public getName(String name) {
this._name = name
} A postfix works in a similar way, the third option behaves slighlty different.
For example if the variable is String _strName; this will get filter to name. It will
take everything after the first capitalize letter. A nil value will use the 
variable name as it is defined in the buffer."
  :group 'jde-wiz
  :type '(cons (string :tag "Enter either the prefix or postfix")
               (radio-button-choice (const "Prefix") 
                                    (const "Postfix")
                                    (const "Everything after the first upcase letter")
                                    (const nil))))

(defcustom jde-wiz-get-set-methods-include (list "Both")
  "Use this variable to set what methods `jde-wiz-get-set-methods' will 
insert in the buffer. The options are get methods only, set methods only,
 and both."
  :group 'jde-wiz
  :type '(list (radio-button-choice (const "Get only")
                                    (const "Set only")
                                    (const "Both"))))
  
(defcustom jde-wiz-get-set-methods-order (list "Get followed by set for each field")
  "Use this variable to set the order in which the 
get and set methods are going to be inserted by
`jde-wiz-get-set-methods'"
  :group 'jde-wiz
  :type '(list (radio-button-choice
                (const "Get followed by set for each field")
                (const "Set followed by get for each field")
                (const "All get methods followed by all set methods")
                (const "All set methods followed by all get methods"))))

(defun jde-wiz-downcase-initials (obj) 
  "It downcase the first letter of obj"
  (concat (downcase (substring obj 0 1)) (substring obj 1)))

(defun jde-wiz-get-class-parts (class-name tokens)
  "Recurse through all the tokens in `tokens' looking for
the tokens of `class-name', returns nil if no token are found"
  (let ((parts (jde-wiz-get-class-parts-helper class-name tokens))
        temp-parts inner-classes)
    (if (not parts)
        (while tokens
          (setq temp-parts (semantic-token-type-parts (car tokens)))
          (setq inner-classes (semantic-find-nonterminal-by-token 'type temp-parts))
          (setq parts (jde-wiz-get-class-parts class-name inner-classes))
          (if parts 
              (setq tokens nil)
            (setq tokens (cdr tokens)))))
    parts))

(defun jde-wiz-get-class-parts-helper (class-name tokens)
  "Checks the top level for the class name `class-name'
if one is found it returns the parts of it, nil is
return otherwise"
  (let (parts current-class)
    (while tokens
      (setq current-class (car tokens))
      (if (string= class-name (semantic-token-name current-class))
          (progn
            (setq parts (semantic-token-type-parts current-class))
            (setq tokens nil)))
      (setq tokens (cdr tokens)))
    parts))

(defun jde-wiz-get-member-p (name functions)
  "Returns t if the variable name has a get method defined in the
current buffer. Functions are semantic tokens for the functions
defined in the current buffer."
  (or (member 
       (concat 
	"get" 
	(upcase-initials (jde-wiz-get-name name))) functions)
      (member 
       (concat 
	"is" 
	(upcase-initials (jde-wiz-get-name name))) functions)))
                      
(defun jde-wiz-set-member-p (name functions)
  "Returns t if the variable name has a set method defined in the current buffer.
Functions are semantic tokens for the functions defined in the current buffer."
  (member (concat "set" (upcase-initials (jde-wiz-get-name name))) functions)) 

(defun jde-wiz-get-set-methods()
  "Generates get and set methods for all the private fields
defined in the current buffer."
  (interactive)
  (let* ((include (car jde-wiz-get-set-methods-include))
	 ;;Indicates if both get and set should be inserted
	 (bothp (string= "Both" include))
	 ;;only get methods should be inserted
	 (get-onlyp (string= "Get only" include))
	 ;;only set methods should be inserted
	 (set-onlyp (string= "Set only" include))
	 (order (car jde-wiz-get-set-methods-order))
	 (get-firstp 
	  (or (string= "All get methods followed by all set methods" order)
	      (string= "Get followed by set for each field" order)))
	 (class (jde-parse-get-class-at-point));;class name 
	 (classes (split-string class "\\."))
	 (class-name (nth (- (length classes) 1) classes))
	 (tokens (semantic-bovinate-toplevel t));;buffer tokens
	 (type (semantic-find-nonterminal-by-token 'type tokens));;class tokens
	 (parts (jde-wiz-get-class-parts class-name type))
	 (variables (semantic-find-nonterminal-by-token 'variable parts));;declared variables
	 ;;non public variables
	 (non-public-variables (jde-wiz-filter-variables-by-typemodifier variables))
	 (functions (semantic-find-nonterminal-by-token 'function parts));;functions
	 (set-get-functions (jde-wiz-get-get-set-methods functions));;get,set,is functions
	 var name staticp finalp report temp)
      
    (setq 
     report 
     (concat 
      (format "%-60.60s" "Field") 
      (if get-firstp "\tGetter  \tSetter\n" "\tSetter  \tGetter\n")))

    (setq 
     report 
     (concat 
      report
      (format "%-60.60s" "------------------------------------------------------------")
      "\t--------\t--------\n"))
    
    (while non-public-variables
      (setq var (car non-public-variables))
      (setq name (semantic-token-name var));;variable name
      (setq type (semantic-token-type var));;variable type i.e. boolean
      (setq staticp (member "static" (semantic-token-variable-modifiers var)));;is it static
      (setq finalp  (member "final" (semantic-token-variable-modifiers var)));;is it final

      (setq 
       report 
       (concat 
	report 
	(format 
	 "%-60.60s"
	 (concat 
	  type " " name " "
	  (and (semantic-token-variable-modifiers var)
	       ;; only if some modifiers are present
	       ;; print them
	       (format "%s" (semantic-token-variable-modifiers var)))))))

      (setq report (concat report "\t"))
     
      ;;order in which the methods should be inserted
      (cond ((string= "Get followed by set for each field" order)
             ;;get method followed by its set method
             ;;getting the get method if it does not exist in the buffer
             (if (not (jde-wiz-get-member-p name set-get-functions))
                 (if (or bothp get-onlyp) 
                     (progn
                       (insert (jde-wiz-get-get-method type name staticp class))
                       (setq report (concat report "[Added  ]")))
                   (setq report (concat report "[Skipped]")))
               (setq report (concat report "[Exists ]")))
             
             (setq report (concat report "\t"))
         
             ;;getting the set method if it does not exist in the buffer
             (if (and (not finalp);; it is final - can not have a setter
                      (not (jde-wiz-set-member-p name set-get-functions)))
                 (if (or bothp set-onlyp)
                     (progn
                       (insert (jde-wiz-get-set-method type name staticp class))
                       (setq report (concat report "[Added  ]")))
                   (setq report (concat report "[Skipped]")))
               (if (jde-wiz-set-member-p name set-get-functions) 
                   (setq report (concat report "[Exists ]"))
                 (if finalp
                     (setq report (concat report "[N/A    ]"))))))
            ;;set method followed by its get method
            ((string= "Set followed by get for each field" order)
             ;;getting the set method if it does not exist in the buffer
             (if (and (not finalp);; it is final - can not have a setter
                      (not (jde-wiz-set-member-p name set-get-functions)))
                 (if (or bothp set-onlyp)
                     (progn
                       (insert (jde-wiz-get-set-method type name staticp class))
                       (setq report (concat report "[Added  ]")))
                   (setq report (concat report "[Skipped]")))
               (if (jde-wiz-set-member-p name set-get-functions)
                   (setq report (concat report "[Exists ]"))
                 (if finalp
                     (setq report (concat report "[N/A    ]")))))

             (setq report (concat report "\t"))
             
             ;;getting the get method if it does not exist in the buffer
             (if (not (jde-wiz-get-member-p name set-get-functions))
                 (if (or bothp get-onlyp) 
                     (progn
                       (insert (jde-wiz-get-get-method type name staticp class))
                       (setq report (concat report "[Added  ]")))
                   (setq report (concat report "[Skipped]")))
               (setq report (concat report "[Exists ]"))))
             
            ;;all the get method first
            ((string= "All get methods followed by all set methods" order)
             ;;getting the get method if it does not exist in the buffer
             (if (not (jde-wiz-get-member-p name set-get-functions))
                 (if (or bothp get-onlyp) 
                     (progn
                       (insert (jde-wiz-get-get-method type name staticp class))
                       (setq report (concat report "[Added  ]")))
                   (setq report (concat report "[Skipped]")))
               (setq report (concat report "[Exists ]")))
             
             (setq report (concat report "\t"))
             
             ;;getting the set method if it does not exist in the buffer
             (if (and (not finalp);; it is final - can not have a setter
                      (not (jde-wiz-set-member-p name set-get-functions)))
                 (if (or bothp set-onlyp)
                     (progn
                       (setq temp (concat temp (jde-wiz-get-set-method type name staticp class)))
                       (setq report (concat report "[Added  ]")))
                   (setq report (concat report "[Skipped]")))
               (if (jde-wiz-set-member-p name set-get-functions) 
                   (setq report (concat report "[Exists ]"))
                 (if finalp
                     (setq report (concat report "[N/A    ]"))))))
             
            ;;all the set method first
            ((string= "All set methods followed by all get methods" order)
             ;;getting the set method if it does not exist in the buffer
             (if (and (not finalp);; it is final - can not have a setter
                      (not (jde-wiz-set-member-p name set-get-functions)))
                 (if (or bothp set-onlyp)
                     (progn
                       (insert (jde-wiz-get-set-method type name staticp class))
                       (setq report (concat report "[Added  ]")))
                   (setq report (concat report "[Skipped]")))
               (if (jde-wiz-set-member-p name set-get-functions)
                   (setq report (concat report "[Exists ]"))
                 (if finalp
                     (setq report (concat report "[N/A    ]")))))

             (setq report (concat report "\t"))
             
             ;;getting the get method if it does not exist in the buffer
             (if (not (jde-wiz-get-member-p name set-get-functions))
                 (if (or bothp get-onlyp) 
                     (progn
                       (setq temp (concat temp (jde-wiz-get-get-method type name staticp class)))
                       (setq report (concat report "[Added  ]")))
                   (setq report (concat report "[Skipped]")))
               (setq report (concat report "[Exists ]")))))
      
      (setq report (concat report "\n"))
      
      (setq non-public-variables (cdr non-public-variables)))

    (if temp (insert temp))
    (if jde-wiz-show-report
        (with-output-to-temp-buffer (concat "*jde-wiz-get-set-methods report for " class "*")
          (princ report))))

  ;;indenting the new code
  (goto-char (scan-lists (point) -1 1))
  (c-indent-exp))
   
(defun jde-wiz-get-get-set-methods(tokens) 
  "Returns a list of the methods that start with set, get or is."
  (let (token name filtered-methods)
    (while tokens
      (setq token (car tokens))
      (setq name (semantic-token-name token))
      (if (or (string-match "^get" name)
              (string-match "^set" name)
              (string-match "^is" name))
          (setq filtered-methods (append filtered-methods (list name))))
      (setq tokens (cdr tokens)))
    filtered-methods))

(defun jde-wiz-filter-variables-by-typemodifier(tokens)
  "Returns a subsets of tokens. It returns the tokens that contains either private or 
protected modifiers"
  (let (token modifiers filtered-tokens)
    (while tokens
      (setq token (car tokens))
      (setq modifiers (semantic-token-variable-modifiers token))
      (if (not (member "public" modifiers))       
          (setq filtered-tokens (append filtered-tokens (list token))))
      (setq tokens (cdr tokens)))
    filtered-tokens))

(defun jde-wiz-get-name(variable) 
  "Gets the name of variable to be used in generation the get set
method templates. For Example if the variable is _Name and the variable
`jde-wiz-get-set-filter-variable` is set to prefix _ this method will 
return Name"
  (let (answer
        (cfs case-fold-search)
        (fix (car jde-wiz-get-set-variable-convention))
        (convention (cdr jde-wiz-get-set-variable-convention)))
    (setq case-fold-search nil)
    (cond ((not convention)
           (setq answer variable))
          ((string= "Prefix" convention)
           (progn
             (if fix
                 (let ((pos (string-match (concat "^" fix) variable)))
                   (if pos 
                       (setq answer (substring variable (+ pos (length fix))))
                     (setq answer variable)))
               (setq answer variable))))
          ((string= "Postfix" convention) 
           (progn
             (if fix
                 (let ((pos (string-match (concat fix "$") variable)))
                   (if pos 
                       (setq answer (substring variable 0 pos))
                     (setq answer variable)))
               (setq answer variable))))
          (t 
           (let ((pos (string-match "[A-Z]." variable)))
             (if pos 
                 (let ((ans (substring variable pos)))
                   (if ans
                       (setq answer ans)
                     (setq answer variable)))
               (setq answer variable)))))
    (setq case-fold-search cfs)
    answer))

           
(defun jde-wiz-get-get-method(type name &optional staticp &optional class-name) 
  "Returns a string representing a get method"
  (let ((filtered-name (jde-wiz-get-name name))
        get (javadoc "") temp temp2)
    (setq 
     get 
     (concat 
      "\n"
      (if jde-wiz-include-javadoc
	  (progn 
	    (setq temp2 jde-wiz-get-javadoc-template)
	    (while temp2
	      (setq temp (car temp2))
	      (while (string-match "%n" temp)
		(setq 
		 temp 
		 (replace-match 
		  (jde-wiz-downcase-initials filtered-name) nil nil temp)))
	      (while (string-match "%t" temp)
		(setq temp (replace-match type nil nil temp)))
	      (setq javadoc (concat javadoc temp "\n"))
	      (setq temp2 (cdr temp2)))
	    javadoc))
      "public " (if staticp "static ")
      type " " (if (string= type "boolean") "is" "get")
      (upcase-initials filtered-name) "() "
      (if jde-gen-k&r "{" "\n{") "\n"
      "return " (if staticp (concat class-name ".") "this.")
      name ";\n}\n"))
    get))
                                  
(defun jde-wiz-get-set-method(type name &optional staticp class-name) 
  "Returns a string representing a set method"
  (let ((filtered-name (jde-wiz-get-name name))
        set (javadoc "") arg-name temp temp2)
    (if jde-wiz-get-set-variable-prefix 
        (setq 
	 arg-name 
	 (jde-wiz-downcase-initials 
	  (concat jde-wiz-get-set-variable-prefix (upcase-initials filtered-name))))
      (setq arg-name (jde-wiz-downcase-initials filtered-name)))
    
    (setq 
     set 
     (concat 
      "\n"
      (if jde-wiz-include-javadoc
	  (progn 
	    (setq temp2 jde-wiz-set-javadoc-template)
	    (while temp2
	      (setq temp (car temp2))
	      (while (string-match "%n" temp)
		(setq temp 
		      (replace-match 
		       (jde-wiz-downcase-initials filtered-name) nil nil temp)))
	      (while (string-match "%t" temp)
		(setq temp (replace-match type nil nil temp)))
	      (while (string-match "%p" temp)
		(setq temp (replace-match arg-name nil nil temp)))
	      (setq javadoc (concat javadoc temp "\n"))
	      (setq temp2 (cdr temp2)))
	    javadoc))
      "public " (if staticp "static ")
      "void set" (upcase-initials filtered-name)
      "(" type " " arg-name ")" 
      (if jde-gen-k&r "{" "\n{") "\n"
      (if staticp (concat class-name ".") "this.")
      name " = " arg-name ";\n}\n"))
    set))

(provide 'jde-wiz)


;;; Change History:

;; $Log: jde-wiz.el,v $
;; Revision 1.57  2001/08/18 05:49:12  paulk
;; Fixed regression error in the method override wizard and reimplemented the method selection dialog to use jde-option-dialog. Also eliminated some dead code.
;;
;; Revision 1.56  2001/08/17 03:50:53  paulk
;; Fixes bugs caused by my rewording of jde-wiz-get-set-methods-order options. Thanks to Javier Lopez.
;;
;; Revision 1.55  2001/08/15 05:01:13  paulk
;; Fixed capitalization bug caused by setting the jde-wiz-get-set-convention
;; to Everything after the first upcase letter. Thanks to Javier Lopez.
;;
;; Revision 1.54  2001/08/14 06:05:49  paulk
;; Updated jde-browse-class to use jde-parse-select-qualified-class-name.
;;
;; Revision 1.53  2001/08/11 06:47:18  paulk
;; * Wizards now observe the jde-gen-k&r indentation preference.
;; * Adds jde-wiz-get-set-methods-include and jde-wiz-get-set-methods-order variables.
;;   Thanks to Javier Lopez.
;;
;; Revision 1.52  2001/08/05 06:02:12  paulk
;; Various fixes and improvements to the get-set method code generation command. Thanks to Javier Lopez.
;;
;; Revision 1.51  2001/08/04 03:42:04  paulk
;; Adds jde-wiz-extend-abstract-class command. Thanks to Javier Lopez.
;;
;; Revision 1.50  2001/07/31 05:07:39  paulk
;; Adds JDE->Wizards->Generate Get/Set Methods. Thanks to Javier Lopez and Sandip Chitale.
;;
;; Revision 1.49  2001/07/08 04:35:48  paulk
;; Added compatibility fixes for NT/XEmacs use of backslash as the
;; default directory sep character. Thanks to William Griswold <wgg@cs.ucsd.edu>
;;
;; Revision 1.48  2001/06/28 04:06:04  paulk
;; Fixed jde-wiz-update-classes so that it updates the classes list to the CLASSPATH environment variable if jde-global-classpath is not defined.
;;
;; Revision 1.47  2001/06/12 07:18:55  paulk
;; Changed jde-parse-declared-type-of to return a qualified type.
;; Thanks to "Evan Easton" <evan@eeaston.com> .
;;
;; Revision 1.46  2001/05/31 05:14:39  paulk
;; Provide support for per-project caching of class data in the Beanshell. Thanks to Matt Conway.
;;
;; Revision 1.45  2001/05/23 05:14:00  paulk
;; Updated jde-wiz-override-method-internal to intent the inserted code.
;;
;; Revision 1.44  2001/05/23 05:03:32  paulk
;; Updated jde-wiz-implement-interface to indent the inserted code.
;;
;; Revision 1.43  2001/05/21 06:45:39  paulk
;; Implement interface command now accepts unqualified interface names.
;;
;; Revision 1.42  2001/04/27 03:58:13  paulk
;; Fixes Override Method Wizard so that it does not require a compiled version of the base class.
;;
;; Revision 1.41  2001/04/16 06:02:27  paulk
;; Normalize paths. Thanks to Nick Sieger.
;;
;; Revision 1.40  2001/04/11 03:16:36  paulk
;; Updated to resolve relative paths relative to the project file that defines them. Thanks to Nick Seiger.
;;
;; Revision 1.39  2001/03/13 03:45:06  paulk
;; Cosmetic changes.
;;
;; Revision 1.38  2000/12/21 13:24:12  paulk
;; Changed jde-wiz-insert-imports to jde-import-insert-imports to reflect recent repackaging scheme.
;;
;; Revision 1.37  2000/12/18 05:22:46  paulk
;; *** empty log message ***
;;
;; Revision 1.36  2000/11/27 06:18:41  paulk
;; Miscellaneous bug fixes and minor enhancements.
;;
;; Revision 1.35  2000/11/20 05:15:16  paulk
;; Added jde-import-organize command. Moved all import-related code from
;; jde-wiz.el to a new package named jde-import.el.
;;
;; Revision 1.34  2000/11/17 04:02:24  paulk
;; jde-wiz-update-class-list now uses the current jde-global-classpath when building the class list. This eliminates the need to restart the beanshell when switching projects.
;;
;; Revision 1.33  2000/11/16 05:44:56  paulk
;; Fixed problem in jde-sort-imports command. The problem was that jde-sort-imports temporarily defined sort-fold-case just before invoking sort-lines. Invoking sort-lines caused the sort package to be loaded. Since sort-fold-case is already defined, the sort package did not bother to define it. Then sort-lines returns to jde-sort-lines which proceeded to destroy the temporary copy of sort-fold-case. The fix is to have jde-sort-lines require the sort package before invoking sort-lines.
;;
;; Revision 1.32  2000/11/16 04:53:26  paulk
;; Adds jde-wiz-kill-extra-imports command contributed by David Ponce.
;;
;; Revision 1.31  2000/10/25 04:35:20  paulk
;; Updated sort import function to reflect new bovinator syntax.
;;
;; Revision 1.30  2000/10/08 12:55:39  paulk
;; *** empty log message ***
;;
;; Revision 1.29  2000/09/24 08:42:52  paulk
;; Now sorts import list after adding an import. To disable this feature, set jde-auto-sort-imports off. Thanks to "Jason Stell" <Jason.Stell@globalone.net>
;;
;; Revision 1.28  2000/08/03 04:54:11  paulk
;; Restored use of the radio buttons by the import wizard. Somehow this functionality got deleted.
;;
;; Revision 1.27  2000/07/28 06:27:46  paulk
;; Committing all modified files.
;;
;; Revision 1.26  2000/07/14 05:22:57  paulk
;; Adds a delegation wizard contributed by Charles Hart.
;;
;; Revision 1.25  2000/07/13 07:18:24  paulk
;; * You can now specify a list of packages to exclude from import
;;   into a source file. See jde-wiz-import-excluded-packages for
;;   more information. Thanks to "Jim Loverde" <loverde@str.com>
;;   for this enhancement.
;;
;; * Changed name of jde-wiz-insert-excluded-packages-regexp to
;;   jde-wiz-import-excluded-packages.
;;
;; Revision 1.23  2000/06/22 02:50:25  paulk
;; The import wizard dialog now uses radio buttons rather than check boxes to select
;;  the class to import. Thanks to Mark Gibson for this enhancement.
;;
;; Revision 1.22  2000/06/01 06:01:14  paulk
;; Added jde-sort-imports command. Thanks to David Ponce <david_ponce@mail.schneider.fr>.
;;
;; Revision 1.21  2000/01/18 07:11:26  paulk
;; Added jde-show-class-source. Thanks to Phil Lord for the initial
;; implementation of this command.
;;
;; Revision 1.20  1999/12/19 07:02:30  paulk
;; Changed import wizard to use jde.util.JdeUtilities.getQualifiedName
;; eliminated redundancy. Thanks to Len Trigg <len@intelligenesis.net>
;; for this improvement.
;;
;; Revision 1.19  1999/11/01 03:11:42  paulk
;; Added jde-browse-class contributed by Rohit Namjoshi <Rohit_Namjoshi@trilogy.com>.
;;
;; Revision 1.18  1999/10/17 04:35:05  paulk
;; Fixed a line in jde-wiz.el, where an int is concat'd with some
;; strings.  This is not allowed by XEmacs 21.1.
;;
;; Revision 1.17  1999/10/01 05:58:14  paulk
;; Added jde-wiz-update-class-list function contributed by Phillip Lord.
;;
;; Revision 1.16  1999/09/17 06:52:50  paulk
;; Fixed regression error where the interface wizard was putting quotes
;; around the code inserted into the source buffer.
;;
;; Revision 1.15  1999/08/29 04:29:18  paulk
;; Patches provided by Michael Ernst <mernst@alum.mit.edu>
;;
;; Revision 1.14  1999/08/23 02:13:43  paulk
;; Fixed regression bug in jde-wiz-implement-interface caused by recent
;; change in jde-wiz-insert-imports.
;;
;; Revision 1.13  1999/06/22 21:12:20  paulk
;; Added variable to filter out unwanted classes from the list of classes being
;; considered for import command by jde-find-and-import. The jde-find-and-import
;; command now prompts the user if more than one class matches the specified
;; import name. Thanks to Phillip Lord <plord@hgmp.mrc.ac.uk> for these enhancements.
;;
;; Revision 1.12  1999/05/07 20:42:25  paulk
;; Cosmetic change.
;;
;; Revision 1.11  1999/05/07 20:40:49  paulk
;; Added new command, jde-wiz-find-and-import, that, given an unqualified class
;; name, generates and inserts an import statement for that class.
;;
;; Revision 1.10  1999/02/17 19:16:07  paulk
;; Provided more robust error handling for the interface wizard. The wizard
;; no longer kills the bsh when it cannot create an interface and provides
;; meaningfull error messages.
;;
;; Revision 1.9  1999/02/15 01:12:54  paulk
;; Fixed bug in jde-wiz-get-method-class that caused it to fail when the open bracket
;; for the class was not on the same line as the class keyworkd. Thanks to
;; P.Lord@mdx.ac.uk (Phillip Lord) for diagnosing this bug.
;;
;; Revision 1.8  1999/02/12 15:13:00  paulk
;; Added jde-wiz-import function.
;;
;; Revision 1.7  1999/02/11 19:14:50  paulk
;; Fixed bug in jde-wiz-update-implements-clause.
;;
;; Revision 1.6  1999/02/11 18:28:40  paulk
;; Corrected missing parentheses.
;;
;; Revision 1.5  1998/11/22 22:03:43  paulk
;; Fixed bug in interface wizard.
;;
;; Revision 1.4  1998/11/22 21:55:33  paulk
;; Fixed bug in interface wizard.
;;
;; Revision 1.3  1998/11/21 02:41:34  paulk
;; Fixed bug.
;; Added implements clause update function to interface implementation wizard.
;;
;; Revision 1.2  1998/11/10 00:46:39  paulk
;; Added smart import insertion to interface wizard.
;;
;; Revision 1.1  1998/11/08 00:39:24  paulk
;; Initial revision
;;


;; End of jde-wiz.el

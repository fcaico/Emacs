;;; jde-parse.el
;; $Revision: 1.34 $ 

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

(require 'semantic)
(require 'semantic-sb)
(require 'semantic-bnf)
(require 'semantic-ctxt)
(require 'avltree)
(require 'eieio)
(require 'jde-java-grammar)

;;; david@dponce.com
(require 'jde-imenu)                    ; All the imenu stuff is here now!

(defcustom jde-auto-parse-enable t
  "Enables automatic reparsing of a Java source buffer after you makes changes to the buffer, but only if the buffer is less than 
`jde-auto-parse-max-buffer-size'."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-auto-parse-buffer-interval 180
  "Time in seconds between the time you change a Java source buffer
and the time the JDE reparses the buffer."
  :group 'jde-project
  :type 'number)

(defcustom jde-auto-parse-max-buffer-size 50000
  "Maximum size in bytes of buffers that the JDE automatically reparses
when `jde-auto-parse-enable' is t. Setting the threshold to 0 causes the
JDE to parse a buffer automatically regardless of its size."
  :group 'jde-project
  :type 'number)


(defvar jde-parse-buffer-needs-reparse-p nil
  "True if buffer changed since last parse.")
(make-variable-buffer-local 'jde-parse-buffer-needs-reparse-p)

(defvar jde-auto-parse-buffer-timer nil)
(make-variable-buffer-local 'jde-auto-parse-buffer-timer)

;;(defun t1 () (interactive) (semantic-bovinate-toplevel))
;;(defun t2 () (interactive) (semantic-bovinate-toplevel t))

;;(defun t1 () (interactive) (semantic-find-nonterminal-by-overlay))

(defun jde-parse-after-buffer-changed ()
  ;; This function should be called only in JDE buffers
  ;; but for some reason it is called in every buffer
  ;; in some versions of XEmacs. Hence the following 
  ;; guard.
  (when (equal major-mode 'jde-mode)
    (message "Test parse buffer")
    (semantic-clear-toplevel-cache)
    (semantic-bovinate-toplevel)))

(defun jde-parse-should-auto-parse-buffer-p ()
  "Return t if the JDE should automatically reparse the buffer"
  (and jde-auto-parse-enable
       (or 
	(<= jde-auto-parse-max-buffer-size 0)
	(< (buffer-size) jde-auto-parse-max-buffer-size))))

(defun jde-parse-buffer-changed-hook (begin end length)
  ;; This function should be called only in JDE buffers
  ;; but for some reason it is called in every buffer
  ;; in some versions of XEmacs. Hence the following 
  ;; guard.
  (when (string= mode-name "JDE")
    (setq jde-parse-buffer-needs-reparse-p t)
    (if (and (jde-parse-should-auto-parse-buffer-p)
	     (not jde-auto-parse-buffer-timer))
	(setq jde-auto-parse-buffer-timer 
	      (run-with-timer 
	       jde-auto-parse-buffer-interval 
	       nil 'jde-parse-after-buffer-changed)))))

(defun jde-parse-buffer-contains-multiple-classes-p ()
  "Returns nonnil if buffer contains multiple class
definitions."
  (let* ((top-level-classes  
	  (semantic-find-nonterminal-by-token 
	   'type 
	   (semantic-bovinate-toplevel)))
	 (top-level-class-count (length top-level-classes)))
    (or 
     (>  top-level-class-count 1)
     (and
      (= top-level-class-count 1)
      (let* ((inner-class-parts (semantic-token-type-parts (car top-level-classes)))
	     (inner-classes 
	      (semantic-find-nonterminal-by-token 
	       'type inner-class-parts)))
	(>= (length inner-classes) 1))))))

					; (defun test ()
					;   (interactive)
					;   (message 
					;    (if (jde-parse-buffer-contains-multiple-classes-p)
					;        "Yes"
					;      "No")))
   
(defvar jde-parse-buffer-contains-multiple-classes-p nil
  "TRUE if buffer contains more than one class definition")
(make-variable-buffer-local 'jde-parse-buffer-contains-multiple-classes-p)


(defun jde-parse-update-after-parse ()
  (when (jde-parse-should-auto-parse-buffer-p)
    (setq jde-parse-buffer-needs-reparse-p nil)
    (if jde-auto-parse-buffer-timer
	(cancel-timer jde-auto-parse-buffer-timer))
    (setq jde-auto-parse-buffer-timer nil))
  (when (and
	 (boundp 'semantic-toplevel-bovine-cache)
	 (car semantic-toplevel-bovine-cache))
    (setq jde-parse-buffer-contains-multiple-classes-p
	  (jde-parse-buffer-contains-multiple-classes-p))
    (setq jde-parse-the-method-map (jde-parse-method-map "Method map"))))

(defun jde-get-java-source-buffers ()
  "Get a list of the Java source buffers open in the
current session."
  (mapcan (lambda (buffer)
	    (save-excursion
	      (set-buffer buffer)
	      (if (equal major-mode 'jde-mode)
		  (list buffer))))
	  (buffer-list)))

(defun jde-get-visible-source-buffers ()
  "Returns a list of visible Java source buffers."
  (delq
   nil
   (mapcar
    (lambda (buffer) 
      (if (get-buffer-window buffer 'visible) buffer))
    (jde-get-java-source-buffers))))

(defun jde-get-selected-source-buffer ()
  (let ((selected-buffer (window-buffer (selected-window))))
    (save-excursion
      (set-buffer selected-buffer)
      (if (equal major-mode 'jde-mode) selected-buffer))))

(defun jde-parse-get-package-name ()
  "Gets the name of the package in which the Java source file in the
current buffer resides."
  (let ((package-re "package[ \t]+\\(.*\\)[ \t]*;"))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward package-re (point-min) t)
	(looking-at package-re)
	(buffer-substring-no-properties
	 (match-beginning 1)
	 (match-end 1))))))

(defun jde-parse-get-package-from-name (class-name)
  "Gets the package portion of a qualified class name."
  (substring 
   class-name 0
   (let ((pos  (position ?. class-name :from-end t)))
     (if pos
	 pos
       0))))

(defun jde-parse-get-unqualified-name (name)
  "Gets the last name in a qualified name." 
  (string-match "[^.]+$" name)
  (substring name (match-beginning 0) (match-end 0)))


(defun jde-parse-get-super-class-at-point ()
  (setq superClass "Object")
  (let ((class-re "extends[ \t]+\\([a-zA-z]+[a-zA-Z0-9._]*\\).*[ \n]*"))
    (save-excursion
      (let ((open-brace-pos
	     (scan-lists (point) -1 1)))
	(when open-brace-pos
	  (goto-char open-brace-pos)
	  (when (re-search-backward class-re (point-min) t)
	    (looking-at class-re)
	    (setq superClass (buffer-substring-no-properties
			      (match-beginning 1)
			      (match-end 1))))))))
  superClass
  )

(defun jde-parse-get-innermost-class-at-point ()
  "Get the innermost class containing point.
If point is in a class, this function returns 
(CLASS_NAME . CLASS_POSITION), where CLASS_NAME is the 
name of the class and CLASS_POSITION is the position
of the first character of the class keyword. Otherwise,
this function returns nil."
  ;; (interactive)
  (let ((left-paren-pos (c-parse-state)))
    (if left-paren-pos
	(save-excursion
	  (catch 'class-found
	    (let ((left-paren-index 0)
		  (left-paren-count (length left-paren-pos)))
	      (while (< left-paren-index left-paren-count)
		(let ((paren-pos (nth left-paren-index left-paren-pos)))
		  (unless (consp paren-pos)
		    (goto-char paren-pos)
		    (if (looking-at "{")
			(let* ((search-end-pos
			       (if (< left-paren-index (1- left-paren-count))
				   (let ((pos (nth (1+ left-paren-index) left-paren-pos)))
				     (if (consp pos)
					 (cdr pos)
				       pos))
				 (point-min)))
                              (case-fold-search nil)
                              (class-re "^[ \t]*\\(\\(public\\|abstract\\|final\\|static\\|strictfp\\|protected\\)[ \t]+\\)*[ \t]*class[ \t]+\\([^ \t\n{]*\\).*")
                              (class-pos (re-search-backward class-re search-end-pos t)))      
                           (if class-pos
			      (progn
				(looking-at class-re)
				(throw
				 'class-found
				 (cons
				  (buffer-substring-no-properties
				   (match-beginning 3)
				   (match-end 3))
				  class-pos))))))))
		  (setq left-paren-index (1+ left-paren-index)))))))))


(defun jde-parse-get-class-at-point () 
  (let ((class-info (jde-parse-get-innermost-class-at-point))
	class-name)
    (while class-info
      (let ((name (car class-info))
	    (pos (cdr class-info)))
	(if (not class-name)
	    (setq class-name name)
	  (setq class-name (concat name "." class-name)))
	(save-excursion
	  (goto-char pos)
	  (setq class-info (jde-parse-get-innermost-class-at-point)))))
    class-name)) 

(defun jde-parse-get-classes-at-point ()
  (interactive)
  (let ((class (jde-parse-get-innermost-class-at-point)))
    (if class (message "%s %s" (car class) (cdr class) ) (message "no class")))
  ;; (goto-char (aref (c-search-uplist-for-classkey (c-parse-state)) 0))
  )


(defun jde-parse-select-qualified-class-name (class action &optional select-args prompt)
  "PROMPT the user to select the fully qualified name for CLASS. Then,
invoke ACTION where action is a function that takes the selected name as its first
argument and SELECT-ARGS as its remaining arguments."
  (condition-case err
      (let ((names 
	     (jde-jeval-r
	      (format "jde.util.JdeUtilities.getQualifiedName(\"%s\");" class))))
	(if names
	    (if (> (length names) 1)
		(let ((dialog
		       (jde-option-dialog
			"Class Name Dialog"
			:options names
			:text (or prompt "Select class.")
			:ok-action action
			:ok-action-args select-args)))
		  (jde-dialog-show dialog))
	      (apply action (car names) select-args))
	  (error "Cannot find class %s on the current classpath." class)))
    (error
     (message "%s" (error-message-string err)))))


(defun jde-parse-qualified-name-at-point ()
  "Returns (cons QUALIFIER NAME) where NAME is the symbol at point and
QUALIFIER is the symbol's qualifier. For example, suppose the name at
point is

     int i = error.msg.length()
                   ^
In this case, this function returns (cons \"error.msg\" \"length\").
This function works only for qualified names that do not contain
white space. It returns null if there is no qualified name at point."
  (let ((symbol-at-point (thing-at-point 'symbol)))
    (when symbol-at-point
      (thing-at-point-looking-at "[^ \n\t();,:+]+")
      (let ((qualified-name 
	     (buffer-substring-no-properties
	      (match-beginning 0)
	      (match-end 0))))
	(string-match "\\(.+[.]\\)*\\([^.]+\\)" qualified-name)
	(let ((qualifier (if (match-beginning 1)
			     (substring qualified-name 
					(match-beginning 1) (match-end 1))))
	      (name (substring qualified-name 
			       (match-beginning 2) (match-end 2))))
	  (if qualifier
	      (setq qualifier (substring qualifier 0 (1- (length qualifier)))))
	  (cons qualifier name))))))


(defun jde-parse-double-backslashes (name)
  (mapconcat (lambda (x) (if (eq x ?\\)
			     "\\\\"
			   (string x)))
	     name ""))

(defun jde-parse-valid-declaration-at (point varname)
  "Verify that a POINT starts a valid java declaration
for the VARNAME variable."
  (save-excursion
    (goto-char point)
    (if (looking-at (concat "\\([][A-Za-z0-9_.\177-\377]+\\)[ \t\n\r]+" 
			    (jde-parse-double-backslashes varname) 
			    "[ \t\n\r]*[),;=]"))
	(let ((type (match-string 1))
	      (type-pos (match-beginning 1)))
	  (goto-char type-pos)
	  ;;  Check for following case.
	  ;;     Object table
	  ;;    //representing objects after all updates.
          ;;    table = new Truc();
          ;;    table.
          ;;  Avoid false hit on updates.
	  (if (not (or 
		    (jde-parse-comment-or-quoted-p)
		    (string= type "instanceof")
                    (string= type "return")))
	      type))
      nil)))

(defun jde-parse-declared-type-of (name)
  "Find in the current buffer the java type of the variable NAME.  The
function returns a cons of two strings.  The first is a string containing
the name of the type, or nil if it cannot be found. The second is a string
containing any qualifying text that precedes the class name, which is nil
if no text that looks like an identifier precedes the type name.  This
function does not give the fully-qualified java class name, it just returns
the type as it is declared, and a qualifier that might be the package or
the containing/outer class of the declared type."
  (save-excursion
    (let (found res pos orgpt resname foundpt qualifier)
      (setq orgpt (point))
      (while (and (not found)
		  (search-backward name nil t))
	(setq pos (point))
	(backward-word 1)
	(setq resname (jde-parse-valid-declaration-at (point) name))
	(setq foundpt (point))
	(goto-char pos)
	(forward-char -1)
	(if (not (null resname))
	    (progn (setq res resname)
		   (setq found t))))
      
      (goto-char orgpt)

      (while (and (not found)
		  (search-forward name nil t))
	(setq pos (point))
	(backward-word 2)
	(setq resname (jde-parse-valid-declaration-at (point) name))
	(setq foundpt (point))
	(goto-char pos)
	(forward-char 1)
	(if (not (null resname))
	    (progn (setq res resname)
		   (setq found t))))

      ;; now check for qualifying identifier. Note: reuses 
      ;; jde-parse-qualified-name-at-point for simplicity, not efficiency
      (if found
	  (let (qualname)
	    (goto-char foundpt)
	    (setq qualname (jde-parse-qualified-name-at-point))
	    (setq qualifier (car qualname))
	    (setq res (cdr qualname))))

      (cons res qualifier))))


(defun jde-display-parse-error (error)
  (let* ((parser-buffer-name "*Java Parser*")
	 (buf (get-buffer parser-buffer-name))) 
    (if (not buf)
	(setq buf (get-buffer-create parser-buffer-name)))
    (set-buffer buf)
    (erase-buffer)
    (insert error)
    (pop-to-buffer buf)))

(defun jde-parse ()
  "*Parses the Java source file displayed in the current buffer.
If the source file parses successfully, this command displays
a success message in the minibuffer. Otherwise, it displays an error
message in the Java Parser buffer. If the Java Parser buffer does
not exist, this command creates it.

Note. This command uses an external Java parser implemented in
Java to parse Java source files. This command uses the JDE's integrated
Java source interpreter, the BeanShell, to invoke the parser. If the
BeanShell is not running, this command starts the BeanShell. Thus,
the first time you invoke the parser you may notice a slight delay
before getting a response. Thereafter, the response should be very
fast."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((parse-error
	 (jde-jeval-r (concat "jde.parser.ParserMain.parseFile(\"" (buffer-file-name) "\");"))))
    (if parse-error
	(jde-display-parse-error parse-error)
      (message "Parsed %s successfully" (buffer-name)))))


;; Thanks to Eric D. Friedman <friedman@lmi.net> for this function.
(defun jde-parse-comment-or-quoted-p ()
  "Returns t if point is in a comment or a quoted string. nil otherwise"
  (interactive "p")
  ;; limit our analysis to the current line.
  (let ((beg (save-excursion (beginning-of-line) (point))))
    (if
        (or
         ;; are we in a javadoc comment?
         (save-excursion
           (re-search-backward
            "^[ \t]*/?\\*"
            beg t))
         ;; are we in a '//' or a '/*' style comment?
         ;; note that /* or /** on a line with only leading whitespace
         ;; will have matched in the previous regex.  We check again here
         ;; because the above case needs to allow for a line with
         ;; the continuation of a comment (using only a '*') whereas this
         ;; requires the presence of a '/' in front of the '*' so as to
         ;; distinguish a comment from a '*' operator.
         ;; To make a long story short,
         ;; the first regex matches
         ;;   /* a comment */
         ;; and
         ;; /**
         ;;  * a comment
         ;;  */
         ;; while the second one matches
         ;; System.out.println(foo); /* a comment */
         ;; but not
         ;; i = 3 * 5;
         ;; if you do something like following, you're on your own:
         ;; i = 3
         ;;       * 5; 
         ;; Suggestions for improving the robustness of this algorithm
         ;; gratefully accepted.
         (save-excursion
           (re-search-backward
            "\\(//\\|/\\*\\)"
            beg t))
         ;; are we in a quoted string?
         (save-excursion
           (re-search-backward
            "\"";;
            beg t)))
        t;; return true if we had any matches; nil otherwise
      nil)))

(defun jde-parse-get-method-at-point (&optional position)
  "Gets the method at POSITION, if specified, otherwise at point.
Returns (CLASS_NAME . METHOD_NAME) if the specified position is
in a method; otherwise, nil."
  ;; Define an internal function that recursively searches a class
  ;; and its subclasses for a method containing point.
  (flet ((search-class 
	  (class pos)
	  (let* ((class-name       (semantic-token-name class))
		 (class-parts      (semantic-token-type-parts class))
		 (class-subclasses (semantic-find-nonterminal-by-token 'type class-parts))
		 (class-methods    (semantic-find-nonterminal-by-token 'function class-parts)))

	    ;; Is point in a method of a subclass of this class?
	    (loop for subclass in class-subclasses do
		  (search-class subclass pos))

	    ;; Is point in any of the methods of this class?
	    (loop for method in class-methods do
		  (let* ((method-name  (semantic-token-name method))
			 (method-start (semantic-token-start method))
			 (method-end   (semantic-token-end method)))
		    (when (and (>= pos method-start) 
			       (<= pos method-end))
		      (throw 'found (cons (cons class-name method-name)
					  (cons method-start method-end)))))))))  
		       
    (let* ((pos (if position position (point)))
	   (tokens (semantic-bovinate-toplevel))
	   (classes (semantic-find-nonterminal-by-token 'type tokens)))
      (catch 'found
	(loop for class in classes
	      do (search-class class pos))))))



(defclass jde-avl-tree ()
  ((tree        :initarg tree
	        :type list
	        :documentation
	        "The tree")
   (compare-fcn :initarg compare-fcn
		:type function
		;; :initform <
		:documentation    
		"Compare function."))
  "Balanced binary tree.")

(defmethod initialize-instance ((this jde-avl-tree) &rest fields)
  "Constructor for binary balanced tree."
  
  ;; Call parent initializer
  (call-next-method)

  (assert (typep  (oref this compare-fcn)  'function))

  (oset this  tree (avltree-create (oref this compare-fcn))))

(defmethod jde-avl-tree-add ((this jde-avl-tree) item)
  "Inserts ITEM in this tree."
  (avltree-enter (oref this tree) item))

(defmethod jde-avl-tree-delete ((this jde-avl-tree) item)
  "Deletes ITEM from THIS tree."
  (avltree-delete (oref this tree) item))

(defmethod jde-avl-tree-is-empty ((this jde-avl-tree))
  "Return t if THIS tree is empty, otherwise return nil."
  (avltree-empty (oref this tree)))

(defmethod jde-avl-tree-find ((this jde-avl-tree) item)
  "Return the element in THIS tree that matches item."
  (avltree-member (oref this tree) item))

(defmethod jde-avl-tree-map ((this jde-avl-tree) map-function)
  "Applies MAP-FUNCTION to all elements of THIS tree."
  (avltree-map (oref this tree) item))

(defmethod jde-avl-tree-first ((this jde-avl-tree))
  "Return the first item in THIS tree."
  (avltree-first (oref this tree)))
  
(defmethod jde-avl-tree-last ((this jde-avl-tree))
  "Return the last item in THIS tree."
  (avltree-last (oref this tree)))

(defmethod jde-avl-tree-copy ((this jde-avl-tree))
  "Return a copy of THIS tree."
  (avltree-copy (oref this tree)))

(defmethod jde-avl-tree-flatten ((this jde-avl-tree))
  "Return a sorted list containing all elements of THIS tree."
  (avltree-flatten (oref this tree)))

(defmethod jde-avl-tree-size ((this jde-avl-tree))
  "Return the number of elements in THIS tree."
  (avltree-size (oref this tree)))

(defmethod jde-avl-tree-clear ((this jde-avl-tree))
  "Delete all elements of THIS tree."
  (avltree-clear (oref this tree)))

(defclass jde-parse-method-map (jde-avl-tree) 
  ()
  "Map of the methods in the current buffer.")


(defun jde-parse-method-map-compare-fcn (m1 m2)
  (and 
   (< (car (cdr m1)) (car (cdr m2)))
   (< (cdr (cdr m1)) (car (cdr m2)))))

(defmethod initialize-instance ((this jde-parse-method-map) &rest fields)
  "Constructor for method map."

  (oset  this compare-fcn 'jde-parse-method-map-compare-fcn)

					;   (oset 
					;    this 
					;    compare-fcn
					;     (lambda (m1 m2)
					;       (and 
					;        (< (car (cdr m1)) (car (cdr m2)))
					;        (< (cdr (cdr m1)) (car (cdr m2))))))

  ;; Call parent initializer.
  (call-next-method)

  (flet ((add-methods 
	  (class)
	  (let* ((class-name       (semantic-token-name class))
		 (class-parts      (semantic-token-type-parts class))
		 (class-subclasses (semantic-find-nonterminal-by-token 'type class-parts))
		 (class-methods    (semantic-find-nonterminal-by-token 'function class-parts)))

	    ;; Add methods of subclasses
	    (loop for subclass in class-subclasses do
		  (add-methods subclass))

	    ;; Add methods of this class?
	    (loop for method in class-methods do
		  (let* ((method-name  (semantic-token-name method))
			 (method-start (semantic-token-start method))
			 (method-end   (semantic-token-end method)))
		    (jde-avl-tree-add 
		     this
		     (cons
		      (cons class-name method-name)
		      (cons method-start method-end))))))))
		       
    (let* ((tokens (semantic-bovinate-toplevel))
	   (classes (semantic-find-nonterminal-by-token 'type tokens)))
      (loop for class in classes do 
	    (add-methods class)))))

(defmethod jde-parse-method-map-get-method-at ((this jde-parse-method-map) &optional pos)
  "Get the method at POS, if specified, otherwise, at point."
  (let ((p (if pos pos (point))))
    (jde-avl-tree-find this (cons (cons "" "") (cons p p)))))

(defvar jde-parse-the-method-map nil
  "Map of methods defined in this buffer sorted by location.")
(make-variable-buffer-local 'jde-parse-the-method-map)


(defun jde-current-buffer-exact-name-match-p (tag)
  (and (tag-exact-match-p tag)
       (equal (buffer-file-name (window-buffer (selected-window))) 
	      (file-of-tag))))

(defun jde-etags-recognize-tags-table () ; see etags-recognize-tags-table
  (let ((recognized (etags-recognize-tags-table)))
    (if recognized 
	;; prefer exact match in current buffer to other files
	(setq find-tag-tag-order '(jde-current-buffer-exact-name-match-p
				   tag-exact-file-name-match-p
				   tag-exact-match-p
				   ))
      recognized)))

(provide 'jde-parse)

;; $Log: jde-parse.el,v $
;; Revision 1.34  2001/08/07 05:30:02  paulk
;; Addes jde-parse-select-qualified-class-name.
;;
;; Revision 1.33  2001/06/28 04:58:03  paulk
;; Fixed mode test.
;;
;; Revision 1.32  2001/06/12 07:18:55  paulk
;; Changed jde-parse-declared-type-of to return a qualified type.
;; Thanks to "Evan Easton" <evan@eeaston.com> .
;;
;; Revision 1.31  2001/05/31 05:14:39  paulk
;; Provide support for per-project caching of class data in the Beanshell. Thanks to Matt Conway.
;;
;; Revision 1.30  2001/03/16 04:49:08  paulk
;; Use major-mode instead of mode-name for testing which mode buffer is in. Thanks to Kevin Burton burton@relativity.yi.org.
;;
;; Revision 1.29  2001/01/05 07:14:35  paulk
;; Removed old version of jde-parse-get-class-at-point.
;;
;; Revision 1.28  2001/01/03 06:10:48  paulk
;; Fixes infinite recursion bug in jde-parse-update-after-parse when creating a new file.
;;
;; Revision 1.27  2000/12/18 05:22:46  paulk
;; *** empty log message ***
;;
;; Revision 1.26  2000/11/27 06:18:40  paulk
;; Miscellaneous bug fixes and minor enhancements.
;;
;; Revision 1.25  2000/10/25 04:44:23  paulk
;; Changed jde-auto-parse-disable-threshold to jde-auto-parse-max-buffer-size.
;;
;; Revision 1.24  2000/10/25 04:32:58  paulk
;; Moved code generated by the semantic bovinator to jde-java-grammar.el.
;;
;; Revision 1.23  2000/10/20 04:09:31  paulk
;; Now uses generalized version of classes menu shipped with
;; semantic. Thanks to David Ponce and Eric Ludlam.
;;
;; Revision 1.22  2000/10/08 12:55:39  paulk
;; *** empty log message ***
;;
;; Revision 1.21  2000/09/05 04:55:28  paulk
;; Bug fixes
;;
;; Revision 1.20  2000/08/31 05:31:15  paulk
;; * Now creates a binary tree, jde-parse-method-map, listing the
;; locations of all methods in the source buffer.
;;
;; * Now parses the source buffer 30 seconds after a change.
;;
;; Revision 1.19  2000/08/16 05:30:35  paulk
;; Set case-fold-search to nil to ensure case sensitivity when parsing buffer.
;;
;; Revision 1.18  2000/08/07 05:06:35  paulk
;; Fixes a couple of bugs in jde-parse-valid-declaration-at. Thanks to Lou Aloia <xlxa@rims.com> and Stephane <s.nicolas@videotron.ca> for the fixes.
;;
;; Revision 1.17  2000/07/28 06:27:46  paulk
;; Committing all modified files.
;;
;; Revision 1.16  2000/07/13 05:22:48  paulk
;; *** empty log message ***
;;
;; Revision 1.15  2000/06/29 02:33:42  paulk
;; Added sort option to Classes index menu. Thanks to David Ponce for this contribution.
;;
;; Revision 1.14  2000/06/22 03:40:16  paulk
;; Index menu now shows variable types and class definitions. Thanks to David Ponce for these enhancments. Changed the name of jde-enable-index-menu to jde-imenu-enable and jde-enable-full-method-signatures-index-menu to jde-imenu-include signature.
;;
;; Revision 1.13  2000/06/09 05:07:06  paulk
;; Classes index menu now shows full signatures of methods. Thanks to Ittay Freiman <ittay@vigiltech.com> for suggesting this enhancement and to David Ponce <david@dponce.com> for implementing it.
;;
;; Revision 1.12  2000/05/26 09:14:10  paulk
;; Updated grammar to handle argument variables with modifiers and array arguments.
;;
;; Revision 1.11  2000/05/16 04:08:55  paulk
;; Adds a Classes index menu to the Emacs menubar.
;;
;; Revision 1.10  2000/05/11 03:07:17  paulk
;; Updated bovinator grammar.
;;
;; Revision 1.9  2000/05/11 01:24:40  paulk
;; Added support for Eric Ludlam's semantic bovinator. Moved regular expression-based imenu indexer to this file.
;;
;; Revision 1.8  2000/03/16 05:18:11  paulk
;; Miscellaneous small bug fixes and enhancements.
;;
;; Revision 1.7  2000/03/08 03:47:02  paulk
;; Fixed regular expression in jde-parse-get-innermost-class-at-point to handle more cases. Thanks to Steve Haflich <smh@franz.com> for reporting the problem and providing a starting point for the fix.
;;
;; Revision 1.6  2000/02/16 04:40:33  paulk
;; Implemented Cygwin/XEmacs compatiblity fixes provided by Fred Hart
;; <cfhart@Z-TEL.com> in bsh-internal.
;;
;; Revision 1.5  2000/02/14 06:21:32  paulk
;; Fixed find innermost class regular expression.
;;
;; Revision 1.4  2000/02/09 05:18:10  paulk
;; Added methods for parsing symbol at point.
;;
;; Revision 1.3  2000/02/01 04:10:48  paulk
;; Fixed regular expression for classes to handle case where point is in
;; a constructor. Thanks to Francois Cogne <cogne@col.bsf.alcatel.fr>.
;;
;; Revision 1.2  1999/08/20 00:52:14  paulk
;; Added jde-parse-get-package-from-name function.
;;
;; Revision 1.1  1999/04/27 16:25:46  paulk
;; Initial revision
;;
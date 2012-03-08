;CONTENTS:
; TODO list
; overall appearance, color and font preferences
; misc
; tags preferences
; C language preferences
; font lock preferences
; P0stScr!pt printing
; ediff preferences
; per buffer preferences
; bat mode
; WoMan mode
; smtpmail, nnmail and gnus

;SECTION: TODO
; gnuwin-32 needs an option to ignore case when matching filenames
;     (or at least optionally treat all-upper names as all-lower)
; wouldn't find-tag-in-order be more efficient if it saved its place
;     on the order list?
;     use modified mouse button as synonym for find-tag?
;     why is exact-match-filename first?  is that one of the MOST 
general?
; adapt ps-print.el to font-lock (default + 8 font lock fonts)
;     figger out how to make it do underlines
;     find some more monospace fonts on the net(?)

;SECTION: overall appearance, color and font preferences

; see 8 bit characters as ISO8859 characters, not \nnn
(standard-display-european 1)

; select colors for improved retinal lifetime
; set these up so all frames get them
(defconst my-fg "DarkOrange1")
(defconst my-bg "Black")
(defconst my-mg "DarkRed")
(setq default-frame-alist
      `((font . "-*-Fixedsys-normal-r-*-*-17-102-*-*-c-*-*-ansi-") ;; 
19.34.4
	(foreground-color . ,my-fg)
	(background-color . ,my-bg)
	(cursor-color . ,my-fg)
	(width . 81) ; so 80th column doesn't wrap
	(height . 36) ; fixed in 19.34.4?
;	(height . 39) ; I want 36, but it comes up 3 short. . .
	,@default-frame-alist))
(set-default-font "-*-Fixedsys-normal-r-*-*-17-102-*-*-c-*-*-ansi-") 
;; 19.34.4
(set-frame-width (selected-frame) 81) ; width not picked up on 1st 
frame
(set-frame-height (selected-frame) 36) ; make it taller than default, 
too
(set-face-background 'modeline my-mg)
(set-face-foreground 'modeline my-fg)
(set-face-background 'region my-mg)
(set-face-background 'highlight my-fg)
(set-face-foreground 'highlight my-bg)

; can't really get bold and italic currently, but make the fonts
; distinctive anyway
(set-face-font 'bold "-*-Terminal-normal-r-*-*-18-108-*-*-c-*-*-oem-")
(set-face-font 'italic "-*-Lucida 
Console-normal-r-*-*-17-102-*-*-c-*-*-ansi-")

; come up iconified

(iconify-frame (selected-frame))

;SECTION: misc 

; let Emacs know about the languages for some locally used extensions

(autoload 'haskell-mode "haskell"
  "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell"
  "Major mode for editing literate Haskell scripts." t)
(setq auto-mode-alist
      (cons '("\\.lhl\\'" . literate-haskell-mode)
	    (cons '("\\.hl\\'" . haskell-mode)
		  (cons '("\\.l\\'" . c-mode)
			(cons '("\\.inc\\'" . asm-mode) auto-mode-alist)))))

; make goto-line easier to do

(global-set-key [?\M-\C-g] 'goto-line)

; highlight matching parens

(show-paren-mode 1)

; pick up the improved ftp from the NT Emacs site

(setq ange-ftp-ftp-program-name
      (expand-file-name "ftp.exe" exec-directory))

; pick up Gnus from its own directory tree

(setq load-path (cons (expand-file-name "e:/gnus-5.4.55/lisp") 
load-path))

(setq Info-default-directory-list
      (cons "e:/gnus-5.4.53/texi" Info-default-directory-list))

;SECTION: tags preferences
; note: this section need to be before the C language preference so 
that
; my-C-find-tag-default can piggy-back off of etags' find-tag-default

;; tweak tags library so that syntax is chosen from first file in tags 
file

(load-library "etags")

; if on a #include line, use the included file as the tag

(defun my-C-find-tag-default ()
  (or (save-excursion
	(beginning-of-line)
	(if (looking-at
	     "^[ \\t]*#[ \\t]*include[ \\t]*[<\"]\\([^>\"]+\\)[>\"]")
	    (match-string 1)))
      (find-tag-default)))

(setq librarys-recognize-table (car tags-table-format-hooks))
(setcar tags-table-format-hooks
	'(lambda ()
	   (if (funcall librarys-recognize-table)
	       (set-syntax-table
		(save-excursion
		  (goto-char (point-min))
		  (search-forward "\f\n")
		  (set-buffer (find-file-noselect (file-of-tag)))
		  (syntax-table)))
	     nil)))

(defun re-search-forward-sensitive (REGEXP &optional BOUND NOERROR 
COUNT)
  "wrapper for re-search-forward that locally binds case-fold-search 
to
nil to force a case-sensitive search."
  (let ((case-fold-search nil))
    (re-search-forward REGEXP BOUND NOERROR COUNT)))

(defun my-tags-search (arg regexp &optional file-list-form)
  "Search through all files listed in tags table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command 
\\[tags-loop-continue].

See documentation of variable `tags-file-name'.
With prefix arg, force case-sensitivity"
  (interactive "P\nsTags search (regexp): ")
  (let ((search-function (if arg 're-search-forward-sensitive
			   're-search-forward)))
    (if (and (equal regexp "")
	     (eq (car tags-loop-scan) search-function)
	     (null tags-loop-operate))
	;; Continue last tags-search as if by M-,.
	(tags-loop-continue nil)
      (setq tags-loop-scan
	    (list search-function (list 'quote regexp) nil t)
	    tags-loop-operate nil)
      (tags-loop-continue (or file-list-form t)))))

; almost always want to use the ARG tags table, might as well set it 
here

(setq tags-file-name "f:/arg/testbuild/tags")

(global-set-key "\C-x," 'my-tags-search)

;SECTION: C language preferences

; steal some code from c++ mode for // comments
(load-library "cc-mode")
(add-hook 'c-mode-hook
	  '(lambda ()
	     (setq comment-start "// "
		   comment-end ""
		   comment-multi-line nil
		   c-comment-start-regexp c-C++-comment-start-regexp
		   c-double-slash-is-comments-p t)
	     (c-setup-dual-comments c-mode-syntax-table)
	     (modify-syntax-entry ?_ "w")))

; set up the preferred Aspen brace conventions
; note: is might be nice to add a feature to c-hanging-colons-alist
; to automagically slap in a break; when a colon's typed on a case 
label

(setq c-hanging-braces-alist '((brace-list-open after)
				 (substatement-open after)
				 (block-close . c-snug-do-while)))

; seems like a buncha ARG's code was written with vi, assuming
; 3 space tab stops - set this up for c & c++ buffers
; Also, use bsd indentation style on each buffer, except 
knr-argdecl-intro
;     (for arg declarations between the function declaration and the 
first {)
;     is 0 and labels go to the left margin.
; and turn on electric C
; notes: users.c handles brace-list-close differently from most 
modules
;	 (a few have it mixed)
;	 acg_ipc.c (and some MBH code?) uses c-basic-offset 3
;	 (ssl.h doesn't use it consistently)

(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (put major-mode 'find-tag-default-function
		  'my-C-find-tag-default)
	     (setq tab-width 3)
	     (c-set-style "bsd")
	     (setq c-basic-offset 3)
	     (c-set-offset 'knr-argdecl-intro 0)
	     (c-set-offset 'label -1000)
	     (c-toggle-auto-state 1)))

;SECTION: font lock preferences

; (global-font-lock-mode t) ; actually, disable (for tags search) 
until
; 			   ; C-S-l or something is done?  (nevermind - etags
;			   ; can search a file without visiting it)

(load-library "font-lock")
(global-set-key [?\C-\S-l] 'font-lock-mode)
(setq font-lock-maximum-decoration t)
(setq font-lock-face-attributes
      ;; a tool for picking colors and fonts more fluently would be
      ;; welcome - could run it in frame B with code in frame A
      ;; to view changes in realtime (could drag and drop be faked
      ;; in emacs?)
      ;; in more detail:
      ;;	mung through emacs-19.nn/etc/rgb.txt
      ;;	group by R>G>B (6 groups), R=G>B (3), R>G=B (3) and
      ;;	R=G=B (1), then separate into 12 groups by hue
      ;;	(based on top 2 colors (or top 1, if one of R>G=B -
      ;;	(R=G=B in a separate group)), then sort by saturation
      ;;	(ratio of top 2 colors to 3rd) and brightness (total
      ;;	(of R,G and B).  Create a minimal frame for each hue
      ;;	(small font?), put each color name in a 3x8 box
      ;;	(color as foreground on top and bottom line, as
      ;;	background on middle line (reduplicate name if short))
      ;;	frame is whatever size (squarish) rectangle (saturation
      ;;	on one axis, brightness on the other) is needed
      ;;	for the # of colors of that hue.  Set up some frames
      ;;	for fonts, too, allow drag and drop onto face names.
      ;;	(might be another frame with each of the 12 hues +
      ;;	white/gray/black to pop a hue's frame to front)
      '((font-lock-comment-face "Orange")
	(font-lock-string-face "LightSalmon")
	(font-lock-keyword-face "SandyBrown")
	(font-lock-function-name-face "Salmon")
	(font-lock-variable-name-face "Sienna1")
	(font-lock-type-face "Goldenrod1")
	(font-lock-reference-face "Goldenrod1")))

(font-lock-make-faces) ; need to create the faces before fonts can be 
set

(set-face-font 'font-lock-function-name-face
	       "-*-Terminal-normal-r-*-*-18-108-*-*-c-*-*-oem-")
(set-face-font 'font-lock-comment-face
	       "-*-Courier New-normal-r-*-*-17-102-*-*-c-*-*-ansi-")
(set-face-font 'font-lock-string-face
	       "-*-Lucida Console-normal-r-*-*-17-102-*-*-c-*-*-ansi-")
(set-face-font 'font-lock-keyword-face
	       "-*-Courier New-normal-r-*-*-17-102-*-*-c-*-*-ansi-")
(set-face-font 'font-lock-type-face
	       "-*-Courier New-normal-r-*-*-17-102-*-*-c-*-*-ansi-")
(set-face-font 'font-lock-reference-face
	       "-*-Terminal-normal-r-*-*-18-108-*-*-c-*-*-oem-")

(put 'narrow-to-region 'disabled nil)


;SECTION: P0stScr!pt printing

; redo this command at Emacs time - shared printer isn't ready
; at startup time (apparently)

(shell-command "net use /persistent:no lpt1: \\\\robtp\\Nashua" nil)
(switch-to-buffer "*scratch*")
(delete-other-windows)

; put print command on F12
; Q: why doesn't Emacs pick up the Print Screen key?

(defun ps-print-to-local-HP-LaserJet ()
  "print to local HP LaserJet"
  (interactive)
  (ps-print-buffer "lpt1"))

(global-set-key [f12] 'ps-print-to-local-HP-LaserJet)

; try this twice - doesn't seem to take on the first try

(set-frame-height (selected-frame) 36) ; make it taller than default, 
too

;SECTION: ediff preferences

(setq ediff-ignore-similar-regions t)
(setq ediff-use-last-dir t)
(setq ediff-diff-options " -b ")

;SECTION: per buffer preferences

; in text mode, make tab stops correlate with tab width
; note: smarter than =text-mode would be to test for tab bound
;       to tab-to-next-tab-stop, but that's a later microproject

(defun make-tab-stops-like-I-like-em ()
  (if (and (/= tab-width 8) (eq major-mode 'text-mode))
      (let ((column (* tab-width (/ 131 tab-width)))
	    (stops ()))
	(while (>= column 0)
	  (setq stops (cons column stops)
		column (- column tab-width)))
	(setq tab-stop-list stops))))

(add-hook 'hack-local-variables-hook 'make-tab-stops-like-I-like-em)

;SECTION: bat mode

(setq auto-mode-alist
      (append
       auto-mode-alist
       (list (cons "\\.[bB][aA][tT]$" 'bat-mode))
       ;; For DOS init files
       (list (cons "CONFIG\\."   'bat-mode))
       (list (cons "AUTOEXEC\\." 'bat-mode))))

(autoload 'bat-mode "bat-mode"
  "DOS and WIndows BAT files" t)

;SECTION: WoMan mode

(autoload 'woman "woman"
          "Decode and browse a UN*X man page." t)
(autoload 'woman-find-file "woman"
          "Find, decode and browse a specific UN*X man-page file." t)

;SECTION: smtpmail, nnmail and gnus

(load-library "message")

(load "mime-setup")

(setq mail-host-address "AspenRes.Com")

(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-default-smtp-server "rose.aspenres.com")
(setq smtpmail-smtp-service "smtp")
(setq smtpmail-local-domain "aspenres.com")
(setq smtpmail-debug-info t)
(load-library "smtpmail")
(setq smtpmail-code-conv-from nil)
(setq user-full-name "Robert Praetorius")
(setq mail-self-blind t)

;; this one may not be for me (yet), will hafta look into it more
;(setq default-buffer-file-type t) ; to prevent ^M problems in GNUS

(setq gnus-select-method '(nnfolder "")
      gnus-secondary-select-methods nil
      nnmail-spool-file "po:robert"
      nnmail-pop-password-required t
      nnmail-treat-duplicates 'warn
      nnfolder-get-new-mail t
      gnus-message-archive-method
        `(nnfolder "archive"
	   (nnfolder-directory ,(nnheader-concat message-directory 
"archive"))
	   (nnfolder-active-file
	    ,(nnheader-concat message-directory "archive/active"))
	   (nnfolder-get-new-mail nil)
	   (nnfolder-inhibit-expiry t))
      gnus-message-archive-group
        '((if (message-news-p) "misc-news" "misc-mail"))
      gnus-outgoing-message-group "nnfolder+archive:sent-mail")

(setq nnmail-split-methods
   '(
;
; To screens
;
     ("mail.NTEmacs"
      
"^To:.*ntemacs-\\(us\\|hack\\)ers\\|^Cc:.*ntemacs-\\(us\\|hack\\)ers")
     ("mail.gnu-win32" "^To:.*gnu-win32\\|^Cc:.*gnu-win32")
     ("mail.misc" "")))

; SECTION: gnuserv

(require 'gnuserv)
(gnuserv-start)

; novice mode crud (arghh)

(put 'downcase-region 'disabled nil)

(put 'set-goal-column 'disabled nil)

(put 'narrow-to-page 'disabled nil)

(setq enable-recursive-minibuffers t)


(put 'eval-expression 'disabled nil)


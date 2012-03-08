;;; DO NOT MODIFY THIS FILE
(if (featurep 'pcmpl-auto) (error "Already loaded"))

;;;### (autoloads (pcomplete/cvs) "pcmpl-cvs" "pcmpl-cvs.el" (14491
;;;;;;  10356))
;;; Generated autoloads from pcmpl-cvs.el

(autoload (quote pcomplete/cvs) "pcmpl-cvs" "\
Completion rules for the `cvs' command." nil nil)

;;;***

;;;### (autoloads (pcomplete/tar pcomplete/make pcomplete/bzip2 pcomplete/gzip)
;;;;;;  "pcmpl-gnu" "pcmpl-gnu.el" (14532 2069))
;;; Generated autoloads from pcmpl-gnu.el

(autoload (quote pcomplete/gzip) "pcmpl-gnu" "\
Completion for `gzip'." nil nil)

(autoload (quote pcomplete/bzip2) "pcmpl-gnu" "\
Completion for `bzip2'." nil nil)

(autoload (quote pcomplete/make) "pcmpl-gnu" "\
Completion for GNU `make'." nil nil)

(autoload (quote pcomplete/tar) "pcmpl-gnu" "\
Completion for the GNU tar utility." nil nil)

(defalias (quote pcomplete/gdb) (quote pcomplete/xargs))

;;;***

;;;### (autoloads (pcomplete/mount pcomplete/umount pcomplete/kill)
;;;;;;  "pcmpl-linux" "pcmpl-linux.el" (14545 58790))
;;; Generated autoloads from pcmpl-linux.el

(autoload (quote pcomplete/kill) "pcmpl-linux" "\
Completion for GNU/Linux `kill', using /proc filesystem." nil nil)

(autoload (quote pcomplete/umount) "pcmpl-linux" "\
Completion for GNU/Linux `umount'." nil nil)

(autoload (quote pcomplete/mount) "pcmpl-linux" "\
Completion for GNU/Linux `mount'." nil nil)

;;;***

;;;### (autoloads (pcomplete/rpm) "pcmpl-rpm" "pcmpl-rpm.el" (14491
;;;;;;  10356))
;;; Generated autoloads from pcmpl-rpm.el

(autoload (quote pcomplete/rpm) "pcmpl-rpm" "\
Completion for RedHat's `rpm' command.
These rules were taken from the output of `rpm --help' on a RedHat 6.1
system.  They follow my interpretation of what followed, but since I'm
not a major rpm user/builder, please send me any corrections you find.
You can use \\[eshell-report-bug] to do so." nil nil)

;;;***

;;;### (autoloads (pcomplete/chgrp pcomplete/chown pcomplete/which
;;;;;;  pcomplete/xargs pcomplete/rm pcomplete/rmdir pcomplete/cd)
;;;;;;  "pcmpl-unix" "pcmpl-unix.el" (14491 10356))
;;; Generated autoloads from pcmpl-unix.el

(autoload (quote pcomplete/cd) "pcmpl-unix" "\
Completion for `cd'." nil nil)

(defalias (quote pcomplete/pushd) (quote pcomplete/cd))

(autoload (quote pcomplete/rmdir) "pcmpl-unix" "\
Completion for `rmdir'." nil nil)

(autoload (quote pcomplete/rm) "pcmpl-unix" "\
Completion for `rm'." nil nil)

(autoload (quote pcomplete/xargs) "pcmpl-unix" "\
Completion for `xargs'." nil nil)

(defalias (quote pcomplete/time) (quote pcomplete/xargs))

(autoload (quote pcomplete/which) "pcmpl-unix" "\
Completion for `which'." nil nil)

(autoload (quote pcomplete/chown) "pcmpl-unix" "\
Completion for the `chown' command." nil nil)

(autoload (quote pcomplete/chgrp) "pcmpl-unix" "\
Completion for the `chgrp' command." nil nil)

;;;***

;;;### (autoloads (pcomplete-shell-setup pcomplete-comint-setup pcomplete-list
;;;;;;  pcomplete-help pcomplete-expand pcomplete-continue pcomplete-expand-and-complete
;;;;;;  pcomplete-reverse pcomplete) "pcomplete" "pcomplete.el" (14600
;;;;;;  3394))
;;; Generated autoloads from pcomplete.el

(autoload (quote pcomplete) "pcomplete" "\
Support extensible programmable completion.
To use this function, just bind the TAB key to it, or add it to your
completion functions list (it should occur fairly early in the list)." t nil)

(autoload (quote pcomplete-reverse) "pcomplete" "\
If cycling completion is in use, cycle backwards." t nil)

(autoload (quote pcomplete-expand-and-complete) "pcomplete" "\
Expand the textual value of the current argument.
This will modify the current buffer." t nil)

(autoload (quote pcomplete-continue) "pcomplete" "\
Complete without reference to any cycling completions." t nil)

(autoload (quote pcomplete-expand) "pcomplete" "\
Expand the textual value of the current argument.
This will modify the current buffer." t nil)

(autoload (quote pcomplete-help) "pcomplete" "\
Display any help information relative to the current argument." t nil)

(autoload (quote pcomplete-list) "pcomplete" "\
Show the list of possible completions for the current argument." t nil)

(autoload (quote pcomplete-comint-setup) "pcomplete" "\
Setup a comint buffer to use pcomplete.
COMPLETEF-SYM should be the symbol where the
dynamic-complete-functions are kept.  For comint mode itself, this is
`comint-dynamic-complete-functions'." nil nil)

(autoload (quote pcomplete-shell-setup) "pcomplete" "\
Setup shell-mode to use pcomplete." nil nil)

;;;***

;;;### (autoloads nil "_pkg" "_pkg.el" (14367 22827))
;;; Generated autoloads from _pkg.el

(if (fboundp (quote package-provide)) (package-provide (quote pcomplete) :version 1.1 :type (quote regular)))

;;;***
(provide 'pcmpl-auto)

;;; pcmpl-maint --- init code for building pcomplete

;; Copyright (C) 1999, 2000 Free Sofware Foundation

;; This file is part of GNU Emacs.

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

;;; Code:

(setq load-path (cons "." load-path))

(require 'pcomplete)

(defun pcomplete-generate-autoloads ()
  (interactive)
  (require 'autoload)
  (setq generated-autoload-file
	(expand-file-name (car command-line-args-left)))
  (setq command-line-args-left (cdr command-line-args-left))
  (batch-update-autoloads))

;;; pcmpl-maint.el ends here

;;; jde-java-grammar.el
;; $Revision: 1.4 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 2000, 2001 Paul Kinnucan.

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

(require 'semantic-java)

(defun jde-parse-semantic-default-setup ()
  "Setup the semantic bovinator for the JDE."

  ;; Set up the buffer for semantic parsing of the Java language.
  ;; The following is automatically done in parent `java-mode-hook'.
  ;;(semantic-default-java-setup)

  ;; imenu & speedbar setup
  (jde-imenu-setup)

  ;; initial parsing of the current buffer
  (semantic-bovinate-toplevel)
  (jde-parse-update-after-parse)

  )

(provide 'jde-java-grammar)

;;; Change History:

;; $Log: jde-java-grammar.el,v $
;; Revision 1.4  2001/05/19 02:35:59  paulk
;; Updated to support semantic 1.4. Thanks to David Ponce.
;;
;; Revision 1.3  2001/02/21 05:55:38  paulk
;; Added require for semantic package.
;;
;; Revision 1.2  2001/01/25 05:38:39  paulk
;; Changed the definition of formal_parameter_list to improve performance (less
;; backtracking) when parsing parameters in method and constructor
;; declarations. Thanks to David Ponce.
;;
;; Revision 1.1  2000/10/25 04:30:31  paulk
;; Initial revision.
;;

;; End of jde-java-grammar.el
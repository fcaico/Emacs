# $Revision: 1.8 $ $Date: 2001/02/25 05:27:24 $ 
#
# This makefile compiles the JDE lisp files. It assumes
# that the JDE is installed in the same directory as
# that packages that it requires, e.g.,
# 
# root
#   emacs
#     site
#       jde
#       elib
#       semantic
#       speedbar
#       eieio
#
# If your installation is different, edit this
# makefile to reflect the actual locations of
# the required packages relative to the JDE lisp
# directory.
#
# Adapted by Paul Kinnucan from a Windows/NT batch script 
# created by Steve Rowley <sgr@alum.mit.edu>
#

EMACS = emacs

all:
	rm -f *.elc jde-compile-script-init
	echo "(add-to-list 'load-path \".\")" > jde-compile-script-init
	echo "(add-to-list 'load-path \"../../semantic\")" >> jde-compile-script-init
	echo "(add-to-list 'load-path \"../../speedbar\")"  >> jde-compile-script-init
	echo "(add-to-list 'load-path \"../../elib\")" >>jde-compile-script-init
	echo "(add-to-list 'load-path \"../../eieio\")" >>jde-compile-script-init
	echo "(setq debug-on-error t)" >> jde-compile-script-init
	$(EMACS) -batch -l jde-compile-script-init -f batch-byte-compile *.el
	rm -f jde-compile-script-init

# End of makefile
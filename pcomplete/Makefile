# Makefile for pcomplete lisp code

# Copyright (C) 1998-1999  John Wiegley <johnw@gnu.org>

# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.

# This file is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

prefix  = /usr/local
datadir = $(prefix)/share

# the directory where you install third-party emacs packges
lispdir = $(datadir)/emacs/site-lisp

# the directory where you install the info doc
infodir = $(prefix)/info
docdir = $(prefix)/doc

EMACS	= emacs
MAKEINFO= makeinfo
TEXI2DVI= texi2dvi
SHELL	= /bin/sh
DVIPS	= dvips
CP	= cp
MKDIR	= mkdir -p
ETAGS	= etags

######################################################################
###        No changes below this line should be necessary          ###
######################################################################

PACKAGE = pcomplete

# the directory where the .elc files will be installed
elcdir  = $(lispdir)/$(PACKAGE)
eldir   = $(elcdir)

MARGS   = --no-init-file --no-site-file -l ./pcmpl-maint.el -batch
BEMACS  = $(EMACS) $(MARGS)
ELC	= $(BEMACS) -f batch-byte-compile

ELFILES =		\
	pcomplete.el	\
	pcmpl-cvs.el	\
	pcmpl-gnu.el	\
	pcmpl-linux.el	\
	pcmpl-rpm.el	\
	pcmpl-unix.el

ELCFILES = $(ELFILES:.el=.elc)

TEXEXTS =  *.cps *.fns *.kys *.vr *.tp *.pg *.log *.aux *.toc *.cp *.ky *.fn

.SUFFIXES: .elc .el
.PHONY: elcfiles clean distclean default
.PHONY: install_elc install install_el

.el.elc:
	$(ELC) $<

######################################################################

default: pcmpl-auto.el elcfiles

elcfiles: Makefile $(ELCFILES)

install_elc: $(ELCFILES) pcmpl-auto.el _pkg.el
	$(MKDIR) $(elcdir)
	$(CP) $(ELCFILES) auto-autoloads.el pcmpl-auto.el _pkg.el $(elcdir)/

install_el:
	$(MKDIR) $(eldir)
	$(CP) $(ELFILES) $(eldir)/

install: install_elc install_el

clean:
	$(RM) *~ core .\#* $(TEXEXTS)

TAGS tags:
	$(ETAGS) $(ELFILES)

distclean: clean
	$(RM) *.elc TAGS
	$(RM) pcmpl-auto.el* auto-autoloads.el*

pcmpl-auto.el: $(ELFILES)
	echo ";;; DO NOT MODIFY THIS FILE" > pcmpl-auto.el
	echo "(if (featurep 'pcmpl-auto) (error \"Already loaded\"))" \
		>> pcmpl-auto.el
	$(BEMACS) -f pcomplete-generate-autoloads ./pcmpl-auto.el .
	echo "(provide 'pcmpl-auto)" >> pcmpl-auto.el
	ln pcmpl-auto.el auto-autoloads.el

TAG = $(shell echo v$(VERSION) | tr '.' '_')
ftpdir=/home/johnw/public_html/Emacs/packages

dist:
	cvs tag -F $(TAG) &&\
	cd /tmp &&\
	cvs export -r $(TAG) -d $(PACKAGE)-$(VERSION) $(PACKAGE) &&\
	cd $(PACKAGE)-$(VERSION) &&\
	make pcmpl-auto.el &&\
	chmod ugo+rX * &&\
	cd .. &&\
	tar cvzf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION) &&\
	zip -r $(PACKAGE)-$(VERSION).zip $(PACKAGE)-$(VERSION) &&\
	rm -rf $(PACKAGE)-$(VERSION)
	mv /tmp/$(PACKAGE)-$(VERSION).tar.gz $(ftpdir)/
	mv /tmp/$(PACKAGE)-$(VERSION).zip $(ftpdir)/
	ln -sf $(PACKAGE)-$(VERSION).tar.gz $(ftpdir)/$(PACKAGE).tar.gz
	ln -sf $(PACKAGE)-$(VERSION).zip $(ftpdir)/$(PACKAGE).zip

# Makefile for clustalw
.PHONY: 

.SILENT:

.SUFFIXES: 

TCLFILES = mafft.tcl pkgIndex.tcl
PKGVERSION = 1.1
PKGNAME = mafft


AR= ar
ARFLAGS = cr
RANLIB = ranlib

COMPILEDIR = ../compile
ARCHDIR=${COMPILEDIR}/lib_${ARCH}/tcl/${PKGNAME}${PKGVERSION}
SRCDIR=src
INCDIR=-Isrc
VPATH = src ${ARCHDIR}

#
# Rules
#
bins: ${ARCHDIR} 
win32bins: ${ARCHDIR} 
dynlibs:
staticlibs: 
win32staticlibs: 

distrib:
	echo "Copying ${PKGNAME} $(PKGVERSION) files to $(PLUGINDIR)"
	for localname in `find ../compile -name mafftEXE -o -name mafftEXE.exe`; do \
		pluginname=`echo $$localname | sed -e "s/..\\\/compile\\\/lib_//" `; \
		dir=`dirname $(PLUGINDIR)/$$pluginname`; \
		local=`echo $$localname | sed -e 's/mafftEXE//'`; \
		echo "cp -R $$local $$dir"; \
		cp -R $$local $$dir; \
		echo "cp $(TCLFILES) $$dir"; \
		cp $(TCLFILES) $$dir; \
	done; \

${ARCHDIR}:
	mkdir -p ${ARCHDIR}; \
	make -B -Cmafft-data/core -f Makefile install; \
	cp -Rf mafft-data/bin/mafft mafftEXE; \
	cp -Rf mafftEXE ${ARCHDIR}; \
	cp -Rf mafft-data ${ARCHDIR}; 

clean:
	rm -f mafftEXE
	cd mafft-data/core; make clean \

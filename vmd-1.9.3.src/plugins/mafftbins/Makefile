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
bins:
win32bins: 
dynlibs:
staticlibs: 
win32staticlibs: 

distrib: 
	echo "Copying ${PKGNAME} $(PKGVERSION) files to $(PLUGINDIR)/archname/tcl/${PKGNAME}${PKGVERSION}/"
	pwd
	cd bins/LINUX; rsync -av mafft scripts binaries $(PLUGINDIR)/LINUX/tcl/${PKGNAME}${PKGVERSION}/
	cd bins/LINUX; rsync -av mafft scripts binaries $(PLUGINDIR)/LINUXAMD64/tcl/${PKGNAME}${PKGVERSION}/
	cd bins/MACOSXX86; rsync -av mafft scripts binaries $(PLUGINDIR)/MACOSXX86/tcl/${PKGNAME}${PKGVERSION}/
	cd bins/mafft-win; rsync -av mafft.bat ms $(PLUGINDIR)/WIN32/tcl/${PKGNAME}${PKGVERSION}/


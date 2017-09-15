##############################################################################
# These are the programs to be installed (fxlib is the library).
##############################################################################
INSTALL_PROGS = fxp fxcanon fxcopy fxesis fxviz fxlib

##############################################################################
# These are the locations for executables, heap images and library files
##############################################################################
PREFIX     = /cygdrive/d/xml
FXP_BINDIR = ${PREFIX}/bin
FXP_LIBDIR = ${PREFIX}/fxp

##############################################################################
# The path where the SML-NJ binaries are located, and the name of the 
# SML-NJ executable with the Compilation manager built-in. If sml is in 
# your PATH at execution time, you fon't need the full path here.  
##############################################################################
SML_BINDIR = /cygdrive/d/smlnj-110.43/bin
SML_EXEC   = ${SML_BINDIR}/sml
##############################################################################
# No need to change this for SML-NJ 110.0.6. For earlier or working versions  
# 110.19 you might have to use the second or third line. This is the
# compilation manager function for making with a named description file. 
##############################################################################
#SML_MAKEDEF= val make = CM.make'
SML_MAKEDEF= val make = CM.make
#SML_MAKEDEF= fun make x = CM.make'{force_relink=true, group=x}

##############################################################################
# These should be fine on most unix machines
##############################################################################
SED       = sed
RM        = rm -f
RMDIR     = rmdir
COPY      = cp -f
CHMOD     = chmod
FIND      = find
#buggy in cygwin
#MKDIRHIER = mkdirhier 
MKDIRHIER = mkdir -p

##############################################################################
# nothing to change below this line
##############################################################################
SRC         = src
DOC         = doc
FXLIB_PRUNE = \( -name CM -o -name CVS -o -name Apps \)

all: fxp.sh images

arch.os:
	if test -s ${SML_BINDIR}/.arch-n-opsys; then\
            ${SML_BINDIR}/.arch-n-opsys | \
            ${SED} -e 's/^.*HEAP_SUFFIX=\(.*\)$$/\1/' > .arch-opsys;\
        else \
            echo "ARCH=x86; OPSYS=win32; HEAP_SUFFIX=x86-win32" | \
            ${SED} -e 's/^.*HEAP_SUFFIX=\(.*\)$$/\1/' > .arch-opsys;\
        fi

fxp.sh: Makefile arch.os
	${RM} fxp.sh
	echo "#!/bin/sh -f" > fxp.sh
	echo >> fxp.sh
	echo "SML_BINDIR=${SML_BINDIR}" >> fxp.sh
	echo "FXP_LIBDIR=${FXP_LIBDIR}" >> fxp.sh
	cat fxp.sh.in >> fxp.sh

image.prog:
	@echo "Creating the ${PROG_NAME} heap image..."
	echo "${SML_MAKEDEF}; make \"${SRC}/${PROG_CM}\"; \
	      SMLofNJ.exportFn(\"${SRC}/_${PROG_NAME}\",${PROG_FUN})" | ${SML_EXEC}

image.fxlib:
image.fxp:
	@make image.prog PROG_NAME=fxp PROG_CM=Apps/Null/null.cm PROG_FUN=Null.null
image.fxcanon:
	@make image.prog PROG_NAME=fxcanon PROG_CM=Apps/Canon/canon.cm PROG_FUN=Canon.canon
image.fxcopy:
	@make image.prog PROG_NAME=fxcopy PROG_CM=Apps/Copy/copy.cm PROG_FUN=Copy.copy
image.fxesis:
	@make image.prog PROG_NAME=fxesis PROG_CM=Apps/Esis/esis.cm PROG_FUN=Esis.esis
image.fxviz:
	@make image.prog PROG_NAME=fxviz PROG_CM=Apps/Viz/viz.cm PROG_FUN=Viz.viz

images:
	for prog in ${INSTALL_PROGS}; do \
	    make image.$${prog}; \
	done

inst.dirs:
	test -d ${FXP_BINDIR} || ${MKDIRHIER} ${FXP_BINDIR}	
	test -d ${FXP_LIBDIR} || ${MKDIRHIER} ${FXP_LIBDIR}	

inst.prog: inst.dirs fxp.sh arch.os
	${RM} ${FXP_BINDIR}/${PROG_NAME} ${FXP_BINDIR}/fxp.sh \
	      ${FXP_LIBDIR}/_${PROG_NAME}.`cat .arch-opsys`
	${COPY} fxp.sh ${FXP_BINDIR}
	${CHMOD} 755 ${FXP_BINDIR}/fxp.sh
	ln -s fxp.sh ${FXP_BINDIR}/${PROG_NAME}
	${COPY} ${SRC}/_${PROG_NAME}.`cat .arch-opsys` ${FXP_LIBDIR}
	${CHMOD} 644 ${FXP_LIBDIR}/_${PROG_NAME}.`cat .arch-opsys`

inst.fxp:
	@make inst.prog PROG_NAME=fxp PROG_CM=Apps/Null/null.cm PROG_FUN=Null.null
inst.fxcanon:
	@make inst.prog PROG_NAME=fxcanon PROG_CM=Apps/Canon/canon.cm PROG_FUN=Canon.canon
inst.fxcopy:
	@make inst.prog PROG_NAME=fxcopy PROG_CM=Apps/Copy/copy.cm PROG_FUN=Copy.copy
inst.fxesis:
	@make inst.prog PROG_NAME=fxesis PROG_CM=Apps/Esis/esis.cm PROG_FUN=Esis.esis
inst.fxviz:
	@make inst.prog PROG_NAME=fxviz PROG_CM=Apps/Viz/viz.cm PROG_FUN=Viz.viz

inst.fxlib:
	for dir in `${FIND} ${SRC} ${FXLIB_PRUNE} -prune -o -type d -print`; do \
	    ${MKDIRHIER} ${FXP_LIBDIR}/$${dir}; \
	done
	for file in `${FIND} ${SRC} ${FXLIB_PRUNE} -prune -o -name '*.sml' -print`; do \
	    ${COPY} $${file} ${FXP_LIBDIR}/$${file}; \
	done
	${COPY} ${SRC}/fxlib.cm ${FXP_LIBDIR}/${SRC}/fxlib.cm
	rm -f ${FXP_LIBDIR}/fxlib.cm
	echo Group is > ${FXP_LIBDIR}/fxlib.cm
	echo "  "${SRC}/fxlib.cm >> ${FXP_LIBDIR}/fxlib.cm
	${COPY} -r ${DOC} ${FXP_LIBDIR}

install:
	for prog in ${INSTALL_PROGS}; do \
	    make inst.$${prog}; \
	done

uninstall: arch.os
	-for prog in ${INSTALL_PROGRAMS}; do \
	  if [ "$${prog}" == "fxlib" ]; then \
	    ${RM} -r ${FXP_LIBDIR/src}; \
	  else \
	    ${RM} ${FXP_BINDIR}/$${prog}; \
	    ${RM} ${FXP_LIBDIR}/_$${prog}.`cat .arch-opsys`; \
	  fi; \
	 done
	-${RM} ${FXP_BINDIR}/fxp.sh
	-${RMDIR} ${FXP_BINDIR} ${FXP_LIBDIR} 

clean:
	-${RM} -f ${SRC}/_fx.* fxp.sh .arch-opsys
	-find ${SRC} -type d -name CM -print | xargs ${RM} -r 

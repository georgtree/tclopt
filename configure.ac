#!/bin/bash -norc
dnl	This file is an input file used by the GNU "autoconf" program to
dnl	generate the file "configure", which is run during Tcl installation
dnl	to configure the system for the local environment.

AC_INIT([tclopt],[0.21])
TEA_INIT()
AC_CONFIG_AUX_DIR(tclconfig)
TEA_PATH_TCLCONFIG
TEA_LOAD_TCLCONFIG
TEA_PREFIX
TEA_SETUP_COMPILER
TEA_ADD_SOURCES([mpfit.c random_gen.c tclopt_wrap.c])
TEA_ADD_HEADERS([generic/mpfit.h generic/random_gen.h])
TEA_ADD_INCLUDES([])
TEA_ADD_LIBS([])
TEA_ADD_CFLAGS([])
TEA_ADD_STUB_SOURCES([])
TEA_ADD_TCL_SOURCES([tclopt.tcl])
TEA_PUBLIC_TCL_HEADERS
TEA_ENABLE_THREADS
TEA_ENABLE_SHARED
TEA_CONFIG_CFLAGS
TEA_ENABLE_SYMBOLS
TEA_MAKE_LIB
TEA_PROG_TCLSH
AC_SUBST(TCLLIBPATH)
AC_CONFIG_FILES([Makefile swig_gen.sh pkgIndex.tcl])
AC_OUTPUT

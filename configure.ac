AC_INIT(verifica, 1.13, [cmarsigli@arpa.emr.it])
AC_CONFIG_SRCDIR([configure.ac])
AM_INIT_AUTOMAKE([foreign])
AC_LANG([C])


profileddir='${sysconfdir}/profile.d'
AC_SUBST(profileddir)


AC_PROG_FC([pgf90 ifort gfortran])

F77=$FC
FFLAGS=$FCFLAGS


AC_CHECK_LIB([m], [sin], , AC_MSG_ERROR([m library not found]), )
AC_CHECK_LIB([popt], [poptAddAlias], , AC_MSG_ERROR([popt library not found]), )
AC_CHECK_LIB([odbc], [SQLDisconnect], , AC_MSG_ERROR([odbc library not found]), )
AC_CHECK_LIB([dballe], [dba_error_get_code], , AC_MSG_ERROR([dballe library not found]), )
AC_CHECK_LIB([cnf], [cnfCreat], , AC_MSG_ERROR([cnf library not found]), )

AC_LANG_PUSH(Fortran)


AC_CHECK_LIB([dballef], [idba_scopa], , AC_MSG_ERROR([dballef library not found]), )
AC_CHECK_LIB([mega], [jeladata5], , AC_MSG_ERROR([mega library not found]), )
AC_CHECK_LIB([hibu], [ngetpoint], , AC_MSG_ERROR([hibu library not found]), )
AC_CHECK_LIB([emos], [gribex], , AC_MSG_ERROR([emos library not found]), )
AC_CHECK_LIB([grib_util], [findgrib], , AC_MSG_ERROR([gributil library not found]), )

AC_LANG_POP

##LIBS="-lm  -lpopt -lodbc -lcnf  -ldballe    $LIBS"


AC_CONFIG_FILES([
Makefile
src/Makefile
])
AC_OUTPUT

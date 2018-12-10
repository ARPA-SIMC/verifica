AC_INIT(verifica, 4.3, [cmarsigli@arpae.it])
AC_CONFIG_SRCDIR([configure.ac])
AM_INIT_AUTOMAKE([foreign])
AC_LANG([C])


profileddir='${sysconfdir}/profile.d'
AC_SUBST(profileddir)


AC_PROG_FC([gfortran pgf90 ifort])

F77=$FC
FFLAGS=$FCFLAGS


AC_CHECK_LIB([m], [sin], , AC_MSG_ERROR([m library not found]), )

AC_LANG_PUSH(Fortran)

LIBDBALLEF_DEFS

AC_CHECK_LIB([mega], [jeladata5], , AC_MSG_ERROR([mega library not found]), )
AC_CHECK_LIB([hibu], [ngetpoint], , AC_MSG_ERROR([hibu library not found]), )
AC_CHECK_LIB([emos], [gribex], , AC_MSG_ERROR([emos library not found]), )
AC_CHECK_LIB([grib_util], [findgrib], , AC_MSG_ERROR([gributil library not found]), )

grib_api_flavor=
AC_CHECK_LIB(
	     [eccodes_f90],
	     [grib_f_open_file],
	     [
	      AC_MSG_RESULT([eccodes code included])
	      AC_DEFINE(HAVE_LIBGRIBAPI, 1, [Enable gribapi code])
	      GRIBAPI_LIBS="-leccodes_f90 -leccodes -ljasper"
	      AC_SUBST(GRIBAPI_LIBS)
	      GRIBAPI_PKGCONFIG="eccodes_f90"
	      AC_SUBST(GRIBAPI_PKGCONFIG)
	      grib_api_flavor=eccodes
	     ],
	     [AC_MSG_RESULT([eccodes library not found, trying with legacy grib_api])],
	     [-leccodes -ljasper]
	    )
if test "x$grib_api_flavor" = "x"; then
AC_CHECK_LIB(
	     [grib_api_f90],
	     [grib_f_open_file],
	     [
	      AC_MSG_RESULT([gribapi code included])
	      AC_DEFINE(HAVE_LIBGRIBAPI, 1, [Enable gribapi code])
	      GRIBAPI_LIBS="-lgrib_api_f90 -lgrib_api -ljasper"
	      AC_SUBST(GRIBAPI_LIBS)
	      GRIBAPI_PKGCONFIG="grib_api_f90"
	      AC_SUBST(GRIBAPI_PKGCONFIG)
	      grib_api_flavor=grib_api
	     ],
	     [AC_MSG_ERROR([eccodes and grib-api library not found])],
	     [-lgrib_api -ljasper]
	    )
fi

AC_LANG_POP

LIBS="${LIBDBALLEF_LIBS} $LIBS -lsim_volgrid6d -lsim_base ${GRIBAPI_LIBS}"

AC_PROG_LIBTOOL

AC_CONFIG_FILES([
Makefile
src/Makefile
])
AC_OUTPUT

SUBDIRS = src

dist_noinst_SCRIPTS = verifica.sh.in
#EXTRA_DIST = verifica.sh.in
profiled_SCRIPTS = verifica.sh
CLEANFILES = $(profiled_SCRIPTS)

edit = sed \
	-e 's,@pkgdatadir\@,$(pkgdatadir),g' \
	-e 's,@prefix\@,$(prefix),g'

verifica.sh: Makefile verifica.sh.in
	rm -f verifica.sh verifica.sh.tmp
	$(edit) verifica.sh.in >verifica.sh.tmp
	chmod +x verifica.sh.tmp
	mv verifica.sh.tmp verifica.sh




dist_pkgdata_DATA = share/analisi.nml.template \
share/areaoss.nml.template \
share/corrispondenze_parametri.txt \
share/expansion_ver.template \
share/getgribdat_ver \
share/griBlocale.txt \
share/lista_ens.nml.template \
share/lista.nml.template \
share/lsm_cleps.grib \
share/orog_cleps.grib \
share/lsm_lmdet.grib \
share/orog_lmdet.grib \
share/lsm_lmsmr2031.grib \
share/orog_lmsmr2031.grib \
share/odbc.nml.template \
share/param.nml.template \
share/profile_cleps \
share/profile_clepsvis \
share/profile_eps51 \
share/profile_epsrm \
share/profile_epsse \
share/profile_lmsmr2 \
share/profile_lmsmr2031 \
share/profile_lmsmr2032 \
share/profile_lmsmr3032 \
share/profile_lmsmr3033 \
share/profile_lmsmr3131 \
share/profile_lmsmr3133 \
share/profile_medit \
share/profile_necmwfeu \
share/profilestra.template \
share/profile_swanad100 \
share/profile_swanad101 \
share/profile_swann1100 \
share/profile_swann1101 \
share/profilever.template \
share/profile_wamad002 \
share/profile_wamad003 \
share/profile_wammed \
share/spiega_GRIBARCH.txt \
share/spiegapar.txt \
share/verana.nml.template

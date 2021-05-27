lib_LTLIBRARIES = libverifica.la
libverifica_la_SOURCES = common_namelists.f90 scores_prob_util_dballe.f90 scores_util_dballe.f90 util_dballe.f90

bin_PROGRAMS = ver_lettura \
               ver_scores_dballe       ver_scores_prob_dballe \
               ver_scores_prob_dballe_resampling

ver_lettura_SOURCES = ver_lettura.f90
ver_lettura_LDADD = libverifica.la
ver_scores_dballe_SOURCES = ver_scores_dballe.f90
ver_scores_dballe_LDADD = libverifica.la
ver_scores_prob_dballe_SOURCES = ver_scores_prob_dballe.f90
ver_scores_prob_dballe_LDADD = libverifica.la
ver_scores_prob_dballe_resampling_SOURCES = ver_scores_prob_dballe_resampling.f90
ver_scores_prob_dballe_resampling_LDADD = libverifica.la

ver_lettura.$(OBJEXT): util_dballe.$(OBJEXT)
ver_scores_dballe.$(OBJEXT): util_dballe.$(OBJEXT) common_namelists.$(OBJEXT)
ver_scores_prob_dballe.$(OBJEXT): util_dballe.$(OBJEXT) scores_prob_util_dballe.$(OBJEXT) common_namelists.$(OBJEXT)
ver_scores_prob_dballe_resampling.$(OBJEXT): util_dballe.$(OBJEXT) scores_prob_util_dballe.$(OBJEXT) common_namelists.$(OBJEXT)

scores_prob_util_dballe.mod: scores_prob_util_dballe.$(OBJEXT)
util_dballe.mod: util_dballe.$(OBJEXT)
common_namelists.mod: common_namelists.$(OBJEXT)

dist_bin_SCRIPTS = ver_prepara_nml.sh \
 ver_scores_prob.sh \
 ver_scores.sh

nodist_include_HEADERS = scores_prob_util_dballe.mod util_dballe.mod common_namelists.mod

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod

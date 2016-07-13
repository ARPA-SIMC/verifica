lib_LTLIBRARIES = libverifica.la
libverifica_la_SOURCES = common_namelists.f90 scores_prob_util_dballe.f90  scores_util_dballe.f90 util_dballe.f90

bin_PROGRAMS = ver_boxfix_dballe    ver_calcola_descrittore ver_calcola_td \
               ver_crea_obsmask     ver_cumula_preci        ver_ffdd \
               ver_interpola_dballe ver_leggidati_ana       ver_leggidati_euro \
               ver_leggidati_mare   ver_leggidati_regio     ver_lettura \
               ver_pulisci          ver_scores_dballe       ver_scores_prob_dballe \
               ver_scores_prob_dballe_resampling

ver_boxfix_dballe_SOURCES = ver_boxfix_dballe.f90
ver_boxfix_dballe_LDADD = libverifica.la
ver_calcola_descrittore_SOURCES = ver_calcola_descrittore.f90
ver_calcola_descrittore_LDADD = libverifica.la
ver_calcola_td_SOURCES = ver_calcola_td.f90
ver_calcola_td_LDADD = libverifica.la
ver_crea_obsmask_SOURCES = ver_crea_obsmask.f90
ver_crea_obsmask_LDADD = libverifica.la
ver_cumula_preci_SOURCES = ver_cumula_preci.f90
ver_cumula_preci_LDADD = libverifica.la
ver_ffdd_SOURCES = ver_ffdd.f90
ver_ffdd_LDADD = libverifica.la
ver_interpola_dballe_SOURCES = ver_interpola_dballe.f90
ver_interpola_dballe_LDADD = libverifica.la
ver_leggidati_ana_SOURCES = ver_leggidati_ana.f90
ver_leggidati_ana_LDADD = libverifica.la
ver_leggidati_euro_SOURCES = ver_leggidati_euro.f90
ver_leggidati_euro_LDADD = libverifica.la
ver_leggidati_mare_SOURCES = ver_leggidati_mare.f90
ver_leggidati_mare_LDADD = libverifica.la
ver_leggidati_regio_SOURCES = ver_leggidati_regio.f90
ver_leggidati_regio_LDADD = libverifica.la
ver_lettura_SOURCES = ver_lettura.f90
ver_lettura_LDADD = libverifica.la
ver_pulisci_SOURCES = ver_pulisci.f90
ver_pulisci_LDADD = libverifica.la
ver_scores_dballe_SOURCES = ver_scores_dballe.f90
ver_scores_dballe_LDADD = libverifica.la
ver_scores_prob_dballe_SOURCES = ver_scores_prob_dballe.f90
ver_scores_prob_dballe_LDADD = libverifica.la
ver_scores_prob_dballe_resampling_SOURCES = ver_scores_prob_dballe_resampling.f90
ver_scores_prob_dballe_resampling_LDADD = libverifica.la

dist_bin_SCRIPTS = ver_calcola_descrittore.sh \
 ver_crea_obsmask.sh \
 ver_cumula_preci.sh \
 ver_estrai_calcola.sh \
 ver_prepara_nml.sh \
 ver_scores_prob.sh \
 ver_scores.sh \
 ver_scrivi_ana.sh \
 ver_scrivi_oss.sh \
 ver_scrivi_euro.sh \
 ver_scrivi_prev.sh \
 ver_pulisci.sh

nodist_include_HEADERS = scores_prob_util_dballe.mod util_dballe.mod common_namelists.mod

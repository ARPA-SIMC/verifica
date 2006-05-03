#!/bin/ksh

### VERIFICA - ver_scores.sh
### script per l'esecuzione del calcolo degli scores deterministici
### usa solo osservati e previsti che si trovano sul database DbAlle
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

if [ "$1" = -h ] ; then
  echo "uso: $0 [-b]"
  echo "calcola gli scores deterministici"
  echo " -b  lancia in modalita' batch"
  exit 1
fi

BATCH=0
if [ "$1" = -b ] ; then
  BATCH=1
fi

if [ $BATCH -eq 0 ] ; then
  if [ -z $EDITOR ] ; then
    echo "ERRORE! Devi exportare la variabile EDITOR" 1>&2
    exit 1
  fi
fi

if [ ! -f ./profilestra ] ; then
  echo "file profilestra mancante, lanciare ver_prepara_naml.sh" 1>&2
  exit 1
fi

if [ ! -f ./profilever ] ; then
  cp $VERSHARE/profilever.template ./profilever
fi
[ $BATCH -eq 0 ] && $EDITOR profilever

. profilestra
. profilever

if [ ! -f ./odbc.nml ] ; then
  cp $VERSHARE/odbc.nml.template ./odbc.nml
fi
[ $BATCH -eq 0 ] && $EDITOR odbc.nml

if [ $deterministico = 'T' ] ; then
  if [ ! -f ./lista.nml ] ; then
    cp $VERSHARE/lista.nml.template ./lista.nml
  fi
  [ $BATCH -eq 0 ] && $EDITOR lista.nml
fi
if [ $probabilistico = 'T' ] ; then
  if [ ! -f ./lista_ens.nml ] ; then
    cp $VERSHARE/lista_ens.nml.template ./lista_ens.nml
  fi
  [ $BATCH -eq 0 ] && $EDITOR lista_ens.nml
fi

if [ $interpola = 'T' ] ; then
  itipo=1
elif [ $boxfix = 'T' ] ; then
  itipo=2
fi

echo ' $stat' > stat.nml
echo '   model="'$mod'",' >> stat.nml
echo '   itipo='$itipo',' >> stat.nml
echo '   iana='$iana',' >> stat.nml
echo '   imet='$imet',' >> stat.nml
echo '   imod='$imod',' >> stat.nml
echo '   ls='$ls',' >> stat.nml
echo '   ruota='$ruota',' >> stat.nml
echo '   nminobs='$nminobs',' >> stat.nml
echo '   media='$media',' >> stat.nml
echo '   massimo='$massimo',' >> stat.nml
echo '   prob='$prob',' >> stat.nml
echo '   distr='$distr',' >> stat.nml
echo '   dxb='$dxb',' >> stat.nml
echo '   dyb='$dyb',' >> stat.nml
echo '   diffh='$diffh',' >> stat.nml
echo '   diffmax='$diffmax',' >> stat.nml
echo '   thr='$thr',' >> stat.nml
echo '   perc='$perc',' >> stat.nml
echo ' $end' >> stat.nml

if [ $deterministico = 'T' ] ; then

  ver_scores_dballe

  STATUS=$?
  if [ $STATUS -ne 0 ] ; then
    echo ' ver_scores_dballe terminato con errore= '$STATUS 1>&2
  fi

  mv fort.55 scatter_plot.dat
  mv fort.23 costloss_det.dat

fi

if [ $probabilistico = 'T' ] ; then

  ver_scores_prob_dballe

  STATUS=$?
  if [ $STATUS -ne 0 ] ; then
    echo ' ver_scores_prob_dballe terminato con errore= '$STATUS 1>&2
  fi

  mv fort.14 brier.dat
  mv fort.12 roc.dat
  mv fort.23 costloss.dat
  mv fort.15 outliers.dat
  mv fort.16 outliers_isto.dat
  mv fort.19 detscores.dat
  mv fort.24 reldiag.dat

fi

#!/bin/bash

### VERIFICA - ver_scores.sh
### script per l'esecuzione del calcolo degli scores deterministici
### usa solo osservati e previsti che si trovano sul database DbAlle
### autore: Chiara Marsigli
# ---------------------------------------------------------------------------

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

if [ ! -f ./profile_verifica ] ; then
  echo "file profile_verifica mancante, lanciare ver_prepara_nml.sh" 1>&2
  exit 1
fi

if [ ! -f ./profile_scores ] ; then
  cp $VERSHARE/profile_scores.template ./profile_scores
fi
[ $BATCH -eq 0 ] && $EDITOR profile_scores

. profile_verifica
. profile_scores

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
echo '   model="'$model'",' >> stat.nml
echo '   itipo='$itipo',' >> stat.nml
echo '   iana='$iana',' >> stat.nml
echo '   reportobs='$reportobs',' >> stat.nml
echo '   reportpre='$reportpre',' >> stat.nml
echo '   analisi='$analisi',' >> stat.nml
echo '   diffh='$diffh',' >> stat.nml
echo '   diffmax='$diffmax',' >> stat.nml
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
  mv fort.16 taldiag.dat
  mv fort.22 detscores.dat
  mv fort.24 reldiag.dat

fi

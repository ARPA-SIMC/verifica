#!/bin/bash

### VERIFICA - ver_calcola_descrittore.sh
### script per il calcolo del descrittore del modello per repinfo.csv
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

if [ "$1" = -h ] ; then
  echo "uso: $0 [-b]"
  echo "calcola il descrittore"
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

if [ ! -f ./caldescr ] ; then
  cp $VERSHARE/caldescr.template ./caldescr
fi
[ $BATCH -eq 0 ] && $EDITOR caldescr

. caldescr

echo ' $ds' > caldescr.nml
echo '   model='$model',' >> caldescr.nml
echo '   itipo='$itipo',' >> caldescr.nml
echo '   imod='$imod',' >> caldescr.nml
echo '   ls='$ls',' >> caldescr.nml
echo '   media='$media',' >> caldescr.nml
echo '   massimo='$massimo',' >> caldescr.nml
echo '   prob='$prob',' >> caldescr.nml
echo '   distr='$distr',' >> caldescr.nml
echo '   dxb='$dxb',' >> caldescr.nml
echo '   dyb='$dyb',' >> caldescr.nml
echo ' $end' >> caldescr.nml

ver_calcola_descrittore

STATUS=$?
if [ $STATUS -ne 0 ] ; then
  echo ' ver_calcola_descrittore terminato con errore= '$STATUS 1>&2
fi

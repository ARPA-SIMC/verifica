#!/bin/bash

### VERIFICA - ver_calcola_descrittore.sh
### script per il calcolo del descrittore del modello per repinfo.csv
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

if [ -z $EDITOR ] ; then
  echo "Errore"
  echo "Devi exportare la variabile EDITOR"
  exit 1
fi

if [ ! -f ./caldescr ] ; then
  cp $VERSHARE/caldescr.template ./caldescr
fi
$EDITOR caldescr

. caldescr

echo ' $ds' > caldescr.nml
echo '   model='$model',' >> caldescr.nml
echo '   itipo=1,' >> caldescr.nml
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

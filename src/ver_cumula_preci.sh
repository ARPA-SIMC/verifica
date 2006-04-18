#!/bin/bash

### VERIFICA - ver_cumula_preci.sh
### script per la cumulazione su intervallo scelto dall'utente
### delle precipitazioni osservate presenti all'interno del database
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

if [ "$1" = -h ] ; then
  echo "uso: $0 [-b]"
  echo "cumula le preci presenti sul database"
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

if [ ! -f ./cumula.nml ] ; then
  cp $VERSHARE/cumula.nml.template ./cumula.nml
fi
[ $BATCH -eq 0 ] && $EDITOR cumula.nml

if [ ! -f ./odbc.nml ] ; then
  cp $VERSHARE/odbc.nml.template ./odbc.nml
fi
[ $BATCH -eq 0 ] && $EDITOR odbc.nml

ver_cumula_preci

STATUS=$?
if [ $STATUS -ne 0 ] ; then
  echo ' ver_cumula_preci terminato con errore= ',$STATUS 1>&2
fi

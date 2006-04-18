#!/bin/bash

### VERIFICA - ver_pulisci.bash
### script per lo svuotamento del dababase DbAlle
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

if [ "$1" = -h ] ; then
  echo "uso: $0 [-b]"
  echo "svuota il database"
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

if [ ! -f ./repinfo.csv ] ; then
  cp /etc/dballe/repinfo.csv ./repinfo.csv
fi
[ $BATCH -eq 0 ] && $EDITOR repinfo.csv

if [ ! -f ./odbc.nml ] ; then
  cp $VERSHARE/odbc.nml.template ./odbc.nml
fi
[ $BATCH -eq 0 ] && $EDITOR odbc.nml

ver_pulisci

STATUS=$?
if [ $STATUS -ne 0 ] ; then
  echo ' ver_pulisci terminato con errore= ',$STATUS 1>&2
fi

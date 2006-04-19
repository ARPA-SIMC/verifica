#!/bin/bash

### VERIFICA - ver_scrivi_euro.sh
### script per la lettura da file (formato fissato) e scrittura su database 
### delle osservazioni fornite dai membri di COSMO
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

if [ "$1" = -h ] ; then
  echo "uso: $0 [-b]"
  echo "carica osservati COSMO sul database"
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

if [ ! -f ./griBlocale.txt ] ; then
  cp $VERSHARE/griBlocale.txt .
fi

if [ ! -f ./repinfo.csv ] ; then
  cp /etc/dballe/repinfo.csv ./repinfo.csv
fi
[ $BATCH -eq 0 ] && $EDITOR repinfo.csv

if [ ! -f ./euro.nml ] ; then
  cp $VERSHARE/euro.nml.template ./euro.nml
fi
[ $BATCH -eq 0 ] && $EDITOR euro.nml

if [ ! -f ./odbc.nml ] ; then
  cp $VERSHARE/odbc.nml.template ./odbc.nml
fi
[ $BATCH -eq 0 ] && $EDITOR odbc.nml

ver_leggidati_euro

STATUS=$?
if [ $STATUS -ne 0 ] ; then
  echo ' ver_leggidati_euro terminato con errore= ',$STATUS 1>&2
fi
#!/bin/bash

### VERIFICA - ver_scrivi_ana.sh
### script per la lettura da file grib e scrittura su database
### di campi analizzati
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

if [ "$1" = -h ] ; then
  echo "uso: $0 [-b]"
  echo "carica campi in formato GRIB sul database"
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

if [ ! -f ./analisi.nml ] ; then
  cp $VERSHARE/analisi.nml.template ./analisi.nml
fi
[ $BATCH -eq 0 ] && $EDITOR analisi.nml

if [ ! -f ./areaoss.nml ] ; then
  cp $VERSHARE/areaoss.nml.template ./areaoss.nml
fi
[ $BATCH -eq 0 ] && $EDITOR areaoss.nml

if [ ! -f ./odbc.nml ] ; then
  cp $VERSHARE/odbc.nml.template ./odbc.nml
fi
[ $BATCH -eq 0 ] && $EDITOR odbc.nml

ver_leggidati_ana

STATUS=$?
if [ $STATUS -ne 0 ] ; then
  echo ' ver_leggidati_ana terminato con errore= '$STATUS 1>&2
fi

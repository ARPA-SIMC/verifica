#!/bin/bash

### VERIFICA - ver_crea_obsmask.sh
### script per la creazione della maschera per le osservazioni
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

if [ "$1" = -h ] ; then
  echo "uso: $0 [-b]"
  echo "crea la maschera per le osservazioni"
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

if [ ! -f ./obsmask.nml ] ; then
  cp $VERSHARE/obsmask.nml.template ./obsmask.nml
fi
[ $BATCH -eq 0 ] && $EDITOR obsmask.nml

if [ ! -f ./odbc.nml ] ; then
  cp $VERSHARE/odbc.nml.template ./odbc.nml
fi
[ $BATCH -eq 0 ] && $EDITOR odbc.nml

ver_crea_obsmask

STATUS=$?
if [ $STATUS -ne 0 ] ; then
  echo ' ver_crea_obsmask terminato con errore= '$STATUS 1>&2
fi

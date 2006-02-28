#!/bin/bash

### VERIFICA - ver_pulisci.bash
### script per lo svuotamento del dababase DbAlle
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

if [ -z $EDITOR ] ; then
  echo "Errore"
  echo "Devi exportare la variabile EDITOR"
  exit 1
fi

if [ ! -f ./repinfo.csv ] ; then
  cp /etc/dballe/repinfo.csv ./repinfo.csv
fi
$EDITOR repinfo.csv

if [ ! -f ./odbc.nml ] ; then
  cp $VERSHARE/odbc.nml.template ./odbc.nml
fi
$EDITOR odbc.nml

ver_pulisci

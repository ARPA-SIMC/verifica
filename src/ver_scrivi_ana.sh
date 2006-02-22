#!/bin/bash

### VERIFICA - ver_scrivi_ana.sh
### script per la lettura da file grib e scrittura su database
### di campi analizzati
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

if [ -z $EDITOR ] ; then
  echo "Errore"
  echo "Devi exportare la variabile EDITOR"
  exit 1
fi

if [ ! -f ./griBlocale.txt ] ; then
  cp $VERSHARE/griBlocale.txt .
fi

if [ ! -f ./repinfo.csv ] ; then
  cp /etc/dballe/repinfo.csv ./repinfo.csv
fi
$EDITOR repinfo.csv

if [ ! -f ./analisi.nml ] ; then
  cp $VERSHARE/analisi.nml.template ./analisi.nml
fi
$EDITOR analisi.nml

if [ ! -f ./areaoss.nml ] ; then
  cp $VERSHARE/areaoss.nml.template ./areaoss.nml
fi
$EDITOR areaoss.nml

if [ ! -f ./odbc.nml ] ; then
  cp $VERSHARE/odbc.nml.template ./odbc.nml
fi
$EDITOR odbc.nml

ver_leggidati_ana

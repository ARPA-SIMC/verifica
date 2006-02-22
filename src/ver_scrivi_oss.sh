#!/bin/bash

### VERIFICA - ver_scrivi_oss.sh
### script per la lettura da file (formato fissato) e scrittura su database 
### delle osservazioni fornite dalle regioni e raccolte dal Piemonte
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

if [ ! -f ./region.nml ] ; then
  cp $VERSHARE/region.nml.template ./region.nml
fi
$EDITOR region.nml

if [ ! -f ./odbc.nml ] ; then
  cp $VERSHARE/odbc.nml.template ./odbc.nml
fi
$EDITOR odbc.nml

ver_leggidati_regio

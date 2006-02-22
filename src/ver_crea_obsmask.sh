#!/bin/bash

### VERIFICA - ver_crea_obsmask.sh
### script per la creazione della maschera per le osservazioni
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

if [ -z $EDITOR ] ; then
  echo "Errore"
  echo "Devi exportare la variabile EDITOR"
  exit 1
fi

if [ ! -f ./obsmask.nml ] ; then
  cp $VERSHARE/obsmask.nml.template ./obsmask.nml
fi
$EDITOR obsmask.nml

if [ ! -f ./odbc.nml ] ; then
  cp $VERSHARE/odbc.nml.template ./odbc.nml
fi
$EDITOR odbc.nml

ver_crea_obsmask

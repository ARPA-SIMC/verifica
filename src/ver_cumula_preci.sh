#!/bin/bash

### VERIFICA - ver_cumula_preci.sh
### script per la cumulazione su intervallo scelto dall'utente
### delle precipitazioni osservate presenti all'interno del database
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

if [ -z $EDITOR ] ; then
  echo "Errore"
  echo "Devi exportare la variabile EDITOR"
  exit 1
fi

if [ ! -f ./cumula.nml ] ; then
  cp $VERSHARE/cumula.nml.template ./cumula.nml
fi
$EDITOR cumula.nml

if [ ! -f ./odbc.nml ] ; then
  cp $VERSHARE/odbc.nml.template ./odbc.nml
fi
$EDITOR odbc.nml

ver_cumula_preci

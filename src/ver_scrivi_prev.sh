#!/bin/bash

### VERIFICA - ver_scrivi_prev.sh
### script per la preparazione di stat.nml e per l'esecuzione dei programmi 
### che caricano su database i previsti interpolati su stazioni 
### o i previsti e gli osservati ricalcolati su pseudostazioni
### I previsti si devono trovare in $SCRATCH/verifica/nome_progetto in un 
### file grib chiamato nome_modello.grib
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

PROJ=$1
if [ $# -lt 1 ] ; then
  echo "uso: ver_scrivi_prev.sh nome_progetto"
  exit 1
fi

if [ -z $EDITOR ] ; then
  echo "Errore"
  echo "Devi exportare la variabile EDITOR"
  exit 1
fi

if [ ! -f ./parametro.nml ] ; then
  echo "file parametro.nml mancante, lanciare ver_prepara_naml.sh" 
  exit 1
fi
$EDITOR parametro.nml

if [ ! -f ./profilever ] ; then
  cp $VERSHARE/profilever.template ./profilever
fi
$EDITOR profilever

if [ ! -f ./anag.nml ] ; then
  cp $VERSHARE/anag.nml.template ./anag.nml
fi

if [ ! -f ./areabox.nml ] ; then
  cp $VERSHARE/areabox.nml.template ./areabox.nml
fi

if [ ! -f ./odbc.nml ] ; then
  cp $VERSHARE/odbc.nml.template ./odbc.nml
fi
$EDITOR odbc.nml

. profilestra
. profilever

. profile_"$mod"

if [ ! -f $SCRATCH/$PROJ/$mod'.grib' ] ; then
  echo "ERRORE: $SCRATCH/$PROJ/$mod.grib non esiste" >&2
  exit 1 
fi
ln -fs $SCRATCH/$PROJ/$mod'.grib' estratti.grib

if [ $interpola = 'T' ] ; then

  echo ' $stat' > stat.nml
  echo '   model="'$mod'",' >> stat.nml
  echo '   itipo=1,' >> stat.nml
  echo '   iana='$iana',' >> stat.nml
  echo '   imet='$imet',' >> stat.nml
  echo '   imod='$imod',' >> stat.nml
  echo '   ls='$ls',' >> stat.nml
  echo '   ruota='$ruota',' >> stat.nml
  echo '   nminobs='$nminobs',' >> stat.nml
  echo '   media='$media',' >> stat.nml
  echo '   massimo='$massimo',' >> stat.nml
  echo '   prob='$prob',' >> stat.nml
  echo '   distr='$distr',' >> stat.nml
  echo '   dxb='$dxb',' >> stat.nml
  echo '   dyb='$dyb',' >> stat.nml
  echo '   diffh='$diffh',' >> stat.nml
  echo '   diffmax='$diffmax',' >> stat.nml
  echo '   thr='$thr',' >> stat.nml
  echo '   perc='$perc',' >> stat.nml
  echo ' $end' >> stat.nml

  ver_interpola_dballe

# box adiacenti (fisse)
elif [ $boxfix = 'T' ] ; then
 
  $EDITOR areabox.nml

  echo ' $stat' > stat.nml
  echo '   model="'$mod'",' >> stat.nml
  echo '   itipo=2,' >> stat.nml
  echo '   iana='$iana',' >> stat.nml
  echo '   imet='$imet',' >> stat.nml
  echo '   imod='$imod',' >> stat.nml
  echo '   ls='$ls',' >> stat.nml
  echo '   ruota='$ruota',' >> stat.nml
  echo '   nminobs='$nminobs',' >> stat.nml
  echo '   media='$media',' >> stat.nml
  echo '   massimo='$massimo',' >> stat.nml
  echo '   prob='$prob',' >> stat.nml
  echo '   distr='$distr',' >> stat.nml
  echo '   dxb='$dxb',' >> stat.nml
  echo '   dyb='$dyb',' >> stat.nml
  echo '   diffh='$diffh',' >> stat.nml
  echo '   diffmax='$diffmax',' >> stat.nml
  echo '   thr='$thr',' >> stat.nml
  echo '   perc='$perc',' >> stat.nml
  echo ' $end' >> stat.nml

  ver_boxfix_dballe

fi

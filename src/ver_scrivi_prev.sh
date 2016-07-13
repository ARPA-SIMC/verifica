#!/bin/bash

### VERIFICA - ver_scrivi_prev.sh
### script per la preparazione di stat.nml e per l'esecuzione dei programmi 
### che caricano su database i previsti interpolati su stazioni 
### o i previsti e gli osservati ricalcolati su pseudostazioni
### I previsti si devono trovare in $SCRATCH/verifica/nome_progetto in un 
### file grib chiamato nome_modello.grib
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

if [ $# -eq 0 ] ; then
  echo "uso: $0 -p nome_progetto [-b]"
  echo "carica campi in formato GRIB sul database dopo"
  echo "averli riferiti spazialmente ai campi di riferimento"
  echo " -p nome_progetto  directory di $SCRATCH dove si trova il grib"
  echo " -b                lancia in modalita' batch"
  exit 1
fi

BATCH=0
while :; do
  case "$1" in
    -p) shift
        if test "$#" -eq 0; then
          echo "manca il nome del progetto!" 1>&2
          exit 1
        fi
        if [ "$1" = -b ] ; then
	  echo "manca il nome del progetto!" 1>&2
	  exit 1
	fi
	PROJ=$1; shift;;
    -b) BATCH=1; shift;;
    *) break;;
  esac
done

if [ $BATCH -eq 0 ] ; then
  if [ -z $EDITOR ] ; then
    echo "ERRORE! Devi exportare la variabile EDITOR" 1>&2
    exit 1
  fi
fi

if [ ! -f ./parametro.nml ] ; then
  echo "file parametro.nml mancante, lanciare ver_prepara_naml.sh" 1>&2
  exit 1
fi
[ $BATCH -eq 0 ] && $EDITOR parametro.nml

if [ ! -f ./profilever ] ; then
  cp $VERSHARE/profilever.template ./profilever
fi
[ $BATCH -eq 0 ] && $EDITOR profilever

if [ ! -f ./anag.nml ] ; then
  cp $VERSHARE/anag.nml.template ./anag.nml
fi

if [ ! -f ./areabox.nml ] ; then
  cp $VERSHARE/areabox.nml.template ./areabox.nml
fi

if [ ! -f ./odbc.nml ] ; then
  cp $VERSHARE/odbc.nml.template ./odbc.nml
fi
[ $BATCH -eq 0 ] && $EDITOR odbc.nml

. profilestra
. profilever

. profile_"$mod"

if [ ! -f $SCRATCH/$PROJ/$mod'.grib' ] ; then
  echo "ERRORE! $SCRATCH/$PROJ/$mod.grib non esiste" 1>&2
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
  echo '   corrq='$corrq',' >> stat.nml
  echo '   thr='$thr',' >> stat.nml
  echo '   perc='$perc',' >> stat.nml
  echo '   lobm='$lobm',' >> stat.nml
  echo '   laree='$laree',' >> stat.nml
  echo '   shapefile='$shapefile',' >> stat.nml
  echo ' $end' >> stat.nml

  ver_interpola_dballe

  STATUS=$?
  if [ $STATUS -ne 0 ] ; then
    echo ' ver_interpola_dballe terminato con errore= '$STATUS 1>&2
  fi

# box adiacenti (fisse)
elif [ $boxfix = 'T' ] ; then
 
  [ $BATCH -eq 0 ] && $EDITOR areabox.nml

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
  echo '   corrq='$corrq',' >> stat.nml
  echo '   thr='$thr',' >> stat.nml
  echo '   perc='$perc',' >> stat.nml
  echo '   lobm='$lobm',' >> stat.nml
  echo '   laree='$laree',' >> stat.nml
  echo '   shapefile='$shapefile',' >> stat.nml
  echo ' $end' >> stat.nml

  ver_boxfix_dballe

  STATUS=$?
  if [ $STATUS -ne 0 ] ; then
    echo ' ver_boxfix_dballe terminato con errore= '$STATUS 1>&2
  fi

fi

#!/bin/bash

### VERIFICA - ver_prepara_nml.sh
### script per la scrittura del profile della variabile e
### per la scrittura delle namelist per i programmi di verifica
### autore: Chiara Marsigli
# ---------------------------------------------------------------------------

if [ "$1" = -h ] ; then
  echo "uso: $0 [-b]"
  echo "prepara le namelist per i programmi succesivi"
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

export VERSHARE=$PWD

if [ ! -f ./profile_verifica ] ; then
  cp $VERSHARE/profile_verifica.template ./profile_verifica
fi
[ $BATCH -eq 0 ] && $EDITOR profile_verifica

. profile_verifica

if [ $enddate -lt $startdate ] ; then
  echo "ERRORE! Data finale minore data iniziale!" 1>&2
  exit
fi

###cp $VERSHARE/profile_* .

# lancio il profile relativo al modello scelto (deve esistere in $VERSHARE)
. profile_"$model"

### preparo le namelist per i programmi successivi
[ -f parameters.nml ] && rm -f parameters.nml
[ -f parametro.nml ] && rm -f parametro.nml
[ -f date.nml ] && rm -f date.nml
[ -f scadenze.nml ] && rm -f scadenze.nml

echo ' $parameters' >> parameters.nml

echo '   nora='$hourrun >> parameters.nml

igiorno=$startdate
giornostop=`date --date ""$enddate" 1 day" '+%Y%m%d'`
ngio=0
while [ $igiorno -ne $giornostop ] ; do
  ngio=` expr $ngio + 1 `
  data=`date --date "$igiorno" '+%d,%m,%Y'`
  echo ' $date' >> date.nml
  echo '   data='$data >> date.nml
  echo ' $end' >> date.nml
  igiorno=`date --date "$igiorno 1 day" '+%Y%m%d'`
done
echo '   ngio='$ngio >> parameters.nml

if [ $inc -ne 0 ] ; then
  is=$scad1
  nscad=0
  while [ $is -le $scad2 ] ; do
    nscad=` expr $nscad + 1 `
    is=` expr $is + $inc `
  done
elif [ $inc -eq 0 ] ; then
  nscad=1
fi
echo '   nscad='$nscad >> parameters.nml
echo '   scad1='$scad1 >> parameters.nml
echo '   scad2='$scad2 >> parameters.nml
echo '   inc='$inc >> parameters.nml

kvar=$ce' '$ct' '`eval echo '$'iv$param`
nvar=1
rm -f profile_"$param"
echo ' par='$param >> profile_"$param"
echo ' livello='$lev >> profile_"$param"
echo ' scadenze="'$scadenze'"' >> profile_"$param"

echo '   nvar='$nvar >> parameters.nml

echo '   nrm='$ensmem >> parameters.nml

if [ $analisi -eq 0 ] ; then
  nore=1
  st_ora=$hourrun
elif [ $analisi -eq 1 ] ; then
  nore=0
  for iora in $ore ; do
    if [ $nore -eq 0 ] ; then
      st_ora=$iora
      nore=` expr $nore + 1 `
    else
      st_ora=$st_ora,$iora
      nore=` expr $nore + 1 `
    fi
  done
fi

echo '   nore='$nore >> parameters.nml
echo '   ore='$st_ora >> parameters.nml

echo ' $end' >> parameters.nml

echo ' $parametro' >> parametro.nml
echo '   kvar='$kvar >> parametro.nml
echo ' $end' >> parametro.nml

cat $VERSHARE/spiegapar.txt >> parametro.nml

echo ' $scadenza' >> scadenze.nml
echo '   scadenze='"$scadenze" >> scadenze.nml
echo ' $end' >> scadenze.nml

#!/bin/bash

### VERIFICA - ver_prepara_nml.sh
### script per la scrittura del profile della variabile usato da 
### ver_estrai_calcola.sh e per la scrittura delle namelist 
### per i programmi di verifica
### autore: Chiara Marsigli
# ----------------------------------------------------------------------------------

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

if [ ! -f ./profilestra ] ; then
  cp $VERSHARE/profilestra.template ./profilestra
fi
[ $BATCH -eq 0 ] && $EDITOR profilestra

. profilestra

if [ $a2$m2$g2 -lt $a1$m1$g1 ] ; then
  echo "ERRORE! Data finale minore data iniziale!" 1>&2
  exit
fi

cp $VERSHARE/profile_* .
cp $VERSHARE/getgribdat_ver .

# lancio il profile relativo al modello scelto (deve esistere in $VERSHARE)
. profile_"$mod"

### preparo le namelist per i programmi successivi
[ -f parameters.nml ] && rm -f parameters.nml
[ -f parametro.nml ] && rm -f parametro.nml
[ -f startend.nml ] && rm -f startend.nml
[ -f date.nml ] && rm -f date.nml
[ -f scadenze.nml ] && rm -f scadenze.nml

echo ' $parameters' >> parameters.nml

echo '   nora='$ora >> parameters.nml

igiorno=$a1$m1$g1
giornostop=`date --date ""$a2$m2$g2" 1 day" '+%Y%m%d'`
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

totparam="$param"' '"$dpar"
echo $totparam
nvar=0
for par in $totparam ; do
  if [ $nvar -eq 0 ] ; then
    kvar=$ce' '$ct' '`eval echo '$'iv$par`
    nvar=` expr $nvar + 1 `
  elif [ $nvar -gt 0 ] ; then
    kvar=$kvar,$ce' '$ct' '`eval echo '$'iv$par`
    nvar=` expr $nvar + 1 `
  fi
  rm -f profile_"$par"
  echo ' par='$par >> profile_"$par"
  echo ' livello='$lev >> profile_"$par"
  echo ' scadenze="'$scadenze'"' >> profile_"$par"
done

echo '   nvar='$nvar >> parameters.nml

echo '   nrm='$mem >> parameters.nml

if [ $analisi -eq 0 ] ; then
  nore=1
  st_ora=$ora
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

#!/bin/bash

### VERIFICA - ver_estrai_calcola.sh
### script per l'estrazione di previsti e analisi da archivio GRIB 
### e per il calcolo di variabili derivate e grandezze cumulate
### Il file grib contenente tutti i previsti viene creato in 
### $SCRATCH/nome_progetto e si chiama nome_modello.grib
### autore: Chiara Marsigli
# Note:
# attenzione! Si usa la variabile d'ambiente $GRB_EXE
# ----------------------------------------------------------------------------------

if [ $# -eq 0 ] ; then
  echo "uso: $0 -p nome_progetto [-b]"
  echo "estrae campi in formato GRIB da GRIB_ARCH"
  echo " -p nome_progetto  directory di $SCRATCH dove mette il grib"
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

if [ ! -f profilestra ] ; then
  echo "file profilestra mancante, lanciare ver_prepara_naml.sh" 1>&2
  exit 1
fi

. profilestra
. profile_"$mod"

mkdir $SCRATCH/$PROJ

gioin=$g1$m1$a1
giofi=$g2$m2$a2
giorni="$gioin"'_'"$giofi"
giornostop=`date --date ""$a2$m2$g2" 1 day" '+%Y%m%d'`

if [ $analisi -eq 1 ] ; then
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
  ora=$st_ora
fi

tot_grib=$SCRATCH/$PROJ/"$mod"'.grib'
[ -f "$tot_grib" ] && rm -f "$tot_grib"

### ciclo sui parametri

for par in $param ;  do 

  . profile_"$par"
  echo $scadenze

# scrivo l'expansion
  sed -e "s%@@dataset%"$mod"%g" \
      -e "s%@@data%"$giorni"%g" \
      -e "s%@@time%"$ora"%g" \
      -e "s%@@scad%""$scadenze""%g" \
      -e "s%@@lev%"$livello"%g" \
      -e "s%@@var%"$par"%g" \
      -e "w expansion.ver"    $VERSHARE/expansion_ver.template

  out_grib=$SCRATCH/$PROJ/"$mod"'_'"$par"'.grib'

# estrazione
  $GRB_EXE/fp_expansion expansion.ver getgribdat_ver
  if [ $? -ne 0 ] ; then 
    exit 1
  fi
  $GRB_EXE/fp_getgrib n DISCHI NASTRI getgribnaml_ver getgribdat_ver $out_grib

# cumulazione della precipitazione sui periodi voluti tramite sottrazione
  if [ $par = "tp" ] ; then
    [ -f "$out_grib" ] && mv "$out_grib" tmp_tp.grib

# cumulo sui periodi scelti dall'utente, tramite inc
    [ -f tmp_pezzi.grib ] && rm -f tmp_pezzi.grib
    igiorno=$a1$m1$g1
    while [ $igiorno -ne $giornostop ] ; do
      giorno=`date --date "$igiorno" '+%d,%m,%Y'`
      if [ $scad1 = "3" -o $scad1 = "6" -o $scad1 = "12" -o $scad1 = "24" ] ; then
        sed -n -e "s%@@data%"$giorno"%g" \
            -e "s%@@scadenza%"$scad1"%g" \
            -e "w ricopia.ged"    $VERSHARE/ricopia_ged.template
        ged ricopia.ged
	newscad1=`echo $scad1 |awk '{a=substr($0,0,3); printf "%03d", a;}'`
	cong tmp_sott.grib tmp_sott_corr.grib -s000 -t"$scad1"
	cat tmp_sott_corr.grib >> tmp_pezzi.grib
      fi
      is1=$scad1
      while [ $is1 -lt $scad2 ] ; do
        is2=` expr $is1 + $inc `
        sed -n -e "s%@@data%"$giorno"%g" \
            -e "s%@@scadenza1%"$is1"%g" \
            -e "s%@@scadenza2%"$is2"%g" \
            -e "w tpsott.ged"    $VERSHARE/tpsott_ged.template
        ged tpsott.ged
        is1=$is2
        cat tmp_sott.grib >> tmp_pezzi.grib
      done
      igiorno=`date --date "$igiorno 1 day" '+%Y%m%d'`
    done
    [ -f tmp_pezzi.grib ] && mv tmp_pezzi.grib "$out_grib" 

# cumulo la pioggia su periodi prefissati (6, 12 o 24)
    for icum in $ncum ; do
      [ -f 'tmp_'"$par"'_'"$icum".grib ] && rm -f 'tmp_'"$par"'_'"$icum".grib
      igiorno=$a1$m1$g1
      while [ $igiorno -ne $giornostop ] ; do
        giorno=`date --date "$igiorno" '+%d,%m,%Y'`
        is1=$scad1
	if [ $scad1 -eq $icum ] ; then
          sed -n -e "s%@@data%"$giorno"%g" \
                 -e "s%@@scadenza%"$is1"%g" \
                 -e "w ricopia.ged"    $VERSHARE/ricopia_ged.template
          ged ricopia.ged
          cat tmp_sott.grib >> 'tmp_'"$par"'_'"$icum".grib
        fi
        while [ $is1 -lt $scad2 ] ; do
          is2=` expr $is1 + $icum `
          sed -n -e "s%@@data%"$giorno"%g" \
              -e "s%@@scadenza1%"$is1"%g" \
              -e "s%@@scadenza2%"$is2"%g" \
              -e "w tpsott.ged"    $VERSHARE/tpsott_ged.template
          ged tpsott.ged
          cat tmp_sott.grib >> 'tmp_'"$par"'_'"$icum".grib
          is1=$is2
        done
        igiorno=`date --date "$igiorno 1 day" '+%Y%m%d'`
      done
      cat 'tmp_'"$par"'_'"$icum".grib >> "$out_grib" 
    done

  fi

  cat "$out_grib" >> "$tot_grib"
  rm "$out_grib"

done
### fine ciclo sui parametri

### ciclo sui parametri derivati
for par in $dpar ; do 

  if [ ! -f profile_"$par" ] ; then
     echo "manca il file profile_""$par"
     exit
  fi
  . profile_"$par"

  if [ $par = "rh" ] ; then

# calcolo dell'umidita' relativa a partire da t e td
    [ -f tmp_pezzi.grib ] && rm -f tmp_pezzi.grib
    cp $tot_grib tmp_tot.grib
    igiorno=$a1$m1$g1
    while [ $igiorno -ne $giornostop ] ; do
      giorno=`date --date "$igiorno" '+%d,%m,%Y'`
      is=$scad1
      while [ $is -le $scad2 ] ; do
        sed -n -e "s%@@data%$giorno%g" \
            -e "s%@@variabile1%$ivt%g" \
            -e "s%@@variabile2%$ivtd%g" \
            -e "s%@@scadenza%$is%g" \
            -e "w umrel.ged"    $VERSHARE/umrel_ged.template
        ged umrel.ged
        cat tmp_umrel.grib >> tmp_pezzi.grib
        is=` expr $is + $inc `
      done
      igiorno=`date --date "$igiorno 1 day" '+%Y%m%d'`
    done
    cat tmp_pezzi.grib >> $tot_grib

  fi

  if [ $par = "ff" ] ; then

# calcolo di intensita' e direzione del vento a partire da u e v
    [ -f tmp_pezzi.grib ] && rm -f tmp_pezzi.grib
    cp $tot_grib tmp_tot.grib
    igiorno=$a1$m1$g1
    while [ $igiorno -ne $giornostop ] ; do
      giorno=`date --date "$igiorno" '+%d,%m,%Y'`
      is=$scad1
      while [ $is -le $scad2 ] ; do
        sed -n -e "s%@@genproc%$gp%g" \
            -e "s%@@tabella%$ct%g" \
            -e "s%@@giorno%$giorno%g" \
            -e "s%@@ucomp%$ivu%g" \
            -e "s%@@vcomp%$ivv%g" \
            -e "s%@@scadenza%$is%g" \
            -e "s%@@intensita%$ivff%g" \
            -e "s%@@direzione%$ivdd%g" \
            -e "w ffdd.nml"    $VERSHARE/ffdd_nml.template
         ver_ffdd
         cat tmp_int.grib >> tmp_pezzi_int.grib
         cat tmp_dir.grib >> tmp_pezzi_dir.grib
         is=` expr $is + $inc `
      done
      igiorno=`date --date "$igiorno 1 day" '+%Y%m%d'`
    done
    cat tmp_pezzi_int.grib >> $tot_grib
    cat tmp_pezzi_dir.grib >> $tot_grib

  fi

done
### fine ciclo sui parametri derivati

# attacco in fondo al file grib land-sea mask e orografia del modello scelto
# (devono esistere in locale o in $VERSHARE)
if [ ! -f lsm_"$mod".grib ] ; then
  if [ -f $VERSHARE/lsm_"$mod".grib ] ; then
    cat $VERSHARE/lsm_"$mod".grib >> $tot_grib
  fi
else
  cat lsm_"$mod".grib >> $tot_grib
fi
if [ ! -f orog_"$mod".grib ] ; then
  if [ -f $VERSHARE/orog_"$mod".grib ] ; then
    cat $VERSHARE/orog_"$mod".grib >> $tot_grib
  fi
else
  cat orog_"$mod".grib >> $tot_grib
fi

# cancello tutti i files temporanei utilizzati
rm -f tmp*.grib

    program crea_obsmask

! c VERIFICA - crea_obsmask.f
! c programma per la produzione di un file grib che contiene una maschera
! c per le osservazioni. Ogni punto di griglia del grib in ingresso
! c riceve valore 1 se giace in una zona coperta da osservazioni, 0 altrimenti
! c autore: Chiara Marsigli

! Copyright (C) 2004

! Questo programma è software libero; è lecito ridistribuirlo e/o
! modificarlo secondo i termini della Licenza Pubblica Generica SMR come
! pubblicata da ARPA SMR ; o la versione 1 della licenza o (a scelta)
! una versione successiva.
! Questo programma è distribuito nella speranza che sia utile, ma SENZA
! ALCUNA GARANZIA; senza neppure la garanzia implicita di
! COMMERCIABILITÀ o di APPLICABILITÀ PER UN PARTICOLARE SCOPO. Si veda
! la Licenza Pubblica Generica SMR per avere maggiori dettagli.
! Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
! Generica SMR insieme a questo programma; in caso contrario, la si può
! ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA) Servizio
! Idro-Meteorologico (SIM), Viale Silvani 6, 40122 Bologna, Italia

! ARPA-SIM
! Viale Silvani, 6 e 2/3 - 40122 Bologna
! URP Tel: + 39 051 6497511 Fax: + 39 051 6497501
! E-mail: urpsim@smr.arpa.emr.it
! Internet: http://www.arpa.emr.it/sim/

    parameter    (MNSTAZ=10000,MNBOX=80000)
    parameter    (MIDIMG=200000,MIDIMV=MIDIMG*4)
    real ::         xgrid(MIDIMV)
    integer ::      kgrib(MIDIMG)
    character(80) :: rfile,ofile
    character(19) :: database,user,password
    real ::         lms,obm,rmdo,alorot,alarot,slon1,slon2,slat1,slat2
    integer ::      iana,nstaz,nbox
    logical ::      ruota,area
    real ::         x(MNSTAZ),y(MNSTAZ),alt(MNSTAZ)
    real ::         xb(MNBOX),yb(MNBOX)
! grib fields
    integer ::      ksec0(2),ksec1(104),ksec2(384),ksec3(2),ksec4(60)
    real ::         psec2(384),psec3(2)

    namelist  /obsmask/rfile,iana,ruota,dist
    namelist  /odbc/database,user,password

    data ksec0/2*0/
    data ksec1/104*0/
    data ksec2/384*0/
    data ksec3/2*0/
    data ksec4/60*0/
    data kgrib/MIDIMG*0/
    data psec2/384*0./
    data psec3/2*0./

    data rmdo/-999.9/

    ofile='obsmask.grib'

    area=.false.

    iug=0
    idimg=MIDIMG
    idimv=MIDIMV

    call pbopen(iug,rfile,'r',ier)
    if(ier /= 0)goto9100
    print*,'pbopen fatta ',ier
    call pbgrib(iug,kgrib,idimg,idimv,ier)
    if(ier == -1)goto9200
    if(ier /= 0)goto9300
    print*,'pbgrib fatta ',ier
    call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
    xgrid,MIDIMV,kgrib,MIDIMG,kword,'D',ier)
    if(ier == -6)goto9400
    if(ier > 0)goto9500
    call pbclose(iug,ier)
    if(ier /= 0)goto9600

! leggo le coordinate di tutte le box del modello
    call leggibox(rfile,MNBOX,xb,yb,nbox,alorot,alarot, &
    ruota,area,slon1,slon2,slat1,slat2)
    print*,'numero totale box ',nbox
    if(ruota)then
        call rot_grib_LAMBO(alorot,alarot,rxeq,ryeq)
        cryeq=cos(ryeq*3.1415927/180.)
        sryeq=sin(ryeq*3.1415927/180.)
        do ib=1,nbox
            rxb=xb(ib)
            ryb=yb(ib)
            call rtlld(rxb,ryb,rxeq,cryeq,sryeq,arxb,aryb)
            xb(ib)=arxb
            yb(ib)=aryb
        enddo
    endif

! leggo tutte le stazioni disponibili in archivio
    call leggiana_db(MNSTAZ,iana,x,y,alt,rmdo,nstaz, &
    database,user,password)
    print*,'numero massimo stazioni ',nstaz

    idimv=ksec4(1)
    if(idimv /= nbox)then
        print*,'PERCHE''??????'
        stop
    endif
    do iv=1,idimv
        xm=xb(iv)
        ym=yb(iv)
        obm=0.
        do ist=1,nstaz
            if(sqrt((xm-x(ist))**2+(ym-y(ist))**2) < dist)then
                obm=1.
                goto111
            endif
        enddo
        111 xgrid(iv)=obm
    enddo

    call pbopen(iug,ofile,'w',ier)
    if(ier /= 0)goto9700
    call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
    xgrid,idimv,kgrib,idimg,kword,'C',ier)
    if(ier /= 0)goto9800
    call pbwrite(iug,kgrib,ksec0(1),ier)
    if(ier < 0)goto9900
    call pbclose(iug,ier)
    if(ier < 0)goto9990

    stop
    9100 print *,'Errore durante la pbopen ',ier
    stop
    9200 print *,'Termino per end of file (pbgrib) ',ier
    stop
    9300 print *,'Errore lettura grib (pbgrib) ',ier
    stop
    9400 print *,'GRDEMO : Pseudo-grib data found (gribex) ',ier
    stop
    9500 print*, 'Errore degribbing (gribex) ',ier
    stop
    9600 print *,'Errore durante la pbclose ',ier
    stop
    9700 print *,'Errore durante la pbopen (write) ',ier
    stop
    9800 print *,'Errore durante la gribex (write) ',ier
    stop
    9900 print *,'Errore durante la pbwrite ',ier
    stop
    9990 print *,'Errore durante la pbclose (write) ',ier
    stop
    end program

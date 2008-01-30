    program ffdd

! c VERIFICA - ffdd.f
! c programma per il calcolo e la codifica in grib di intensita' e direzione
! c del vento a partire dai campi gribbati u e v
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

    parameter (MIDIMG=1200000)
    integer :: kgrib(MIDIMG)
    REAL, ALLOCATABLE :: xgridu(:),xgridv(:)
    REAL, ALLOCATABLE :: xgridf(:),xgridd(:)
    integer :: level(3),var(3),est(3),scad(4),data(3),ora(2)
    real :: alat(4),alon(4)
    character :: uvfile*20,ifile*20,dfile*20
! grib fields
    integer :: ksec0(2),ksec1(104),ksec2(22),ksec3(2),ksec4(42)
    REAL :: psec2(10),psec3(2),dummy(1)

    integer :: genproc,tabella,ucomp,vcomp,scadenza
    integer :: intensita,direzione
    integer :: giorno(3)
    namelist      /parameters/genproc,tabella,giorno,ucomp,vcomp, &
    scadenza,intensita,direzione

    data ksec0/2*0/
    data ksec1/104*0/
    data ksec2/22*0/
    data ksec3/2*0/
    data ksec4/42*0/
    data kgrib/MIDIMG*0/
    data psec2/10*0./
    data psec3/2*0./

    data level/-1,-1,-1/, var/-1,-1,-1/, est/-1,-1,-1/, &
    scad/-1,-1,-1,-1/, data/-1,-1,-1/, ora/-1,-1/

    pi=acos(-1.)
! RTA è 180/pi

! Read parameters
    open(8,file='ffdd.nml',status='old')
    read(8,nml=parameters,err=9000)
    close(8)

    uvfile='tmp_tot.grib'
    ifile='tmp_int.grib'
    dfile='tmp_dir.grib'
    data(1)=giorno(1)
    data(2)=giorno(2)
    data(3)=giorno(3)
    var(2)=tabella
    scad(2)=scadenza
    print*,'ffdd.f - scadenza ',scadenza

! lettura grib allo scopo di avere MIDIMV (ksec4(1))
    iug=0
    call pbopen(iug,uvfile,'r',ier)
    if(ier /= 0)goto9100
    CALL pbgrib(iug,kgrib,MIDIMG,idimg,ier)
    if(ier /= 0)goto9800
    CALL gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
     dummy,SIZE(dummy), &
     kgrib,MIDIMG,idimg,'J',ier)
    if(ier /= 0)goto9600
    MIDIMV=ksec4(1)
!CALL pbgrib(unit, this%rawgrib, this%cursize*nb, actsize, ier)
!IF (ier == -3 .AND. this%cursize < maxsize) THEN ! rawgrib too small
!pzsec4 => zzsec4 ! Gribex requires a valid section 4 also for 'I/J', satisfy it!
!ierval = 1 ! Do not abort in case of error
!CALL gribex(this%isec0, this%isec1, this%isec2, this%zsec2, &
! this%isec3, this%zsec3, this%isec4, pzsec4, SIZE(pzsec4), &
! this%rawgrib, this%cursize, actsize, 'J', ierval)
!ALLOCATE(this%zsec4(MAX(1,this%isec4(1))), STAT=ierval)
    call pbclose(iug,ier)
    if(ier /= 0)goto9500

    ALLOCATE(xgridu(MIDIMV))
    ALLOCATE(xgridv(MIDIMV))
    ALLOCATE(xgridf(MIDIMV))
    ALLOCATE(xgridd(MIDIMV))

    iug=0
    idimg=MIDIMG
    idimv=MIDIMV
    imd=-32768
    rmd=-1.5E21
    call pbopen(iug,uvfile,'r',ier)
    if(ier /= 0)goto9100

    var(3)=ucomp
    call findgribest(iug,kgrib,idimg,data,ora,scad,level,var,est,ier)
    if(ier == -1)then
        goto 9150
    elseif(ier /= 0)then
        goto 9200
    endif
    call getinfoest(-1,kgrib,idimg,data,ora,scad,level,var,est, &
    alat(1),alat(2),alon(1),alon(2),ny,nx,dy,dx,idrt, &
    alarot,alorot,rot,ija,ier)
    if(ier /= 0)goto 9300
    call getdata(kgrib,idimg,imd,rmd,xgridu,idimv,ibm,ier)
    if(ibm /= 0 .OR. ier /= 0)goto 9400

    var(3)=vcomp
    call findgribest(iug,kgrib,idimg,data,ora,scad,level,var,est,ier)
    if(ier == -1)then
        goto 9150
    elseif(ier /= 0)then
        goto 9200
    endif
    call getdata(kgrib,idimg,imd,rmd,xgridv,idimv,ibm,ier)
    if(ibm /= 0 .OR. ier /= 0)goto 9400

    call pbclose(iug,ier)
    if(ier /= 0)goto9500

    do iv=1,idimv
        u=xgridu(iv)
        v=xgridv(iv)
        if(u == 0. .AND. v == 0.)then
            ff=0.
            dd=0.
        elseif(u /= rmd .AND. u /= rmd)then
            ff=sqrt(u**2+v**2)
            dd=atan2(u,v)*180/pi+180.
            if(u == 0. .AND. v < 0.)dd=0.
            if(ff < 1.)dd=0. !definizione di calma di vento!!!
        ! attenzione, si considera in metri al secondo!
        else
            ff=rmd
            dd=rmd
        endif
        xgridf(iv)=ff
        xgridd(iv)=dd
    enddo

    ksec0(1)=idimg
    ksec0(2)=1

    ksec1(1)=var(2)
    ksec1(2)=var(1)
    ksec1(3)=genproc
    ksec1(4)=255
    ksec1(5)=128
    ksec1(6)=intensita   !wind speed
    print*,'variabile ',ksec1(6)
    ksec1(7)=level(1)
    ksec1(8)=level(2)
    ksec1(9)=level(3)
    ksec1(10)=mod(data(3),100)
    ksec1(11)=data(2)
    ksec1(12)=data(1)
    ksec1(13)=ora(1)
    ksec1(14)=ora(2)
    ksec1(15)=scad(1)
    ksec1(16)=scad(2)
    ksec1(17)=scad(3)
    ksec1(18)=scad(4)
    ksec1(19)=0
    ksec1(20)=0
    ksec1(21)=int(data(3)/100)+1
    ksec1(22)=0
    ksec1(23)=0
    ksec1(24)=1
    ksec1(37)=255

    ksec2(1)=idrt
    ksec2(2)=nx
    ksec2(3)=ny
    ksec2(4)=alat(1)*1000.
    ksec2(5)=alon(1)*1000.
    ksec2(6)=128
    ksec2(7)=alat(2)*1000.
    ksec2(8)=alon(2)*1000.
    ksec2(9)=(max(ksec2(7),ksec2(4))-min(ksec2(7),ksec2(4)))/(ny-1)
    ksec2(10)=(ksec2(8)-ksec2(5))/(nx-1)
    ksec2(11)=ija
    ksec2(12)=0
    ksec2(13)=alarot*1000.
    ksec2(14)=alorot*1000.

    ksec4(1)=idimv
    ksec4(2)=24

    call pbopen(iug,ifile,'w',ier)
    if(ier /= 0)goto9100
    call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
    xgridf,idimv,kgrib,idimg,kword,'C',ier)
    if(ier /= 0)goto9600
    call pbwrite(iug,kgrib,ksec0(1),ier)
    if(ier < 0)goto9700
    call pbclose(iug,ier)
    if(ier < 0)goto9500

    ksec1(6)=direzione   !wind direction
    print*,'variabile ',ksec1(6)

    call pbopen(iug,dfile,'w',ier)
    if(ier /= 0)goto9100
    call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
    xgridd,idimv,kgrib,idimg,kword,'C',ier)
    if(ier /= 0)goto9600
    call pbwrite(iug,kgrib,ksec0(1),ier)
    if(ier < 0)goto9700
    call pbclose(iug,ier)
    if(ier < 0)goto9500

    stop
    9000 print *,"Errore durante la lettura della namelist ",ier
    stop
    9100 print *,"Errore durante la pbopen ",ier
    stop
    9150 print *,"Grib non trovato ",ier
    stop
    9200 print *,"Errore durante la findgribest ",ier
    stop
    9300 print *,"Errore durante la getinfo ",ier
    stop
    9400 print *,"Errore durante la getdata ",ier
    stop
    9500 print *,"Errore durante la pbclose ",ier
    stop
    9600 print *,"Errore durante la gribex ",ier
    stop
    9700 print *,"Errore durante la pbwrite ",ier
    stop
    9800 print *,"Errore durante la pbgrib ",ier
    stop
    end program

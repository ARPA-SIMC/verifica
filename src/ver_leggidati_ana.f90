    program leggidati_ana

! c VERIFICA - leggidati_ana.f
! c programma per la lettura da file grib e scrittura su database dballe
! c di "osservati" su grigliato (analisi)
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

    USE util_dballe

    IMPLICIT NONE

    integer, parameter :: MNBOX=750000
    integer, parameter :: MIDIMG=2500000

    real :: xb(MNBOX),yb(MNBOX),alte(MNBOX),h
    integer :: pos(MNBOX)
    integer :: scaddb(4)
    integer :: block,station
    character :: name*20,cb*5,rep_memo*20
    logical :: forever
    character :: cvar*6
    real :: dato
    real :: a,b,alarot,alorot,rot,blat,blon,cryeq,sryeq,dx,dy,factz,rmd
    real :: rlon,rlat,rxeq,ryeq
    integer :: idrt,igrid,ija,imd,ier,ibm
    integer :: ib,nbox,id_ana,idbhandle,iug,ix,iy,nx,ny,nb2
    integer :: midimv,idimg,idimv
    integer :: idayv,imonthv,iyearv,ihourv,iminv,iminuti

    integer :: kgrib(MIDIMG)
    REAL, ALLOCATABLE :: xgrid(:)
    integer :: ksec0(2),ksec1(104),ksec2(384),ksec3(2),ksec4(60)
    REAL :: psec2(384),psec3(2),dummy(1)
    integer :: level(3),var(3),est(3),scad(4),data(3),ora(2)
    real :: alat(4),alon(4)
    integer :: leveltype1,l1,leveltype2,l2
    integer :: pind,fctime,period
! namelist variables
    character(len=80) :: fileorog='',vfile=''
    logical :: lorog=.false.,ruota=.false.,area=.false.
    real :: slon1=10.,slon2=10.,slat1=45.,slat2=45.
    character(512) :: database='',user='',password=''

    data ksec0/2*0/
    data ksec1/104*0/
    data ksec2/384*0/
    data ksec3/2*0/
    data ksec4/60*0/
! ksec4(5)=0 per real data
    data kgrib/MIDIMG*0/
    data psec2/384*0./
    data psec3/2*0./

    data block/90/
    data forever/.true./

! database
    integer :: handle
    integer :: debug=1
    integer :: handle_err

!    integer :: ier

    namelist  /analisi/lorog,fileorog,vfile,ruota
    namelist  /areaoss/area,slon1,slon2,slat1,slat2
    namelist  /odbc/database,user,password

    PRINT*,'program leggidati_ana'

    open(1,file='odbc.nml',status='old')
    read(1,nml=odbc,err=9003)
    close(1)

    factz=1./9.81

    open(1,file='analisi.nml',status='old')
    read(1,nml=analisi,err=9001)
    close(1)

    open(1,file='areaoss.nml',status='old')
    read(1,nml=areaoss,err=9002)
    close(1)

! old
! lettura grib allo scopo di avere MIDIMV (ksec4(1))
    iug=0
    call pbopen(iug,fileorog,'r',ier)
    if(ier /= 0)goto9100
    CALL pbgrib(iug,kgrib,MIDIMG,idimg,ier)
    if(ier /= 0)goto9800
    CALL gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
     dummy,SIZE(dummy), &
     kgrib,MIDIMG,idimg,'J',ier)
    if(ier /= 0)goto9600
    MIDIMV=ksec4(1)
    call pbclose(iug,ier)
    if(ier /= 0)goto9500

! new
! lettura grib allo scopo di avere MIDIMV (ksec4(1))
!    call grib_open_file(ifile,vfile,'r')

!    call grib_new_from_file(ifile,igrib, iret)
!   Loop on all the messages in a file.
!    call grib_get(igrib,'numberOfDataPoints',MIDIMV)
!    write(*,'(i7)') MIDIMV
    
!    print*,'MIDIMV',MIDIMV

!    call grib_close_file(ifile)

    ALLOCATE(xgrid(MIDIMV))

    print*,'orografia e coordinate delle box'
    iug=0
    idimg=MIDIMG
    idimv=MIDIMV
    imd=-32768
    rmd=-1.5E21
    igrid=0                   !sono griglie regolari
    call pbopen(iug,fileorog,'r',ier)
    if(ier /= 0)goto9100
    call getinfoest(iug,kgrib,idimg,data,ora,scad,level, &
    var,est,alat(1),alat(2),alon(1),alon(2), &
    ny,nx,dy,dx,idrt,alarot,alorot,rot,ija,ier)
    if(ier /= 0)goto 9300
    call getdata(kgrib,idimg,imd,rmd,xgrid,idimv,ibm,ier)
    if(ibm /= 0 .OR. ier /= 0)goto 9400
    call pbclose(iug,ier)
    if(ier < 0)goto9500
    if(ija == 64)then
        blat=alat(1)
        do iy=1,ny
            blon=alon(1)
            do ix=1,nx
                ib=ix+(iy-1)*nx
                xb(ib)=blon
                yb(ib)=blat
                alte(ib)=xgrid(ib)*factz
                pos(ib)=ib
                blon=blon+dx
            enddo
            blat=blat+dy
        enddo
    elseif(ija == 0)then    !con 128 non funziona
        blat=alat(1)
        do iy=1,ny
            blon=alon(1)
            do ix=1,nx
                ib=ix+(iy-1)*nx
                xb(ib)=blon
                yb(ib)=blat
                alte(ib)=xgrid(ib)*factz
                pos(ib)=ib
                blon=blon+dx
            enddo
            blat=blat-dy
        enddo
    endif
    nbox=ib
    if(nbox > MNBOX)then
        print*,'ERRORE - MNBOX INSUFFICIENTE'
        call exit (1)
    elseif(nbox < 0)then
        print*,'ERRORE! - nbox= ',nbox
        call exit(2)
    else
        print*,'nbox ',nbox
    endif
    if(ruota)then
        call rot_grib_LAMBO(alorot,alarot,rxeq,ryeq)
        cryeq=cos(ryeq*3.1415927/180.)
        sryeq=sin(ryeq*3.1415927/180.)
        do ib=1,nbox
            call rtlld(xb(ib),yb(ib),rxeq,cryeq,sryeq,rlon,rlat)
            xb(ib)=rlon
            yb(ib)=rlat
            alte(ib)=alte(ib)
            pos(ib)=ib
        enddo
    endif

    if(area)then
        nb2=0
        do ib=1,nbox
            rlon=xb(ib)
            rlat=yb(ib)
            if(rlon >= slon1 .AND. rlon < slon2 .AND. &
            rlat >= slat1 .AND. rlat < slat2)then
                nb2=nb2+1
                xb(nb2)=rlon
                yb(nb2)=rlat
                alte(nb2)=alte(ib)
                pos(nb2)=ib
            endif
        enddo
        nbox=nb2
    endif
    print*,'nuove box ',nbox

! PREPARAZIONE DELL' ARCHIVIO
    print*,"database=",database

    ier=idba_error_set_callback(0,C_FUNLOC(idba_default_error_handler),debug,handle_err)

    ier=idba_presentati(idbhandle,database)

    ier=idba_preparati(idbhandle,handle,"write","write","write")

    iug=0
    idimg=MIDIMG
    idimv=MIDIMV
    imd=-32768
    rmd=-1.5E21
    igrid=0                   !sono griglie regolari
    call pbopen(iug,vfile,'r',ier)
    if(ier /= 0)goto9100

! CICLO SUI GRIB
    do while(forever)

        call getinfoest(iug,kgrib,idimg,data,ora,scad,level, &
        var,est,alat(1),alat(2),alon(1),alon(2), &
        ny,nx,dy,dx,idrt,alarot,alorot,rot,ija,ier)
        if(ier == -1)goto 111
        if(ier /= 0)goto 9300
        call getdata(kgrib,idimg,imd,rmd,xgrid,idimv,ibm,ier)
        if(ibm /= 0 .OR. ier /= 0)goto 9400

        CALL variabile(3,var,cvar,a,b,.TRUE.)
            
        CALL JELADATA5(DATA(1),DATA(2),DATA(3),ora(1),ora(2),iminuti)

        CALL converti_scadenze(4,scad,scaddb) ! converte in secondi

        scadenze: select case(scaddb(4))
!        case(4) ! cumulata
!           pind=1
!           fctime=0
!           period=scaddb(3)-scaddb(2)
!           iminuti=iminuti+scaddb(3)/60 ! porto la validità alla fine del periodo
        case(0) ! istantanea
           pind=254
           if(scaddb(3)/=0)then 
              print*,'case 0 - p1= ',scaddb(2),' p2= ',scaddb(3)
              call exit(1)
           endif
           fctime=0
           period=0
        case(1) ! analisi inizializzata
           pind=254
           if(scaddb(2)/=0)then
              print*,'case 1 - p1= ',scaddb(2),' p2= ',scaddb(3)
              call exit(1)
           endif
           fctime=0
           period=0
        case(2) ! prodotto valido in un periodo
           pind=205
           fctime=0
           period=scaddb(3)-scaddb(2)
        case(4) ! analisi di precipitazione !!! era 13!!!
           pind=1
           print*,'controllo - verrebbe fctime= ',scaddb(2),' period= ',scaddb(3)
           fctime=0
           period=scaddb(3)
! la validità dovrebbe già essere alla fine del periodo, non sposto iminuti
        case default
           print*,'scadenza non gestito'
           call exit(1)
        end select scadenze
! non sposto iminuti per gli altri casi dato che dovrebbero essere analisi!

        CALL JELADATA6(idayv,imonthv,iyearv,ihourv,iminv,iminuti)
        
    ! INSERIMENTO DEI PARAMETRI NELL' ARCHIVIO
        ier=idba_unsetall (handle)

        do ib=1,nbox

! anagrafica
            ier=idba_setcontextana (handle)

            rlon=xb(ib)
            rlat=yb(ib)
            h=alte(ib)
            WRITE(cb,'(i5.5)')ib
            name='_gp'//cb
            station=ib

            if(xgrid(pos(ib)) /= rmd)then

               ier=idba_set (handle,"lon",rlon)
               ier=idba_set (handle,"lat",rlat)
               ier=idba_set (handle,"mobile",0)

               ier=idba_set (handle,"name",name)
               ier=idba_set (handle,"block",block)
!            ier=idba_set (handle,"station",station)
               if(lorog)then
                  ier=idba_set (handle,"height",h)
               endif

               ier=idba_set (handle,"rep_memo",'analisi')
               
               ier=idba_prendilo (handle)
               ier=idba_enq(handle,"*ana_id",id_ana)
! dati

               ier=idba_unsetall (handle)

               ier=idba_set(handle,"ana_id",id_ana)

            ! codice per le analisi
               ier=idba_set (handle,"rep_memo",'analisi')
            
! non ho previsto nessuno strato
               leveltype2=0
               l2=0
               livelli: select case(level(1))
               case(1) ! livello del suolo, per la pioggia
                  leveltype1=1
                  l1=0
               case(105) ! altezza specifica sopra al suolo (t e td 2m,v 10m)
                  leveltype1=103
                  l1=level(2)*1000 ! l'altezza deve essere in mm
               case(100) ! livello isobarico
                  leveltype1=100
                  l1=level(2)*100 ! la pressione deve essere in Pa
               case default
                  print*,'tipo livello non gestito'
                  call exit(1)
               end select livelli
               ier=idba_setlevel(handle,leveltype1,l1,leveltype2,l2)
               
               ier=idba_set (handle,"year",iyearv)
               ier=idba_set (handle,"month",imonthv)
               ier=idba_set (handle,"day",idayv)
               ier=idba_set (handle,"hour",ihourv)
               ier=idba_set (handle,"min",iminv)
               ier=idba_set (handle,"sec",00)
               
               ier=idba_settimerange(handle,pind,fctime,period)
               
            ! if(xgrid(pos(ib)).le.0.)xgrid(pos(ib))=0.
               dato=xgrid(pos(ib))
               ier=idba_set(handle,cvar,dato)
               ier=idba_prendilo (handle)

            endif

        enddo                  !nbox

    enddo                     !giri

    111 continue                  !fine grib

    ier=idba_fatto(handle)
    ier=idba_arrivederci(idbhandle)

    call pbclose(iug,ier)
    if(ier < 0)goto9500

    DEALLOCATE(xgrid)

    stop
    9001 print *,"Errore durante la lettura della namelist analisi"
    stop
    9002 print *,"Errore durante la lettura della namelist areaoss"
    stop
    9003 print *,"Errore durante la lettura della namelist odbc"
    stop
    9100 print *,"Errore durante la pbopen ",ier
    stop
    9300 print *,"Errore durante la getinfoest ",ier
    stop
    9400 print *,"Errore durante la getdata ",ier
    stop
    9500 print *,"Errore durante la pbclose ",ier
    stop
    9600 print *,"Errore durante la gribex ",ier
    stop
    9800 print *,"Errore durante la pbgrib ",ier
    stop

    end program


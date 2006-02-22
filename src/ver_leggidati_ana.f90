    program leggidati_ana

! c VERIFICA - leggidati_ana.f
! c programma per la lettura da file grib e scrittura su database mysql
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

    parameter (MNBOX=80000)
    parameter (MIDIMG=80000,MIDIMV=MIDIMG*4)

    real :: xb(MNBOX),yb(MNBOX),alte(MNBOX),h
    integer :: pos(MNBOX)
    integer :: scaddb(4),p1,p2
    integer :: block,station
    character name*20,cb*5
    logical :: forever

    character cvar*6,fileorog*80,vfile*80
    logical ::   ruota,area

    integer ::   xgrib(MIDIMG)
    real ::      xgrid(MIDIMV)
    integer ::   ksec0(2),ksec1(104),ksec2(22),ksec3(2),ksec4(42)
    real ::      psec2(10),psec3(2)
    integer ::   level(3),var(3),est(3),scad(4),data(3),ora(2)
    real ::      alat(4),alon(4)
    character(19) :: database,user,password

    data block/90/
    data forever/.true./

! database
    integer :: handler,handle
    logical :: init,debug,rmmiss
    data init,debug,rmmiss/.false.,.true.,.false./

    external error_handle

    namelist  /analisi/fileorog,vfile,ruota
    namelist  /areaoss/area,slon1,slon2,slat1,slat2
    namelist  /odbc/database,user,password

    open(1,file='odbc.nml',status='old',readonly)
    read(1,nml=odbc,err=9003)
    close(1)

    factz=1./9.81

    open(1,file='analisi.nml',status='old',readonly)
    read(1,nml=analisi,err=9001)
    close(1)

    open(1,file='areaoss.nml',status='old',readonly)
    read(1,nml=areaoss,err=9002)
    close(1)

    print*,'orografia e coordinate delle box'
    iug=0
    idimg=MIDIMG
    idimv=MIDIMV
    imd=-32768
    rmd=-1.5E21
    igrid=0                   !sono griglie regolari
    call pbopen(iug,fileorog,'r',ier)
    if(ier /= 0)goto9100
    call getinfoest(iug,xgrib,idimg,data,ora,scad,level, &
    var,est,alat(1),alat(2),alon(1),alon(2), &
    ny,nx,dy,dx,idrt,alarot,alorot,rot,ija,ier)
    if(ier /= 0)goto 9300
    call getdata(xgrib,idimg,imd,rmd,xgrid,idimv,ibm,ier)
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

    call idba_error_set_callback(0,error_handle,debug,handle_err)

    call idba_presentati(idbhandle,database,user,password)

    if (init)then
    ! solo se richiesta completa cancellazione iniziale
    ! o è la prima volta che si inseriscono i dati
        call idba_preparati(idhandle,handle, &
        "reuse","rewrite","rewrite")
        call idba_scopa(handle,"repinfo.csv")
        call idba_fatto(handle)
        rmmiss = .false.
    end if

    if (rmmiss) then
        call idba_preparati(idhandle,handle, &
        "reuse","rewrite","rewrite")
    else
        call idba_preparati(idhandle,handle, &
        "reuse","add","add")
    endif

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

        call getinfoest(iug,xgrib,idimg,data,ora,scad,level, &
        var,est,alat(1),alat(2),alon(1),alon(2), &
        ny,nx,dy,dx,idrt,alarot,alorot,rot,ija,ier)
        if(ier == -1)goto 111
        if(ier /= 0)goto 9300
        call getdata(xgrib,idimg,imd,rmd,xgrid,idimv,ibm,ier)
        if(ibm /= 0 .OR. ier /= 0)goto 9400

    ! INSERIMENTO DEI PARAMETRI NELL' ARCHIVIO
        call idba_unsetall (handle)

        call idba_seti (handle,"year",data(3))
        call idba_seti (handle,"month",data(2))
        call idba_seti (handle,"day",data(1))
        call idba_seti (handle,"hour",ora(1))
        call idba_seti (handle,"min",ora(2))
        call idba_seti (handle,"sec",00)

    ! codice per le analisi
        call idba_seti (handle,"rep_cod",105)

        call idba_seti (handle,"leveltype",level(1))
        call idba_seti (handle,"l1",level(2))
        call idba_seti (handle,"l2",level(3))

        if(scad(4) > 0)then
            call converti_scadenze(4,scad,scaddb)
            p1=0-(scaddb(3)-scaddb(2))
            p2=0
        else
            p1=0
            p2=0
        endif

        call idba_seti (handle,"pindicator",scad(4))
        call idba_seti (handle,"p1",p1)
        call idba_seti (handle,"p2",p2)

        call variabile(3,var,cvar,a,b,.true.)

        do ib=1,nbox

            rlon=xb(ib)
            rlat=yb(ib)
            h=alte(ib)
            write(cb,'(i5.5)')ib
            name='_gp'//cb
            station=ib

        ! prima faccio unset di ana_id senno' ricopre sempre!!!
            call idba_unset (handle,"ana_id")

            call idba_setc (handle,"name",name)
            call idba_seti (handle,"block",block)
            call idba_seti (handle,"station",station)

            call idba_setr (handle,"lon",rlon)
            call idba_setr (handle,"lat",rlat)
            call idba_setr (handle,"height",h)

            call idba_seti (handle,"mobile",0)

        ! if(xgrid(pos(ib)).le.0.)xgrid(pos(ib))=0.
            dato=xgrid(pos(ib))

            call idba_setr(handle,cvar,dato)
            call idba_prendilo (handle)

        enddo                  !nbox

    enddo                     !giri

    111 continue                  !fine grib

    call idba_fatto(handle)
    call idba_arrivederci(idbhandle)

    call pbclose(iug,ier)
    if(ier < 0)goto9500

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

    end program


    PROGRAM boxfix_dballe

! c VERIFICA - boxfix_dballe.f
! c programma per il calcolo di medie, massimi, percentili e distribuzioni
! c di previsti ed osservati all'interno di super-box non sovrapponentesi
! c autore: Chiara Marsigli
! c ultima modifica: 16 marzo 2004 - corrette coordinate box
! c 12 dicembre 2005: aggiornato per Dballe (ora fa anche l'ensemble)
! c 14 ottobre 2010: aggiornato per Dballe 4.19

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
    USE common_namelists

    parameter (MIDIMG=1200000)
    parameter (MNSTAZ=55000,MNGIO=150)
    parameter (MNBOX=750000)
    integer :: kgrib(MIDIMG)
    REAL, ALLOCATABLE :: xgrid(:),lsm(:),obm(:),oro(:)
    REAL, ALLOCATABLE :: rmgrid(:,:)
    real :: x(MNSTAZ),y(MNSTAZ),alt(MNSTAZ)
    real :: xb(MNBOX),yb(MNBOX)
    real :: xbox(MNSTAZ),ybox(MNSTAZ),altbox(MNSTAZ)
! ATTENZIONE!!!! da sistemare!!!!!
    integer :: block,station
    character(20) :: name
    character(5) :: cb
    real :: xpmod(MNBOX),ypmod(MNBOX)
    real :: rlon,rlat,h
    real :: obsst(MNSTAZ)   ! sulle stazioni
    real :: obs(MNSTAZ),pred(MNSTAZ,MNRM) ! nelle box
    integer :: level(3),var(3),est(3),scad(4),ora(2)
    integer :: dataval(3),oraval(2)
    real :: a,b,dato
    INTEGER :: iscaddb,scaddb(4),idummy(2),npmod
    integer :: leveltype1,l1,leveltype2,l2
    integer :: pind,fctime,period
    real :: alat(4),alon(4)
    character(len=80) :: vfile,obmfile
    character :: cel*3,descrfisso*20
    character(len=20) :: descr
    real :: dum
    integer :: lsvar,ivor
    INTEGER :: id_area_box(MNBOX),id_area_st(MNSTAZ),id_area_mod(MNBOX) !per le medie su aree
! the other namelist variables are in the module
    real :: pblon=10.,pblat=45.
    integer :: ni=1,nj=1

    integer :: ier

    integer :: ksec0(2),ksec1(104),ksec2(384),ksec3(2),ksec4(60)
    REAL :: psec2(384),psec3(2),dummy(1)

    data ksec0/2*0/
    data ksec1/104*0/
    data ksec2/384*0/
    data ksec3/2*0/
    data ksec4/60*0/
! ksec4(5)=0 per real data
    data kgrib/MIDIMG*0/
    data psec2/384*0./
    data psec3/2*0./

! database
    INTEGER :: handler,handlew,handleanar,handleanaw,id_ana
    integer :: debug=1
    integer :: handle_err

! namelist dove si specificano il punto in basso a sinistra (pblon,pblat)
! e il numero di punti in x ed in y (ni,nj) dell'area da coprire di box
! il passo e' dxb,dyb
    namelist /areabox/pblon,pblat,ni,nj

    data level/-1,-1,-1/, var/-1,-1,-1/, est/-1,-1,-1/, &
    scad/-1,-1,-1,-1/, ora/-1,-1/
    data rmdo/-999.9/,rmddb/32767./,block/80/
! block vale 80 per le pseudostazioni box

    print*,'program boxfix'

    open(1,file='odbc.nml',status='old')
    read(1,nml=odbc,err=9001)
    close(1)
    open(1,file='stat.nml',status='old')
    read(1,nml=stat,err=9002)
    close(1)
    open(1,file='areabox.nml',status='old')
    read(1,nml=areabox,err=9013)
    close(1)

    do ist=1,MNSTAZ
        x(ist)=rmdo
        y(ist)=rmdo
        alt(ist)=rmdo
    enddo

! gestione degli errori
    ier=idba_error_set_callback(0,C_FUNLOC(idba_default_error_handler),debug,handle_err)

! connessione con database
    ier=idba_presentati(idbhandle,database)
    PRINT*,'aperto database ',database

! apertura database in scrittura
    ier=idba_preparati(idbhandle,handlew,"write","write","write")
! apertura database in scrittura anagrafica
    ier=idba_preparati(idbhandle,handleanaw,"write","write","write")

    CALL modello(model,ivlsm,ivor,0,.FALSE.,.FALSE.)
    PRINT*,'ivlsm per obm ',ivlsm
    IF(diffh .OR. corrq .OR. (ls >= 0))THEN
      CALL modello(model,ivlsm,ivor,ls,diffh,corrq)
      PRINT*,'ivlsm ',ivlsm,' ivor ',ivor
    ENDIF

! il tipo di elaborazione e' fisso per questa routine (=2)
    call descrittore(model,itipo,imod,ls,media,massimo,prob, &
    distr,dxb,dyb,descr)
    print*,'descrittore ',descr
    descrfisso=descr

    vfile='estratti.grib'

! lettura grib allo scopo di avere MIDIMV (ksec4(1))
! new version
!    call grib_open_file(ifile,vfile,'r')
!
!    call grib_new_from_file(ifile,igrib, iret)
!   Loop on all the messages in a file.
!    call grib_get(igrib,'numberOfDataPoints',MIDIMV)
!    write(*,'(i7)') MIDIMV
!    
!    print*,'MIDIMV',MIDIMV
!
!    call grib_close_file(ifile)

! old version
    iug=0
    call pbopen(iug,vfile,'r',ier)
    if(ier /= 0)goto9100
    CALL pbgrib(iug,kgrib,MIDIMG,idimg,ier)
    if(ier /= 0)goto9800
    CALL gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
     dummy,SIZE(dummy), &
     kgrib,MIDIMG,idimg,'J',ier)
    if(ier /= 0)goto9700
    MIDIMV=ksec4(1)
    call pbclose(iug,ier)
    if(ier /= 0)goto9500

    ALLOCATE(xgrid(MIDIMV))
    ALLOCATE(lsm(MIDIMV))
    ALLOCATE(oro(MIDIMV))
    ALLOCATE(obm(MIDIMV))
    ALLOCATE(rmgrid(MIDIMV,MNRM))

    iug=0
    idimg=MIDIMG
    idimv=MIDIMV
    imd=-32768
    rmd=-1.5E21
    igrid=0                     !sono griglie regolari
    obmfile='obm_'//model(1:nlenvera(model))//'.grib'
    PRINT*,'obmfile= ',obmfile

! queste sono le box sulle quali fare i conti
    blat=pblat
    do j=1,nj
        blon=pblon
        do i=1,ni
            ib=i+ni*(j-1)
            xb(ib)=blon
            yb(ib)=blat
            blon=blon+dxb
        enddo
        blat=blat+dyb
    enddo
    nbox=ib
    print*,'numero totale box ',nbox
    do ib=1,nbox
        print*,xb(ib),yb(ib)
    enddo

! leggo le coordinate del modello
    call leggibox(vfile,MNBOX,xpmod,ypmod,npmod,alorot,alarot, &
    ruota,.false.,dum,dum,dum,dum)
    print*,'numero totale punti modello ',npmod

    call rot_grib_LAMBO(alorot,alarot,rxeq,ryeq)
    cryeq=cos(ryeq*3.1415927/180.)
    sryeq=sin(ryeq*3.1415927/180.)
! se le coordinate del modello sono ruotate, le antiruoto
    if(ruota)then
        do ip=1,npmod
            call rtlld(xpmod(ip),ypmod(ip),rxeq,cryeq,sryeq,rlon,rlat)
            xpmod(ip)=rlon
            ypmod(ip)=rlat
        enddo
    endif

! leggo la observation mask
    if(lobm == 1)then
        call pbopen(iug,obmfile,'r',ier)
        if(ier /= 0)goto 9100
        var(3)=ivlsm
        call findgribest(iug,kgrib,idimg,data,ora, &
        scad,level,var,est,ier)
        if(ier == -1)then
            print*,'non trovo la observation mask! '
            PRINT*,data,ora,scad,level,var,est
            call exit(1)
        elseif(ier /= 0)then
            goto 9200
        endif
        call getdata(kgrib,idimg,imd,rmd,obm,idimv, &
        ibm,ier)
        if(ibm /= 0 .OR. ier /= 0)goto 9400
        call pbclose(iug,ier)
        if(ier < 0)goto9500
    else
        do iv=1,MIDIMV
            obm(iv)=1.
        enddo
    endif

    IF(laree)THEN
! leggo l'appartenenza alle aree (bacini) dei punti del modello
!!!      CALL selarea_grib(vfile,shapefile,id_area_mod)
!!!      PRINT*,PACK(id_area_mod,mask=(id_area_mod > 0))
    ENDIF

    call pbopen(iug,vfile,'r',ier)
    if(ier /= 0)goto 9100

! leggo la land-sea mask
    if(ls >= 0)then
        var(3)=ivlsm
        call findgribest(iug,kgrib,idimg,data,ora, &
        scad,level,var,est,ier)
        if(ier == -1)then
            print*,'non trovo la land-sea mask! '
            call exit(1)
        elseif(ier /= 0)then
            goto 9200
        endif
        call getdata(kgrib,idimg,imd,rmd,lsm,idimv, &
        ibm,ier)
        if(ibm /= 0 .OR. ier /= 0)goto 9400
    endif
! leggo l'orografia
    IF(diffh .OR. corrq)THEN
        var(3)=ivor
        call findgrib(iug,kgrib,idimg,data,ora, &
        scad,level,var,ier)
        if(ier == -1)then
            print*,'non trovo orografia! '
            call exit(1)
        elseif(ier /= 0)then
            goto 9200
        endif
        call getdata(kgrib,idimg,imd,rmd,oro,idimv, &
        ibm,ier)
        if(ibm /= 0 .OR. ier /= 0)goto 9400
    endif

! Read parameters
    open(1,file='parameters.nml',status='old')
    read(1,nml=parameters,err=9001)
    close(1)
    if(nrm > MNRM)then
        print*,'ERRORE! nrm MAGGIORE DI MNRM!'
        call exit (2)
    endif
    ora(1)=nora/100.
    ora(2)=mod(nora,100)

    write(*,*)'nrm ',nrm

    open(1,file='scadenze.nml',status='old')
    read(1,nml=scadenza,err=9003)
    close(1)

    call cleankey(2,3,4,level,var,est,scad,data,idummy)
    open(1,file='parametro.nml',status='old')
    read(1,nml=parametro,err=9012)
    close(1)
    do i=1,3
        var(i)=kvar(i,1)
    enddo
    call variabile(3,var,cvar,a,b,.true.)
    lsvar=ls
    write(*,*)'variabile ',var,' cvar ',cvar, &
    ' lsvar ',lsvar,' a ',a,' b ',b

    IF(imet == 0 .OR. imet == 1)THEN         ! scalare

    ! Read date
      OPEN(3,file='date.nml',status='old')
      DO igio=1,ngio
        READ(3,nml=date,err=9004)
        ora(1)=nora/100.
        ora(2)=MOD(nora,100)
        PRINT*,'data ',DATA,' ora ',ora
        WRITE(88,*)DATA
        DO iscad=1,nscad
          DO is=1,4
            scad(is)=scadenze(is,iscad)
          ENDDO
          PRINT*,'scadenza ',scad
          WRITE(88,*)scad

! gestione scadenze
! scad(4) (ksec1(18)) e' posto pari a 13 per esprimere le scadenze relative 
! a piogge analizzate: p1 e p2 vanno considerati all'indietro a partire da
! data e ora del GRIB (es: p1=0 e p2=1 significa andare indietro di 1 ora)
          IF(scad(4) == 13)THEN
            iscaddb=0
          ELSE
          ! fisso la scadenza cui chiedere le osservazioni (data e ora di validita')
          ! la scadenza puo' essere al secondo o al terzo posto, nell'altro
          ! o c'e' 0 o c'e' l'estremo inferiore dell'intervallo di cumulazione
            iscaddb=MAX(scadenze(2,iscad),scadenze(3,iscad))
          ENDIF

          CALL JELADATA5(DATA(1),DATA(2),DATA(3), &
           ora(1),ora(2),iminuti)
          iminuti=iminuti+iscaddb*60
          CALL JELADATA6(iday,imonth,iyear, &
           ihour,imin,iminuti)
          dataval(1)=iday
          dataval(2)=imonth
          dataval(3)=iyear
          oraval(1)=ihour
          oraval(2)=imin
          
          DO iore=1,nore
            
            IF(iore > 1)THEN
              ! trovo data e ora dell'emissione per ore successive (analisi)
              ora(1)=ore(iore)/100.
              ora(2)=MOD(ore(iore),100)
              CALL JELADATA5(DATA(1),DATA(2),DATA(3), &
               ora(1),ora(2),iminuti)
              iminuti=iminuti+iscaddb*60
              CALL JELADATA6(iday,imonth,iyear, &
               ihour,imin,iminuti)
              dataval(1)=iday
              dataval(2)=imonth
              dataval(3)=iyear
              oraval(1)=ihour
              oraval(2)=imin
            ENDIF
            
            PRINT*,'validita'' ',dataval,oraval,iscaddb
            PRINT*,'emissione ',DATA,ora,iscaddb
            
            DO irm=1,nrm
              est(1)=-1
              est(2)=-1
              est(3)=-1
              CALL findgribest(iug,kgrib,idimg,DATA,ora, &
               scad,level,var,est,ier)
              IF(ier == -1)THEN
                PRINT*,'grib mancante - azzero tutto l''ensemble'
                GOTO 111
              ELSEIF(ier /= 0)THEN
                GOTO 9200
              ENDIF
              CALL getinfoest(-1,kgrib,idimg,DATA,ora,scad,level, &
               var,est,alat(1),alat(2),alon(1),alon(2), &
               ny,nx,dy,dx,idrt,alarot,alorot,rot,ija,ier)
              IF(ier /= 0)GOTO 9300
              CALL getdata(kgrib,idimg,imd,rmd,xgrid,idimv, &
               ibm,ier)
              IF(ibm /= 0 .OR. ier /= 0)GOTO 9400
              DO iv=1,idimv
                
                ! converto i previsti nell'unita' di misura prevista dalla Blocale
                rmgrid(iv,irm)=a+xgrid(iv)*b
              ENDDO
            ENDDO            ! nrm
            
            ! leggo le osservazioni presenti in archivio per questa scadenza
            DO istaz=1,MNSTAZ
              obsst(istaz)=rmddb
            ENDDO
            
            ! apertura database in lettura
            ier=idba_preparati(idbhandle,handler,"read","read","read")
            ! apertura database in lettura anagrafica
            ier=idba_preparati(idbhandle,handleanar,"read","read","read")
            ! le box sono gia' antiruotate e le stazioni le leggo dal db normali
            CALL leggioss_db(handler,handleanar,3,2, &
             dataval,oraval,cvar,scad, &
             rxeq,ryeq,.FALSE.,rmddb, &
             MNSTAZ,x,y,alt,nstdispo,obsst)
            ! esce il dato su punto eventualmente ruotato nell'unita' di misura
            ! in cui e' rappresentato nel database
            ier=idba_fatto(handler)
            ier=idba_fatto(handleanar)

            ! c ATTENZIONE!!! ora la thr da usare con prob va nella stessa unita'
            ! c di misura, cioe' in quella del db, cioe' in quella specificata
            ! c nella tabella B!!!

            IF(laree)THEN
              nbox=1
              xbox(1)=20.0
              ybox(1)=50.0
              id_area_box=1
              id_area_st=1
              ! calcolo su aree predefinite (bacini)
              CALL medaree(MIDIMV,MNRM,MNSTAZ,MNBOX, &
               id_area_box,nbox,alt,id_area_st,obsst,rmgrid, &
               npmod,id_area_mod, &
               nminobs,rmddb,rmd,rmdo,nrm,media,massimo,prob, &
               distr,perc, &
               lsm,lsvar,obm,thr,obs,pred,nb,altbox)              
            ELSE

            ! calcolo sulle box
              CALL medbox(MIDIMV,MNRM,MNSTAZ,MNBOX, &
               xb,yb,nbox,dxb,dyb,x,y,alt,obsst,rmgrid, &
               npmod,xpmod,ypmod, &
               nminobs,rmddb,rmd,rmdo,nrm,media,massimo,prob, &
               distr,perc, &
               lsm,lsvar,obm,thr,obs,pred,nb,xbox,ybox,altbox)
              
            ENDIF

            DO ib=1,nb
              WRITE(88,*)obs(ib),pred(ib,1),xbox(ib),ybox(ib), &
               altbox(ib)
            ENDDO
            
            ! conversione delle scadenze in secondi (e correzione scadenze sbagliate)
            CALL converti_scadenze(4,scad,scaddb)
            
            scadenzep: select case(scaddb(4))
            case(4) ! cumulata
               pind=1
               fctime=scaddb(3)
               period=scaddb(3)-scaddb(2)
            case(0) ! istantanea
               pind=254
               if(scaddb(3)/=0)then
                  print*,'case 1 - p1= ',scaddb(2),' p2= ',scaddb(3)
                  call exit(1)
               endif
               fctime=scaddb(2)
               period=0
            case(1) ! analisi inizializzata
               pind=254
               if(scaddb(2)/=0)then
                  print*,'case 1 - p1= ',scaddb(2),' p2= ',scaddb(3)
                  call exit(1)
               endif
               fctime=scaddb(2)
               period=0
            case(2) ! prodotto valido in un periodo
               pind=205
               fctime=scaddb(3)
               period=scaddb(3)-scaddb(2)
            case(13) ! analisi di precipitazione
               pind=1
               print*,'controllo - verrebbe fctime= ',scaddb(2),' period= ',scaddb(3)
               fctime=scaddb(2)
               period=scaddb(3)
            case default
               print*,'scadenza non gestito'
               call exit(1)
            end select scadenzep
            ier=idba_settimerange(handlew,pind,fctime,period)

            ier=idba_set (handlew,"year",dataval(3))
            ier=idba_set (handlew,"month",dataval(2))
            ier=idba_set (handlew,"day",dataval(1))
            ier=idba_set (handlew,"hour",oraval(1))
            ier=idba_set (handlew,"min",oraval(2))
            ier=idba_set (handlew,"sec",0)
            
            ! scrittura su database
            ! scrivo i previsti
            DO irm=1,nrm
              IF(nrm > 1)THEN
                WRITE(cel,'(i3.3)')irm
                descr=descrfisso(1:nlenvera(descrfisso)) &
                 //'el'//cel
              ELSE
                descr=descrfisso
              ENDIF
              PRINT*,'descr ',descr
              DO ib=1,nb
                IF(pred(ib,irm) /= rmddb .AND. &
                 pred(ib,irm) /= rmdo)THEN
                  rlat=ybox(ib)
                  rlon=xbox(ib)
                  h=altbox(ib)
                  WRITE(cb,'(i5.5)')ib
                  name='_box'//cb
                  station=ib
                  
! imposto tutta l'anagrafica                  

                  ier=idba_unsetall(handleanaw)
                  ier=idba_setcontextana(handleanaw)

                  ier=idba_set (handleanaw,"rep_memo",descr)

                  ier=idba_set (handleanaw,"lat",rlat)
                  ier=idba_set (handleanaw,"lon",rlon)
                  ier=idba_set (handleanaw,"mobile",0)

                  ier=idba_set (handleanaw,"name",name)
                  ier=idba_set (handleanaw,"block",BLOCK)
                  ier=idba_set (handleanaw,"station",station)
!                  if (abs(h - rmdo) > 0.1)then
!                     ier=idba_set (handleanaw,"height",h)
!                  endif

                  ier=idba_prendilo (handleanaw)
                  
!                  ier=idba_enq (handleanaw,"*ana_id",id_ana)

! ora scrivo i dati previsti

                  ier=idba_unsetall(handlew)
                  
                  ier=idba_settimerange(handlew,pind,fctime,period)
                  
                  ier=idba_set (handlew,"year",dataval(3))
                  ier=idba_set (handlew,"month",dataval(2))
                  ier=idba_set (handlew,"day",dataval(1))
                  ier=idba_set (handlew,"hour",oraval(1))
                  ier=idba_set (handlew,"min",oraval(2))
                  ier=idba_set (handlew,"sec",0)

                  ier=idba_set (handlew,"lat",rlat)
                  ier=idba_set (handlew,"lon",rlon)
                  ier=idba_set (handlew,"rep_memo",descr)
!                  ier=idba_set (handlew,"ana_id",id_ana)

                  leveltype2=dba_mvi
                  l2=dba_mvi
                  livelli1: select case(level(1))
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
                     print*,'livello non gestito'
                     call exit(1)
                  end select livelli1
                  ier=idba_setlevel(handlew,leveltype1,l1,leveltype2,l2)
                  
                  IF(imet == 0)THEN ! scalare
                    
                    ! attenzione!!!!!! ho bisogno che il minimo sia 0????
                    ! niente conversione!!! Viene fatta in lettura!
                     dato=pred(ib,irm)
! provvisiorio per caricare le preci leggermente negative
                    if(dato <0)then
                       dato=0.
                    endif
                    
                  ELSEIF(imet == 1)THEN !scalare direzione
                    
                    IF(pred(ib,irm) <= 1. .AND. &
                     pred(ib,irm) /= 0.)THEN
                      dato=1
                    ELSE
                      dato=pred(ib,irm)
                    ENDIF
                    
                  ENDIF
                  
                  ier=idba_set (handlew,cvar,dato)
                  ier=idba_prendilo (handlew)
                  
                ENDIF      !previsti
              ENDDO         !nb
            ENDDO            !nrm
            
            ! scrivo gli osservati
            nlm=nlenvera(model)
            descr='oss'//descrfisso((nlm+1):(nlm+5))
            PRINT*,descr,dataval,oraval
            DO ib=1,nb
              IF(obs(ib) /= rmddb .AND. &
               obs(ib) /= rmdo)THEN
                rlat=ybox(ib)
                rlon=xbox(ib)
                h=altbox(ib)
                WRITE(cb,'(i5.5)')ib
                name='_box'//cb
                station=ib

            scadenzeo: select case(scaddb(4))
            case(4) ! cumulata
               pind=1
               fctime=0
               period=scaddb(3)-scaddb(2)
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
            case(13) ! analisi di precipitazione
               pind=1
               print*,'controllo - verrebbe fctime= ',scaddb(2),' period= ',scaddb(3)
               fctime=0
               period=scaddb(3)-scaddb(2)
            case default
               print*,'scadenza non gestito'
               call exit(1)
            end select scadenzeo
                
! il time range indicator speciale per le preci analizzate e' 13, ma in database
! deve essere comunque 4

! imposto tutta l'anagrafica
                
                ier=idba_unsetall(handleanaw)
                ier=idba_setcontextana (handleanaw)

                ier=idba_set (handleanaw,"rep_memo",descr)

                ier=idba_set (handleanaw,"lat",rlat)
                ier=idba_set (handleanaw,"lon",rlon)
                ier=idba_set (handleanaw,"mobile",0)

                ier=idba_set (handleanaw,"name",name)
                ier=idba_set (handleanaw,"block",BLOCK)
                ier=idba_set (handleanaw,"station",station)                
!                if (abs(h - rmdo) > 0.1)then
!                   ier=idba_set (handleanaw,"height",h)
!                endif

                ier=idba_prendilo (handleanaw)
!                ier=idba_enq (handleanaw, "*ana_id", id_ana)
                
! ora scrivo i dati osservati

                ier=idba_unsetall(handlew)
                ier=idba_set (handlew,"lat",rlat)
                ier=idba_set (handlew,"lon",rlon)
                ier=idba_set (handlew,"rep_memo",descr)
 !               ier=idba_set (handlew, "ana_id", id_ana)

                ier=idba_settimerange(handlew,pind,fctime,period)
                
                ier=idba_set (handlew,"year",dataval(3))
                ier=idba_set (handlew,"month",dataval(2))
                ier=idba_set (handlew,"day",dataval(1))
                ier=idba_set (handlew,"hour",oraval(1))
                ier=idba_set (handlew,"min",oraval(2))
                ier=idba_set (handlew,"sec",0)

                leveltype2=dba_mvi
                l2=dba_mvi
                livelli2: select case(level(1))
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
                   print*,'livello non gestito'
                   call exit(1)
                end select livelli2
                ier=idba_setlevel(handlew,leveltype1,l1,leveltype2,l2)
                                                
                IF(imet == 0)THEN ! scalare
                  
                  ! attenzione!!!!!! ho bisogno che il minimo sia 0????
                  ! niente conversione!!! Viene fatta in lettura!
                  dato=obs(ib)
                  
                ELSEIF(imet == 1)THEN !scalare direzione
                  
                  IF(obs(ib) <= 1. .AND. &
                   obs(ib) /= 0.)THEN
                    dato=1
                  ELSE
                    dato=obs(ib)
                  ENDIF
                  
                ENDIF
                
                ier=idba_set (handlew,cvar,dato)
                ier=idba_prendilo (handlew)
                
              ENDIF        !osservati
            ENDDO          !nb
            
111         CONTINUE
            
          ENDDO            !nore
        ENDDO              !nscad
      ENDDO                !ngio
      CLOSE(3)             !date
    ENDIF                  !cvar
    
    CLOSE(1)
    call pbclose(iug,ier)
    if(ier < 0)goto9500

! chiusura database
! chiusura database
    ier=idba_fatto(handlew)
    ier=idba_fatto(handleanaw)
    ier=idba_arrivederci(idbhandle)

    stop
    9001 print *,"Errore durante la lettura della namelist parameters ",ier
    stop
    9012 print *,"Errore durante la lettura della namelist parametro ",ier
    stop
    9013 print *,"Errore durante la lettura della namelist areabox ",ier
    stop
    9002 print *,"Errore durante la lettura della namelist stat ",ier
    stop
    9003 print *,"Errore durante la lettura della namelist scadenza ",ier
    stop
    9004 print *,"Errore durante la lettura della namelist date ",ier
    stop
    9100 print *,"Errore durante la pbopen ",ier
    stop
    9200 print *,"Errore durante la findgribest ",ier
    stop
    9300 print *,"Errore durante la getinfoest ",ier
    stop
    9400 print *,"Errore durante la getdata ",ier
    stop
    9500 print *,"Errore durante la pbclose ",ier
    stop
    9700 print *,"Errore durante la gribex ",ier
    stop
    9800 print *,"Errore durante la pbgrib ",ier
    stop
    END PROGRAM boxfix_dballe

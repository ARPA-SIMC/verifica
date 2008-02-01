    program interpola_dballe

! c VERIFICA - interpola_dballe.f
! c programma per l'interpolazione degli output dei modelli su punti sparsi
! c autore: Chiara Marsigli
! c 26 febbraio 2003 - corretta gestione ngetpoint per uso
! c maschera mare-terra
! c 31 luglio 2003 - modifica alla subroutine leggiana_db per scegliere se
! c leggere le osservazioni (itipostaz=0) o l'analisi (itipostaz=2)
! c 12 genaio 2005 - aggiunta gestione verifica di analisi
! c estate 2005 - adattamento al nuovo database Dballe
! c 6 dicembre 2005 - sistemata gestione ensemble
! c 9 dicembre 2005 - tolto il ciclo sulle variabili (aggiunto in verifica_batch)
! c e inserito imet

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

    parameter (MIDIMG=1200000)
    parameter (MNSCAD=72,MNGIO=150,MNRM=102)
    parameter (MNBOX=150000)
    integer :: kgrib(MIDIMG)
    REAL, ALLOCATABLE :: xgrid(:),lsm(:),oro(:)
    REAL, ALLOCATABLE :: xgridu(:),xgridv(:)
    integer :: level(3),var(3),est(3),scad(4),ora(2)
    integer :: dataval(3),oraval(2),scaddb(4)
    integer :: varv(3)
    real :: a,b,dato
    integer :: iscaddb
    real :: alat(4),alon(4)
    character :: vfile*60,cvar*6,cvarv*6,cel*3,descrfisso*20
    character(len=20) :: descr
    real :: xpmod(MNBOX),ypmod(MNBOX)
    real :: hdiff,rdum
    logical :: wind
    INTEGER :: lsvar,ivor,dum(2)
! namelist variables
    integer :: nora=0000,ngio=1,nscad=1,scad1=1,scad2=1,inc=1
    integer :: nvar=1,nrm=1,nore=1,ore(24)=0000
    integer :: data(3)=(/-1,-1,-1/),scadenze(4,MNSCAD)=-1,kvar(3,2)=-1
    character(len=10) :: model=''
    integer :: itipo=1,iana=0,imet=0,imod=0,ls=-1,nminobs=1
    logical :: ruota=.false.,diffh=.false.
    logical :: media=.false.,massimo=.false.,prob=.false.,distr=.false.
    real :: dxb=1.0,dyb=1.0,diffmax=100.,thr=1.,perc=50.
    character(19) :: database='',user='',password=''

    REAL, ALLOCATABLE :: x(:),y(:),alt(:)
    REAL, ALLOCATABLE :: xstaz(:,:),xstazv(:,:)

! grib fields
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
    INTEGER :: handler,handle
    INTEGER :: debug=1
    INTEGER :: handle_err

    COMMON /point/ij1,ij2,ij3,ij4
    
    NAMELIST /parameters/nora,ngio,nscad,scad1,scad2,inc, &
     nvar,nrm,nore,ore
    NAMELIST /date/DATA
    NAMELIST /scadenza/scadenze
    NAMELIST /parametro/kvar
    NAMELIST /stat/model,itipo,iana,imet,imod,ls, &
     ruota,nminobs,media,massimo,prob,distr,dxb,dyb, &
     diffh,diffmax,thr,perc
    NAMELIST /odbc/database,user,password

    DATA level/-1,-1,-1/, var/-1,-1,-1/, est/-1,-1,-1/, &
     scad/-1,-1,-1,-1/, ora/-1,-1/, varv/-1,-1,-1/
    DATA rmdo/-999.9/

    PRINT*,'program interpola'

    OPEN(1,file='odbc.nml',status='old')
    READ(1,nml=odbc,err=9001)
    CLOSE(1)
    OPEN(1,file='stat.nml',status='old')
    READ(1,nml=stat,err=9002)
    CLOSE(1)

! gestione degli errori
    CALL idba_error_set_callback(0,idba_default_error_handler,debug,handle_err)
    
! connessione con database
    CALL idba_presentati(idbhandle,database,user,password)
    
! apertura database in lettura
    CALL idba_preparati(idbhandle,handler,"read","read","read")
    
    CALL idba_quantesono(handler,nstaz)
    PRINT*,'massimo numero pseudo-stazioni ',nstaz

! allocazione matrici
    ALLOCATE(x(1:nstaz))
    ALLOCATE(y(1:nstaz))
    ALLOCATE(alt(1:nstaz))

! leggo tutte le stazioni disponibili
    call leggiana_db(iana,x,y,alt,rmdo,nstaz,handler)

    IF(diffh .OR. (ls >= 0))THEN
      CALL modello(model,ivlsm,ivor,ls,diffh)
      PRINT*,' ivlsm ',ivlsm,' ivor ',ivor
    ENDIF

! il tipo di elaborazione e' fisso per questa routine (=1)
    call descrittore(model,itipo,imod,ls,media,massimo,prob, &
    distr,dxb,dyb,descr)
    print*,'descrittore ',descr
    descrfisso=descr

! apertura database in scrittura
    call idba_preparati(idbhandle,handle,"write","write","write")

    vfile='estratti.grib'
! lettura grib allo scopo di avere MIDIMV (ksec4(1))
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
    ALLOCATE(xgridu(MIDIMV))
    ALLOCATE(xgridv(MIDIMV))

    iug=0
    idimg=MIDIMG
    idimv=MIDIMV
    imd=-32768
    igrid=0                    !sono griglie regolari
    call pbopen(iug,vfile,'r',ier)
    if(ier /= 0)goto9100

! leggo le coordinate del modello
    CALL leggibox(vfile,MNBOX,xpmod,ypmod,npmod,alorot,alarot, &
     ruota,.FALSE.,rdum,rdum,rdum,rdum)
    print*,'numero totale punti modello ',npmod

! leggo la land-sea mask
    if (ls >= 0) then
        var(3)=ivlsm
        PRINT*,DATA,ora,scad,level,var,est
        call findgribest(iug,kgrib,idimg,data,ora, &
        scad,level,var,est,ier)
        if(ier == -1)then
            print*,'non trovo la land-sea mask! '
            call exit(1)
        elseif(ier /= 0)then
            goto 9200
        endif
        call getdata(kgrib,idimg,imd,rmdo,lsm,idimv, &
        ibm,ier)
        if(ibm /= 0 .OR. ier /= 0)goto 9400
    endif
! leggo l'orografia
    if(diffh)then
        var(3)=ivor
        call findgrib(iug,kgrib,idimg,data,ora, &
        scad,level,var,ier)
        if(ier == -1)then
            print*,'non trovo orografia! '
            call exit(1)
        elseif(ier /= 0)then
            goto 9200
        endif
        call getdata(kgrib,idimg,imd,rmdo,oro,idimv, &
        ibm,ier)
        if(ibm /= 0 .OR. ier /= 0)goto 9400
    endif

! Read parameters
    open(1,file='parameters.nml',status='old')
    read(1,nml=parameters,err=9001)
    close(1)
    ora(1)=nora/100.
    ora(2)=mod(nora,100)
    print*,'nrm ',nrm
    print*,'nvar in parameters ',nvar

! allocazione matrici
    ALLOCATE(xstaz(1:nstaz,1:nrm))
    ALLOCATE(xstazv(1:nstaz,1:nrm))

    open(1,file='scadenze.nml',status='old')
    read(1,nml=scadenza,err=9003)
    close(1)

    call cleankey(2,3,4,level,var,est,scad,data,dum)
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
      
      wind=.FALSE.
      
      ! Read date
      OPEN(3,file='date.nml',status='old')
      DO igio=1,ngio
        READ(3,nml=date,err=9004)
        ora(1)=nora/100.
        ora(2)=MOD(nora,100)
        PRINT*,'data ',DATA,' ora ',ora
        DO iscad=1,nscad
          DO is=1,4
            scad(is)=scadenze(is,iscad)
          ENDDO
          PRINT*,'scadenza ',scad
          
! gestione scadenze
! scad(4) (ksec1(18)) e' posto pari a 13 per esprimere le scadenze relative 
! a piogge analizzate: p1 e p2 vanno considerati all'indietro a partire da
! data e ora del GRIB (es: p1=0 e p2=1 significa andare indietro di 1 ora)
          IF(scad(4) == 13)THEN
            iscaddb=0
          ELSE
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
                PRINT*,'grib mancante - azzero ', &
                 'tutto l''ensemble'
                xstaz=rmdo
                GOTO 111
              ELSEIF(ier /= 0)THEN
                GOTO 9200
              ENDIF
              CALL getinfoest(-1,kgrib,idimg, &
               DATA,ora,scad,level,var,est, &
               alat(1),alat(2),alon(1),alon(2), &
               ny,nx,dy,dx,idrt,alarot,alorot,rot,ija,ier)
              IF(ier /= 0)GOTO 9300
              CALL getdata(kgrib,idimg,imd,rmdo,xgrid,idimv, &
               ibm,ier)
              IF(ibm /= 0 .OR. ier /= 0)GOTO 9400
              IF(lsvar == -1)THEN
                lsvar=1
                DO ivec=1,idimv
                  IF(xgrid(ivec) == rmdo)THEN
                    lsm(ivec)=0
                  ELSE
                    lsm(ivec)=1
                  ENDIF
                ENDDO
              ELSEIF(lsvar == 0)THEN
                DO ivec=1,idimv
                  IF (xgrid(ivec) == rmdo .OR. lsm(ivec)==1)THEN
                    lsm(ivec)=1
                  ELSE
                    lsm(ivec)=0
                  ENDIF
                ENDDO
              ELSEIF(lsvar == 1)THEN
                DO ivec=1,idimv
                  IF (xgrid(ivec) == rmdo .OR. lsm(ivec)==0)THEN
                    lsm(ivec)=0
                  ELSE
                    lsm(ivec)=1
                  ENDIF
                ENDDO
              ENDIF
              
           ! Interpolation of predicted data on (lat,lon) station points
              IF(ruota)THEN
                CALL rot_grib_LAMBO(alorot,alarot, &
                 tlm0d,tph0d)
              ENDIF
              DO ist=1,nstaz
                IF(ABS(x(ist)-rmdo) > 0.1 .AND. &
                 ABS(y(ist)-rmdo) > 0.1)THEN

                  CALL ngetpoint(x(ist),y(ist), &
                   xgrid,xgrid,lsm, &
                   idimv,nx,ny,alon(1),alat(1),dx,dy, &
                   igrid,ija,tlm0d,tph0d,wind,imod, &
                   lsvar,xint,dummy,ier)
                  
                  IF(ier == 2 .OR. ier == 4)THEN
           ! cerca di interpolare su un punto che non e' nel dominio dei dati!
                    xstaz(ist,irm)=rmdo
                  ELSEIF(ier == 0)THEN                  
                    IF (diffh) THEN
                      ind=ij1
                      hdiff=ABS(oro(ind)/9.81-alt(ist))
                      IF (hdiff <= diffmax) THEN
                        xstaz(ist,irm)=xint
                      ELSE
                        xstaz(ist,irm)=rmdo
                      ENDIF
                    ELSE
                      xstaz(ist,irm)=xint
                    ENDIF
                  ELSE
                    GOTO 9600
                  ENDIF
                ENDIF
              ENDDO
            ENDDO         !nrm
            
           ! conversione delle scadenze in secondi (e correzione scadenze sbagliate)
            CALL converti_scadenze(4,scad,scaddb)
            
            IF(scaddb(4) == 13) THEN
              wp1=0-scaddb(3)
              wp2=0
              wpind=4
              CALL idba_set (handle,"p1",wp1)
              CALL idba_set (handle,"p2",wp2)
              CALL idba_set (handle,"pindicator",wpind)
            ELSE
              CALL idba_set (handle,"p1",scaddb(2))
              CALL idba_set (handle,"p2",scaddb(3))
              CALL idba_set (handle,"pindicator",scaddb(4))
            ENDIF
            
            CALL idba_setdate(handle,dataval(3),dataval(2),dataval(1),oraval(1),oraval(2),0)
            
            ! scrittura su database
            DO irm=1,nrm
              IF(nrm > 1)THEN
                WRITE(cel,'(i3.3)')irm
                descr=descrfisso(1:nlenvera(descrfisso)) &
                 //'el'//cel
              ENDIF
              PRINT*,'scrivo: descr ',descr
              
              CALL idba_set (handle,"rep_memo",descr)
              
              DO ist=1,nstaz
                IF(ABS(x(ist)-rmdo) > 0.1 .AND. &
                 ABS(y(ist)-rmdo) > 0.1 .AND. &
                 xstaz(ist,irm) /= rmdo)THEN
                  rlat=y(ist)
                  rlon=x(ist)
                  h=alt(ist)
                  
                  ! imposto tutta l'anagrafica
                  
                  CALL idba_set (handle,"lat",rlat)
                  CALL idba_set (handle,"lon",rlon)
                  CALL idba_set (handle,"mobile",0)
                  
                  CALL idba_set (handle,"leveltype", &
                   level(1))
                  CALL idba_set (handle,"l1",level(2))
                  CALL idba_set (handle,"l2",level(3))
                  
                  IF(imet == 0)THEN ! scalare
                    
                    ! attenzione!!!!!! ho bisogno che il minimo sia 0????
                    dato=a+xstaz(ist,irm)*b
                    
                  ELSEIF(imet == 1)THEN !scalare direzione
                    
                    IF(xstaz(ist,irm) <= 1. .AND. &
                     xstaz(ist,irm) /= 0.)THEN
                      dato=1.
                    ELSE
                      dato=a+xstaz(ist,irm)*b
                    ENDIF
                    
                  ENDIF
                  
                  CALL idba_set (handle,cvar,dato)
                  CALL idba_prendilo (handle)
                  
                ENDIF
              ENDDO      !nstaz
              
            ENDDO         !nrm
            
111         CONTINUE      !grib non trovato
            
          ENDDO            !nore
        ENDDO               !nscad
      ENDDO                  !ngio
      CLOSE(3)               !date
      
    ELSEIF(imet == 2)THEN     ! vettore
      
      wind=.TRUE.
      
      DO i=1,3
        varv(i)=kvar(i,2)
      ENDDO
      CALL variabile(3,varv,cvarv,a,b,.TRUE.)
      
      OPEN(3,file='date.nml',status='old')
      DO igio=1,ngio
        READ(3,nml=date,err=9004)
        ora(1)=nora/100.
        ora(2)=MOD(nora,100)
        PRINT*,'data ',DATA
        DO iscad=1,nscad
          DO is=1,4
            scad(is)=scadenze(is,iscad)
          ENDDO
          PRINT*,'scadenza ',scad
          
          IF(scad(4) == 13)THEN
            iscaddb=0
          ELSE
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
            
            ! u component
            DO irm=1,nrm
              est(1)=-1
              est(2)=-1
              est(3)=-1
              CALL findgribest(iug,kgrib,idimg,DATA,ora, &
               scad,level,var,est,ier)
              IF(ier == -1)THEN
                PRINT*,'grib mancante - azzero ', &
                 'tutto l''ensemble'
                xstaz=rmdo
                xstazv=rmdo
                GOTO 222
              ELSEIF(ier /= 0)THEN
                GOTO 9200
              ENDIF
              CALL getinfoest(-1,kgrib,idimg, &
               DATA,ora,scad,level,var,est, &
               alat(1),alat(2),alon(1),alon(2), &
               ny,nx,dy,dx,idrt,alarot,alorot,rot, &
               ija,ier)
              IF(ier /= 0)GOTO 9300
              CALL getdata(kgrib,idimg,imd,rmdo,xgrid,idimv, &
               ibm,ier)
              IF(ibm /= 0 .OR. ier /= 0)GOTO 9400
              IF(lsvar == -1)THEN
                lsvar=1
                DO ivec=1,idimv
                  IF(xgrid(ivec) == rmdo)THEN
                    lsm(ivec)=0
                  ELSE
                    lsm(ivec)=1
                  ENDIF
                ENDDO
              ELSEIF(lsvar == 0)THEN
                DO ivec=1,idimv
                  IF (xgrid(ivec) == rmdo .OR. lsm(ivec)==1)THEN
                    lsm(ivec)=1
                  ELSE
                    lsm(ivec)=0
                  ENDIF
                ENDDO
              ELSEIF(lsvar == 1)THEN
                DO ivec=1,idimv
                  IF (xgrid(ivec) == rmdo .OR. lsm(ivec)==0)THEN
                    lsm(ivec)=0
                  ELSE
                    lsm(ivec)=1
                  ENDIF
                ENDDO
              ENDIF
              DO iv=1,idimv
                xgridu(iv)=xgrid(iv)
              ENDDO
            ENDDO
            ! v component
            DO irm=1,nrm
              est(1)=-1
              est(2)=-1
              est(3)=-1
              CALL findgribest(iug,kgrib,idimg,DATA,ora, &
               scad,level,varv,est,ier)
              IF(ier == -1)THEN
                PRINT*,'grib mancante - azzero ', &
                 'tutto l''ensemble'
                DO jrm=1,nrm
                  DO ist=1,nstaz
                    xstaz(ist,jrm)=rmdo
                    xstazv(ist,jrm)=rmdo
                  ENDDO
                ENDDO
                GOTO 222
              ELSEIF(ier /= 0)THEN
                GOTO 9200
              ENDIF
              CALL getdata(kgrib,idimg,imd,rmdo,xgrid,idimv, &
               ibm,ier)
              IF(ibm /= 0 .OR. ier /= 0)GOTO 9400
              DO iv=1,idimv
                xgridv(iv)=xgrid(iv)
              ENDDO
            ENDDO
            
            ! Interpolation of predicted data on (lat,lon) station points
            IF(ruota)THEN
              CALL rot_grib_LAMBO(alorot,alarot, &
               tlm0d,tph0d)
            ENDIF
            DO irm=1,nrm
              DO ist=1,nstaz
                IF(ABS(x(ist)-rmdo) > 0.1 .AND. &
                 ABS(y(ist)-rmdo) > 0.1)THEN
                  CALL ngetpoint(x(ist),y(ist), &
                   xgridu,xgridv, &
                   lsm, &
                   idimv,nx,ny,alon(1),alat(1), &
                   dx,dy,igrid, &
                   ija,tlm0d,tph0d,wind,imod,lsvar, &
                   xintu,xintv,ier)
                  IF(ier == 2 .OR. ier == 4)THEN
                    ! cerco di interpolare su un punto che non e' nel dominio dei dati
                    xstaz(ist,irm)=rmdo
                    xstazv(ist,irm)=rmdo
                  ELSEIF(ier == 0)THEN
                    xstaz(ist,irm)=xintu
                    xstazv(ist,irm)=xintv
                  ELSE
                    GOTO 9600
                  ENDIF
                ENDIF
              ENDDO
            ENDDO         !nrm
            
            ! scrittura su database
            DO irm=1,nrm
              IF(nrm > 1)THEN
                WRITE(cel,'(i3.3)')irm
                descr=descrfisso(1:nlenvera(descrfisso)) &
                 //'el'//cel
              ENDIF
              PRINT*,'scrivo: descr ',descr
              DO ist=1,nstaz
                IF(ABS(x(ist)-rmdo) > 0.1 .AND. &
                 ABS(y(ist)-rmdo) > 0.1 .AND. &
                 xstaz(ist,irm) /= rmdo)THEN
                  rlat=y(ist)
                  rlon=x(ist)
                  h=alt(ist)
                  
                  ! imposto tutta l'anagrafica
                  
                  CALL idba_set (handle,"lat",rlat)
                  CALL idba_set (handle,"lon",rlon)
                  CALL idba_set (handle,"mobile",0)
                  
                  CALL idba_set (handle,"rep_memo",descr)
                  
                  CALL idba_set (handle,"leveltype", &
                   level(1))
                  CALL idba_set (handle,"l1",level(2))
                  CALL idba_set (handle,"l2",level(3))
                  
                  dato=a+xstaz(ist,irm)*b
                  CALL idba_set (handle,cvar,dato)
                  ! Scrivo anche v
                  dato=a+xstazv(ist,irm)*b
                  CALL idba_set (handle,cvarv,dato)
                  
                  CALL idba_prendilo (handle)
                  
                ENDIF
              ENDDO      !nstaz
              
            ENDDO         !nrm
            
222         CONTINUE      !grib non trovato
            
          ENDDO            !nore
        ENDDO               !nscad
      ENDDO                  !ngio
      
      CLOSE(3)               !date
      
    ELSE
      PRINT*,'errore, imet non consentito'
      CALL EXIT(1)
    ENDIF                     !imet
    
    call pbclose(iug,ier)
    if(ier < 0)goto9500

! chiusura database
    call idba_fatto(handler)
    call idba_fatto(handle)
    call idba_arrivederci(idbhandle)

    stop
    9001 print *,"Errore durante la lettura della namelist parameters "
    stop
    9002 print *,"Errore durante la lettura della namelist stat "
    stop
    9012 print *,"Errore durante la lettura della namelist parametro "
    stop
    9003 print *,"Errore durante la lettura della namelist scadenza "
    stop
    9004 print *,"Errore durante la lettura della namelist date "
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
    9600 print *,"Errore durante la ngetpoint ",ier
    stop
    9700 print *,"Errore durante la gribex ",ier
    stop
    9800 print *,"Errore durante la pbgrib ",ier
    stop
    end program

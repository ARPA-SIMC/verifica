    program cumula_preci

! c   VERIFICA - cumula_preci.f
! c   programma per cumulare sull'intervallo scelto dall'utente
! c   le precipitazioni presenti in database e cumulate su intervallo dato
! c   e' possibile scegliere la percentuale di intervalli mancanti accettabile
! c   Autore: Chiara Marsigli

! Copyright (C) 2005

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

    parameter (MNSTAZ=10000)

    real :: rlon,rlat,h
    integer :: fctime,period
! namelist variables
    INTEGER :: giornoi=01,mesei=01,annoi=2000,orai=00,mini=00
    integer :: giornof=01,mesef=01,annof=2000,oraf=00,minf=00
    INTEGER :: incr=60,ncum=60,interval=60
    real :: perc=1.
    character(512) :: database='',user='',password=''

    integer :: nstep,nsog,nstaz
    INTEGER :: DATA(3),ora(2),repcod,icodice
    integer :: dataval(3),oraval(2)
    character(len=6) :: cvar
    character(len=10) :: btable
    character(len=20) :: repmemo

    type(anaid_type), ALLOCATABLE :: anaid(:)

    INTEGER :: handle,handler
    integer :: debug=1
    integer :: handle_err

    integer :: ier

    namelist /odbc/database,user,password
    NAMELIST /cumula/giornoi,mesei,annoi,orai,mini, &
    giornof,mesef,annof,oraf,minf,incr,ncum,interval,perc

! incr = intervallo di cumulazione delle preci nel database (minuti)
! ncum = intervallo di cumulazione desiderato (minuti)
! interval = intervallo desiderato tra una cumulata e la successiva (minuti)
! perc = percentuale di dati mancanti accettabile per ottenere la cumulata

    data rmdo/-999.9/
    data cvar/'B13011'/

    PRINT*,'program cumula_preci'
    
    open(1,file='odbc.nml',status='old')
    read(1,nml=odbc)
    close(1)

    open(1,file='cumula.nml',status='old')
    read(1,nml=cumula)
    close(1)

    OPEN(2,file='log_cumulate.txt',status='unknown')

! numero di step di precipitazione che occorrono per cumulare su ncum
    nstep=ncum/incr
    nsog=nstep*perc

    data(3)=annoi
    data(2)=mesei
    data(1)=giornoi
    ora(1)=orai
    ora(2)=mini
    dataval(3)=annoi
    dataval(2)=mesei
    dataval(1)=giornoi
    oraval(1)=orai
    oraval(2)=mini
    call JELADATA5(giornoi,mesei,annoi,orai,mini,iminuti)
    call JELADATA5(giornof,mesef,annof,oraf,minf,iminmax)
    PRINT*,'dal ',giornoi,mesei,annoi,orai,mini
    print*,'al ',giornof,mesef,annof,oraf,minf
    print*,'incremento in minuti ',incr

! PREPARAZIONE DELL' ARCHIVIO

! gestione degli errori
    ier=idba_error_set_callback(0,C_FUNLOC(idba_default_error_handler),debug,handle_err)

! connessione con database
    print*,"database=",database
    ier=idba_presentati(idbhandle,database)

! apertura database in lettura
    ier=idba_preparati(idbhandle,handler,"read","read","read")
! apertura database in scrittura
    ier=idba_preparati(idbhandle,handle,"write","write","write")

    ier=idba_quantesono(handler,nstaz)
    print*,'massimo numero pseudo-stazioni ',nstaz
! allocazione matrici
    ALLOCATE(anaid(1:nstaz))
    call leggiana_db_all(anaid,nstaz,handler)

    STATIONS: DO ist=1,nstaz

      ier=idba_unsetall (handler)

!      ier=idba_set (handler,"ana_id",anaid(ist))

      ier=idba_set (handler,"lat",anaid(ist)%lat)
      ier=idba_set (handler,"lon",anaid(ist)%lon)

      dataval(3)=annoi
      dataval(2)=mesei
      dataval(1)=giornoi
      oraval(1)=orai
      oraval(2)=mini
      CALL JELADATA5(DATA(1),DATA(2),DATA(3),ora(1),ora(2), &
       iminuti)
      ! INIZIO CICLO SUI GIORNI
      MINUTES: DO WHILE (iminuti.LE.iminmax)

        ier=idba_set (handler,"year",dataval(3))
        ier=idba_set (handler,"month",dataval(2))
        ier=idba_set (handler,"day",dataval(1))
        ier=idba_set (handler,"hour",oraval(1))
        ier=idba_set (handler,"min",oraval(2))
        ier=idba_set (handler,"sec",00)

        iminutiw=iminuti
        ndati=0
        prec=0.
        STEPS: DO i=1,nstep
          CALL JELADATA6(idayw,imonthw,iyearw,ihourw,iminw, &
           iminutiw)
          ier=idba_set (handler,"year",iyearw)
          ier=idba_set (handler,"month",imonthw)
          ier=idba_set (handler,"day",idayw)
          ier=idba_set (handler,"hour",ihourw)
          ier=idba_set (handler,"min",iminw)
          ier=idba_set (handler,"sec",00)
          
          period=incr*60 !lo trasformo in secondi
          fctime=0
          ier=idba_settimerange(handler,1,fctime,period)

! non funziona con il livello impostato così!!!
!          ier=idba_setlevel(handler,1,0,0,0)

          ier=idba_set (handler,"var",cvar)

          PRINT*,'estraggo ',idayw,imonthw,iyearw,ihourw,iminw,fctime,period,cvar

          ier=idba_voglioquesto (handler,N)
          
          IF(N == 0)THEN
            PRINT*,'oss - non ci sono dati'
          ELSEIF (N == 1) THEN
            ier=idba_dammelo (handler,btable)

            ier=idba_enq (handler,"lat",rlat)
            ier=idba_enq (handler,"lon",rlon)
            ier=idba_enq (handler,"height",h)
            ier=idba_enq (handler,"ana_id",icodice)
            ier=idba_enq (handler,"rep_memo",repmemo)
            
            ier=idba_enq (handler,btable,dato)
            
            ndati=ndati+1
            prec=prec+dato
          ELSE
            PRINT*,'ho trovato ',N,' dati!!!'
            call exit (1)
          ENDIF
          
          iminutiw=iminutiw-incr

        ENDDO STEPS

        IF (ndati < nsog) THEN
          WRITE(2,*)'butto la cumulata perche'' ci sono solo ',ndati,' dati'
          WRITE(2,*)icodice,rlon,rlat,idayw,imonthw,iyearw,ihourw,iminw
          GOTO 55
        ELSE
          WRITE(2,*)'tengo la cumulata! Percentuale dati usati ',ndati,' su ',nstep
        ENDIF
        
        ! INSERIMENTO DEI PARAMETRI NELL' ARCHIVIO

        ier=idba_unsetall (handle)
        
        ier=idba_set (handle,"year",dataval(3))
        ier=idba_set (handle,"month",dataval(2))
        ier=idba_set (handle,"day",dataval(1))
        ier=idba_set (handle,"hour",oraval(1))
        ier=idba_set (handle,"min",oraval(2))
        ier=idba_set (handle,"sec",00)
        
        period=ncum*60 !lo trasformo in secondi
        fctime=0
        ier=idba_settimerange(handle,1,fctime,period)
        
        ier=idba_set (handle,"rep_memo",repmemo)
        
        ier=idba_set (handle,"ana_id",icodice)
        
        ier=idba_set (handle,"mobile",0)
        
        ier=idba_setlevel(handle,1,0,0,0)

        WRITE(2,*)'scrivo ',dataval(3),dataval(2),dataval(1),oraval(1),oraval(2),prec,icodice,repcod
        
        ier=idba_unset (handle,"B13011") !TOTAL PRECIPITATION [KG/M2]
        
        IF (prec /= rmdo) ier=idba_set(handle,"B13011",prec)

        ier=idba_prendilo (handle)
        
55      CONTINUE
        
    ! trovo nuova data e ora di validita' del dato
        iminuti=iminuti+interval
        call JELADATA6(iday,imonth,iyear,ihour,imin,iminuti)
        dataval(1)=iday
        dataval(2)=imonth
        dataval(3)=iyear
        oraval(1)=ihour
        oraval(2)=imin
      ENDDO MINUTES
      
    ENDDO STATIONS

    CLOSE(2)

    ier=idba_fatto(handler)
    ier=idba_fatto(handle)
    ier=idba_arrivederci(idbhandle)

    stop
    end program

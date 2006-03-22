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

    parameter (MNSTAZ=10000)

    real :: rlon,rlat,h
    INTEGER :: giornoi,mesei,annoi,orai,mini,giornof,mesef,annof,oraf,minf
    INTEGER :: incr,ncum,nstep,nsog
    real :: perc
    integer :: data(3),ora(2),p1,p2,repcod
    integer :: dataval(3),oraval(2)
    character cvar*6
    character btable*10

    real, ALLOCATABLE :: x(:),y(:),alt(:)
    integer, ALLOCATABLE :: anaid(:)

    character(19) :: database,user,password
    integer :: handle,rewrite
    logical :: init,debug,rmmiss
    data init,debug,rmmiss/.false.,.true.,.false./
    external error_handle

    namelist  /odbc/database,user,password
    NAMELIST  /cumula/giornoi,mesei,annoi,orai,mini, &
    giornof,mesef,annof,oraf,minf,incr,ncum,interval,perc

! incr = intervallo di cumulazione delle preci nel database (minuti)
! ncum = intervallo di cumulazione desiderato (minuti)
! interval = intervallo desiderato tra una cumulata e la successiva (minuti)
! perc = percentuale di dati mancanti accettabile per ottenere la cumulata

    data rmd/9999/,rmdo/-999.9/
    data cvar/'B13011'/

    open(1,file='odbc.nml',status='old',readonly)
    read(1,nml=odbc)
    close(1)

    open(1,file='cumula.nml',status='old',readonly)
    read(1,nml=cumula)
    close(1)

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
    call idba_error_set_callback(0,error_handle,debug,handle_err)

! connessione con database
    print*,"database=",database
    call idba_presentati(idbhandle,database,user,password)

! apertura database in lettura
    call idba_preparati(idbhandle,handler,"read","read","read")
! apertura database in scrittura
    call idba_preparati(idhandle,handle,"reuse","rewrite","rewrite")

    call idba_quantesono(handler,nstaz)
    print*,'massimo numero pseudo-stazioni ',nstaz
! allocazione matrici
    ALLOCATE(x(1:nstaz))
    ALLOCATE(y(1:nstaz))
    ALLOCATE(alt(1:nstaz))
    ALLOCATE(anaid(1:nstaz))
    call leggiana_db_all(x,y,alt,anaid,rmdo,nstaz,handler)

    NSTAZ: DO ist=1,nstaz

      CALL idba_unsetall (handler)

      CALL idba_seti (handler,"ana_id",anaid(ist))

      CALL JELADATA5(DATA(1),DATA(2),DATA(3),ora(1),ora(2), &
       iminuti)
      ! INIZIO CICLO SUI GIORNI
      IMINUTI: DO WHILE (iminuti.LE.iminmax)

        CALL idba_seti (handler,"year",dataval(3))
        CALL idba_seti (handler,"month",dataval(2))
        CALL idba_seti (handler,"day",dataval(1))
        CALL idba_seti (handler,"hour",oraval(1))
        CALL idba_seti (handler,"min",oraval(2))
        CALL idba_seti (handler,"sec",00)

        iminutiw=iminuti
        ndati=0
        prec=0.
        NSTEP: DO i=1,nstep
          CALL JELADATA6(idayw,imonthw,iyearw,ihourw,iminw, &
           iminutiw)
          CALL idba_seti (handler,"year",iyearw)
          CALL idba_seti (handler,"month",imonthw)
          CALL idba_seti (handler,"day",idayw)
          CALL idba_seti (handler,"hour",ihourw)
          CALL idba_seti (handler,"min",iminw)
          CALL idba_seti (handler,"sec",00)
          
          p1=-(incr*60) !lo trasformo in secondi
          p2=0
          CALL idba_seti (handler,"pindicator",4)
          CALL idba_seti (handler,"p1",p1)
          CALL idba_seti (handler,"p2",p2)

          CALL idba_seti (handler,"leveltype",1)
          CALL idba_seti (handler,"l1",0)
          CALL idba_seti (handler,"l2",0)
          CALL idba_setc (handler,"var",cvar)

!          PRINT*,'estraggo ',idayw,imonthw,iyearw,ihourw,iminw,p1,p2,cvar,icodice

          CALL idba_voglioquesto (handler,N)
          
          IF(N == 0)THEN
!            PRINT*,'oss - non ci sono dati'
          ELSEIF (N == 1) THEN
            CALL idba_dammelo (handler,btable)

            CALL idba_enqr (handler,"lat",rlat)
            CALL idba_enqr (handler,"lon",rlon)
            CALL idba_enqr (handler,"height",h)
            CALL idba_enqi (handler,"ana_id",icodice)
            CALL idba_enqi (handler,"rep_cod",repcod)
            
            CALL idba_enqr (handler,btable,dato)
            
            ndati=ndati+1
            prec=prec+dato
          ELSE
            PRINT*,'ho trovato ',N,' dati!!!'
            call exit (1)
          ENDIF
          
          iminutiw=iminutiw-incr

        ENDDO NSTEP

        IF (ndati < nsog) THEN
          PRINT*,'butto la cumulata perche'' ci sono solo ',ndati,' dati'
          PRINT*,icodice,rlon,rlat,idayw,imonthw,iyearw,ihourw,iminw,p1
          GOTO 55
        ELSE
          PRINT*,'tengo la cumulata! Percentuale dati usati ',ndati,' su ',nstep
        ENDIF
        
        ! INSERIMENTO DEI PARAMETRI NELL' ARCHIVIO

        CALL idba_unsetall (handle)
        
        CALL idba_seti (handle,"year",dataval(3))
        CALL idba_seti (handle,"month",dataval(2))
        CALL idba_seti (handle,"day",dataval(1))
        CALL idba_seti (handle,"hour",oraval(1))
        CALL idba_seti (handle,"min",oraval(2))
        CALL idba_seti (handle,"sec",00)
        
        p1=-(ncum*60) !lo trasformo in secondi
        p2=0
        
        CALL idba_seti (handle,"pindicator",4)
        CALL idba_seti (handle,"p1",p1)
        CALL idba_seti (handle,"p2",p2)
        
        CALL idba_seti (handle,"rep_cod",repcod)
        
        CALL idba_setr (handle,"lat",rlat)
        CALL idba_setr (handle,"lon",rlon)
        CALL idba_setr (handle,"height",h)
        CALL idba_seti (handle,"ana_id",icodice)
        
        CALL idba_seti (handle,"mobile",0)
        
        CALL idba_seti (handle,"leveltype",1)
        CALL idba_seti (handle,"l1",0)
        CALL idba_seti (handle,"l2",0)
        
        PRINT*,'scrivo ',dataval(3),dataval(2),dataval(1),oraval(1),oraval(2),p1,p2,prec,icodice,repcod
        
        CALL idba_unset (handle,"B13011") !TOTAL PRECIPITATION [KG/M2]
        
        IF (prec /= rmdo) CALL idba_setr(handle,"B13011",prec)

        CALL idba_prendilo (handle)
        
55      CONTINUE
        
    ! trovo nuova data e ora di validita' del dato
        iminuti=iminuti+interval
        call JELADATA6(iday,imonth,iyear,ihour,imin,iminuti)
        dataval(1)=iday
        dataval(2)=imonth
        dataval(3)=iyear
        oraval(1)=ihour
        oraval(2)=imin
      ENDDO IMINUTI
      
    ENDDO NSTAZ

    call idba_fatto(handler)
    call idba_fatto(handle)
    call idba_arrivederci(idbhandle)

    stop
    end program
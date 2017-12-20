    PROGRAM scores_prob_dballe

! c VERIFICA - scores_prob_dballe.f
! c programma per il calcolo degli scores probabilistici
! c autore: Chiara Marsigli
! c ultima modifica: 7 febbraio 2006 - passaggio a DbAlle

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
    USE scores_prob_util_dballe
    USE common_namelists

    parameter (MNSTAZ=5000,MNGIO=190,MNORE=1)
    parameter (MNV=MNSTAZ*MNGIO*MNORE)
! attenzione!!! Non sono usate, servono solo per dare
! un riferimento a chi dimensiona i vettori dinamicamente
    integer :: ora(2),scad(4)
    integer :: dataval(3),oraval(2),scaddb(4)
    INTEGER :: itipost,ntot,h,sum_nowght
    type(anaid_type) :: icodice
    real :: dato
    integer :: leveltype1,l1,leveltype2,l2
    integer :: pind,fctime,period
    character(len=20) :: descr,descrfisso
    character(len=3) :: cel
    REAL :: bs,bss,roca,clarea,outr,rps,rpss,bssd,rpssd,succrate(3)
    integer :: pesi(MNRM)
    LOGICAL :: loutput

    REAL, ALLOCATABLE :: oss(:,:),prev(:,:,:),prevr(:,:,:)
    type(anaid_type), ALLOCATABLE :: anaid(:)
    INTEGER, ALLOCATABLE :: wght(:,:),temp_wght(:),distrib(:)

    CHARACTER(len=10) :: btable
    INTEGER :: handle,handle_err,handleana,USTAZ
    integer :: debug=1

    integer :: ier

    DATA rmdo/-999.9/,imd/32767/,rmddb/-999.9/,rmds/-9.999/
    DATA loutput/.TRUE./

    NAMELIST /pesirm/pesi

    OPEN(55,file='ctrl_output.dat',status='unknown')

    PRINT*,'program scores_prob'

    open(1,file='odbc.nml',status='old')
    read(1,nml=odbc,err=9001)
    close(1)
! Read parameters
    open(1,file='parameters.nml',status='old')
    read(1,nml=parameters,err=9001)
    close(1)
! ora di inizio dei runs
    ora(1)=nora/100.
    ora(2)=mod(nora,100)
    open(1,file='stat.nml',status='old')
    read(1,nml=stat,err=9002)
    close(1)
    open(1,file='lista.nml',status='old')
    read(1,nml=lista,err=9006)
    close(1)
    open(1,file='lista_ens.nml',status='old')
    read(1,nml=listaens,err=9003)
    sum_nowght=0
    DO irm=1,nrm
      sum_nowght=sum_nowght+nowght(irm)
    ENDDO
    IF(sum_nowght /= nelsupens)THEN
      PRINT*,'-----------------------------------'
      PRINT*,'ATTENZIONE!!!'
      PRINT*,'la somma dei pesi costanti (nowght)'
      PRINT*,'per i primi nrm= ',nrm,'elementi'
      PRINT*,'e'' diversa da nelsupens'
      PRINT*,'sum_nowght= ',sum_nowght
      PRINT*,'nelsupens= ',nelsupens
      PRINT*,'-----------------------------------'
    ENDIF
    close(1)
    open(1,file='scadenze.nml',status='old')
    read(1,nml=scadenza,err=9003)
    close(1)
    if(nsoglie > MNSOG)then
        print*,'troppe soglie! '
        call exit (1)
    endif

    call descrittore(model,itipo,imod,ls,media,massimo,prob, &
    distr,dxb,dyb,descr)
    print*,'descrittore ',descr
    descrfisso=descr
    if(itipo == 1)then
        itipost=0
        if(iana == 1)itipost=90
    elseif(itipo == 2)then
        itipost=80
    endif

! gestione degli errori
    ier=idba_error_set_callback(0,C_FUNLOC(idba_default_error_handler),debug,handle_err)

! connessione con database
    ier=idba_presentati(idbhandle,database)

! apertura database in lettura
    ier=idba_preparati(idbhandle,handle,"read","read","read")
    ier=idba_preparati(idbhandle,handleana,"read","read","read")

    ier=idba_set(handle,"rep_memo",reteref)
    
! leggo tutte le stazioni presenti in archivio
    ier=idba_quantesono(handle,nstaz)
    print*,'massimo numero pseudo-stazioni ',nstaz
    if(nstaz > MNSTAZ)then
        print*,'SONO TANTE ',nstaz,' STAZIONI!! SEI SICURO/A?'
    endif
! allocazione matrici anagrafica
    ALLOCATE(anaid(1:nstaz))

    PRINT*,'lselect= ',lselect
    call leggiana_db_scores(iana,anaid, &
    itipost,rmdo,nstaz,handle,lselect)
    print*,'numero massimo stazioni ',nstaz

! allocazione matrici dati
    ALLOCATE(oss(1:nstaz, 1:ngio))
    ALLOCATE(prev(1:nstaz, 1:ngio, 1:nrm))

! allocazione matrice pesi
    ALLOCATE(wght(1:ngio, 1:nrm))
    ALLOCATE(temp_wght(1:nrm))
    ALLOCATE(distrib(1:nrm))

    wght=1

    open(11,file='scores_prob.dat',status='unknown')

    write(11,*)' variabile= ',cvar !uscite probabilistiche
    print*,'variabile ',cvar

    OPEN(20,file='rmse.dat')
    OPEN(21,file='sprd.dat')

    DO iscad=1,nscad
      DO is=1,4
        scad(is)=scadenze(is,iscad)
      ENDDO
      PRINT*,'scadenza ',scad
      
      iscaddb=scad1+inc*(iscad-1)
      WRITE(11,'(a,i3)')' scadenza= ',iscaddb
      WRITE(22,'(a,i3)')' scadenza= ',iscaddb
      
      WRITE(20,'(a,i3)')' scadenza= ',iscaddb
      WRITE(21,'(a,i3)')' scadenza= ',iscaddb

      OPEN(1,file='date.nml',status='old')
      OPEN(2,file='pesi.nml',status='old')
      
      distrib = 0
      prev = rmddb
      oss = rmddb
      
      ng=0
      DO igio=1,ngio
        
        READ(1,nml=date,err=9004)
        ! trovo data e ora di validita'
        CALL JELADATA5(DATA(1),DATA(2),DATA(3),ora(1),ora(2),iminuti)
        iminuti=iminuti+iscaddb*60
        CALL JELADATA6(iday,imonth,iyear,ihour,imin,iminuti)
        dataval(1)=iday
        dataval(2)=imonth
        dataval(3)=iyear
        oraval(1)=ihour
        oraval(2)=imin
          
        WRITE(55,*)'data validita'' ',dataval,oraval
        CALL flush(55)
        
        ! lettura previsioni da database
        
        ! ricominciamo perche' ho gia' fatto una query nella subroutine
        ier=idba_unsetall(handle)
        
        ier=idba_set (handle,'query','bigana')
        
        ier=idba_set (handle,"year",dataval(3))
        ier=idba_set (handle,"month",dataval(2))
        ier=idba_set (handle,"day",dataval(1))
        ier=idba_set (handle,"hour",oraval(1))
        ier=idba_set (handle,"min",oraval(2))
        ier=idba_set (handle,"sec",0)

        ! conversione delle scadenze in secondi (e correzione scadenze sbagliate)
        CALL converti_scadenze(4,scad,scaddb)

! questa gestione delle scadenze è fatta rispetto al vettore scad scritto come
! lo è per i grib in formato GRIB1. Passando a GRIB2 scad sarà posta identica
! come sono scritte le scadenze in dballe 
        scadenzep: SELECT CASE(scaddb(4))
        CASE(4) ! cumulata
          pind=1
          fctime=scaddb(3)
          period=scaddb(3)-scaddb(2)
        CASE(0) ! istantanea
          pind=254
          IF(scaddb(3)/=0)THEN
            PRINT*,'case 1 - p1= ',scaddb(2),' p2= ',scaddb(3)
            CALL EXIT(1)
          ENDIF
          fctime=scaddb(2)
          period=0
        CASE(1) ! analisi inizializzata
          pind=254
          IF(scaddb(2)/=0)THEN
            PRINT*,'case 1 - p1= ',scaddb(2),' p2= ',scaddb(3)
            CALL EXIT(1)
          ENDIF
          fctime=scaddb(2)
          period=0
        CASE(2) ! prodotto valido in un periodo
          pind=205
          fctime=scaddb(3)
          period=scaddb(3)-scaddb(2)
        CASE(13) ! analisi di precipitazione
          pind=1
          PRINT*,'controllo - verrebbe fctime= ',scaddb(2),' period= ',scaddb(3)
          fctime=scaddb(2)
          period=scaddb(3)
        CASE default
          PRINT*,'indicatore scadenza non gestito'
          CALL EXIT(1)
        END SELECT scadenzep
        ier=idba_settimerange(handle,pind,fctime,period)
        
        ier=idba_set (handle,"var",cvar)
        
        DO irm=1,nrm
          WRITE(cel,'(i3.3)')irm
          descr=descrfisso(1:nlenvera(descrfisso))//'el'//cel
          
          ier=idba_set (handle,"rep_memo",descr)
          
          PRINT*,'prev ',descr,dataval,oraval,'iscaddb',iscaddb
          
          ier=idba_voglioquesto (handle,N)

          IF(N == 0)THEN
            PRINT*,'pre - non ci sono dati'
            PRINT*,dataval,oraval
            GOTO 66
          ELSE
!             PRINT*,"pre - numero di dati trovati ",N
          ENDIF
            
          DO idati=1,N
            
            ier=idba_dammelo (handle,btable)
            ! sara' da impostare mentre per ora e' solo richiesto
            ier=idba_enqlevel(handle,leveltype1,l1,leveltype2,l2)
            
            ! ier=idba_enqi (handle,"mobile",mobile)
            
!            ier=idba_enq (handle,"ana_id",icodice)
            ier=idba_enq (handle,"lat",icodice%lat)
            ier=idba_enq (handle,"lon",icodice%lon)
           
! commento temporaneo per gestire pb allocazione memoria da elencamele
            !mst  interrogo sezione anagrafica per avere l'altezza
!!$            ier=idba_set (handleana,"ana_id",icodice)
!!$            ier=idba_quantesono(handleana,USTAZ)
!!$            ier=idba_elencamele (handleana)
!!$            ier=idba_enq (handleana,"height",h)
            
!!$            IF(iquota >= 0)THEN
!!$              IF(c_e_i(h))THEN
!!$                IF(iquota == 0)THEN !pianura
!!$                  IF(h >= hlimite)goto20
!!$                ELSEIF(iquota == 1)THEN !montagna
!!$                  IF(h < hlimite)goto20
!!$                ELSEIF(iquota > 1)THEN
!!$                  PRINT*,'iquota non gestito ',iquota
!!$                ENDIF
!!$              ELSE
!!$                goto20
!!$              ENDIF
!!$            ENDIF
            
            ipos=0
            DO i=1,nstaz
!              IF(icodice == anaid(i))THEN
               if(icodice%lat == anaid(i)%lat .and. &
                    icodice%lon == anaid(i)%lon)then !!!
                ipos=i
              ENDIF
            ENDDO
            
            IF(ipos == 0)goto20
            
            ier=idba_enq (handle,btable,dato)
            
            prev(ipos,igio,irm)=dato

!            print*,'letto da db',igio,irm,prev(ipos,igio,irm)
            
20          CONTINUE
            
          ENDDO            ! idati
        ENDDO               ! nrm

        ! lettura osservazioni da database
        
        ier=idba_unsetall(handle)
        
        IF(itipost == 0)THEN
          ier=idba_set (handle,"priomin",0)
          ier=idba_unset (handle,"priomax")
          ier=idba_set (handle,"query","best")
          descr="oss"
        ELSEIF(itipost == 80)THEN
          ier=idba_unset (handle,"query")
          nlm=nlenvera(model)
          descr='oss'//descrfisso((nlm+1):(nlm+5))
          ier=idba_set (handle,"rep_memo",descr)
        ELSEIF(itipost == 90)THEN
          ier=idba_unset (handle,"query")
          descr="ana"
          ier=idba_set (handle,"rep_memo",descr)
        ENDIF
        
        ier=idba_set (handle,'query','bigana')
        
        ier=idba_set (handle,"year",dataval(3))
        ier=idba_set (handle,"month",dataval(2))
        ier=idba_set (handle,"day",dataval(1))
        ier=idba_set (handle,"hour",oraval(1))
        ier=idba_set (handle,"min",oraval(2))
        ier=idba_set (handle,"sec",0)
          
        scadenzeo: SELECT CASE(scaddb(4))
        CASE(4) ! cumulata
          pind=1
          fctime=0
          period=scaddb(3)-scaddb(2)
        CASE(0) ! istantanea
          pind=254
          IF(scaddb(3)/=0)THEN 
            PRINT*,'case 0 - p1= ',scaddb(2),' p2= ',scaddb(3)
            CALL EXIT(1)
          ENDIF
          fctime=0
          period=0
        CASE(1) ! analisi inizializzata
          pind=254
          IF(scaddb(2)/=0)THEN
            PRINT*,'case 1 - p1= ',scaddb(2),' p2= ',scaddb(3)
            CALL EXIT(1)
          ENDIF
          fctime=0
          period=0
        CASE(2) ! prodotto valido in un periodo
          pind=205
          fctime=0
          period=scaddb(3)-scaddb(2)
        CASE(13) ! analisi di precipitazione
          pind=1
          PRINT*,'controllo - verrebbe fctime= ',scaddb(2),' period= ',scaddb(3)
          fctime=0
          period=scaddb(3)-scaddb(2)
        CASE default
          PRINT*,'indicatore scadenza non gestito'
          CALL EXIT(1)
        END SELECT scadenzeo
        ier=idba_settimerange(handle,pind,fctime,period)
        
        ier=idba_set (handle,"var",cvar)
          
        ! print*,'oss ',descr,dataval,oraval
        
        ier=idba_voglioquesto (handle,N)
        IF(N == 0)THEN
          PRINT*,'oss - non ci sono dati'
          PRINT*,dataval,oraval
          GOTO 66
        ELSE
          !PRINT*,"oss - numero di dati trovati ",N
        ENDIF
        
        DO idati=1,N
          
          ier=idba_dammelo (handle,btable)
          ! sara' da impostare mentre per ora e' solo richiesto
          ier=idba_enqlevel(handle,leveltype1,l1,leveltype2,l2)
          
!          ier=idba_enq (handle,"ana_id",icodice)
          ier=idba_enq (handle,"lat",icodice%lat)
          ier=idba_enq (handle,"lon",icodice%lon)
          
! commento temporaneo per gestire pb allocazione memoria da elencamele
          !mst  interrogo sezione anagrafica per avere l'altezza
!!$          ier=idba_set (handleana,"ana_id",icodice)
!!$          ier=idba_quantesono(handleana,USTAZ)
!!$          ier=idba_elencamele (handleana)
!!$          ier=idba_enq (handleana,"height",h)
            
!!$          IF(iquota >= 0)THEN
!!$            IF(c_e_i(h))THEN
!!$              IF(iquota == 0)THEN !pianura
!!$                IF(h >= hlimite .OR. h < -900.)goto30
!!$              ELSEIF(iquota == 1)THEN !montagna
!!$                IF(h < hlimite .OR. h == REAL(imd))goto30
!!$              ELSEIF(iquota > 1)THEN
!!$                PRINT*,'iquota non gestito ',iquota
!!$              ENDIF
!!$            ELSE
!!$              goto30
!!$            ENDIF
!!$          ENDIF
          
          ipos=0
          DO i=1,nstaz
!            IF(icodice == anaid(i))THEN
             if(icodice%lat == anaid(i)%lat .and. &
                  icodice%lon == anaid(i)%lon)then !!!
              ipos=i
            ENDIF
          ENDDO
          
          IF(ipos == 0)goto30
          
          ier=idba_enq (handle,btable,dato)
          
          oss(ipos,igio)=dato

!          if(igio == 1)then
!             print*,ipos,oss(ipos,igio)
!          endif
          
30        CONTINUE
          
        ENDDO               ! idati

        ! attribuizione dei pesi
        ! non si pesa l'ensemble completo!
        IF(nrm == 51)THEN
          DO irm=1,nrm
            wght(igio,irm)=1
          ENDDO
        ELSE
          IF(lwght)THEN
            ! se ho un mini-ensemble leggo i pesi
            READ(2,nml=pesirm,err=9005)
            DO irm=1,nrm
              wght(igio,irm)=pesi(irm)
            ENDDO
          ELSE
            ! se non voglio pesare li eguaglio a nowght
            DO irm=1,nrm
              wght(igio,irm)=nowght(irm)
            ENDDO

          ENDIF
        ENDIF
        DO irm=1,nrm
          temp_wght(irm)=wght(igio,irm)
        ENDDO

        ! calcolo l'errore assoluto per giorno e per elemento
        WRITE(22,'(a,i3)')' giorno= ',igio
        npu=0
        ossmed=0.
        DO is=1,nstaz
          IF(oss(is,igio) /= rmddb)THEN
            ossmed=ossmed+oss(is,igio)
            npu=npu+1
          ENDIF
        ENDDO
        IF(npu > 0)ossmed=ossmed/REAL(npu)
        WRITE(22,'(a,f8.3)')' ossmed= ',ossmed
!!$        IF(ossmed > 0.2)THEN
!!$          CALL terr(nstaz,nrm,oss(:,1),prev(:,1,:),nstaz,nrm, &
!!$           nelsupens,rmddb,rmdo,temp_wght,loutput,iposiz)
!!$          distrib(iposiz)=distrib(iposiz)+1
!!$          ng=ng+1
!!$        ENDIF
        
66      CONTINUE
        
      ENDDO                  !ngio
      CLOSE(1)               !naml date
      CLOSE(2)               !naml pesirm

      IF(lwght)THEN
        WRITE(44,'(a,i3)')'scadenza= ',iscaddb
        WRITE(44,'(1x,a3,3x,a4)')'IRM','FREQ'
        DO irm=1,MNRM
          WRITE(44,'(3x,i1,2x,f6.4,a,i3)')irm,REAL(distrib(irm))/REAL(ng),' ng= ',ng
        ENDDO
      ENDIF
      
      WRITE(11,'(3x,a,4x,a,4x,a,5x,a,5x,a,5x,a,5x,a,5x,a,4x,a,5x,a,4x,a,4x,a,4x,a,4x,a)') &
       'thr','ntot','nocc','bs','rel','res','bss','roca','cla','outr','rps','rpss','bssd','rpssd'
      
      ! output degli scores

      lsprerr=.FALSE.
    
      print*,'lsprerr',lsprerr

      IF(lsprerr)THEN
        CALL sprerr(oss,prev,rmddb)
      ELSE
      
        IF(prob)THEN
          CALL brier_prob(nstaz,ngio,nrm,oss,prev, &
           ngio,nstaz,nrm, &
           nelsupens,rmddb,rmds,soglie(1),wght,loutput, &
           ntot,rnocc,bs,bss)
        ELSE

           nel=count(wght(1,:)>0)
           print*,'nel= ',nel
           if(nel /= nrm)then
              ALLOCATE(prevr(1:nstaz,1:ngio,1:nel))
              do ig=1,ngio
                 iel=1
                 do irm=1,nrm
                    if(wght(ig,irm)>0)then
                       prevr(:,ig,iel)=prev(:,ig,irm)
                       iel=iel+1
                    endif
                 enddo
              enddo
              CALL outrange(nstaz,ngio,nrm,oss,prevr,ngio,nstaz,nel, &
                   rmddb,rmds,wght,loutput,outr)
              DEALLOCATE(prevr)
           else
              CALL outrange(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
                   rmddb,rmds,wght,loutput,outr)
           endif

          CALL ranked(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm,nelsupens, &
           nsoglie,rmddb,rmds,soglie(1:nsoglie),wght,loutput,rps,rpss,rpssd)
          CALL mode_population(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
           rmddb,rmds,wght,loutput,succrate)

          print*,'succrate',succrate

          DO iso=1,nsoglie

             print*,'calcolo il brier'
            CALL brier(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
             nelsupens,rmddb,rmds,soglie(iso),wght,loutput, &
             ntot,nocc,bs,rel,res,bss,bssd)
!            PRINT*,'dopo brier',soglie(iso),ntot,nocc
            CALL roc(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
             nelsupens,rmddb,rmds,soglie(iso),wght,loutput,ntot,nocc,roca)
            CALL costloss(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
             nelsupens,rmddb,rmds,soglie(iso),wght,loutput,ntot,nocc,clarea)
            PRINT*,soglie(iso),ntot,nocc,bs,rel,res,bss,roca,clarea,outr
            WRITE(11,'(1x,f5.1,2(2x,i6),11(2x,f6.3))') &
             soglie(iso),ntot,nocc,bs,rel,res,bss,roca,clarea,outr,rps,rpss,bssd,rpssd
            
         ENDDO               !nsoglie
 
      ENDIF
     ENDIF
   ENDDO                     !nscad
    
   CLOSE(11)
    
    ! chiusura database
   ier=idba_fatto(handle)
   ier=idba_fatto(handleana)
   ier=idba_arrivederci(idbhandle)
    
   CLOSE(55)
    
   STOP
9001 PRINT*,'errore nella lettura della namelist parameters'
   STOP
9002 PRINT*,'errore nella lettura della namelist stat'
   STOP
9003 PRINT*,'errore nella lettura della namelist listaens'
   STOP
9004 PRINT*,'errore nella lettura della namelist date'
   STOP
9005 PRINT*,'errore nella lettura della namelist pesirm'
    STOP
9006 PRINT*,'errore nella lettura della namelist lista'
    STOP
  END PROGRAM scores_prob_dballe

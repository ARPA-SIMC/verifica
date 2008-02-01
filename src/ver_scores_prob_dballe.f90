    program scores_prob_dballe

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

    parameter (MNSTAZ=5000,MNSCAD=72,MNGIO=190,MNORE=1)
    parameter (MNSOG=10,MNV=MNSTAZ*MNGIO*MNORE,MNRM=102)
! attenzione!!! Non sono usate, servono solo per dare
! un riferimento a chi dimensiona i vettori dinamicamente
    integer :: ora(2),var(3),scad(4),level(3)
    integer :: dataval(3),oraval(2),scaddb(4),p1,p2
    INTEGER :: icodice,itipost,ntot,h,sum_nowght
    real :: dato
    character(len=20) :: descr,descrfisso
    character(len=3) :: cel
    REAL :: bs,bss,roca,clarea,outr,rps,rpss
    integer :: pesi(MNRM)
    LOGICAL :: loutput
! namelist variables
    integer :: nora=0000,ngio=1,nscad=1,scad1=1,scad2=1,inc=1
    integer :: nvar=1,nrm=1,nore=1,ore(24)=0000
    integer :: data(3)=(/-1,-1,-1/),scadenze(4,MNSCAD)=-1
    character(len=10) :: model=''
    integer :: itipo=1,iana=0,imet=0,imod=0,ls=-1,nminobs=1
    logical :: ruota=.false.,diffh=.false.
    logical :: media=.false.,massimo=.false.,prob=.false.,distr=.false.
    real :: dxb=1.0,dyb=1.0,diffmax=100.,thr=1.,perc=50.
    CHARACTER(len=6) :: cvar=''
    INTEGER :: iquota=-1
    INTEGER :: nsoglie=1
    REAL :: hlimite=100.,soglie(MNSOG)=0.
    LOGICAL :: lselect=.FALSE.,lwght=.false.
    integer :: nowght(MNRM)=1
    INTEGER :: nelsupens=102
    CHARACTER(LEN=19) :: database='',user='',password=''

    REAL, ALLOCATABLE :: oss(:,:),prev(:,:,:),osse(:),previ(:,:)
    INTEGER, ALLOCATABLE :: anaid(:)
    INTEGER, ALLOCATABLE :: wght(:,:),temp_wght(:),distrib(:)

    CHARACTER(len=10) :: btable
    INTEGER :: handle,handle_err,handleana,USTAZ
    integer :: debug=1

    DATA rmdo/-999.9/,imd/32767/,rmddb/-999.9/,rmds/-9.999/
    DATA loutput/.TRUE./
    NAMELIST /parameters/nora,ngio,nscad,scad1,scad2,inc, &
     nvar,nrm,nore,ore
    NAMELIST /stat/model,itipo,iana,imet,imod,ls,ruota, &
     nminobs,media,massimo,prob,distr,dxb,dyb,diffh,diffmax, &
     thr,perc
    NAMELIST /lista/cvar,iquota,hlimite,nsoglie,soglie,lwght,nowght, &
     nelsupens,lselect
    NAMELIST /date/DATA
    NAMELIST /scadenza/scadenze
    NAMELIST /pesirm/pesi
    NAMELIST /odbc/database,user,password

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
    open(1,file='lista_ens.nml',status='old')
    read(1,nml=lista,err=9003)
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
    call idba_error_set_callback(0,idba_default_error_handler,debug,handle_err)

! connessione con database
    call idba_presentati(idbhandle,database,user,password)

! apertura database in lettura
    call idba_preparati(idbhandle,handle,"read","read","read")
    call idba_preparati(idbhandle,handleana,"read","read","read")

! leggo tutte le stazioni presenti in archivio
    call idba_quantesono(handle,nstaz)
    print*,'massimo numero pseudo-stazioni ',nstaz
    if(nstaz > MNSTAZ)then
        print*,'SONO TANTE ',nstaz,' STAZIONI!! SEI SICURO/A?'
    endif
! llocazione matrici anagrafica
    ALLOCATE(anaid(1:nstaz))

    PRINT*,'lselect= ',lselect
    call leggiana_db_scores(iana,anaid, &
    itipost,rmdo,nstaz,handle,lselect)
    print*,'numero massimo stazioni ',nstaz

! llocazione matrici dati
    ALLOCATE(oss(1:nstaz, 1:ngio))
    ALLOCATE(prev(1:nstaz, 1:ngio, 1:nrm))
    ALLOCATE(osse(1:nstaz))
    ALLOCATE(previ(1:nstaz, 1:nrm))

! llocazione matrice pesi
    ALLOCATE(wght(1:ngio, 1:nrm))
    ALLOCATE(temp_wght(1:nrm))
    ALLOCATE(distrib(1:nrm))

    open(11,file='scores_prob.dat',status='unknown')

    write(11,*)' variabile= ',cvar !uscite probabilistiche
    print*,'variabile ',cvar

    do iscad=1,nscad
        do is=1,4
            scad(is)=scadenze(is,iscad)
        enddo
        print*,'scadenza ',scad

        iscaddb=scad1+inc*(iscad-1)
        write(11,'(a,i3)')' scadenza= ',iscaddb
        write(22,'(a,i3)')' scadenza= ',iscaddb

        open(1,file='date.nml',status='old')
        open(2,file='pesi.nml',status='old')

        distrib = 0
        prev = rmddb
        oss = rmddb

        ng=0
        DO igio=1,ngio

            osse = rmddb
            previ = rmddb

            read(1,nml=date,err=9004)
        ! trovo data e ora di validita'
            call JELADATA5(data(1),data(2),data(3), &
            ora(1),ora(2),iminuti)
            iminuti=iminuti+iscaddb*60
            call JELADATA6(iday,imonth,iyear, &
            ihour,imin,iminuti)
            dataval(1)=iday
            dataval(2)=imonth
            dataval(3)=iyear
            oraval(1)=ihour
            oraval(2)=imin

            WRITE(55,*)'data validita'' ',dataval,oraval
            call flush(55)

        ! lettura previsioni da database

        ! ricominciamo perche' ho gia' fatto una query nella subroutine
            call idba_unsetall(handle)

            CALL idba_set (handle,'query','bigana')

            call idba_set (handle,"year",dataval(3))
            call idba_set (handle,"month",dataval(2))
            call idba_set (handle,"day",dataval(1))
            call idba_set (handle,"hour",oraval(1))
            call idba_set (handle,"min",oraval(2))
            call idba_set (handle,"sec",0)

        ! conversione delle scadenze in secondi (e correzione scadenze sbagliate)
            call converti_scadenze(4,scad,scaddb)
            call idba_set (handle,"p1",scaddb(2))
            call idba_set (handle,"p2",scaddb(3))
            call idba_set (handle,"pindicator",scaddb(4))

            call idba_set (handle,"var",cvar)

            do irm=1,nrm
                write(cel,'(i3.3)')irm
                descr=descrfisso(1:nlenvera(descrfisso)) &
                //'el'//cel

                call idba_set (handle,"rep_memo",descr)

            ! PRINT*,'prev ',descr,dataval,oraval,iscaddb

                call idba_voglioquesto (handle,N)
               ! PRINT*,'numero dati trovati= ',N
                if(N == 0)then
                    PRINT*,'pre - non ci sono dati'
                    print*,dataval,oraval
                    goto 66
                else
                 ! PRINT*,"pre - numero di dati trovati ",N
                endif

                do idati=1,N

                    call idba_dammelo (handle,btable)
                ! sara' da impostare mentre per ora e' solo richiesto
                    call idba_enq (handle,"leveltype", &
                    level(1))
                    call idba_enq (handle,"l1",level(2))
                    call idba_enq (handle,"l2",level(3))

                ! call idba_enqi (handle,"mobile",mobile)

                    call idba_enq (handle,"ana_id",icodice)

!mst  interrogo sezione anagrafica per avere l'altezza
                    CALL idba_set (handleana,"ana_id",icodice)
                    CALL idba_quantesono(handleana,USTAZ)
                    CALL idba_elencamele (handleana)
                    CALL idba_enq (handleana,"height",h)
                    
                    IF(iquota >= 0)THEN
                      IF(c_e_i(h))THEN
                        IF(iquota == 0)THEN !pianura
                          IF(h >= hlimite)goto20
                        ELSEIF(iquota == 1)THEN !montagna
                          IF(h < hlimite)goto20
                        ELSEIF(iquota > 1)THEN
                          PRINT*,'iquota non gestito ',iquota
                        ENDIF
                      ELSE
                        goto20
                      ENDIF
                    ENDIF

                    do i=1,nstaz
                        if(icodice == anaid(i))then
                            ipos=i
                        endif
                    enddo

                    call idba_enq (handle,btable,dato)

                    prev(ipos,igio,irm)=dato
                    previ(ipos,irm)=dato

                    20 continue

                enddo            ! idati
            enddo               ! nrm

        ! lettura osservazioni da database

            call idba_unsetall(handle)

            if(itipost == 0)then
                call idba_set (handle,"priomin",0)
                call idba_unset (handle,"priomax")
                call idba_set (handle,"query","best")
                descr="oss"
            elseif(itipost == 80)then
                call idba_unset (handle,"query")
                nlm=nlenvera(model)
                descr='oss'//descrfisso((nlm+1):(nlm+5))
                call idba_set (handle,"rep_memo",descr)
            elseif(itipost == 90)then
                call idba_unset (handle,"query")
                descr="ana"
                call idba_set (handle,"rep_memo",descr)
            endif

            CALL idba_set (handle,'query','bigana')
                
            call idba_set (handle,"year",dataval(3))
            call idba_set (handle,"month",dataval(2))
            call idba_set (handle,"day",dataval(1))
            call idba_set (handle,"hour",oraval(1))
            call idba_set (handle,"min",oraval(2))
            call idba_set (handle,"sec",0)

            if(scaddb(4) > 0)then
                p1=0-(scaddb(3)-scaddb(2))
                p2=0
            else
                p1=0
                p2=0
            endif

            call idba_set (handle,"p1",p1)
            call idba_set (handle,"p2",p2)
            call idba_set (handle,"pindicator",scaddb(4))

            call idba_set (handle,"var",cvar)

        ! print*,'oss ',descr,dataval,oraval

            call idba_voglioquesto (handle,N)
            if(N == 0)then
                PRINT*,'oss - non ci sono dati'
                print*,dataval,oraval
                goto 66
              ELSE
               !PRINT*,"oss - numero di dati trovati ",N
            endif

            do idati=1,N

                call idba_dammelo (handle,btable)
            ! sara' da impostare mentre per ora e' solo richiesto
                call idba_enq (handle,"leveltype", &
                level(1))
                call idba_enq (handle,"l1",level(2))
                call idba_enq (handle,"l2",level(3))

                call idba_enq (handle,"ana_id",icodice)

!mst  interrogo sezione anagrafica per avere l'altezza
                CALL idba_set (handleana,"ana_id",icodice)
                CALL idba_quantesono(handleana,USTAZ)
                CALL idba_elencamele (handleana)
                CALL idba_enq (handleana,"height",h)

                IF(iquota >= 0)THEN
                  IF(c_e_i(h))THEN
                    IF(iquota == 0)THEN !pianura
                      IF(h >= hlimite .OR. h < -900.)goto30
                    ELSEIF(iquota == 1)THEN !montagna
                      IF(h < hlimite .OR. h == REAL(imd))goto30
                    ELSEIF(iquota > 1)THEN
                      PRINT*,'iquota non gestito ',iquota
                    ENDIF
                  ELSE
                    goto30
                  ENDIF
                ENDIF

                do i=1,nstaz
                    if(icodice == anaid(i))then
                        ipos=i
                    endif
                enddo

                call idba_enq (handle,btable,dato)

                oss(ipos,igio)=dato
                osse(ipos)=dato

                30 continue

            enddo               ! idati

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
            write(22,'(a,i3)')' giorno= ',igio
            npu=0
            ossmed=0.
            do is=1,nstaz
                if(osse(is) /= rmddb)then
                    ossmed=ossmed+osse(is)
                    npu=npu+1
                endif
            enddo
            if(npu > 0)ossmed=ossmed/real(npu)
            write(22,'(a,f8.3)')' ossmed= ',ossmed
            if(ossmed > 0.2)then
                call terr(nstaz,nrm,osse,previ,nstaz,nrm, &
                nelsupens,rmddb,rmdo,temp_wght,loutput,iposiz)
                distrib(iposiz)=distrib(iposiz)+1
                ng=ng+1
            endif

            66 continue
            
          ENDDO                  !ngio
        close(1)               !naml date
        close(2)               !naml pesirm

        if(lwght)then
            write(44,'(a,i3)')'scadenza= ',iscaddb
            write(44,'(1x,a3,3x,a4)')'IRM','FREQ'
            do irm=1,MNRM
                write(44,'(3x,i1,2x,f6.4,a,i3)') &
                irm,real(distrib(irm))/real(ng),' ng= ',ng
            enddo
        endif

        write(11,'(3x,a,4x,a,4x,a, &
        5x,a,5x,a,5x,a,5x,a, &
        5x,a,4x,a,5x,a,4x,a,4x,a)') &
        'thr','ntot','nocc', &
        'bs','rel','res','bss', &
        'roca','cla','outr','rps','rpss'

    ! output degli scores

        if(prob)then
            call brier_prob(nstaz,ngio,nrm,oss,prev, &
            ngio,nstaz,nrm, &
            nelsupens,rmddb,rmds,soglie(1),wght,loutput, &
            ntot,rnocc,bs,bss)
        else
            CALL outrange(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
            nelsupens,rmddb,rmds,wght,loutput,outr)
            CALL ranked(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm,nsoglie, &
            nelsupens,rmddb,rmds,soglie,wght,loutput,rps,rpss)

            do iso=1,nsoglie

                CALL brier(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
                nelsupens,rmddb,rmds,soglie(iso),wght,loutput, &
                ntot,nocc,bs,rel,res,bss)
                call roc(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
                nelsupens,rmddb,rmds,soglie(iso),wght,loutput,ntot,nocc,roca)
                call costloss(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
                nelsupens,rmddb,rmds,soglie(iso),wght,loutput,ntot,nocc,clarea)
                print*,soglie(iso),ntot,nocc, &
                bs,rel,res,bss, &
                roca,clarea,outr
                write(11,'(1x,f5.1,2(2x,i6),9(2x,f6.3))') &
                soglie(iso),ntot,nocc, &
                bs,rel,res,bss, &
                roca,clarea,outr,rps,rpss

            enddo               !nsoglie
        endif

    enddo                     !nscad

    close(11)

! chiusura database
    call idba_fatto(handle)
    call idba_fatto(handleana)
    call idba_arrivederci(idbhandle)

    close(55)

    stop
    9001 print*,'errore nella lettura della namelist parameters'
    stop
    9002 print*,'errore nella lettura della namelist stat'
    stop
    9003 print*,'errore nella lettura della namelist lista'
    stop
    9004 print*,'errore nella lettura della namelist date'
    stop
    9005 print*,'errore nella lettura della namelist pesirm'
    stop
    end program

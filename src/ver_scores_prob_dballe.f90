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

    INCLUDE "dballe/dballef.h"

    parameter (MNSTAZ=5000,MNSCAD=72,MNGIO=190,MNORE=1)
    parameter (MNSOG=10,MNV=MNSTAZ*MNGIO*MNORE,MNRM=102)
! attenzione!!! Non sono usate, servono solo per dare
! un riferimento a chi dimensiona i vettori dinamicamente
    INTEGER ::   nora,ngio,nscad,nvar,nrm,nsoglie,nminobs,nelsupens
    integer ::   scad1,scad2,inc
    integer ::   scadenze(4,MNSCAD)
    integer ::   itipo,iana,imod,ls,iquota
    logical ::   ruota,media,massimo,prob,distr,diffh
    real ::      dxb,dyb,diffmax,thr,hlimite,soglie(MNSOG),perc
    integer ::   nore,ore(24)
    integer ::   data(3),ora(2),var(3),scad(4),level(3)
    integer ::   dataval(3),oraval(2),scaddb(4),p1,p2
    integer ::   icodice,itipost,ntot
    real ::      dato
    character descr*20,descrfisso*20,model*10
    character cvar*6,cel*3
    REAL ::      bs,bss,roca,clarea,outr,rps,rpss
    logical ::   lwght
    integer ::   nowght(MNRM),pesi(MNRM)
    integer ::   temp_wght(MNRM),distrib(MNRM)

    real, ALLOCATABLE :: oss(:,:),prev(:,:,:),osse(:),previ(:,:)
    integer, ALLOCATABLE :: anaid(:)
    integer, ALLOCATABLE :: wght(:,:)

    character(LEN=19) :: database,user,password

    character btable*10
    INTEGER :: handle,handle_err
    integer :: debug = 1

    data      rmdo/-999.9/,imd/32767/,rmddb/-999.9/,rmds/-9.999/
    namelist  /parameters/nora,ngio,nscad,scad1,scad2,inc, &
    nvar,nrm,nore,ore
    namelist  /stat/model,itipo,iana,imet,imod,ls,ruota, &
    nminobs,media,massimo,prob,distr,dxb,dyb,diffh,diffmax, &
    thr,perc
    NAMELIST  /lista/cvar,iquota,hlimite,nsoglie,soglie,lwght,nowght,nelsupens
    namelist  /date/data
    namelist  /scadenza/scadenze
    namelist  /pesirm/pesi
    namelist  /odbc/database,user,password

    OPEN(55,file='ctrl_output.dat',status='unknown')

    print*,'program scores_prob'

    open(1,file='odbc.nml',status='old',readonly)
    read(1,nml=odbc,err=9001)
    close(1)
! Read parameters
    open(1,file='parameters.nml',status='old',readonly)
    read(1,nml=parameters,err=9001)
    close(1)
! ora di inizio dei runs
    ora(1)=nora/100.
    ora(2)=mod(nora,100)
    open(1,file='stat.nml',status='old',readonly)
    read(1,nml=stat,err=9002)
    close(1)
    open(1,file='lista_ens.nml',status='old',readonly)
    read(1,nml=lista,err=9003)
    close(1)
    open(1,file='scadenze.nml',status='old',readonly)
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

! leggo tutte le stazioni presenti in archivio
    call idba_quantesono(handle,nstaz)
    print*,'massimo numero pseudo-stazioni ',nstaz
    if(nstaz > MNSTAZ)then
        print*,'SONO TANTE ',nstaz,' STAZIONI!! SEI SICURO/A?'
    endif
! llocazione matrici anagrafica
    ALLOCATE(anaid(1:nstaz))

    call leggiana_db_scores(iana,anaid, &
    itipost,rmdo,nstaz,handle)
    print*,'numero massimo stazioni ',nstaz

! llocazione matrici dati
    ALLOCATE(oss(1:nstaz, 1:ngio))
    ALLOCATE(prev(1:nstaz, 1:ngio, 1:nrm))
    ALLOCATE(osse(1:nstaz))
    ALLOCATE(previ(1:nstaz, 1:nrm))

! llocazione matrice pesi
    ALLOCATE(wght(1:ngio, 1:nrm))

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

        open(1,file='date.nml',status='old',readonly)
        open(2,file='pesi.nml',status='old',readonly)

        distrib = 0
        prev = rmddb
        oss = rmddb

        ng=0
        do igio=1,ngio

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
            ! print*,'numero dati trovati= ',N
                if(N == 0)then
                    PRINT*,'pre - non ci sono dati'
                    print*,dataval,oraval
                    goto 66
                else
            !      PRINT*,"pre - numero di dati trovati ",N
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

                    do i=1,nstaz
                        if(icodice == anaid(i))then
                            ipos=i
                        endif
                    enddo

                    if(iquota == 0)then !pianura
                        if(h >= hlimite)goto20
                    elseif(iquota == 1)then !montagna
                        if(h < hlimite)goto20
                    elseif(iquota > 1)then
                        print*,'iquota non gestito ',iquota
                    endif

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
          !      PRINT*,"oss - numero di dati trovati ",N
            endif

            do idati=1,N

                call idba_dammelo (handle,btable)
            ! sara' da impostare mentre per ora e' solo richiesto
                call idba_enq (handle,"leveltype", &
                level(1))
                call idba_enq (handle,"l1",level(2))
                call idba_enq (handle,"l2",level(3))

                call idba_enq (handle,"ana_id",icodice)

                do i=1,nstaz
                    if(icodice == anaid(i))then
                        ipos=i
                    endif
                enddo

                call idba_enq (handle,btable,dato)

                if(iquota == 0)then !pianura
                    if(h >= hlimite .OR. h < -900.)goto30
                elseif(iquota == 1)then !montagna
                    if(h < hlimite .OR. h == real(imd))goto30
                elseif(iquota > 1)then
                    print*,'iquota non gestito ',iquota
                endif

                oss(ipos,igio)=dato
                osse(ipos)=dato

                30 continue

            enddo               ! idati

        ! attribuizione dei pesi
            if(lwght)then
            ! se ho un mini-ensemble leggo i pesi
                read(2,nml=pesirm,err=9005)
                do irm=1,nrm
                    wght(igio,irm)=pesi(irm)
                enddo
            else
            ! se non voglio pesare li eguaglio a nowght
                do irm=1,nrm
                    wght(igio,irm)=nowght(irm)
                enddo
            endif
        ! non si pesa l'ensemble completo!
            if(nrm == 51)then
                do irm=1,nrm
                    wght(igio,irm)=1
                enddo
            endif
            do irm=1,nrm
                temp_wght(irm)=wght(igio,irm)
            enddo

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
                nelsupens,rmddb,rmdo,temp_wght,ipos)
                distrib(ipos)=distrib(ipos)+1
                ng=ng+1
            endif

            66 continue

        enddo                  !ngio
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
            nelsupens,rmddb,rmds,soglie(1),wght, &
            ntot,rnocc,bs,bss)
        else
            CALL outrange(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
            nelsupens,rmddb,rmds,wght,outr)
            CALL ranked(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm,nsoglie, &
            nelsupens,rmddb,rmds,soglie,wght,rps,rpss)

            do iso=1,nsoglie

                CALL brier(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
                nelsupens,rmddb,rmds,soglie(iso),wght, &
                ntot,nocc,bs,rel,res,bss)
                call roc(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
                nelsupens,rmddb,rmds,soglie(iso),wght,ntot,nocc,roca)
                call costloss(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
                nelsupens,rmddb,rmds,soglie(iso),wght,ntot,nocc,clarea)
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

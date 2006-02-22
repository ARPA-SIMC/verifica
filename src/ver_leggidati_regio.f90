    program leggidati_regio

! c   VERIFICA - leggidati_regio.f
! c   programma per caricare su dballe i dati osservati
! c   che provengono dalle regioni per la verifica di LAMI
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

    parameter (nstaz=2000,nmesi=100,MNRE=12)

    real :: lonoss(nstaz),latoss(nstaz),alte(nstaz)
    integer :: idata(3),block,station
    character(7) :: scode,code(nstaz),nomest(nstaz)*20,nome*20
    character reg(MNRE)*2,mese(nmesi)*2,anno(nmesi)*4
    character path*80,pathana*80,cdum*2

    character(19) :: database,user,password
    integer :: handle
    logical :: init,debug,rmmiss
    data init,debug,rmmiss/.false.,.true.,.false./
    external error_handle

    namelist  /regioni/path,pathana,nre,reg,nme,mese,anno
    namelist  /odbc/database,user,password

    data rmd/9999/,rmdo/-999.9/

    open(1,file='odbc.nml',status='old',readonly)
    read(1,nml=odbc)
    close(1)

    open(1,file='region.nml',status='old',readonly)
    read(1,nml=regioni)
    close(1)

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

! INIZIO CICLO SUI MESI

    do nm=1,nme

        print*,'----------------------------'
        print*,'MESE: ',mese(nm),' ANNO: ',anno(nm)

    ! INIZIO CICLO SULLE REGIONI
        do ns=1,nre

            print*,'----------------------------'
            print*,'regione ',reg(ns)
            print*,' '

        ! APERTURA FILE DI ANAGRAFICA ED ASSEGNAZIONE PARAMETRI DELLA STAZIONE

            print*,'file anag ',pathana(1:istr_lunghezza(pathana))// &
            reg(ns)//'_ANAG.DAT'
            open(1,file=pathana(1:istr_lunghezza(pathana))// &
            reg(ns)//'_ANAG.DAT',err=996)
            numestaz=0
            read(1,*)
            do istan=1,nstaz    !anag
                read(1,'(a7,1x,a20,1x,f8.3,1x,f8.3,1x,i4)',end=225) &
                scode,nome,wlat,wlon,ialt

                numestaz=numestaz+1

                code(istan)=scode
                nomest(istan)=nome
                latoss(istan)=wlat
                lonoss(istan)=wlon
                if(ialt == 9999)then
                    alte(istan)=-999.9
                else
                    alte(istan)=real(ialt)
                endif
            enddo               !anag

            225 continue

            close(1)

            print*,'numero stazioni: ',numestaz

        ! ora leggo i dati

            open(1,file=path(1:istr_lunghezza(path)) &
            //reg(ns)//anno(nm)// &
            mese(nm)//'.DAT',status='old',err=997)

            743 read(1,'(a7,1x,i4,i2,i2,1x,2i2,5(1x,i4))',end=223) &
            scode,idata(3),idata(2),idata(1),iora,imin, &
            ipreci,itemp,idirv,ivelv,iumrel
            do nstana=1,numestaz
                if (scode == code(nstana))then
                    nsts=nstana
                ! print*,'stazione ',nsts,scode
                    goto 198
                endif
            enddo
            write(300,*)'stazione non trovata: ',scode
            goto 743

            198 continue

        ! conversione delle variabili per l'archivio
            if(ipreci /= rmd)then
                preci=real(ipreci)*0.1
            else
                preci=rmdo
            endif
            if(itemp /= rmd)then
                temp=real(itemp)*0.1+273.16
            else
                temp=rmdo
            endif
            if(idirv /= rmd)then
                dirv=real(idirv)
            else
                dirv=rmdo
            endif
            if(ivelv /= rmd)then
                velv=real(ivelv)*0.1
            else
                velv=rmdo
            endif
            if(iumrel /= rmd)then
                umrel=real(iumrel)
            else
                velv=rmdo
            endif
        ! calcolo td da t e umrel
            if(itemp /= rmd .AND. iumrel /= rmd)then
                td=trug(umrel,temp)
            else
                td=rmdo
            endif

        ! INSERIMENTO DEI PARAMETRI NELL' ARCHIVIO


            call idba_unsetall (handle)

        ! print*,'datatime ',idata(3),idata(2),idata(1),iora,imin,00
            call idba_seti (handle,"year",idata(3))
            call idba_seti (handle,"month",idata(2))
            call idba_seti (handle,"day",idata(1))
            call idba_seti (handle,"hour",iora)
            call idba_seti (handle,"min",imin)
            call idba_seti (handle,"sec",00)

        ! codice per gli osservati delle regioni
            call idba_seti (handle,"rep_cod",50)

            call idba_setc (handle,"name",nomest(nsts))

            read(code(nsts),'(a2,i2.2,i3.3)')cdum,block,station
            call idba_seti (handle,"block",block)
            call idba_seti (handle,"station",station)

            call idba_setr (handle,"lat",latoss(nsts))
            call idba_setr (handle,"lon",lonoss(nsts))
            call idba_setr (handle,"height",alte(nsts))

            call idba_seti (handle,"mobile",0)

        ! inserimento dati

            if (preci /= rmdo) then
                call idba_seti (handle,"leveltype",1)
                call idba_seti (handle,"l1",0)
                call idba_seti (handle,"l2",0)
                call idba_seti (handle,"pindicator",4)
                call idba_seti (handle,"p1",-10800)
                call idba_seti (handle,"p2",0)
                call idba_setr(handle,"B13011",preci)
                call idba_prendilo (handle)
                call idba_unset (handle,"B13011")
            ! aggiungo altre info
            ! if (hmo.ne.rmd) then
            ! call idba_setc(handle,"*var", "B22021")
            ! call idba_seti(handle,"*B22071",3)
            ! call idba_critica(handle)
            ! end if

            endif

            if (temp /= rmdo) then
                call idba_seti (handle,"leveltype",105)
                call idba_seti (handle,"l1",2)
                call idba_seti (handle,"l2",0)
                call idba_seti (handle,"pindicator",0)
                call idba_seti (handle,"p1",0)
                call idba_seti (handle,"p2",0)
                call idba_setr(handle,"B12001",temp)
                call idba_prendilo (handle)
                call idba_unset (handle,"B12001")
            endif

            if (dirv /= rmdo) then
                call idba_seti (handle,"leveltype",105)
                call idba_seti (handle,"l1",10)
                call idba_seti (handle,"l2",0)
                call idba_seti (handle,"pindicator",0)
                call idba_seti (handle,"p1",0)
                call idba_seti (handle,"p2",0)
                call idba_setr(handle,"B11001",dirv)
                call idba_prendilo (handle)
                call idba_unset (handle,"B11001")
            endif

            if (velv /= rmdo) then
                call idba_seti (handle,"leveltype",105)
                call idba_seti (handle,"l1",10)
                call idba_seti (handle,"l2",0)
                call idba_seti (handle,"pindicator",0)
                call idba_seti (handle,"p1",0)
                call idba_seti (handle,"p2",0)
                call idba_setr(handle,"B11002",velv)
                call idba_prendilo (handle)
                call idba_unset (handle,"B11002")
            endif

            if (umrel /= rmdo) then
                call idba_seti (handle,"leveltype",105)
                call idba_seti (handle,"l1",2)
                call idba_seti (handle,"l2",0)
                call idba_seti (handle,"pindicator",0)
                call idba_seti (handle,"p1",0)
                call idba_seti (handle,"p2",0)
                call idba_setr(handle,"B13003",umrel)
                call idba_prendilo (handle)
                call idba_unset (handle,"B13003")
            endif

            if (td /= rmdo) then
                call idba_seti (handle,"leveltype",105)
                call idba_seti (handle,"l1",2)
                call idba_seti (handle,"l2",0)
                call idba_seti (handle,"pindicator",0)
                call idba_seti (handle,"p1",0)
                call idba_seti (handle,"p2",0)
                call idba_setr(handle,"B12003",td)
                call idba_prendilo (handle)
                call idba_unset (handle,"B12003")
            endif

            goto 743
            223 continue
            close(1)

        enddo                  !regione
    enddo                     !mese

    call idba_fatto(handle)
    call idba_arrivederci(idbhandle)

    stop
    996 print*,'errore apertura file anag ',ns,nm
    stop
    997 print*,'errore apertura file dati ',ns,nm
    stop
    end program

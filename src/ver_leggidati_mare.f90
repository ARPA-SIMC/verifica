    program leggidati_mare

! c   VERIFICA - leggidati_mare.f
! c   programma per caricare su dballe i dati osservati dalle boe
! c   dei parametri del mare (rete RON)
! c   Autore: Luca Delli Passeri su codice di Francesco Boccanera
! c   Revisionato: Paolo Patruno

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

! QUESTO PROGRAMMA DEVE LEGGERE I DATI DELLE BOE
! E CARICARLI NEL DATABASE

    parameter (nstaz=20,nmesi=100,nanni=100)

    real :: hmo,tm,tp,dmo,tma

! hmo = altezza media onda al posto di temp
! tm =periodo medio
! tp =periodo di picco
! dmo =direzione media di prop. dell'onda
! tma =temp ogni tre ore boa mare

    integer :: idata(3)

! ------

    data rmd/99.9/

    character nomboe(nstaz)*2,mese(nmesi)*2,anno(nanni)*4
    character path*80,pathana*80

    namelist  /boe/path,pathana,nboe,nomboe,nme,mese,anno

! --------

    character(19) :: database,user,password
    integer :: handle,rewrite
    logical :: init,debug,rmmiss
    character(1000) :: messaggio
    data init,debug,rmmiss/.false.,.true.,.false./
    external error_handle

    namelist  /odbc/database,user,password

! --------

    open(1,file='odbc.nml',status='old',readonly)
    read(1,nml=odbc)
    close(1)

    open(1,file='boe.nml',status='old',readonly)
    read(1,nml=boe)
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

! INIZIO CICLO SUL NUMERO DI BOE
    do ns=1,nboe

        print*,'----------------------------'
        print*,'nome boa ',nomboe(ns)
        print*,' '

    ! APERTURA FILE DI ANAGRAFICA ED ASSEGNAZIONE PARAMETRI DELLA STAZIONE

        open(1,file=pathana(1:istr_lunghezza(pathana))//nomboe(ns)// &
        '_ANAG.dat',status='old')
        numestaz=0
        read(1,*)
        read(1,*)
        read(1,'(9x,f6.3,4x,f6.3,4x,i1)')wlat,wlon,ialt
        close(1)


    ! print*,"setto i parametri"

        call idba_unsetall (handle)

        call idba_seti (handle,"rep_cod",1)

        call idba_setc (handle,"name",nomboe(ns))
        call idba_seti (handle,"block",1)
        call idba_seti (handle,"station",ns)

        call idba_setr (handle,"lat",wlat)
        call idba_setr (handle,"lon",wlon)
        call idba_seti (handle,"height",ialt)

        call idba_seti (handle,"leveltype",1)
        call idba_seti (handle,"l1",0)
        call idba_seti (handle,"l2",0)

        call idba_seti (handle,"pindicator",0)
        call idba_seti (handle,"p1",0)
        call idba_seti (handle,"p2",0)

        call idba_seti (handle,"mobile",0)

    ! INIZIO CICLO SUI MESI

        do nm=1,nme

            print*,'MESE: ',mese(nm)," ANNO: ",anno(nm)
            print*,' '

        ! ora leggo i dati

            open(1,file=path(1:istr_lunghezza(path)) &
            //nomboe(ns)//anno(nm)// &
            mese(nm)//'.dat.new',status='old',form='formatted')


            743 read(1,*,end=223) &
            idata(1),idata(2),idata(3),iora,imin,isec, &
            hmo,tm,tp,dmo,tma

            if (hmo /= rmd) hmo=hmo*1.
            if (tm /= rmd) tm=tm*1.
            if (tp /= rmd) tp=tp*1.
            if (dmo /= rmd) dmo=dmo*1.
            if (tma /= rmd) tma=tma*1.+273.15

        ! INSERIMENTO DEI PARAMETRI NELL' ARCHIVIO

            call idba_seti (handle,"year",idata(3))
            call idba_seti (handle,"month",idata(2))
            call idba_seti (handle,"day",idata(1))
            call idba_seti (handle,"hour",iora)
            call idba_seti (handle,"min",imin)
            call idba_seti (handle,"sec",isec)


            if ( .NOT. rmmiss)then

            ! inserimento dati
                call idba_unset (handle,"B22070") !SIGNIFICANT WAVE HEIGHT [M, ##.##]
                call idba_unset (handle,"B22074") !AVERAGE WAVE PERIOD [S, ##.#]
                call idba_unset (handle,"B22001") !DIRECTION OF WAVES [DEGREE TRUE, 3 digits]
                call idba_unset (handle,"B22071") !SPECTRAL PEAK WAVE PERIOD [S, ##.#]
                call idba_unset (handle,"B22042") !SEA/WATER TEMPERATURE [K, ###.#]

                if (hmo /= rmd) call idba_setr(handle,"B22070",hmo)
                if (tm /= rmd)  call idba_setr(handle,"B22074",tm)
                if (tp /= rmd)  call idba_setr(handle,"B22071",tp)
                if (dmo /= rmd) call idba_setr(handle,"B22001",dmo)
                if (tma /= rmd) call idba_setr(handle,"B22042",tma)

            else

            ! inserimento dati con cancellazione dati segnati mancanti
                if (hmo /= rmd)then
                    call idba_setr (handle,   "B22070",hmo)
                else
                    call idba_unset (handle,  "B22070")
                    print *,"cancello i dati   B22070",idata,iora
                    call idba_setc(handle,"var","B22070")
                    call idba_dimenticami(handle)
                end if

                if (tm /= rmd)then
                    call idba_setr(handle,    "B22074",tm)
                else
                    call idba_unset (handle,  "B22074")
                    print *,"cancello i dati   B22074",idata,iora
                    call idba_setc(handle,"var","B22074")
                    call idba_dimenticami(handle)
                end if

                if (tp /= rmd)then
                    call idba_setr(handle,    "B22071",tp)
                else
                    call idba_unset (handle,  "B22071")
                    print *,"cancello i dati   B22071",idata,iora
                    call idba_setc(handle,"var","B22071")
                    call idba_dimenticami(handle)
                end if

                if (dmo /= rmd)then
                    call idba_setr(handle,    "B22001",dmo)
                else
                    call idba_unset (handle,  "B22001")
                    print *,"cancello i dati   B22001",idata,iora
                    call idba_setc(handle,"var","B22001")
                    call idba_dimenticami(handle)
                end if

                if (tma /= rmd)then
                    call idba_setr(handle,    "B22042",tma)
                else
                    call idba_unset (handle,  "B22042")
                    print *,"cancello i dati   B22042",idata,iora
                    call idba_setc(handle,"var","B22042")
                    call idba_dimenticami(handle)
                end if

            end if

            call idba_prendilo (handle)

        ! aggiungo altre info
        ! if (hmo.ne.rmd) then
        ! call idba_setc(handle,"*var", "B22021")
        ! call idba_seti(handle,"*B22071",3)
        ! call idba_critica(handle)
        ! end if

            goto 743
            223 continue
            close(1)

        enddo                  !mese
    enddo                     !boa

    call idba_fatto(handle)
    call idba_arrivederci(idbhandle)

    stop

    end program



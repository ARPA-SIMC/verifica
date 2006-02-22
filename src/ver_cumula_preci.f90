    program cumula_preci

! c   VERIFICA - cumula_preci.f
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

    parameter (MNSTAZ=10000)

    real :: rlon,rlat,h
    integer :: giornoi,mesei,annoi,orai,giornof,mesef,annof,oraf,incora
    integer :: data(3),ora(2),p1,p2,repcod
    character cvar*6
    character btable*10

    character(19) :: database,user,password
    integer :: handle,rewrite
    logical :: init,debug,rmmiss
    data init,debug,rmmiss/.false.,.true.,.false./
    external error_handle

    namelist  /odbc/database,user,password
    namelist  /cumula/giornoi,mesei,annoi,orai, &
    giornof,mesef,annof,oraf,incora,ncum

! ncum = intervallo di cumulazione desiderato (ore)
! incora = intervallo di cumulazione delle preci nel database (ore)

    data rmd/9999/,rmdo/-999.9/
    data cvar/'B13011'/
    data repcod/50/

    open(1,file='odbc.nml',status='old',readonly)
    read(1,nml=odbc)
    close(1)

    open(1,file='cumula.nml',status='old',readonly)
    read(1,nml=cumula)
    close(1)

    data(3)=annoi
    data(2)=mesei
    data(1)=giornoi
    ora(1)=orai
    ora(2)=0
    call JELADATA5(giornoi,mesei,annoi,orai,0,iminuti)
    call JELADATA5(giornof,mesef,annof,oraf,0,iminmax)
    print*,'dal ',giornoi,mesei,annoi,orai,0
    print*,'al ',giornof,mesef,annof,oraf,0
    print*,'incremento ',incora

! PREPARAZIONE DELL' ARCHIVIO

! gestione degli errori
    call idba_error_set_callback(0,error_handle,debug,handle_err)

! connessione con database
    print*,"database=",database
    call idba_presentati(idbhandle,database,user,password)

! apertura database in lettura
    call idba_preparati(idbhandle,handler,"read","read","read")
! apertura database in lettura
    call idba_preparati(idbhandle,handlerr,"read","read","read")

! apertura database in scrittura
    call idba_preparati(idhandle,handle,"reuse","add","add")

! INIZIO CICLO SUI GIORNI
    do while (iminuti.le.iminmax)

        call idba_unsetall (handler)
        call idba_unsetall (handlerr)

        call idba_seti (handler,"year",data(3))
        call idba_seti (handler,"month",data(2))
        call idba_seti (handler,"day",data(1))
        call idba_seti (handler,"hour",ora(1))
        call idba_seti (handler,"min",ora(2))
        call idba_seti (handler,"sec",00)

        p1=-(incora*60*60)
        p2=0

        call idba_seti (handler,"pindicator",4)
        call idba_seti (handler,"p1",p1)
        call idba_seti (handler,"p2",p2)
        call idba_seti (handlerr,"pindicator",4)
        call idba_seti (handlerr,"p1",p1)
        call idba_seti (handlerr,"p2",p2)

        call idba_seti (handler,"rep_cod",repcod)
        call idba_seti (handlerr,"rep_cod",repcod)

        call idba_seti (handler,"leveltype",1)
        call idba_seti (handler,"l1",0)
        call idba_seti (handler,"l2",0)
        call idba_seti (handlerr,"leveltype",1)
        call idba_seti (handlerr,"l1",0)
        call idba_seti (handlerr,"l2",0)

        call idba_setc (handler,"var",cvar)
        call idba_setc (handlerr,"var",cvar)

        print*,data,ora,p1,cvar

        call idba_voglioquesto (handler,N)
        if(N == 0)then
            print*,'oss - non ci sono dati'
            print*,dataval,oraval
            goto 66
        else
            print*,'oss - numero di dati trovati ',N
        endif

        do idati=1,N

            call idba_dammelo (handler,btable)

            call idba_enqr (handler,"lat",rlat)
            call idba_enqr (handler,"lon",rlon)
            call idba_enqr (handler,"height",h)

            call idba_enqi (handler,"ana_id",icodice)

            call idba_enqr (handler,btable,dato)
        ! print*,'primo valore ',dato

            prec=dato

        ! chiedo gli altri dati necessari per fare la cumulata
            iminutiw=iminuti
            if(mod(ora(1),6) == 0)then
                nore=ncum/incora
                ndati=0
                do i=1,(nore-1)
                    iminutiw=iminutiw-incora*60
                    call JELADATA6(idayw,imonthw,iyearw,ihourw,iminw, &
                    iminutiw)
                ! print*,'estraggo ',idayw,imonthw,iyearw,ihourw,iminw
                    call idba_seti (handlerr,"year",iyearw)
                    call idba_seti (handlerr,"month",imonthw)
                    call idba_seti (handlerr,"day",idayw)
                    call idba_seti (handlerr,"hour",ihourw)
                    call idba_seti (handlerr,"min",iminw)
                    call idba_seti (handlerr,"sec",00)

                ! settare le coordinate non serve! Bisogna settare ana_id
                    call idba_seti (handlerr,"ana_id",icodice)

                    call idba_voglioquesto (handlerr,NN)

                ! print*,'altri dati trovati ',NN

                    ndati=ndati+1
                    if(NN > 1)print*,'non e'' possibile!'
                    do jdati=1,NN
                        call idba_dammelo (handlerr,btable)
                        call idba_enqr (handlerr,btable,dato)
                        prec=prec+dato
                    enddo
                enddo
            ! print*,'numero altri dati usati ',ndati

            ! INSERIMENTO DEI PARAMETRI NELL' ARCHIVIO

                call idba_unsetall (handle)

                print*,'datatime ',data(3),data(2),data(1),ora(1),ora(2)
                call idba_seti (handle,"year",data(3))
                call idba_seti (handle,"month",data(2))
                call idba_seti (handle,"day",data(1))
                call idba_seti (handle,"hour",ora(1))
                call idba_seti (handle,"min",ora(2))
                call idba_seti (handle,"sec",00)

                p1=-(ncum*60*60)
                p2=0

                call idba_seti (handle,"pindicator",4)
                call idba_seti (handle,"p1",p1)
                call idba_seti (handle,"p2",p2)

                call idba_seti (handle,"rep_cod",repcod)

                call idba_setr (handle,"lat",rlat)
                call idba_setr (handle,"lon",rlon)
                call idba_setr (handle,"height",h)

                call idba_seti (handle,"mobile",0)

                call idba_seti (handle,"leveltype",1)
                call idba_seti (handle,"l1",0)
                call idba_seti (handle,"l2",0)

            ! inserimento dati

                call idba_unset (handle,"B13011") !TOTAL PRECIPITATION [KG/M2]

                if (preci /= rmdo) call idba_setr(handle,"B13011",prec)

                call idba_prendilo (handle)

            endif

        enddo

        66 continue

        print*,'numero stazioni: ',numestaz

    ! trovo nuova data e ora di validita' del dato
        call JELADATA5(data(1),data(2),data(3),ora(1),ora(2), &
        iminuti)
        iminuti=iminuti+incora*60
        call JELADATA6(iday,imonth,iyear,ihour,imin,iminuti)
        data(1)=iday
        data(2)=imonth
        data(3)=iyear
        ora(1)=ihour
        ora(2)=imin

    enddo                     !dowhile

    call idba_fatto(handler)
    call idba_fatto(handle)
    call idba_arrivederci(idbhandle)

    stop
    end program

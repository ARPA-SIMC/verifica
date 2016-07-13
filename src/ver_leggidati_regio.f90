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

!!$gfortran -I/home/marsigli/svn_new/verifica/branches/dballe4/src/ 
!!$-o ver_leggidati_regio 
!!$/home/marsigli/svn_new/verifica/branches/dballe4/src/ver_leggidati_regio.f90  
!!$/home/marsigli/svn_new/verifica/branches/dballe4/src/util_dballe.o 
!!$-lhibu -lmega -lgrib_util -lemos -ldballef -lcnf -lodbc -lpopt -lm 
!!$-lsim_base -lsim_grib 

    USE util_dballe

    IMPLICIT NONE

    integer, parameter :: nstaz=2000,nmesi=100,MNRE=12

    real :: lonoss(nstaz),latoss(nstaz)
    real :: alte(nstaz)
    integer :: idata(3)
    character(LEN=7) :: scode,code(nstaz)
    character(LEN=20) :: nomest(nstaz),nome
    integer :: imd ! valore mancante che si trova nei file delle regioni
    REAL :: rmdo ! valore interno al programma, non scrivo il valore in db se e' dato mancante!
! namelist variables
    CHARACTER(len=80) :: path='',pathana=''
    INTEGER :: nre=1,nme=1
    CHARACTER(len=2) :: reg(MNRE)='',mese(nmesi)=''
    CHARACTER(len=4) :: anno(nmesi)=''
    CHARACTER(512) :: database='',user='',password=''

    integer :: nsts,ns,nm,nstana,numestaz
    integer :: ialt,id_ana,idbhandle,imin,iora,istan
    integer :: itemp,ipreci,iumrel,ivelv,idirv
    real :: dirv,velv,td,temp,umrel,preci,wlon,wlat

    integer :: handle
    integer :: debug=1
    integer :: handle_err

    integer :: ier

    namelist  /regioni/path,pathana,nre,reg,nme,mese,anno
    namelist  /odbc/database,user,password

    data imd/9999/,rmdo/-999.9/

    print*,'program leggidati_regio'

    open(1,file='odbc.nml',status='old')
    read(1,nml=odbc)
    close(1)

    open(1,file='region.nml',status='old')
    read(1,nml=regioni)
    close(1)

! PREPARAZIONE DELL' ARCHIVIO
    print*,"database=",database

    ier=idba_error_set_callback(0,C_FUNLOC(idba_default_error_handler),debug,handle_err)

    ier=idba_presentati(idbhandle,database)

    ier=idba_preparati(idbhandle,handle,"write","write","write")

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
                nomest(istan)=nome !mst potrebbe bastare il codice della stazione es:EM00230
                latoss(istan)=wlat
                lonoss(istan)=wlon
                if(ialt == imd)then
                  alte(istan)=dba_mvr
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
                    nsts=nstana !mst più che il nome è utile il codice della stazione (cioè scode) perchè contiene sempre l'identificativo della regione
                    goto 198
                endif
            enddo
            write(300,*)'stazione non trovata: ',scode
            goto 743

            198 continue

        ! conversione delle variabili per l'archivio
            if(ipreci /= imd)then
                preci=real(ipreci)*0.1
            else
                preci=rmdo
            endif
            if(itemp /= imd)then
                temp=real(itemp)*0.1+273.16
            else
                temp=rmdo
            endif
            if(idirv /= imd)then
                dirv=real(idirv)
            else
                dirv=rmdo
            endif
            if(ivelv /= imd)then
                velv=real(ivelv)*0.1
            else
                velv=rmdo
            endif
            if(iumrel /= imd)then
                umrel=real(iumrel)
            else
                umrel=rmdo
            endif
        ! calcolo td da t e umrel
            IF(itemp /= imd .AND. iumrel /= imd .AND. iumrel /= 0)THEN
              td=trug(umrel,temp)
            ELSE
              td=rmdo
            ENDIF

        ! INSERIMENTO DEI PARAMETRI NELL' ARCHIVIO

            ier=idba_unsetall (handle)

! anagrafica
            ier=idba_setcontextana (handle)
! obbligatori
! setto la rete dei dati con questa anagrafica
            ier=idba_set (handle,"rep_memo",'regioni') !rete=regioni

            print*,nsts,latoss(nsts)

            ier=idba_set (handle,"lat",latoss(nsts))
            ier=idba_set (handle,"lon",lonoss(nsts))
            ier=idba_set (handle,"mobile",0)

            ier=idba_set (handle,"name",nomest(nsts))
            ier=idba_set (handle,"block",69)
            ier=idba_set (handle,"height",alte(nsts))

            ier=idba_prendilo (handle)

            ier=idba_enq(handle,"*ana_id",id_ana)
! dati
            ier=idba_unsetall (handle)

            ! obbligatori
!            ier=idba_set (handle,"lat",latoss(nsts))
!            ier=idba_set (handle,"lon",lonoss(nsts))
!            ier=idba_set (handle,"mobile",0)

            ier=idba_set(handle,"ana_id",id_ana)

        ! print*,'datatime ',idata(3),idata(2),idata(1),iora,imin,00
            ier=idba_setdate (handle,idata(3),idata(2),idata(1),iora,imin,00)

        ! codice per gli osservati delle regioni
            ier=idba_set (handle,"rep_memo",'regioni') !rete=regioni

        ! inserimento dati
            if (preci /= rmdo) then
               ier=idba_setlevel(handle,1,0,0,0)
               ier=idba_settimerange(handle,1,0,10800)
               ier=idba_set(handle,"B13011",preci)
               ier=idba_prendilo (handle)
               ier=idba_unset (handle,"B13011")
            ! aggiungo altre info
            ! if (hmo.ne.imd) then
            ! ier=idba_setc(handle,"*var", "B22021")
            ! ier=idba_seti(handle,"*B22071",3)
            ! ier=idba_critica(handle)
            ! end if

            endif

            if (temp /= rmdo) then
               ier=idba_setlevel(handle,103,2000,0,0)
               ier=idba_settimerange(handle,254,0,0)
               ier=idba_set(handle,"B12101",temp)
               ier=idba_prendilo (handle)
               ier=idba_unset (handle,"B12101")
            endif

            if (dirv /= rmdo) then
               ier=idba_setlevel(handle,103,10000,0,0)
               ier=idba_settimerange(handle,254,0,0)
               ier=idba_set(handle,"B11001",dirv)
               ier=idba_prendilo (handle)
               ier=idba_unset (handle,"B11001")
            endif

            if (velv /= rmdo) then
               ier=idba_setlevel(handle,103,10000,0,0)
               ier=idba_settimerange(handle,254,0,0)
               ier=idba_set(handle,"B11002",velv)
               ier=idba_prendilo (handle)
               ier=idba_unset (handle,"B11002")
            endif

            if (umrel /= rmdo) then
               ier=idba_setlevel(handle,103,2000,0,0)
               ier=idba_settimerange(handle,254,0,0)
               ier=idba_set(handle,"B13003",umrel)
               ier=idba_prendilo (handle)
               ier=idba_unset (handle,"B13003")
            endif

            if (td /= rmdo) then
               ier=idba_setlevel(handle,103,2000,0,0)
               ier=idba_settimerange(handle,254,0,0)
               ier=idba_set(handle,"B12103",td)
               ier=idba_prendilo (handle)
               ier=idba_unset (handle,"B12103")
            endif

            goto 743
            223 continue
            close(1)

        enddo                  !regione
    enddo                     !mese

    ier=idba_fatto(handle)
    ier=idba_arrivederci(idbhandle)

    stop
    996 print*,'errore apertura file anag ',ns,nm
    stop
    997 print*,'errore apertura file dati ',ns,nm
    stop
    end program

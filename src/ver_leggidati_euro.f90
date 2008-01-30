    PROGRAM ver_leggidati_euro

! c   VERIFICA - ver_leggidati_euro.f
! c   programma per caricare su dballe i dati osservati
! c   che provengono dai membri di COSMO
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

    parameter (nstaz=20000,nmesi=100)

    real :: lonoss(nstaz),latoss(nstaz)
    integer :: alte(nstaz)
    REAL :: preci(nstaz,31)
    INTEGER :: data(3),ora(2)
    INTEGER :: giomax,nme,ialt
    CHARACTER mese(nmesi)*2,anno(nmesi)*4,canno*4,cmese*2
    CHARACTER path*80,reg*2

    character(19) :: database,user,password
    integer :: handle
    integer :: debug = 1
    integer :: handle_err

    NAMELIST  /euro/path,reg,nme,mese,anno
    namelist  /odbc/database,user,password

    data imd/9999/

    open(1,file='odbc.nml',status='old')
    read(1,nml=odbc)
    close(1)

    open(1,file='euro.nml',status='old')
    read(1,nml=euro)
    close(1)

! PREPARAZIONE DELL' ARCHIVIO
    print*,"database=",database

    call idba_error_set_callback(0,idba_default_error_handler,debug,handle_err)

    call idba_presentati(idbhandle,database,user,password)

    CALL idba_preparati(idbhandle,handle,"write","write","write")

! INIZIO CICLO SUI MESI

    DO nm=1,nme

        print*,'----------------------------'
        print*,'MESE: ',mese(nm),' ANNO: ',anno(nm)

        cmese=mese(nm)
        canno=anno(nm)
        READ(cmese,'(i2)')imese
        READ(canno,'(i4)')ianno
        giomax=ngiorni_mese(imese,ianno)

        OPEN(1,file=path(1:nlenvera(path))//reg//canno(3:4)//cmese// &
        '.txt',status='old')
        READ(1,*)

        numestaz=0
        DO istaz=1,nstaz
           READ(1,*,END=111) &
           lonoss(istaz),latoss(istaz),ialt, &
           (preci(istaz,igio),igio=1,giomax)
          
          numestaz=numestaz+1
          
          IF(ialt == imd)THEN
            alte(istaz)=dba_mvi
          ELSE
            alte(istaz)=ialt
          ENDIF

        ENDDO
        
111     CONTINUE
        
        CLOSE(1)
        
        PRINT*,'numero stazioni: ',numestaz
        
! INSERIMENTO DEI PARAMETRI NELL' ARCHIVIO

        data(1)=1
        data(2)=imese
        data(3)=ianno
        ora(1)=06
        ora(2)=00
        
        DO igio=1,giomax

          DO istaz=1,numestaz
            
            CALL idba_unsetall (handle)
          
! anagrafica
            CALL idba_setcontextana (handle)
            ! obbligatori
            CALL idba_set (handle,"lat",latoss(istaz))
            CALL idba_set (handle,"lon",lonoss(istaz))
            CALL idba_set (handle,"mobile",0)

            CALL idba_set (handle,"height",alte(istaz))
            CALL idba_set (handle,"block",69)
        
            CALL idba_prendilo (handle)

            CALL idba_enq(handle,"ana_id",id_ana)

! dati
            call idba_unsetall (handle)

            CALL idba_set(handle,"ana_id",id_ana)
            
            ! print*,'datatime ',data(3),data(2),data(1),ora(1),ora(2),00
            CALL idba_set (handle,"year",data(3))
            CALL idba_set (handle,"month",data(2))
            CALL idba_set (handle,"day",data(1))
            CALL idba_set (handle,"hour",ora(1))
            CALL idba_set (handle,"min",ora(2))
            CALL idba_set (handle,"sec",00)
            
            CALL idba_set (handle,"leveltype",1)
            CALL idba_set (handle,"l1",0)
            CALL idba_set (handle,"l2",0)
            CALL idba_set (handle,"pindicator",4)
            CALL idba_set (handle,"p1",-86400)
            CALL idba_set (handle,"p2",0)
            
            ! codice per gli osservati delle regioni
            CALL idba_set (handle,"rep_cod",50)
            
          ! inserimento dati
            IF(preci(istaz,igio) >= 0.)THEN
              CALL idba_set(handle,"B13011",preci(istaz,igio))
              CALL idba_prendilo (handle)
              CALL idba_unset (handle,"B13011")
            ENDIF
            ! aggiungo altre info
            ! if (hmo.ne.imd) then
            ! call idba_setc(handle,"*var", "B22021")
            ! call idba_seti(handle,"*B22071",3)
            ! call idba_critica(handle)
            ! end if
            
          ENDDO !stazioni
            
          CALL JELADATA5(DATA(1),DATA(2),DATA(3),ora(1),ora(2),iminuti)
          iminuti=iminuti+1440
          CALL JELADATA6(idayv,imonthv,iyearv,ihourv,iminv,iminuti)
          data(1)=idayv
          data(2)=imonthv
          data(3)=iyearv
          ora(1)=ihourv
          ora(2)=iminv
          
        ENDDO !giorni

      ENDDO !mese
      
      CALL idba_fatto(handle)
      CALL idba_arrivederci(idbhandle)
      
      STOP
997   PRINT*,'errore apertura file dati ',nm
      STOP
      END PROGRAM ver_leggidati_euro

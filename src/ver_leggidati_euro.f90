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

    REAL :: lonoss(nstaz),latoss(nstaz)
    REAL :: alte(nstaz)
    REAL :: preci(nstaz,31)
    INTEGER :: DATA(3),ora(2)
    INTEGER :: giomax,ialt
    CHARACTER :: canno*4,cmese*2
! namelist variables
    CHARACTER(len=80) :: path=''
    CHARACTER(len=2) :: reg='',mese(nmesi)=''
    CHARACTER(len=4) :: anno(nmesi)=''
    INTEGER :: nme=0
    CHARACTER(512) :: database='',user='',password=''
    
    integer :: handle
    integer :: debug=1
    integer :: handle_err

    integer :: ier

    NAMELIST /euro/path,reg,nme,mese,anno
    namelist /odbc/database,user,password

    data imd/9999/

    print*,'program leggidati_euro'

    open(1,file='odbc.nml',status='old')
    read(1,nml=odbc)
    close(1)

    open(1,file='euro.nml',status='old')
    read(1,nml=euro)
    close(1)

! PREPARAZIONE DELL' ARCHIVIO
    print*,"database=",database

    ier=idba_error_set_callback(0,C_FUNLOC(idba_default_error_handler),debug,handle_err)

    ier=idba_presentati(idbhandle,database)

    ier=idba_preparati(idbhandle,handle,"write","write","write")

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
            alte(istaz)=dba_mvr
          ELSE
            alte(istaz)=real(ialt)
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
            
            ier=idba_unsetall (handle)
          
! anagrafica
            ier=idba_setcontextana (handle)
! obbligatori
! setto la rete dei dati con questa anagrafica
            ier=idba_set (handle,"rep_memo",'regioni') !rete=regioni

            ier=idba_set (handle,"lat",latoss(istaz))
            ier=idba_set (handle,"lon",lonoss(istaz))
            ier=idba_set (handle,"mobile",0)

            ier=idba_set (handle,"height",alte(istaz))
            ier=idba_set (handle,"block",69)
        
            ier=idba_prendilo (handle)

            ier=idba_enq(handle,"*ana_id",id_ana)

! dati
            ier=idba_unsetall (handle)

            ier=idba_set(handle,"ana_id",id_ana)
            
            ! print*,'datatime ',data(3),data(2),data(1),ora(1),ora(2),00
            ier=idba_set (handle,"year",data(3))
            ier=idba_set (handle,"month",data(2))
            ier=idba_set (handle,"day",data(1))
            ier=idba_set (handle,"hour",ora(1))
            ier=idba_set (handle,"min",ora(2))
            ier=idba_set (handle,"sec",00)
            
            ier=idba_setlevel(handle,1,0,0,0)
            ier=idba_settimerange(handle,1,0,86400)

            ! codice per gli osservati delle regioni
            ier=idba_set (handle,"rep_memo",'regioni')
            
          ! inserimento dati
            IF(preci(istaz,igio) >= 0.)THEN
              ier=idba_set(handle,"B13011",preci(istaz,igio))
              ier=idba_prendilo (handle)
              ier=idba_unset (handle,"B13011")
            ENDIF
            ! aggiungo altre info
            ! if (hmo.ne.imd) then
            ! ier=idba_setc(handle,"*var", "B22021")
            ! ier=idba_seti(handle,"*B22071",3)
            ! ier=idba_critica(handle)
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
      
      ier=idba_fatto(handle)
      ier=idba_arrivederci(idbhandle)
      
      STOP
      END PROGRAM ver_leggidati_euro

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

    parameter (nstaz=6000,nmesi=100)

    real :: lonoss(nstaz),latoss(nstaz),alte(nstaz)
    REAL :: preci(nstaz,31)
    INTEGER :: data(3),ora(2)
    INTEGER :: giomax,nme,ialt
    CHARACTER mese(nmesi)*2,anno(nmesi)*4,canno*4,cmese*2
    character path*80

    character(19) :: database,user,password
    integer :: handle
    logical :: init,debug,rmmiss
    data init,debug,rmmiss/.false.,.true.,.false./
    external error_handle

    namelist  /euro/path,nme,mese,anno
    namelist  /odbc/database,user,password

    data rmd/9999/,rmdo/-999.9/

    open(1,file='odbc.nml',status='old',readonly)
    read(1,nml=odbc)
    close(1)

    open(1,file='euro.nml',status='old',readonly)
    read(1,nml=euro)
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

    DO nm=1,nme

        print*,'----------------------------'
        print*,'MESE: ',mese(nm),' ANNO: ',anno(nm)

        cmese=mese(nm)
        canno=anno(nm)
        READ(cmese,'(i2)')imese
        READ(canno,'(i4)')ianno
        giomax=ngiorni_mese(imese,ianno)

        OPEN(1,file=path(1:nlenvera(path))//'Co'//canno(3:4)//cmese// &
        '.txt',status='old',readonly)
        READ(1,*)

        numestaz=0
        DO istaz=1,nstaz
          READ(1,'(2(2x,f6.2),1x,i4,31(1x,f6.1)',END=111)lonoss(istaz), &
          latoss(istaz),ialt,(preci(istaz,igio),igio=1,giomax)
          
          numestaz=numestaz+1
          
          IF(ialt == 9999)THEN
            alte(istan)=-999.9
          ELSE
            alte(istan)=REAL(ialt)
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
          
          ! print*,'datatime ',data(3),data(2),data(1),ora(1),ora(2),00
            CALL idba_seti (handle,"year",data(3))
            CALL idba_seti (handle,"month",data(2))
            CALL idba_seti (handle,"day",data(1))
            CALL idba_seti (handle,"hour",ora(1))
            CALL idba_seti (handle,"min",ora(2))
            CALL idba_seti (handle,"sec",00)
            
            CALL idba_seti (handle,"leveltype",1)
            CALL idba_seti (handle,"l1",0)
            CALL idba_seti (handle,"l2",0)
            CALL idba_seti (handle,"pindicator",4)
            CALL idba_seti (handle,"p1",-86400)
            CALL idba_seti (handle,"p2",0)
            
            ! codice per gli osservati delle regioni
            CALL idba_seti (handle,"rep_cod",50)
            
            CALL idba_setr (handle,"lat",latoss(istaz))
            CALL idba_setr (handle,"lon",lonoss(istaz))
            CALL idba_setr (handle,"height",alte(istaz))
            
            CALL idba_seti (handle,"mobile",0)
          
          ! inserimento dati
            IF(preci(istaz,igio) >= 0.)THEN
              CALL idba_setr(handle,"B13011",preci(istaz,igio))
              CALL idba_prendilo (handle)
              CALL idba_unset (handle,"B13011")
            ENDIF
            ! aggiungo altre info
            ! if (hmo.ne.rmd) then
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

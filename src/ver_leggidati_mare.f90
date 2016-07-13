program leggidati_mare

! c   VERIFICA - leggidati_mare.f
! c   programma per caricare su dballe i dati osservati dalle boe
! c   dei parametri del mare (rete RON)
! c   Autore: Luca Delli Passeri su codice di Francesco Boccanera
! c   Revisionato: Paolo Patruno, Andrea Valentini

! Copyright (C) 2006

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

! QUESTO PROGRAMMA  LEGGE I DATI DELLE BOE
! E LI CARICA NEL DATABASE DB-all.e

  USE util_dballe

  parameter (nstaz=20)
  
  real :: field(5),fact(5),const(5)
  
  ! "B22070" SIGNIFICANT WAVE HEIGHT [M, ##.##]
  ! "B22074" AVERAGE WAVE PERIOD [S, ##.#]
  ! "B22001" DIRECTION OF WAVES [DEGREE TRUE, 3 digits]
  ! "B22071" SPECTRAL PEAK WAVE PERIOD [S, ##.#]
  ! "B22042" SEA/WATER TEMPERATURE [K, ###.#]
  
  ! (1) hm0 = altezza media onda al posto di temp
  ! (2) tm =periodo medio
  ! (3) tp =periodo di picco
  ! (4) dir =direzione media di prop. dell'onda
  ! (5) tma =temp ogni tre ore boa mare
      
  INTEGER :: idata(3)
  CHARACTER(len=80) :: stringa
  CHARACTER(len=6) :: var(5)
! namelist variables
  CHARACTER(len=80) :: path=''
  CHARACTER(len=20) :: nomefile(nstaz)='',nome(nstaz)=''
  INTEGER :: versione(nstaz)=0,nboe=0
  REAL :: BLOCK(nstaz)=0.,station(nstaz)=0.
  REAL :: rlat(nstaz)=0.,rlon(nstaz)=0.,hstaz(nstaz)=0.
  CHARACTER(512) :: database='',user='',password=''

  namelist  /boe/path,nomefile,versione,nboe,nome,block,station,rlat,rlon,hstaz, &
       rwdata

  data rmd/-999./

  integer :: ier
  
  INTEGER :: handle,handle_ana
  logical :: init,rwdata
  integer :: debug=1
  integer :: handle_err
  data var/ "B22070", "B22074", "B22001", "B22071", "B22042"/
  data fact/ 1.,1.,1.,1.,1./
  data const/ 0.,0.,0.,0.,273.15/
  
  data init,rwdata/.false.,.false./
  
  namelist  /odbc/database,user,password

  PRINT*,'program leggidati_mare'
  
  open(1,file='odbc.nml',status='old')
  read(1,nml=odbc)
  close(1)
        
  open(1,file='boe.nml',status='old')
  read(1,nml=boe)
  close(1)

  print*,"nboe",nboe," nome ",nome," nomefile",nomefile

  ! PREPARAZIONE DELL' ARCHIVIO
  
  ier=idba_error_set_callback(0,C_FUNLOC(idba_default_error_handler),debug,handle_err)
  
  ier=idba_presentati(idbhandle,database)
  
  if (init)then
     ! solo se richiesta completa cancellazione iniziale
     ! o è la prima volta che si inseriscono i dati

     ier=idba_preparati(idbhandle,handle, &
          "write","write","write")
     ier=idba_scopa(handle,"repinfo.csv")
     ier=idba_fatto(handle)
     rwdata = .false.
  end if

  if (rwdata) then
     ier=idba_preparati(idbhandle,handle, &
          "write","write","write")
  else
     ier=idba_preparati(idbhandle,handle, &
          "write","add","add")
  endif

     ier=idba_preparati(idbhandle,handle_ana, &
          "write","write","write")

  ! INIZIO CICLO SUL NUMERO DI BOE
  do ns=1,nboe
     
     print*,'----------------------------'
     print*,'nome boa ',nome(ns)
     print*,' '
     
     ! print*,"setto i parametri"
     
     !anagrafica
     
     ier=idba_unsetall (handle)
     ier=idba_unsetall (handle_ana)
     
     ier=idba_setcontextana (handle_ana)

     ier=idba_set (handle_ana,"lat",rlat(ns))
     ier=idba_set (handle_ana,"lon",rlon(ns))
     ier=idba_set (handle_ana,"mobile",0)


     ier=idba_set (handle_ana,"name",nome(ns))
     ier=idba_set (handle_ana,"block",block(ns))
     ier=idba_set (handle_ana,"station",station(ns))
     ier=idba_set (handle_ana,"height",hstaz(ns))
     
     ier=idba_prendilo (handle_ana)
     ier=idba_enq (handle_ana,"ana_id",ana_id)
     

     ! dati (la temperatira non c'è)

     ier=idba_unsetall (handle)
     
     ier=idba_set (handle,"ana_id",ana_id)
     
     ier=idba_set (handle,"rep_memo","boe")
     ier=idba_setlevel (handle,1,0,0,0)
     ier=idba_settimerange (handle,254,0,0)
          
     print *,"apro file", path(1:istr_lunghezza(path))//"/" &
          //nomefile(ns)(1:istr_lunghezza(nomefile(ns)))
     open(1,file=path(1:istr_lunghezza(path))//"/" &
          //nomefile(ns)(1:istr_lunghezza(nomefile(ns))),&
          status='old',form='formatted')
     
743  read(1,'(a)',end=223)stringa

     field=rmd
     
     if (versione(ns) == 1 )then
        
        read(stringa,'(i4,1x,4(i2,1x))')idata(3),idata(2),idata(1),iora,imin
        !            print*,idata(3),idata(2),idata(1),iora,imin
        isec=0
        
        stringa = stringa(17:)
        
        read(stringa,*)field(:4)
        !            print*,field
        
        iconf=80
        
     else if  (versione(ns) == 2 )then
        
        read(stringa,'(5x,i4,1x,5(i2,1x))')idata(3),idata(2),idata(1),iora,imin
        !            print*,idata(3),idata(2),idata(1),iora,imin,isec
        
        stringa = stringa(24:)
        
        read(stringa,*)field(:4)
        !            print*,field
        
        iconf=50
        
     else
        
        print *, "Errore: versione non gestita ",ns,"-->",versione(ns) 
        call exit(1)
        
     end if

     ! conversione unità di misura 
     
     where (field .ne. rmd)
        field=field*fact
     end where
     
     ! INSERIMENTO DEI PARAMETRI NELL' ARCHIVIO
     
     ier=idba_setdate (handle,idata(3),idata(2),idata(1),iora,imin,isec)
     
     do i=1,5
        
        ! inserimento dati con cancellazione dati segnati mancanti
        if (field(i) == rmd .and. rwdata )then
           !                    print *,"cancello i dati",var(i),idata,iora
           ier=idba_set(handle,"var",var(i))
           ier=idba_dimenticami(handle)
        else if (field(i) /= rmd ) then
           ier=idba_set(handle,var(i),field(i))
           ier=idba_prendilo (handle)
           ier=idba_set(handle,"*B33007",iconf)
           ier=idba_critica(handle)
           ier=idba_unset (handle,"*B33007")
           ier=idba_unset (handle,var(i))
           
        end if
        
     end do
     
     goto 743
     
223  continue
     close(1)
     
  enddo                     !boa
  
  ier=idba_fatto(handle)
  ier=idba_fatto(handle_ana)
  ier=idba_arrivederci(idbhandle)
  
  stop
  
end program leggidati_mare

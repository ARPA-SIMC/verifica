PROGRAM calcola_td

! c   VERIFICA - calcola_td.f90
! c   programma per calcolare la td a partire da t ed rh
! c   legge t ed rh caricate su database e qui scrive la td calcolata
! c   Autore: Chiara Marsigli e Maria Stefania Tesini
! pgf90 -Mbounds -o ver_calcola_td ver_calcola_td.f90 
! /home/marsigli/svn_old/verifica/trunk/src/util_dballe.f90 
! -lhibu -lmega -lgrib_util -lemos -ldballef -lcnf -lodbc -lpopt -lm

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

!  INCLUDE "dballe/dballef.h"

USE util_dballe

PARAMETER (MNSTAZ=10000)
REAL :: rlon,rlat,h
INTEGER :: giornoi,mesei,annoi,orai,mini,giornof,mesef,annof,oraf,minf
INTEGER :: iminuti,iminmax
INTEGER :: interval=60
INTEGER :: DATA(3),ora(2)
INTEGER :: dataval(3),oraval(2)
CHARACTER(len=6) :: cvart,cvarrh,cvartd
CHARACTER btable*10, rep_memo*20
REAL :: t,rh,td
REAL :: rmdo

INTEGER, ALLOCATABLE :: anaid(:)

CHARACTER(19) :: database,user,password
INTEGER :: handle,handler
INTEGER :: debug = 1
INTEGER :: handle_err

integer :: ier

NAMELIST  /odbc/database,user,password

DATA rmdo/-999.9/
DATA cvart/'B12101'/,cvarrh/'B13003'/,cvartd/'B12103'/

OPEN(1,file='odbc.nml',status='old')
READ(1,nml=odbc)
CLOSE(1)

PRINT*,'Scrivi inizio periodo (dd mm aaaa)'
READ(*,*)giornoi,mesei,annoi
PRINT*,'Scrivi fine periodo (dd mm aaaa)'
READ(*,*)giornof,mesef,annof

orai=00
mini=00
  
DATA(3)=annoi
DATA(2)=mesei
DATA(1)=giornoi
ora(1)=orai
ora(2)=mini
  
oraf=23
minf=00
  
CALL JELADATA5(giornoi,mesei,annoi,orai,mini,iminuti)
CALL JELADATA5(giornof,mesef,annof,oraf,minf,iminmax)
PRINT*,'dal ',giornoi,mesei,annoi,orai,mini
PRINT*,'al ',giornof,mesef,annof,oraf,minf
PRINT*,'intervallo in minuti ',interval
  
! PREPARAZIONE DELL' ARCHIVIO

! gestione degli errori
ier=idba_error_set_callback(0,C_FUNLOC(idba_default_error_handler),debug,handle_err)

! connessione con database
PRINT*,"database=",database
ier=idba_presentati(idbhandle,database)

! apertura database in lettura
ier=idba_preparati(idbhandle,handler,"read","read","read")
! apertura database in scrittura
ier=idba_preparati(idbhandle,handle,"write","write","write")

ier=idba_quantesono(handler,nstaz)
PRINT*,'massimo numero pseudo-stazioni ',nstaz
! allocazione matrici
ALLOCATE(anaid(1:nstaz))
CALL leggiana_db_all(anaid,nstaz,handler)
 
STAZIONI: DO ist=1,nstaz
  
  ier=idba_unsetall (handler)
  
  ier=idba_set (handler,"ana_id",anaid(ist))
  
  dataval(3)=annoi
  dataval(2)=mesei
  dataval(1)=giornoi
  oraval(1)=orai
  oraval(2)=mini
  CALL JELADATA5(DATA(1),DATA(2),DATA(3),ora(1),ora(2), &
   iminuti)
! INIZIO CICLO SUI GIORNI
  MINUTI: DO WHILE (iminuti <= iminmax)
        
    ier=idba_set (handler,"year",dataval(3))
    ier=idba_set (handler,"month",dataval(2))
    ier=idba_set (handler,"day",dataval(1))
    ier=idba_set (handler,"hour",oraval(1))
    ier=idba_set (handler,"min",oraval(2))
    ier=idba_set (handler,"sec",00)
    
    ier=idba_setlevel(handle,103,2000,0,0)
    ier=idba_settimerange(handle,254,0,0)
        
! estrazione t
    ier=idba_set (handler,"var",cvart)
    ier=idba_voglioquesto (handler,N)          
    IF(N == 0)THEN
!      PRINT*,'oss t - non ci sono dati ',dataval(3),dataval(2),dataval(1),oraval(1),oraval(2)
      t=rmdo
    ELSE
      nctrl=0
      DATIT: DO idati=1,N
        ier=idba_dammelo (handler,btable)
        ier=idba_enq (handler,"lat",rlat)
        ier=idba_enq (handler,"lon",rlon)
        ier=idba_enq (handler,"height",h)
        ier=idba_enq (handler,"ana_id",icodice)
        ier=idba_enq (handler,"rep_memo",rep_memo)
        nctrl=nctrl+1
        IF(nctrl > 1)THEN
          PRINT*,'ho trovato ',nctrl,' dati!!!'
          CALL EXIT (1)
        ENDIF
        ier=idba_enq (handler,btable,dato)
        t=dato
      ENDDO DATIT
    ENDIF
    
! estrazione rh
    ier=idba_set (handler,"var",cvarrh)
    ier=idba_voglioquesto (handler,N)
    IF(N == 0)THEN
!      PRINT*,'oss rh - non ci sono dati ',dataval(3),dataval(2),dataval(1),oraval(1),oraval(2)
      rh=rmdo
    ELSE
      nctrl=0
      DATIRH: DO idati=1,N
        ier=idba_dammelo (handler,btable)
        ier=idba_enq (handler,"lat",rlat)
        ier=idba_enq (handler,"lon",rlon)
        ier=idba_enq (handler,"height",h)
        ier=idba_enq (handler,"ana_id",icodice)
        ier=idba_enq (handler,"rep_memo",rep_memo)
        nctrl=nctrl+1
        IF(nctrl > 1)THEN
          PRINT*,'ho trovato ',nctrl,' dati!!!'
          CALL EXIT (1)
        ENDIF
        ier=idba_enq (handler,btable,dato)
        rh=dato
      ENDDO DATIRH
    ENDIF

    ! calcolo td da t e rh (termolib)
    IF(t /= rmdo .AND. rh /= rmdo .AND. rh /= 0)THEN
      td=trug(rh,t)
    ELSE
      td=rmdo
    ENDIF
    
    ! INSERIMENTO DEI PARAMETRI NELL' ARCHIVIO
    ier=idba_unsetall (handle)
    
    ier=idba_set (handle,"year",dataval(3))
    ier=idba_set (handle,"month",dataval(2))
    ier=idba_set (handle,"day",dataval(1))
    ier=idba_set (handle,"hour",oraval(1))
    ier=idba_set (handle,"min",oraval(2))
    ier=idba_set (handle,"sec",00)
    
    ier=idba_set (handle,"rep_memo",rep_memo)
    
    ier=idba_set (handle,"ana_id",icodice)
    
    ier=idba_set (handle,"mobile",0)
    
    ier=idba_setlevel(handle,103,2000,0,0)
    ier=idba_settimerange(handle,254,0,0)
        
    IF (td /= rmdo) THEN
      PRINT*,'scrivo td ',ist,dataval(3),dataval(2),dataval(1),oraval(1),oraval(2)
      ier=idba_set(handle,"B12103",td)
      ier=idba_prendilo (handle)
      ier=idba_unset (handle,"B12103")
    ENDIF
    
    ! trovo nuova data e ora di validita' del dato
    iminuti=iminuti+interval
    CALL JELADATA6(iday,imonth,iyear,ihour,imin,iminuti)
    dataval(1)=iday
    dataval(2)=imonth
    dataval(3)=iyear
    oraval(1)=ihour
    oraval(2)=imin
  ENDDO MINUTI
  
ENDDO STAZIONI

ier=idba_fatto(handler)
ier=idba_fatto(handle)
ier=idba_arrivederci(idbhandle)

STOP
END PROGRAM calcola_td

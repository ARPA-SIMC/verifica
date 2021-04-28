!*****************************************************************************
! c VERIFICA - util_dballe.f90
! c subroutines di utilita' per i programmi di verifica
! c autore: Chiara Marsigli

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
!*****************************************************************************

  MODULE util_dballe

    USE grib_api
    USE missing_values
    INCLUDE "dballe/dballeff.h"

    type anaid_type
       integer :: lat
       integer :: lon
    endtype anaid_type
    
    CONTAINS

!*****************************************************************************

    subroutine leggiana_db(iana,x,y,alt,rmdo,nstaz,handle)

! c VERIFICA - util_dballe.f90
! c legge l'anagrafica stazioni dal database
! c itipostaz e' codificato come il WMO block number:
! c <70 (valore vero) per stazioni vere (prima era itipostaz=0)
! c =80 (valore fittizio) per punti medi di box (prima era itipostaz=1)
! c =90 (valore fittizio) per punti di griglia su cui e' definita l'analisi (prima era itipostaz=2)
! c autore: Chiara Marsigli

    integer :: handle,nstaz,iana
    real :: h
    character(20) :: namest

    integer :: ier

    real :: x(nstaz),y(nstaz),alt(nstaz)

! inizializzazione matrici
    x = rmdo
    y = rmdo
    alt = rmdo

    print*,'stazioni ',nstaz
    do ist=1,nstaz
       
       ier=idba_elencamele(handle)
       
       ier=idba_enq (handle,"ana_id",icodice)
       ier=idba_enq (handle,"lat",rlat)
       ier=idba_enq (handle,"lon",rlon)
       ier=idba_enq (handle,"height",h)
       ier=idba_enq (handle,"name",namest)
       
       if(.not.c_e_c(namest))namest=''
       if(.not. c_e(h))h=9999.
       
       IF (nstaz.LT.icodice) THEN
          PRINT*,"ATTENTION !!!!"
          PRINT*,"FATAL ERROR"
          PRINT*,"sono state cancellate alcune stazioni"
          PRINT*,"opzione non prevista: ",nstaz,"<",icodice
          CALL EXIT(1)
       END IF

       if(iana == 0)then
          ! voglio solo le stazioni vere (nome stazione non inizia per '_')
          if(namest(1:1) /= '_')then
             print*,ist,icodice,rlat,rlon,h,namest
             x(icodice)=rlon
             y(icodice)=rlat
             if(h == 9999.)then
                alt(icodice)=rmdo
             else
                alt(icodice)=h
             endif
          endif
       elseif(iana == 1)then
          ! voglio solo le analisi (nome inizia per '_ana')
          if(namest(1:3) == '_gp')then
!             write(33,*)ist,icodice,rlat,rlon,h,namest
             x(icodice)=rlon
             y(icodice)=rlat
             if(h == 9999.)then
                alt(icodice)=rmdo
             else
                alt(icodice)=h
             endif
          endif
       endif
    enddo

    return
    end subroutine leggiana_db

!*****************************************************************************

    subroutine leggiana_db_scores(iana,anaid, &
    itipost,rmdo,nstaz,handle,lselect)

! c VERIFICA - util_dballe.f90
! c legge l'anagrafica stazioni dal database
! c itipostaz e' codificato come il WMO block number:
! c <70 (valore vero) per stazioni vere (prima era itipostaz=0)
! c =80 (valore fittizio) per punti medi di box (prima era itipostaz=1)
! c =90 (valore fittizio) per punti di griglia su cui e' definita l'analisi
! c (prima era itipostaz=2)
! c autore: Chiara Marsigli

    integer :: handle,iana,nstaz,itipostaz

    type(anaid_type) :: anaid(nstaz)

    LOGICAL :: lselect

    integer :: ier

    integer :: ilat,ilon
    
    REAL :: x(nstaz),y(nstaz),alt(nstaz),toll
    REAL :: rlat,rlon
    DOUBLE PRECISION :: dlat,dlon

    character(9) :: ident
    character(50) :: repmemo
    
    DATA toll/0.0001/

! inizializzazione matrici
    x = rmdo
    y = rmdo
    alt = rmdo

    IF(lselect)THEN
       nread=0
       OPEN(1,file='selstaz.dat',status='old')
       DO ist=1,nstaz
          READ(1,*,END=200)rlon,rlat
          nread=nread+1
          x(nread)=rlon
          y(nread)=rlat
       ENDDO
200    CONTINUE
       CLOSE(1)
       PRINT*,'lette ',nread,' stazioni'
    ENDIF
    
    i=0
    DO ist=1,nstaz
       
       ier=idba_elencamele(handle)
       
 !      ier=idba_enq (handle,"ana_id",icodice)
       ier=idba_enq (handle,"block",itipostaz)
       ier=idba_enq (handle,"lat",dlat)
       ier=idba_enq (handle,"lon",dlon)
       ier=idba_enq (handle,"lat",ilat)
       ier=idba_enq (handle,"lon",ilon)
       ier=idba_enq (handle,"ident",ident)
       ier=idba_enq (handle,"rep_memo",repmemo)
       
       IF(.NOT.c_e_i(itipostaz))itipostaz=itipost
!       print*,'itipostaz',itipostaz
       
       IF(lselect)THEN
          
          DO jst=1,nread
             IF(ABS(dlon-x(jst))<toll .AND. ABS(dlat-y(jst))<toll)THEN
                i=i+1
                PRINT*,ist,dlon,dlat,repmemo,itipostaz
                anaid(i)%lat = ilat
                anaid(i)%lon = ilon
             endif
          ENDDO
          
       ELSE
          
          IF(iana == 0)THEN
             IF(itipost == 0)THEN
                ! voglio solo le stazioni vere (itipostaz<70)
                IF(itipostaz < 70)THEN
                   i=i+1
                   !              PRINT*,ist,icodice,itipostaz
                   anaid(i)%lat = ilat
                   anaid(i)%lon = ilon
                ENDIF
             ELSEIF(itipost == 80)THEN
                ! voglio solo le box (itipost=80)
                IF(itipostaz == 80)THEN
                   i=i+1
!                   PRINT*,ist,itipostaz
                   anaid(i)%lat = ilat
                   anaid(i)%lon = ilon
                ENDIF
             ENDIF
          ELSEIF(iana == 1)THEN
             ! voglio solo le analisi (itipostaz=200)
             IF(itipostaz == 90)THEN
                i=i+1
!                WRITE(33,*)ist,icodice,itipostaz
                anaid(i)%lat = ilat
                anaid(i)%lon = ilon
             ENDIF
          ELSE
             PRINT*,"ERRORE"
             PRINT*,"iana, opzione non gestita"
             
          ENDIF
          
       ENDIF
       
    ENDDO
    
    nstaz=i

    return
    end subroutine leggiana_db_scores

!*****************************************************************************

    subroutine leggi_selstaz(anaid,nstaz,handle)

! VERIFICA - util_dballe.f90
! legge l'anagrafica stazioni dal database
! solo le stazioni scelte dall'utente e specificate nel file di appoggio 
! autore: Chiara Marsigli

    integer :: handle,nstaz

    type(anaid_type) :: anaid(nstaz)

    integer :: ilon,ilat
    
    integer :: ier

    OPEN(1,file='selstaz.dat',status='old')

    i=0
    do ist=1,nstaz
       
!      READ(1,*,end=100)rlon,rlat
      READ(1,*,end=100)ilon,ilat
      
!      ier=idba_set (handle,"lat",rlat)
!      ier=idba_set (handle,"lon",rlon)

! esiste una API che permette di ottenere ana_id da lon e lat??????

!      ier=idba_enq (handle,"ana_id",icodice)

      i=i+1
      anaid(i)%lat = ilat
      anaid(i)%lon = ilon

    enddo
    
100 continue
    nstaz=i

    close(1)

    return
    end subroutine leggi_selstaz


!*****************************************************************************

    SUBROUTINE leggiana_db_all(anaid,nstaz,handle)

! c VERIFICA - util_dballe.f90
! c legge l'anagrafica stazioni dal database
! c autore: Chiara Marsigli

      integer :: handle,nstaz
      integer :: ilat,ilon

      type(anaid_type) :: anaid(nstaz)
      
      integer :: ier
      
      character(20) :: namest

      print*,'stazioni ',nstaz
      DO ist=1,nstaz

         ier=idba_elencamele(handle)

!        ier=idba_enq (handle,"ana_id",icodice)
         ier=idba_enq (handle,"lat",ilat)
         ier=idba_enq (handle,"lon",ilon)
         ier=idba_enq (handle,"name",namest)
         
         PRINT*,ist,ilat,ilon,namest
         anaid(i)%lat = ilat
         anaid(i)%lon = ilon
         
      enddo
      
      return
    end subroutine leggiana_db_all

!**************************************************************************

    INTEGER FUNCTION nlenvera(stringa)

! mstart
! VERIFICA - util_dballe.f90
! function intera per calcolare la lunghezza effettiva (senza bianchi)
! di una stringa carattere
! appunto: esistono la funzione implicita trim per isolare la stringa
! senza bianchi e la funzione implicita len_trim per avere la lunghezza 
! della stringa bianchi esclusi!!!
! autore: Chiara Marsigli
! stringa  character*80
! mend

    CHARACTER(*) :: stringa

    nbianco=index(stringa,' ')
    if(nbianco == 0)then
        nlenvera=len(stringa)
    else
        nlenvera=nbianco-1
    endif

    return
    end function nlenvera

!**************************************************************************

    INTEGER FUNCTION istr_lunghezza (stringa )

    character stringa*(*)

    istr_lunghezza= len (stringa)

    do while (( stringa(istr_lunghezza:istr_lunghezza).eq." " .or. &
        stringa(istr_lunghezza:istr_lunghezza).eq."").and. &
        istr_lunghezza.gt.0)
        istr_lunghezza=istr_lunghezza - 1

    enddo
    return
    end function istr_lunghezza

!**************************************************************************

    subroutine converti_scadenze(ns,scad,scaddb)

    integer :: scad(ns),scaddb(ns)

    integer :: fact(0:12)
    data fact/60,3600,86400,2592000,31536000, &
    315360000,946080000,0, &
    0,0,10800,21600,43200/

! se scad(1)=0 il valore e' in minuti, quindi
! il fattore moltiplicativo per trasformarlo in secondi e' 60;
! se scad(1)=1 il valore e' in ore, quindi il fattore e' 3600 ...

    if(scad(1) >= 0 .AND. scad(1) <= 12)then
        ifac=fact(scad(1))
    elseif(scad(1) == 254)then
        ifac=1
    else
        ifac=0
    endif

    if(ifac == 0)then
        print*,'ERRORE! SCAD(1) NON GESTITO (RISERVATO)'
        call exit(1)
    endif

    scaddb(1)=254             ! non usato
    scaddb(2)=scad(2)*ifac
    scaddb(3)=scad(3)*ifac
    scaddb(4)=scad(4)

    return
    end subroutine converti_scadenze

!*****************************************************************************

    INTEGER FUNCTION ngiorni_mese(nm,na)

    INTEGER :: giomax(12)
    
    DATA giomax/31,28,31,30,31,30,31,31,30,31,30,31/

    ngiorni_mese=giomax(nm)

    IF(nm == 2)THEN
      IF(MOD(na,4) == 0)THEN
        IF(MOD(na,100) /= 0)THEN
          ngiorni_mese=29
        ENDIF
      ENDIF
      IF(MOD(na,400) == 0)THEN
        ngiorni_mese=29
      ENDIF
    ENDIF

    RETURN
    END FUNCTION ngiorni_mese

!************************************************************************

    LOGICAL FUNCTION c_e_i(var)

!       Verifica la condizione di presenza o assenza del dato secondo
!       le specifiche dballe restituendo una variabile logical .true.
!       se c'e` il dato (ossia esso e` diverso da 32767)
!
!       INPUT:
!       VAR     Integer dato di cui verificare la presenza
!       OUTPUT:
!       C_E_i   LOGICAL .TRUE.se il dato e` presente


    integer :: var

    c_e_i=.TRUE.
    IF (var == dba_mvi )c_e_i= .FALSE. 
    RETURN
    END FUNCTION c_e_i

!************************************************************************

    LOGICAL FUNCTION c_e_c(var)

!       Verifica la condizione di presenza o assenza del dato secondo
!       le specifiche meteodata restituendo una variabile logical .true.
!       se c'e` il dato (ossia esso e` diverso da stringa nulla)

!       INPUT:
!       VAR     CHAR*(*)        dato di cui verificare la presenza
!       OUTPUT:
!       C_E_c   LOGICAL         .TRUE.se il dato e` presente


    CHARACTER(len=*) :: var
    
    c_e_c=.FALSE.
    IF (var /= dba_mvc )c_e_c=.TRUE.
    RETURN
    END FUNCTION c_e_c

!*****************************************************************************

  END MODULE util_dballe

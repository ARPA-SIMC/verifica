!*****************************************************************************
! c VERIFICA - util_dballe.f
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

    subroutine leggiana_db(iana,x,y,alt,rmdo,nstaz,handle)

! c VERIFICA - util.f
! c legge l'anagrafica stazioni dal database
! c itipostaz e' codificato come il WMO block number:
! c <70 (valore vero) per stazioni vere (prima era itipostaz=0)
! c =80 (valore fittizio) per punti medi di box (prima era itipostaz=1)
! c =90 (valore fittizio) per punti di griglia su cui e' definita l'analisi (prima era itipostaz=2)
! c autore: Chiara Marsigli

    integer :: handle,nstaz,iana
    character(20) :: namest

    real :: x(nstaz),y(nstaz),alt(nstaz)

! nizializzazione matrici
    x = rmdo
    y = rmdo
    alt = rmdo

    print*,'stazioni ',nstaz
    do ist=1,nstaz

        call idba_elencamele(handle)

        call idba_enqi (handle,"ana_id",icodice)
        call idba_enqr (handle,"lat",rlat)
        call idba_enqr (handle,"lon",rlon)
        call idba_enqr (handle,"height",h)
        call idba_enqc (handle,"name",namest)

        if(iana == 0)then
        ! voglio solo le stazioni vere (nome stazione non inizia per '_')
            if(namest(1:1) /= '_')then
                print*,ist,icodice,rlat,rlon,h,namest
                x(icodice)=rlon
                y(icodice)=rlat
                alt(icodice)=h

            endif
        elseif(iana == 1)then
        ! voglio solo le analisi (nome inizia per '_ana')
            if(namest(1:3) == '_gp')then
                write(33,*)ist,icodice,rlat,rlon,h,namest
                x(icodice)=rlon
                y(icodice)=rlat
                alt(icodice)=h

            endif
        endif
    enddo

    return
    end subroutine leggiana_db

!*****************************************************************************

    subroutine leggiana_db_scores(iana,x,y,alt,anaid, &
    itipost,rmdo,nstaz,handle)

! c VERIFICA - util.f
! c legge l'anagrafica stazioni dal database
! c itipostaz e' codificato come il WMO block number:
! c <70 (valore vero) per stazioni vere (prima era itipostaz=0)
! c =80 (valore fittizio) per punti medi di box (prima era itipostaz=1)
! c =90 (valore fittizio) per punti di griglia su cui e' definita l'analisi
! c (prima era itipostaz=2)
! c autore: Chiara Marsigli

    integer :: handle,iana,nstaz

    real :: x(nstaz),y(nstaz),alt(nstaz)
    integer :: anaid(nstaz)

! nizializzazione matrici
    x = rmdo
    y = rmdo
    alt = rmdo

    i=0
    do ist=1,nstaz
        call idba_elencamele(handle)

        call idba_enqi (handle,"ana_id",icodice)
        call idba_enqr (handle,"lat",rlat)
        call idba_enqr (handle,"lon",rlon)
        call idba_enqr (handle,"height",h)
        call idba_enqi (handle,"block",itipostaz)

        if(iana == 0)then
            if(itipost == 0)then
            ! voglio solo le stazioni vere (itipostaz<70)
                if(itipostaz < 70)then
                    i=i+1
                    print*,ist,icodice,rlat,rlon,h,itipostaz
                    x(i)=rlon
                    y(i)=rlat
                    alt(i)=h
                    anaid(i)=icodice
                endif
            elseif(itipost == 80)then
            ! voglio solo le box (itipost=80)
                if(itipostaz == 80)then
                    i=i+1
                    print*,ist,icodice,rlat,rlon,h,itipostaz
                    x(i)=rlon
                    y(i)=rlat
                    alt(i)=h
                    anaid(i)=icodice
                endif
            endif
        elseif(iana == 1)then
        ! voglio solo le analisi (itipostaz=200)
            if(itipostaz == 90)then
                i=i+1
                write(33,*)ist,icodice,rlat,rlon,h,itipostaz
                x(i)=rlon
                y(i)=rlat
                alt(i)=h
                anaid(i)=icodice
            endif
        endif
    enddo

    nstaz=i

    return
    end subroutine leggiana_db_scores


!*****************************************************************************

    SUBROUTINE leggiana_db_all(x,y,alt,anaid,rmdo,nstaz,handle)

! c VERIFICA - util.f
! c legge l'anagrafica stazioni dal database
! c autore: Chiara Marsigli

    integer :: handle,nstaz
    character(20) :: namest

    real :: x(nstaz),y(nstaz),alt(nstaz)
    integer :: anaid(nstaz)

! nizializzazione matrici
    x = rmdo
    y = rmdo
    alt = rmdo
    anaid = 0

    print*,'stazioni ',nstaz
    DO ist=1,nstaz

        call idba_elencamele(handle)

        call idba_enqi (handle,"ana_id",icodice)
        call idba_enqr (handle,"lat",rlat)
        call idba_enqr (handle,"lon",rlon)
        call idba_enqr (handle,"height",h)
        call idba_enqc (handle,"name",namest)

        PRINT*,ist,icodice,rlat,rlon,h,namest
        x(ist)=rlon
        y(ist)=rlat
        alt(ist)=h
        anaid(ist)=icodice
        
    enddo

    return
    end subroutine leggiana_db_all

!*****************************************************************************

    subroutine leggioss_db(handle,nd,no, &
    dataval,oraval,cvar,scad, &
    rxeq,ryeq,ruota,rmddb, &
    MNSTAZ,x,y,alt,nstdispo,obs)

! c VERIFICA - util.f
! c legge le osservazioni dal database
! c autore: Chiara Marsigli

    real :: x(MNSTAZ),y(MNSTAZ),alt(MNSTAZ),obs(MNSTAZ)
    logical :: ruota
! dichiarazioni database
    integer :: dataval(nd),oraval(no),level(3),scad(nd),scaddb(4)
    integer :: icodice,handle,repcod,p1,p2
    real :: rlat,rlon,h
    character descr*20,cvar*6,btable*10
! Equatore della rotazione
    real :: rxeq,ryeq

    print*,'util.f - leggioss_db'

    cryeq=cos(ryeq*3.1415927/180.)
    sryeq=sin(ryeq*3.1415927/180.)

    rmdo=-999.9
    do i=1,MNSTAZ
        x(i)=rmdo
        y(i)=rmdo
        alt(i)=rmdo
        obs(i)=rmddb
    enddo

    descr="oss"

    print*,'oss ',dataval,oraval,cvar

! Lettura osservazioni da database

    call idba_unsetall(handle)

    call idba_seti (handle,"priomin",0)
    call idba_unset (handle,"priomax")
    call idba_setc (handle,"query","best")

    call idba_seti (handle,"year",dataval(3))
    call idba_seti (handle,"month",dataval(2))
    call idba_seti (handle,"day",dataval(1))
    call idba_seti (handle,"hour",oraval(1))
    call idba_seti (handle,"min",oraval(2))
    call idba_seti (handle,"sec",0)

! conversione delle scadenze in secondi (e correzione scadenze sbagliate)
    call converti_scadenze(4,scad,scaddb)
    if(scaddb(4) > 0)then
        p1=0-(scaddb(3)-scaddb(2))
        p2=0
    else
        p1=0
        p2=0
    endif
    call idba_seti (handle,"p1",p1)
    call idba_seti (handle,"p2",p2)
    call idba_seti (handle,"pindicator",scad(4))

    call idba_setc (handle,"var",cvar)

    call idba_voglioquesto (handle,N)
    if(N == 0)then
        print*,'non ci sono dati'
        goto 66
    else
        print*,'numero dati ',N
    endif

    nv=0
    do idati=1,N

        call idba_dammelo (handle,btable)
    ! sara' da impostare mentre per ora e' solo richiesto
        call idba_enqi (handle,"leveltype", &
        level(1))
        call idba_enqi (handle,"l1",level(2))
        call idba_enqi (handle,"l2",level(3))

        call idba_enqr (handle,"lat",rlat)
        call idba_enqr (handle,"lon",rlon)
        call idba_enqr (handle,"height",h)

        call idba_enqi (handle,"rep_cod",repcod)
        call idba_enqi (handle,"ana_id",icodice)

        if(repcod >= 100)goto30

        nv=nv+1
        call idba_enqr (handle,btable,dato)
        obs(nv)=dato
    ! print*,'ecco ',ipos,iv,dato
        if(ruota)then
            call tlld(rlon,rlat,rxeq,cryeq,sryeq,trlon,trlat)
            rlon=trlon
            rlat=trlat
        endif
        x(icodice)=rlon
        y(icodice)=rlat
        alt(icodice)=h

        30 continue

    enddo                     ! idati
    nstdispo=nv

    66 continue

    return
    end subroutine leggioss_db

!*****************************************************************************

    subroutine cleankey(n2,n3,n4,level,var,est,scad,data,ora)
    integer :: level(n3),var(n3),est(n3),scad(n4),data(n3),ora(n2)

! c VERIFICA - util.f
! c azzera le chiavi di ricerca grib
! c autore: Chiara Marsigli

    level(1)=-1
    level(2)=-1
    level(3)=-1
    var(1)=-1
    var(2)=-1
    var(3)=-1
    est(1)=-1
    est(2)=-1
    est(3)=-1
    scad(1)=-1
    scad(2)=-1
    scad(3)=-1
    scad(4)=-1
    data(1)=-1
    data(2)=-1
    data(3)=-1
    ora(1)=-1
    ora(2)=-1

    return
    end subroutine cleankey

!****************************************************************************

    subroutine descrittore(model,itipo,imod,ls,media,massimo,prob, &
    distr,dxb,dyb,descr)

! c VERIFICA - util.f
! c qui viene prodotto il descrittore da utilizzare per caratterizzare
! c il dato all'interno del database
! c autore: Chiara Marsigli

    integer ::   imod,ls,itipo
    logical ::   media,massimo,prob,distr
    real ::      dxb,dyb
    character model*10,descr*20
    character d2*5,c2*2,c3*3

    if(itipo == 1)then
        c2='sp'
        if(imod == 0)then
            c3='npo'
        elseif(imod == 1 .AND. ls < 0)then
            c3='bil'
        elseif(imod == 1 .AND. ls >= 0)then
            c3='pes'
        endif
        d2=c2//c3
    elseif(itipo == 2)then
        if(media == .TRUE. .AND. massimo == .FALSE. &
         .AND. prob == .FALSE. .AND. distr == .FALSE. )then
            c3='med'
        elseif(massimo == .TRUE. .AND. media == .FALSE. &
             .AND. prob == .FALSE. .AND. distr == .FALSE. )then
            c3='max'
        elseif(prob == .TRUE. .AND. distr == .FALSE. .AND. &
            massimo == .FALSE. .AND. media == .FALSE. )then
            c3='prb'
        elseif(distr == .TRUE. .AND. prob == .FALSE. .AND. &
            massimo == .FALSE. .AND. media == .FALSE. )then
            c3='dst'
        endif
        write(c2,'(i2.2)')int(dxb*10.)
        d2=c3//c2
    else
        print*,'tipo di elaborazione non gestita ',itipo
        stop
    endif

    descr=model(1:nlenvera(model))//d2

    return
    end subroutine descrittore

!**************************************************************************

    SUBROUTINE modello(model,ivlsm,ivor,ls,diffh)

    INTEGER :: ivlsm,ivor,ls
    character model*10,profile*20
    character cdum*10,civor*3,civlsm*3
    logical diffh

! c VERIFICA - util.f
! c assegna i valori dei parametri orografia e lsm
! c autore: Chiara Marsigli

    profile='profile_'//model(1:nlenvera(model))
    IF(diffh)THEN
      OPEN(44,file=profile,status='old')
      DO WHILE (.TRUE.)
        READ(44,'(a10)',END=222)cdum
        IF(cdum(4:5) == 'or')THEN
          civor=cdum(7:9)
          READ(civor,'(i3.3)')ivor
          CLOSE(44)
          goto111
        ENDIF
      ENDDO
    ENDIF
111 CONTINUE
    IF(ls >= 0)THEN
      OPEN(44,file=profile,status='old')
      DO WHILE (.TRUE.)
        READ(44,'(a10)',END=333)cdum
        IF(cdum(4:6) == 'lsm')THEN
          civlsm=cdum(8:10)
          READ(civlsm,'(i3.3)')ivlsm
          CLOSE(44)
          goto444
        ENDIF
      ENDDO
    ENDIF
444 CONTINUE

    return
    222 print*,'manca ivor in profile_nomemodello!'
    call exit(1)
    return
    333 print*,'manca ivlsm in profile_nomemodello!'
    call exit(1)
    return
    end subroutine modello

!**************************************************************************

    subroutine variabile(n3,var,cvar,a,b,lgrib)

! c VERIFICA - util.f
! c specifica le caratteristiche che ha nel database la variabile in questione:
! c valore in Blocale e rappresentazione
! c autore: Chiara Marsigli

    character cvar*6,cvarl*6,mnem*10
    integer :: var(n3)
    real :: a,b
    logical :: lgrib

    open(1,file='griBlocale.txt',status='old')

    if(lgrib)then
        do while(.true.)
            read(1,*,end=222)iv1,iv2,iv3,cvar,a,b
            if(var(1) == iv1 .AND. var(2) == iv2 .AND. var(3) == iv3) &
            goto111
        enddo
    else
        do while(.true.)
            read(1,*,end=222)iv1,iv2,iv3,cvarl,a,b,mnem
            if(cvarl == cvar) &
            goto111
        enddo
    endif

    222 print*,'variabile non presente in griBlocale.txt',var,cvarl
    call exit (2)


    111 print*,'trovata! Blocale= ',cvar,a,b
    close(1)

    return
    end subroutine variabile

!********************************************************************************

! MSTART ESAT   viene dalla termolib.for

! function ESAT (T)

! Calcola la pressione di vapor saturo alla temperatura (t)
! secondo la formula:

! ESAT = 6.1078*EXP((17.2693882*TC)/(TC+237.3))

! Dove :  TC e` la Temperatura  espressa in gradi Celsius

! Uso :
! x = ESAT (T)

! Input :
! T   R*4  Temperatura                         (K.)

! Output :
! ESAT      R*4   Pressione di vapor saturo                      (mb.)
! ESAT =  -999.9  Se non e' possibile calcolarla.
! MEND
    FUNCTION ESAT(T)
    DATA ABZ/273.16/

    IF(T < 0)THEN
        ESAT=-999.9
        RETURN
    END IF

    TC=T-ABZ
    ESAT=6.1078*EXP((17.2693882*TC)/(TC+237.3))
    RETURN
    end FUNCTION ESAT

! MSTART TRUG  viene dalla termolib.for

! function TRUG (UMID,T)

! Calcola la temperatura di rugiada secondo la stessa formula
! usata per il calcolo dell'umidita` relativa, risolta per
! la temperatura di rugiada.

! Uso :
! x = TRUG (UMID,T)

! Input :
! UMID   R*4  Umidita` relativa                               (%)
! T      R*4  Temperatura dell'aria                           (K.)

! Output :
! TRUG       R*4  Temperatura di ruguada                           (K.)
! TRUG =  -999.9  Se non e' possibile calcolarla
! MEND
    FUNCTION TRUG(UMID,T)

    PARAMETER ABZ=273.16,D=237.3,C=17.2693882,B=6.1078

    IF(UMID < 0)GO TO 9999
    IF(T < 0)GO TO 9999
    ES=ESAT(T)                ! >>> Calcolo la P.v.s
    E=UMID/100.*ES            ! >>> Calcolo la P.v.
    TRUG=(D*LOG(E/B))/(C-LOG(E/B)) ! >>> Calcolo la T.d.
    TRUG=TRUG+ABZ             ! >>> Calcolo la T.d. in Kelvin
    RETURN
    9999 CONTINUE
    TRUG=-999.9
    RETURN
    end FUNCTION TRUG

!************************************************************************

    function nlenvera(stringa)

! mstart
! VERIFICA - util.f
! function intera per calcolare la lunghezza effettiva (senza bianchi)
! di una stringa carattere
! autore: Chiara Marsigli
! stringa  character*80
! mend

    character(80) :: stringa

    nbianco=index(stringa,' ')
    if(nbianco == 0)then
        nlenvera=len(stringa)
    else
        nlenvera=nbianco-1
    endif

    return
    end function nlenvera

!**************************************************************************

    subroutine error_handle (debug)

    logical :: debug
    integer :: idba_error_code
    CHARACTER messaggio*1000
    ier=idba_error_code ()
    if (ier /= 0)then
        if (debug)then
            print *,ier
            call idba_error_message(messaggio)
            print *,messaggio(:istr_lunghezza(messaggio))
            call idba_error_context(messaggio)
            print *,messaggio(:istr_lunghezza(messaggio))
            call idba_error_details(messaggio)
            print *,messaggio(:istr_lunghezza(messaggio))
        end if

        call exit (1)

    end if

    return
    end subroutine error_handle

!**************************************************************************

    integer function istr_lunghezza (stringa )

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
    315360000,946080000,3153600000, &
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

    subroutine leggibox(vfile,MNBOX,xb,yb,nbox,alorot,alarot, &
    ruota,area,slon1,slon2,slat1,slat2)

! c VERIFICA - util.f
! c legge le ccordinate delle pseudostazioni da file grib (punti di griglia)
! c autore: Chiara Marsigli

    real ::      xb(MNBOX),yb(MNBOX)
    character vfile*60
    parameter (MIDIMG=80000,MIDIMV=MIDIMG*4)
    integer ::   xgrib(MIDIMG)
    real ::      xgrid(MIDIMV)
    logical ::   ruota,area
! grib fields
    integer ::   ksec0(2),ksec1(104),ksec2(22),ksec3(2),ksec4(42)
    real ::      psec2(10),psec3(2)
    integer ::   level(3),var(3),est(3),scad(4),data(3),ora(2)
    real ::      alat(4),alon(4)

    print*,'subroutine leggibox'
! leggo un grib tanto per gradire, per conoscerne la griglia
    iug=0
    idimg=MIDIMG
    idimv=MIDIMV
    imd=-32768
    rmd=-1.5E21
    igrid=0   !sono griglie regolari
    call pbopen(iug,vfile,'r',ier)
    if(ier /= 0)goto9100
    call getinfoest(iug,xgrib,idimg,data,ora,scad,level, &
    var,est,alat(1),alat(2),alon(1),alon(2), &
    ny,nx,dy,dx,idrt,alarot,alorot,rot,ija,ier)
    if(ier /= 0)goto 9300
    call pbclose(iug,ier)
    if(ier < 0)goto9500

    if(area)then
        call rot_grib_LAMBO(alorot,alarot,rxeq,ryeq)
        cryeq=cos(ryeq*3.1415927/180.)
        sryeq=sin(ryeq*3.1415927/180.)
        if(ruota)then
            call tlld(slon1,slat1,rxeq,cryeq,sryeq,rslon1,rslat1)
            call tlld(slon2,slat2,rxeq,cryeq,sryeq,rslon2,rslat2)
            slon1=rslon1
            slon2=rslon2
            slat1=rslat1
            slat2=rslat2
            print*,'estremi area box ',slon1,slon2,slat1,slat2
        endif
    endif

    if(ija == 64)then
        blat=alat(1)
        do iy=1,ny
            blon=alon(1)
            do ix=1,nx
                ib=ix+(iy-1)*nx
                xb(ib)=blon
                yb(ib)=blat
                blon=blon+dx
            enddo
            blat=blat+dy
        enddo
    elseif(ija == 0)then    !con 128 non funziona
        blat=alat(1)
        do iy=1,ny
            blon=alon(1)
            do ix=1,nx
                ib=ix+(iy-1)*nx
                xb(ib)=blon
                yb(ib)=blat
                blon=blon+dx
            enddo
            blat=blat-dy
        enddo
    else
        print*,'valore di ija non previsto!'
        call exit(1)
    endif
    nbox=ib

    if(area)then
        nb=0
        do i=1,nbox
            if(xb(i) >= slon1 .AND. xb(i) <= slon2 .AND. &
            yb(i) >= slat1 .AND. yb(i) <= slat2)then
                nb=nb+1
                xb(nb)=xb(i)
                yb(nb)=yb(i)
            endif
        enddo
        nbox=nb
    endif

    if(nbox > MNBOX)then
        print*,'ERRORE - MNBOX INSUFFICIENTE'
        call exit (1)
    endif

    return
    9100 print *,"Errore durante la pbopen ",ier
    return
    9300 print *,"Errore durante la getinfoest ",ier
    return
    9500 print *,"Errore durante la pbclose ",ier
    return
    end subroutine leggibox

!********************************************************************************

    subroutine medbox(MIDIMV,MNRM,MNSTAZ,MNBOX, &
    xb,yb,nbox,dxb,dyb,x,y,alt,obsst,rmgrid, &
    npmod,xpmod,ypmod, &
    nminobs,rmddb,rmd,rmdo,nrm,media,massimo,prob, &
    distr,perc, &
    lsm,ls,obm,thr,obs,pred,nb,xbox,ybox,altbox)

! c VERIFICA - util.f
! c calcola i valori medi, i valori massimi o le frequenze/probabilita'
! c di previsti e osservati all'interno di box
! c autore: Chiara Marsigli

    parameter (mnpo=1000)
    real ::      obsst(MNSTAZ),rmgrid(MIDIMV,MNRM)
    real ::      lsm(MIDIMV),obm(MIDIMV)
    real ::      x(MNSTAZ),y(MNSTAZ),alt(MNSTAZ)
    real ::      xb(MNBOX),yb(MNBOX)
    real ::      xpmod(npmod),ypmod(npmod)
    real ::      xbox(MNSTAZ),ybox(MNSTAZ),altbox(MNSTAZ)
    real ::      obs(MNSTAZ),pred(MNSTAZ,MNRM)
    real ::      thr,perc,medo,medp
    real ::      vecto(mnpo),vectp(mnpo)
    logical ::   media,massimo,prob,distr

    print*,'util.f - medbox ',nminobs

! Computation of average value in a box
    ib=0
    do ibox=1,nbox
        eblon1=xb(ibox)-dxb/2.
        eblon2=eblon1+dxb
        eblat1=yb(ibox)-dyb/2.
        eblat2=eblat1+dyb
        npo=0
    ! il ciclo va sempre fatto su tutte le stazioni,
    ! perche' sono riempite lasciando i buchi
        do ist=1,MNSTAZ
            if(x(ist) >= eblon1 .AND. x(ist) < eblon2 &
             .AND. y(ist) >= eblat1 .AND. y(ist) < eblat2 &
             .AND. obsst(ist) /= rmddb)then
                npo=npo+1
            endif
        enddo
    ! write(77,*)'box ',ibox,xb(ibox),yb(ibox),' npo ',npo
    ! procedo solo se ci sono sufficienti osservazioni per box
        if(npo >= nminobs)then
            ib=ib+1
            if(ib > MNSTAZ)then
                print*,'ERRORE! ib MAGGIORE DI MNSTAZ!'
                call exit (1)
            endif
            xbox(ib)=xb(ibox)
            ybox(ib)=yb(ibox)

        ! write(99,*)'box ',ib,xbox(ib),ybox(ib),' npo ',npo

            if(media)then       ! MEDIA

                obs(ib)=0.
                altbox(ib)=0.
                do ist=1,MNSTAZ
                    if(x(ist) >= eblon1 .AND. x(ist) < eblon2 &
                     .AND. y(ist) >= eblat1 .AND. y(ist) < eblat2 &
                     .AND. obsst(ist) /= rmddb)then
                        obs(ib)=obs(ib)+obsst(ist)
                        altbox(ib)=altbox(ib)+alt(ist)
                    endif
                enddo
                if(npo /= 0)then
                    obs(ib)=obs(ib)/real(npo)
                    altbox(ib)=altbox(ib)/real(npo)
                ! write(99,*)obs(ib),altbox(ib)
                else
                    obs(ib)=rmddb
                    altbox(ib)=rmdo
                endif
                do irm=1,nrm
                    npp=0
                    pred(ib,irm)=0.
                    do ip=1,npmod
                        if(xpmod(ip) >= eblon1 &
                         .AND. xpmod(ip) < eblon2 &
                         .AND. ypmod(ip) >= eblat1 &
                         .AND. ypmod(ip) < eblat2 &
                         .AND. rmgrid(ip,irm) /= rmd)then
                            if(nint(obm(ip)) == 1)then
                                if(ls >= 0)then
                                    if(nint(lsm(ip)) == ls)then
                                        npp=npp+1
                                        pred(ib,irm)=pred(ib,irm)+ &
                                        rmgrid(ip,irm)
                                    endif
                                else
                                    npp=npp+1
                                    pred(ib,irm)=pred(ib,irm)+rmgrid(ip,irm)
                                ! write(99,*)pred(ib,irm)
                                endif
                            endif
                        endif
                    enddo
                    if(npp /= 0)then
                        pred(ib,irm)=pred(ib,irm)/real(npp)
                    else
                        pred(ib,irm)=rmddb
                    endif
                enddo            ! nrm

            elseif(massimo)then ! MASSIMO

                obs(ib)=-999.9
                altbox(ib)=0.
                do ist=1,MNSTAZ
                    if(x(ist) >= eblon1 .AND. x(ist) < eblon2 &
                     .AND. y(ist) >= eblat1 .AND. y(ist) < eblat2 &
                     .AND. obsst(ist) /= rmddb)then
                        obs(ib)=max(obs(ib),obsst(ist))
                        altbox(ib)=altbox(ib)+alt(ist)
                    endif
                enddo
                if(npo /= 0)then
                    altbox(ib)=altbox(ib)/real(npo)
                else
                    altbox(ib)=rmdo
                    obs(ib)=rmddb
                endif
                do irm=1,nrm
                    pred(ib,irm)=-999.9
                    ncont=0
                    do ip=1,npmod
                        if(xpmod(ip) >= eblon1 &
                         .AND. xpmod(ip) < eblon2 &
                         .AND. ypmod(ip) >= eblat1 &
                         .AND. ypmod(ip) < eblat2 &
                         .AND. rmgrid(ip,irm) /= rmd)then
                            if(nint(obm(ip)) == 1)then
                                if(ls >= 0)then
                                    if(nint(lsm(ip)) == ls)then
                                        ncont=ncont+1
                                        pred(ib,irm)= &
                                        max(pred(ib,irm),rmgrid(ip,irm))
                                    endif
                                else
                                    ncont=ncont+1
                                    pred(ib,irm)= &
                                    max(pred(ib,irm),rmgrid(ip,irm))
                                endif
                            endif
                        endif
                    enddo
                    if(ncont == 0)pred(ib,irm)=rmddb
                enddo            ! nrm

            elseif(prob)then    ! PROB

                obs(ib)=0.
                altbox(ib)=0.
                do ist=1,MNSTAZ
                    if(x(ist) >= eblon1 .AND. x(ist) < eblon2 &
                     .AND. y(ist) >= eblat1 .AND. y(ist) < eblat2 &
                     .AND. obsst(ist) /= rmddb &
                     .AND. obsst(ist) >= thr)then
                        obs(ib)=obs(ib)+1.
                        altbox(ib)=altbox(ib)+alt(ist)
                    endif
                enddo
                if(npo /= 0)then
                    obs(ib)=(obs(ib)/real(npo))*10.
                    altbox(ib)=altbox(ib)/real(npo)
                else
                    obs(ib)=rmddb
                    altbox(ib)=rmdo
                endif
                do irm=1,nrm
                    npp=0
                    pred(ib,irm)=0.
                    do ip=1,npmod
                        if(xpmod(ip) >= eblon1 &
                         .AND. xpmod(ip) < eblon2 &
                         .AND. ypmod(ip) >= eblat1 &
                         .AND. ypmod(ip) < eblat2 &
                         .AND. rmgrid(ip,irm) /= rmd)then
                            if(nint(obm(ip)) == 1)then
                                if(ls >= 0)then
                                    if(nint(lsm(ip)) == ls)then
                                        npp=npp+1
                                        if(rmgrid(ip,irm) >= thr)then
                                            pred(ib,irm)=pred(ib,irm)+1.
                                        endif
                                    endif
                                else
                                    npp=npp+1
                                    if(rmgrid(ip,irm) >= thr)then
                                        pred(ib,irm)=pred(ib,irm)+1.
                                    endif
                                endif
                            endif
                        endif
                    enddo
                    if(npp /= 0)then
                        pred(ib,irm)=(pred(ib,irm)/real(npp))*10.
                    else
                        pred(ib,irm)=rmddb
                    endif
                enddo            ! nrm

            elseif(distr)then   ! DISTR

                nvo=0
                do ist=1,MNSTAZ
                    if(x(ist) >= eblon1 .AND. x(ist) < eblon2 &
                     .AND. y(ist) >= eblat1 .AND. y(ist) < eblat2 &
                     .AND. obsst(ist) /= rmddb)then
                        nvo=nvo+1
                        vecto(nvo)=obsst(ist)
                    endif
                enddo
                do irm=1,nrm
                    nvp=0
                    do ip=1,npmod
                        if(xpmod(ip) >= eblon1 &
                         .AND. xpmod(ip) < eblon2 &
                         .AND. ypmod(ip) >= eblat1 &
                         .AND. ypmod(ip) < eblat2 &
                         .AND. rmgrid(ip,irm) /= rmd)then
                            if(nint(obm(ip)) == 1)then
                                if(ls >= 0)then
                                    if(nint(lsm(ip)) == ls)then
                                        nvp=nvp+1
                                        vectp(nvp)=rmgrid(ip,irm)
                                    endif
                                else
                                    nvp=nvp+1
                                    vectp(nvp)=rmgrid(ip,irm)
                                endif
                            endif
                        endif
                    enddo
                    if(nvp >= 3)then
                        call percentile(nvo,nvp,vecto,vectp,medo,medp,perc)
                        write(22,'(4(1x,a,1x,i4),2(1x,a,1x,f10.6)') &
                        'box',ib,'nvo',nvo,'irm',irm,'nvp',nvp, &
                        'medo',medo,'medp',medp
                        obs(ib)=medo
                        pred(ib,irm)=medp
                    else
                        obs(ib)=rmddb
                        pred(ib,irm)=rmddb
                        write(22,'(4(1x,a,1x,i4),2(1x,a,1x,f10.6)') &
                        'box',ib,'nvo',nvo,'irm',irm,'nvp',nvp, &
                        'o',obs(ib),'p',pred(ib,irm)
                    endif
                enddo            ! nrm

            endif               !media o massimo o probabilita'

        endif                  !nminobs
    enddo                     !nbox
    nb=ib
    print*,'numero box buone ',nb
    write(23,*)'numero box buone ',nb

    return
    end subroutine medbox

!*****************************************************************************

    subroutine percentile(nvo,nvp,vecto,vectp,medo,medp,perc)

    real :: vecto(nvo),vectp(nvp)
    real :: medo,medp,perc

    k=(perc/100.)*nvo        ! percentile relativo al campione
! print*,'k obs ',k,' perc ',selip(k,nvo,vecto)
    medo=selip(k,nvo,vecto)
    k=(perc/100.)*nvp        ! percentile relativo al campione
! print*,'k pre ',k,' perc ',selip(k,nvp,vectp)
    medp=selip(k,nvp,vectp)

    return
    end subroutine percentile

!********************************************************************

    subroutine shell(n,a)

    real :: a(n)

    inc=1
    1 inc=3*inc+1
    if(inc <= n)goto1
    2 continue
    inc=inc/3
    do i=inc+1,n
        v=a(i)
        j=i
        3 if(a(j-inc) > v)then
            a(j)=a(j-inc)
            j=j-inc
            if(j <= inc)goto4
            goto3
        endif
        4 a(j)=v
    enddo
    if(inc > 1)goto2
    return
    end subroutine shell

!************************************************************************

    function selip(k,n,arr)

    parameter(M=100)
    integer :: k,n,M,isel(M+2)
    real :: selip,arr(n),sel(M+2)

    kk=k
    ahi=1.E15
    alo=-1.E15
    1 continue
    mm=0
    nlo=0
    sum=0.
    nxtmm=M+1
    do i=1,n
        if(arr(i) >= alo .AND. arr(i) <= ahi)then
            mm=mm+1
            if(arr(i) == alo)nlo=nlo+1
            if(mm <= M)then
                sel(mm)=arr(i)
            elseif(mm == nxtmm)then
                nxtmm=mm+mm/M
                sel(1+mod(i+mm+kk,M))=arr(i)
            endif
            sum=sum+arr(i)
        endif
    enddo
    if(kk <= nlo)then
        selip=alo
        return
    elseif(mm <= M)then
        call shell(mm,sel)
        selip=sel(kk)
        return
    endif
    sel(M+1)=sum/mm
    call shell(M+1,sel)
    sel(M+2)=ahi
    do j=1,M+2
        isel(j)=0
    enddo
    do i=1,n
        if(arr(i) >= alo .AND. arr(i) <= ahi)then
            jl=0
            ju=M+2
            2 if(ju-jl > 1)then
                jm=(ju+jl)/2
                if(arr(i) >= sel(jm))then
                    jl=jm
                else
                    ju=jm
                endif
                goto2
            endif
            isel(ju)=isel(ju)+1
        endif
    enddo
    j=1
    3 if(kk > isel(j))then
        alo=sel(j)
        kk=kk-isel(j)
        j=j+1
        goto3
    endif
    ahi=sel(j)
    goto1

    end function selip

!************************************************************************

    FUNCTION ngiorni_mese(nm,na)

    INTEGER ngiorni_mese
    INTEGER giomax(12)
    
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

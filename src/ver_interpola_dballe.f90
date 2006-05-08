    program interpola_dballe

! c VERIFICA - interpola_dballe.f
! c programma per l'interpolazione degli output dei modelli su punti sparsi
! c autore: Chiara Marsigli
! c 26 febbraio 2003 - corretta gestione ngetpoint per uso
! c maschera mare-terra
! c 31 luglio 2003 - modifica alla subroutine leggiana_db per scegliere se
! c leggere le osservazioni (itipostaz=0) o l'analisi (itipostaz=2)
! c 12 genaio 2005 - aggiunta gestione verifica di analisi
! c estate 2005 - adattamento al nuovo database Dballe
! c 6 dicembre 2005 - sistemata gestione ensemble
! c 9 dicembre 2005 - tolto il ciclo sulle variabili (aggiunto in verifica_batch)
! c e inserito imet


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

    parameter (MIDIMG=80000,MIDIMV=MIDIMG*4)
    parameter (MNSCAD=30,MNGIO=150,MNRM=102)
    integer ::   xgrib(MIDIMG)
    real ::      xgrid(MIDIMV),lsm(MIDIMV),oro(MIDIMV)
    real ::      xgridu(MIDIMV),xgridv(MIDIMV)
    integer ::   level(3),var(3),est(3),scad(4),data(3),ora(2)
    integer ::   dataval(3),oraval(2),scaddb(4)
    integer ::   varv(3),ore(24)
    real ::      a,b,dato
    integer ::   iscaddb
    real ::      alat(4),alon(4)
    character vfile*60,cvar*6,cvarv*6,cel*3,descrfisso*20
    character descr*20
! namelists
    INTEGER ::   kvar(3,2),lsvar,nore
    integer ::   scadenze(4,MNSCAD),dum(2)
    integer ::   imet,imod,ls,itipo,iana,ivor
    logical ::   ruota,media,massimo,prob,distr,diffh
    real ::      dxb,dyb,diffmax,hdiff,thr,perc
    character model*10
    character(19) :: database,user,password

    real, ALLOCATABLE :: x(:),y(:),alt(:)
    real, ALLOCATABLE :: xstaz(:,:),xstazv(:,:)

! database
    integer :: handler,handle
    logical :: init,debug,rmmiss
    data init,debug,rmmiss/.true.,.true.,.false./

    external error_handle

    common /point/ij1,ij2,ij3,ij4

    namelist  /parameters/nora,ngio,nscad,scad1,scad2,inc, &
    nvar,nrm,nore,ore
    namelist  /date/data
    namelist  /scadenza/scadenze
    namelist  /parametro/kvar
    namelist  /stat/model,itipo,iana,imet,imod,ls, &
    ruota,nminobs,media,massimo,prob,distr,dxb,dyb, &
    diffh,diffmax,thr,perc
    namelist  /odbc/database,user,password

    data level/-1,-1,-1/, var/-1,-1,-1/, est/-1,-1,-1/, &
    scad/-1,-1,-1,-1/, data/-1,-1,-1/, ora/-1,-1/, &
    varv/-1,-1,-1/
    data rmdo/-999.9/

    print*,'program interpola'

    open(1,file='odbc.nml',status='old',readonly)
    read(1,nml=odbc,err=9001)
    close(1)
    open(1,file='stat.nml',status='old',readonly)
    read(1,nml=stat,err=9002)
    close(1)

! gestione degli errori
    call idba_error_set_callback(0,error_handle,debug,handle_err)

! connessione con database
    call idba_presentati(idbhandle,database,user,password)

! apertura database in lettura
    call idba_preparati(idbhandle,handler,"read","read","read")

    call idba_quantesono(handler,nstaz)
    print*,'massimo numero pseudo-stazioni ',nstaz

! llocazione matrici
    ALLOCATE(x(1:nstaz))
    ALLOCATE(y(1:nstaz))
    ALLOCATE(alt(1:nstaz))

! leggo tutte le stazioni disponibili
    call leggiana_db(iana,x,y,alt,rmdo,nstaz,handler)
    
    IF(diffh .OR. (ls >= 0))THEN
      CALL modello(model,ivlsm,ivor,ls,diffh)
      PRINT*,' ivlsm ',ivlsm,' ivor ',ivor
    ENDIF

! il tipo di elaborazione e' fisso per questa routine (=1)
    call descrittore(model,itipo,imod,ls,media,massimo,prob, &
    distr,dxb,dyb,descr)
    print*,'descrittore ',descr
    descrfisso=descr

! apertura database in scrittura
    call idba_preparati(idbhandle,handle,"reuse","rewrite","rewrite")

    iug=0
    idimg=MIDIMG
    idimv=MIDIMV
    imd=-32768
    igrid=0                    !sono griglie regolari
    vfile='estratti.grib'
    call pbopen(iug,vfile,'r',ier)
    if(ier /= 0)goto9100

! leggo la land-sea mask
    if (ls >= 0) then
        var(3)=ivlsm
        PRINT*,DATA,ora,scad,level,var,est
        call findgribest(iug,xgrib,idimg,data,ora, &
        scad,level,var,est,ier)
        if(ier == -1)then
            print*,'non trovo la land-sea mask! '
            call exit(1)
        elseif(ier /= 0)then
            goto 9200
        endif
        call getdata(xgrib,idimg,imd,rmdo,lsm,idimv, &
        ibm,ier)
        if(ibm /= 0 .OR. ier /= 0)goto 9400
    endif
! leggo l'orografia
    if(diffh)then
        var(3)=ivor
        call findgrib(iug,xgrib,idimg,data,ora, &
        scad,level,var,ier)
        if(ier == -1)then
            print*,'non trovo orografia! '
            call exit(1)
        elseif(ier /= 0)then
            goto 9200
        endif
        call getdata(xgrib,idimg,imd,rmdo,oro,idimv, &
        ibm,ier)
        if(ibm /= 0 .OR. ier /= 0)goto 9400
    endif

! Read parameters
    open(1,file='parameters.nml',status='old',readonly)
    read(1,nml=parameters,err=9001)
    close(1)
    ora(1)=nora/100.
    ora(2)=mod(nora,100)
    print*,'nrm ',nrm
    print*,'nvar in parameters ',nvar

! llocazione matrici
    ALLOCATE(xstaz(1:nstaz,1:nrm))
    ALLOCATE(xstazv(1:nstaz,1:nrm))

    open(1,file='scadenze.nml',status='old',readonly)
    read(1,nml=scadenza,err=9003)
    close(1)

    call cleankey(2,3,4,level,var,est,scad,data,dum)
    open(1,file='parametro.nml',status='old',readonly)
    read(1,nml=parametro,err=9012)
    close(1)
    do i=1,3
        var(i)=kvar(i,1)
    enddo
    call variabile(3,var,cvar,a,b,.true.)

    lsvar=ls
    write(*,*)'variabile ',var,' cvar ',cvar, &
    ' lsvar ',lsvar,' a ',a,' b ',b

    if(imet == 0 .OR. imet == 1)then         ! scalare

        wind=.false.

    ! Read date
        open(3,file='date.nml',status='old',readonly)
        do igio=1,ngio
            read(3,nml=date,err=9004)
            print*,'data ',data
            do iscad=1,nscad
                do is=1,4
                    scad(is)=scadenze(is,iscad)
                enddo
                print*,'scadenza ',scad

                iscaddb=max(scadenze(2,iscad),scadenze(3,iscad))
                call JELADATA5(data(1),data(2),data(3), &
                ora(1),ora(2),iminuti)
                iminuti=iminuti+iscaddb*60
                call JELADATA6(iday,imonth,iyear, &
                ihour,imin,iminuti)
                dataval(1)=iday
                dataval(2)=imonth
                dataval(3)=iyear
                oraval(1)=ihour
                oraval(2)=imin

                do iore=1,nore

                    if(iore > 1)then
                    ! trovo data e ora dell'emissione per ore successive (analisi)
                        ora(1)=ore(iore)/100.
                        ora(2)=mod(ore(iore),100)
                        oraval(1)=ora(1)
                        oraval(2)=ora(2)
                    endif

                    print*,'validita'' ',dataval,oraval,iscaddb
                    print*,'emissione ',data,ora,iscaddb

                    do irm=1,nrm
                        est(1)=-1
                        est(2)=-1
                        est(3)=-1
                        call findgribest(iug,xgrib,idimg,data,ora, &
                        scad,level,var,est,ier)
                        if(ier == -1)then
                            print*,'grib mancante - azzero ', &
                            'tutto l''ensemble'
                            xstaz=rmdo
                            goto 111
                        elseif(ier /= 0)then
                            goto 9200
                        endif
                        call getinfoest(-1,xgrib,idimg, &
                        data,ora,scad,level,var,est, &
                        alat(1),alat(2),alon(1),alon(2), &
                        ny,nx,dy,dx,idrt,alarot,alorot,rot,ija,ier)
                        if(ier /= 0)goto 9300
                        call getdata(xgrib,idimg,imd,rmdo,xgrid,idimv, &
                        ibm,ier)
                        if(ibm /= 0 .OR. ier /= 0)goto 9400
                        IF(lsvar == -1)THEN
                          lsvar=1
                          DO ivec=1,idimv
                            IF(xgrid(ivec) == rmdo)THEN
                              lsm(ivec)=0
                            ELSE
                              lsm(ivec)=1
                            ENDIF
                          ENDDO
                        ELSEIF(lsvar == 0)THEN
                          DO ivec=1,idimv
                            IF (xgrid(ivec) == rmdo .or. lsm(ivec)==1)THEN
                              lsm(ivec)=1
                            ELSE
                              lsm(ivec)=0
                            ENDIF
                          ENDDO
                        ELSEIF(lsvar == 1)then
                          DO ivec=1,idimv
                            IF (xgrid(ivec) == rmdo .or. lsm(ivec)==0)THEN
                              lsm(ivec)=0
                            ELSE
                              lsm(ivec)=1
                            ENDIF
                          ENDDO
                        ENDIF

                    ! Interpolation of predicted data on (lat,lon) station points
                        if(ruota == .TRUE. )then
                            call rot_grib_LAMBO(alorot,alarot, &
                            tlm0d,tph0d)
                        endif
                        do ist=1,nstaz
                            if(abs(x(ist)-rmdo) > 0.1 .AND. &
                            abs(y(ist)-rmdo) > 0.1)then
                                call ngetpoint(x(ist),y(ist), &
                                xgrid,xgrid,lsm, &
                                idimv,nx,ny,alon(1),alat(1),dx,dy, &
                                igrid,ija,tlm0d,tph0d,wind,imod, &
                                lsvar,xint,dummy,ier)
                                if(ier == 2 .OR. ier == 4)then
                                ! cerca di interpolare su un punto che non e' nel dominio dei dati!
                                    xstaz(ist,irm)=rmdo
                                elseif(ier == 0)then
                                    if (diffh) then
                                        ind=ij1
                                        hdiff=abs(oro(ind)/9.81-alt(ist))
                                        if (hdiff <= diffmax) then
                                            xstaz(ist,irm)=xint
                                        else
                                            xstaz(ist,irm)=rmdo
                                        endif
                                    else
                                        xstaz(ist,irm)=xint
                                    endif
                                else
                                    goto 9600
                                endif
                            endif
                        enddo
                    enddo         !nrm

                ! conversione delle scadenze in secondi (e correzione scadenze sbagliate)
                    call converti_scadenze(4,scad,scaddb)

                    call idba_seti (handle,"p1",scaddb(2))
                    call idba_seti (handle,"p2",scaddb(3))
                    call idba_seti (handle,"pindicator",scaddb(4))

                    call idba_seti (handle,"year",dataval(3))
                    call idba_seti (handle,"month",dataval(2))
                    call idba_seti (handle,"day",dataval(1))
                    call idba_seti (handle,"hour",oraval(1))
                    call idba_seti (handle,"min",oraval(2))
                    call idba_seti (handle,"sec",0)

                ! scrittura su database
                    do irm=1,nrm
                        if(nrm > 1)then
                            write(cel,'(i3.3)')irm
                            descr=descrfisso(1:nlenvera(descrfisso)) &
                            //'el'//cel
                        endif
                        print*,'scrivo: descr ',descr

                        call idba_setc (handle,"rep_memo",descr)

                        do ist=1,nstaz
                            if(abs(x(ist)-rmdo) > 0.1 .AND. &
                            abs(y(ist)-rmdo) > 0.1 .AND. &
                            xstaz(ist,irm) /= rmdo)then
                                rlat=y(ist)
                                rlon=x(ist)
                                h=alt(ist)

                            ! imposto tutta l'anagrafica

                            ! prima faccio unset di ana_id senno' ricopre sempre!!!
                                call idba_unset (handle,"ana_id")

                                call idba_seti (handle,"height",h)
                                call idba_setr (handle,"lat",rlat)
                                call idba_setr (handle,"lon",rlon)

                                call idba_seti (handle,"leveltype", &
                                level(1))
                                call idba_seti (handle,"l1",level(2))
                                call idba_seti (handle,"l2",level(3))

                                call idba_seti (handle,"mobile",0)

                                if(imet == 0)then ! scalare

                                ! attenzione!!!!!! ho bisogno che il minimo sia 0????
                                    dato=a+xstaz(ist,irm)*b

                                elseif(imet == 1)then !scalare direzione

                                    if(xstaz(ist,irm) <= 1. .AND. &
                                    xstaz(ist,irm) /= 0.)then
                                        dato=1.
                                    else
                                        dato=a+xstaz(ist,irm)*b
                                    endif

                                endif

                                call idba_setr (handle,cvar,dato)
                                call idba_prendilo (handle)

                            endif
                        enddo      !nstaz

                    enddo         !nrm

                    111 continue      !grib non trovato

                enddo            !nore
            enddo               !nscad
        enddo                  !ngio
        close(3)               !date

    elseif(imet == 2)then     ! vettore

        wind=.true.

        do i=1,3
            varv(i)=kvar(i,2)
        enddo
        call variabile(3,varv,cvarv,a,b,.true.)

        open(3,file='date.nml',status='old',readonly)
        do igio=1,ngio
            read(3,nml=date,err=9004)
            print*,'data ',data
            do iscad=1,nscad
                do is=1,4
                    scad(is)=scadenze(is,iscad)
                enddo
                print*,'scadenza ',scad

                iscaddb=max(scadenze(2,iscad),scadenze(3,iscad))
                call JELADATA5(data(1),data(2),data(3), &
                ora(1),ora(2),iminuti)
                iminuti=iminuti+iscaddb*60
                call JELADATA6(iday,imonth,iyear, &
                ihour,imin,iminuti)
                dataval(1)=iday
                dataval(2)=imonth
                dataval(3)=iyear
                oraval(1)=ihour
                oraval(2)=imin

                do iore=1,nore

                    if(iore > 1)then
                    ! trovo data e ora dell'emissione per ore successive (analisi)
                        ora(1)=ore(iore)/100.
                        ora(2)=mod(ore(iore),100)
                        oraval(1)=ora(1)
                        oraval(2)=ora(2)
                    endif

                    print*,'validita'' ',dataval,oraval,iscaddb
                    print*,'emissione ',data,ora,iscaddb

                ! u component
                    do irm=1,nrm
                        est(1)=-1
                        est(2)=-1
                        est(3)=-1
                        call findgribest(iug,xgrib,idimg,data,ora, &
                        scad,level,var,est,ier)
                        if(ier == -1)then
                            print*,'grib mancante - azzero ', &
                            'tutto l''ensemble'
                            xstaz=rmdo
                            xstazv=rmdo
                            goto 222
                        elseif(ier /= 0)then
                            goto 9200
                        endif
                        call getinfoest(-1,xgrib,idimg, &
                        data,ora,scad,level,var,est, &
                        alat(1),alat(2),alon(1),alon(2), &
                        ny,nx,dy,dx,idrt,alarot,alorot,rot, &
                        ija,ier)
                        if(ier /= 0)goto 9300
                        call getdata(xgrib,idimg,imd,rmdo,xgrid,idimv, &
                        ibm,ier)
                        if(ibm /= 0 .OR. ier /= 0)goto 9400
                        IF(lsvar == -1)THEN
                          lsvar=1
                          DO ivec=1,idimv
                            IF(xgrid(ivec) == rmdo)THEN
                              lsm(ivec)=0
                            ELSE
                              lsm(ivec)=1
                            ENDIF
                          ENDDO
                        ELSEIF(lsvar == 0)THEN
                          DO ivec=1,idimv
                            IF (xgrid(ivec) == rmdo .or. lsm(ivec)==1)THEN
                              lsm(ivec)=1
                            ELSE
                              lsm(ivec)=0
                            ENDIF
                          ENDDO
                        ELSEIF(lsvar == 1)then
                          DO ivec=1,idimv
                            IF (xgrid(ivec) == rmdo .or. lsm(ivec)==0)THEN
                              lsm(ivec)=0
                            ELSE
                              lsm(ivec)=1
                            ENDIF
                          ENDDO
                        ENDIF
                        do iv=1,idimv
                            xgridu(iv)=xgrid(iv)
                        enddo
                    enddo
                ! v component
                    do irm=1,nrm
                        est(1)=-1
                        est(2)=-1
                        est(3)=-1
                        call findgribest(iug,xgrib,idimg,data,ora, &
                        scad,level,varv,est,ier)
                        if(ier == -1)then
                            print*,'grib mancante - azzero ', &
                            'tutto l''ensemble'
                            do jrm=1,nrm
                                do ist=1,nstaz
                                    xstaz(ist,jrm)=rmdo
                                    xstazv(ist,jrm)=rmdo
                                enddo
                            enddo
                            goto 222
                        elseif(ier /= 0)then
                            goto 9200
                        endif
                        call getdata(xgrib,idimg,imd,rmdo,xgrid,idimv, &
                        ibm,ier)
                        if(ibm /= 0 .OR. ier /= 0)goto 9400
                        do iv=1,idimv
                            xgridv(iv)=xgrid(iv)
                        enddo
                    enddo

                ! Interpolation of predicted data on (lat,lon) station points
                    if(ruota == .TRUE. )then
                        call rot_grib_LAMBO(alorot,alarot, &
                        tlm0d,tph0d)
                    endif
                    do irm=1,nrm
                        do ist=1,nstaz
                            if(abs(x(ist)-rmdo) > 0.1 .AND. &
                            abs(y(ist)-rmdo) > 0.1)then
                                call ngetpoint(x(ist),y(ist), &
                                xgridu,xgridv, &
                                lsm, &
                                idimv,nx,ny,alon(1),alat(1), &
                                dx,dy,igrid, &
                                ija,tlm0d,tph0d,wind,imod,lsvar, &
                                xintu,xintv,ier)
                                if(ier == 2 .OR. ier == 4)then
                                ! cerco di interpolare su un punto che non e' nel dominio dei dati
                                    xstaz(ist,irm)=rmdo
                                    xstazv(ist,irm)=rmdo
                                elseif(ier == 0)then
                                    xstaz(ist,irm)=xintu
                                    xstazv(ist,irm)=xintv
                                else
                                    goto 9600
                                endif
                            endif
                        enddo
                    enddo         !nrm

                ! scrittura su database
                    do irm=1,nrm
                        if(nrm > 1)then
                            write(cel,'(i3.3)')irm
                            descr=descrfisso(1:nlenvera(descrfisso)) &
                            //'el'//cel
                        endif
                        print*,'scrivo: descr ',descr
                        do ist=1,nstaz
                            if(abs(x(ist)-rmdo) > 0.1 .AND. &
                            abs(y(ist)-rmdo) > 0.1 .AND. &
                            xstaz(ist,irm) /= rmdo)then
                                rlat=y(ist)
                                rlon=x(ist)
                                h=alt(ist)

                            ! imposto tutta l'anagrafica

                            ! prima faccio unset di ana_id senno' ricopre sempre!!!
                                call idba_unset (handle,"ana_id")

                                call idba_seti (handle,"height",h)
                                call idba_setr (handle,"lat",rlat)
                                call idba_setr (handle,"lon",rlon)
                                call idba_setc (handle,"rep_memo",descr)

                                call idba_seti (handle,"leveltype", &
                                level(1))
                                call idba_seti (handle,"l1",level(2))
                                call idba_seti (handle,"l2",level(3))

                                call idba_seti (handle,"mobile",0)

                                call idba_setc (handle,"rep_memo",descr)

                                dato=a+xstaz(ist,irm)*b
                                call idba_seti (handle,cvar,dato)
                            ! Scrivo anche v
                                dato=a+xstazv(ist,irm)*b
                                call idba_seti (handle,cvarv,dato)

                                call idba_prendilo (handle)

                            endif
                        enddo      !nstaz

                    enddo         !nrm

                    222 continue      !grib non trovato

                enddo            !nore
            enddo               !nscad
        enddo                  !ngio

        close(3)               !date

    else
        print*,'errore, imet non consentito'
        call exit(1)
    endif                     !imet

    call pbclose(iug,ier)
    if(ier < 0)goto9500

! chiusura database
    call idba_fatto(handler)
    call idba_fatto(handle)
    call idba_arrivederci(idbhandle)

    stop
    9001 print *,"Errore durante la lettura della namelist parameters "
    stop
    9002 print *,"Errore durante la lettura della namelist stat "
    stop
    9012 print *,"Errore durante la lettura della namelist parametro "
    stop
    9003 print *,"Errore durante la lettura della namelist scadenza "
    stop
    9004 print *,"Errore durante la lettura della namelist date "
    stop
    9100 print *,"Errore durante la pbopen ",ier
    stop
    9200 print *,"Errore durante la findgribest ",ier
    stop
    9300 print *,"Errore durante la getinfoest ",ier
    stop
    9400 print *,"Errore durante la getdata ",ier
    stop
    9500 print *,"Errore durante la pbclose ",ier
    stop
    9600 print *,"Errore durante la ngetpoint ",ier
    stop
    end program

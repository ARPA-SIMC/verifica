    program ver_scores_dballe

! mstart
! dx VERIFICA - ver_scores_dballe.f90 - programma per il calcolo degli score deterministici
! autore: Chiara Marsigli
! ultima modifica: 29 gennaio 2021 - update verifica

! ATTENZIONE!!! ora le soglie vanno nell'unita' di misura prevista dalla
! Blocale, per omogeneita' tra le variabili!!! E per ora lascio cosi'
! anche l'output, poi vediamo!

! valori indicativi:
! MSNTAZ par massimo numero di stazioni e pseudo-stazioni
! MNSCAD par massimo numero di scadenze di previsione
! MNGIO  par massimo numero di giorni su cui fare la verifica
! MNORE  par massimo numero di ore al giorno su cui fare la verifica (analisi)
! MNSOG  par massimo numero di soglie per il calcolo degli scores
! MNV    par dimensione massima dei vettori di previsti e osservati
! namelist parameters:
! nora   int ora di inizio dei run da verificare (per una previsione)
! oppure prima delle ore da verificare (per una analisi)
! 4 cifre: hhmm
! ngio   int numero di giorni su cui fare la verifica
! nscad  int numero di scadenze di previsione da verificare
! nvar   int numero di variabili da verificare
! nrm    int numero di elementi dell'ensemble
! scad1  int prima delle scadenze di previsione da verificare
! scad2  int ultima delle scadenze di previsione da verificare
! inc    int incremento (in ore) della scadenza di previsione
! namelist stat:
! itipo  int numero identificativo del tipo di verifica (1 su punti, 2 su box)
! iana   int 0 se verifico contro osservati, 1 se contro un'analisi
! diffh  log T se si considera la differenza di altezza tra punto oss e prev
! usato solo da interpola_dballe.f
! diffmax rea se diffh=T, max differenza di altezza consentita
! namelist lista:
! iquota int per verificare solo punti sopra o sotto ad una certa quota
! -1 se le voglio tutte
! 0 solo pianura (sotto hlimite)
! 1 solo montagna (sopra hlimite)
! hlimite rea valore limite per discriminare le quote
! lthr   int = 0 se non si vogliono calcolare gli scores per soglia
! > 0 per calcolare gli scores sopra le soglie
! < 0 per calcolare gli scores sotto le soglie
! nsoglie int numero di soglie sa verificare
! soglie rea vettore (MNSOG) di soglie da usare per la verifica
! daily  log T se voglio gli scores per giorno (e per ora nel caso di verifica di analisi)
! nore   int numero di ore al giorno su cui fare la verifica delle analisi
! data   int (3) data (gg,mm,aaaa) di partenza su cui fare la verifica
! database cha nome del database da cui si vuole attingere
! user     cha username ''
! password cha password ''

! ora    int (2) ora di partenza su cui fare la verifica
! var    int vettore (3) contentente i tre valori che identificano
! nella codifica grib la variabile da verificare
! oss    rea vettore (MNV) degli osservati
! prev   rea vettore (MNV) dei previsti
! dataval int (3) data di validita' del valore da estrarre
! oraval int (2) ora  di validita' del valore da estrarre
! icodice int posizione (univoca) della (pseudo)stazione richiesta nel database odbc
! itipost int tipo della (pseudo)stazione desunto in base alle namelists
! mend

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

    USE util_dballe
    USE common_namelists
    USE datetime_class
    
    PARAMETER (MNSTAZ=5000,MNGIO=366,MNORE=1)
    PARAMETER (MNV=MNSTAZ*MNGIO*MNORE)
! attenzione!!! Non sono usate, servono solo per dare
! un riferimento a chi dimensiona i vettori dinamicamente
    INTEGER :: ora(2),scad(4)
    INTEGER :: dataval(3),oraval(2),scaddb(4)
    INTEGER :: itipost,h
    type(anaid_type) :: icodice
    REAL :: dato
    integer :: leveltype1,l1,leveltype2,l2
    integer :: pind,fctime,period
    CHARACTER :: descr*20,descrfisso*20,cel*3
    INTEGER :: npo
    REAL :: maerr,mserr,rmserr,stdv,bi
    REAL :: cog

    REAL, ALLOCATABLE :: oss(:),prev(:,:),lon(:),lat(:)
    type(anaid_type), ALLOCATABLE :: anaid(:)
    REAL, ALLOCATABLE :: previ(:)

    CHARACTER(LEN=10) :: btable
    INTEGER :: handle,handle_err,handleana,USTAZ
    INTEGER :: debug=1

    integer :: ier

    TYPE(datetime) :: dt1
    TYPE(timedelta) :: td1
    INTEGER :: iyear,imonth,iday,ihour,imin
    
    DATA      rmdo/-999.9/,imd/32767/,rmddb/-999.9/
    
    print*,'program scores'

    open(1,file='odbc.nml',status='old')
    read(1,nml=odbc,err=9001)
    close(1)
! Read parameters
    open(1,file='parameters.nml',status='old')
    read(1,nml=parameters,err=9001)
    close(1)
! ora di inizio dei runs
    ora(1)=nora/100
    ora(2)=mod(nora,100)
    open(1,file='stat.nml',status='old')
    read(1,nml=stat,err=9002)
    close(1)

    open(2,file='lista.nml',status='old')
    read(2,nml=lista,err=9003)

    descrfisso=reportpre
    print*,'descrittore ',descrfisso
    if(itipo == 1)then
        itipost=0
        if(iana == 1)itipost=90 ! verifica contro analisi
    elseif(itipo == 2)then ! verifica su box
        itipost=80
    endif

! gestione degli errori
    ier=idba_error_set_callback(0,C_FUNLOC(idba_default_error_handler),debug,handle_err)

! connessione con database
    ier=idba_presentati(idbhandle,database)

! apertura database in lettura
    ier=idba_preparati(idbhandle,handle,"read","read","read")
    ier=idba_preparati(idbhandle,handleana,"read","read","read")

    ier=idba_set(handle,"rep_memo",reteref)

! leggo tutte le stazioni presenti in archivio
    ier=idba_quantesono(handle,nstaz)
    print*,'massimo numero stazioni e pseudo-stazioni ',nstaz
    if(nstaz > MNSTAZ)then
        print*,'SONO TANTE ',nstaz,' STAZIONI!! SEI SICURO/A?'
    endif
!------------------------------------

! allocazione matrici
    ALLOCATE(anaid(1:nstaz))

! provvisorio!
! lettura punti (staz o pseudostaz) su cui fare la verifica da db
!------------------------------------------- 
! leggo tutte le stazioni presenti in db
    PRINT*,'lselect= ',lselect
    CALL leggiana_db_scores(iana,anaid, &
     itipost,rmdo,nstaz,handle,lselect)
    PRINT*,'numero massimo stazioni ',nstaz
!----------------------------------------

! cosi' diventera' una volta risolto il problema nella nuova subroutine
! lettura punti (staz o pseudostaz) su cui fare la verifica da db
!------------------------------------------- 
!    IF(lselect)THEN
! leggo solo le stazione selezionate dall'utente
!      CALL leggi_selstaz(anaid,nstaz,handle)
!      PRINT*,'numero massimo stazioni ',nstaz
!    ELSE
! leggo tutte le stazioni presenti in db
!      CALL leggiana_db_scores(iana,anaid, &
!       itipost,rmdo,nstaz,handle)
!      PRINT*,'numero massimo stazioni ',nstaz
!    ENDIF
!----------------------------------------

    nv=nstaz*ngio*nore
    if(nv > MNV)then
        print*,'attenzione!!! nv vale ',nv
        print*,'Forse i tuoi vettori sono un po'' troppo grandi!'
        print*,'Rischi di avere la memoria rovinata!'
        print*,'Puoi ridurre il periodo (ngio),'
        print*,'il numero delle ore al giorno (nore)'
        print*,'il numero di stazioni in archivio (nstaz)'
        print*,'o il numero di elementi dell''ensemble (nrm)'
!        stop
    endif

    print*,'numero di elementi dei vettori ',nv

    OPEN(1,file='date.nml',status='old')
    DO igio=1,ngio
      READ(1,nml=date,err=9004)
      IF(igio == 1)THEN
        igioi=data(1)
        imesei=data(2)
        iannoi=data(3)
      ENDIF
      IF(igio == ngio)THEN
        igiof=data(1)
        imesef=data(2)
        iannof=data(3)
      ENDIF
      iorai=nora/100
      imini=MOD(nora,100)
    ENDDO
    CLOSE(1)
      
    open(66,file='bias_error_day.dat',status='unknown')
    open(11,file='bias_error.dat',status='unknown')
    open(10,file='cont_table.dat',status='unknown')
    open(20,file='scores_per_scad.dat',status='unknown')
    open(13,file='andam.dat',status='unknown')
    open(15,file='CentreOfGravity.dat',status='unknown')

    open(1,file='scadenze.nml',status='old')
    read(1,nml=scadenza,err=9003)
    write(11,*)' descrittore= ',descrfisso
    write(66,*)' descrittore= ',descrfisso
    write(10,*)' descrittore= ',descrfisso
    write(20,*)' descrittore= ',descrfisso
    write(13,*)' descrittore= ',descrfisso
    write(15,*)' descrittore= ',descrfisso

    WRITE(11,'(a,2(1x,i2),1x,i4,1x,a,2(1x,i2),1x,i4,a,2(1x,i2))')&
     &' periodo= ',igioi,imesei,iannoi,'-',igiof,imesef,iannof,' ora= ',iorai,imini
    WRITE(66,'(a,2(1x,i2),1x,i4,1x,a,2(1x,i2),1x,i4,a,2(1x,i2))')&
     &' periodo= ',igioi,imesei,iannoi,'-',igiof,imesef,iannof,' ora= ',iorai,imini
    WRITE(10,'(a,2(1x,i2),1x,i4,1x,a,2(1x,i2),1x,i4,a,2(1x,i2))')&
     &' periodo= ',igioi,imesei,iannoi,'-',igiof,imesef,iannof,' ora= ',iorai,imini
    WRITE(20,'(a,2(1x,i2),1x,i4,1x,a,2(1x,i2),1x,i4,a,2(1x,i2))')&
     &' periodo= ',igioi,imesei,iannoi,'-',igiof,imesef,iannof,' ora= ',iorai,imini
    WRITE(13,'(a,2(1x,i2),1x,i4,1x,a,2(1x,i2),1x,i4,a,2(1x,i2))')&
     &' periodo= ',igioi,imesei,iannoi,'-',igiof,imesef,iannof,' ora= ',iorai,imini

    write(11,*)' variabile= ',cvar
    write(66,*)' variabile= ',cvar
    write(10,*)' variabile= ',cvar
    write(20,*)' variabile= ',cvar
    write(13,*)' variabile= ',cvar
    write(15,*)' variabile= ',cvar
    write(13,'(2a)')' o ',' p '
    print*,'variabile ',cvar

! allocazione matrici
    ALLOCATE(oss(1:nv))
    ALLOCATE(lon(1:nv))
    ALLOCATE(lat(1:nv))
    ALLOCATE(prev(1:nv, 1:nrm))
    ALLOCATE(previ(1:nv))

    do iscad=1,nscad
        do is=1,4
            scad(is)=scadenze(is,iscad)
        enddo
        print*,'scadenza ',scad

    ! inizializzazione matrici
        oss = rmddb
        prev = rmddb
        previ = rmddb

        iscaddb=scad1+inc*(iscad-1)
        write(11,'(a,i3)')' scadenza= ',iscaddb
        write(11,'(4x,a3,8x,a5,8x,a5,7x,a6,9x,a4,9x,a4)') &
        'npo','maerr','mserr','rmserr','bias','stdv'
        write(13,'(a,i3)')' scadenza= ',iscaddb
        write(15,'(a,i3)')' scadenza= ',iscaddb
        WRITE(15,'(4x,a3,6x,a3,6x,a8)')'npo','cog','distmean'
        write(66,'(a,i3)')' scadenza= ',iscaddb
        write(66,'(3(1x,a3),2(8x,a5),7x,a6,9x,a4,9x,a4)') &
        'gio','ora','npo','maerr','mserr','rmserr','bias','stdv'
        print*,'scadenza ',iscaddb

        open(1,file='date.nml',status='old')
        do igio=1,ngio
            read(1,nml=date,err=9004)
        ! ora di inizio
            ora(1)=nora/100
            ora(2)=mod(nora,100)

        ! trovo data e ora di validita' della previsione
!            call JELADATA5(data(1),data(2),data(3), &
!            ora(1),ora(2),iminuti)
!            iminuti=iminuti+iscaddb*60
!            call JELADATA6(iday,imonth,iyear, &
!            ihour,imin,iminuti)

            dt1=datetime_new(year=data(3), month=data(2), day=data(1), hour=ora(1), minute=ora(2))
!            dt1=datetime_new(simpledate=data(3)//data(2)//data(1)//ora(1)//ora(2))
            td1=timedelta_new(minute=60*iscaddb)
            dt1=dt1+td1
            call getval(dt1, year=iyear, month=imonth, day=iday, hour=ihour, minute=imin)
            
            dataval(1)=iday
            dataval(2)=imonth
            dataval(3)=iyear
            oraval(1)=ihour
            oraval(2)=imin

            do iore=1,nore

                if(iore > 1)then
                ! trovo data e ora dell'emissione per ore successive (analisi)
                    ora(1)=ore(iore)/100
                    ora(2)=mod(ore(iore),100)
                    oraval(1)=ora(1)
                    oraval(2)=ora(2)
                endif

            ! lettura previsioni da database

            ! ricominciamo 
                ier=idba_unsetall(handle)

                ier=idba_set (handle,"year",dataval(3))
                ier=idba_set (handle,"month",dataval(2))
                ier=idba_set (handle,"day",dataval(1))
                ier=idba_set (handle,"hour",oraval(1))
                ier=idba_set (handle,"min",oraval(2))
                ier=idba_set (handle,"sec",0)

            ! conversione delle scadenze in secondi (e correzione scadenze sbagliate)
                call converti_scadenze(4,scad,scaddb)

                print*,'scad scaddb',scad,scaddb
                
! questa gestione delle scadenze è fatta rispetto al vettore scad scritto come
! lo è per i grib in formato GRIB1. Passando a GRIB2 scad sarà posta identica
! come sono scritte le scadenze in dballe 
                scadenzep: select case(scaddb(4))
                case(4) ! cumulata
                   pind=1
                   fctime=scaddb(3)
                   period=scaddb(3)-scaddb(2)
                case(0) ! istantanea
                   pind=254
                   if(scaddb(3)/=0)then
                      print*,'case 1 - p1= ',scaddb(2),' p2= ',scaddb(3)
                      call exit(1)
                   endif
                   fctime=scaddb(2)
                   period=0
                case(1) ! analisi inizializzata
                   pind=254
                   if(scaddb(2)/=0)then
                      print*,'case 1 - p1= ',scaddb(2),' p2= ',scaddb(3)
                      call exit(1)
                   endif
                   fctime=scaddb(2)
                   period=0
                case(2) ! prodotto valido in un periodo
!!!                   pind=205
                   pind=2
                   if(analisi == 0)then ! verifica forecast
                      fctime=scaddb(3)
                   elseif(analisi == 1)then ! verifica analisi
                      fctime=0
                   endif
                   period=scaddb(3)-scaddb(2)
                case(3) ! media
                   pind=0
                   fctime=scaddb(2)
                   period=scaddb(3)
                case(13) ! analisi di precipitazione
                   pind=1
                   print*,'controllo - verrebbe fctime= ',scaddb(2), &
                        ' period= ',scaddb(3)
                   fctime=scaddb(2)
                   period=scaddb(3)
                case default
                   print*,'indicatore scadenza non gestito'
                   call exit(1)
                end select scadenzep
                ier=idba_settimerange(handle,pind,fctime,period)

! gestione scadenze
! scad(4) (ksec1(18)) e' posto pari a 13 per esprimere le scadenze relative 
! a piogge analizzate: p1 e p2 vanno considerati all'indietro a partire da
! data e ora del GRIB (es: p1=0 e p2=1 significa andare indietro di 1 ora)

                ier=idba_set (handle,"var",cvar)

                do irm=1,nrm

                    IF(nrm == 1)THEN
                      descr=descrfisso
                    ELSEIF(nrm > 1) then
                      WRITE(cel,'(i3.3)')irm
                      descr=descrfisso(1:nlenvera(descrfisso)) &
                       //'el'//cel
                    ENDIF

                    ier=idba_set (handle,"rep_memo",descr)

                    print*,'prev ',descr,dataval,oraval,scaddb,cvar

                    ier=idba_voglioquesto (handle,N)
                ! print*,'numero di dati trovati',N
                    if(N == 0)then
                        print*,'pre - non ci sono dati'
                        print*,dataval,oraval
!                        goto 66
                        goto 99
                    else
                      PRINT*,'pre - numero di dati trovati ',N
                    endif

                    do idati=1,N

                        ier=idba_dammelo (handle,btable)
                    ! sara' da impostare mentre per ora e' solo richiesto
                        ier=idba_enqlevel(handle,leveltype1,l1,leveltype2,l2)

                    ! ier=idba_enq (handle,"mobile",mobile)

!                        ier=idba_enq (handle,"ana_id",icodice)
                        ier=idba_enq (handle,"lat",icodice%lat)
                        ier=idba_enq (handle,"lon",icodice%lon)
                        
                        
!mst  interrogo sezione anagrafica per avere l'altezza
!                        ier=idba_set (handleana,"ana_id",icodice)
                        ier=idba_set(handleana,"lat",icodice%lat)
                        ier=idba_set(handleana,"lon",icodice%lon)
                        ier=idba_quantesono(handleana,USTAZ)
                        ier=idba_elencamele (handleana)
                        ier=idba_enq (handleana,"height",h)

                        IF(iquota >= 0)THEN
                          IF(c_e_i(h))THEN
                            IF(iquota == 0)THEN !pianura
                              IF(h >= hlimite)goto20
                            ELSEIF(iquota == 1)THEN !montagna
                              IF(h < hlimite)goto20
                            ELSEIF(iquota > 1)THEN
                              PRINT*,'iquota non gestito ',iquota
                            ENDIF
                          ELSE
                            goto20
                          ENDIF
                        ENDIF

                        ipos=0
                        do i=1,nstaz
!                            if(icodice == anaid(i))then
                           if(icodice%lat == anaid(i)%lat .and. &
                                icodice%lon == anaid(i)%lon)then !!!
                              ipos=i
                           endif
                        enddo
                        IF(ipos == 0)goto20

                        ier=idba_enq (handle,btable,dato)

                        iv=ipos+nstaz*(igio-1)+nstaz*ngio*(iore-1)
                        prev(iv,irm)=dato
!                        print*,'prev ',iv,prev(iv,irm),icodice%lon,icodice%lat

                        20 continue

                    enddo         ! idati
                enddo            ! nrm

            ! lettura osservazioni da database

99              continue
                
                ier=idba_unsetall(handle)

                if(itipost == 0)then
                    ier=idba_set (handle,"priomin",0)
!                    ier=idba_set (handle,"priomax",99)
                    ier=idba_set (handle,"query","best")
!                    descr="oss"
                    descr=reteref
                    ier=idba_set (handle,"rep_memo",descr)
                elseif(itipost == 80)then
                    ier=idba_unset (handle,"query")
                    nlm=nlenvera(model)
                    descr='oss'//descrfisso((nlm+1):(nlm+5))
                    ier=idba_set (handle,"rep_memo",descr)
                elseif(itipost == 90)then
                    ier=idba_unset (handle,"query")
                    descr="analisi"
                    ier=idba_set (handle,"rep_memo",descr)
                endif

                ier=idba_set (handle,"year",dataval(3))
                ier=idba_set (handle,"month",dataval(2))
                ier=idba_set (handle,"day",dataval(1))
                ier=idba_set (handle,"hour",oraval(1))
                ier=idba_set (handle,"min",oraval(2))
                ier=idba_set (handle,"sec",0)

                scadenzeo: select case(scaddb(4))
                case(4) ! cumulata
                   pind=1
                   fctime=0
                   period=scaddb(3)-scaddb(2)
                case(0) ! istantanea
                   pind=254
                   if(scaddb(3)/=0)then 
                      print*,'case 0 - p1= ',scaddb(2),' p2= ',scaddb(3)
                      call exit(1)
                   endif
                   fctime=0
                   period=0
                case(1) ! analisi inizializzata
                   pind=254
                   if(scaddb(2)/=0)then
                      print*,'case 1 - p1= ',scaddb(2),' p2= ',scaddb(3)
                      call exit(1)
                   endif
                   fctime=0
                   period=0
                case(2) ! prodotto valido in un periodo
!!!                   pind=205
                   pind=2
                   fctime=0
                   period=scaddb(3)-scaddb(2)
                case(3) ! media
                   pind=0
                   fctime=scaddb(2)
                   period=scaddb(3)
                case(13) ! analisi di precipitazione
                   pind=1
                   print*,'controllo - verrebbe fctime= ', &
                        scaddb(2),' period= ',scaddb(3)
                   fctime=0
                   period=scaddb(3)-scaddb(2)
                case default
                   print*,'indicatore scadenza non gestito'
                   call exit(1)
                end select scadenzeo
                ier=idba_settimerange(handle,pind,fctime,period)

! in time range indicator speciale per le preci analizzate e' 13, ma in database
! deve essere comunque 4 

                ier=idba_set (handle,"var",cvar)

                PRINT*,'oss ',descr,dataval,oraval,cvar,pind,fctime,period

                ier=idba_voglioquesto (handle,N)
                if(N == 0)then
                    print*,'oss - non ci sono dati'
                    print*,dataval,oraval
                    goto 66
                else
                  PRINT*,'oss - numero di dati trovati ',N
                endif

                do idati=1,N

                    ier=idba_dammelo (handle,btable)
                ! sara' da impostare mentre per ora e' solo richiesto
                    ier=idba_enqlevel(handle,leveltype1,l1,leveltype2,l2)

!                    ier=idba_enq (handle,"ana_id",icodice)
                    ier=idba_enq (handle,"lat",icodice%lat)
                    ier=idba_enq (handle,"lon",icodice%lon)

!mst  interrogo sezione anagrafica per avere l'altezza
!                    ier=idba_set (handleana,"ana_id",icodice)
                    ier=idba_set (handleana,"lat",icodice%lat)
                    ier=idba_set (handleana,"lon",icodice%lon)
                    ier=idba_quantesono(handleana,USTAZ)
                    ier=idba_elencamele (handleana)
                    ier=idba_enq (handleana,"height",h)
                    
                    IF(iquota >= 0)THEN
                      IF(c_e_i(h))THEN
                        IF(iquota == 0)THEN !pianura
                          IF(h >= hlimite .OR. h < -900.)goto30
                        ELSEIF(iquota == 1)THEN !montagna
                          IF(h < hlimite .OR. h == REAL(imd))goto30
                        ELSEIF(iquota > 1)THEN
                          PRINT*,'iquota non gestito ',iquota
                        ENDIF
                      ELSE
                        goto30
                      ENDIF
                    ENDIF

                    ipos=0
                    do i=1,nstaz
                       if(icodice%lat == anaid(i)%lat .and. &
                            icodice%lon == anaid(i)%lon)then !!!
                            ipos=i
                        endif
                     enddo
                    IF(ipos == 0)goto30

                    ier=idba_enq (handleana,"lon",rlon)
                    ier=idba_enq (handleana,"lat",rlat)
                    ier=idba_enq (handle,btable,dato)

                    iv=ipos+nstaz*(igio-1)+nstaz*ngio*(iore-1)
                    if(iv > nv)then
                        print*,'iv maggiore di nv!'
                        stop
                    endif

                    oss(iv)=dato
                    lon(iv)=rlon
                    lat(iv)=rlat
!                    print*,'oss ',iv,oss(iv),rlon,rlat

                    30 continue

                enddo            ! idati

                if(daily)then
                ! calcolo degli scores giorno per giorno e per ora E per elemento!
                    lag=nstaz*(igio-1)+nstaz*ngio*(iore-1)
                    write(13,'(a,1x,i3)')'giorno',igio
                    do irm=1,nrm
                        do i=1,nstaz
                            previ(i)=prev(i+lag,irm)
                        enddo
                        IF(.NOT.ldir)THEN
                          CALL mae(nstaz,oss(1+lag),previ,nstaz, &
                           rmddb,rmdo,npo,maerr)
                          CALL mse(nstaz,oss(1+lag),previ,nstaz, &
                           rmddb,rmdo,npo,mserr,rmserr,stdv)
                          CALL bias(nstaz,oss(1+lag),previ,nstaz, &
                           rmddb,rmdo,npo,bi)
                        ELSE
                          CALL mae_dd(nstaz,oss(1+lag),previ,nstaz, &
                           rmddb,rmdo,npo,maerr)
                          CALL mse_dd(nstaz,oss(1+lag),previ,nstaz, &
                           rmddb,rmdo,npo,mserr,rmserr)
                          CALL bias_dd(nstaz,oss(1+lag),previ,nstaz, &
                           rmddb,rmdo,npo,bi)
                        ENDIF
                        WRITE(66,'(1x,i3,1x,i2,1x,i6,5(1x,f12.3))') &
                        igio,iore,npo,maerr,mserr,rmserr,bi,stdv
                        write(20,'(a8,i3)')' giorno= ',igio
                        if(lthr /= 0)then
                            call score_con_table(nstaz, &
                            oss(1+lag),previ, &
                            nstaz,nsoglie,soglie,iscaddb, &
                            rmddb,rmdo,lthr)
                        endif
                    enddo         !nrm
                endif

                66 continue

            enddo               !nore

        enddo                  !ngio
        close(1)               !naml date

    ! output degli scores

        PRINT*,'nstaz ',nstaz
        nv=nstaz*ngio*nore
        print*,'nv ',nv

        write(55,*)'scad ',iscaddb
 
    ! ottengo gli scores deterministici per ogni elemento
        do irm=1,nrm
            do i=1,nv
                previ(i)=prev(i,irm)
            enddo
        ! attenzione!!! Passo nv e non MNV perche' sono allocabili!
            IF(.NOT.ldir)THEN
              CALL mae(nv,oss,previ,nv,rmddb,rmdo,npo,maerr)
              CALL mse(nv,oss,previ,nv,rmddb,rmdo,npo,mserr,rmserr,stdv)
              CALL cgravity(nv,oss,previ,lon,lat,nv,rmddb,rmdo,npo,distmean,cog)
              CALL bias(nv,oss,previ,nv,rmddb,rmdo,npo,bi)
             ELSE
              CALL mae_dd(nv,oss,previ,nv,rmddb,rmdo,npo,maerr)
              CALL mse_dd(nv,oss,previ,nv,rmddb,rmdo,npo,mserr,rmserr)
              CALL bias_dd(nv,oss,previ,nv,rmddb,rmdo,npo,bi)
            ENDIF
            write(11,'(1x,i6,5(1x,f12.3))') &
            npo,maerr,mserr,rmserr,bi,stdv
            WRITE(15,'(1x,i6,2(1x,f10.3))')npo,cog,distmean
            write(20,'(a11,i3)')' scadenza= ',iscaddb
            if(lthr /= 0)then
                CALL score_con_table(nv,oss,previ,nv, &
                nsoglie,soglie,iscaddb,rmddb,rmdo,lthr)
            endif

            CALL splot(nv,oss,previ,nv,rmddb,rmdo,npo)
        enddo

    enddo                     !nscad

    close(3)
    close(2)
    close(10)
    close(11)
    close(20)
    close(13)
    close(15)
    close(66)

! chiusura database
    ier=idba_fatto(handle)
    ier=idba_fatto(handleana)
    ier=idba_arrivederci(idbhandle)

    stop
    9001 print*,'errore nella lettura della namelist parameters'
    stop
    9002 print*,'errore nella lettura della namelist stat'
    stop
    9003 print*,'errore nella lettura della namelist lista'
    stop
    9004 print*,'errore nella lettura della namelist date'
    stop
  end program ver_scores_dballe

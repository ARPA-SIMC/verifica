    program crea_obsmask

! c VERIFICA - crea_obsmask.f
! c programma per la produzione di un file grib che contiene una maschera
! c per le osservazioni. Ogni punto di griglia del grib in ingresso
! c riceve valore 1 se giace in una zona coperta da osservazioni, 0 altrimenti
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

    USE util_dballe

    parameter (MNBOX=150000)
    parameter (MIDIMG=1200000)
    integer :: kgrib(MIDIMG)
    REAL, ALLOCATABLE :: xgrid(:)
    character(80) :: rfile,ofile
    character(19) :: database,user,password
    REAL :: obm,rmdo,dist,alorot,alarot,slon1,slon2,slat1,slat2
    integer :: iana,nstaz,nbox
    logical :: ruota,area
    real, ALLOCATABLE :: x(:),y(:),alt(:)
    real :: xb(MNBOX),yb(MNBOX)
! grib fields
    integer :: ksec0(2),ksec1(104),ksec2(384),ksec3(2),ksec4(60)
    REAL :: psec2(384),psec3(2),dummy(1)

    integer :: debug = 1
    INTEGER :: handle,handle_err

    namelist  /obsmask/rfile,iana,ruota,dist
    namelist  /odbc/database,user,password

    data ksec0/2*0/
    data ksec1/104*0/
    data ksec2/384*0/
    data ksec3/2*0/
    data ksec4/60*0/
! ksec4(5)=0 per real data
    data kgrib/MIDIMG*0/
    data psec2/384*0./
    data psec3/2*0./

    data rmdo/-999.9/

    open(1,file='odbc.nml',status='old')
    read(1,nml=odbc,err=9001)
    close(1)
    open(1,file='obsmask.nml',status='old')
    read(1,nml=obsmask,err=9002)
    close(1)

! gestione degli errori
    call idba_error_set_callback(0,idba_default_error_handler,debug,handle_err)

! connessione con database
    call idba_presentati(idbhandle,database,user,password)

! apertura database in lettura
    call idba_preparati(idbhandle,handle,"read","read","read")

    ofile='obsmask.grib'

    area=.false.

! lettura grib allo scopo di avere MIDIMV (ksec4(1))
    iug=0
    call pbopen(iug,rfile,'r',ier)
    if(ier /= 0)goto9100
    call pbgrib(iug,kgrib,MIDIMG,idimg,ier)
    if(ier /= 0)goto9300
    call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
    dummy,size(dummy), &
    kgrib,MIDIMG,idimg,'J',ier)
    if(ier > 0)goto9500
    MIDIMV=ksec4(1)
    PRINT*,'MIDIMV= ',MIDIMV
    call pbclose(iug,ier)
    if(ier /= 0)goto9600

    ALLOCATE(xgrid(MIDIMV))

    iug=0
    idimg=MIDIMG
    psec3(2)=rmdo

    PRINT*,'apro file ',rfile
    call pbopen(iug,rfile,'r',ier)
    if(ier /= 0)goto9100
    PRINT*,'pbopen fatta ',ier

    KPR=0     !DEBUG PRINT SWITCH
    CALL SETPAR (KBIT,KNEG,KPR) !number of bit in computer word    
    kinlen=idimg*kbit/8

    call pbgrib(iug,kgrib,kinlen,koutlen,ier)
    if(ier == -1)goto9200
    if(ier /= 0)goto9300
    print*,'pbgrib fatta ',ier
    call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
    xgrid,MIDIMV,kgrib,MIDIMG,idimg,'D',ier)
    if(ier == -6)goto9400
    if(ier > 0)goto9500
    call pbclose(iug,ier)
    if(ier /= 0)goto9600

! leggo le coordinate di tutte le box del modello
    call leggibox(rfile,MNBOX,xb,yb,nbox,alorot,alarot, &
    ruota,area,slon1,slon2,slat1,slat2)
    print*,'numero totale box ',nbox
    if(ruota)then
        call rot_grib_LAMBO(alorot,alarot,rxeq,ryeq)
        cryeq=cos(ryeq*3.1415927/180.)
        sryeq=sin(ryeq*3.1415927/180.)
        do ib=1,nbox
            rxb=xb(ib)
            ryb=yb(ib)
            call rtlld(rxb,ryb,rxeq,cryeq,sryeq,arxb,aryb)
            xb(ib)=arxb
            yb(ib)=aryb
        enddo
    endif

    call idba_quantesono(handle,nstaz)
    print*,'massimo numero pseudo-stazioni ',nstaz

! allocazione matrici
    ALLOCATE(x(1:nstaz))
    ALLOCATE(y(1:nstaz))
    ALLOCATE(alt(1:nstaz))

! leggo tutte le stazioni disponibili in archivio
    call leggiana_db(iana,x,y,alt,rmdo,nstaz,handle)
    print*,'numero massimo stazioni ',nstaz

    idimv=ksec4(1)
    if(idimv /= nbox)then
        print*,'PERCHE''??????'
        stop
    endif
    do iv=1,idimv
        xm=xb(iv)
        ym=yb(iv)
        obm=0.
        do ist=1,nstaz
            if(sqrt((xm-x(ist))**2+(ym-y(ist))**2) < dist)then
                obm=1.
                goto111
            endif
        enddo
        111 xgrid(iv)=obm
    enddo

    call pbopen(iug,ofile,'w',ier)
    if(ier /= 0)goto9700
    call gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
    xgrid,MIDIMV,kgrib,MIDIMG,kword,'C',ier)
    if(ier /= 0)goto9800
    call pbwrite(iug,kgrib,ksec0(1),ier)
    if(ier < 0)goto9900
    call pbclose(iug,ier)
    if(ier < 0)goto9990

    stop
    9001 print *,"Errore durante la lettura della namelist odbc "
    stop
    9002 print *,"Errore durante la lettura della namelist obsmask "
    stop
    9100 print *,'Errore durante la pbopen ',ier
    stop
    9200 print *,'Termino per end of file (pbgrib) ',ier
    stop
    9300 print *,'Errore lettura grib (pbgrib) ',ier
    stop
    9400 print *,'GRDEMO : Pseudo-grib data found (gribex) ',ier
    stop
    9500 print*, 'Errore degribbing (gribex) ',ier
    stop
    9600 print *,'Errore durante la pbclose ',ier
    stop
    9700 print *,'Errore durante la pbopen (write) ',ier
    stop
    9800 print *,'Errore durante la gribex (write) ',ier
    stop
    9900 print *,'Errore durante la pbwrite ',ier
    stop
    9990 print *,'Errore durante la pbclose (write) ',ier
    stop
    end program

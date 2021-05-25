    PROGRAM ver_lettura

    USE util_dballe
    IMPLICIT NONE

    character(LEN=10) :: btable="",starbtable(12)
    CHARACTER(LEN=10) :: dati(12),d1,d2,d3
    real :: dato
! namelist variables
    character(512) :: database='',user='',password=''

    INTEGER :: handle,handleana,handle_err
    integer :: debug=1,i,ii,n,nn,idbhandle

    NAMELIST  /odbc/database,user,password
    NAMELIST /lettura/selperiod,yearmin,monthmin,daymin,yearmax,monthmax,daymax,&
     seldate,idata,iora,imin,selrep,rep_memo,selscad,pind,fctime,period,selvar,btable

    CHARACTER(20) :: name,rep_memo=""
    real :: lat,lon
    INTEGER :: height,mobile,BLOCK,ana_id
    integer :: leveltype1,leveltype2,l1,l2
    integer :: year,month,day,hour,min,sec

    LOGICAL :: selperiod=.FALSE.,seldate=.FALSE.,selrep=.FALSE.
    LOGICAL :: selscad=.FALSE.,selvar=.FALSE.
    INTEGER :: yearmin=2009,monthmin=01,daymin=01,yearmax=2009,monthmax=03,daymax=31
    INTEGER :: idata(3),iora=00,imin=00
    integer :: pind=1,fctime=0,period=0

    integer :: imd=9999

    integer :: ier

    PRINT*,'program lettura'

    open(1,file='odbc.nml',status='old')
    read(1,nml=odbc,err=9001)
    close(1)

    open(1,file='lettura.nml',status='old')
    read(1,nml=lettura,err=9001)
    close(1)

    PRINT*,selperiod,yearmin,monthmin,daymin,yearmax,monthmax,daymax,&
     seldate,idata,iora,imin,selrep,rep_memo,selscad,pind,fctime,period,selvar,btable

    open(11,file='lettura.out',status='unknown')
    open(21,file='lettura_metview.out',status='unknown')

    ier=idba_error_set_callback(0,C_FUNLOC(idba_default_error_handler),debug,handle_err)

    ier=idba_presentati(idbhandle,database)
    ier=idba_preparati(idbhandle,handle,"read","read","read")
    ier=idba_preparati(idbhandle,handleana,"read","read","read")

    IF(seldate)ier=idba_setdate (handle,idata(3),idata(2),idata(1),iora,imin,00)
    IF(selscad)ier=idba_settimerange(handle,pind,fctime,period)
    IF(selrep)ier=idba_set (handle,"rep_memo",rep_memo)
    IF(selvar)ier=idba_set(handle,"var",btable)

    d1='1000'
    d2='20090423'
    d3='06'

    WRITE(21,*)'#GEO'
    WRITE(21,*)'PARAMETER = 013023'
    WRITE(21,*)'#lat   long   level   date   time    value'
    WRITE(21,*)'#DATA'

    ier=idba_voglioquesto (handle,N)
    print*,"numero dei dati da recuperare ",N

    do i=1,N

        ier=idba_dammelo (handle,btable)

        ier=idba_enq (handle,"lat",lat)
        ier=idba_enq (handle,"lon",lon)

        ier=idba_enqlevel(handle,leveltype1,l1,leveltype2,l2)

        ier=idba_enqtimerange(handle,pind,fctime,period)

        ier=idba_enq (handle,"mobile",mobile)

        ier=idba_enq (handle,"year",year)
        ier=idba_enq (handle,"month",month)
        ier=idba_enq (handle,"day",day)
        ier=idba_enq (handle,"hour",hour)
        ier=idba_enq (handle,"min",min)
        ier=idba_enq (handle,"sec",sec)

        ier=idba_enq (handle,"rep_memo",rep_memo)
        ier=idba_enq (handle,btable,dato)

        ier=idba_enq (handle,"ana_id",ana_id)
       
! chiedo l'anagrafica 
        ier=idba_set (handleana,"ana_id",ana_id)
        ier=idba_quantesono (handleana,NN)
        ier=idba_elencamele (handleana)
        ier=idba_enq (handleana,"name",name)
        IF (.not. c_e_c(name)) name=''
        ier=idba_enq (handleana,"height",height)
!!$        PRINT*,name,height
        IF (.not. c_e_i(height)) height=imd
        ier=idba_enq (handleana,"block",BLOCK)
        IF (.not. c_e_i(block)) block=imd

        ier=idba_voglioancora (handle,NN)

        do ii=1,NN
           ier=idba_ancora (handle,starbtable(ii))
           ier=idba_enq(handle,starbtable(ii),dati(ii))
        end do
        
        WRITE(11,12) &
         name,lat,lon,height,rep_memo,year,month,day,hour,min,sec, &
         pind,fctime,period,leveltype1,l1,leveltype2,l2,btable,dato

        WRITE(21,*)lat,lon,d1,d2,d3,dato

     enddo
     ier=idba_fatto(handle)
     ier=idba_arrivederci(idbhandle)
     CLOSE(11)
     CLOSE(21)
     
     stop
     
12   FORMAT(a,2(1x,f9.2),1x,i4,1x,i3,1x,i4,5(1x,i2), &
          1x,i3,2(1x,i10),2(1x,i3,1x,i6),1x,a,1x,f7.1)
     
9001 print *,"Errore durante la lettura della namelist odbc"
     call exit (1)
     END PROGRAM ver_lettura

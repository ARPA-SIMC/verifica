    program leggi

    character(LEN=10) :: btable,starbtable(12)
    character(LEN=10) :: dati(12)
! character*10 dato
    real :: dato
    logical :: found,starfound
    external error_handle

! --------

    character(19) :: database,user,password
    integer :: handle,handle_err
    character(1000) :: messaggio
    logical :: debug
    data debug/.true./

    namelist  /odbc/database,user,password

    character(20) :: name
    real :: lat,lon
    integer :: height,rep_cod,mobile
    integer :: leveltype,l1,l2,pindicator,p1,p2
    integer :: year,month,day,hour,min,sec

! --------


    open(1,file='odbc.nml',status='old',readonly)
    read(1,nml=odbc,err=9001)
    close(1)

    call idba_error_set_callback(0,error_handle,debug,handle_err)

    call idba_presentati(idbhandle,database,user,password)
    call idba_preparati(idbhandle,handle,"read","read","read")

    call idba_unsetall (handle)

! Only return values with best priority
! call idba_setc (handle,"querybest","t")

    call idba_setc (handle,"var",'B12001')
! call idba_seti (handle,"rep_cod",1)
! call idba_seti (handle,"station",1)
! call idba_seti (handle,"year",2004)
! call idba_seti (handle,"month",9)
! call idba_seti (handle,"day",15)
! call idba_seti (handle,"hour",12)
! call idba_seti (handle,"min",0)
! call idba_seti (handle,"sec",0)


! Definiamo il box 7D

! call idba_seti (handle,"priomax",15)
! call idba_seti (handle,"priomin",10)

! call idba_setr (handle,"latmax",45.4)
! call idba_setr (handle,"lonmax",15.0)

! call idba_setr (handle,"latmin",35.2)
! call idba_setr (handle,"lonmin",10.3)

! call idba_seti (handle,"yearmax",2004)
! call idba_seti (handle,"monthmax",9)
! call idba_seti (handle,"daymax",10)
! call idba_seti (handle,"hourmax",12)
! call idba_seti (handle,"minumax",0)
! call idba_seti (handle,"secmax",0)

! call idba_seti (handle,"yearmin",2004)
! call idba_seti (handle,"monthmin",9)
! call idba_seti (handle,"daymin",10)
! call idba_seti (handle,"hourmin",12)
! call idba_seti (handle,"minumin",0)
! call idba_seti (handle,"secmin",0)

    call idba_voglioquesto (handle,N)
    print*,"numero dei dati da recuperare ",N

    do i=1,N

        call idba_dammelo (handle,btable)

        call idba_enqc (handle,"name",name)
        call idba_enqr (handle,"lat",lat)
        call idba_enqr (handle,"lon",lon)
        call idba_enqi (handle,"height",height)

        call idba_enqi (handle,"leveltype",leveltype)
        call idba_enqi (handle,"l1",l1)
        call idba_enqi (handle,"l2",l2)

        call idba_enqi (handle,"pindicator",pindicator)
        call idba_enqi (handle,"p1",p1)
        call idba_enqi (handle,"p2",p2)

        call idba_enqi (handle,"mobile",mobile)

        call idba_enqi (handle,"year",year)
        call idba_enqi (handle,"month",month)
        call idba_enqi (handle,"day",day)
        call idba_enqi (handle,"hour",hour)
        call idba_enqi (handle,"min",min)
        call idba_enqi (handle,"sec",sec)

        call idba_enqi (handle,"rep_cod",rep_cod)
        call idba_enqr (handle,btable,dato)

    ! call idba_voglioancora (handle,NN)

    ! do ii=1,NN
    ! call idba_ancora (handle,starbtable(ii))
    ! call idba_enqc(handle,starbtable(ii),dati(ii))
    ! end do

    ! print*,'----------------------------------------'
    ! print *,name,lat,lon,height,rep_cod
    ! print *,leveltype,l1,l2
    ! print *,pindicator,p1,p2
    ! print *,year,month,day,hour,min,sec
    ! print *,btable,dato,(starbtable(j),dati(j),j=1,nn)

        write(11,12) &
        name,lat,lon,height,rep_cod,year,month,day,hour, &
        min,sec,pindicator,p1,p2,leveltype,l1,l2,btable,dato

    enddo
    call idba_fatto(handle)
    call idba_arrivederci(idbhandle)

    stop

    12 FORMAT(a,2(1x,f9.2),1x,i4,1x,i3,1x,i4,5(1x,i2), &
    1x,i3,2(1x,i10),3(1x,i3),1x,a,1x,f7.1)

    9001 print *,"Errore durante la lettura della namelist odbc"
    call exit (1)
    end program



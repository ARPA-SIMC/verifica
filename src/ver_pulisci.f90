    program ver_pulisci

    character(19) :: database,user,password
    integer :: handle
    logical :: debug
    data debug/.true./
    external error_handle

    namelist  /odbc/database,user,password

    open(1,file='odbc.nml',status='old',readonly)
    read(1,nml=odbc)
    close(1)

! PREPARAZIONE DELL' ARCHIVIO
    print*,"database=",database

    call idba_error_set_callback(0,error_handle,debug,handle_err)

    call idba_presentati(idbhandle,database,user,password)

! richiesta completa cancellazione iniziale
    call idba_preparati(idhandle,handle, &
    "reuse","rewrite","rewrite")
    call idba_scopa(handle,"repinfo.csv")
    call idba_fatto(handle)

    stop
    end program

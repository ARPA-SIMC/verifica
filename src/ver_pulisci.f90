    program ver_pulisci

    INCLUDE "dballe/dballef.h"

! namelist variables
    character(LEN=19) :: database='',user='',password=''

    INTEGER :: handle,handle_err
    integer :: debug=1

    namelist  /odbc/database,user,password

    PRINT*,'program pulisci'

    open(1,file='odbc.nml',status='old')
    read(1,nml=odbc)
    close(1)

! PREPARAZIONE DELL' ARCHIVIO
    print*,"cancello database=",database

    call idba_error_set_callback(0,idba_default_error_handler,debug,handle_err)

    call idba_presentati(idbhandle,database,user,password)

! richiesta completa cancellazione iniziale
    call idba_preparati(idbhandle,handle, &
    "write","write","write")
    call idba_scopa(handle,"repinfo.csv")
    call idba_fatto(handle)

    call idba_arrivederci(idbhandle)


    stop
    end program

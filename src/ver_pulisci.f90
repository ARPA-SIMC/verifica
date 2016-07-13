    program ver_pulisci

    INCLUDE "dballe/dballeff.h"

! namelist variables
    character(LEN=512) :: database='',user='',password=''

    INTEGER :: handle,handle_err
    integer :: debug=1

    integer :: ier

    namelist  /odbc/database,user,password

    PRINT*,'program pulisci'

    open(1,file='odbc.nml',status='old')
    read(1,nml=odbc)
    close(1)

! PREPARAZIONE DELL' ARCHIVIO
    print*,"cancello database=",database

    ier=idba_error_set_callback(0,C_FUNLOC(idba_default_error_handler),debug,handle_err)

    ier=idba_presentati(idbhandle,database)

! richiesta completa cancellazione iniziale
    ier=idba_preparati(idbhandle,handle, &
    "write","write","write")
    ier=idba_scopa(handle,"repinfo.csv")
    ier=idba_fatto(handle)

    ier=idba_arrivederci(idbhandle)


    stop
    end program

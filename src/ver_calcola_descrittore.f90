    PROGRAM calcola_descrittore

! programma che genera automaticamente il descrittore
! del campo che si vuole verificare

    USE util_dballe

    CHARACTER(len=15) :: descr
! namelist variables
    CHARACTER(len=10) :: model=''
    INTEGER :: itipo=1,imod=0,ls=-1
    LOGICAL :: media=.FALSE.,massimo=.FALSE.,prob=.FALSE.,distr=.FALSE.
    REAL :: dxb=1.0,dyb=1.0

    namelist /ds/model,itipo,imod,ls, &
    media,massimo,prob,distr,dxb,dyb

    PRINT*,'program calcola_descrittore'

    open(1,file='caldescr.nml',status='old')
    read(1,nml=ds,err=9005)
    close(1)

    call descrittore(model,itipo,imod,ls,media,massimo,prob, &
    distr,dxb,dyb,descr)

    print*,'descrittore ',descr

    stop
    9005 print*,'errore lettura namelist'
    END PROGRAM calcola_descrittore

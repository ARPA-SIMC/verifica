    program calcola_descrittore

! programma che genera automaticamente il descrittore
! del campo che si vuole verificare

    integer ::   imod,ls,itipo
    logical ::   media,massimo,prob,distr
    real ::      dxb,dyb
    character model*10,descr*15

    namelist  /ds/model,itipo,imod,ls, &
    media,massimo,prob,distr,dxb,dyb

    open(1,file='caldescr.nml',status='old')
    read(1,nml=ds,err=9005)
    close(1)

    call descrittore(model,itipo,imod,ls,media,massimo,prob, &
    distr,dxb,dyb,descr)

    print*,'descrittore ',descr

    stop
    9005 print*,'errore lettura namelist'
    end program

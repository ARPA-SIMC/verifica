    program leggi

    INCLUDE "dballe/dballef.h"

    character(LEN=10) :: btable,starbtable(12)
    character(LEN=10) :: dati(12)
! character*10 dato
    real :: dato
    logical :: found,starfound

! --------

    character(19) :: database,user,password
    INTEGER :: handle,handleana,handle_err
    character(1000) :: messaggio
    integer :: debug = 1

    namelist  /odbc/database,user,password

    character(20) :: name
    real :: lat,lon
    INTEGER :: height,rep_cod,mobile,BLOCK,ana_id
    integer :: leveltype,l1,l2,pindicator,p1,p2
    integer :: year,month,day,hour,min,sec

! --------


    open(1,file='odbc.nml',status='old')
    read(1,nml=odbc,err=9001)
    close(1)

    open(11,file='lettura.out',status='unknown')

    call idba_error_set_callback(0,idba_default_error_handler,debug,handle_err)

    call idba_presentati(idbhandle,database,user,password)
    call idba_preparati(idbhandle,handle,"read","read","read")
    call idba_preparati(idbhandle,handleana,"read","read","read")

! Only return values with best priority
! call idba_set (handle,"query","best")

! call idba_set (handle,"var",'B12001')
! call idba_set (handle,"rep_cod",1)
! call idba_set (handle,"station",1)
! call idba_set (handle,"year",2004)
! call idba_set (handle,"month",9)
! call idba_set (handle,"day",15)
! call idba_set (handle,"hour",12)
! call idba_set (handle,"min",0)
! call idba_set (handle,"sec",0)


! Definiamo il box 7D

! call idba_set (handle,"priomax",15)
! call idba_set (handle,"priomin",10)

! call idba_set (handle,"latmax",45.4)
! call idba_set (handle,"lonmax",15.0)

! call idba_set (handle,"latmin",35.2)
! call idba_set (handle,"lonmin",10.3)

! call idba_set (handle,"yearmax",2004)
! call idba_set (handle,"monthmax",9)
! call idba_set (handle,"daymax",10)
! call idba_set (handle,"hourmax",12)
! call idba_set (handle,"minumax",0)
! call idba_set (handle,"secmax",0)

! call idba_set (handle,"yearmin",2004)
! call idba_set (handle,"monthmin",9)
! call idba_set (handle,"daymin",10)
! call idba_set (handle,"hourmin",12)
! call idba_set (handle,"minumin",0)
! call idba_set (handle,"secmin",0)

    call idba_voglioquesto (handle,N)
    print*,"numero dei dati da recuperare ",N

    do i=1,N

        call idba_dammelo (handle,btable)

        call idba_enq (handle,"lat",lat)
        call idba_enq (handle,"lon",lon)

        call idba_enq (handle,"leveltype",leveltype)
        call idba_enq (handle,"l1",l1)
        call idba_enq (handle,"l2",l2)

        call idba_enq (handle,"pindicator",pindicator)
        call idba_enq (handle,"p1",p1)
        call idba_enq (handle,"p2",p2)

        call idba_enq (handle,"mobile",mobile)

        call idba_enq (handle,"year",year)
        call idba_enq (handle,"month",month)
        call idba_enq (handle,"day",day)
        call idba_enq (handle,"hour",hour)
        call idba_enq (handle,"min",min)
        call idba_enq (handle,"sec",sec)

        call idba_enq (handle,"rep_cod",rep_cod)
        call idba_enq (handle,btable,dato)

        CALL idba_enq (handle,"ana_id",ana_id)
       
! chiedo l'anagrafica 
        CALL idba_set (handleana,"ana_id",ana_id)
        CALL idba_quantesono (handleana,NN)
        CALL idba_elencamele (handleana)
        CALL idba_enq (handleana,"name",name)
        CALL idba_enq (handleana,"height",height)
        CALL idba_enq (handleana,"block",BLOCK)

        call idba_voglioancora (handle,NN)

        do ii=1,NN
           call idba_ancora (handle,starbtable(ii))
           call idba_enq(handle,starbtable(ii),dati(ii))
        end do
        
        print*,'----------------------------------------'
        PRINT *,name,lat,lon,height,rep_cod,block
        print *,leveltype,l1,l2
        print *,pindicator,p1,p2
        print *,year,month,day,hour,min,sec
        print *,btable,dato,(starbtable(j),dati(j),j=1,nn)
        
        write(11,12) &
             name,lat,lon,height,rep_cod,year,month,day,hour, &
             min,sec,pindicator,p1,p2,leveltype,l1,l2,btable,dato
        
     enddo
     call idba_fatto(handle)
     call idba_arrivederci(idbhandle)
     close(11)

     stop
     
12   FORMAT(a,2(1x,f9.2),1x,i4,1x,i3,1x,i4,5(1x,i2), &
          1x,i3,2(1x,i10),3(1x,i3),1x,a,1x,f7.1)
     
9001 print *,"Errore durante la lettura della namelist odbc"
     call exit (1)
end program leggi



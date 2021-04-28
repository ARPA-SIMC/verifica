MODULE common_namelists

! here the namelist variables are declared
    PARAMETER (MNSCAD=72,MNSOG=10,MNRM=102)
! parameters.nml
    integer :: nora=0000,ngio=1,nscad=1,scad1=1,scad2=1,inc=1
    integer :: nvar=1,nrm=1,nore=1,ore(24)=0000
! date.nml
    integer :: data(3)=(/-1,-1,-1/)
! scadenze.nml
    integer :: scadenze(4,MNSCAD)=-1
! parametro.nml
    integer :: kvar(3,2)=-1
! stat.nml
    character(len=10) :: model=''
    integer :: itipo=1,iana=0,analisi=0
    LOGICAL :: diffh=.FALSE.,corrq=.FALSE.
    real :: diffmax=100.
    LOGICAL :: laree=.false.
    CHARACTER(len=512) :: shapefile=''
    CHARACTER(len=20) :: reportobs='',reportpre=''
! lista.nml
    CHARACTER(len=6) :: cvar=''
    CHARACTER(len=20) :: reteref=''
    INTEGER :: iquota=-1,lthr=0
    INTEGER :: nsoglie=1
    REAL :: hlimite=100.,soglie(MNSOG)=0.,distmean=1.
    LOGICAL :: daily=.FALSE.,ldir=.FALSE.,lselect=.FALSE.
!lista_ens.nml
    LOGICAL :: lwght=.FALSE.,lsprerr=.FALSE.
    integer :: nowght(MNRM)=1
    INTEGER :: nelsupens=102
! odbc.nml
    character(512) :: database='',user='',password=''

! here the namelists are declared
    NAMELIST /parameters/nora,ngio,nscad,scad1,scad2,inc, &
     nvar,nrm,nore,ore
    NAMELIST /date/DATA
    NAMELIST /scadenza/scadenze
    NAMELIST /parametro/kvar
    NAMELIST /stat/model,itipo,iana, &
         reportobs,reportpre, &
         analisi, &
         diffh,diffmax,corrq,laree,shapefile
    NAMELIST  /lista/cvar,reteref,iquota,hlimite,lthr,nsoglie,soglie,daily,ldir,lselect, &
     distmean
    NAMELIST /listaens/lwght,nowght,nelsupens,lselect,lsprerr
    NAMELIST /odbc/database,user,password

CONTAINS

END MODULE common_namelists

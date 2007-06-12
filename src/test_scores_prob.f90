PROGRAM test_scores_prob

PARAMETER(nstaz=2,ngio=2,nrm=3,nelsupens=3,nsoglie=3)

REAL :: oss(nstaz,ngio),prev(nstaz,ngio,nrm)
REAL :: rmddb,rmds
INTEGER :: ntot,nocc
REAL :: rps,rpss,outr,bs,rel,res,bss,roca,clarea
REAL :: soglie(nsoglie)
INTEGER :: wght(ngio,nrm)

DATA wght/1,1,1,1,1,1/
DATA soglie/1.,5.,20./
DATA oss/0.5,2.,10.,30./
DATA prev/0.5,2.,10.,30.,0.5,2.,10.,30.,0.5,2.,10.,30./ ! deterministica
!DATA prev/2.,2.,2.,2.,0.5,0.5,0.5,0.5,5.,5.,5.,5./ ! low res
!DATA prev/10.,30.,0.5,2.,30.,0.5,10.,2.,0.5,10.,30.,2./ ! high res

DO igio=1,ngio
  PRINT*,'igio= ',igio
  DO istaz=1,nstaz
    PRINT*,'istaz= ',istaz
    DO irm=1,nrm
      PRINT*,'irm= ',istaz,' oss ',oss(istaz,igio),' prev ',prev(istaz,igio,irm)
    ENDDO
  ENDDO
ENDDO

CALL ranked(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm,nsoglie, &
 nelsupens,rmddb,rmds,soglie,wght,rps,rpss)
CALL outrange(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
 nelsupens,rmddb,rmds,wght,outr)

WRITE(11,*)'RPS= ',rps,' RPSS ',rpss
WRITE(11,*)'OUTR= ',outr

WRITE(11,'(3x,a,4x,a,4x,a,5x,a,5x,a,5x,a,5x,a,5x,a,3x,a)')'thr','ntot','nocc','bs','rel','res','bss','roca','clarea'
DO iso=1,nsoglie
  CALL brier(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
   nelsupens,rmddb,rmds,soglie(iso),wght, &
   ntot,nocc,bs,rel,res,bss)
  CALL roc(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
   nelsupens,rmddb,rmds,soglie(iso),wght,ntot,nocc,roca)
  CALL costloss(nstaz,ngio,nrm,oss,prev,ngio,nstaz,nrm, &
   nelsupens,rmddb,rmds,soglie(iso),wght,ntot,nocc,clarea)
  PRINT*,soglie(iso),ntot,nocc, &
   bs,rel,res,bss, &
   roca,clarea,outr
  WRITE(11,'(1x,f5.1,2(2x,i6),6(2x,f6.3))')soglie(iso),ntot,nocc,bs,rel,res,bss,roca,clarea
ENDDO

END PROGRAM test_scores_prob

!***********************************************************************
! RISULTATI
!
! deterministica:
! RPS=     0.000000    
! OUTR=     0.000000    
!   thr    ntot    nocc     bs     rel     res     bss     roca   clarea
!   1.0       4       3   0.000   0.000   0.188   1.000   1.000   0.999
!   5.0       4       2   0.000   0.000   0.250   1.000   1.000   0.999
!  20.0       4       1   0.000   0.000   0.188   1.000   1.000   0.999
!
!low res:
! RPS=    0.3148148    
! OUTR=    0.5000000    
!   thr    ntot    nocc     bs     rel     res     bss     roca   clarea
!   1.0       4       3   0.194   0.007   0.000  -0.037   0.500   0.000
!   5.0       4       2   0.500   0.250   0.000  -1.000   0.500   0.000
!  20.0       4       1   0.250   0.063   0.000  -0.333   0.500   0.000
!
!high res:
! RPS=    0.3333333    
! OUTR=    0.2500000    
!   thr    ntot    nocc     bs     rel     res     bss     roca   clarea
!   1.0       4       3   0.167   0.000   0.021   0.111   0.667   0.098
!   5.0       4       2   0.500   0.333   0.083  -1.000   0.250   0.000
!  20.0       4       1   0.333   0.333   0.188  -0.778   0.000   0.000
!
!***********************************************************************

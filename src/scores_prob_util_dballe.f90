!****************************************************************************
! c VERIFICA - scores_prob_util_dballe.f
! c subroutines per il calcolo degli scores probabilistici
! c autore: Chiara Marsigli

! Copyright (C) 2004

! Questo programma è software libero; è lecito ridistribuirlo e/o
! modificarlo secondo i termini della Licenza Pubblica Generica SMR come
! pubblicata da ARPA SMR ; o la versione 1 della licenza o (a scelta)
! una versione successiva.
! Questo programma è distribuito nella speranza che sia utile, ma SENZA
! ALCUNA GARANZIA; senza neppure la garanzia implicita di
! COMMERCIABILITÀ o di APPLICABILITÀ PER UN PARTICOLARE SCOPO. Si veda
! la Licenza Pubblica Generica SMR per avere maggiori dettagli.
! Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
! Generica SMR insieme a questo programma; in caso contrario, la si può
! ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA) Servizio
! Idro-Meteorologico (SIM), Viale Silvani 6, 40122 Bologna, Italia

! ARPA-SIM
! Viale Silvani, 6 e 2/3 - 40122 Bologna
! URP Tel: + 39 051 6497511 Fax: + 39 051 6497501
! E-mail: urpsim@smr.arpa.emr.it
! Internet: http://www.arpa.emr.it/sim/
!****************************************************************************

  MODULE scores_prob_util_dballe

  CONTAINS
  
  SUBROUTINE brier(MNSTAZ,MNGIO,MNRM,obs,pred,ngio,nstaz,nfc, &
   nelsupens,rmddb,rmdo,thr,wght,loutput,ntot,nocc,bs,rel,res,bss,bssd)
  
  ! VERIFICA - scores_prob_util.f
  ! subroutine per il calcolo di Brier Score, Brier Skill Score,
  ! BS decomposition e Reliability Diagram
  ! calcolo anche il Debiased BSS (BSSD), seguendo Weigel et al., 2007
  
  PARAMETER (NK=10)
  REAL, INTENT(in) :: obs(MNSTAZ,MNGIO),pred(MNSTAZ,MNGIO,MNRM)
  INTEGER, INTENT(in) :: MNSTAZ,MNGIO,MNRM
  INTEGER, INTENT(in) :: ngio,nstaz,nfc,nelsupens
  REAL, INTENT(in) :: rmddb,rmdo,thr
  REAL :: ofreq(0:NK)
  INTEGER :: npu(0:NK)
  ! automatic array, vengono automaticamente allocati e deallocati 
  ! solo nella subroutine, ma la loro dimensione deve venire passata
  REAL :: obar(0:nelsupens),ybar(0:nelsupens) 
  INTEGER :: nbar(0:nelsupens)
  
  INTEGER, INTENT(in) :: wght(MNGIO,MNRM)
  REAL, INTENT(out) :: bs,rel,res,bss,bssd
  INTEGER, INTENT(out) :: ntot,nocc
  LOGICAL :: loutput
  
!    PRINT*,'subroutine brier',nfc
  
  oavg=0.
  ntot=0
  nocc=0
  
  DO ik=0,NK
    ofreq(ik)=0.
    npu(ik)=0
  ENDDO
  
  ! mi riconduco ad un super-ensemble completo
  nel=nelsupens
  
  DO k=0,nel
    obar(k)=0.
    ybar(k)=0.
    nbar(k)=0
  ENDDO
  
  bs=0.
  bscli=0.
  DO ig=1,ngio
!     print*,'igio',ig
    DO is=1,nstaz
!!!       if(ig == 1)then
!!!          print*,is,obs(is,ig)
!!!       endif
      ! check on missing data
      nmd=0
      ifc=1
      IF(obs(is,ig) == rmddb)then
!!!         print*,obs(is,ig)
!!!         print*,rmddb
         nmd=1
      endif
      DO WHILE(nmd.EQ.0.AND.ifc.LE.nfc)
        IF(pred(is,ig,ifc) == rmddb)nmd=1
        ifc=ifc+1
      ENDDO
      ! if missing data (nmd=1) no action is taken
      IF(nmd == 0)THEN
        kobs=0
        IF(obs(is,ig) > thr)kobs=1
        IF(obs(is,ig) > thr)print*,is,obs(is,ig)
        kpred=0
        DO ifc=1,nfc
          IF(pred(is,ig,ifc) > thr)kpred=kpred+wght(ig,ifc)
        ENDDO
! controllo coppie        
!        print*,'obs',obs(is,ig),'pred',pred(is,ig,9)
        ! computation of brier score
        p=REAL(kpred)/REAL(nel)
        o=REAL(kobs)
!        print*,'threshold',thr,'obs frequency',o,'pred probability',p
!        print*,ig,is,o,p
        bs=bs+(p-o)**2
        ntot=ntot+1
        nocc=nocc+kobs
        ! reliability diagram
        kprob=NINT(p*10.)
!        print*,'controllo ','p',p,'o',o,'kprob',kprob
        ofreq(kprob)=ofreq(kprob)+o
        npu(kprob)=npu(kprob)+1
        ! brier components
        obar(kpred)=obar(kpred)+o
        nbar(kpred)=nbar(kpred)+1
      ENDIF
    ENDDO                  !nstaz
!    print*,'ntot=',ntot
  ENDDO                     !ngio
  
  WRITE(24,'(2(1x,a))')'forecast','observed'
  DO ik=0,NK
    IF(npu(ik) > 0)THEN
      ofreq(ik)=ofreq(ik)/REAL(npu(ik))
    ELSE
      ofreq(ik)=rmdo
    ENDIF
    IF(loutput)WRITE(24,'(2(2x,f7.2))')REAL(ik)/10.,ofreq(ik)
  ENDDO
  
  IF(ntot /= 0)THEN
    bs=bs/REAL(ntot)
    
    DO k=0,nel
      IF(nbar(k) > 1)obar(k)=obar(k)/REAL(nbar(k))
      ybar(k)=REAL(k)/REAL(nel)
    ENDDO
    
    oavg=REAL(nocc)/REAL(ntot)
    unc = oavg*(1.-oavg)
    res = 0.
    rel = 0.
    DO k=0,nel
      res=res+((obar(k)-oavg)**2)*REAL(nbar(k))
      rel=rel+((ybar(k)-obar(k))**2)*REAL(nbar(k))
    ENDDO
    res=res/REAL(ntot)
    rel=rel/REAL(ntot)
        
  ELSE
    bs=rmdo
    rel=rmdo
    res=rmdo
  ENDIF

! check
  bscheck=rel-res+unc
!  print*,'bs=',bs,'bscheck=',bscheck
  
  IF(ntot /= 0 .and. nocc /= 0)THEN
    bscli=REAL(nocc)/REAL(ntot)*(1.-REAL(nocc)/REAL(ntot))
    IF(bscli /= 0.)THEN
      bss=(bscli-bs)/bscli
      bssd=1-(bs/(bscli+(1/real(nel)*bscli)))
    ELSE
      bss=rmdo
      bssd=rmdo
    ENDIF
  ELSE
    bss=rmdo
    bscli=rmdo
  ENDIF

  IF(loutput)THEN
     WRITE(14,'(/1x,10hTHRESHOLD=,f6.1)') thr
     WRITE(14,'(1x,5hNTOT=,i6,7h  NOCC=,i6,8h  P_CLI=,f10.3)')ntot,nocc,bscli
     WRITE(14,'(1x,3hBS=,f13.6)') bs
     WRITE(14,'(1x,4hrel=,f13.6)') rel
     WRITE(14,'(1x,4hres=,f13.6)') res
     WRITE(14,'(1x,4hunc=,f13.6)') unc
     WRITE(14,'(1x,5hobar=,f13.6)') oavg
     WRITE(14,'(1x,4hBSS=,f13.6)') bss
     WRITE(14,'(1x,5hBSSD=,f13.6)') bssd
  ENDIF

  RETURN
  END SUBROUTINE brier
    
!**************************************************************************************
    SUBROUTINE brier_prob(MNSTAZ,MNGIO,MNRM,obs,pred,ngio,nstaz,nfc, &
     nelsupens,rmddb,rmdo,thr,wght,loutput,ntot,nocc,bs,bss)

! c VERIFICA - scores_prob_util.f
! c subroutine per il calcolo di Brier Score, Brier Skill Score e BS decomposition
    
    REAL, INTENT(in) :: obs(MNSTAZ,MNGIO),pred(MNSTAZ,MNGIO,MNRM)
    INTEGER, INTENT(in) :: MNSTAZ,MNGIO,MNRM
    INTEGER, INTENT(in) :: ngio,nstaz,nfc,nelsupens
    REAL, INTENT(in) :: rmddb,rmdo,thr
    INTEGER, INTENT(in) :: wght(MNGIO,MNRM)
    REAL :: o,p
    REAL, INTENT(out) :: bs,bss,nocc
    INTEGER, INTENT(out) :: ntot
    LOGICAL :: loutput

!    PRINT*,'subroutine brier_prob',nfc

    ntot=0
    nocc=0.

! mi riconduco ad un super-ensemble completo
    nel=nelsupens

    bs=0.
    bscli=0.
    do ig=1,ngio
        do is=1,nstaz
        ! check on missing data
            nmd=0
            ifc=1
            if(obs(is,ig) == rmddb)nmd=1
            do while(nmd.eq.0.and.ifc.le.nfc)
                if(pred(is,ig,ifc) == rmddb)nmd=1
                ifc=ifc+1
            enddo
        ! if missing data (nmd=1) no action is taken
            if(nmd == 0)then
                o=obs(is,ig)/10.
                p=0.
                do ifc=1,nfc     !numero di membri effettivi
                    p=p+pred(is,ig,ifc)/10.
                enddo
                p=p/real(nfc)
            ! computation of brier score
                bs=bs+(p-o)**2
                ntot=ntot+1
                nocc=nocc+o
            endif
        enddo                  !nstaz
    enddo                     !ngio

    if(ntot /= 0)then
        bs=bs/real(ntot)
        bscli=nocc/real(ntot)*(1.-nocc/real(ntot))
        if(bscli /= 0.)then
            bss=(bscli-bs)/bscli
        else
            bss=rmdo
        endif

        IF(loutput)THEN
          WRITE(14,'(/1x,10hTHRESHOLD=,f6.1)') thr
          WRITE(14,'(1x,5hNTOT=,i6,7h  NOCC=,f9.2,8h  P_CLI=,f10.3)') &
           ntot,nocc,bscli
          WRITE(14,'(1x,3hBS=,f13.6)') bs
          WRITE(14,'(1x,4hBSS=,f13.6)') bss
        ENDIF
    else
        bs=rmdo
        bscli=rmdo
        bss=rmdo
    endif

    return
    end subroutine brier_prob

!**************************************************************************************

    SUBROUTINE roc(MNSTAZ,MNGIO,MNRM,obs,pred,ngio,nstaz,nfc, &
     nelsupens,rmddb,rmdo,thr,wght,loutput,ntot,nocc,roca)

! c VERIFICA - scores_prob_util.f
! c subroutine per il calcolo della ROC area e delle ROC curves
    
    REAL, INTENT(in) :: obs(MNSTAZ,MNGIO),pred(MNSTAZ,MNGIO,MNRM)
    INTEGER, INTENT(in) :: MNSTAZ,MNGIO,MNRM
    INTEGER, INTENT(in) :: ngio,nstaz,nfc,nelsupens
    REAL, INTENT(in) :: rmddb,rmdo,thr
    INTEGER, INTENT(in) :: wght(MNGIO,MNRM)
    INTEGER :: nobs(2)
    INTEGER :: kobs(0:nelsupens,2),nprev(0:nelsupens)
    REAL :: hit(0:nelsupens),far(0:nelsupens),fareck(0:nelsupens)
    REAL, INTENT(out) :: roca
    INTEGER, INTENT(out) :: ntot,nocc
    LOGICAL :: loutput

!    PRINT*,'subroutine roc',nfc

! mi riconduco ad un super-ensemble completo
    nel=nelsupens

! initialisation
    do kclass=1,2
        nobs(kclass)=0
        do kpred=0,nel
            kobs(kpred,kclass)=0
            nprev(kpred)=0
        enddo
    enddo
    ncont=0

    do ig=1,ngio
        do is=1,nstaz
        ! check on missing data
            nmd=0
            ifc=1
            if(abs(obs(is,ig)-rmddb) < 1.0E-4)nmd=1
            do while(nmd.eq.0.and.ifc.le.nfc)
                if(abs(pred(is,ig,ifc)-rmddb) < 1.0E-4)nmd=1
                ifc=ifc+1
            enddo
        ! kpred prob class definition
        ! nobs(1)   total number of occurrences
        ! nobs(2)   total number of non-occurrences
        ! kobs(.,1) number of occurrences per prob. class
        ! kobs(.,2) number of non-occurrences per prob. class
        ! if missing data (nmd=1) no action is taken
            if(nmd == 0)then
                kpred=0
                do ifc=1,nfc
                    if(pred(is,ig,ifc) > thr)then
                        kpred=kpred+wght(ig,ifc)
                    endif
                enddo
                nprev(kpred)=nprev(kpred)+1
                ncont=ncont+1
                if(obs(is,ig) > thr)then
                    nobs(1)=nobs(1)+1
                    kobs(kpred,1)=kobs(kpred,1)+1
                else
                    nobs(2)=nobs(2)+1
                    kobs(kpred,2)=kobs(kpred,2)+1
                endif
            endif
        enddo                  !nstaz
    enddo                     !ngio

!==================================================
! calcolo del BS; fabrizio e chiara
    if(nobs(1) /= 0 .AND. nobs(2) /= 0)then
    ! computation of false alarm rate and hit rate
    ! hit rate = number of occur. per prob. class / total number of occur.
    ! fa rate = number of non-occur. per prob. class / total number of non-occur.
        do kpred=0,nel
            hit(kpred)=real(kobs(kpred,1))/real(nobs(1))
            far(kpred)=real(kobs(kpred,2))/real(nobs(2))
        enddo
        bs = 0.
        do k = 0,nel
            h = hit(k)
            f = far(k)
        ! write(33,'(1x,2(a,f13.6),1x,a,i8)')'far ',f,' hit ',h,
        ! $           'nprev',nprev(k)
            w = real(k)/real(nel)
            bs = bs + obar*h*(1.-w)**2+(1.-obar)*f*w**2
        enddo
    ! print *,'bs = ',bs
    endif
!==================================================

    if(nobs(1) /= 0 .AND. nobs(2) /= 0)then
    ! computation of false alarm rate and hit rate
    ! hit rate = number of occur. per prob. class / total number of occur.
    ! fa rate = number of non-occur. per prob. class / total number of non-occur.
        roca=0.
        prevfar=1.
        prevhit=1.
        do kpred=0,nel
            hit(kpred)=0.
            far(kpred)=0.
            do ipred=kpred,nel
                hit(kpred)=hit(kpred)+ &
                real(kobs(ipred,1))/real(nobs(1))
                far(kpred)=far(kpred)+ &
                real(kobs(ipred,2))/real(nobs(2))
            enddo
            roca=roca+.5*(hit(kpred)+prevhit)*(prevfar-far(kpred))
            prevfar=far(kpred)
            prevhit=hit(kpred)
            if(nprev(kpred) /= 0)then
                fareck(kpred)=real(kobs(kpred,2))/real(nprev(kpred))
            else
                fareck(kpred)=rmdo
            endif
        enddo
    ! output far and hit
        IF(loutput)THEN
          WRITE(12,'(/1x,10hTHRESHOLD=,f6.1)') thr
          WRITE(12,'(1x,5hNTOT=,i6,7h  NOCC=,i6,8h  P_CLI=,f15.8)') &
           nobs(1)+nobs(2),nobs(1), &
           REAL(nobs(1)*nobs(2))/REAL((nobs(1)+nobs(2))**2)
          WRITE(12,'(1x,5hAREA=,f10.3)')roca
          WRITE(12,'(//8x,a,8x,a,5x,a,8x,a/)') &
           'far','hit','fareck','nprev'
          DO kpred=0,nel
            WRITE (12,'(3(1x,f10.3),1x,i8)')far(kpred),hit(kpred) &
             ,fareck(kpred),nprev(kpred)
          ENDDO
        ENDIF
    ! write(13,'(1x,a,1x,f5.1)')'thr',thr
    ! do kpred=0,nel
    ! write (13,'((1x,i3,1x,f10.3))')
    ! $           kpred,real(nprev(kpred))/real(ncont)
    ! enddo
    else
        roca=rmdo
    endif
    ntot=nobs(1)+nobs(2)
    nocc=nobs(1)

    return
    end subroutine roc

!***********************************************************************************

    SUBROUTINE terr(MNSTAZ,MNRM,obs,pred,nstaz,nrm, &
     nelsupens,rmddb,rmdo,wght,loutput,ipos)

! c VERIFICA - scores_prob_util.f
! c subroutine per il calcolo del total error di ogni membro

    REAL, INTENT(in) :: obs(MNSTAZ),pred(MNSTAZ,MNRM)
    INTEGER, INTENT(in) :: MNSTAZ,MNRM
    INTEGER, INTENT(in) :: nstaz,nrm,nelsupens
    REAL, INTENT(in) :: rmddb,rmdo
    INTEGER, INTENT(inout) :: wght(MNRM)
!    INTEGER :: num(nelsupens)
    INTEGER, ALLOCATABLE :: num(:)
    INTEGER, INTENT(out) :: ipos
    LOGICAL :: loutput
    
    ALLOCATE(num(1:nelsupens))
    
    npu=0
    err=0.
    errmin=9999.
    do irm=1,nrm
        num(irm)=irm
        npu=0
        do is=1,nstaz
            if(obs(is) /= rmddb .AND. &
            pred(is,irm) /= rmddb)then
                npu=npu+1
                err=err+abs(obs(is)-pred(is,irm))
            endif
        enddo
        if(npu > 0)then
            err=err/real(npu)
        else
            err=9999.
        endif
        if(err < errmin)then
            errmin=err
            irmmin=irm
        endif
        IF(loutput)WRITE(22,'(a,i2,a,i3,a,f8.3)') &
         'elemento= ',irm,' peso= ',wght(irm), &
         ' terr= ',err
    enddo

    IF(loutput)WRITE(22,'(a,f8.3,a,i2,a,i2)') &
     'errmin= ',errmin,' irmmin= ',irmmin,' num ',num(irmmin)

    do irm=1,nrm
        do jrm=irm+1,nrm
            if(wght(jrm) > wght(irm))then
                tmp=num(irm)
                num(irm)=num(jrm)
                num(jrm)=tmp
                tmp=wght(irm)
                wght(irm)=wght(jrm)
                wght(jrm)=tmp
            endif
        enddo
    enddo

    do irm=1,nrm
        if(irmmin == num(irm))ipos=irm
    enddo

    IF(loutput)WRITE(22,'(a,f8.3,a,i2,a,i2)') &
     'errmin= ',errmin,' irmmin= ',irmmin,' num ',ipos

    RETURN
    END SUBROUTINE terr
    
!***********************************************************************************

    SUBROUTINE costloss(MNSTAZ,MNGIO,MNRM,obs,pred,ngio,nstaz,nfc, &
     nelsupens,rmddb,rmdo,thr,wght,loutput,ntot,nocc,area)

! c VERIFICA - scores_prob_util.f
! c subroutine per il calcolo delle Cost-loss curves e della Cost-loss area

    PARAMETER (NCL=1000)
    REAL, INTENT(in) :: obs(MNSTAZ,MNGIO),pred(MNSTAZ,MNGIO,MNRM)
    INTEGER, INTENT(in) :: MNSTAZ,MNGIO,MNRM
    INTEGER, INTENT(in) :: ngio,nstaz,nfc,nelsupens
    REAL, INTENT(in) :: rmddb,rmdo,thr
    INTEGER, INTENT(in) :: wght(MNGIO,MNRM)
    INTEGER :: nobs(2)
    REAL :: omed
    REAL :: me,mecli,mep
    REAL :: enve(0:NCL)    
    INTEGER :: kobs(0:nelsupens,2),npre(0:nelsupens)
    REAL :: hit(0:nelsupens),far(0:nelsupens)
    REAL :: ks(0:nelsupens)
    REAL, ALLOCATABLE :: val(:,:)
    REAL, INTENT(out) :: area
    INTEGER, INTENT(out) :: ntot,nocc
    LOGICAL :: loutput
    
!    PRINT*,'subroutine cost-loss ',nfc

    ALLOCATE(val(0:nelsupens,0:NCL))

! mi riconduco ad un super-ensemble completo
    nel=nelsupens

! initialisation
    do kclass=1,2
        nobs(kclass)=0
        do kpred=0,nel
            kobs(kpred,kclass)=0
            npre(kpred)=0
            do icl=0,NCL
                val(kpred,icl)=0.
            enddo
        enddo
    enddo
    fact=1./real(NCL)

    do ig=1,ngio
        do is=1,nstaz
        ! check on missing data
            nmd=0
            ifc=1
            if(abs(obs(is,ig)-rmddb) < 1.0E-4)nmd=1
            do while(nmd.eq.0.and.ifc.le.nfc)
                if(abs(pred(is,ig,ifc)-rmddb) < 1.0E-4)nmd=1
                ifc=ifc+1
            enddo
        ! if missing data (nmd=1) no action is taken
        ! kpred     probability class definition
        ! nobs(1)   total number of occurrences
        ! nobs(2)   total number of non-occurrences
        ! kobs(.,1) number of occurrences per prob. class
        ! kobs(.,2) number of non-occurrences per prob. class
            if(nmd == 0)then
                kpred=0
                do ifc=1,nfc
                    if(pred(is,ig,ifc) > thr)then
                        kpred=kpred+wght(ig,ifc)
                    endif
                enddo
                npre(kpred)=npre(kpred)+1
                if(obs(is,ig) > thr)then
                    nobs(1)=nobs(1)+1
                    kobs(kpred,1)=kobs(kpred,1)+1
                else
                    nobs(2)=nobs(2)+1
                    kobs(kpred,2)=kobs(kpred,2)+1
                endif
            endif
        enddo                  !nstaz
    enddo                     !ngio

    if(nobs(1) /= 0 .AND. nobs(2) /= 0)then
    ! computation of false alarm rate and hit rate
    ! hit rate = number of occur. per prob. class / total number of occur.
    ! fa rate = number of non-occur. per prob. class / total number of non-occur.
        do kpred=0,nel
            na=0
            nb=0
            hit(kpred)=0.
            far(kpred)=0.
            ks(kpred)=0.
            do ipred=kpred,nel
                na=na+kobs(ipred,1)
                nb=nb+kobs(ipred,2)
                hit(kpred)=hit(kpred)+ &
                real(kobs(ipred,1))/real(nobs(1))
                far(kpred)=far(kpred)+ &
                real(kobs(ipred,2))/real(nobs(2))
            enddo
        ! print*,'kpred ',kpred
        ! abcd=real(nobs(1)+nobs(2))
        ! print*,'a ',real(na)/abcd,' b ',real(nb)/abcd
        ! print*,'c ',real(nobs(1)-na)/abcd,
        ! $           ' d ',real(nobs(2)-nb)/abcd
        ! print*,' a+c ',nobs(1),' b+d ',nobs(2)
        enddo
        omed=real(nobs(1))/real(nobs(1)+nobs(2))

    ! Calcolo delle curve di cost-loss
    ! ciclo sulle classi di probabilita' (sulle p*)
        fact=1./real(NCL)
        do kpred=0,nel
            ks(kpred)=hit(kpred)-far(kpred)
        ! ciclo sui valori di C/L
            do icl=0,NCL
                cl=real(icl)*fact
                mep=omed*cl
                mecli=min(cl,omed)
                me=omed+far(kpred)*cl*(1.-omed)-hit(kpred)*omed*(1.-cl)
                me=max(0.,me)     ! aggiungo per problemi di arrotondamento
                val(kpred,icl)=max(0.,mecli-me)/max(mecli-mep,1.E-10)
            enddo
        enddo

    ! calcolo della curva envelope di tutte le altre
        area=0.
        do icl=0,NCL
            enve(icl)=val(0,icl)
            do kpred=1,nel
                enve(icl)=max(val(kpred,icl),enve(icl))
            enddo
            area=area+2.*enve(icl)
        enddo
        area=0.5*fact*(area-enve(0)-enve(NCL))

    ! output di controllo
    ! write(18,'(1x,a,f6.1,a,i7,a,f7.5)')'THR = ',thr,
    ! $        ' NOBS = ',nobs(1),' OMED = ',omed
    ! write(18,'(1x,a,5(5x,a))')'PCL','   NPRE','   NOSS',
    ! $        '  NNOSS','  HIT','  FAR'
    ! do kpred=0,nel
    ! write(18,'(1x,i3,3(5x,i7),2(5x,f9.6))')kpred,npre(kpred),
    ! $           kobs(kpred,1),kobs(kpred,2),hit(kpred),far(kpred)
    ! enddo
    ! roc curve
    ! do kpred=0,nel
    ! write(25,'(2(1x,f5.3))')hit(kpred),far(kpred)
    ! enddo

    ! output per la grafica
        IF(loutput)THEN
          DO icl=0,NCL
            WRITE(23,'(2(1x,e15.3))') &
             REAL(icl)*fact,enve(icl)
        ! o in uno stesso file
        ! write(17,'(52(1x,f9.6),1x,e15.3)')
        ! $              real(icl)*fact,
        ! $              (val(kpred,icl),
        ! $              kpred=1,nel),
        ! $              enve(icl)
          ENDDO
        ENDIF

    ! fine controllo sulle osservazioni
    else
        area=rmdo
    ! output per la grafica
        IF(loutput)THEN
          DO icl=0,NCL
            WRITE(23,'(2(1x,e15.3))') &
             REAL(icl)*fact,rmdo
          ENDDO
        ENDIF
    endif

    ntot=nobs(1)+nobs(2)
    nocc=nobs(1)

    DEALLOCATE(val)

    return
    end subroutine costloss

!***********************************************************************************

    SUBROUTINE outrange(MNSTAZ,MNGIO,MNRM,obs,pred,ngio,nstaz,nfc, &
     rmddb,rmdo,wght,loutput,outr)

! c VERIFICA - scores_prob_util.f
! c subroutine per il calcolo della Percentage of Outliers e dell'errore associato

    REAL, INTENT(in) :: obs(MNSTAZ,MNGIO)
    REAL, INTENT(in) :: pred(MNSTAZ,MNGIO,nfc)
    INTEGER, INTENT(in) :: MNSTAZ,MNGIO,MNRM
    INTEGER, INTENT(in) :: ngio,nstaz,nfc
    REAL, INTENT(in) :: rmddb,rmdo
    INTEGER, INTENT(in) :: wght(MNGIO,MNRM)
    REAL :: predor(nfc)
    INTEGER :: inter(0:nfc),rankno0(0:nfc),nzero,nvalid
    REAL, INTENT(out) :: outr
    REAL :: outr_rh
    LOGICAL :: loutput
    REAL :: harvest
    
    PRINT*,'subroutine outrange',nfc
    
    CALL init_random_seed()
 
! initialisation
    outr=0.
    pout=0.
    pin=0.
    poutn=0.
    pinn=0.
    poutx=0.
    pinx=0.
    errn=0.
    errx=0.
    kerrmaxn=1
    kerrmaxx=1
    do i=0,nfc
        inter(i)=0
        rankno0(i)=0
    enddo

    ntot=0
    nzero=0
    nvalid=0
    do ig=1,ngio
        do is=1,nstaz
        ! check on missing data
            nmd=0
            ifc=1
            if(abs(obs(is,ig)-rmddb) < 1.0E-6)nmd=1
            do while(nmd.eq.0.and.ifc.le.nfc)
                if(abs(pred(is,ig,ifc)-rmddb) < 1.0E-6)nmd=1
                ifc=ifc+1
            enddo
        ! if missing data (nmd=1) no action is taken
            if(nmd == 0)then
                ntot=ntot+1
                predn=pred(is,ig,1)
                predx=pred(is,ig,1)
                do ifc=2,nfc
                    if(pred(is,ig,ifc) < predn)then
                        predn=pred(is,ig,ifc) !previsto minimo
                    endif
                    if(pred(is,ig,ifc) > predx)then
                        predx=pred(is,ig,ifc) !previsto massimo
                    endif
                enddo
!                print*,'previsti min e max',predn,predx
            ! controllo sia sopra il massimo sia sotto il minimo
                if(obs(is,ig) < predn .OR. obs(is,ig) > predx)then
                    pout=pout+1.  !outliers totali
                else
                    pin=pin+1.
                endif
            ! controllo solo sotto il minimo
                if(obs(is,ig) < predn)then
                    errn=errn+abs(obs(is,ig)-predn)
                    poutn=poutn+1. !outliers sotto il minimo
                    kerr=int(abs(obs(is,ig)-predn))+1
                    kerrmaxn=max(kerr,kerrmaxn)
                else
                    pinn=pinn+1.
                endif
            ! controllo solo sopra il massimo
                if(obs(is,ig) > predx)then
                    errx=errx+abs(obs(is,ig)-predx)
                    poutx=poutx+1. !outliers sopra il massimo
                    kerr=int(abs(obs(is,ig)-predx))+1
                    kerrmaxx=max(kerr,kerrmaxx)
                    if(kerr > 50)then
                        kerr=51
                    endif
                    if(kerr > 200)then
                        print*,'kerr ',kerr,' obs ' ,obs(is,ig), &
                        ' pred ',predx
                    endif
                else
                    pinx=pinx+1.
                endif
            ! ordino il vettore in senso crescente e posiziono l'osservazione
            ! negli intervalli
                do ifc=1,nfc
                    predor(ifc)=pred(is,ig,ifc)
                enddo
                call ordine(predor,nfc)
!                print*,'vettore ordinato',predor
!                print*,'osservato',obs(is,ig)

! method 1 - assign rank random in case of zeros
! the rank random is assigned at maximum between rank 2 and nfc-1, 
! excluding the two extreme bins (since in that case obs is NOT an outlier)

                if(obs(is,ig) < predor(1))inter(0)=inter(0)+1
!!!                if(obs(is,ig) >= predor(nfc))inter(nfc)=inter(nfc)+1
                if(obs(is,ig) > predor(nfc))inter(nfc)=inter(nfc)+1
                do ifc=1,nfc-1
                   if(obs(is,ig) >= predor(ifc) .AND. &
                        obs(is,ig) < predor(ifc+1))then
                      inter(ifc)=inter(ifc)+1
                   endif
                enddo
                if(obs(is,ig) == predor(nfc))then
                   inter(nfc-1)=inter(nfc-1)+1
                endif

                if(obs(is,ig) == 0.)then
                   do ifc=1,nfc
                      if(predor(ifc) == 0.)mrank=ifc
                   enddo
                   if(mrank==1)goto 333
                   CALL RANDOM_NUMBER(harvest)
                   if(mrank < nfc)then
                      irand=1+NINT(harvest*(mrank-1))
                      inter(irand)=inter(irand)+1
                   elseif(mrank == nfc)then
                      irand=1+NINT(harvest*(mrank-2))
                      inter(irand)=inter(irand)+1                      
                   endif
                endif
333             continue

! method 2 - exclude zeros

                if(obs(is,ig) == 0. .AND. predor(1) == 0. .AND. predor(nfc) == 0.)then
                   nzero=nzero+1
                else

                   nvalid=nvalid+1

                   if(obs(is,ig) < predor(1))rankno0(0)=rankno0(0)+1
                   if(obs(is,ig) > predor(nfc))rankno0(nfc)=rankno0(nfc)+1
                   do ifc=1,nfc-1
                      if(obs(is,ig) >= predor(ifc) .AND. &
                           obs(is,ig) < predor(ifc+1))then
                         rankno0(ifc)=rankno0(ifc)+1
                      endif
                   enddo
                   if(obs(is,ig) == predor(nfc))rankno0(nfc-1)=rankno0(nfc-1)+1
                endif
             endif
        enddo
    enddo
    nocc=int(pout+pin)

    print*,'ntot',ntot,'nocc',nocc

    IF(nocc /= 0)THEN
      outr=pout/(pout+pin)
      IF((poutn+pinn) > 0)THEN
        outrn=poutn/(poutn+pinn)
      ELSE
        outrn=rmdo
      ENDIF
      IF(poutn > 0.)THEN
        errn=errn/poutn
      ELSE
        errn=rmdo
      ENDIF
      IF((poutx+pinx) > 0)THEN
        outrx=poutx/(poutx+pinx)
      ELSE
        outrx=rmdo
      ENDIF
      IF(poutx > 0.)THEN
        errx=errx/poutx
      ELSE
        errn=rmdo
      ENDIF
    ELSE
      outr=rmdo
    ENDIF

    if(ntot /= 0)then
       outr_rh=inter(0)/REAL(ntot)+inter(nfc)/REAL(ntot)
    else
       outr_rh=rmdo
    endif

    IF(loutput)then
       WRITE(15,'(1x,a9,f6.3,2x,2(a9,f6.3,1x,a9,f7.3,2x))') &
     'OUTRANGE=',outr, &
     'OUTRAmin=',outrn,'errmedio=',errn, &
     'OUTRAmax=',outrx,'errmedio=',errx
    ! write(15,'(/1x,8(a5,f8.1,2x))')'pout',pout,'pin',pin,
    ! $     'poutn',poutn,'pinn',pinn,'poutx',poutx,'pinx',pinx
       WRITE(15,'(1x,a,2(a,i3))')'max class',' below min ',kerrmaxn, &
     ' above max ',kerrmaxx
       WRITE(15,'(1x,a,1x,f6.3/)')'OUTRANGE FROM RANK HISTOGRAM=',outr_rh
    ENDIF
    IF(ntot == 0)RETURN
    IF(nvalid == 0)RETURN
    IF(loutput)THEN
      DO i=0,nfc
        WRITE(16,'(1x,i3,1x,f7.5,1x,f7.5)')i,inter(i)/REAL(ntot),rankno0(i)/REAL(nvalid)
      ENDDO
    ENDIF
    print*,'nzero ',nzero,'nvalid ',nvalid

    return
    end subroutine outrange

!***********************************************************************************

    SUBROUTINE mode_population(MNSTAZ,MNGIO,MNRM,obs,pred,ngio,nstaz,nfc, &
     rmddb,rmdo,wght,loutput,succrate)

! c VERIFICA - scores_prob_util.f
! c subroutine per il calcolo della moda della popolazione dell'ensemble
! c following Ziehmann 2001

    REAL, INTENT(in) :: obs(MNSTAZ,MNGIO),pred(MNSTAZ,MNGIO,MNRM)
    INTEGER, INTENT(in) :: MNSTAZ,MNGIO,MNRM
    INTEGER, INTENT(in) :: ngio,nstaz,nfc
    REAL, INTENT(in) :: rmddb,rmdo
    INTEGER, INTENT(in) :: wght(MNGIO,MNRM)
    REAL, INTENT(out) :: succrate(3)
    REAL :: media
    INTEGER :: class(3), numcasi(3), co, cm, predindex, modepop
    LOGICAL :: loutput
    
    PRINT*,'subroutine mode_population',nfc

    succrate=0.
    numcasi=0
    ntot=0
    do ig=1,ngio
       do is=1,nstaz
          ! check on missing data
          nmd=0
          ifc=1
          if(abs(obs(is,ig)-rmddb) < 1.0E-6)nmd=1
          do while(nmd.eq.0.and.ifc.le.nfc)
             if(abs(pred(is,ig,ifc)-rmddb) < 1.0E-6)nmd=1
             ifc=ifc+1
          enddo
          ! if missing data (nmd=1) no action is taken
          if(nmd == 0)then
             ntot=ntot+1
             class=0
             do ifc=1,nfc
                if((pred(is,ig,ifc)-0.0) < 1.0E-1)then
                   class(1)=class(1)+1
                elseif(pred(is,ig,ifc) <= 3.)then
                   class(2)=class(2)+1
                else
                   class(3)=class(3)+1
                endif
             enddo
             modepop=max(class(1),class(2),class(3))
             if(modepop <= 8)then
                predindex=1 ! low predictability
             elseif(modepop >= 12)then
                predindex=2 ! high predictability
             else
                predindex=3
             endif
             numcasi(predindex)=numcasi(predindex)+1
             media=sum(pred(is,ig,:))/nfc
             if((obs(is,ig)-0.0) < 1.0E-1)then
                co=1
             elseif(obs(is,ig) <= 3.)then
                co=2
             else
                co=3
             endif
             if((media-0.0) < 1.0E-1)then
                cm=1
             elseif(media <= 3.)then
                cm=2
             else
                cm=3
             endif
             if(co == cm)then
                succrate(predindex)=succrate(predindex)+1
             endif
          endif
       enddo
    enddo
    if(ntot /= 0)then
       print*,'num succ ',succrate
       print*,'num casi ',numcasi,ntot
       succrate=succrate/ntot
    endif
       
    return
  end subroutine mode_population

!***********************************************************************************
    
    SUBROUTINE ranked(MNSTAZ,MNGIO,MNRM,obs,pred,ngio,nstaz,nfc, &
    nelsupens,nsoglie,rmddb,rmdo,soglie,wght,loutput,rps,rpss,rpssd)

! c VERIFICA - scores_prob_util.f
! c subroutine per il calcolo di Ranked Probability Score e 
! c Ranked Probability Skill Score
! calcolo anche il Debiased RPSS (RPSSD), seguendo Weigel et al., 2007

    REAL, INTENT(in) :: obs(MNSTAZ,MNGIO),pred(MNSTAZ,MNGIO,MNRM)
    INTEGER, INTENT(in) :: MNSTAZ,MNGIO,MNRM
    INTEGER, INTENT(in) :: ngio,nstaz,nfc,nsoglie,nelsupens
    REAL, INTENT(in) :: rmddb,rmdo
    REAL, INTENT(in) :: soglie(nsoglie)
    INTEGER, INTENT(in) :: wght(MNGIO,MNRM)
    REAL, ALLOCATABLE :: pobs(:),ppred(:),nocc(:),cate(:)
    INTEGER :: ntot,iso,icate,ncate
    REAL :: rpscli
    REAL :: rankp,rankpcli
    REAL, INTENT(out) :: rps,rpss,rpssd
    LOGICAL :: loutput

    print*,'subroutine ranked ',nfc

  ! mi riconduco ad un super-ensemble completo
    nel=nelsupens

! calcolo il numero di categorie (nsoglie + 1)
    ncate=nsoglie+1
    ALLOCATE(pobs(ncate),ppred(ncate),nocc(ncate),cate(ncate))
    do iso=1,nsoglie
       cate(iso)=soglie(iso)
    enddo
    cate(ncate)=9999
    
! compute the sample climatology for every threshold
    ntot=0
    DO icate=1,ncate
       nocc(icate)=0
    ENDDO
    DO ig=1,ngio
      DO is=1,nstaz
        nmd=0
        ifc=1
        IF(ABS(obs(is,ig)-rmddb).LT.1.0E-6)nmd=1
!        IF(ABS(obs(is,ig)-0.) < 1.0E-6)nmd=1
        DO WHILE(nmd.EQ.0.AND.ifc.LE.nfc)
          IF(ABS(pred(is,ig,ifc)-rmddb).LT.1.0E-6)nmd=1 
!          IF(ABS(pred(is,ig,ifc)-0.) < 1.0E-6)nmd=1 
          ifc=ifc+1
        ENDDO
        ! IF missing DATA (nmd=1) no action is taken 
        IF(nmd == 0)THEN
          ntot=ntot+1
          DO icate=1,ncate
            IF(obs(is,ig) <= cate(icate))THEN
              nocc(icate)=nocc(icate)+1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
    ENDDO
    DO icate=1,ncate
      IF(ntot /= 0)nocc(icate)=nocc(icate)/REAL(ntot)
    ENDDO
    print*,'nocc',(nocc(icate),icate=1,ncate)

    ntot=0
    rps=0.
    rpscli=0.
    DO ig=1,ngio
      DO is=1,nstaz
        ! check on missing DATA
        nmd=0
        ifc=1
        IF(ABS(obs(is,ig)-rmddb) < 1.0E-6)nmd=1
!        IF(ABS(obs(is,ig)-0.) < 1.0E-6)nmd=1
        DO WHILE(nmd == 0.AND.ifc <= nfc)
          IF(ABS(pred(is,ig,ifc)-rmddb) < 1.0E-6)nmd=1 
!          IF(ABS(pred(is,ig,ifc)-0.) < 1.0E-6)nmd=1 
          ! controllo coppie        
!          print*,'obs ',obs(is,ig),'member ',ifc,'pred ',pred(is,ig,ifc)
          ifc=ifc+1
        ENDDO
        ! IF missing DATA (nmd=1) no action is taken 
        IF(nmd == 0)THEN
!           print*,'obs',obs(is,ig),'pred',(pred(is,ig,ifc),ifc=1,nfc)
          ntot=ntot+1
          DO icate=1,ncate
            pobs(icate)=0.
            ppred(icate)=0.
          ENDDO
          ! computation of cumulated Pm and Om
          DO icate=1,ncate
            IF(obs(is,ig) <= cate(icate))THEN
              pobs(icate)=1.
            ENDIF
            DO ifc=1,nfc
              IF(pred(is,ig,ifc) <= cate(icate))THEN
!                ppred(iso)=ppred(iso)+1./REAL(nfc)
                ppred(icate)=ppred(icate)+wght(ig,ifc)/REAL(nel)
              ENDIF
            ENDDO
          ENDDO
          rankp=0.
          rankpcli=0.
          ! computation of Ranked Probability Score
          DO icate=1,ncate
!            print*,icate,'pobs',pobs(icate),'ppred',ppred(icate)
             rankp=rankp+(ppred(icate)-pobs(icate))**2
             rankpcli=rankpcli+(nocc(icate)-pobs(icate))**2
          ENDDO

          rankp=rankp/real(ncate-1)
          rankpcli=rankpcli/real(ncate-1)

          IF(loutput)WRITE(19,*)'igio ',ig,'istaz ',is,'rankp ',rankp,'rankpcli ',rankpcli
          rps=rps+rankp
          rpscli=rpscli+rankpcli
        ENDIF
      ENDDO
    ENDDO

    print*,'ngioxnstaz',ngio,nstaz,'ntot',ntot,'rps',rps

    rps=rps/REAL(ntot)
    rpscli=rpscli/REAL(ntot)
    
! compute reference RPS (RPScli) and the Skill Score (RPSS)
    IF(rpscli /= 0.)THEN
      rpss=(rpscli-rps)/rpscli
      rpssd=1-(rps/(rpscli+(1/real(nel)*rpscli)))
    ELSE
      rpss=rmdo
      rpssd=rmdo
    ENDIF

    IF(loutput)WRITE(19,*)'rps ',rps,'rpscli ',rpscli,'rpss ',rpss,'rpssd ',rpssd

    RETURN
    END SUBROUTINE ranked

!***********************************************************************************

    SUBROUTINE ordine (x,n)

! c VERIFICA - scores_prob_util.f
! c riordinamento in senso crescente degli elementi di un vettore

    REAL, INTENT(inout) :: x(n)
    
    DO i=1,n-1
      imin=i
      DO j=i+1,n
        IF(x(j) < x(imin))imin=j
      ENDDO
      IF(imin > i)THEN
        temp=x(imin)
        x(imin)=x(i)
        x(i)=temp
      ENDIF
    ENDDO
    
    RETURN
    END SUBROUTINE ordine

!*****************************************************************************

    SUBROUTINE sprerr(obs,pred,rmddb)

    ! VERIFICA - score_prob_util.f90
    ! calcolo di spread ed errore dell'ensemble per produrre la relazione spr/err

    REAL, INTENT(in) :: obs(:,:),pred(:,:,:) !dim1=nstazioni dim2=ngiorni dim3=nfc
    REAL, INTENT(in) :: rmddb
    INTEGER :: nrmse
    REAL :: med(SIZE(obs,1),SIZE(obs,2)),rmsesp,sprdsp
    
    nstaz=SIZE(pred,1)
    ngio=SIZE(pred,2)
    nfc=SIZE(pred,3)

    print*,'nfc',nfc,'nstaz',nstaz,'ngio',ngio

    ! calcolo la media
    med=rmddb
    FORALL(istaz=1:nstaz, igio=1:ngio, pred(istaz,igio,1) /= rmddb )
      med(istaz,igio)=SUM(pred(istaz,igio,:))/nfc
    END FORALL

    rmsesp=0.
    sprdsp=0.
    nrmse=0
    DO igio=1,ngio
      DO istaz=1,nstaz
        ! calcolo l'errore della media
        IF(obs(istaz,igio) /= rmddb .AND. med(istaz,igio) /= rmddb)THEN
           nrmse=nrmse+1
           rmsesp=rmsesp+abs(med(istaz,igio)-obs(istaz,igio))
        ENDIF
        ! calcolo lo spread
        DO ifc=1,nfc
          IF(obs(istaz,igio) /= rmddb .AND. pred(istaz,igio,ifc) /= rmddb)THEN
            sprdsp=sprdsp+abs(pred(istaz,igio,ifc)-med(istaz,igio))
          ENDIF
        ENDDO
!        WRITE(20,*)rmsesp
!        WRITE(21,*)sprdsp
      ENDDO
    ENDDO
    rmsesp=rmsesp/REAL(nrmse)
    sprdsp=sprdsp/(REAL(nfc-1)*REAL(nrmse)) 
    print*,'MAE',rmsesp,'SPR',sprdsp

    RETURN
    END SUBROUTINE sprerr

!*****************************************************************************
          subroutine init_random_seed()
            use iso_fortran_env, only: int64
            implicit none
            integer, allocatable :: seed(:)
            integer :: i, n, un, istat, dt(8), pid
            integer(int64) :: t
          
            call random_seed(size = n)
            allocate(seed(n))
            ! First try if the OS provides a random number generator
            open(newunit=un, file="/dev/urandom", access="stream", &
                 form="unformatted", action="read", status="old", iostat=istat)
            if (istat == 0) then
               read(un) seed
               close(un)
            else
               ! Fallback to XOR:ing the current time and pid. The PID is
               ! useful in case one launches multiple instances of the same
               ! program in parallel.
               call system_clock(t)
               if (t == 0) then
                  call date_and_time(values=dt)
                  t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
                       + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
                       + dt(3) * 24_int64 * 60 * 60 * 1000 &
                       + dt(5) * 60 * 60 * 1000 &
                       + dt(6) * 60 * 1000 + dt(7) * 1000 &
                       + dt(8)
               end if
               pid = getpid()
               t = ieor(t, int(pid, kind(t)))
               do i = 1, n
                  seed(i) = lcg(t)
               end do
            end if
            call random_seed(put=seed)
          contains
            ! This simple PRNG might not be good enough for real work, but is
            ! sufficient for seeding a better PRNG.
            function lcg(s)
              integer :: lcg
              integer(int64) :: s
              if (s == 0) then
                 s = 104729
              else
                 s = mod(s, 4294967296_int64)
              end if
              s = mod(s * 279470273_int64, 4294967291_int64)
              lcg = int(mod(s, int(huge(0), int64)), kind(0))
            end function lcg
          end subroutine init_random_seed


    END MODULE scores_prob_util_dballe

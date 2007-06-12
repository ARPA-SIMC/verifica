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

    subroutine brier(MNSTAZ,MNGIO,MNRM,obs,pred,ngio,nstaz,nfc, &
    nelsupens,rmddb,rmdo,thr,wght,ntot,nocc,bs,rel,res,bss)

! c VERIFICA - scores_prob_util.f
! c subroutine per il calcolo di Brier Score, Brier Skill Score,
! c BS decomposition e Reliability Diagram

    parameter (NK=10)
    real :: obs(MNSTAZ,MNGIO),pred(MNSTAZ,MNGIO,MNRM)
    REAL :: ofreq(0:NK),thr
    integer :: npu(0:NK)
    real, ALLOCATABLE :: obar(:),ybar(:)
    integer, ALLOCATABLE :: nbar(:)
    integer :: wght(MNGIO,MNRM)

    print*,'subroutine brier',nfc

    N=nelsupens
! allocazione matrici dati
    ALLOCATE(obar(0:N))
    ALLOCATE(ybar(0:N))
    ALLOCATE(nbar(0:N))

    oavg=0.
    ntot=0
    nocc=0

    do ik=0,NK
        ofreq(ik)=0.
        npu(ik)=0
    enddo

! mi riconduco ad un super-ensemble completo
    nel=N

    do k=0,nel
        obar(k)=0.
        ybar(k)=0.
        nbar(k)=0
    enddo

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
                kobs=0
                if(obs(is,ig) > thr)kobs=1
                kpred=0
                do ifc=1,nfc
                    if(pred(is,ig,ifc) > thr) &
                    kpred=kpred+wght(ig,ifc)
                enddo
            ! computation of brier score
                p=real(kpred)/real(nel)
                o=real(kobs)
                bs=bs+(p-o)**2
                ntot=ntot+1
                nocc=nocc+kobs
            ! reliability diagram
                kprob=nint(p*10.)
                ofreq(kprob)=ofreq(kprob)+o
                npu(kprob)=npu(kprob)+1
            ! brier components
                obar(kpred)=obar(kpred)+o
                nbar(kpred)=nbar(kpred)+1
            endif
        enddo                  !nstaz
    enddo                     !ngio

    write(24,'(2(1x,a))')'forecast','observed'
    do ik=0,NK
        if(npu(ik) > 0)then
            ofreq(ik)=ofreq(ik)/real(npu(ik))
        else
            ofreq(ik)=rmdo
        endif
        write(24,'(2(2x,f7.2))')real(ik)/10.,ofreq(ik)
    enddo

    if(ntot /= 0)then
        bs=bs/real(ntot)
        bscli=real(nocc)/real(ntot)*(1.-real(nocc)/real(ntot))

        do k=0,nel
            if(nbar(k) > 1)obar(k)=obar(k)/real(nbar(k))
            ybar(k)=real(k)/real(nel)
        enddo

        oavg=real(nocc)/real(ntot)
        unc = bscli
        res = 0.
        rel = 0.
        do k=0,nel
            res=res+((obar(k)-oavg)**2)*real(nbar(k))
            rel=rel+((ybar(k)-obar(k))**2)*real(nbar(k))
        enddo
        res=res/real(ntot)
        rel=rel/real(ntot)

        if(bscli /= 0.)then
            bss=(bscli-bs)/bscli
        else
            bss=rmdo
        endif

        write(14,'(/1x,10hTHRESHOLD=,f6.1)') thr
        write(14,'(1x,5hNTOT=,i6,7h  NOCC=,i6,8h  P_CLI=,f10.3)') &
        ntot,nocc,bscli
        write(14,'(1x,3hBS=,f13.6)') bs
        write(14,'(1x,4hrel=,f13.6)') rel
        write(14,'(1x,4hres=,f13.6)') res
        write(14,'(1x,4hunc=,f13.6)') unc
        write(14,'(1x,5hobar=,f13.6)') oavg
        write(14,'(1x,4hBSS=,f13.6)') bss

    else
        bs=rmdo
        rel=rmdo
        res=rmdo
        bscli=rmdo
        bss=rmdo
    endif

    return
    end subroutine brier

!**************************************************************************************
    subroutine brier_prob(MNSTAZ,MNGIO,MNRM,obs,pred,ngio,nstaz,nfc, &
    nelsupens,rmddb,rmdo,thr,wght,ntot,nocc,bs,bss)

! c VERIFICA - scores_prob_util.f
! c subroutine per il calcolo di Brier Score, Brier Skill Score e BS decomposition

    real ::    obs(MNSTAZ,MNGIO),pred(MNSTAZ,MNGIO,MNRM)
    integer :: wght(MNGIO,MNRM)
    REAL ::    o,p,nocc,thr

    print*,'subroutine brier_prob',nfc

    N=nelsupens

    ntot=0
    nocc=0.

! mi riconduco ad un super-ensemble completo
    nel=N

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

        write(14,'(/1x,10hTHRESHOLD=,f6.1)') thr
        write(14,'(1x,5hNTOT=,i6,7h  NOCC=,f9.2,8h  P_CLI=,f10.3)') &
        ntot,nocc,bscli
        write(14,'(1x,3hBS=,f13.6)') bs
        write(14,'(1x,4hBSS=,f13.6)') bss
    else
        bs=rmdo
        bscli=rmdo
        bss=rmdo
    endif

    return
    end subroutine brier_prob

!**************************************************************************************

    subroutine roc(MNSTAZ,MNGIO,MNRM,obs,pred,ngio,nstaz,nfc, &
    nelsupens,rmddb,rmdo,thr,wght,ntot,nocc,roca)

! c VERIFICA - scores_prob_util.f
! c subroutine per il calcolo della ROC area e delle ROC curves

    integer :: nobs(2)
    REAL :: obs(MNSTAZ,MNGIO),pred(MNSTAZ,MNGIO,MNRM),thr
    integer :: wght(MNGIO,MNRM)
    integer, ALLOCATABLE :: kobs(:,:),nprev(:)
    real, ALLOCATABLE :: hit(:),far(:),fareck(:)

    print*,'subroutine roc',nfc

    N=nelsupens
! allocazione matrici dati
    ALLOCATE(kobs(0:N,2))
    ALLOCATE(nprev(0:N))
    ALLOCATE(hit(0:N))
    ALLOCATE(far(0:N))
    ALLOCATE(fareck(0:N))

! mi riconduco ad un super-ensemble completo
    nel=N

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
        write(12,'(/1x,10hTHRESHOLD=,f6.1)') thr
        write(12,'(1x,5hNTOT=,i6,7h  NOCC=,i6,8h  P_CLI=,f15.8)') &
        nobs(1)+nobs(2),nobs(1), &
        real(nobs(1)*nobs(2))/real((nobs(1)+nobs(2))**2)
        write(12,'(1x,5hAREA=,f10.3)')roca
        write(12,'(//8x,a,8x,a,5x,a,8x,a/)') &
        'far','hit','fareck','nprev'
        do kpred=0,nel
            write (12,'(3(1x,f10.3),1x,i8)')far(kpred),hit(kpred) &
            ,fareck(kpred),nprev(kpred)
        enddo
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

    subroutine terr(MNSTAZ,MNRM,obs,pred,nstaz,nrm, &
    nelsupens,rmddb,rmdo,wght,ipos)

! c VERIFICA - scores_prob_util.f
! c subroutine per il calcolo del total error di ogni membro

    real :: obs(MNSTAZ),pred(MNSTAZ,MNRM)
    integer :: wght(MNRM)
    INTEGER, ALLOCATABLE :: num(:)

    N=nelsupens
    ALLOCATE(num(N))

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
        write(22,'(a,i2,a,i3,a,f8.3)') &
        'elemento= ',irm,' peso= ',wght(irm), &
        ' terr= ',err
    enddo

    write(22,'(a,f8.3,a,i1,a,i2)') &
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

    write(22,'(a,f8.3,a,i1,a,i2)') &
    'errmin= ',errmin,' irmmin= ',irmmin,' num ',ipos

    return
    end subroutine terr

!***********************************************************************************

    subroutine costloss(MNSTAZ,MNGIO,MNRM,obs,pred,ngio,nstaz,nfc, &
    nelsupens,rmddb,rmdo,thr,wght,ntot,nocc,area)

! c VERIFICA - scores_prob_util.f
! c subroutine per il calcolo delle Cost-loss curves e della Cost-loss area

    parameter (NCL=1000)

    integer :: nobs(2)
    real ::    obs(MNSTAZ,MNGIO),pred(MNSTAZ,MNGIO,MNRM)
    integer :: wght(MNGIO,MNRM)
    REAL ::    omed,thr
    real ::    me,mecli,mep
    real ::    enve(0:NCL)

    integer, ALLOCATABLE :: kobs(:,:),npre(:)
    real, ALLOCATABLE :: hit(:),far(:)
    real, ALLOCATABLE :: ks(:)
    real, ALLOCATABLE :: val(:,:)

    print*,'subroutine cost-loss ',nfc

    N=nelsupens
    ALLOCATE(kobs(0:N,2),npre(0:N))
    ALLOCATE(hit(0:N))
    ALLOCATE(far(0:N))
    ALLOCATE(ks(0:N))
    ALLOCATE(val(0:N,0:NCL))

! mi riconduco ad un super-ensemble completo
    nel=N

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
    ! write(21,'(2(1x,f5.3))')hit(kpred),far(kpred)
    ! enddo

    ! output per la grafica
        do icl=0,NCL
            write(23,'(2(1x,e15.3))') &
            real(icl)*fact,enve(icl)
        ! o in uno stesso file
        ! write(17,'(52(1x,f9.6),1x,e15.3)')
        ! $              real(icl)*fact,
        ! $              (val(kpred,icl),
        ! $              kpred=1,nel),
        ! $              enve(icl)
        enddo

    ! fine controllo sulle osservazioni
    else
        area=rmdo
    ! output per la grafica
        do icl=0,NCL
            write(23,'(2(1x,e15.3))') &
            real(icl)*fact,rmdo
        enddo
    endif

    ntot=nobs(1)+nobs(2)
    nocc=nobs(1)

    return
    end subroutine costloss

!***********************************************************************************

    subroutine outrange(MNSTAZ,MNGIO,MNRM,obs,pred,ngio,nstaz,nfc, &
    nelsupens,rmddb,rmdo,wght,outr)

! c VERIFICA - scores_prob_util.f
! c subroutine per il calcolo della Percentage of Outliers e dell'errore associato

    real ::      obs(MNSTAZ,MNGIO),pred(MNSTAZ,MNGIO,MNRM)
    integer ::   wght(MNGIO,MNRM)

    REAL, ALLOCATABLE :: predor(:)
    INTEGER, ALLOCATABLE :: inter(:)

    print*,'subroutine outrange',nfc

    N=nelsupens
    ALLOCATE(predor(N))
    ALLOCATE(inter(0:N))

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
    enddo

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
                if(obs(is,ig) < predor(1))inter(0)=inter(0)+1
                if(obs(is,ig) >= predor(nfc))inter(nfc)=inter(nfc)+1
                do ifc=1,nfc-1
                    if(obs(is,ig) >= predor(ifc) .AND. &
                    obs(is,ig) < predor(ifc+1))then
                        inter(ifc)=inter(ifc)+1
                    endif
                enddo
            endif
        enddo
    enddo
    nocc=int(pout+pin)
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
    write(15,'(1x,a9,f6.3,2x,2(a9,f6.3,1x,a9,f7.3,2x))') &
    'OUTRANGE=',outr, &
    'OUTRAmin=',outrn,'errmedio=',errn, &
    'OUTRAmax=',outrx,'errmedio=',errx
! write(15,'(/1x,8(a5,f8.1,2x))')'pout',pout,'pin',pin,
! $     'poutn',poutn,'pinn',pinn,'poutx',poutx,'pinx',pinx
    write(15,'(1x,a,2(a,i3)/)')'classe max',' sotto min ',kerrmaxn, &
    ' sopra max ',kerrmaxx
    if(ntot == 0)return
    do i=0,nfc
        write(16,'(1x,i3,1x,f7.5)')i,inter(i)/real(ntot)
    enddo

    return
    end subroutine outrange

!***********************************************************************************

    SUBROUTINE ranked(MNSTAZ,MNGIO,MNRM,obs,pred,ngio,nstaz,nfc,nsoglie, &
     nelsupens,rmddb,rmdo,soglie,wght,rps,rpss)

! c VERIFICA - scores_prob_util.f
! c subroutine per il calcolo di Ranked Probability Score e 
! c Ranked Probability Skill Score

    REAL :: obs(MNSTAZ,MNGIO),pred(MNSTAZ,MNGIO,MNRM)
    REAL :: soglie(nsoglie)
    INTEGER :: wght(MNGIO,MNRM)
    INTEGER :: ntot
    REAL, ALLOCATABLE :: pobs(:),ppred(:),nocc(:)
    REAL :: rps,rpss,rpscli

    print*,'subroutine ranked ',nfc

    N=nelsupens
! allocazione matrici dati
    ALLOCATE(pobs(nsoglie))
    ALLOCATE(ppred(nsoglie))
    ALLOCATE(nocc(nsoglie))

! compute the sample climatology for every threshold
    ntot=0
    DO iso=1,nsoglie
      nocc(iso)=0
    ENDDO
    DO ig=1,ngio
      DO is=1,nstaz
        nmd=0
        ifc=1
        IF(ABS(obs(is,ig)-rmddb).LT.1.0E-6)nmd=1
        DO WHILE(nmd.EQ.0.AND.ifc.LE.nfc)
          IF(ABS(pred(is,ig,ifc)-rmddb).LT.1.0E-6)nmd=1 
          ifc=ifc+1
        ENDDO
        ! IF missing DATA (nmd=1) no action is taken 
        IF(nmd == 0)THEN
          ntot=ntot+1
          DO iso=1,nsoglie
            IF(obs(is,ig).LE.soglie(iso))THEN
              nocc(iso)=nocc(iso)+1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
    ENDDO
    DO iso=1,nsoglie
      IF(ntot /= 0)nocc(iso)=nocc(iso)/REAL(ntot)
    ENDDO

    rps=0.
    DO ig=1,ngio
      DO is=1,nstaz
        ! check on missing DATA
        nmd=0
        ifc=1
        IF(ABS(obs(is,ig)-rmddb).LT.1.0E-6)nmd=1
        DO WHILE(nmd.EQ.0.AND.ifc.LE.nfc)
          IF(ABS(pred(is,ig,ifc)-rmddb).LT.1.0E-6)nmd=1 
          ifc=ifc+1
        ENDDO
        ! IF missing DATA (nmd=1) no action is taken 
        IF(nmd == 0)THEN
          ntot=ntot+1
          DO iso=1,nsoglie
            pobs(iso)=0.
            ppred(iso)=0.
          ENDDO
          ! computation of cumulated Pm and Om
          DO iso=1,nsoglie
            IF(obs(is,ig).LE.soglie(iso))THEN
              pobs(iso)=1.
            ENDIF
            DO ifc=1,nfc
              IF(pred(is,ig,ifc).LE.soglie(iso))THEN
                ppred(iso)=ppred(iso)+1./REAL(nfc)
              ENDIF
            ENDDO
          ENDDO
          ! computation of Ranked Probability Score
          rankp=0.
          rankpcli=0.
          DO iso=1,nsoglie
            rankp=rankp+(ppred(iso)-pobs(iso))**2
            rankpcli=rankpcli+(nocc(iso)-pobs(iso))**2
          ENDDO
          WRITE(19,*)'igio ',ig,'istaz ',is,'rankp ',rankp/REAL(nsoglie),'rankpcli ',rankpcli/REAL(nsoglie)
          rps=rps+rankp/real(nsoglie)
          rpscli=rpscli+rankpcli/real(nsoglie)
        ENDIF
      ENDDO
    ENDDO

    rps=rps/REAL(nstaz*ngio)
    rpscli=rpscli/REAL(nstaz*ngio)
    
! compute reference RPS (RPScli) and the Skill Score (RPSS)
    IF(rpscli /= 0.)THEN
      rpss=(rpscli-rps)/rpscli
    ELSE
      rpss=rmdo
    ENDIF

    WRITE(19,*)'rps ',rps,'rpscli ',rpscli,'rpss ',rpss

    RETURN
    END SUBROUTINE ranked

!***********************************************************************************

    subroutine ordine (x,n)

! c VERIFICA - scores_prob_util.f
! c riordinamento in senso crescente degli elementi di un vettore

    real :: x(n)

    do i=1,n-1
        imin=i
        do j=i+1,n
            if(x(j) < x(imin))imin=j
        enddo
        if(imin > i)then
            temp=x(imin)
            x(imin)=x(i)
            x(i)=temp
        endif
    enddo

    return
    end subroutine ordine

!****************************************************************************
! c VERIFICA - scores_util_dballe.f
! c subroutines per il calcolo degli scores deterministici
! c autori: Chiara Marsigli e Francesco Boccanera

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

    subroutine mae(MNV,obs,pred,nv,rmddb,rmd,npo,maerr)

! c VERIFICA - scores_util.f
! c subroutine per il calcolo del mean absolute error

    integer :: npo
    real :: maerr
    real :: obs(MNV),pred(MNV)

    maerr=0.
    npo=0
    do iv=1,nv
        if(pred(iv) /= rmddb .AND. obs(iv) /= rmddb)then
            npo=npo+1
            maerr=maerr+abs(pred(iv)-obs(iv))
            print*,'maerr ',npo,pred(iv),obs(iv),maerr
        endif
    enddo
    if(npo > 0)then
        maerr=maerr/real(npo)
    else
        maerr=rmd
    endif

    return
    end subroutine mae

!****************************************************************

    subroutine mse(MNV,obs,pred,nv,rmddb,rmd,npo,mserr,rmserr)

! c VERIFICA - scores_util.f
! c subroutine per il calcolo del mean square error

    integer :: npo
    real :: mserr,rmserr
    real :: obs(MNV),pred(MNV)

    mserr=0.
    npo=0
    do iv=1,nv
        if(pred(iv) /= rmddb .AND. obs(iv) /= rmddb)then
            npo=npo+1
            mserr=mserr+(pred(iv)-obs(iv))**2
        endif
    enddo
    if(npo > 0)then
        mserr=mserr/real(npo)
        rmserr=sqrt(mserr)
    else
        mserr=rmd
        rmserr=rmd
    endif

    return
    end subroutine mse

!******************************************************************

    subroutine bias(MNV,obs,pred,nv,rmddb,rmd,npo,b)

! c VERIFICA - scores_util.f
! c subroutine per il calcolo del bias (mean error)

    INTEGER :: npo
    REAL :: obs(MNV),pred(MNV),b

    real :: p,o

    b=0.
    p=0.
    o=0.
    npo=0
    do iv=1,nv
        if(pred(iv) /= rmddb .AND. obs(iv) /= rmddb)then
            npo=npo+1
            b=b+(pred(iv)-obs(iv))
            p=p+pred(iv)
            o=o+obs(iv)
        endif
    enddo
    if(npo > 0)then
        b=b/real(npo)
        p=p/real(npo)
        o=o/real(npo)
    else
        b=rmd
    endif

    write(13,*)o,p

    return
    end subroutine bias

!*****************************************************************

    SUBROUTINE cgravity(MNV,obs,pred,lon,lat,nv,rmddb,rmd,npo,distmean,cog)

! c VERIFICA - scores_util.f
! c subroutine per il calcolo del centre of gravity (PREVIEW-PLAINFLOOD)

    REAL, PARAMETER :: r_earth=6378.
    REAL :: pi ! non prende la funzione atan in un parameter

    INTEGER :: npo
    REAL :: obs(MNV),pred(MNV),lon(MNV),lat(MNV)
    REAL :: cog
    REAL :: distmean ! dist in km dal centro all'outlet

    REAL :: xc_f,yc_f,xc_o,yc_o,F,O,diff_cog_lon,diff_cog_lat,cog_diff

    pi=4.0*ATAN(1.)

    xc_f=0.
    yc_f=0.
    xc_o=0.
    yc_o=0.
    F=0.
    O=0.

    DO iv=1,nv
      IF(pred(iv) /= rmddb .AND. obs(iv) /= rmddb)THEN
        npo=npo+1
        xc_f=xc_f+(pred(iv)*lon(iv))
        yc_f=yc_f+(pred(iv)*lat(iv))
        xc_o=xc_o+(obs(iv)*lon(iv))
        yc_o=yc_o+(obs(iv)*lat(iv))
        F=F+pred(iv)
        O=O+obs(iv)
      ENDIF
    ENDDO

    WRITE(15,*)'sum_F=',F,' sum_O=',O
    WRITE(15,*)'sum_lon_f=',xc_f,' sum_lat_f=',yc_f
    WRITE(15,*)'sum_lon_o=',xc_o,' sum_lat_o=',yc_o

    IF(npo > 0)THEN
      xc_f=xc_f/F
      yc_f=yc_f/F
      xc_o=xc_o/O
      yc_o=yc_o/O
    ENDIF

    diff_cog_lat=(yc_f-yc_o)*r_earth*pi/180.
    diff_cog_lon=(xc_f-xc_o)*r_earth*pi/180.

    cog_diff=SQRT(diff_cog_lat**2+diff_cog_lon**2)

    IF(distmean /= 0.)cog=cog_diff/distmean

    WRITE(15,*)'lon_f=',xc_f,' lat_f=',yc_f
    WRITE(15,*)'lon_o=',xc_o,' lat_o=',yc_o
    WRITE(15,*)'cog_diff= ',cog_diff
    WRITE(15,*)'cog= ',cog

    return
    END SUBROUTINE cgravity

!*****************************************************************

    SUBROUTINE score_con_table(MNV,obs,pred,nv, &
     nsoglie,soglie,iscaddb,rmddb,rmd,lthr)
    
! c VERIFICA - scores_util.f
! c subroutine per il calcolo della tabella di contingenza e dei suoi indici
    
    REAL :: soglie(nsoglie)
    REAL :: obs(MNV),pred(MNV)
    INTEGER :: npo
    REAL :: mserr,rmserr,b
    INTEGER :: lthr
    PARAMETER (flat=180.)
    
    WRITE(10,'(//a9,i3/)')'scadenza=',iscaddb
    WRITE(20,'(2(6x,a3),5x,a3,2(6x,a2),7x,a2,7x,a3,6x,a2,5x,a6,3x,a6,5x,a3,4x,a6,4x,a4)') &
     'thr','npo','nos','bs','hr','ts','pod','fa', &
     'rnd ts','rnd fa','hss','rmserr','bias'
    
    DO ith=1,nsoglie
      no=0
      nf=0
      nc=0
      npo=0
      mserr=0.
      b=0.
      
      IF(lthr == 1)THEN      !sopra le soglie
        DO iv=1,nv
          IF(pred(iv) /= rmddb .AND. obs(iv) /= rmddb)THEN
            npo=npo+1
            IF(obs(iv) >= soglie(ith)) THEN
              no=no+1
              mserr=mserr+(pred(iv)-obs(iv))**2
              b=b+(pred(iv)-obs(iv))
            ENDIF
            IF(pred(iv) >= soglie(ith))THEN
              nf=nf+1
              IF(obs(iv) >= soglie(ith)) nc=nc+1
            END IF
          END IF
        END DO
        CALL costloss_det(MNV,obs,pred,nv,rmddb,rmd, &
         soglie(ith),area)
      ELSEIF(lthr == -1)THEN   !sotto le soglie
        DO iv=1,nv
          IF(pred(iv) /= rmddb .AND. obs(iv) /= rmddb)THEN
            npo=npo+1
            IF(obs(iv) <= soglie(ith)) THEN
              no=no+1
              mserr=mserr+(pred(iv)-obs(iv))**2
              b=b+(pred(iv)-obs(iv))
            ENDIF
            IF(pred(iv) <= soglie(ith))THEN
              nf=nf+1
              IF(obs(iv) <= soglie(ith)) nc=nc+1
            END IF
          END IF
        END DO
      ELSEIF(lthr == 2)THEN  !classi chiuse
        IF (ith == nsoglie) goto111
        DO iv=1,nv
          IF(pred(iv) /= rmddb .AND. obs(iv) /= rmddb)THEN
            npo=npo+1
            IF(obs(iv) >= soglie(ith) .AND. &
             obs(iv) < soglie(ith+1))THEN
              no=no+1
              mserr=mserr+(pred(iv)-obs(iv))**2
              b=b+(pred(iv)-obs(iv))
            ENDIF
            IF(pred(iv) >= soglie(ith) .AND. &
             pred(iv) < soglie(ith+1))THEN
              nf=nf+1
              IF(obs(iv) >= soglie(ith) .AND. &
               obs(iv) < soglie(ith+1)) nc=nc+1
            END IF
          END IF
        END DO
      ELSEIF(lthr == 3)THEN  !classi chiuse e variabile tra 0 e 360
        IF (ith == nsoglie)THEN 
          goto111
        ELSEIF (ith == (nsoglie-1))THEN
          DO iv=1,nv
            IF(pred(iv) /= rmddb .AND. obs(iv) /= rmddb)THEN
              npo=npo+1
              IF(obs(iv) >= soglie(ith) .AND. obs(iv) < 360. .OR. &
               obs(iv) >= 0. .AND. obs(iv) < soglie(ith+1))THEN
                no=no+1
                err=pred(iv)-obs(iv)
                err=err + (SIGN(1.,flat-err)+SIGN(1.,-flat-err))*flat
                mserr=mserr+(err)**2
                b=b+err
              ENDIF
              IF(pred(iv) >= soglie(ith) .AND. pred(iv) < 360. .OR. &
               pred(iv) >= 0. .AND. pred(iv) < soglie(ith+1))THEN
                nf=nf+1
                IF(obs(iv) >= soglie(ith) .AND. obs(iv) < 360. .OR. &
                 obs(iv) >= 0. .AND. obs(iv) < soglie(ith+1)) nc=nc+1
              END IF
            END IF
          END DO
        ELSE
          DO iv=1,nv
            IF(pred(iv) /= rmddb .AND. obs(iv) /= rmddb)THEN
              npo=npo+1
              IF(obs(iv) >= soglie(ith) .AND. &
               obs(iv) < soglie(ith+1))THEN
                no=no+1
                err=pred(iv)-obs(iv)
                err=err + (SIGN(1.,flat-err)+SIGN(1.,-flat-err))*flat
                mserr=mserr+(err)**2
                b=b+err
              ENDIF
              IF(pred(iv) >= soglie(ith) .AND. &
               pred(iv) < soglie(ith+1))THEN
                nf=nf+1
                IF(obs(iv) >= soglie(ith) .AND. &
                 obs(iv) < soglie(ith+1)) nc=nc+1
              END IF
            END IF
          END DO
        ENDIF
      ENDIF
      
    ! calcolo degli scores statistici
      
      IF(no > 0)THEN
        
        mserr=mserr/REAL(no)
        rmserr=SQRT(mserr)
        b=b/REAL(no)
        
        IF(nf > 0 .AND. npo /= 0) THEN
          
          WRITE(10,'(a,f6.1/)')'cont. table ',soglie(ith)
          ia=nc
          ib=nf-ia
          ic=no-ia
          id=npo-ia-ib-ic
          WRITE(10,'(11x,a)')'obs'
          WRITE(10,'(9x,a,6x,a/)')'y','n'
          WRITE(10,'(4x,a,1x,i6,2x,i6)')'y',ia,ib
          WRITE(10,'(1x,a)')'fc'
          WRITE(10,'(4x,a,1x,i6,2x,i6/)')'n',ic,id
          
          ! li trasformo in reali prima di fare i conti perche' ottengo numeri
          ! troppo grandi per essere contenuti in un intero
          rno=real(no)
          rnf=real(nf)
          rnc=real(nc)
          rnpo=real(npo)
          
          bs=rnf/rno
          fa=(rnf-rnc)/rnf
          ts=rnc/(rnf+rno-rnc)
          pod=rnc/rno
          po=rno/rnpo
          pf=rnf/rnpo
          pc=po*pf
          rts=pc/(pf+po-pc)
          rfa=(pf-pc)/pf
          hr=(rnpo-rnf-rno+2*rnc)/rnpo
          
          WRITE(77,*)rnpo,rno,rnf,rnc
          IF((npo-no) /= 0 .OR. (npo-nf) /= 0)THEN
            hss=2*(rnc*(rnpo-rnf-rno+rnc)-(rnf-rnc)*(rno-rnc))/ &
             (rno*(rnpo-rnf)+rnf*(rnpo-rno))
          ELSE
            hss=rmd
          ENDIF
          
          WRITE(20,'(1x,f10.4,2(2x,i6),10(1x,f8.3))') &
           soglie(ith),npo,no,bs,hr,ts,pod,fa,rts,rfa,hss,rmserr,b
        ELSE
          WRITE(20,'(1x,f10.4,2(2x,i6),10(1x,f8.3))') &
           soglie(ith),npo,no,rmd,rmd,rmd,rmd,rmd,rmd,rmd,rmd,rmserr,b
        END IF
        
      ELSE
        
        mserr=rmd
        rmserr=rmd
        b=rmd
        
        WRITE(20,'(1x,f10.4,2(2x,i6),10(1x,f8.3))') &
         soglie(ith),npo,no,rmd,rmd,rmd,rmd,rmd,rmd,rmd,rmd,rmd,rmd
        
      ENDIF
    END DO                    !nsoglie
    
111 CONTINUE
    
    RETURN
    END SUBROUTINE score_con_table

!****************************************************************

    subroutine splot(MNV,obs,pred,nv,rmddb,rmd,npo)

    integer :: npo
    real :: obs(MNV),pred(MNV)

! c VERIFICA - scores_util.f
! c subroutine per la realizzazione di uno scatter plot

    npo=0
! write(55,'(2x,a,2x,a)')'obs','pre'
    do iv=1,nv
        if(pred(iv) /= rmddb .AND. obs(iv) /= rmddb)then
            npo=npo+1
            write(55,'(2(1x,f5.1))')obs(iv),pred(iv)
        endif
    enddo

    return
    end subroutine splot

!****************************************************************

    subroutine costloss_det(MNV,obs,pred,nv,rmddb,rmd,thr,area)

! c VERIFICA - scores_util.f
! c subroutine per il calcolo della Cost-loss curve e della Cost-loss area
! c per il deterministico
! c autore: Chiara Marsigli

    parameter (NCL=1000)

    real :: obs(MNV),pred(MNV)

    integer :: nobs(2),na,nb
    real :: hit,far,ks
    real :: omed
    real :: me,mecli,mep
    real :: enve(0:NCL)

!    print*,'subroutine cost-loss_det '

! initialisation
    do kclass=1,2
        nobs(kclass)=0
        na=0
        nb=0
    enddo

    do iv=1,nv
    ! check on missing data
        nmd=0
        if(abs(obs(iv)-rmddb) < 1.0E-4)nmd=1
        if(abs(pred(iv)-rmddb) < 1.0E-4)nmd=1
    ! if missing data (nmd=1) no action is taken
    ! nobs(1)   total number of occurrences
    ! nobs(2)   total number of non-occurrences
        if(nmd == 0)then
            if(obs(iv) > thr)then
                nobs(1)=nobs(1)+1
                if(pred(iv) > thr)na=na+1
            else
                nobs(2)=nobs(2)+1
                if(pred(iv) > thr)nb=nb+1
            endif
        endif
    enddo                     !nv

    if(nobs(1) /= 0 .AND. nobs(2) /= 0)then
    ! computation of false alarm rate and hit rate
    ! hit rate = number of occur. / total number of occur.
    ! fa rate = number of non-occur. / total number of non-occur.
        hit=real(na)/real(nobs(1))
        far=real(nb)/real(nobs(2))
        ks=hit-far
        omed=real(nobs(1))/real(nobs(1)+nobs(2))

    ! Calcolo delle curve di cost-loss
        fact=1./real(NCL)
    ! ciclo sui valori di C/L
        do icl=0,NCL
            cl=real(icl)*fact
            mep=omed*cl
            mecli=min(cl,omed)
            me=omed+far*cl*(1.-omed)-hit*omed*(1.-cl)
            me=max(0.,me)       ! aggiungo per problemi di arrotondamento
            enve(icl)=max(0.,mecli-me)/max(mecli-mep,1.E-10)
        enddo

    ! calcolo della curva envelope
        area=0.
        do icl=0,NCL
            area=area+2.*enve(icl)
        enddo
        area=0.5*fact*(area-enve(0)-enve(NCL))

    ! output per la grafica
        do icl=0,NCL
            write(23,'(2(1x,e15.3))') &
            real(icl)*fact,enve(icl)
        enddo

    ! fine controllo sulle osservazioni
    else
        area=rmd
    ! output per la grafica
        do icl=0,NCL
            write(23,'(2(1x,e15.3))') &
            real(icl)*fact,rmd
        enddo
    endif

    ntot=nobs(1)+nobs(2)
    nocc=nobs(1)

    return
    end subroutine costloss_det

!*****************************************************************************

      subroutine mae_dd(MNV,obs,pred,nv,rmddb,rmd,npo,maerr)

! c VERIFICA - scores_util.f
! c subroutine per il calcolo del mean absolute error della direzione (0-360)

      parameter (flat=180.)

      integer :: npo
      real :: maerr
      real :: obs(MNV),pred(MNV)

      real :: err

      maerr=0.
      npo=0
      do iv=1,nv
         IF(pred(iv) /= rmddb .AND. obs(iv) /= rmddb)THEN
            npo=npo+1
            err=pred(iv)-obs(iv)
            err=err + (sign(1.,flat-err)+sign(1.,-flat-err))*flat
            maerr=maerr+abs(err)
         endif
      enddo
      if(npo > 0)then
         maerr=maerr/real(npo)
      else
         maerr=rmd
      endif

      return
      end subroutine mae_dd

!*****************************************************************  

      subroutine mse_dd(MNV,obs,pred,nv,rmddb,rmd,npo,mserr,rmserr)    

! c VERIFICA - scores_util.f
! c subroutine per il calcolo del mean square error della direzione (0-360)

      parameter (flat=180.)

      integer :: npo
      real :: mserr,rmserr
      real :: obs(MNV),pred(MNV)

      real :: err

      mserr=0.
      npo=0
      do iv=1,nv
         IF(pred(iv) /= rmddb .AND. obs(iv) /= rmddb)THEN
            npo=npo+1
            err=pred(iv)-obs(iv)
            err=err + (sign(1.,flat-err)+sign(1.,-flat-err))*flat
            mserr=mserr+(err)**2
         endif
      enddo
      if(npo.gt.0)then
         mserr=mserr/real(npo)
         rmserr=sqrt(mserr)
      else
         mserr =rmd
         rmserr=rmd
      endif

      return
      end subroutine mse_dd

!*******************************************************************

      subroutine bias_dd(MNV,obs,pred,nv,rmddb,rmd,npo,b)

! c VERIFICA - scores_util.f
! c subroutine per il calcolo del bias (mean error) della direzione (0-360)

      parameter (flat=180.)

      REAL :: obs(MNV),pred(MNV),b
      INTEGER :: npo

      REAL :: p,o,err

      b=0.
      p=0.
      o=0.
      npo=0
      do iv=1,nv
         IF(pred(iv) /= rmddb .AND. obs(iv) /= rmddb)THEN
            npo=npo+1
            err=pred(iv)-obs(iv)
            err=err + (sign(1.,flat-err)+sign(1.,-flat-err))*flat
            b=b+(err)
            p=p+pred(iv)
            o=o+obs(iv)
         endif
      enddo
      if(npo > 0)then
         b=b/real(npo)
         p=p/real(npo)
         o=o/real(npo)
      else
         b=rmd
      endif

      write(13,*)o,p

      return
      end

!******************************************************************

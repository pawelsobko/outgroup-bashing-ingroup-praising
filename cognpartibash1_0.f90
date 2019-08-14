program cognpartibash
    ! program to calculate evolution in a model with
    ! cognitive, opposition bashing and partisan show-off
    ! version 1.0
    ! adapted from NETLOGO code

    implicit none

    ! program variables
    integer :: nagents ! max 10000
    integer :: nruns   ! number of program runs (max 20)
    integer :: nsteps  ! number of program steps
    real*8  :: pcognitive                    ! probability of cognitive encounter
    real*8  :: bashingratio             ! ratio of bashing to partisan encounters
    real*8  :: pbashing                      ! probability of bashing encounter
    real*8  :: ppartisan                     ! probability of partisan praise encounter
    real*8  :: alpha                         ! additive correction for propartisan encounter
    real*8  :: delta                         ! strength of repulsion due to opposition bashing encounter
    real*8  :: mu                            ! speed of convergence of opinions

    ! agent arrays
    real*8,dimension(10001)  :: opinion      ! opinions of agents, bound between -1 and 1
    integer,dimension(10001) :: plusagents   ! array holding agents with positive opinions
    integer,dimension(10001) :: minusagents  ! array holding agents with negative opinions
    integer :: maxplusagent                  ! number of plusagents
    integer :: maxminusagent                 ! number of minusagents                       
    real*8                   :: plusopinion  ! averaged opinion for plus agents
    real*8                   :: minusopinion ! averaged opinion for minus agents
    real*8                   :: totalopinion ! averaged opinion of all agents

    ! HISTOGRAMS 
    ! final distribution histograms
    ! single run histograms
    integer,dimension(51,20) :: opinionhist ! final opinion distributions for each run
    integer,dimension(51,21)    :: opinionhistaver ! final opinion distribution averaged over runs and p values
    ! 

    


    integer :: i,j,k,l,m,n,ii,jj,kp,km,irun,istep,ipstep    ! temporary and dummy variables
    real*8  :: x,y,xx,yy,prob1,prob2         ! temporary and dummy real variables


    open(10,file="cogbashpart.z")
    write(10,*)"! nx 51 ny 21 xmin -1 xmax 1.0 ymin 0 ymax 1 "
    write(*,*)" COGNITIVE, OPPOSITION BASHING, PARTISAN PRAISE SIMULATION"
    !write(*,*)"Probability of cognitive encounter (1-p)"
    !read(*,*) pcognitive
    write(*,*)"Ratio of bashing to noncognitive encounterd"
    read(*,*) bashingratio
    write(*,*)"Proportisan opinion shift alpha = "
    read(*,*) alpha
    write(*,*)"Opposition bashing strength delta"
    read(*,*) delta
    write(*,*)"Speed of convergence mu"
    read(*,*) mu
    !write(*,*)"Random seed"
    !read(*,*) iseed
    
    nagents=1000
    nruns=20
    nsteps=200*nagents
    opinionhistaver = 0

    ! grand loop over p values
    do ipstep=1,21
    pcognitive=1.d0-(ipstep-1)/20.d0    


    !zeroing arrays
    opinionhist = 0

    plusagents = 0
    minusagents = 0

    call random_seed()

    pbashing=(1.d0-pcognitive)*bashingratio
    ppartisan=(1.d0-pcognitive)*(1.d0-bashingratio)
    write(*,*)"alpha=",alpha,"  delta=",delta," mu=",mu,"  pbashing=",pbashing," ppartisan=",ppartisan    



    ! loop over runs
    do irun=1,nruns
        ! setting up initial opinions
        kp=0
        km=0
        do ii=1,nagents
            call random_number(x)
            x=x*2.d0-1.d0
            opinion(ii)=x
            if(opinion(ii).ge.0.d0) then
                kp=kp+1
                plusagents(kp)=ii 
            else
                km=km+1
                minusagents(km)=ii 
            end if
        enddo ! over ii nagents
        maxplusagent =kp
        maxminusagent=km
        ! calculating initial averages of opinions
        totalopinion=0.d0
        plusopinion=0.d0
        minusopinion=0.d0
        do ii=1,nagents
            totalopinion=totalopinion+opinion(ii)/(1.d0*nagents)
        end do ! over ii nagents
        if(maxplusagent.ge.1) then
            do jj=1,maxplusagent
                plusopinion=plusopinion+opinion(plusagents(jj))/(1.d0*maxplusagent)
            enddo ! over jj
        endif
        if(maxminusagent.ge.1) then
            do jj=1,maxminusagent
                minusopinion=minusopinion+opinion(minusagents(jj))/(1.d0*maxminusagent)
            enddo ! over jj
        endif
        ! screen output of initial data
        write(*,*)"RUN ",irun, " pbashing",pbashing, "ppartisan",ppartisan
        write(*,*)"NPLUS=",maxplusagent," NMINUS",maxminusagent," TOT opinion",totalopinion,&
            "PLUS opinion",plusopinion," MINUS opinion",minusopinion



        ! opinion change steps    
        do istep=1,nsteps
            ! pick random agent ii
            call random_number(x)
            ii=int(x*nagents)+1
            call random_number(prob1) ! select specific encounter type
            ! Opposition bashing encounter
            if(prob1.le.pbashing) then
                if(opinion(ii).gt.0) then
                    opinion(ii)=opinion(ii)*(1.d0+(1.d0-abs(opinion(ii)))*delta*maxminusagent/nagents)
                else
                    opinion(ii)=opinion(ii)*(1.d0+(1.d0-abs(opinion(ii)))*delta*maxplusagent/nagents)
                endif
                if(opinion(ii).gt.1.d0) opinion(ii)=1.d0
                if(opinion(ii).lt.-1.d0) opinion(ii)=-1.d0
            endif
            ! propartisan encounter
            if((prob1.gt.pbashing).and.(prob1.le.pbashing+ppartisan)) then
                if(opinion(ii).ge.0.d0) then
                    call random_number(x)
                    jj=int(x*maxplusagent)+1 ! pick random plus agent
                    xx=(opinion(plusagents(jj))+alpha)
                    if(xx.gt.1.d0) xx=1.d0
                    opinion(ii)=opinion(ii)*(1.d0-mu)+xx*mu
                    if(opinion(ii).gt.1.d0) opinion(ii)=1.d0
                else
                    call random_number(x)
                    jj=int(x*maxminusagent)+1 ! pick random plus agent
                    xx=(opinion(minusagents(jj))-alpha)
                    if(xx.lt.-1.d0) xx=-1.d0
                    opinion(ii)=opinion(ii)*(1.d0-mu)+xx*mu
                    if(opinion(ii).lt.-1.d0) opinion(ii)=-1.d0
                endif
            endif
            ! cognitive encounter
            if(prob1.gt.pbashing+ppartisan) then
                call random_number(x)
                jj=int(x*nagents)+1
                opinion(ii)=(1.d0-mu)*opinion(ii)+mu*opinion(jj)
            endif

            ! recalculate the plusagents and minusagents arrays
            kp=0
            km=0
            plusagents = 0
            minusagents = 0
            do ii=1,nagents
                if(opinion(ii).ge.0.d0) then
                    kp=kp+1
                    plusagents(kp)=ii 
                else
                    km=km+1
                    minusagents(km)=ii 
                end if
            enddo ! over ii nagents            
            maxplusagent =kp
            maxminusagent=km
        enddo ! over istep timesteps

        !recalculate opinions after all time steps
        totalopinion=0.d0
        plusopinion=0.d0
        minusopinion=0.d0
        do ii=1,nagents
            totalopinion=totalopinion+opinion(ii)/(1.d0*nagents)
        end do ! over ii nagents
        if(maxplusagent.ge.1) then
            do jj=1,maxplusagent
                plusopinion=plusopinion+opinion(plusagents(jj))/(1.d0*maxplusagent)
            enddo ! over jj
        endif
        if(maxminusagent.ge.1) then
            do jj=1,maxminusagent
                minusopinion=minusopinion+opinion(minusagents(jj))/(1.d0*maxminusagent)
            enddo ! over jj
        endif
        ! screen output of initial data
        write(*,*)"NPLUS=",maxplusagent," NMINUS",maxminusagent," TOT opinion",totalopinion,&
            "PLUS opinion",plusopinion," MINUS opinion",minusopinion

        ! calculate partial histogram for run irun
        do ii=1,nagents
            l=int(opinion(ii)*25)+26
            opinionhist(l,irun)=opinionhist(l,irun)+1
        enddo ! over ii
        










    enddo ! over runs

    do irun=1,nruns
        do l=1,51
            opinionhistaver(l,ipstep)=opinionhistaver(l,ipstep)+opinionhist(l,irun)
        enddo
    end do

    write(10,*) (opinionhistaver(l,ipstep),l=1,51)


    enddo ! over ipstep





    close(10)

end program cognpartibash
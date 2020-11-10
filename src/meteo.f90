module Meteo

    implicit none
    
    private
    
    ! Public interface
    public  :: MeteoType
    
    ! User data types
    type MeteoType
        integer, dimension(:), allocatable  :: ivTimeStamp  ! Time stamp (read)
        real, dimension(:), allocatable     :: rvVel        ! Horizontal wind speed (m/s, read)
        real, dimension(:), allocatable     :: rvDir        ! Horizontal wind direction (Â°, read)
        real, dimension(:), allocatable     :: rvTa         ! Air temperature (K, read)
        real, dimension(:), allocatable     :: rvUstar      ! Friction velocity (m/s, read)
        real, dimension(:), allocatable     :: rvH0         ! Turbulent sensible heat flux (W/m2, read)
        real, dimension(:), allocatable     :: rvZi         ! Mixing height (m, read)
        real, dimension(:), allocatable     :: rvLm1        ! Reciprocal of Obukhov length (m**-1, computed)
        real, dimension(:), allocatable     :: rvWstar      ! Deardoff velocity (m/s, read)
    contains
        procedure   :: get
        procedure   :: estimateSigmaY
        procedure   :: estimateSigmaZ
    end type MeteoType
    
contains

    function get(this, sFileName) result(iRetCode)
    
        ! Routine arguments
        class(MeteoType), intent(out)   :: this
        character(len=*), intent(in)    :: sFileName
        integer                         :: iRetCode
        
        ! Locals
        integer             :: iErrCode
        integer             :: iLUN
        integer             :: iNumLines
        character(len=80)   :: sBuffer
        integer             :: iLine
        integer             :: iYear, iMonth, iDay, iHour, iMinute, iSecond
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Count lines and reserve workspace
        iNumLines = -1
        open(newunit=iLUN, file=sFileName, status='old', action='read', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 1
            return
        end if
        do
            read(iLUN, "(a)", iostat=iErrCode) sBuffer
            if(iErrCode /= 0) exit
            iNumLines = iNumLines + 1
        end do
        if(iNumLines <= 0) then
            iRetCode = 2
            return
        end if
        if(allocated(this % ivTimeStamp)) deallocate(this % ivTimeStamp)
        if(allocated(this % rvVel))       deallocate(this % rvVel)
        if(allocated(this % rvDir))       deallocate(this % rvDir)
        if(allocated(this % rvTa))        deallocate(this % rvTa)
        if(allocated(this % rvUstar))     deallocate(this % rvUstar)
        if(allocated(this % rvH0))        deallocate(this % rvH0)
        if(allocated(this % rvZi))        deallocate(this % rvZi)
        if(allocated(this % rvLm1))       deallocate(this % rvLm1)
        if(allocated(this % rvWstar))     deallocate(this % rvWstar)
        allocate(this % ivTimeStamp(iNumLines))
        allocate(this % rvVel(iNumLines))
        allocate(this % rvDir(iNumLines))
        allocate(this % rvTa(iNumLines))
        allocate(this % rvUstar(iNumLines))
        allocate(this % rvH0(iNumLines))
        allocate(this % rvZi(iNumLines))
        allocate(this % rvLm1(iNumLines))
        allocate(this % rvWstar(iNumLines))
        
        ! Read actual data
        rewind(iLUN)
        read(iLUN, "(a)") sBuffer
        do iLine = 1, iNumLines
            read(iLUN, "(a)") sBuffer
            read(sBuffer(1:19), "(i4,5(1x,i2))", iostat=iErrCode) iYear, iMonth, iDay, iHour, iMinute, iSecond
            if(iErrCode /= 0) then
                iRetCode = 3
                close(iLUN)
                return
            end if
            read(sBuffer(21:), *, iostat=iErrCode) &
                this % rvVel(iLine), &
                this % rvDir(iLine), &
                this % rvTa(iLine), &
                this % rvUstar(iLine), &
                this % rvH0(iLine), &
                this % rvZi(iLine)
        end do
        
        ! Compute basic turbulence indices
        this % rvLm1 = -this % rvH0 / (305.904 * this % rvUstar**3 * this % rvTa)
        where(this % rvH0 > 0.)
            this % rvWstar = (0.0081725 * this % rvH0 * this % rvZi / this % rvTa) ** (1./3.)
        elsewhere
            this % rvWstar = 0.
        endwhere
        
    end function get
    
    
    function estimateSigmaY( &
        this, &     ! Current meteo set
        iStep, &    ! Current step index
        rX, &       ! Downwind distance (m)
        rSigmaY &   ! Output value of Sigma(Y) (m)
    ) result(iRetCode)
    
        ! Routine arguments
        class(MeteoType), intent(in)    :: this
        integer, intent(in)             :: iStep
        real, intent(in)                :: rX
        real, intent(out)               :: rSigmaY
        integer                         :: iRetCode
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Validate parameters
        if(iStep < 1 .or. iStep > size(this % ivTimeStamp)) then
            iRetCode = 1
            return
        end if
        
        ! Compute the Sigma(Y)
        if(rX <= 0.) then
            rSigmaY = 0.
        else
            rSigmaY = (rX / this % rvVel(iStep)) * &
                      sqrt( &
                        this % rvUstar(iStep) ** 2 + &
                        0.25 * this % rvWstar(iStep) ** 2 / ( &
                            1. + 0.9 * rX * this % rvWstar(iStep) / (this % rvZi(iStep) * this % rvVel(iStep)) &
                        ) &
                      )
        end if
        
    end function estimateSigmaY
    
    
    function estimateSigmaZ( &
        this, &
        iStep, &
        rX, &
        rHm, &
        rSigmaZ &
    ) result(iRetCode)
    
        ! Routine arguments
        class(MeteoType), intent(in)    :: this
        integer, intent(in)             :: iStep
        real, intent(in)                :: rX
        real, intent(in)                :: rHm
        real, intent(out)               :: rSigmaZ
        integer                         :: iRetCode
        
        ! Locals
        real    :: rWstar
        real    :: rSigmaC2_1
        real    :: rSigmaC2_2
        real    :: rSigmaC2_3
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Validate parameters
        if(iStep < 1 .or. iStep > size(this % ivTimeStamp)) then
            iRetCode = 1
            return
        end if
        
        ! Transfer vector data to scalars (just for human use: so the cumbersome
        ! formulae become a bit better
        rWstar = this % rvWstar(iStep)
        rZi    = this % rvZi(iStep)
        rVel   = this % rvVel(iStep)
        
        ! Compute the Sigma(Y)
        if(rX <= 0.) then
            rSigmaZ = 0.
        else
            rSigmaC2_1 = 1.54 * rWstar**2 * (rHm / rZi)**2./3. * (rX/rVel)**2
        end if
        
    end function estimateSigmaZ

end module Meteo

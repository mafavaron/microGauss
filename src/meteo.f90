module Meteo

    implicit none
    
    private
    
    ! Public interface
    public  :: MeteoType
    
    ! User data types
    type MeteoType
        integer, dimension(:), allocatable  :: ivTimeStamp  ! Time stamp (read)
        real, dimension(:), allocatable     :: rvVel        ! Horizontal wind speed (m/s, read)
        real, dimension(:), allocatable     :: rvDir        ! Horizontal wind direction (°, read)
        real, dimension(:), allocatable     :: rvTa         ! Air temperature (K, read)
        real, dimension(:), allocatable     :: rvUstar      ! Friction velocity (m/s, read)
        real, dimension(:), allocatable     :: rvH0         ! Turbulent sensible heat flux (W/m2, read)
        real, dimension(:), allocatable     :: rvZi         ! Mixing height (m, read)
        real, dimension(:), allocatable     :: rvLm1        ! Reciprocal of Obukhov length (m**-1, computed)
        real, dimension(:), allocatable     :: rvWs         ! Deardoff velocity (m/s, read)
        real, dimension(:), allocatable     :: rvSigmaU     ! Horizontal natural sigma (m/s, computed)
        real, dimension(:), allocatable     :: rvSigmaW     ! Vertical natural sigma (m/s, computed)
    contains
        procedure   :: get
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
        if(allocated(this % rvWs))        deallocate(this % rvWs)
        allocate(this % ivTimeStamp(iNumLines))
        allocate(this % rvVel(iNumLines))
        allocate(this % rvDir(iNumLines))
        allocate(this % rvTa(iNumLines))
        allocate(this % rvUstar(iNumLines))
        allocate(this % rvH0(iNumLines))
        allocate(this % rvZi(iNumLines))
        allocate(this % rvLm1(iNumLines))
        allocate(this % rvWs(iNumLines))
        
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
        
        ! Compute the remaining columns
        this % rvLm1 = -this % rvH0 / (305.904 * this % rvUstar**3 * this % rvTa)
        where(this % rvH0 > 0.)
            this % rvWs = (0.0081725 * this % rvH0 * this % rvZi / this % rvTa) ** (1./3.)
        elsewhere
            this % rvWs = 0.
        endwhere
        
    end function get

end module Meteo

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
        real, dimension(:), allocatable     :: rvTemp       ! Air temperature (°C, read)
        real, dimension(:), allocatable     :: rvUstar      ! Friction velocity (m/s, read)
        real, dimension(:), allocatable     :: rvH0         ! Turbulent sensible heat flux (W/m2, read)
        real, dimension(:), allocatable     :: rvZi         ! Mixing height (m, read)
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
        if(allocated(this % rvTemp))      deallocate(this % rvTemp)
        if(allocated(this % rvUstar))     deallocate(this % rvUstar)
        if(allocated(this % rvH0))        deallocate(this % rvH0)
        if(allocated(this % rvZi))        deallocate(this % rvZi)
        allocate(this % ivTimeStamp(iNumLines))
        allocate(this % rvVel(iNumLines))
        allocate(this % rvDir(iNumLines))
        allocate(this % rvTemp(iNumLines))
        allocate(this % rvUstar(iNumLines))
        allocate(this % rvH0(iNumLines))
        allocate(this % rvZi(iNumLines))
        
    end function get

end module Meteo

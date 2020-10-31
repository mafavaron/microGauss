module Meteo

    implicit none
    
    private
    
    ! Public interface
    public  :: MeteoType
    
    ! User data types
    type MeteoType
        integer, dimension(:), allocatable  :: ivTimeStamp
        real, dimension(:), allocatable     :: rvVel
        real, dimension(:), allocatable     :: rvDir
        real, dimension(:), allocatable     :: rvTemp
        real, dimension(:), allocatable     :: rvUstar
        real, dimension(:), allocatable     :: rvH0
        real, dimension(:), allocatable     :: rvZi
    contains
        procedure   :: get
    end type MeteoType
    
contains

    function get(this, sFileName) result(iRetCode
    
        ! Routine arguments
        class(MeteoType), intent(out)   :: this
        character(len=*), intent(in)    :: sFileName
        integer                         :: iRetCode
        
        ! Locals
        integer :: iErrCode
        integer :: iLUN
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
    end function get

end module Meteo

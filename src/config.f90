module Config

    implicit none
    
    private
    
    ! Public interface
    public  :: ConfigType
    
    ! User types
    
    type ConfigType
        ! General
        character(len=256)  :: sRunName
        character(len=256)  :: sOutFile
        ! Receptor grid
        real(8)             :: rX0  ! X coordinate of SW receptor in grid (read)
        real(8)             :: rY0  ! Y coordinate of SW receptor in grid (read)
        real(8)             :: rX1  ! X coordinate of NE receptor in grid (computed)
        real(8)             :: rY1  ! Y coordinate of NE receptor in grid (computed)
        integer             :: iNx  ! Number of receptors along X direction (read)
        integer             :: iNy  ! Number of receptors along Y direction (read)
        real(8)             :: rDxy ! Grid spacing (read)
        ! Emission
        real(8)             :: rXe  ! Source X coordinate (read)
        real(8)             :: rYe  ! Source Y coordinate (read)
        real                :: rHe  ! Height of stack tip above ground (read)
        real                :: rDe  ! Stack tip diameter (read)
        real                :: rVe  ! Initial speed of exhausts at stack tip (read)
        real                :: rTe  ! Initial temperature of exhaust at stack tip (read)
        logical             :: lIn  ! .true. if source is within receptor grid (computed)
        ! Meteo data
        character(len=256)  :: sMeteo   ! Name of meteo input file
    contains
        procedure           :: get
        procedure           :: complete
    end type ConfigType
    
contains

    function get(this, sIniFile) result(iRetCode)
    
        ! Routine arguments
        class(ConfigType), intent(out)  :: this
        character(len=*), intent(in)    :: sIniFile
        integer                         :: iRetCode
        
        ! Locals
        integer             :: iErrCode
        integer             :: iLUN
        character(len=256)  :: sRunName
        character(len=256)  :: sOutFile
        real(8)             :: rX0
        real(8)             :: rY0
        integer             :: iNx
        integer             :: iNy
        real(8)             :: rDxy
        real(8)             :: rXe
        real(8)             :: rYe
        real                :: rHe
        real                :: rDe
        real                :: rVe
        real                :: rTe
        character(len=256)  :: sMeteo
        
        ! Namelists
        namelist /General/ sRunName, sOutFile
        namelist /Receptors/ rX0, rY0, iNx, iNy, rDxy
        namelist /Emission/ rXe, rYe, rHe, rDe, rVe, rTe
        namelist /Meteorology/ sMeteo
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Gather configuration data from file
        open(newunit=iLUN, file=sIniFile, status='old', action='read', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 1
            return
        end if
        read(iLUN, nml=General, iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 2
            return
        end if
        read(iLUN, nml=Receptors, iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 3
            return
        end if
        read(iLUN, nml=Emission, iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 4
            return
        end if
        read(iLUN, nml=Meteorology, iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 5
            return
        end if
        close(iLUN)
        
        ! Transfer the data read to configuration object
        this % sRunName = sRunName
        this % sOutFile = sOutFile
        this % rX0      = rX0
        this % rY0      = rY0
        this % iNx      = iNx
        this % iNy      = iNy
        this % rDxy     = rDxy
        this % rXe      = rXe
        this % rYe      = rYe
        this % rHe      = rHe
        this % rDe      = rDe
        this % rVe      = rVe
        this % rTe      = rTe
        this % sMeteo   = sMeteo
        
    end function get
    
    
    function complete(this) result(iRetCode)
    
        ! Routine arguments
        class(ConfigType), intent(inout)    :: this
        integer                             :: iRetCode
        
        ! Locals
        
        ! Assume success (will falsify on failure
        iRetCode = 0
        
        ! Compute the configuration's missing elements
        this % rX1 = this % rX0 + (this % iNx - 1) * this % rDxy
        this % rY1 = this % rY0 + (this % iNy - 1) * this % rDxy
        this % lIn = (this % rX0 <= this % rXe .and. this % rXe <= this % rX1) .and. &
                     (this % rY0 <= this % rYe .and. this % rYe <= this % rY1)
        
    end function complete

end module Config

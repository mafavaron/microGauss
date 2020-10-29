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
        real(8)             :: rX0
        real(8)             :: rY0
        integer             :: iNx
        integer             :: iNy
        real(8)             :: rDxy
        ! Emission
        real(8)             :: rXe
        real(8)             :: rYe
        real                :: rHe
        real                :: rDe
        real                :: rVe
        real                :: rTe
        ! Meteo data
        character(len=256)  :: sMeteo
    contains
        procedure           :: get
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
        open(newunit=iLUN, status='old', action='read', iostat=iErrCode)
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

end module Config

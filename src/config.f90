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

end module Config

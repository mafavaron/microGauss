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
    end type ConfigType

end module Config

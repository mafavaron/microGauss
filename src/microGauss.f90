! microGauss - An experiment-oriented simplified Gaussian dispersion model
!
! This is open-source code, covered by the MIT license.
!
! Author: Patrizia Favaron
!
program microGauss

    use config
    
    implicit none
    
    ! Locals
    integer             :: iRetCode
    character(len=256)  :: sIniFile
    type(ConfigType)    :: tCfg
    
    ! Get command line parameters
    if(command_argument_count() /= 1) then
        print *, "microGauss - An experiment-oriented simplified"
        print *, "             Gaussian dispersion model"
        print *
        print *, "Usage:"
        print *
        print *, "  ./microGauss <Ini_File>"
        print *
        print *, "This is open-source software, covered by the MIT license."
        print *
        print *, "Author: Patrizia Favaron"
        print *
        stop
    end if
    call get_command_argument(1, sIniFile)
    
    ! Get configuration data
    iRetCode = tCfg % get(sIniFile)
    if(iRetCode /= 0) then
        print *, "microGauss:: error: Invalid configuration - Ret.code = ", iRetCode
        stop
    end if
    
    ! Complete and check configuration
    iRetCode = tCfg % complete()
    if(iRetCode == 1) then
        print *, "microGauss:: error: Source is outside the receptors grid"
        stop
    elseif(iRetCode == 2) then
        print *, "microGauss:: error: Receptors gridis empty"
        stop
    end if
    
    ! Leave
    print *, "*** End Job ***"

end program microGauss

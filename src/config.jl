"""
# module config

- Julia version: 1.5.1
- Author: Patrizia Favaron
- Date: 2020-10-28

# Examples

```jldoctest
julia>
```
"""

using IniFile

module config

    struct Config
        sRunName::string
        sOutFile::string
        rX0::Float64
        rY0::Float64
        iNx::Int64
        iNy::Int64
        rDxy::Float64
        rXe::Float64
        rYe::Float64
        rHe::Float64
        rVe::Float64
        rDe::Float64
        rTe::Float64
        sMeteo::string
    end

    function getConfig(sConfigFileName::string)

        # Assume success (will falsify on failure)
        iRetCode = 0
        sErrMsg  = "Success"

        # Get configuration data
        ini = read(IniFile(), ini_file)
        sRunName = get(ini, "General", "Run", "")
        sOutFile = get(ini, "General", "OutFile", "")
        if sOutFile == ""
            iRetCode = 1
            sErrMsg  = "Output file name is missing"
        end
        rX0      = get(ini, "Receptors", "X0", missing)
        rY0      = get(ini, "Receptors", "Y0", missing)
        iNx      = get(ini, "Receptors", "NX", missing)
        iNy      = get(ini, "Receptors", "NY", missing)
        rDxy     = get(ini, "Receptors", "DXY", missing)
        rXe      = get(ini, "Emission", "Xe", missing)
        rYe      = get(ini, "Emission", "Ye", missing)
        rHe      = get(ini, "Emission", "He", missing)
        rVe      = get(ini, "Emission", "Ve", missing)
        rDe      = get(ini, "Emission", "De", missing)
        rTe      = get(ini, "Emission", "Te", missing)
        sMeteo   = get(ini, "Meteorology", "MeteoFile", missing)
        println(sMeteo)

    end

end

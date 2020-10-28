#=
miniGauss.jl:
- Julia version: 1.5.1
- Author: Patrizia Favaron
- Date: 2020-10-27
=#

# Main program for 'microGauss' toy Gaussian dispersion model.

using ConfParser
using DataFrames
using CSV

# Get command line parameters
num_args = length(ARGS)
if num_args != 1
    println("microGauss.jl - Toy Gaussian dispersion model")
    println("")
    println("Usage:")
    println("")
    println("  julia microGauss.jl <config_file>")
    println("")
    println("This is open-source software, covered by the MIT License")
    println("")
    println("Written by: Patrizia Favaron")
    println("")
    exit()
end
ini_file = ARGS[1]

# Get configuration data
try
    config = ConfParse(ini_file)
    parse_conf!(config)
    sRunName = retrieve(config, "General", "Run")
    sOutFile = retrieve(config, "General", "OutFile")
    sRunName = retrieve(config, "General", "Run")
    rX0      = retrieve(config, "Receptors", "X0")
    rY0      = retrieve(config, "Receptors", "Y0")
    iNx      = retrieve(config, "Receptors", "NX")
    iNy      = retrieve(config, "Receptors", "NY")
    rDxy     = retrieve(config, "Receptors", "DXY")
    rXe      = retrieve(config, "Emission", "Xe")
    rYe      = retrieve(config, "Emission", "Ye")
    rHe      = retrieve(config, "Emission", "He")
    rVe      = retrieve(config, "Emission", "Ve")
    rDe      = retrieve(config, "Emission", "De")
    rTe      = retrieve(config, "Emission", "Te")
    sMeteo   = retrieve(config, "Meteorology", "MeteoFile")
catch
    println("miniGauss:: Error: Configuration file is missing or invalid")
    exit(1)
end

# Get meteo data as a data-frame
try
    dfMeteo = CSV.read(sMeteo)
catch
    println("miniGauss:: Error: Meteo file is missing or invalid")
    exit(2)
end

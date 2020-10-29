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

# Get meteo data as a data-frame
try
    global dfMeteo = CSV.read(sMeteo, DataFrame)
catch
    println("miniGauss:: Error: Meteo file is missing or invalid")
    exit(2)
end

println(dfMeteo)

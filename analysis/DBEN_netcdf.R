# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(ncdf4)
library(ncdf4.helpers)
library(tidyverse)


# Read example netCDF file ####

# open a netCDF file
example_netcdf <- nc_open("/home/laura/Documents/Collaborations/DBEN/example_output_for_netcdf_formatting/LPJGUESS_BA_S1_FIN2_fullcolumn.nc")
example_netcdf
attributes(example_netcdf$var)
attributes(example_netcdf$dim)
# get dimensions
Time <- ncvar_get(example_netcdf, "Time")
pft <- ncvar_get(example_netcdf, "pft")
# get variables
ba_var <- ncvar_get(example_netcdf,"BA")
dlname <- ncatt_get(example_netcdf,"BA","long_name")
dunits <- ncatt_get(example_netcdf,"BA","units")
fillvalue <- ncatt_get(example_netcdf,"BA","_FillValue")

# open a netCDF file
var1_netcdf <- nc_open("/home/laura/rsofun/data/outputs_mod/nc_files/412ppm/BIA/BiomeEP_BAgrowth_P0_BIA_412ppm.nc")
var1_netcdf <- nc_open("/home/laura/rsofun/data/outputs_mod/nc_files/412ppm/BIA/BiomeEP_nbp_P0_BIA_412ppm.nc")
var1_netcdf <- nc_open("/home/laura/rsofun/data/outputs_mod/nc_files/412ppm/FIN/BiomeEP_cwood_size_P0_FIN_412ppm.nc")
var1_netcdf <- nc_open("/home/laura/rsofun/data/outputs_mod/nc_files/412ppm/FIN/BiomeEP_cveg_P0_FIN_412ppm.nc")
var1_netcdf <- nc_open("/home/laura/rsofun/data/outputs_mod/nc_files/412ppm/FIN/BiomeEP_stemmort_pft_P0_FIN_412ppm.nc")
var1_netcdf
attributes(var1_netcdf$var)
attributes(var1_netcdf$dim)
# get dimensions
time <- ncvar_get(var1_netcdf, "time")
sizeclass <- ncvar_get(var1_netcdf, "sizeclass")
pft <- ncvar_get(var1_netcdf, "pft")
# get variables
dvar <- ncvar_get(var1_netcdf,"stemmort")
dlname <- ncatt_get(var1_netcdf,"BAgrowth","long_name")
dunits <- ncatt_get(var1_netcdf,"BAgrowth","units")
fillvalue <- ncatt_get(var1_netcdf,"BAgrowth","_FillValue")



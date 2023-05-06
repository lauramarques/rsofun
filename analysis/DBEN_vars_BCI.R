# This script saves the outputs from the model simulations

# load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(ncdf4)
library(ncdf4.helpers)

# 412 ppm ####

# P0 ####
# Baseline run BCI  
BiomeE_P0_BCI_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BCI_aCO2_annual_tile.csv")
BiomeE_P0_BCI_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BCI_aCO2_annual_cohorts.csv")

PFT_species <- c("Broadleaf_evergreen_shade_int_PFT5","Broadleaf_evergreen_shade_tol_PFT6",
                 "Broadleaf_deciduous_PFT7","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(5,6,7,8)
PFT_BCI <- tibble(PFT_species,PFT,PFT_reorder)
PFT_BCI

# Calculate deathrate and wood lifespan
BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT) %>%
  summarise(deathrate=mean(deathrate)) %>%
  mutate(wood_lifespan=1/deathrate)

# Pools ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cveg_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground woody biomass ####
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGcwood <- BiomeE_P0_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGcwood = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGcwood) 
AGcwood_wid <- AGcwood %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_AGcwood_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                      "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cwood_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>% ungroup()
cwood_size_wid <- cwood_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cwood_size_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                       "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% ungroup()
nstem_size_wid <- nstem_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_nstem_size_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_lai_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                       "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_CA_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                     "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_BA_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_height_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                    "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# Fluxes ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_WBgrowth_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                        "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_BAgrowth_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cmort_wid <- cmort %>% select(c(year,cmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cmort_pft_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

cmort <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% ungroup()
cmort_wid <- cmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cmort_size_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                            "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>% 
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
stemmort_wid <- stemmort %>% select(c(year,stemmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_stemmort_pft_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

stemmort <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% ungroup()
stemmort_wid <- stemmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_stemmort_size_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_gpp_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_npp_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_P0_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_nbp_P0_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)

# PS1  ####
BiomeE_PS1_BCI_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS1_BCI_aCO2_annual_tile.csv")
BiomeE_PS1_BCI_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS1_BCI_aCO2_annual_cohorts.csv")

PFT_species <- c("Broadleaf_evergreen_shade_int_PFT5","Broadleaf_evergreen_shade_tol_PFT6",
                 "Broadleaf_deciduous_PFT7","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(5,6,7,8)
PFT_BCI <- tibble(PFT_species,PFT,PFT_reorder)
PFT_BCI

# Pools ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cveg_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground woody biomass ####
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGcwood <- BiomeE_PS1_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS1_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGcwood = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGcwood) 
AGcwood_wid <- AGcwood %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_AGcwood_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                     "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cwood_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                       "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>% ungroup()
cwood_size_wid <- cwood_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[100,150)`=0,`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cwood_size_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                            "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% ungroup()
nstem_size_wid <- nstem_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_nstem_size_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_lai_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                     "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_CA_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_BA_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_height_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                        "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# Fluxes ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_WBgrowth_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_BAgrowth_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cmort_wid <- cmort %>% select(c(year,cmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cmort_pft_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

cmort <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% ungroup()
cmort_wid <- cmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cmort_size_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>% 
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
stemmort_wid <- stemmort %>% select(c(year,stemmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_stemmort_pft_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

stemmort <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% ungroup()
stemmort_wid <- stemmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_stemmort_size_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_gpp_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_PS1_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_npp_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_PS1_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS1_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_nbp_PS1_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)

# PS2  ####
BiomeE_PS2_BCI_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS2_BCI_aCO2_annual_tile.csv")
BiomeE_PS2_BCI_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS2_BCI_aCO2_annual_cohorts.csv")

PFT_species <- c("Broadleaf_evergreen_shade_int_PFT5","Broadleaf_evergreen_shade_tol_PFT6",
                 "Broadleaf_deciduous_PFT7","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(5,6,7,8)
PFT_BCI <- tibble(PFT_species,PFT,PFT_reorder)
PFT_BCI

# Pools ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cveg_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground woody biomass ####
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGcwood <- BiomeE_PS2_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS2_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGcwood = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGcwood) 
AGcwood_wid <- AGcwood %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_AGcwood_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                     "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cwood_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                       "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>% ungroup()
cwood_size_wid <- cwood_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cwood_size_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                            "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% ungroup()
nstem_size_wid <- nstem_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_nstem_size_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_lai_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                     "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_CA_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_BA_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_height_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                        "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# Fluxes ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_WBgrowth_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_BAgrowth_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cmort_wid <- cmort %>% select(c(year,cmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cmort_pft_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

cmort <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% ungroup()
cmort_wid <- cmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cmort_size_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>% 
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
stemmort_wid <- stemmort %>% select(c(year,stemmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_stemmort_pft_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

stemmort <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% ungroup()
stemmort_wid <- stemmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_stemmort_size_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_gpp_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_PS2_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_npp_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_PS2_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS2_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_nbp_PS2_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)

# PS3  ####
BiomeE_PS3_BCI_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS3_BCI_aCO2_annual_tile.csv")
BiomeE_PS3_BCI_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS3_BCI_aCO2_annual_cohorts.csv")

PFT_species <- c("Broadleaf_evergreen_shade_int_PFT5","Broadleaf_evergreen_shade_tol_PFT6",
                 "Broadleaf_deciduous_PFT7","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(5,6,7,8)
PFT_BCI <- tibble(PFT_species,PFT,PFT_reorder)
PFT_BCI

# Pools ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cveg_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground woody biomass ####
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGcwood <- BiomeE_PS3_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS3_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGcwood = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGcwood) 
AGcwood_wid <- AGcwood %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_AGcwood_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                     "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cwood_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                       "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>% ungroup()
cwood_size_wid <- cwood_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cwood_size_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                            "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% ungroup()
nstem_size_wid <- nstem_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_nstem_size_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_lai_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                     "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_CA_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_BA_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_height_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                        "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# Fluxes ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_WBgrowth_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_BAgrowth_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cmort_wid <- cmort %>% select(c(year,cmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cmort_pft_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

cmort <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% ungroup()
cmort_wid <- cmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cmort_size_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>% 
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
stemmort_wid <- stemmort %>% select(c(year,stemmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_stemmort_pft_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

stemmort <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% ungroup()
stemmort_wid <- stemmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_stemmort_size_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_gpp_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_PS3_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_npp_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_PS3_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS3_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_nbp_PS3_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)

# PS4  ####
BiomeE_PS4_BCI_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS4_BCI_aCO2_annual_tile.csv")
BiomeE_PS4_BCI_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS4_BCI_aCO2_annual_cohorts.csv")

PFT_species <- c("Broadleaf_evergreen_shade_int_PFT5","Broadleaf_evergreen_shade_tol_PFT6",
                 "Broadleaf_deciduous_PFT7","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(5,6,7,8)
PFT_BCI <- tibble(PFT_species,PFT,PFT_reorder)
PFT_BCI

# Pools ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cveg_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground woody biomass ####
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGcwood <- BiomeE_PS4_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS4_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGcwood = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGcwood) 
AGcwood_wid <- AGcwood %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_AGcwood_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                     "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cwood_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                       "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>% ungroup()
cwood_size_wid <- cwood_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cwood_size_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                            "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% ungroup()
nstem_size_wid <- nstem_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_nstem_size_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_lai_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                     "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_CA_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_BA_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_height_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                        "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# Fluxes ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_WBgrowth_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_BAgrowth_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cmort_wid <- cmort %>% select(c(year,cmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cmort_pft_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

cmort <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% ungroup()
cmort_wid <- cmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cmort_size_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>% 
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
stemmort_wid <- stemmort %>% select(c(year,stemmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_stemmort_pft_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

stemmort <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% ungroup()
stemmort_wid <- stemmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_stemmort_size_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_gpp_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_PS4_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_npp_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_PS4_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS4_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_nbp_PS4_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)

# PS5  ####
BiomeE_PS5_BCI_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS5_BCI_aCO2_annual_tile.csv")
BiomeE_PS5_BCI_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS5_BCI_aCO2_annual_cohorts.csv")

PFT_species <- c("Broadleaf_evergreen_shade_int_PFT5","Broadleaf_evergreen_shade_tol_PFT6",
                 "Broadleaf_deciduous_PFT7","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(5,6,7,8)
PFT_BCI <- tibble(PFT_species,PFT,PFT_reorder)
PFT_BCI

# Pools ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year)%>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cveg_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground woody biomass ####
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGcwood <- BiomeE_PS5_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS5_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGcwood = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGcwood) 
AGcwood_wid <- AGcwood %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_AGcwood_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                     "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cwood_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                       "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>% ungroup()
cwood_size_wid <- cwood_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cwood_size_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                            "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% ungroup()
nstem_size_wid <- nstem_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_nstem_size_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_lai_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                     "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_CA_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_BA_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_height_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                        "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# Fluxes ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_WBgrowth_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_BAgrowth_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cmort_wid <- cmort %>% select(c(year,cmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cmort_pft_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

cmort <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% ungroup()
cmort_wid <- cmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cmort_size_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>% 
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
stemmort_wid <- stemmort %>% select(c(year,stemmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_stemmort_pft_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

stemmort <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% ungroup()
stemmort_wid <- stemmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_stemmort_size_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_gpp_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_PS5_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_npp_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_PS5_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS5_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_nbp_PS5_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)

# PS6  ####
BiomeE_PS6_BCI_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS6_BCI_aCO2_annual_tile.csv")
BiomeE_PS6_BCI_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS6_BCI_aCO2_annual_cohorts.csv")

PFT_species <- c("Broadleaf_evergreen_shade_int_PFT5","Broadleaf_evergreen_shade_tol_PFT6",
                 "Broadleaf_deciduous_PFT7","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(5,6,7,8)
PFT_BCI <- tibble(PFT_species,PFT,PFT_reorder)
PFT_BCI

# Pools ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cveg_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground woody biomass ####
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGcwood <- BiomeE_PS6_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS6_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGcwood = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGcwood) 
AGcwood_wid <- AGcwood %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_AGcwood_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                     "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cwood_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                       "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>% ungroup()
cwood_size_wid <- cwood_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cwood_size_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                            "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% ungroup()
nstem_size_wid <- nstem_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_nstem_size_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_lai_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                     "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_CA_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_BA_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_height_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                        "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# Fluxes ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_WBgrowth_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_BAgrowth_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cmort_wid <- cmort %>% select(c(year,cmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cmort_pft_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

cmort <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% ungroup()
cmort_wid <- cmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_cmort_size_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>% 
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
stemmort_wid <- stemmort %>% select(c(year,stemmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_stemmort_pft_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

stemmort <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% ungroup()
stemmort_wid <- stemmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_stemmort_size_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_gpp_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_PS6_BCI_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_npp_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_PS6_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS6_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/BCI/", 
                 "BiomeEP_nbp_PS6_BCI_412ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)

#________________####
# 562 ppm ####

# P0 ####
# Baseline run BCI  
BiomeE_P0_BCI_eCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/562ppm/BiomeE_P0_BCI_eCO2_annual_tile.csv")
BiomeE_P0_BCI_eCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/562ppm/BiomeE_P0_BCI_eCO2_annual_cohorts.csv")

PFT_species <- c("Broadleaf_evergreen_shade_int_PFT5","Broadleaf_evergreen_shade_tol_PFT6",
                 "Broadleaf_deciduous_PFT7","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(5,6,7,8)
PFT_BCI <- tibble(PFT_species,PFT,PFT_reorder)
PFT_BCI

# Pools ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cveg_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground woody biomass ####
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGcwood <- BiomeE_P0_BCI_eCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_BCI_eCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGcwood = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGcwood) 
AGcwood_wid <- AGcwood %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_AGcwood_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                     "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cwood_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                       "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>% ungroup()
cwood_size_wid <- cwood_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cwood_size_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                            "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% ungroup()
nstem_size_wid <- nstem_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_nstem_size_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_lai_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                     "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_CA_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_BA_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_height_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                        "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# Fluxes ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_WBgrowth_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_BAgrowth_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cmort_wid <- cmort %>% select(c(year,cmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cmort_pft_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

cmort <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% ungroup()
cmort_wid <- cmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cmort_size_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>% 
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
stemmort_wid <- stemmort %>% select(c(year,stemmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_stemmort_pft_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

stemmort <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% ungroup()
stemmort_wid <- stemmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_stemmort_size_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_gpp_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_P0_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_npp_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_P0_BCI_eCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_BCI_eCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_nbp_P0_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)

# PS1  ####
BiomeE_PS1_BCI_eCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/562ppm/BiomeE_PS1_BCI_eCO2_annual_tile.csv")
BiomeE_PS1_BCI_eCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/562ppm/BiomeE_PS1_BCI_eCO2_annual_cohorts.csv")

PFT_species <- c("Broadleaf_evergreen_shade_int_PFT5","Broadleaf_evergreen_shade_tol_PFT6",
                 "Broadleaf_deciduous_PFT7","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(5,6,7,8)
PFT_BCI <- tibble(PFT_species,PFT,PFT_reorder)
PFT_BCI

# Pools ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cveg_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground woody biomass ####
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGcwood <- BiomeE_PS1_BCI_eCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS1_BCI_eCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGcwood = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGcwood) 
AGcwood_wid <- AGcwood %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_AGcwood_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                     "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year)%>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cwood_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                       "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>% ungroup()
cwood_size_wid <- cwood_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cwood_size_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                            "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% ungroup()
nstem_size_wid <- nstem_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_nstem_size_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_lai_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                     "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_CA_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_BA_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_height_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                        "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# Fluxes ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_WBgrowth_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_BAgrowth_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cmort_wid <- cmort %>% select(c(year,cmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cmort_pft_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

cmort <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% ungroup()
cmort_wid <- cmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cmort_size_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>% 
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
stemmort_wid <- stemmort %>% select(c(year,stemmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_stemmort_pft_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

stemmort <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% ungroup()
stemmort_wid <- stemmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_stemmort_size_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_gpp_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_PS1_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_npp_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_PS1_BCI_eCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS1_BCI_eCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_nbp_PS1_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)

# PS2  ####
BiomeE_PS2_BCI_eCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/562ppm/BiomeE_PS2_BCI_eCO2_annual_tile.csv")
BiomeE_PS2_BCI_eCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/562ppm/BiomeE_PS2_BCI_eCO2_annual_cohorts.csv")

PFT_species <- c("Broadleaf_evergreen_shade_int_PFT5","Broadleaf_evergreen_shade_tol_PFT6",
                 "Broadleaf_deciduous_PFT7","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(5,6,7,8)
PFT_BCI <- tibble(PFT_species,PFT,PFT_reorder)
PFT_BCI

# Pools ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cveg_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground woody biomass ####
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGcwood <- BiomeE_PS2_BCI_eCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS2_BCI_eCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGcwood = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGcwood) 
AGcwood_wid <- AGcwood %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_AGcwood_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                     "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cwood_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                       "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>% ungroup()
cwood_size_wid <- cwood_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cwood_size_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                            "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% ungroup()
nstem_size_wid <- nstem_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_nstem_size_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_lai_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                     "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_CA_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_BA_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_height_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                        "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# Fluxes ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_WBgrowth_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_BAgrowth_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cmort_wid <- cmort %>% select(c(year,cmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cmort_pft_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

cmort <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% ungroup()
cmort_wid <- cmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cmort_size_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>% 
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
stemmort_wid <- stemmort %>% select(c(year,stemmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_stemmort_pft_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

stemmort <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% ungroup()
stemmort_wid <- stemmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_stemmort_size_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_gpp_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_PS2_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_npp_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_PS2_BCI_eCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS2_BCI_eCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_nbp_PS2_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)

# PS3  ####
BiomeE_PS3_BCI_eCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/562ppm/BiomeE_PS3_BCI_eCO2_annual_tile.csv")
BiomeE_PS3_BCI_eCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/562ppm/BiomeE_PS3_BCI_eCO2_annual_cohorts.csv")

PFT_species <- c("Broadleaf_evergreen_shade_int_PFT5","Broadleaf_evergreen_shade_tol_PFT6",
                 "Broadleaf_deciduous_PFT7","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(5,6,7,8)
PFT_BCI <- tibble(PFT_species,PFT,PFT_reorder)
PFT_BCI

# Pools ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cveg_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground woody biomass ####
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGcwood <- BiomeE_PS3_BCI_eCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS3_BCI_eCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGcwood = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGcwood) 
AGcwood_wid <- AGcwood %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_AGcwood_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                     "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cwood_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                       "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>% ungroup()
cwood_size_wid <- cwood_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cwood_size_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                            "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% ungroup()
nstem_size_wid <- nstem_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_nstem_size_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_lai_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                     "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_CA_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_BA_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_height_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                        "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# Fluxes ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_WBgrowth_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup() #%>% mutate(BAgrowth=ifelse(BAgrowth=="Inf",NA,BAgrowth))
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_BAgrowth_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cmort_wid <- cmort %>% select(c(year,cmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cmort_pft_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

cmort <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% ungroup()
cmort_wid <- cmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cmort_size_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>% 
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
stemmort_wid <- stemmort %>% select(c(year,stemmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_stemmort_pft_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

stemmort <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% ungroup()
stemmort_wid <- stemmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_stemmort_size_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_gpp_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_PS3_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_npp_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_PS3_BCI_eCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS3_BCI_eCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_nbp_PS3_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)

# PS4  ####
BiomeE_PS4_BCI_eCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/562ppm/BiomeE_PS4_BCI_eCO2_annual_tile.csv")
BiomeE_PS4_BCI_eCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/562ppm/BiomeE_PS4_BCI_eCO2_annual_cohorts.csv")

PFT_species <- c("Broadleaf_evergreen_shade_int_PFT5","Broadleaf_evergreen_shade_tol_PFT6",
                 "Broadleaf_deciduous_PFT7","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(5,6,7,8)
PFT_BCI <- tibble(PFT_species,PFT,PFT_reorder)
PFT_BCI

# Pools ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cveg_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground woody biomass ####
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGcwood <- BiomeE_PS4_BCI_eCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS4_BCI_eCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGcwood = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGcwood) 
AGcwood_wid <- AGcwood %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_AGcwood_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                     "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cwood_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                       "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>% ungroup()
cwood_size_wid <- cwood_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cwood_size_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                            "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% ungroup()
nstem_size_wid <- nstem_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_nstem_size_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_lai_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                     "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_CA_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_BA_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_height_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                        "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# Fluxes ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_WBgrowth_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup() #%>% mutate(BAgrowth=ifelse(BAgrowth=="Inf",NA,BAgrowth))
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_BAgrowth_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cmort_wid <- cmort %>% select(c(year,cmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cmort_pft_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

cmort <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% ungroup()
cmort_wid <- cmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cmort_size_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>% 
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
stemmort_wid <- stemmort %>% select(c(year,stemmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_stemmort_pft_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

stemmort <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% ungroup()
stemmort_wid <- stemmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_stemmort_size_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_gpp_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_PS4_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_npp_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_PS4_BCI_eCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS4_BCI_eCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_nbp_PS4_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)

# PS5  ####
BiomeE_PS5_BCI_eCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/562ppm/BiomeE_PS5_BCI_eCO2_annual_tile.csv")
BiomeE_PS5_BCI_eCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/562ppm/BiomeE_PS5_BCI_eCO2_annual_cohorts.csv")

PFT_species <- c("Broadleaf_evergreen_shade_int_PFT5","Broadleaf_evergreen_shade_tol_PFT6",
                 "Broadleaf_deciduous_PFT7","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(5,6,7,8)
PFT_BCI <- tibble(PFT_species,PFT,PFT_reorder)
PFT_BCI

# Pools ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cveg_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground woody biomass ####
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGcwood <- BiomeE_PS5_BCI_eCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS5_BCI_eCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGcwood = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGcwood) 
AGcwood_wid <- AGcwood %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_AGcwood_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                     "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cwood_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                       "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>% ungroup()
cwood_size_wid <- cwood_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cwood_size_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                            "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% ungroup()
nstem_size_wid <- nstem_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_nstem_size_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_lai_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                     "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_CA_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_BA_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_height_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                        "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# Fluxes ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_WBgrowth_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup() #%>% mutate(BAgrowth=ifelse(BAgrowth=="Inf",NA,BAgrowth))
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_BAgrowth_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cmort_wid <- cmort %>% select(c(year,cmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cmort_pft_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

cmort <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% ungroup()
cmort_wid <- cmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cmort_size_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>% 
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
stemmort_wid <- stemmort %>% select(c(year,stemmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_stemmort_pft_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

stemmort <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% ungroup()
stemmort_wid <- stemmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_stemmort_size_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_gpp_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_PS5_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_npp_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_PS5_BCI_eCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS5_BCI_eCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_nbp_PS5_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)

# PS6  ####
BiomeE_PS6_BCI_eCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/562ppm/BiomeE_PS6_BCI_eCO2_annual_tile.csv")
BiomeE_PS6_BCI_eCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/562ppm/BiomeE_PS6_BCI_eCO2_annual_cohorts.csv")

PFT_species <- c("Broadleaf_evergreen_shade_int_PFT5","Broadleaf_evergreen_shade_tol_PFT6",
                 "Broadleaf_deciduous_PFT7","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(5,6,7,8)
PFT_BCI <- tibble(PFT_species,PFT,PFT_reorder)
PFT_BCI

# Pools ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cveg_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground woody biomass ####
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGcwood <- BiomeE_PS6_BCI_eCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS6_BCI_eCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGcwood = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGcwood) 
AGcwood_wid <- AGcwood %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_AGcwood_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                     "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cwood_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                       "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>% ungroup()
cwood_size_wid <- cwood_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cwood_size_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                            "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% ungroup()
nstem_size_wid <- nstem_size %>% 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_nstem_size_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_lai_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                     "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_CA_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_BA_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_height_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                        "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# Fluxes ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_WBgrowth_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup() #%>% mutate(BAgrowth=ifelse(BAgrowth=="Inf",NA,BAgrowth))
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_BAgrowth_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
cmort_wid <- cmort %>% select(c(year,cmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cmort_pft_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

cmort <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% ungroup()
cmort_wid <- cmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = cmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_cmort_size_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>% 
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
stemmort_wid <- stemmort %>% select(c(year,stemmort,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_stemmort_pft_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

stemmort <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% ungroup()
stemmort_wid <- stemmort %>% 
  pivot_wider(names_from = dbh_bins, values_from = stemmort,values_fill = 0) %>% arrange(year) %>%
  select(-year) %>% mutate(`[150,200)`=0,`[200,250)`=0)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_stemmort_size_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
sizeclass <- as.array(c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_gpp_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_PS6_BCI_eCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_BCI) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  mutate(`1`=0,`2`=0,`3`=0,`4`=0,) %>% 
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  relocate(`4`,.after =`3`) %>% relocate(`5`,.after =`4`) %>% relocate(`6`,.after =`5`) %>% 
  relocate(`8`,.after =`7`) %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_npp_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_PS6_BCI_eCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_PS6_BCI_eCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/562ppm/BCI/", 
                 "BiomeEP_nbp_PS6_BCI_562ppm", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)





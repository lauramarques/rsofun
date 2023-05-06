library(dplyr)
library(tibble)
library(rsofun)
library(ggplot2)
library(patchwork)
library(multidplyr)
library(stringr)

#if(!require(devtools)){install.packages(devtools)}
#devtools::install_github("computationales/ingestr")
library(ingestr)

# Precipitation (kg/m2 = mm) ####
load("~/Documents/Collaborations/DBEN/cru_jra_1901-2020/allyears/forcing_sel_sites_precip.RData")
precip
precip <- precip %>% rownames_to_column(var="date") %>% 
  mutate(date=gsub('X','',date),date=gsub('\\.','-',date),date=as.Date(date),
         doy = lubridate::yday(date),month=lubridate::month(date),year=lubridate::year(date))
prec_FIN <- precip %>% select(date,doy,year, FI) %>% rename(prec=FI)
prec_BIA <- precip %>% select(date,doy,year, BIA) %>% rename(prec=BIA)
prec_BCI <- precip %>% select(date,doy,year, BCI) %>% rename(prec=BCI)

# Relative humidity (%) #### 
load("~/Documents/Collaborations/DBEN/cru_jra_1901-2020/allyears/forcing_sel_sites_rhum.RData")
rhum
rhum <- rhum %>% rownames_to_column(var="date") %>% 
  mutate(date=gsub('X','',date),date=gsub('\\.','-',date),date=as.Date(date),
         doy = lubridate::yday(date),month=lubridate::month(date),year=lubridate::year(date))
rh_FIN <- rhum %>% select(date,doy,year, FI) %>% rename(rh=FI) %>% mutate(rh=rh*100)
rh_BIA <- rhum %>% select(date,doy,year, BIA) %>% rename(rh=BIA) %>% mutate(rh=rh*100)
rh_BCI <- rhum %>% select(date,doy,year, BCI) %>% rename(rh=BCI) %>% mutate(rh=rh*100)

# Air temperature (K, converted to Celsius) ####
load("~/Documents/Collaborations/DBEN/cru_jra_1901-2020/allyears/forcing_sel_sites_temp.RData")
temp
temp <- temp %>% rownames_to_column(var="date") %>% 
  mutate(date=gsub('X','',date),date=gsub('\\.','-',date),date=as.Date(date),
         doy = lubridate::yday(date),month=lubridate::month(date),year=lubridate::year(date))
temp_FIN <- temp %>% select(date,doy,year, FI) %>% rename(temp=FI) %>% mutate(temp=temp-273.15)
temp_BIA <- temp %>% select(date,doy,year, BIA) %>% rename(temp=BIA) %>% mutate(temp=temp-273.15)
temp_BCI <- temp %>% select(date,doy,year, BCI) %>% rename(temp=BCI) %>% mutate(temp=temp-273.15)

# Total surface downwelling radiation (W/m2) ####
load("~/Documents/Collaborations/DBEN/cru_jra_1901-2020/allyears/forcing_sel_sites_tswrf.RData")
tswrf 
tswrf <- tswrf %>% rownames_to_column(var="date") %>% 
  mutate(date=gsub('X','',date),date=gsub('\\.','-',date),date=as.Date(date),
         doy = lubridate::yday(date),month=lubridate::month(date),year=lubridate::year(date)) %>% filter(year>1900)
ppfd_FIN <- tswrf %>% select(date,doy,year, FI) %>% rename(ppfd=FI) 
ppfd_BIA <- tswrf %>% select(date,doy,year, BIA) %>% rename(ppfd=BIA)
ppfd_BCI <- tswrf %>% select(date,doy,year, BCI) %>% rename(ppfd=BCI) 

# Wind speed (m/s) ####
load("~/Documents/Collaborations/DBEN/cru_jra_1901-2020/allyears/forcing_sel_sites_wind.RData")
wind
wind <- wind %>% rownames_to_column(var="date") %>% 
  mutate(date=gsub('X','',date),date=gsub('\\.','-',date),date=as.Date(date),
         doy = lubridate::yday(date),month=lubridate::month(date),year=lubridate::year(date))
wind_FIN <- wind %>% select(date,doy,year, FI) %>% rename(wind=FI) 
wind_BIA <- wind %>% select(date,doy,year, BIA) %>% rename(wind=BIA) 
wind_BCI <- wind %>% select(date,doy,year, BCI) %>% rename(wind=BCI)

# CO2 ####
co2 <- read.csv("~/Documents/Collaborations/DBEN/cru_jra_1901-2020/co2.csv")
co2 <- co2 %>% rename(co2=CO2)

# PAR (umol/m2/s) - Calculated from radiation after converting units (1W/m2 = 4.6 umol/m2/s) ####
par_FIN <- ppfd_FIN %>% rename(par=ppfd) %>% mutate(par=par*4.6)
par_BIA <- ppfd_BIA %>% rename(par=ppfd) %>% mutate(par=par*4.6)
par_BCI <- ppfd_BCI %>% rename(par=ppfd) %>% mutate(par=par*4.6)

# Atmospheric pressure - Use ingestr ####
patm_FIN <- calc_patm(elv = 143, patm0 = 101325)
patm_BIA <- calc_patm(elv = 165 , patm0 = 101325)
patm_BCI <- calc_patm(elv = 120, patm0 = 101325)

# P-model drivers ####
# Needed to simulate soil temperature and soil water content 
p_model_drivers
#date
#temp (C)
#prec (mm)
#vpd (Pa)
#ppfd (mol m-2 d-1)
#patm (Pa)
#ccov_int
#ccov
#snow
#rain = prec
#fapar
#co2
#doy
#tmin
#tmax

# Download variables using ingestr ####
sites <- tibble(sitename=c("FIN","BIA","BCI"),lon=c(23.25,23.75,-79.75),
                lat=c(62.25,52.75,9.25), elv=c(143,165,120), year_start=1991,
                year_end=2020,classid="EBF",c4=F,whc=240,koeppen_code="Csa",
                igbp_land_use="Mixed Forests",plant_functional_type="Evergreen Needleleaf Trees")

## FIN ####

### sitename ####
sitename <- "FIN"

### forcing ####
p_model_drivers$forcing

df_cru_FIN <- ingestr::ingest_bysite(
  sitename  = "FIN",
  source    = "cru",
  getvars   = c("temp","tmax", "tmin", "ccov","vpd"),
  dir       = "~/data/cru/ts_4.06/",
  timescale = "d",
  year_start = 1901,
  year_end  = 2020,
  lon       = sites[1,2][[1]],
  lat       = sites[1,3][[1]],
  verbose   = FALSE
)
df_cru_FIN
ccov_FIN <- as.data.frame(df_cru_FIN) %>% select(date, ccov,ccov_int)
vpd_FIN <- as.data.frame(df_cru_FIN) %>% select(date, vpd)

settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/modis_subsets/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  n_focal           = 0,
  network           = NA
)
df_modis_fpar <- ingestr::ingest_bysite(
  sitename  = "FIN",
  source    = "modis",
  year_start= 1991,
  year_end  = 2020,
  lon       = sites[1,2][[1]],
  lat       = sites[1,3][[1]],  
  settings  = settings_modis,
  verbose   = FALSE
)
df_modis_fpar_FIN <- df_modis_fpar
fapar_FIN <- as.data.frame(df_modis_fpar_FIN) %>% select(date, fapar)

forcing_FIN <- temp_FIN %>% left_join(prec_FIN) %>% left_join(vpd_FIN) %>% left_join(par_FIN) %>% 
  rename(ppfd=par) %>% mutate(ppfd=ppfd*1e-6*86400) %>% 
  mutate(patm=patm_FIN) %>% left_join(ccov_FIN) %>% mutate(snow=0.0) %>% mutate(rain=prec) %>%
  left_join(fapar_FIN) %>% left_join(co2) %>% mutate(tmin=temp,tmax=temp) %>% filter(year>=1991) %>%
  as_tibble() %>% 
  # Need to check there are no NAs in vpd, ppfd and ccov:
  mutate(ppfd=ifelse(is.na(ppfd),mean(ppfd,na.rm=T),ppfd)) %>%
  mutate(vpd=ifelse(is.na(vpd),mean(vpd,na.rm=T),vpd)) %>%
  mutate(ccov=ifelse(is.na(ccov),mean(ccov,na.rm=T),ccov)) %>%
  mutate(fapar=ifelse(is.na(fapar),1,fapar))
max(forcing_FIN$date)

###  params_siml ####
p_model_drivers$params_siml[[1]]
colnames(p_model_drivers$params_siml[[1]])
params_siml <- tibble(
  spinup             = TRUE,
  spinupyears        = 10,
  recycle            = 1,
  soilmstress        = TRUE,
  tempstress         = TRUE,
  calc_aet_fapar_vpd = FALSE,
  in_ppfd            = TRUE,
  in_netrad          = FALSE,
  outdt              = 1,
  ltre               = FALSE,
  ltne               = FALSE,
  ltrd               = FALSE,
  ltnd               = FALSE,
  lgr3               = TRUE,
  lgn3               = FALSE,
  lgr4               = FALSE,
  firstyeartrend     = 1991,
  nyeartrend         = 30
)
###  site_info ####
p_model_drivers$site_info
site_info <- tibble(
  lon              = sites[1,2][[1]],
  lat              = sites[1,3][[1]], 
  elv              = sites[1,4][[1]],  
  year_end         = 2020,
  classid          = "EBF",
  c4               = F,
  whc              = 240,
  koeppen_code     = "Csa",
  igbp_land_use    = "Mixed Forests",
  plant_functional_type="Evergreen Needleleaf Trees",
  date_start       = "1991-01-01",
  date_end         = "2020-12-31")

###  params_soil ####
p_model_drivers$params_soil
p_model_drivers_FIN <- tibble(sitename = sitename,
                     forcing = list(tibble(forcing_FIN)),
                     params_siml = list(tibble(params_siml)),
                     site_info = list(tibble(site_info)),
                     params_soil=p_model_drivers$params_soil,
                     .name_repair = "unique")

## FIN run model ####
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286,
  tau_acclim_tempstress = 10,
  par_shape_tempstress  = 0.0
)

p_model_drivers$forcing
p_model_drivers_FIN$forcing
#p_model_drivers_FIN$forcing[[1]]$vpd <- 375
#p_model_drivers_FIN$forcing[[1]]$ppfd <- 13.3
#p_model_drivers_FIN$forcing[[1]]$ccov <- 77

# run the model for these parameters
output_FIN <- rsofun::runread_pmodel_f(
  p_model_drivers_FIN,
  par = params_modl
)
output_FIN$data
output_FIN$data[[1]] %>% 
  ggplot(aes(date, gpp)) +
  geom_line()

tsoil_FIN <- as.data.frame(output_FIN$data[[1]]) %>% select(date, tsoil)
wscal_FIN <- as.data.frame(output_FIN$data[[1]]) %>% select(date, wscal)

## FIN BiomeE forcing ####
names(biomee_p_model_drivers$forcing[[1]])
names(biomee_forcing_FIN)

biomee_forcing_FIN <- temp_FIN %>% left_join(tsoil_FIN) %>% left_join(prec_FIN) %>% 
  mutate(snow=0.0) %>% left_join(vpd_FIN) %>% left_join(rh_FIN) %>% left_join(ppfd_FIN) %>%
  left_join(par_FIN) %>% mutate(patm=patm_FIN) %>% left_join(wind_FIN) %>% left_join(ccov_FIN) %>% 
  left_join(co2) %>% left_join(wscal_FIN) %>% filter(year>=1991) %>%
  as_tibble() %>% relocate(doy, .after=year) %>% relocate(ccov, .after=ccov_int) %>% 
  # Need to check there are no NAs in vpd, ppfd and ccov:
  mutate(ppfd=ifelse(is.na(ppfd),mean(ppfd,na.rm=T),ppfd)) %>%
  mutate(vpd=ifelse(is.na(vpd),mean(vpd,na.rm=T),vpd)) %>%
  mutate(ccov=ifelse(is.na(ccov),mean(ccov,na.rm=T),ccov)) %>%
  mutate(par=ifelse(is.na(par),mean(par,na.rm=T),par)) %>%
  mutate(tsoil=ifelse(is.na(tsoil),mean(tsoil,na.rm=T),tsoil)) %>%
  mutate(wscal=ifelse(is.na(wscal),mean(wscal,na.rm=T),wscal)) %>%
  rename(temp_soil=tsoil,swc=wscal)

### Order of years ####
order_years <- as.factor(c(2014 ,1996, 2010, 1998, 1991, 2002, 2012, 2016, 2017, 1999, 2008, 2001, 2015, 2019, 
                           2006, 1993, 1994, 2020, 2000, 2004, 1992, 1995, 2018, 2011, 2005, 2007, 2013, 2009, 
                           1997, 2003))
df_order_years <- as.data.frame(order_years) %>% rename(year=order_years) %>% 
  mutate(order=seq(1:30),year=as.character(year),year=as.double(year)) %>% as.tibble()
biomee_forcing_FIN <- biomee_forcing_FIN %>% arrange(factor(year, levels = order_years)) %>%
  left_join(df_order_years) %>% relocate(order, .after = year) %>% rename(yearID=year, year=order)
unique(biomee_forcing_FIN$year)

write.csv(biomee_forcing_FIN,"~/Documents/Collaborations/DBEN/cru_jra_1901-2020/biomee_forcing_FIN.csv")

## BIA forcing ####

### sitename ####
sitename <- "BIA"

### forcing ####
p_model_drivers$forcing

df_cru_BIA <- ingestr::ingest_bysite(
  sitename  = "BIA",
  source    = "cru",
  getvars   = c("temp","tmax", "tmin", "ccov","vpd"),
  dir       = "~/data/cru/ts_4.06/",
  timescale = "d",
  year_start = 1901,
  year_end  = 2020,
  lon       = sites[2,2][[1]],
  lat       = sites[2,3][[1]],
  verbose   = FALSE
)
df_cru_BIA
ccov_BIA <- as.data.frame(df_cru_BIA) %>% select(date, ccov,ccov_int)
vpd_BIA <- as.data.frame(df_cru_BIA) %>% select(date, vpd)

settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/modis_subsets/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  n_focal           = 0,
  network           = NA
)
df_modis_fpar_BIA <- ingestr::ingest_bysite(
  sitename  = "BIA",
  source    = "modis",
  year_start= 1991,
  year_end  = 2020,
  lon       = sites[2,2][[1]],
  lat       = sites[2,3][[1]],  
  settings  = settings_modis,
  verbose   = FALSE
)
fapar_BIA <- as.data.frame(df_modis_fpar_BIA) %>% select(date, fapar)

forcing_BIA <- temp_BIA %>% left_join(prec_BIA) %>% left_join(vpd_BIA) %>% left_join(par_BIA) %>% 
  rename(ppfd=par) %>% mutate(ppfd=ppfd*1e-6*86400) %>% 
  mutate(patm=patm_BIA) %>% left_join(ccov_BIA) %>% mutate(snow=0.0) %>% mutate(rain=prec) %>%
  left_join(fapar_BIA) %>% left_join(co2) %>% mutate(tmin=temp,tmax=temp) %>% filter(year>=1991) %>%
  as_tibble() %>% 
  # Need to check there are no NAs in vpd, ppfd and ccov:
  mutate(ppfd=ifelse(is.na(ppfd),mean(ppfd,na.rm=T),ppfd)) %>%
  mutate(vpd=ifelse(is.na(vpd),mean(vpd,na.rm=T),vpd)) %>%
  mutate(ccov=ifelse(is.na(ccov),mean(ccov,na.rm=T),ccov)) %>%
  mutate(fapar=ifelse(is.na(fapar),1,fapar))
max(forcing_BIA$date)

###  params_siml ####
p_model_drivers$params_siml[[1]]
colnames(p_model_drivers$params_siml[[1]])
params_siml <- tibble(
  spinup             = TRUE,
  spinupyears        = 10,
  recycle            = 1,
  soilmstress        = TRUE,
  tempstress         = TRUE,
  calc_aet_fapar_vpd = FALSE,
  in_ppfd            = TRUE,
  in_netrad          = FALSE,
  outdt              = 1,
  ltre               = FALSE,
  ltne               = FALSE,
  ltrd               = FALSE,
  ltnd               = FALSE,
  lgr3               = TRUE,
  lgn3               = FALSE,
  lgr4               = FALSE,
  firstyeartrend     = 1991,
  nyeartrend         = 30
)
###  site_info ####
p_model_drivers$site_info
site_info <- tibble(
  lon              = sites[2,2][[1]],
  lat              = sites[2,3][[1]], 
  elv              = sites[2,4][[1]], 
  year_end         = 2020,
  classid          = "EBF",
  c4               = F,
  whc              = 240,
  koeppen_code     = "Csa",
  igbp_land_use    = "Mixed Forests",
  plant_functional_type="Evergreen Needleleaf Trees",
  date_start       = "1991-01-01",
  date_end         = "2020-12-31")

###  params_soil ####
p_model_drivers$params_soil
p_model_drivers_BIA <- tibble(sitename = sitename,
                              forcing = list(tibble(forcing_BIA)),
                              params_siml = list(tibble(params_siml)),
                              site_info = list(tibble(site_info)),
                              params_soil=p_model_drivers$params_soil,
                              .name_repair = "unique")

## BIA run model ####
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286,
  tau_acclim_tempstress = 10,
  par_shape_tempstress  = 0.0
)

p_model_drivers$forcing
p_model_drivers_BIA$forcing

# run the model for these parameters
output_BIA <- rsofun::runread_pmodel_f(
  p_model_drivers_BIA,
  par = params_modl
)
output_BIA$data
output_BIA$data[[1]] %>% 
  ggplot(aes(date, gpp)) +
  geom_line()

tsoil_BIA <- as.data.frame(output_BIA$data[[1]]) %>% select(date, tsoil)
wscal_BIA <- as.data.frame(output_BIA$data[[1]]) %>% select(date, wscal)

## BIA BiomeE forcing ####
names(biomee_p_model_drivers$forcing[[1]])
names(biomee_forcing_BIA)

biomee_forcing_BIA <- temp_BIA %>% left_join(tsoil_BIA) %>% left_join(prec_BIA) %>% 
  mutate(snow=0.0) %>% left_join(vpd_BIA) %>% left_join(rh_BIA) %>% left_join(ppfd_BIA) %>%
  left_join(par_BIA) %>% mutate(patm=patm_BIA) %>% left_join(wind_BIA) %>% left_join(ccov_BIA) %>% 
  left_join(co2) %>% left_join(wscal_BIA) %>% filter(year>=1991) %>%
  as_tibble() %>% relocate(doy, .after=year) %>% relocate(ccov, .after=ccov_int) %>% 
  # Need to check there are no NAs in vpd, ppfd and ccov:
  mutate(ppfd=ifelse(is.na(ppfd),mean(ppfd,na.rm=T),ppfd)) %>%
  mutate(vpd=ifelse(is.na(vpd),mean(vpd,na.rm=T),vpd)) %>%
  mutate(ccov=ifelse(is.na(ccov),mean(ccov,na.rm=T),ccov)) %>%
  mutate(par=ifelse(is.na(par),mean(par,na.rm=T),par)) %>%
  mutate(tsoil=ifelse(is.na(tsoil),mean(tsoil,na.rm=T),tsoil)) %>%
  mutate(wscal=ifelse(is.na(wscal),mean(wscal,na.rm=T),wscal))  %>%
  rename(temp_soil=tsoil,swc=wscal)

### Order of years ####
order_years <- as.factor(c(2014 ,1996, 2010, 1998, 1991, 2002, 2012, 2016, 2017, 1999, 2008, 2001, 2015, 2019, 
                           2006, 1993, 1994, 2020, 2000, 2004, 1992, 1995, 2018, 2011, 2005, 2007, 2013, 2009, 
                           1997, 2003))
df_order_years <- as.data.frame(order_years) %>% rename(year=order_years) %>% 
  mutate(order=seq(1:30),year=as.character(year),year=as.double(year)) %>% as.tibble()
biomee_forcing_BIA <- biomee_forcing_BIA %>% arrange(factor(year, levels = order_years)) %>%
  left_join(df_order_years) %>% relocate(order, .after = year) %>% rename(yearID=year, year=order)
unique(biomee_forcing_BIA$year)

write.csv(biomee_forcing_BIA,"~/Documents/Collaborations/DBEN/cru_jra_1901-2020/biomee_forcing_BIA.csv")

## BCI forcing ####

### sitename ####
sitename <- "BCI"

### forcing ####
p_model_drivers$forcing

df_cru_BCI <- ingestr::ingest_bysite(
  sitename  = "BCI",
  source    = "cru",
  getvars   = c("temp","tmax", "tmin", "ccov","vpd"),
  dir       = "~/data/cru/ts_4.06/",
  timescale = "d",
  year_start = 1901,
  year_end  = 2020,
  lon       = sites[3,2][[1]],
  lat       = sites[3,3][[1]],
  verbose   = FALSE
)
df_cru_BCI
ccov_BCI <- as.data.frame(df_cru_BCI) %>% select(date, ccov,ccov_int)
vpd_BCI <- as.data.frame(df_cru_BCI) %>% select(date, vpd)

settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/modis_subsets/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  n_focal           = 0,
  network           = NA
)
df_modis_fpar_BCI <- ingestr::ingest_bysite(
  sitename  = "BCI",
  source    = "modis",
  year_start= 1991,
  year_end  = 2020,
  lon       = sites[3,2][[1]],
  lat       = sites[3,3][[1]],  
  settings  = settings_modis,
  verbose   = FALSE
)
fapar_BCI <- as.data.frame(df_modis_fpar_BCI) %>% select(date, fapar)

forcing_BCI <- temp_BCI %>% left_join(prec_BCI) %>% left_join(vpd_BCI) %>% left_join(par_BCI) %>% 
  rename(ppfd=par) %>% mutate(ppfd=ppfd*1e-6*86400) %>% 
  mutate(patm=patm_BCI) %>% left_join(ccov_BCI) %>% mutate(snow=0.0) %>% mutate(rain=prec) %>%
  left_join(fapar_BCI) %>% left_join(co2) %>% mutate(tmin=temp,tmax=temp) %>% filter(year>=1991) %>%
  as_tibble() %>% 
  # Need to check there are no NAs in vpd, ppfd and ccov:
  mutate(ppfd=ifelse(is.na(ppfd),mean(ppfd,na.rm=T),ppfd)) %>%
  mutate(vpd=ifelse(is.na(vpd),mean(vpd,na.rm=T),vpd)) %>%
  mutate(ccov=ifelse(is.na(ccov),mean(ccov,na.rm=T),ccov)) %>%
  mutate(fapar=ifelse(is.na(fapar),1,fapar))
max(forcing_BCI$date)

###  params_siml ####
p_model_drivers$params_siml[[1]]
colnames(p_model_drivers$params_siml[[1]])
params_siml <- tibble(
  spinup             = TRUE,
  spinupyears        = 10,
  recycle            = 1,
  soilmstress        = TRUE,
  tempstress         = TRUE,
  calc_aet_fapar_vpd = FALSE,
  in_ppfd            = TRUE,
  in_netrad          = FALSE,
  outdt              = 1,
  ltre               = FALSE,
  ltne               = FALSE,
  ltrd               = FALSE,
  ltnd               = FALSE,
  lgr3               = TRUE,
  lgn3               = FALSE,
  lgr4               = FALSE,
  firstyeartrend     = 1991,
  nyeartrend         = 30
)
###  site_info ####
p_model_drivers$site_info
site_info <- tibble(
  lon              = sites[3,2][[1]],
  lat              = sites[3,3][[1]], 
  elv              = sites[3,4][[1]], 
  year_end         = 2020,
  classid          = "EBF",
  c4               = F,
  whc              = 240,
  koeppen_code     = "Csa",
  igbp_land_use    = "Mixed Forests",
  plant_functional_type="Evergreen Needleleaf Trees",
  date_start       = "1991-01-01",
  date_end         = "2020-12-31")

###  params_soil ####
p_model_drivers$params_soil
p_model_drivers_BCI <- tibble(sitename = sitename,
                              forcing = list(tibble(forcing_BCI)),
                              params_siml = list(tibble(params_siml)),
                              site_info = list(tibble(site_info)),
                              params_soil=p_model_drivers$params_soil,
                              .name_repair = "unique")

## BCI run model ####
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286,
  tau_acclim_tempstress = 10,
  par_shape_tempstress  = 0.0
)

p_model_drivers$forcing
p_model_drivers_BCI$forcing

# run the model for these parameters
output_BCI <- rsofun::runread_pmodel_f(
  p_model_drivers_BCI,
  par = params_modl
)
output_BCI$data
output_BCI$data[[1]] %>% 
  ggplot(aes(date, gpp)) +
  geom_line()

tsoil_BCI <- as.data.frame(output_BCI$data[[1]]) %>% select(date, tsoil)
wscal_BCI <- as.data.frame(output_BCI$data[[1]]) %>% select(date, wscal)

## BCI BiomeE forcing ####
names(biomee_p_model_drivers$forcing[[1]])
names(biomee_forcing_BCI)

biomee_forcing_BCI <- temp_BCI %>% left_join(tsoil_BCI) %>% left_join(prec_BCI) %>% 
  mutate(snow=0.0) %>% left_join(vpd_BCI) %>% left_join(rh_BCI) %>% left_join(ppfd_BCI) %>%
  left_join(par_BCI) %>% mutate(patm=patm_BCI) %>% left_join(wind_BCI) %>% left_join(ccov_BCI) %>% 
  left_join(co2) %>% left_join(wscal_BCI) %>% filter(year>=1991) %>%
  as_tibble() %>% relocate(doy, .after=year) %>% relocate(ccov, .after=ccov_int) %>% 
  # Need to check there are no NAs in vpd, ppfd and ccov:
  mutate(ppfd=ifelse(is.na(ppfd),mean(ppfd,na.rm=T),ppfd)) %>%
  mutate(vpd=ifelse(is.na(vpd),mean(vpd,na.rm=T),vpd)) %>%
  mutate(ccov=ifelse(is.na(ccov),mean(ccov,na.rm=T),ccov)) %>%
  mutate(par=ifelse(is.na(par),mean(par,na.rm=T),par)) %>%
  mutate(tsoil=ifelse(is.na(tsoil),mean(tsoil,na.rm=T),tsoil)) %>%
  mutate(wscal=ifelse(is.na(wscal),mean(wscal,na.rm=T),wscal)) %>%
  rename(temp_soil=tsoil,swc=wscal) 

### Order of years ####
order_years <- as.factor(c(2014 ,1996, 2010, 1998, 1991, 2002, 2012, 2016, 2017, 1999, 2008, 2001, 2015, 2019, 
                           2006, 1993, 1994, 2020, 2000, 2004, 1992, 1995, 2018, 2011, 2005, 2007, 2013, 2009, 
                           1997, 2003))
df_order_years <- as.data.frame(order_years) %>% rename(year=order_years) %>% 
  mutate(order=seq(1:30),year=as.character(year),year=as.double(year)) %>% as.tibble()
biomee_forcing_BCI <- biomee_forcing_BCI %>% arrange(factor(year, levels = order_years)) %>%
  left_join(df_order_years) %>% relocate(order, .after = year) %>% rename(yearID=year, year=order)
unique(biomee_forcing_BCI$year)

write.csv(biomee_forcing_BCI,"~/Documents/Collaborations/DBEN/cru_jra_1901-2020/biomee_forcing_BCI.csv")

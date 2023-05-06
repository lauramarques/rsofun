
# load packages
library(rsofun)
library(dplyr)
library(ggplot2)
library(patchwork)
library(multidplyr)

# CO2 412 ppm ####

## Define drivers #### 

sitename <- "BIA"

site_info <- tibble(
  sitename="BIA",
  lon = 23.75,
  lat = 52.75,
  elv = NA,
  year_start = 1991,
  year_end = 2020,
  classid = NA,
  c4 = FALSE,
  whc = NA,
  koeppen_code = NA,
  igbp_land_use = "Mixed Forests",
  plant_functional_type = "Broadleaf trees"
)

site_info <- site_info %>% 
  dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

params_siml <- tibble(
  spinup                = TRUE,
  spinupyears           = 510, 
  recycle               = 30,  
  firstyeartrend        = 0, 
  nyeartrend            = 450,
  outputhourly          = TRUE,
  outputdaily           = TRUE,
  do_U_shaped_mortality = TRUE,
  update_annualLAImax   = TRUE,
  do_closedN_run        = TRUE,
  method_photosynth     = "pmodel",
  method_mortality      = "dbh"
)

params_tile <- tibble(
  soiltype     = 3,     
  FLDCAP       = 0.4,   
  WILTPT       = 0.05,  
  K1           = 2.0,   
  K2           = 0.05,  
  K_nitrogen   = 2.4, 
  MLmixRatio   = 0.8, 
  etaN         = 0.0, 
  LMAmin       = 0.02,
  fsc_fine     = 1.0,   
  fsc_wood     = 0.0,   
  GR_factor    = 0.33,  
  l_fract      = 0.0,   
  retransN     = 0.0,   
  f_initialBSW = 0.2,
  f_N_add      = 0.02,  
  # add calibratable params
  tf_base        = 1,
  par_mort       = 0.15,
  par_mort_under = 1
)

# Run site simulations
# Lon 23.75°, Lat 52.75° Temperate: Bialowieza, Poland (BIA): 
# Picea abies - shade tolerant needleleaf (PFT2)
# Betula pendula - shade intolerant broadleaf deciduous (PFT3)
# Carpinus betulus/Tilia cordata - shade tolerant broadleaf deciduous (PFT4)
# Grasses combined (PFT8) C3

# Shade tolerant-low Vcmax, low resp. rate and low mortality rate
# Shade intolerant-high Vcmax, high resp. rate and high mortality rate

params_species <- tibble(
  # species         0 1 2 3 4    ...
  lifeform      = c(9999,0,1,1,1,rep(1,11)),        
  phenotype     = c(9999,0,0,0,1,rep(1,11)),        
  pt            = rep(0,16),                      
  # Root parameters
  alpha_FR      = rep(1.2,16),                    
  rho_FR        = rep(200,16),                    
  root_r        = rep(2.9E-4,16), 
  root_zeta     = rep(0.29,16), 
  Kw_root       = rep(3.5e-09,16),               
  leaf_size     = rep(0.04,16), 
  # Photosynthesis parameters
  Vmax          = rep(35.0E-6,16),               
  Vannual       = rep(1.2,16),                   
  wet_leaf_dreg = rep(0.3,16),                   
  m_cond        = c(9999,7.0,9.0,9.0,9.0,rep(9.0,11)),                 
  alpha_phot    = rep(0.06,16), 
  gamma_L       = rep(0.02,16), 
  gamma_LN      = rep(70.5 ,16),                
  gamma_SW      = rep(0.08,16),                 
  gamma_FR      = rep(12.0,16),                 
  tc_crit       = c(9999,12,15,15,0,rep(0,11)), 
  tc_crit_on    = c(9999,10,12,12,0,rep(15,11)),
  gdd_crit      = c(9999,80,120,120,0,rep(0,11)),
  betaON        = c(9999,0.2,0.2,0.2,0.2,rep(0.2,11)),     
  betaOFF       = c(9999,0.1,0.1,0.1,0.1,rep(0.1,11)),
  seedlingsize  = c(9999,0.01,0.05,0.05,0.05,rep(0.05,11)),  # c(9999,0.005,0.02,0.02,0.06,rep(0.05,11)), 
  #seedlingsize  = c(9999,0.005,0.02,0.02,0.06,rep(0.05,11)),  
  LNbase        = c(9999,1.0E-3,0.8E-3,0.7E-3,0.4E-3,rep(0.5E-3,11)),  
  lAImax        = c(9999,2.0,3.0,3.0,3.5,rep(3.5,11)), # c(9999,2.0,3.2,3.3,3.5,rep(3.5,11)), 
  #lAImax        = c(9999,2.0,3.2,3.3,3.5,rep(3.5,11)), 
  Nfixrate0     = rep(0,16),                      
  NfixCost0     = rep(0,16),                     
  phiCSA        = rep(0.25E-4,16),               
  mortrate_d_c  = c(9999,0.05,0.03,0.02,0.01,rep(0.02,11)),  
  mortrate_d_u  = rep(0.075,16),                  
  maturalage    = c(9999,0,5,5,5,rep(5,11)),   
  v_seed        = c(9999,0.1,0.1,0.1,0.1,rep(0.1,11)),  # c(9999,0.4,0.1,0.1,0.1,rep(0.1,11)),   
  #v_seed        = c(9999,0.4,0.1,0.1,0.1,rep(0.1,11)),   
  fNSNmax       = rep(5,16),                      
  LMA           = c(9999,0.025,0.025,0.025,0.14,rep(0.14,11)), 
  rho_wood      = c(9999,120,350,350,300,rep(300,11)), # c(9999,80,320,350,300,rep(300,11)),  
  #rho_wood      = c(9999,120,320,350,300,rep(300,11)),         
  alphaBM       = rep(5200,16),                   
  thetaBM       = rep(2.5,16), 
  # add calibratable params
  kphio         = rep(0.05,16),
  phiRL         = rep(3.5,16),  # c(9999,1.0,1.0,1.1,1.5,rep(1.2,11)), 
  LAI_light     = rep(3.5,16)   
) 

params_soil <- tibble(
  type              = c("Coarse","Medium","Fine","CM","CF","MF","CMF","Peat","MCM"),
  GMD               = c(0.7, 0.4, 0.3, 0.1, 0.1, 0.07, 0.007, 0.3, 0.3),
  GSD               = c(5.0, 5.3, 7.4, 6.1, 6.1, 14.0, 15.0, 7.4, 7.4),
  vwc_sat           = c(0.380, 0.445, 0.448, 0.412, 0.414, 0.446, 0.424, 0.445, 0.445),
  chb               = c(3.5,6.4,11.0,4.8,6.3,8.4,6.3,6.4,6.4),
  psi_sat_ref       = c(-600, -790, -910, -1580, -1680, -1880, -5980, -790, -790),
  k_sat_ref         = c(130.8, 75.1, 53.2, 12.1, 11.1, 12.7, 1.69, 53.2, 53.2), 
  alphaSoil         = rep(1, 9),
  heat_capacity_dry = c(1.2e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.4e6, 1.0)
)

init_cohort <- tibble(
  init_cohort_species = seq(1,10,1),   
  init_cohort_nindivs = rep(0.008,10),  # c(2.0,.05,.05,.05,rep(.05,6)),  
  init_cohort_bsw     = rep(0.2,10),    # c(0.01,0.2,0.2,0.2,rep(0.2,6)),
  init_cohort_bHW     = rep(0.0, 10), 
  init_cohort_nsc     = rep(0.5,10)     # c(0.04,0.5,0.5,0.5,rep(0.5,6)),
)

init_soil <- tibble( #list
  init_fast_soil_C    = 0.0,    # 0.5, 
  init_slow_soil_C    = 0.0,    # 40,   
  init_Nmineral       = 0.015,  # 50.0E-3, 
  N_input             = 0.0008  # 11.E-3
)

df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1)
)

# Disturbance regime 

#This contains the forcing time series data frame where the disturbance is to be defined as the fraction 
#of aboveground biomass harvested (`harv`). Additional specifications of the disturbance forcing, 
#which are more specific to the simulations done here, are hard-coded (see below). 

#Model is first run to steady state. Then for another 100 years undisturbed (continued steady-state) 
#until first disturbance. After first disturbance left undisturbed for 900 years to allow for 
#recovery (in some simulations recovery may take long). Then a regime of repeated disturbance after 
#simulation year 1000 to investigate disturbance-recovery (non-steady state) dynamics, 
#first at low frequency for 1000 years (disturbance every 250 years), then at high frequency for 1000 
#years (disturbance every 25 years).

# Disturbance is implemented by:
# - year 100 first disturbance applied, then recovery until year 1000
# - year 1000 second disturbance, then every 250 years disturbed for 1000 years
# - ... then every 25 years disturbed for another 1000 years

#To be handled by model by (new) forcing time series as input (`harv`)
fharv <- 0.9
harv_vec <- rep(0, 999)
harv_vec[100] <- fharv
harv_vec <- c(harv_vec, rep(c(fharv, rep(0, 249)), 4), rep(c(fharv, rep(0, 24)), 40), 0)
df_harv <- tibble(year = seq(length(harv_vec)), harv = harv_vec)

df_harv <- tibble(year = seq(1:450), harv = c(rep(0,200),0,rep(0,249)))
#df_harv <- tibble(year = seq(1:450), harv = c(rep(0,100),rep(c(fharv, rep(0, 69)), 5)))

#df_harv %>%  ggplot(aes(year, harv)) + geom_line() + ylim(0, 1)

## Define forcing data ####
biomee_forcing_BIA <- read.csv("~/Documents/Collaborations/DBEN/cru_jra_1901-2020/biomee_forcing_BIA.csv")
biomee_forcing_BIA
df_forcing <- biomee_forcing_BIA

## Define CO2 ####
df_forcing$co2 <- 412 # 562

# Repeat mean seasonal cycle `nyears` times # Add harvest forcing to drivers. 
nyears <- nrow(df_harv)/length(unique(biomee_forcing_BIA$year))
df_forcing <- df_forcing %>% 
  slice(rep(1:n(), nyears)) %>% rename(yearID=year) %>%
  mutate(year = rep(1:450, each = 365)) %>% relocate(year, .after=yearID) %>%
  mutate(hour=11.5)

# Add harvest to forcing, assuming harvest on Jan 1st.
df_forcing_disturb <- df_forcing %>% 
  left_join(
    df_harv %>% 
      mutate(doy = 1),
    by = c("doy", "year")
  ) %>% 
  mutate(harv = ifelse(is.na(harv), 0, harv))

## for control simulation
df_forcing <- df_forcing %>%
  mutate(harv = 0)

# Add N deposition as NOx and NHy.
df_forcing <- df_forcing %>% 
  mutate(nox = 0, nhy = 0)

df_forcing_disturb <- df_forcing_disturb %>% 
  mutate(nox = 0, nhy = 0)

## for versions above 4.0
df_drivers_disturb <-tibble(sitename = site_info$sitename,
                            site_info = list(tibble(site_info)),
                            params_siml = list(tibble(params_siml)),
                            params_tile = list(tibble(params_tile)),
                            params_species=list(tibble(params_species)),
                            params_soil=list(tibble(params_soil)),
                            init_cohort=list(tibble(init_cohort)),
                            init_soil=list(tibble(init_soil)),
                            forcing=list(tibble(df_forcing_disturb)),
                            .name_repair = "unique")

### Run the model
out_sc1 <- runread_biomee_f(
  df_drivers_disturb,
  makecheck = TRUE,
  parallel = FALSE
)

g1 <- out_sc1$data[[1]]$output_annual_tile %>%
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  theme_classic()+labs(x = "Year", y = "plantC") 

g2 <- out_sc1$data[[1]]$output_annual_cohorts %>% group_by(PFT,year) %>%
  summarise(sumBA=sum(DBH*DBH*pi/4*density/10000)) %>% mutate(PFT=as.factor(PFT)) %>%
  ggplot() +
  geom_line(aes(x = year, y = sumBA,col=PFT)) +
  theme_classic()+labs(x = "Year", y = "BA") + 
  scale_colour_discrete(labels = c("Grass","Broadleaf1","Broadleaf2","Needleleaf"))

g3 <- out_sc1$data[[1]]$output_annual_cohorts %>% group_by(PFT,year) %>%
  summarise(CrownArea=sum(Acrown*density/10000)) %>% mutate(PFT=as.factor(PFT)) %>%
  ggplot() +
  geom_line(aes(x = year, y = CrownArea,col=PFT)) +
  theme_classic()+labs(x = "Year", y = "CrownArea") + 
  scale_colour_discrete(labels = c("Grass","Broadleaf1","Broadleaf2","Needleleaf"))

g4 <- out_sc1$data[[1]]$output_annual_cohorts %>% group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% mutate(PFT=as.factor(PFT)) %>%
  ggplot() +
  geom_line(aes(x = year, y = npp,col=PFT)) +
  theme_classic()+labs(x = "Year", y = "NPP") + 
  scale_colour_discrete(labels = c("Grass","Broadleaf1","Broadleaf2","Needleleaf"))

g5 <- out_sc1$data[[1]]$output_annual_cohorts %>% group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() +
  geom_line(aes(x = year, y = BAgrowth,col=PFT)) +
  theme_classic()+labs(x = "Year", y = "BAgrowth") + 
  scale_colour_discrete(labels = c("Grass","Broadleaf1","Broadleaf2","Needleleaf"))

print(g1/g2/g3/g4/g5)

out_sc1$data[[1]]$output_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x=year, y=BAgrowth,col=PFT)) +
  labs(x = "year", y = expression(paste("Basal area growth (", m^2, " ",ha^-1, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grasses","Tilia cordata","Betula pendula","Picea abies"))

g6 <- out_sc1$data[[1]]$output_annual_tile %>%
  ggplot() +
  geom_line(aes(x = year, y = (SapwoodC+WoodC)*0.75)) +
  theme_classic()+labs(x = "Year", y = "AGW") + geom_hline(yintercept=10, col="grey")
g6

## Outputs ####
### P0 ####
write.csv(out_sc1$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_annual_tile.csv")
write.csv(out_sc1$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_annual_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_daily_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_daily_tile.csv")
write.csv(out_sc1$data[[1]]$output_daily_cohorts,"~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_daily_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_hourly_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_hourly_tile.csv")

### PS-1 (0.01 or nfrequency=100) ####
write.csv(out_sc1$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS1_BIA_aCO2_annual_tile.csv")
write.csv(out_sc1$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs_mod/412ppm/BiomeE_PS1_BIA_aCO2_annual_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_daily_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS1_BIA_aCO2_daily_tile.csv")
write.csv(out_sc1$data[[1]]$output_daily_cohorts,"~/rsofun/data/outputs_mod/412ppm/BiomeE_PS1_BIA_aCO2_daily_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_hourly_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS1_BIA_aCO2_hourly_tile.csv")

### PS-2 (0.02 or nfrequency=75) ####
write.csv(out_sc1$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS2_BIA_aCO2_annual_tile.csv")
write.csv(out_sc1$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs_mod/412ppm/BiomeE_PS2_BIA_aCO2_annual_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_daily_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS2_BIA_aCO2_daily_tile.csv")
write.csv(out_sc1$data[[1]]$output_daily_cohorts,"~/rsofun/data/outputs_mod/412ppm/BiomeE_PS2_BIA_aCO2_daily_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_hourly_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS2_BIA_aCO2_hourly_tile.csv")

### PS-3 (0.04 or nfrequency=50) ####
write.csv(out_sc1$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS3_BIA_aCO2_annual_tile.csv")
write.csv(out_sc1$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs_mod/412ppm/BiomeE_PS3_BIA_aCO2_annual_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_daily_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS3_BIA_aCO2_daily_tile.csv")
write.csv(out_sc1$data[[1]]$output_daily_cohorts,"~/rsofun/data/outputs_mod/412ppm/BiomeE_PS3_BIA_aCO2_daily_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_hourly_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS3_BIA_aCO2_hourly_tile.csv")

### PS-4 (0.08 or nfrequency=25) ####
write.csv(out_sc1$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS4_BIA_aCO2_annual_tile.csv")
write.csv(out_sc1$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs_mod/412ppm/BiomeE_PS4_BIA_aCO2_annual_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_daily_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS4_BIA_aCO2_daily_tile.csv")
write.csv(out_sc1$data[[1]]$output_daily_cohorts,"~/rsofun/data/outputs_mod/412ppm/BiomeE_PS4_BIA_aCO2_daily_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_hourly_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS4_BIA_aCO2_hourly_tile.csv")

### PS-5 (0.20 or nfrequency=15) ####
write.csv(out_sc1$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS5_BIA_aCO2_annual_tile.csv")
write.csv(out_sc1$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs_mod/412ppm/BiomeE_PS5_BIA_aCO2_annual_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_daily_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS5_BIA_aCO2_daily_tile.csv")
write.csv(out_sc1$data[[1]]$output_daily_cohorts,"~/rsofun/data/outputs_mod/412ppm/BiomeE_PS5_BIA_aCO2_daily_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_hourly_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS5_BIA_aCO2_hourly_tile.csv")

### PS-6 (0.40 or nfrequency=10) ####
write.csv(out_sc1$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS6_BIA_aCO2_annual_tile.csv")
write.csv(out_sc1$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs_mod/412ppm/BiomeE_PS6_BIA_aCO2_annual_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_daily_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS6_BIA_aCO2_daily_tile.csv")
write.csv(out_sc1$data[[1]]$output_daily_cohorts,"~/rsofun/data/outputs_mod/412ppm/BiomeE_PS6_BIA_aCO2_daily_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_hourly_tile,   "~/rsofun/data/outputs_mod/412ppm/BiomeE_PS6_BIA_aCO2_hourly_tile.csv")

library(rsofun)
library(dplyr)
library(ggplot2)
library(patchwork)
library(multidplyr)

# CO2 562 ppm ####

## Define drivers #### 

sitename <- "BIA"

site_info <- tibble(
  sitename="BIA",
  lon = 23.75,
  lat = 52.75,
  elv = NA,
  year_start = 1991,
  year_end = 2020,
  classid = NA,
  c4 = FALSE,
  whc = NA,
  koeppen_code = NA,
  igbp_land_use = "Mixed Forests",
  plant_functional_type = "Broadleaf trees"
)

site_info <- site_info %>% 
  dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

params_siml <- tibble(
  spinup                = TRUE,
  spinupyears           = 510, 
  recycle               = 30,  
  firstyeartrend        = 0, 
  nyeartrend            = 450,
  outputhourly          = TRUE,
  outputdaily           = TRUE,
  do_U_shaped_mortality = TRUE,
  update_annualLAImax   = TRUE,
  do_closedN_run        = TRUE,
  method_photosynth     = "pmodel",
  method_mortality      = "dbh"
)

params_tile <- tibble(
  soiltype     = 3,     
  FLDCAP       = 0.4,   
  WILTPT       = 0.05,  
  K1           = 2.0,   
  K2           = 0.05,  
  K_nitrogen   = 2.4, 
  MLmixRatio   = 0.8, 
  etaN         = 0.0, 
  LMAmin       = 0.02,
  fsc_fine     = 1.0,   
  fsc_wood     = 0.0,   
  GR_factor    = 0.33,  
  l_fract      = 0.0,   
  retransN     = 0.0,   
  f_initialBSW = 0.2,
  f_N_add      = 0.02,  
  # add calibratable params
  tf_base        = 1,
  par_mort       = 0.15,
  par_mort_under = 1
)

# Run site simulations
# Lon 23.75°, Lat 52.75° Temperate: Bialowieza, Poland (BIA): 
# Picea abies - shade tolerant needleleaf (PFT2)
# Betula pendula - shade intolerant broadleaf deciduous (PFT3)
# Carpinus betulus/Tilia cordata - shade tolerant broadleaf deciduous (PFT4)
# Grasses combined (PFT8) C3

# Shade tolerant-low Vcmax, low resp. rate and low mortality rate
# Shade intolerant-high Vcmax, high resp. rate and high mortality rate

params_species <- tibble(
  # species         0 1 2 3 4    ...
  lifeform      = c(9999,0,1,1,1,rep(1,11)),        
  phenotype     = c(9999,0,0,0,1,rep(1,11)),        
  pt            = rep(0,16),                      
  # Root parameters
  alpha_FR      = rep(1.2,16),                    
  rho_FR        = rep(200,16),                    
  root_r        = rep(2.9E-4,16), 
  root_zeta     = rep(0.29,16), 
  Kw_root       = rep(3.5e-09,16),               
  leaf_size     = rep(0.04,16), 
  # Photosynthesis parameters
  Vmax          = rep(35.0E-6,16),               
  Vannual       = rep(1.2,16),                   
  wet_leaf_dreg = rep(0.3,16),                   
  m_cond        = c(9999,7.0,9.0,9.0,9.0,rep(9.0,11)),                 
  alpha_phot    = rep(0.06,16), 
  gamma_L       = rep(0.02,16), 
  gamma_LN      = rep(70.5 ,16),                
  gamma_SW      = rep(0.08,16),                 
  gamma_FR      = rep(12.0,16),                 
  tc_crit       = c(9999,12,15,15,0,rep(0,11)), 
  tc_crit_on    = c(9999,10,12,12,0,rep(15,11)),
  gdd_crit      = c(9999,80,120,120,0,rep(0,11)),
  betaON        = c(9999,0.2,0.2,0.2,0.2,rep(0.2,11)),     
  betaOFF       = c(9999,0.1,0.1,0.1,0.1,rep(0.1,11)),
  seedlingsize  = c(9999,0.01,0.05,0.05,0.05,rep(0.05,11)),  
  LNbase         = c(9999,1.0E-3,0.8E-3,0.7E-3,0.4E-3,rep(0.5E-3,11)),  
  lAImax         = c(9999,2.0,3.0,3.0,3.5,rep(3.5,11)), 
  Nfixrate0     = rep(0,16),                      
  NfixCost0     = rep(0,16),                     
  phiCSA        = rep(0.25E-4,16),               
  mortrate_d_c  = c(9999,0.05,0.03,0.02,0.01,rep(0.02,11)),  
  mortrate_d_u  = rep(0.075,16),                  
  maturalage    = c(9999,0,5,5,5,rep(5,11)),   
  v_seed        = c(9999,0.1,0.1,0.1,0.1,rep(0.1,11)),     
  fNSNmax       = rep(5,16),                      
  LMA           = c(9999,0.025,0.025,0.025,0.14,rep(0.14,11)), 
  rho_wood      = c(9999,120,350,350,300,rep(300,11)),         
  alphaBM       = rep(5200,16),                   
  thetaBM       = rep(2.5,16), 
  # add calibratable params
  kphio         = rep(0.05,16),
  phiRL         = rep(3.5,16),  
  LAI_light     = rep(3.5,16)   
) 

params_soil <- tibble(
  type              = c("Coarse","Medium","Fine","CM","CF","MF","CMF","Peat","MCM"),
  GMD               = c(0.7, 0.4, 0.3, 0.1, 0.1, 0.07, 0.007, 0.3, 0.3),
  GSD               = c(5.0, 5.3, 7.4, 6.1, 6.1, 14.0, 15.0, 7.4, 7.4),
  vwc_sat           = c(0.380, 0.445, 0.448, 0.412, 0.414, 0.446, 0.424, 0.445, 0.445),
  chb               = c(3.5,6.4,11.0,4.8,6.3,8.4,6.3,6.4,6.4),
  psi_sat_ref       = c(-600, -790, -910, -1580, -1680, -1880, -5980, -790, -790),
  k_sat_ref         = c(130.8, 75.1, 53.2, 12.1, 11.1, 12.7, 1.69, 53.2, 53.2), 
  alphaSoil         = rep(1, 9),
  heat_capacity_dry = c(1.2e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.4e6, 1.0)
)

init_cohort <- tibble(
  init_cohort_species = seq(1,10,1),   
  init_cohort_nindivs = rep(0.008,10),  
  init_cohort_bsw     = rep(0.2,10),  
  init_cohort_bHW     = rep(0.0, 10), 
  init_cohort_nsc     = rep(0.5,10)   
)

init_soil <- tibble( #list
  init_fast_soil_C    = 0.0,    
  init_slow_soil_C    = 0.0,    
  init_Nmineral       = 0.015,  
  N_input             = 0.0008  
)

df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1)
)

#This contains the forcing time series data frame where the disturbance is to be defined as the fraction 
#of aboveground biomass harvested (`harv`). Additional specifications of the disturbance forcing, 
#which are more specific to the simulations done here, are hard-coded (see below). 

#Model is first run to steady state. Then for another 100 years undisturbed (continued steady-state) 
#until first disturbance. After first disturbance left undisturbed for 900 years to allow for 
#recovery (in some simulations recovery may take long). Then a regime of repeated disturbance after 
#simulation year 1000 to investigate disturbance-recovery (non-steady state) dynamics, 
#first at low frequency for 1000 years (disturbance every 250 years), then at high frequency for 1000 
#years (disturbance every 25 years).

# Disturbance is implemented by:
# - year 100 first disturbance applied, then recovery until year 1000
# - year 1000 second disturbance, then every 250 years disturbed for 1000 years
# - ... then every 25 years disturbed for another 1000 years

#To be handled by model by (new) forcing time series as input (`harv`)
fharv <- 0.9
harv_vec <- rep(0, 999)
harv_vec[100] <- fharv
harv_vec <- c(harv_vec, rep(c(fharv, rep(0, 249)), 4), rep(c(fharv, rep(0, 24)), 40), 0)
df_harv <- tibble(year = seq(length(harv_vec)), harv = harv_vec)

df_harv <- tibble(year = seq(1:450), harv = c(rep(0,200),0,rep(0,249)))
#df_harv <- tibble(year = seq(1:450), harv = c(rep(0,100),rep(c(fharv, rep(0, 69)), 5)))

#df_harv %>%  ggplot(aes(year, harv)) + geom_line() + ylim(0, 1)

## Define forcing data ####
biomee_forcing_BIA <- read.csv("~/Documents/Collaborations/DBEN/cru_jra_1901-2020/biomee_forcing_BIA.csv")
biomee_forcing_BIA
df_forcing <- biomee_forcing_BIA

## Define CO2 ####
df_forcing$co2 <- 562 # 412

# Repeat mean seasonal cycle `nyears` times # Add harvest forcing to drivers. 
nyears <- nrow(df_harv)/length(unique(biomee_forcing_BIA$year))
df_forcing <- df_forcing %>% 
  slice(rep(1:n(), nyears)) %>% rename(yearID=year) %>%
  mutate(year = rep(1:450, each = 365)) %>% relocate(year, .after=yearID) %>%
  mutate(hour=11.5)

# Add harvest to forcing, assuming harvest on Jan 1st.
df_forcing_disturb <- df_forcing %>% 
  left_join(
    df_harv %>% 
      mutate(doy = 1),
    by = c("doy", "year")
  ) %>% 
  mutate(harv = ifelse(is.na(harv), 0, harv))

## for control simulation
df_forcing <- df_forcing %>%
  mutate(harv = 0)

# Add N deposition as NOx and NHy.
df_forcing <- df_forcing %>% 
  mutate(nox = 0, nhy = 0)

df_forcing_disturb <- df_forcing_disturb %>% 
  mutate(nox = 0, nhy = 0)

## for versions above 4.0
df_drivers_disturb <-tibble(sitename = site_info$sitename,
                            site_info = list(tibble(site_info)),
                            params_siml = list(tibble(params_siml)),
                            params_tile = list(tibble(params_tile)),
                            params_species=list(tibble(params_species)),
                            params_soil=list(tibble(params_soil)),
                            init_cohort=list(tibble(init_cohort)),
                            init_soil=list(tibble(init_soil)),
                            forcing=list(tibble(df_forcing_disturb)),
                            .name_repair = "unique")

### Run the model
out_sc1 <- runread_biomee_f(
  df_drivers_disturb,
  makecheck = TRUE,
  parallel = FALSE
)

g1 <- out_sc1$data[[1]]$output_annual_tile %>%
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  theme_classic()+labs(x = "Year", y = "plantC") 

g2 <- out_sc1$data[[1]]$output_annual_cohorts %>% group_by(PFT,year) %>%
  summarise(sumBA=sum(DBH*DBH*pi/4*density/10000)) %>% mutate(PFT=as.factor(PFT)) %>%
  ggplot() +
  geom_line(aes(x = year, y = sumBA,col=PFT)) +
  theme_classic()+labs(x = "Year", y = "BA") + 
  scale_colour_discrete(labels = c("Grass","Broadleaf1","Broadleaf2","Needleleaf"))

g3 <- out_sc1$data[[1]]$output_annual_cohorts %>% group_by(PFT,year) %>%
  summarise(CrownArea=sum(Acrown*density/10000)) %>% mutate(PFT=as.factor(PFT)) %>%
  ggplot() +
  geom_line(aes(x = year, y = CrownArea,col=PFT)) +
  theme_classic()+labs(x = "Year", y = "CrownArea") + 
  scale_colour_discrete(labels = c("Grass","Broadleaf1","Broadleaf2","Needleleaf"))

g4 <- out_sc1$data[[1]]$output_annual_cohorts %>% group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% mutate(PFT=as.factor(PFT)) %>%
  ggplot() +
  geom_line(aes(x = year, y = npp,col=PFT)) +
  theme_classic()+labs(x = "Year", y = "NPP") + 
  scale_colour_discrete(labels = c("Grass","Broadleaf1","Broadleaf2","Needleleaf"))

g5 <- out_sc1$data[[1]]$output_annual_cohorts %>% group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() +
  geom_line(aes(x = year, y = BAgrowth,col=PFT)) +
  theme_classic()+labs(x = "Year", y = "BAgrowth") + 
  scale_colour_discrete(labels = c("Grass","Broadleaf1","Broadleaf2","Needleleaf"))

print(g1/g2/g3/g4/g5)

g6 <- out_sc1$data[[1]]$output_annual_tile %>%
  ggplot() +
  geom_line(aes(x = year, y = (SapwoodC+WoodC)*0.75)) +
  theme_classic()+labs(x = "Year", y = "AGW") + geom_hline(yintercept=10, col="grey")
g6

## Outputs ####
### P0 ####
write.csv(out_sc1$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_P0_BIA_eCO2_annual_tile.csv")
write.csv(out_sc1$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs_mod/562ppm/BiomeE_P0_BIA_eCO2_annual_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_daily_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_P0_BIA_eCO2_daily_tile.csv")
write.csv(out_sc1$data[[1]]$output_daily_cohorts,"~/rsofun/data/outputs_mod/562ppm/BiomeE_P0_BIA_eCO2_daily_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_hourly_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_P0_BIA_eCO2_hourly_tile.csv")

### PS-1 (0.01 or nfrequency=100) ####
write.csv(out_sc1$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS1_BIA_eCO2_annual_tile.csv")
write.csv(out_sc1$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs_mod/562ppm/BiomeE_PS1_BIA_eCO2_annual_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_daily_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS1_BIA_eCO2_daily_tile.csv")
write.csv(out_sc1$data[[1]]$output_daily_cohorts,"~/rsofun/data/outputs_mod/562ppm/BiomeE_PS1_BIA_eCO2_daily_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_hourly_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS1_BIA_eCO2_hourly_tile.csv")

### PS-2 (0.02 or nfrequency=75) ####
write.csv(out_sc1$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS2_BIA_eCO2_annual_tile.csv")
write.csv(out_sc1$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs_mod/562ppm/BiomeE_PS2_BIA_eCO2_annual_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_daily_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS2_BIA_eCO2_daily_tile.csv")
write.csv(out_sc1$data[[1]]$output_daily_cohorts,"~/rsofun/data/outputs_mod/562ppm/BiomeE_PS2_BIA_eCO2_daily_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_hourly_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS2_BIA_eCO2_hourly_tile.csv")

### PS-3 (0.04 or nfrequency=50) ####
write.csv(out_sc1$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS3_BIA_eCO2_annual_tile.csv")
write.csv(out_sc1$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs_mod/562ppm/BiomeE_PS3_BIA_eCO2_annual_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_daily_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS3_BIA_eCO2_daily_tile.csv")
write.csv(out_sc1$data[[1]]$output_daily_cohorts,"~/rsofun/data/outputs_mod/562ppm/BiomeE_PS3_BIA_eCO2_daily_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_hourly_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS3_BIA_eCO2_hourly_tile.csv")

### PS-4 (0.08 or nfrequency=25) ####
write.csv(out_sc1$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS4_BIA_eCO2_annual_tile.csv")
write.csv(out_sc1$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs_mod/562ppm/BiomeE_PS4_BIA_eCO2_annual_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_daily_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS4_BIA_eCO2_daily_tile.csv")
write.csv(out_sc1$data[[1]]$output_daily_cohorts,"~/rsofun/data/outputs_mod/562ppm/BiomeE_PS4_BIA_eCO2_daily_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_hourly_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS4_BIA_eCO2_hourly_tile.csv")

### PS-5 (0.20 or nfrequency=15) ####
write.csv(out_sc1$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS5_BIA_eCO2_annual_tile.csv")
write.csv(out_sc1$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs_mod/562ppm/BiomeE_PS5_BIA_eCO2_annual_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_daily_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS5_BIA_eCO2_daily_tile.csv")
write.csv(out_sc1$data[[1]]$output_daily_cohorts,"~/rsofun/data/outputs_mod/562ppm/BiomeE_PS5_BIA_eCO2_daily_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_hourly_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS5_BIA_eCO2_hourly_tile.csv")

### PS-6 (0.40 or nfrequency=10) ####
write.csv(out_sc1$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS6_BIA_eCO2_annual_tile.csv")
write.csv(out_sc1$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs_mod/562ppm/BiomeE_PS6_BIA_eCO2_annual_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_daily_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS6_BIA_eCO2_daily_tile.csv")
write.csv(out_sc1$data[[1]]$output_daily_cohorts,"~/rsofun/data/outputs_mod/562ppm/BiomeE_PS6_BIA_eCO2_daily_cohorts.csv")
write.csv(out_sc1$data[[1]]$output_hourly_tile,   "~/rsofun/data/outputs_mod/562ppm/BiomeE_PS6_BIA_eCO2_hourly_tile.csv")

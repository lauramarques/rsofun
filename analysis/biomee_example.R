#.rs.restartR()

library(dplyr)
library(tibble)
library(rsofun)
#library(ggplot2)
library(patchwork)
library(multidplyr)
library(devtools)

sitename <- "CH-Lae"

# Take only year 2004 to 2014, corresponding to subset of data for site CH-Lae
site_info <- tibble(
  sitename="CH-Lae",
  lon = 8.365,
  lat = 47.47808,
  elv = 700,
  year_start = 2004,
  year_end = 2014,
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
  spinupyears           = 700, 
  recycle               = 0,  
  firstyeartrend        = 2009, 
  nyeartrend            = 100,  
  outputhourly          = TRUE,
  outputdaily           = TRUE,
  do_U_shaped_mortality = TRUE,
  update_annualLAImax   = TRUE,
  do_closedN_run        = TRUE,
  do_reset_veg          = FALSE,
  dist_frequency        = 0,
  method_photosynth     = "pmodel", # pmodel, gs_leuning
  method_mortality      = "dbh" 
)

params_tile <- tibble(
  soiltype     = 3,     
  FLDCAP       = 0.4,   
  WILTPT       = 0.05,  
  K1           = 2.0,   
  K2           = 0.05,  
  K_nitrogen   = 8.0,   
  MLmixRatio   = 0.8,   
  etaN         = 0.025, 
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
  par_mort       = 1, 
  par_mort_under = 1
)

params_species <- tibble(
  # species         0 1 2 3 4    ...
  lifeform      = rep(1,16),                      # 0 for grasses; 1 for trees
  phenotype     = c(0,1,1,rep(1,13)),             # 0 for Deciduous; 1 for Evergreen
  pt            = rep(0,16),                      # 0 for C3; 1 for C4
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
  m_cond        = rep(7.0,16), 
  alpha_phot    = rep(0.06,16), 
  gamma_L       = rep(0.02,16), 
  gamma_LN      = rep(70.5 ,16),  
  gamma_SW      = rep(0.08,16),   
  gamma_FR      = rep(12.0,16),   
  tc_crit       = rep(283.16,16),  
  tc_crit_on    = rep(280.16,16),   
  gdd_crit      = rep(280.0,16),  
  betaON        = rep(0,2,16),     
  betaOFF       = rep(0,1,16),  
  # Allometry parameters
  alphaHT       = rep(36,16),                   
  thetaHT       = rep(0.5,16),                   
  alphaCA       = rep(150,16),                   
  thetaCA       = rep(1.5,16),                   
  alphaBM       = rep(5200,16),                   
  thetaBM       = c(2.36,2.30,2.54,rep(2.30,13)), 
  # Reproduction parameters
  seedlingsize  = rep(0.05,16), 
  maturalage    = rep(5,16),     
  v_seed        = rep(0.1,16), 
  # Mortality parameters
  mortrate_d_c  = rep(0.01,16),                   
  mortrate_d_u  = rep(0.075,16),  
  # Leaf parameters
  LMA           = c(0.05,0.17,0.11,rep(0.1,13)),  
  leafLS        = rep(1,16), 
  LNbase        = rep(0.8E-3,16), 
  CNleafsupport = rep(80,16),
  rho_wood      = c(590,370,350,rep(300,13)), 
  taperfactor   = rep(0.75,16),
  lAImax        = rep(3.5,16), 
  tauNSC        = rep(3,16), 
  fNSNmax       = rep(5,16),                      
  phiCSA        = rep(0.25E-4,16),    
  # C/N ratios for plant pools
  CNleaf0      = rep(25,16),  
  CNsw0        = rep(350,16),  
  CNwood0      = rep(350,16),  
  CNroot0      = rep(40,16),  
  CNseed0      = rep(20,16),  
  Nfixrate0     = rep(0,16),                      
  NfixCost0     = rep(12,16),
  internal_gap_frac      = rep(0.1,16),
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
  psi_sat_ref       = c(-600, -790, -910, -1580, -1680, -1880, -5980, -790, -790), # Pa
  k_sat_ref         = c(130.8, 75.1, 53.2, 12.1, 11.1, 12.7, 1.69, 53.2, 53.2), # mol/(s MPa m)
  alphaSoil         = rep(1, 9),
  heat_capacity_dry = c(1.2e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.4e6, 1.0)
)

init_cohort <- tibble(
  init_n_cohorts      = 1,          # number of PFTs
  init_cohort_species = seq(1, 10), # indicates sps # 1 - Fagus sylvatica
  init_cohort_nindivs = 0.05,       
  init_cohort_bl      = 0.0,        
  init_cohort_br      = 0.0,        
  init_cohort_bsw     = 0.05,       
  init_cohort_bHW     = 0.0,        
  init_cohort_seedC   = 0.0,        
  init_cohort_nsc     = 0.05        
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

load("data-raw/CH-LAE_forcing.rda")

if (params_siml$method_photosynth == "gs_leuning"){
  forcingLAE <- forcingLAE %>% 
    dplyr::group_by(lubridate::month(datehour),lubridate::day(datehour),lubridate::hour(datehour)) %>% 
    summarise_at(vars(1:13), list(~mean(., na.rm = TRUE)))
  forcing <- forcingLAE[,-c(1:3)]
  forcing <- bind_rows(replicate(800, forcing, simplify = FALSE)) # Duplicate for the # of transient years
  
} else if (params_siml$method_photosynth == "pmodel"){ #&& dt_secs != (60*60*24)){
  forcingLAE <- forcingLAE %>% 
    dplyr::group_by(lubridate::month(datehour),lubridate::day(datehour)) %>% 
    summarise_at(vars(1:13), list(~mean(., na.rm = TRUE)))
  forcing <- forcingLAE[,-c(1:2)]
  forcing <- bind_rows(replicate(800, forcing, simplify = FALSE)) # Duplicate for the # of transient years
}

if (params_siml$method_photosynth == "gs_leuning"){
  forcing <- forcing %>% mutate(Swdown = Swdown*1) # levels = *1, *1.15 and *1.30
  #forcing <- forcing %>% mutate(aCO2_AW = aCO2_AW*1.30) # levels = *1, *1.15 and *1.30
} else if (params_siml$method_photosynth == "pmodel"){ 
  forcing <- forcing %>% mutate(PAR = PAR*1) # levels = *1, *1.15 and *1.30
}

forcing <- forcing %>% rename(year=YEAR, doy= DOY, hour=HOUR, par=PAR, ppfd=Swdown, temp=TEMP, temp_soil=SoilT, rh=RH,
                              prec=RAIN, wind=WIND, patm=PRESSURE, co2=aCO2_AW, swc=SWC)

print(packageVersion("rsofun"))

## for versions above 4.0
df_drivers <- tibble(sitename,
                     site_info = list(tibble(site_info)),
                     params_siml = list(tibble(params_siml)),
                     params_tile = list(tibble(params_tile)),
                     params_species=list(tibble(params_species)),
                     params_soil=list(tibble(params_soil)),
                     init_cohort=list(tibble(init_cohort)),
                     init_soil=list(tibble(init_soil)),
                     forcing=list(tibble(forcing)),
                     .name_repair = "unique")

#for (x in 1:10) {
#install()
out <- run_biomee_f_bysite( sitename,
                            params_siml,
                            site_info,
                            forcing,
                            params_tile,
                            params_species,
                            params_soil,
                            init_cohort,
                            init_soil,
                            makecheck = TRUE
)

out$output_annual_tile$GPP[600]
out$output_annual_tile$plantC[600]

gpp <- out$output_annual_tile$GPP[600]
plantC <-out$output_annual_tile$plantC[600]
print(x)
print(gpp)
print(plantC)
#print(gg1/gg2)
#}

#gg1 <- out$output_annual_tile %>%
#  ggplot() +
#  geom_line(aes(x = year, y = GPP)) +
#  theme_classic()+labs(x = "Year", y = "GPP")

#gg2 <- out$output_annual_tile %>%
#  ggplot() +
#  geom_line(aes(x = year, y = plantC)) +
#  theme_classic()+labs(x = "Year", y = "plantC")
#print(gg1/gg2)

#out$output_annual_tile %>%
#  ggplot() +
#  geom_line(aes(x = year, y = Density)) +
#  theme_classic()+labs(x = "Year", y = "Density")

#dd4 <- out$output_annual_tile
#ee2 <- out$output_annual_cohorts

#print("Writing luxembourg.pdf")
#print(gg1/gg2)
#ggsave("luxembourg.pdf")
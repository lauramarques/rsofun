# This script calibrates the model for the target variables selected

# load packages
library(dplyr)
library(tibble)
library(rsofun)
library(ggplot2)
library(multidplyr)
library(patchwork)

# benchmark ####

# FIN ####

## Regrowth curves ####
# AGcwood (aboveground woody carbon) 
benchmark_regrowth <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_regrowth_curves.csv")
benchmark_regrowth_FIN <- benchmark_regrowth %>% filter(Biome=="Boreal")
plot_regrowth_bench <- ggplot(benchmark_regrowth_FIN,aes(x=bin_num,y=AGcwood_kgCm2_med)) + 
  geom_errorbar(aes(ymin=AGcwood_kgCm2_10, ymax=AGcwood_kgCm2_90), width=.2, col="blue") + 
  geom_point(col="blue") + geom_hline(yintercept=10, col="grey") +
  labs(x = "Years after disturbance", y = expression(paste("Aboveground woody biomass (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(0,210)) + scale_y_continuous(lim=c(0,18)) + 
  theme_classic() + ggtitle("Benchmark target")
plot_regrowth_bench

BiomeE_P0_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_FIN_aCO2_annual_tile.csv")
plot_regrowth_out <- BiomeE_P0_FIN_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW)  %>%
  ggplot() + 
  geom_line(aes(x = year, y = AGW)) + geom_hline(yintercept=10, col="grey") +
  scale_x_continuous(lim=c(31,200),breaks=seq(50,200,50)) + scale_y_continuous(lim=c(0,18)) +
  theme_classic() + ggtitle("Model output")
plot_regrowth_out

plot_regrowth_out + plot_regrowth_bench

# Regrowth plots together
regrowth_out <- BiomeE_P0_FIN_aCO2_annual_tile %>%
  slice(510+31:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:420, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW) %>% filter(year>=1&year<=210)

fig_regrowth <- ggplot() + geom_point(data=benchmark_regrowth_FIN,aes(x=bin_num,y=AGcwood_kgCm2_med),col="blue") + 
  geom_errorbar(data=benchmark_regrowth_FIN,aes(x=bin_num,y=AGcwood_kgCm2_med,
               ymin=AGcwood_kgCm2_10, ymax=AGcwood_kgCm2_90), width=.2, col="blue") + 
  geom_line(data=regrowth_out,aes(x = year, y = AGW),color="darkred",size=1) + 
  geom_hline(yintercept=10, col="grey") +
  labs(x = "Years after disturbance", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(0,210)) + scale_y_continuous(lim=c(0,22)) + 
  theme_classic() + ggtitle("Regrowth FIN") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_regrowth

## Biomass ####
# AGcwood (aboveground woody carbon) 
benchmark_biomass <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_eq_dynamics.csv")
benchmark_biomass_FIN <- benchmark_biomass %>% filter(site=="FI") %>% mutate(time=Year-1565)
plot_biomass_bench <- ggplot(benchmark_biomass_FIN,aes(x=Year,y=AGB_kgCm2)) + 
  geom_errorbar(aes(ymin=AGB_lower_kgCm2, ymax=AGB_upper_kgCm2), width=.2, col="blue") + 
  geom_point(col="blue") + geom_hline(yintercept=10, col="grey") +
  scale_y_continuous(lim=c(4,15)) + theme_classic() + ggtitle("Benchmark target")
plot_biomass_bench

BiomeE_P0_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_FIN_aCO2_annual_tile.csv")
plot_biomass_out <- BiomeE_P0_FIN_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW)  %>%
  ggplot() + 
  geom_line(aes(x = year, y = AGW)) + geom_hline(yintercept=10, col="grey") +
  scale_x_continuous(lim=c(200,450)) + scale_y_continuous(lim=c(4,15)) + 
  theme_classic() + ggtitle("Model output")
plot_biomass_out

plot_biomass_out + plot_biomass_bench

# Biomass plots together
biomass_out <- BiomeE_P0_FIN_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW) %>% filter(year>=420&year<=450)

fig_biomass <- ggplot() + geom_point(data=benchmark_biomass_FIN,aes(x=time,y=AGB_kgCm2),col="blue") + 
  geom_errorbar(data=benchmark_biomass_FIN,aes(x=time,y=AGB_kgCm2,
                ymin=AGB_lower_kgCm2, ymax=AGB_upper_kgCm2), width=.2, col="blue") + 
  geom_line(data=biomass_out,aes(x = year, y = AGW),color="darkred",size=1) + 
  #geom_hline(yintercept=10, col="grey") +
  labs(x = "Years", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(420,450)) + scale_y_continuous(lim=c(3,15)) + 
  theme_classic() + ggtitle("Biomass FIN") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_biomass

## Stand structure ####
stand_structure <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_stand_structure.csv")
stand_structure_FIN <- stand_structure %>% filter(site=="FI")
### N stems size ####
plot_stems_bench <- ggplot(stand_structure_FIN,aes(x=dbh_classes_num,y=nstem_size_ha.1)) + 
  geom_errorbar(aes(ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point(col="blue") + scale_x_continuous(lim=c(0,100),breaks = seq(0,100,10)) +
  scale_y_continuous(lim=c(0,750)) +
  theme_classic() + ggtitle("Benchmark target")
plot_stems_bench

BiomeE_P0_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_FIN_aCO2_annual_cohorts.csv")
plot_stems_out <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>%
  group_by(dbh_bins) %>%
  summarise(nstem_size=mean(nstem_size)) %>% mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90)) %>%
  ggplot() + 
  geom_point(aes(x = dbh_class_num, y = nstem_size)) + 
  scale_x_continuous(lim=c(0,60),breaks = seq(0,60,10)) +
  scale_y_continuous(lim=c(0,750)) + 
  theme_classic() + ggtitle("Model output")
plot_stems_out

plot_stems_out + plot_stems_bench

# Stems plots together
stems_out <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>%
  group_by(dbh_bins) %>%
  summarise(nstem_size=mean(nstem_size)) %>% 
  mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90)) %>% filter(dbh_class_num>=10)

fig_stems <- ggplot() + geom_point(data=stand_structure_FIN,aes(x=dbh_classes_num,y=nstem_size_ha.1),col="blue") + 
  geom_errorbar(data=stand_structure_FIN,aes(x=dbh_classes_num,y=nstem_size_ha.1,
                ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point(data=stems_out,aes(x = dbh_class_num, y = nstem_size),color="darkred",size=2) + 
  labs(x = "DBH bins (cm)", y = expression(paste("Stems ", ha^-1, ") "))) + 
  scale_x_continuous(lim=c(0,250)) + scale_y_continuous(lim=c(0,550)) + 
  theme_classic() + ggtitle("Stems size FIN") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_stems

### AGcwood size ####
# AGcwood (aboveground woody carbon) 
plot_cwood_bench <- ggplot(stand_structure_FIN,aes(x=dbh_classes_num,y=AGB_size_kgCm.2)) + 
  geom_errorbar(aes(ymin=AGB_size_lower_kgCm.2, ymax=AGB_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point(col="blue") + scale_y_continuous(limits = c(0,6.5)) + 
  theme_classic() + ggtitle("Benchmark target")
plot_cwood_bench

BiomeE_P0_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_FIN_aCO2_annual_cohorts.csv")
plot_cwood_out <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*0.75*density/10000)) %>%
  group_by(dbh_bins) %>%
  summarise(cwood_size=mean(cwood_size)) %>% mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90)) %>%
  ggplot() + 
  geom_point(aes(x = dbh_class_num, y = cwood_size)) + 
  scale_y_continuous(limits = c(0,6.5)) + 
  theme_classic() + ggtitle("Model output")
plot_cwood_out

plot_cwood_out + plot_cwood_bench

# Cwood plots together
cwood_out <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*0.75*density/10000)) %>%
  group_by(dbh_bins) %>%
  summarise(cwood_size=mean(cwood_size)) %>% 
  mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90)) %>% filter(dbh_class_num>=10)

fig_cwood <- ggplot() + geom_point(data=stand_structure_FIN,aes(x=dbh_classes_num,y=AGB_size_kgCm.2),col="blue") + 
  geom_errorbar(data=stand_structure_FIN,aes(x=dbh_classes_num,y=AGB_size_kgCm.2,
                                             ymin=AGB_size_lower_kgCm.2, ymax=AGB_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point(data=cwood_out,aes(x = dbh_class_num, y = cwood_size),color="darkred",size=2) + 
  labs(x = "DBH bins (cm)", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(0,250)) + scale_y_continuous(lim=c(0,6)) + 
  theme_classic() + ggtitle("AGcwood FIN") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_cwood

# All plots
fig_regrowth + fig_biomass + fig_stems + fig_cwood +
  plot_layout(ncol = 1) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("~/rsofun/data/figures/BiomeEP_P0_FIN_412ppm_Calibration.pdf", width = 6, height = 8, dpi=300)

plot_regrowth_out + plot_regrowth_bench + plot_biomass_out + plot_biomass_bench +
  plot_cwood_out + plot_cwood_bench + plot_stems_out + plot_stems_bench +
  plot_layout(ncol = 2) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("~/rsofun/data/figures/BiomeEP_P0_FIN_412ppm_Calibration_both.pdf", width = 6, height = 8, dpi=300)

# BIA ####

## Regrowth curves ####
# AGcwood (aboveground woody carbon) 
benchmark_regrowth <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_regrowth_curves.csv")
benchmark_regrowth_BIA <- benchmark_regrowth %>% filter(Biome=="Temperate")
plot_regrowth_bench <- ggplot(benchmark_regrowth_BIA,aes(x=bin_num,y=AGcwood_kgCm2_med)) + 
  geom_errorbar(aes(ymin=AGcwood_kgCm2_10, ymax=AGcwood_kgCm2_90), width=.2, col="blue") + 
  geom_point(col="blue") + geom_hline(yintercept=10, col="grey") +
  scale_x_continuous(lim=c(0,210)) + scale_y_continuous(lim=c(0,18)) + 
  theme_classic() + ggtitle("Benchmark target")
plot_regrowth_bench

BiomeE_P0_BIA_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_annual_tile.csv")
plot_regrowth_out <- BiomeE_P0_BIA_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_BIA_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW)  %>%
  ggplot() + 
  geom_line(aes(x = year, y = AGW)) + geom_hline(yintercept=10, col="grey") +
  scale_x_continuous(lim=c(31,100)) + scale_y_continuous(lim=c(0,18)) +
  theme_classic() + ggtitle("Model output")
plot_regrowth_out

plot_regrowth_out + plot_regrowth_bench

# Regrowth plots together
regrowth_out <- BiomeE_P0_BIA_aCO2_annual_tile %>%
  slice(510+31:nrow(BiomeE_P0_BIA_aCO2_annual_tile)) %>% 
  mutate(year = 1:420, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW) %>% filter(year>=1&year<=210)

fig_regrowth <- ggplot() + geom_point(data=benchmark_regrowth_BIA,aes(x=bin_num,y=AGcwood_kgCm2_med),col="blue") + 
  geom_errorbar(data=benchmark_regrowth_BIA,aes(x=bin_num,y=AGcwood_kgCm2_med,
                                                ymin=AGcwood_kgCm2_10, ymax=AGcwood_kgCm2_90), width=.2, col="blue") + 
  geom_line(data=regrowth_out,aes(x = year, y = AGW),color="darkred",size=1) + 
  geom_hline(yintercept=10, col="grey") +
  labs(x = "Years after disturbance", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(0,210)) + scale_y_continuous(lim=c(0,22)) + 
  theme_classic() + ggtitle("Regrowth BIA") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_regrowth

## Biomass ####
# AGcwood (aboveground woody carbon) 
benchmark_biomass <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_eq_dynamics.csv")
benchmark_biomass_BIA <- benchmark_biomass %>% filter(site=="BIA") %>% mutate(time=Year-1565)
plot_biomass_bench <- ggplot(benchmark_biomass_BIA,aes(x=Year,y=AGB_kgCm2)) + 
  geom_point(col="blue") +
  geom_errorbar(aes(ymin=AGB_lower_kgCm2, ymax=AGB_upper_kgCm2), width=.2, col="blue") + 
   geom_hline(yintercept=10, col="grey") +
  scale_y_continuous(lim=c(4,15)) + theme_classic() + ggtitle("Benchmark target")
plot_biomass_bench

BiomeE_P0_BIA_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_annual_tile.csv")
plot_biomass_out <- BiomeE_P0_BIA_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_BIA_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW)  %>%
  ggplot() + 
  geom_line(aes(x = year, y = AGW)) + geom_hline(yintercept=10, col="grey") +
  scale_x_continuous(lim=c(200,450)) + scale_y_continuous(lim=c(4,15)) + 
  theme_classic() + ggtitle("Model output")
plot_biomass_out

plot_biomass_out + plot_biomass_bench

# Biomass plots together
biomass_out <- BiomeE_P0_BIA_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_BIA_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW) %>% filter(year>=370&year<=450)

fig_biomass <- ggplot() + geom_point(data=benchmark_biomass_BIA,aes(x=time,y=AGB_kgCm2),col="blue") + 
  geom_errorbar(data=benchmark_biomass_BIA,aes(x=time,y=AGB_kgCm2,
                ymin=AGB_lower_kgCm2, ymax=AGB_upper_kgCm2), width=.2, col="blue") + 
  geom_line(data=biomass_out,aes(x = year, y = AGW),color="darkred",size=1) + 
  #geom_hline(yintercept=10, col="grey") +
  labs(x = "Years", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(365,450)) + scale_y_continuous(lim=c(8,15)) + 
  theme_classic() + ggtitle("Biomass BIA") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_biomass

## Stand structure ####
stand_structure <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_stand_structure.csv")
stand_structure_BIA <- stand_structure %>% filter(site=="BIA")
### N stems size ####
plot_stems_bench <- ggplot(stand_structure_BIA,aes(x=dbh_classes_num,y=nstem_size_ha.1)) + 
  geom_errorbar(aes(ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point(col="blue") + scale_x_continuous(lim=c(0,100),breaks = seq(0,100,10)) +
  scale_y_continuous(lim=c(0,750)) +
  theme_classic() + ggtitle("Benchmark target")
plot_stems_bench

BiomeE_P0_BIA_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_annual_cohorts.csv")
plot_stems_out <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>%
  group_by(dbh_bins) %>%
  summarise(nstem_size=mean(nstem_size)) %>% mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90)) %>%
  ggplot() + 
  geom_point(aes(x = dbh_class_num, y = nstem_size)) + 
  scale_x_continuous(lim=c(0,60),breaks = seq(0,60,10)) +
  scale_y_continuous(lim=c(0,750)) + 
  theme_classic() + ggtitle("Model output")
plot_stems_out

plot_stems_out + plot_stems_bench

# Stems plots together
stems_out <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>%
  group_by(dbh_bins) %>%
  summarise(nstem_size=mean(nstem_size)) %>% 
  mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90)) %>% filter(dbh_class_num>=15)

fig_stems <- ggplot() + geom_point(data=stand_structure_BIA,aes(x=dbh_classes_num,y=nstem_size_ha.1),col="blue") + 
  geom_errorbar(data=stand_structure_BIA,aes(x=dbh_classes_num,y=nstem_size_ha.1,
                                             ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point(data=stems_out,aes(x = dbh_class_num, y = nstem_size),color="darkred",size=2) + 
  labs(x = "DBH bins (cm)", y = expression(paste("Stems ", ha^-1, ") "))) + 
  scale_x_continuous(lim=c(0,250)) + scale_y_continuous(lim=c(0,550)) + 
  theme_classic() + ggtitle("Stems size BIA") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_stems

### AGcwood size ####
# AGcwood (aboveground woody carbon) 
plot_cwood_bench <- ggplot(stand_structure_BIA,aes(x=dbh_classes_num,y=AGB_size_kgCm.2)) + 
  geom_errorbar(aes(ymin=AGB_size_lower_kgCm.2, ymax=AGB_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point(col="blue") + scale_y_continuous(limits = c(0,6.5)) + 
  theme_classic() + ggtitle("Benchmark target")
plot_cwood_bench

BiomeE_P0_BIA_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_annual_cohorts.csv")
plot_cwood_out <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*0.75*density/10000)) %>%
  group_by(dbh_bins) %>%
  summarise(cwood_size=mean(cwood_size)) %>% mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90)) %>%
  ggplot() + 
  geom_point(aes(x = dbh_class_num, y = cwood_size)) + 
  scale_y_continuous(limits = c(0,6.5)) + 
  theme_classic() + ggtitle("Model output")
plot_cwood_out

plot_cwood_out + plot_cwood_bench

# Cwood plots together
cwood_out <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*0.75*density/10000)) %>%
  group_by(dbh_bins) %>%
  summarise(cwood_size=mean(cwood_size)) %>% 
  mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90)) %>% filter(dbh_class_num>=15)

fig_cwood <- ggplot() + geom_point(data=stand_structure_BIA,aes(x=dbh_classes_num,y=AGB_size_kgCm.2),col="blue") + 
  geom_errorbar(data=stand_structure_BIA,aes(x=dbh_classes_num,y=AGB_size_kgCm.2,
                ymin=AGB_size_lower_kgCm.2, ymax=AGB_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point(data=cwood_out,aes(x = dbh_class_num, y = cwood_size),color="darkred",size=2) + 
  labs(x = "DBH bins (cm)", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(0,250)) + scale_y_continuous(lim=c(0,6)) + 
  theme_classic() + ggtitle("AGcwood BIA") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_cwood

# All plots
fig_regrowth + fig_biomass + fig_stems + fig_cwood +
  plot_layout(ncol = 1) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("~/rsofun/data/figures/BiomeEP_P0_BIA_412ppm_Calibration.pdf", width = 6, height = 8, dpi=300)

plot_regrowth_out + plot_regrowth_bench + plot_biomass_out + plot_biomass_bench +
  plot_cwood_out + plot_cwood_bench + plot_stems_out + plot_stems_bench +
  plot_layout(ncol = 2) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("~/rsofun/data/figures/BiomeEP_P0_BIA_412ppm_Calibration_both.pdf", width = 6, height = 8, dpi=300)

# BCI ####

## Regrowth curves ####
# AGcwood (aboveground woody carbon) 
benchmark_regrowth <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_regrowth_curves.csv")
benchmark_regrowth_BCI <- benchmark_regrowth %>% filter(Biome=="Tropics")
plot_regrowth_bench <- ggplot(benchmark_regrowth_BCI,aes(x=bin_num,y=AGcwood_kgCm2_med)) + 
  geom_errorbar(aes(ymin=AGcwood_kgCm2_10, ymax=AGcwood_kgCm2_90), width=.2, col="blue") + 
  geom_point(col="blue") + geom_hline(yintercept=10, col="grey") +
  #scale_x_continuous(lim=c(0,210)) + scale_y_continuous(lim=c(0,18)) + 
  theme_classic() + ggtitle("Benchmark target")
plot_regrowth_bench

BiomeE_P0_BCI_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BCI_aCO2_annual_tile.csv")
plot_regrowth_out <- BiomeE_P0_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW)  %>%
  ggplot() + 
  geom_line(aes(x = year, y = AGW)) + geom_hline(yintercept=10, col="grey") +
  scale_x_continuous(lim=c(31,100)) + scale_y_continuous(lim=c(0,18)) +
  theme_classic() + ggtitle("Model output")
plot_regrowth_out

plot_regrowth_out + plot_regrowth_bench

# Regrowth plots together
regrowth_out <- BiomeE_P0_BCI_aCO2_annual_tile %>%
  slice(510+31:nrow(BiomeE_P0_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:420, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW) %>% filter(year>=1&year<=210)

fig_regrowth <- ggplot() + geom_point(data=benchmark_regrowth_BCI,aes(x=bin_num,y=AGcwood_kgCm2_med),col="blue") + 
  geom_errorbar(data=benchmark_regrowth_BCI,aes(x=bin_num,y=AGcwood_kgCm2_med,
                                                ymin=AGcwood_kgCm2_10, ymax=AGcwood_kgCm2_90), width=.2, col="blue") + 
  geom_line(data=regrowth_out,aes(x = year, y = AGW),color="darkred",size=1) + 
  geom_hline(yintercept=10, col="grey") +
  labs(x = "Years after disturbance", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(0,210)) + scale_y_continuous(lim=c(0,22)) + 
  theme_classic() + ggtitle("Regrowth BCI") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_regrowth

## Biomass ####
# AGcwood (aboveground woody carbon) 
benchmark_biomass <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_eq_dynamics.csv")
benchmark_biomass_BCI <- benchmark_biomass %>% filter(site=="BCI") %>% mutate(time=Year-1565)
plot_biomass_bench <- ggplot(benchmark_biomass_BCI,aes(x=Year,y=AGB_kgCm2)) + 
  geom_errorbar(aes(ymin=AGB_lower_kgCm2, ymax=AGB_upper_kgCm2), width=.2, col="blue") + 
  geom_point(col="blue") + geom_hline(yintercept=15, col="grey") +
  scale_y_continuous(lim=c(10,25)) + 
  theme_classic() + ggtitle("Benchmark target")
plot_biomass_bench

BiomeE_P0_BCI_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BCI_aCO2_annual_tile.csv")
plot_biomass_out <- BiomeE_P0_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW)  %>%
  ggplot() + 
  geom_line(aes(x = year, y = AGW)) + geom_hline(yintercept=15, col="grey") +
  scale_x_continuous(lim=c(200,450)) + scale_y_continuous(lim=c(10,25)) + 
  theme_classic() + ggtitle("Model output")
plot_biomass_out

plot_biomass_out + plot_biomass_bench

# Biomass plots together
biomass_out <- BiomeE_P0_BCI_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_BCI_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW) %>% filter(year>=415&year<=450)

fig_biomass <- ggplot() + geom_point(data=benchmark_biomass_BCI,aes(x=time,y=AGB_kgCm2),col="blue") + 
  geom_errorbar(data=benchmark_biomass_BCI,aes(x=time,y=AGB_kgCm2,
                                               ymin=AGB_lower_kgCm2, ymax=AGB_upper_kgCm2), width=.2, col="blue") + 
  geom_line(data=biomass_out,aes(x = year, y = AGW),color="darkred",size=1) + 
  #geom_hline(yintercept=10, col="grey") +
  labs(x = "Years", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(415,450)) + scale_y_continuous(lim=c(10,25)) + 
  theme_classic() + ggtitle("Biomass BCI") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_biomass

## Stand structure ####
stand_structure <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_stand_structure.csv")
stand_structure_BCI <- stand_structure %>% filter(site=="BCI")
### N stems size ####
plot_stems_bench <- ggplot(stand_structure_BCI,aes(x=dbh_classes_num,y=nstem_size_ha.1)) + 
  geom_errorbar(aes(ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point(col="blue") + scale_x_continuous(lim=c(0,100),breaks = seq(0,100,10)) +
  scale_y_continuous(lim=c(0,750)) +
  theme_classic() + ggtitle("Benchmark target")
plot_stems_bench

BiomeE_P0_BCI_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BCI_aCO2_annual_cohorts.csv")
plot_stems_out <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>%
  group_by(dbh_bins) %>%
  summarise(nstem_size=mean(nstem_size)) %>% mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90,100,150)) %>%
  ggplot() + 
  geom_point(aes(x = dbh_class_num, y = nstem_size)) + 
  scale_x_continuous(lim=c(0,150),breaks = seq(0,150,10)) +
  scale_y_continuous(lim=c(0,750)) + 
  theme_classic() + ggtitle("Model output")
plot_stems_out

plot_stems_out + plot_stems_bench

# Stems plots together
stems_out <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>%
  group_by(dbh_bins) %>%
  summarise(nstem_size=mean(nstem_size)) %>% 
  mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90,100,150)) %>% filter(dbh_class_num>=5)

fig_stems <- ggplot() + geom_point(data=stand_structure_BCI,aes(x=dbh_classes_num,y=nstem_size_ha.1),col="blue") + 
  geom_errorbar(data=stand_structure_BCI,aes(x=dbh_classes_num,y=nstem_size_ha.1,
                ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point(data=stems_out,aes(x = dbh_class_num, y = nstem_size),color="darkred",size=2) + 
  labs(x = "DBH bins (cm)", y = expression(paste("Stems ", ha^-1, ") "))) + 
  #scale_x_continuous(lim=c(0,250)) + scale_y_continuous(lim=c(0,550)) + 
  theme_classic() + ggtitle("Stems size BCI") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_stems

### AGcwood size ####
# AGcwood (aboveground woody carbon) 
plot_cwood_bench <- ggplot(stand_structure_BCI,aes(x=dbh_classes_num,y=AGB_size_kgCm.2)) + 
  geom_errorbar(aes(ymin=AGB_size_lower_kgCm.2, ymax=AGB_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point(col="blue") + scale_y_continuous(limits = c(0,6.5)) + 
  theme_classic() + ggtitle("Benchmark target")
plot_cwood_bench

BiomeE_P0_BCI_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BCI_aCO2_annual_cohorts.csv")
plot_cwood_out <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*0.75*density/10000)) %>%
  group_by(dbh_bins) %>%
  summarise(cwood_size=mean(cwood_size)) %>% mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90,100,150)) %>%
  ggplot() + 
  geom_point(aes(x = dbh_class_num, y = cwood_size)) + 
  scale_y_continuous(limits = c(0,6.5)) + 
  theme_classic() + ggtitle("Model output")
plot_cwood_out

plot_cwood_out + plot_cwood_bench

# Cwood plots together
cwood_out <- BiomeE_P0_BCI_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*0.75*density/10000)) %>%
  group_by(dbh_bins) %>%
  summarise(cwood_size=mean(cwood_size)) %>% 
  mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90,100,150)) %>% filter(dbh_class_num>=5)

fig_cwood <- ggplot() + geom_point(data=stand_structure_BCI,aes(x=dbh_classes_num,y=AGB_size_kgCm.2),col="blue") + 
  geom_errorbar(data=stand_structure_BCI,aes(x=dbh_classes_num,y=AGB_size_kgCm.2,
                                             ymin=AGB_size_lower_kgCm.2, ymax=AGB_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point(data=cwood_out,aes(x = dbh_class_num, y = cwood_size),color="darkred",size=2) + 
  labs(x = "DBH bins (cm)", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(0,250)) + scale_y_continuous(lim=c(0,6.5)) + 
  theme_classic() + ggtitle("AGcwood BCI") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_cwood

# All plots
fig_regrowth + fig_biomass + fig_stems + fig_cwood +
  plot_layout(ncol = 1) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("~/rsofun/data/figures/BiomeEP_P0_BCI_412ppm_Calibration.pdf", width = 6, height = 8, dpi=300)

plot_regrowth_out + plot_regrowth_bench + plot_biomass_out + plot_biomass_bench +
  plot_cwood_out + plot_cwood_bench + plot_stems_out + plot_stems_bench +
  plot_layout(ncol = 2) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("~/rsofun/data/figures/BiomeEP_P0_BCI_412ppm_Calibration_both.pdf", width = 6, height = 8, dpi=300)






# _________________####
# Calibration ####
## Target 1. Regrowth
regrowth_dynamics_FIN # kg C m-2

## Target 2. AGB - Biomass dynamics
regrowth_dynamics_FIN # kg C m-2

## Target 3. N stems per ha
nstem_FIN <- stand_structure_FIN %>% select(dbh_classes_num,nstem_size_ha.1) %>% rename(nstem_ha=nstem_size_ha.1)

## Target 3. N stems per ha
nstem_FIN <- stand_structure_FIN %>% select(dbh_classes_num,nstem_size_ha.1) %>% rename(nstem_ha=nstem_size_ha.1)
cwood_FIN <- stand_structure_FIN %>% select(dbh_classes_num,nstem_size_ha.1) %>% rename(nstem_ha=nstem_size_ha.1)


# Prepare the observed target variables: ddf_obs
ddf_obs <- data.frame(
  variables = c("regrowth","biomass","stem_c1","stem_c2","stem_c3","stem_c4","stem_c5","stem_c6","stem_c7",
                "stem_c8","stem_c9","stem_c10","stem_c11","stem_c12","stem_c13","stem_c14","stem_c15","stem_c16",
                "cwood_c1","cwood_c2","cwood_c3","cwood_c4","cwood_c5","cwood_c6","cwood_c7",
                "cwood_c8","cwood_c9","cwood_c10","cwood_c11","cwood_c12","cwood_c13","cwood_c14","cwood_c15","cwood_c16"),
  targets_obs = c(XXXXXXX, XXXXXXX,
                  nstem_FIN$nstem_ha[1],nstem_FIN$nstem_ha[2],nstem_FIN$nstem_ha[3],nstem_FIN$nstem_ha[4],
                  nstem_FIN$nstem_ha[5],nstem_FIN$nstem_ha[6],nstem_FIN$nstem_ha[7],nstem_FIN$nstem_ha[8],
                  nstem_FIN$nstem_ha[9],nstem_FIN$nstem_ha[10],nstem_FIN$nstem_ha[11],nstem_FIN$nstem_ha[12],
                  nstem_FIN$nstem_ha[13],nstem_FIN$nstem_ha[14],nstem_FIN$nstem_ha[15],nstem_FIN$nstem_ha[16],
                  nstem_FIN$nstem_ha[1],nstem_FIN$nstem_ha[2],nstem_FIN$nstem_ha[3],nstem_FIN$nstem_ha[4],
                  nstem_FIN$nstem_ha[5],nstem_FIN$nstem_ha[6],nstem_FIN$nstem_ha[7],nstem_FIN$nstem_ha[8],
                  nstem_FIN$nstem_ha[9],nstem_FIN$nstem_ha[10],nstem_FIN$nstem_ha[11],nstem_FIN$nstem_ha[12],
                  nstem_FIN$nstem_ha[13],nstem_FIN$nstem_ha[14],nstem_FIN$nstem_ha[15],nstem_FIN$nstem_ha[16])
) 
save(ddf_obs, file = "~/Documents/Collaborations/DBEN/inputs_mod/ddf_obs.RData")

# FIN 2 ####

## Regrowth curves ####
# AGcwood (aboveground woody carbon) 
benchmark_regrowth <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_regrowth_curves.csv")
benchmark_regrowth_FIN <- benchmark_regrowth %>% filter(Biome=="Boreal")

regrowth_out <- out_sc1$data[[1]]$output_annual_tile %>%
  slice(510+31:nrow(out_sc1$data[[1]]$output_annual_tile)) %>% 
  mutate(year = 1:420, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW) %>% filter(year>=1&year<=210)

fig_regrowth <- ggplot() + geom_point(data=benchmark_regrowth_FIN,aes(x=bin_num,y=AGcwood_kgCm2_med),col="blue") + 
  geom_errorbar(data=benchmark_regrowth_FIN,aes(x=bin_num,y=AGcwood_kgCm2_med,
                                                ymin=AGcwood_kgCm2_10, ymax=AGcwood_kgCm2_90), width=.2, col="blue") + 
  geom_line(data=regrowth_out,aes(x = year, y = AGW),color="darkred",size=1) + 
  geom_hline(yintercept=10, col="grey") +
  labs(x = "Years after disturbance", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(0,210)) + scale_y_continuous(lim=c(0,22)) + 
  theme_classic() + ggtitle("Regrowth FIN") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_regrowth

## Biomass ####
# AGcwood (aboveground woody carbon) 
benchmark_biomass <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_eq_dynamics.csv")
benchmark_biomass_FIN <- benchmark_biomass %>% filter(site=="FI") %>% mutate(time=Year-1565)

biomass_out <- out_sc1$data[[1]]$output_annual_tile %>%
  slice(510+1:nrow(out_sc1$data[[1]]$output_annual_tile)) %>% 
  mutate(year = 1:450, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW) %>% filter(year>=420&year<=450)

fig_biomass <- ggplot() + geom_point(data=benchmark_biomass_FIN,aes(x=time,y=AGB_kgCm2),col="blue") + 
  geom_errorbar(data=benchmark_biomass_FIN,aes(x=time,y=AGB_kgCm2,
                                               ymin=AGB_lower_kgCm2, ymax=AGB_upper_kgCm2), width=.2, col="blue") + 
  geom_line(data=biomass_out,aes(x = year, y = AGW),color="darkred",size=1) + 
  #geom_hline(yintercept=10, col="grey") +
  labs(x = "Years", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(420,450)) + scale_y_continuous(lim=c(3,15)) + 
  theme_classic() + ggtitle("Biomass FIN") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_biomass

## Stand structure ####
stand_structure <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_stand_structure.csv")
stand_structure_FIN <- stand_structure %>% filter(site=="FI")
### N stems size ####

stems_out <- out_sc1$data[[1]]$output_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>%
  group_by(dbh_bins) %>%
  summarise(nstem_size=mean(nstem_size)) %>% 
  mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90)) %>% filter(dbh_class_num>=10)

fig_stems <- ggplot() + geom_point(data=stand_structure_FIN,aes(x=dbh_classes_num,y=nstem_size_ha.1),col="blue") + 
  geom_errorbar(data=stand_structure_FIN,aes(x=dbh_classes_num,y=nstem_size_ha.1,
                                             ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point(data=stems_out,aes(x = dbh_class_num, y = nstem_size),color="darkred",size=2) + 
  labs(x = "DBH bins (cm)", y = expression(paste("Stems ", ha^-1, ") "))) + 
  scale_x_continuous(lim=c(0,250)) + scale_y_continuous(lim=c(0,550)) + 
  theme_classic() + ggtitle("Stems size FIN") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_stems

### AGcwood size ####
cwood_out <- out_sc1$data[[1]]$output_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*0.75*density/10000)) %>%
  group_by(dbh_bins) %>%
  summarise(cwood_size=mean(cwood_size)) %>% 
  mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90)) %>% filter(dbh_class_num>=10)

fig_cwood <- ggplot() + geom_point(data=stand_structure_FIN,aes(x=dbh_classes_num,y=AGB_size_kgCm.2),col="blue") + 
  geom_errorbar(data=stand_structure_FIN,aes(x=dbh_classes_num,y=AGB_size_kgCm.2,
                                             ymin=AGB_size_lower_kgCm.2, ymax=AGB_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point(data=cwood_out,aes(x = dbh_class_num, y = cwood_size),color="darkred",size=2) + 
  labs(x = "DBH bins (cm)", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(0,250)) + scale_y_continuous(lim=c(0,6)) + 
  theme_classic() + ggtitle("AGcwood FIN") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_cwood

# All plots
fig_regrowth + fig_biomass + fig_stems + fig_cwood +
  plot_layout(ncol = 1) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("~/rsofun/data/figures/BiomeEP_P0_FIN_412ppm_Calibration2.pdf", width = 6, height = 8, dpi=300)

# BIA 2 ####

## Regrowth curves ####
# AGcwood (aboveground woody carbon) 
benchmark_regrowth <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_regrowth_curves.csv")
benchmark_regrowth_BIA <- benchmark_regrowth %>% filter(Biome=="Temperate")

# Regrowth plots together
regrowth_out <- out_sc1$data[[1]]$output_annual_tile %>%
  slice(510+31:nrow(out_sc1$data[[1]]$output_annual_tile)) %>% 
  mutate(year = 1:420, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW) %>% filter(year>=1&year<=210)

fig_regrowth <- ggplot() + geom_point(data=benchmark_regrowth_BIA,aes(x=bin_num,y=AGcwood_kgCm2_med),col="blue") + 
  geom_errorbar(data=benchmark_regrowth_BIA,aes(x=bin_num,y=AGcwood_kgCm2_med,
                                                ymin=AGcwood_kgCm2_10, ymax=AGcwood_kgCm2_90), width=.2, col="blue") + 
  geom_line(data=regrowth_out,aes(x = year, y = AGW),color="darkred",size=1) + 
  geom_hline(yintercept=10, col="grey") +
  labs(x = "Years after disturbance", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(0,210)) + scale_y_continuous(lim=c(0,22)) + 
  theme_classic() + ggtitle("Regrowth BIA") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_regrowth

## Biomass ####
# AGcwood (aboveground woody carbon) 
benchmark_biomass <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_eq_dynamics.csv")
benchmark_biomass_BIA <- benchmark_biomass %>% filter(site=="BIA") %>% mutate(time=Year-1565)

# Biomass plots together
biomass_out <- out_sc1$data[[1]]$output_annual_tile %>%
  slice(510+1:nrow(out_sc1$data[[1]]$output_annual_tile)) %>% 
  mutate(year = 1:450, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW) %>% filter(year>=370&year<=450)

fig_biomass <- ggplot() + geom_point(data=benchmark_biomass_BIA,aes(x=time,y=AGB_kgCm2),col="blue") + 
  geom_errorbar(data=benchmark_biomass_BIA,aes(x=time,y=AGB_kgCm2,
                                               ymin=AGB_lower_kgCm2, ymax=AGB_upper_kgCm2), width=.2, col="blue") + 
  geom_line(data=biomass_out,aes(x = year, y = AGW),color="darkred",size=1) + 
  #geom_hline(yintercept=10, col="grey") +
  labs(x = "Years", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(365,450)) + scale_y_continuous(lim=c(8,15)) + 
  theme_classic() + ggtitle("Biomass BIA") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_biomass

## Stand structure ####
stand_structure <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_stand_structure.csv")
stand_structure_BIA <- stand_structure %>% filter(site=="BIA")
### N stems size ####
stems_out <- out_sc1$data[[1]]$output_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>%
  group_by(dbh_bins) %>%
  summarise(nstem_size=mean(nstem_size)) %>% 
  mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90,100)) %>% filter(dbh_class_num>=15)

fig_stems <- ggplot() + geom_point(data=stand_structure_BIA,aes(x=dbh_classes_num,y=nstem_size_ha.1),col="blue") + 
  geom_errorbar(data=stand_structure_BIA,aes(x=dbh_classes_num,y=nstem_size_ha.1,
                                             ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point(data=stems_out,aes(x = dbh_class_num, y = nstem_size),color="darkred",size=2) + 
  labs(x = "DBH bins (cm)", y = expression(paste("Stems ", ha^-1, ") "))) + 
  scale_x_continuous(lim=c(0,250)) + scale_y_continuous(lim=c(0,550)) + 
  theme_classic() + ggtitle("Stems size BIA") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_stems

### AGcwood size ####
cwood_out <- out_sc1$data[[1]]$output_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*0.75*density/10000)) %>%
  group_by(dbh_bins) %>%
  summarise(cwood_size=mean(cwood_size)) %>% 
  mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90,100)) %>% filter(dbh_class_num>=15)

fig_cwood <- ggplot() + geom_point(data=stand_structure_BIA,aes(x=dbh_classes_num,y=AGB_size_kgCm.2),col="blue") + 
  geom_errorbar(data=stand_structure_BIA,aes(x=dbh_classes_num,y=AGB_size_kgCm.2,
                                             ymin=AGB_size_lower_kgCm.2, ymax=AGB_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point(data=cwood_out,aes(x = dbh_class_num, y = cwood_size),color="darkred",size=2) + 
  labs(x = "DBH bins (cm)", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(0,250)) + scale_y_continuous(lim=c(0,6)) + 
  theme_classic() + ggtitle("AGcwood BIA") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_cwood

# All plots
fig_regrowth + fig_biomass + fig_stems + fig_cwood +
  plot_layout(ncol = 1) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')


# BCI 2 ####

## Regrowth curves ####
# AGcwood (aboveground woody carbon) 
benchmark_regrowth <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_regrowth_curves.csv")
benchmark_regrowth_BCI <- benchmark_regrowth %>% filter(Biome=="Tropics")

# Regrowth plots together
regrowth_out <- out_sc1$data[[1]]$output_annual_tile %>%
  slice(510+31:nrow(out_sc1$data[[1]]$output_annual_tile)) %>% 
  mutate(year = 1:420, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW) %>% filter(year>=1&year<=210)

fig_regrowth <- ggplot() + geom_point(data=benchmark_regrowth_BCI,aes(x=bin_num,y=AGcwood_kgCm2_med),col="blue") + 
  geom_errorbar(data=benchmark_regrowth_BCI,aes(x=bin_num,y=AGcwood_kgCm2_med,
                                                ymin=AGcwood_kgCm2_10, ymax=AGcwood_kgCm2_90), width=.2, col="blue") + 
  geom_line(data=regrowth_out,aes(x = year, y = AGW),color="darkred",size=1) + 
  geom_hline(yintercept=10, col="grey") +
  labs(x = "Years after disturbance", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(0,210)) + scale_y_continuous(lim=c(0,22)) + 
  theme_classic() + ggtitle("Regrowth BCI") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_regrowth

## Biomass ####
# AGcwood (aboveground woody carbon) 
benchmark_biomass <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_eq_dynamics.csv")
benchmark_biomass_BCI <- benchmark_biomass %>% filter(site=="BCI") %>% mutate(time=Year-1565)

# Biomass plots together
biomass_out <- out_sc1$data[[1]]$output_annual_tile %>%
  slice(510+1:nrow(out_sc1$data[[1]]$output_annual_tile)) %>% 
  mutate(year = 1:450, AGW = (SapwoodC+WoodC)*0.75) %>%
  select(year, AGW) %>% filter(year>=415&year<=450)

fig_biomass <- ggplot() + geom_point(data=benchmark_biomass_BCI,aes(x=time,y=AGB_kgCm2),col="blue") + 
  geom_errorbar(data=benchmark_biomass_BCI,aes(x=time,y=AGB_kgCm2,
                                               ymin=AGB_lower_kgCm2, ymax=AGB_upper_kgCm2), width=.2, col="blue") + 
  geom_line(data=biomass_out,aes(x = year, y = AGW),color="darkred",size=1) + 
  #geom_hline(yintercept=10, col="grey") +
  labs(x = "Years", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(415,450)) + scale_y_continuous(lim=c(10,25)) + 
  theme_classic() + ggtitle("Biomass BCI") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_biomass

## Stand structure ####
stand_structure <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_stand_structure.csv")
stand_structure_BCI <- stand_structure %>% filter(site=="BCI")

### N stems size ####
stems_out <- out_sc1$data[[1]]$output_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>%
  group_by(dbh_bins) %>%
  summarise(nstem_size=mean(nstem_size)) %>% 
  mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90,100,150)) %>% filter(dbh_class_num>=5)

fig_stems <- ggplot() + geom_point(data=stand_structure_BCI,aes(x=dbh_classes_num,y=nstem_size_ha.1),col="blue") + 
  geom_errorbar(data=stand_structure_BCI,aes(x=dbh_classes_num,y=nstem_size_ha.1,
                                             ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point(data=stems_out,aes(x = dbh_class_num, y = nstem_size),color="darkred",size=2) + 
  labs(x = "DBH bins (cm)", y = expression(paste("Stems ", ha^-1, ") "))) + 
  #scale_x_continuous(lim=c(0,250)) + scale_y_continuous(lim=c(0,550)) + 
  theme_classic() + ggtitle("Stems size BCI") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_stems

### AGcwood size ####
cwood_out <- out_sc1$data[[1]]$output_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*0.75*density/10000)) %>%
  group_by(dbh_bins) %>%
  summarise(cwood_size=mean(cwood_size)) %>% 
  mutate(dbh_class_num = c(1,5,10,15,20,30,40,50,60,70,80,90,100,150)) %>% filter(dbh_class_num>=5)

fig_cwood <- ggplot() + geom_point(data=stand_structure_BCI,aes(x=dbh_classes_num,y=AGB_size_kgCm.2),col="blue") + 
  geom_errorbar(data=stand_structure_BCI,aes(x=dbh_classes_num,y=AGB_size_kgCm.2,
                                             ymin=AGB_size_lower_kgCm.2, ymax=AGB_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point(data=cwood_out,aes(x = dbh_class_num, y = cwood_size),color="darkred",size=2) + 
  labs(x = "DBH bins (cm)", y = expression(paste("AGcwood (kg C ", m^-2, ") "))) + 
  scale_x_continuous(lim=c(0,250)) + scale_y_continuous(lim=c(0,6.5)) + 
  theme_classic() + ggtitle("AGcwood BCI") + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fig_cwood

# All plots
fig_regrowth + fig_biomass + fig_stems + fig_cwood +
  plot_layout(ncol = 1) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

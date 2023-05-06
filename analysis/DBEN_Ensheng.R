# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4) 
library(lmerTest) 
library(effects) 
library(MuMIn)
library(sjPlot)
library(ggeffects)
library(patchwork)

# P0 Ensheng simulations ####
p0_FIN_co2A_out_annual_tile    <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_P0_FIN_aCO2_Ecosystem_yearly.csv")
p0_FIN_co2A_out_annual_cohorts <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_P0_FIN_aCO2_Cohort_yearly.csv")

p0_BIA_co2A_out_annual_tile    <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_BIA/BiomeE_P0_BIA_aCO2_Ecosystem_yearly.csv")
p0_BIA_co2A_out_annual_cohorts <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_BIA/BiomeE_P0_BIA_aCO2_Cohort_yearly.csv")

p0_BCI_co2A_out_annual_tile    <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_BCI/BiomeE_P0_BCI_aCO2_Ecosystem_yearly.csv")
p0_BCI_co2A_out_annual_cohorts <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_BCI/BiomeE_P0_BCI_aCO2_Cohort_yearly.csv")

#RColorBrewer::brewer.pal(8, "Set1")

# Plant C (Biomass) ####
p0_FIN_co2A_out_annual_tile %>% 
  ggplot() + 
  geom_line(aes(x=year, y=plantC),col="#377EB8") + 
  labs(x = "t", y = expression(paste("Plant C (kg C ", m^-2, ") "))) + 
  theme_classic() 

p0_BIA_co2A_out_annual_tile %>% 
  ggplot() + 
  geom_line(aes(x=year, y=plantC),col="#377EB8") + 
  labs(x = "t", y = expression(paste("Plant C (kg C ", m^-2, ") "))) + 
  theme_classic() 

p0_BCI_co2A_out_annual_tile %>% 
  ggplot() + 
  geom_line(aes(x=year, y=plantC),col="#377EB8") + 
  labs(x = "t", y = expression(paste("Plant C (kg C ", m^-2, ") "))) + 
  theme_classic() 

# Basal area ####
p0_FIN_co2A_out_annual_cohorts %>% #
  group_by(PFT,yr) %>%
  summarise(sumBA=sum(dbh*dbh*pi/4*Density)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = sumBA,col=PFT)) +
  labs(x = "t", y = expression(paste("Basal area (", m^-2, " ", ha^-1, ") "))) + 
  theme_classic()

p0_BIA_co2A_out_annual_cohorts %>% #
  group_by(PFT,yr) %>%
  summarise(sumBA=sum(dbh*dbh*pi/4*Density)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = sumBA,col=PFT)) +
  labs(x = "t", y = expression(paste("Basal area (", m^-2, " ", ha^-1, ") "))) + 
  theme_classic()

p0_BCI_co2A_out_annual_cohorts %>% #
  group_by(PFT,yr) %>%
  summarise(sumBA=sum(dbh*dbh*pi/4*Density)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = sumBA,col=PFT)) +
  labs(x = "t", y = expression(paste("Basal area (", m^-2, " ", ha^-1, ") "))) + 
  theme_classic()

# PS Ensheng simulations ####
pS1_FIN_co2A_out_annual_tile    <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_PS_FIN_aCO2_01_Ecosystem_yearly.csv")
pS1_FIN_co2A_out_annual_cohorts <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_PS_FIN_aCO2_01_Cohort_yearly.csv")
pS2_FIN_co2A_out_annual_tile    <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_PS_FIN_aCO2_02_Ecosystem_yearly.csv")
pS2_FIN_co2A_out_annual_cohorts <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_PS_FIN_aCO2_02_Cohort_yearly.csv")
pS3_FIN_co2A_out_annual_tile    <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_PS_FIN_aCO2_04_Ecosystem_yearly.csv")
pS3_FIN_co2A_out_annual_cohorts <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_PS_FIN_aCO2_04_Cohort_yearly.csv")
pS4_FIN_co2A_out_annual_tile    <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_PS_FIN_aCO2_08_Ecosystem_yearly.csv")
pS4_FIN_co2A_out_annual_cohorts <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_PS_FIN_aCO2_08_Cohort_yearly.csv")
pS5_FIN_co2A_out_annual_tile    <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_PS_FIN_aCO2_20_Ecosystem_yearly.csv")
pS5_FIN_co2A_out_annual_cohorts <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_PS_FIN_aCO2_20_Cohort_yearly.csv")
pS6_FIN_co2A_out_annual_tile    <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_PS_FIN_aCO2_40_Ecosystem_yearly.csv")
pS6_FIN_co2A_out_annual_cohorts <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_PS_FIN_aCO2_40_Cohort_yearly.csv")

# Crown area PS ####
figCA0 <- p0_FIN_co2A_out_annual_cohorts %>% 
  group_by(PFT,yr) %>%
  summarise(CrownArea=sum(Acrown*Density/10000)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = CrownArea,col=PFT)) +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3,1)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.title = element_blank()) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA0
figCA1 <- pS1_FIN_co2A_out_annual_cohorts %>% 
  group_by(PFT,yr) %>%
  summarise(CrownArea=sum(Acrown*Density/10000)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = CrownArea,col=PFT)) +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3,1)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.title = element_blank()) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA1
figCA2 <- pS2_FIN_co2A_out_annual_cohorts %>% 
  group_by(PFT,yr) %>%
  summarise(CrownArea=sum(Acrown*Density/10000)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = CrownArea,col=PFT)) +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3,1)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.title = element_blank()) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA2
figCA3 <- pS3_FIN_co2A_out_annual_cohorts %>% 
  group_by(PFT,yr) %>%
  summarise(CrownArea=sum(Acrown*Density/10000)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = CrownArea,col=PFT)) +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3,1)) +
  labs(x = "t", y = expression(paste("CA (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA3
figCA4 <- pS4_FIN_co2A_out_annual_cohorts %>% 
  group_by(PFT,yr) %>%
  summarise(CrownArea=sum(Acrown*Density/10000)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = CrownArea,col=PFT)) +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3,1)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.title = element_blank()) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA4
figCA5 <- pS5_FIN_co2A_out_annual_cohorts %>% 
  group_by(PFT,yr) %>%
  summarise(CrownArea=sum(Acrown*Density/10000)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = CrownArea,col=PFT)) +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3,1)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.title = element_blank()) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA5
figCA6 <- pS6_FIN_co2A_out_annual_cohorts %>% 
  group_by(PFT,yr) %>%
  summarise(CrownArea=sum(Acrown*Density/10000)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = CrownArea,col=PFT)) +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3,1)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.title = element_blank()) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA6

figCABiomeE <- figCA0  + figCA1  + figCA2  + figCA3 +
   figCA4  + figCA5  + figCA6  +
  plot_layout(ncol = 1) + 
  plot_annotation(title = 'BiomeE outputs (by Ensheng)') +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
figCABiomeE
ggsave("/home/laura/Documents/Collaborations/DBEN/Ensheng/BiomeE_FIN_CA.pdf", width = 5, height = 10, dpi=300)

# PS BiomeEP simulations ####
BiomeE_P0_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_FIN_aCO2_annual_tile.csv")
BiomeE_P0_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_FIN_aCO2_annual_cohorts.csv")
BiomeE_PS1_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS1_FIN_aCO2_annual_tile.csv")
BiomeE_PS1_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS1_FIN_aCO2_annual_cohorts.csv")
BiomeE_PS2_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS2_FIN_aCO2_annual_tile.csv")
BiomeE_PS2_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS2_FIN_aCO2_annual_cohorts.csv")
BiomeE_PS3_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS3_FIN_aCO2_annual_tile.csv")
BiomeE_PS3_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS3_FIN_aCO2_annual_cohorts.csv")
BiomeE_PS4_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS4_FIN_aCO2_annual_tile.csv")
BiomeE_PS4_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS4_FIN_aCO2_annual_cohorts.csv")
BiomeE_PS5_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS5_FIN_aCO2_annual_tile.csv")
BiomeE_PS5_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS5_FIN_aCO2_annual_cohorts.csv")
BiomeE_PS6_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS6_FIN_aCO2_annual_tile.csv")
BiomeE_PS6_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS6_FIN_aCO2_annual_cohorts.csv")

# Crown area PS ####
figCA0 <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = CA,col=PFT)) +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3,1)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.title = element_blank()) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA0

figCA1 <- BiomeE_PS1_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = CA,col=PFT)) +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3,1)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.title = element_blank()) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA1
figCA2 <- BiomeE_PS2_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = CA,col=PFT)) +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3,1)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.title = element_blank()) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA2
figCA3 <- BiomeE_PS3_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = CA,col=PFT)) +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3,1)) +
  labs(x = "t", y = expression(paste("CA (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA3
figCA4 <- BiomeE_PS4_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = CA,col=PFT)) +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3,1)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.title = element_blank()) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA4
figCA5 <- BiomeE_PS5_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = CA,col=PFT)) +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3,1)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.title = element_blank()) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA5
figCA6 <- BiomeE_PS6_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = CA,col=PFT)) +
  scale_y_continuous(limits=c(0,3), breaks = seq(0,3,1)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.title = element_blank()) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA6

figCABiomeEP <- figCA0  + figCA1  + figCA2  + figCA3 +
  figCA4  + figCA5  + figCA6  +
  plot_layout(ncol = 1) + 
  plot_annotation(title = 'BiomeEP outputs') +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
figCABiomeEP
ggsave("/home/laura/Documents/Collaborations/DBEN/Ensheng/BiomeEP_FIN_CA.pdf", width = 5, height = 10, dpi=300)

# All plots ####
# Plant C (Biomass) ####
fig1a <- p0_FIN_co2A_out_annual_tile %>% #filter(year>510) %>%
  ggplot() + 
  geom_line(aes(x=year, y=plantC),col="#377EB8") + 
  labs(x = "t", y = expression(paste("Plant C (kg C ", m^-2, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10)) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig1a

# GPP ####
fig1b <- p0_FIN_co2A_out_annual_tile %>% #filter(year>510) %>%
  ggplot() + 
  geom_line(aes(x=year, y=GPP),col="#377EB8") + 
  labs(x = "t", y = expression(paste("GPP (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10)) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig1b

# Rauto ####
fig1c <- p0_FIN_co2A_out_annual_tile %>% #filter(year>510) %>%
  ggplot() + 
  geom_line(aes(x=year, y=Rauto),col="#377EB8") + 
  labs(x = "t", y = expression(paste("Rauto (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10)) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig1c

# Soil C ####
fig1d <- p0_FIN_co2A_out_annual_tile %>% #filter(year>510) %>%
  ggplot() + 
  geom_line(aes(x=year, y=soilC),col="#377EB8") + 
  labs(x = "t", y =expression(paste("Soil C (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10)) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig1d

# Basal area ####
fig2a <- p0_FIN_co2A_out_annual_cohorts %>% #filter(year>510) %>%
  group_by(PFT,yr) %>%
  summarise(sumBA=sum(dbh*dbh*pi/4*Density)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = sumBA,col=PFT)) +
  #geom_smooth(aes(x=year, y=sumBA, color=PFT),se=F,size=.5) + 
  labs(x = "t", y = expression(paste("Basal area (", m^-2, " ", ha^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.85, .20),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_colour_discrete(labels = c("Grass","Needleleaf1","Needleleaf2","Broadleaf")) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig2a

# Crown area ####
fig2b <- p0_FIN_co2A_out_annual_cohorts %>% #filter(year>510) %>%
  group_by(PFT,yr) %>%
  summarise(CrownArea=sum(Acrown*Density/10000)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = CrownArea,col=PFT)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.85, .80),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_colour_discrete(labels = c("Grass","Needleleaf1","Needleleaf2","Broadleaf")) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig2b

# Leaf area ####
fig2c <- p0_FIN_co2A_out_annual_cohorts %>% #filter(year>510) %>%
  group_by(PFT,yr) %>%
  summarise(LeafArea=sum(Aleaf*Density/10000)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = LeafArea,col=PFT)) +
  labs(x = "t", y = expression(paste("Leaf area (", m^-2, " ", m^-2, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.85, .80),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_colour_discrete(labels = c("Grass","Needleleaf1","Needleleaf2","Broadleaf")) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig2c

# Biomass ####
fig2d <- p0_FIN_co2A_out_annual_cohorts %>% #filter(year>510) %>%
  group_by(PFT,yr) %>%
  summarise(biomass=sum((bl*Density + br*Density + bSW*Density + bHW*Density
                         + seed*Density + nsc*Density)/10000)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = biomass ,col=PFT)) +
  labs(x = "t", y = expression(paste("Biomass (Kg C ", m^-2, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.85, .20),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_colour_discrete(labels = c("Grass","Needleleaf1","Needleleaf2","Broadleaf")) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig2d






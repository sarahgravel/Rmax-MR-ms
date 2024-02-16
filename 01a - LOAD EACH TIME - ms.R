# Sarah Gravel
# 01 Load data & packages

#---- Packages ----

library(here)
library(plyr) # for ddply
library(tidyverse)
library(data.table)
library(forcats)
library(wrappedtools)
library(kit) 
library(Rfast)
library(FSA) 
library(FSAdata)
library(nlstools)
library(plotrix)
library(r2symbols)
library(scales)
library(fishtree)
library(phytools)
library(ape)
library(stringr)
library(brms)
library(geiger)
library(nlme)
library(caper)
library(MuMIn)
library(car)
library(regclass)
library(bbmle)
library(lattice)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(MASS)
library(ggbreak)
library(scales)
library(cowplot)
library(grid)
library(gridExtra)
library(ggridges)
library(bayesplot)
library(RColorBrewer)
library(usethis)

# ---- Constants ----
kb <- 8.617333262*10^-5

# ---- Final data sets ----
# Full data set for the 84 species (output from 00g)
# rmax, LHT, MR (with RMR, MMR, and AS), and ecological lifestyle
# Each row has unique combination of MRtype (RMR, MMR or AS) and DFtype ('BigN', 'MeanT', or 'Both')
DF_FULL <- read.csv("data/FULL_Rmax_LHT_MR_ms.csv", header=TRUE, fileEncoding = 'UTF-8') %>% as.data.frame() 
# Filter by MRtype (RMR, MMR, or AS) and DFtype (sample size or temperature MR dataset)
# to obtain data sets for RMR, MMR, and AS models.
# Note: Vars are transformed and/or standardized in the next script (01b)
# RMR: 
# Biggest sample size N:
DF_rmaxRMRbigN <- DF_FULL %>%
  filter(MRtype == 'RMR' & (DFtype == 'BigN' | DFtype == 'Both')) %>%
  dplyr::select(-c("MRtype", "DFtype"))
DF_rmaxRMRbigN$Lifestyle <- as.factor(DF_rmaxRMRbigN$Lifestyle) # lifestyle as factor
DF_rmaxRMRbigN$TeleostOrElasmo <- as.factor(DF_rmaxRMRbigN$TeleostOrElasmo) # teleost vs. shark factor
DF_rmaxRMRbigN$phylo <- str_replace_all(DF_rmaxRMRbigN$ScientificName, " ", "_") # add phylo col
# Closest to mean temperature 15C:
DF_rmaxRMRmeanT <- DF_FULL %>%
  filter(MRtype == 'RMR' & (DFtype == 'MeanT' | DFtype == 'Both')) %>%
  dplyr::select(-c("MRtype", "DFtype"))
DF_rmaxRMRmeanT$Lifestyle <- as.factor(DF_rmaxRMRmeanT$Lifestyle)
DF_rmaxRMRmeanT$TeleostOrElasmo <- as.factor(DF_rmaxRMRmeanT$TeleostOrElasmo)
DF_rmaxRMRmeanT$phylo <- str_replace_all(DF_rmaxRMRmeanT$ScientificName, " ", "_")
# MMR: 
# Biggest sample size N:
DF_rmaxMMRbigN <- DF_FULL %>%
  filter(MRtype == 'MMR' & (DFtype == 'BigN' | DFtype == 'Both')) %>%
  dplyr::select(-c("MRtype", "DFtype"))
DF_rmaxMMRbigN$Lifestyle <- as.factor(DF_rmaxMMRbigN$Lifestyle)
DF_rmaxMMRbigN$TeleostOrElasmo <- as.factor(DF_rmaxMMRbigN$TeleostOrElasmo)
DF_rmaxMMRbigN$phylo <- str_replace_all(DF_rmaxMMRbigN$ScientificName, " ", "_") 
# Closest to mean temperature 15C:
DF_rmaxMMRmeanT <- DF_FULL %>%
  filter(MRtype == 'MMR' & (DFtype == 'MeanT' | DFtype == 'Both')) %>%
  dplyr::select(-c("MRtype", "DFtype"))
DF_rmaxMMRmeanT$Lifestyle <- as.factor(DF_rmaxMMRmeanT$Lifestyle)
DF_rmaxMMRmeanT$TeleostOrElasmo <- as.factor(DF_rmaxMMRmeanT$TeleostOrElasmo)
DF_rmaxMMRmeanT$phylo <- str_replace_all(DF_rmaxMMRmeanT$ScientificName, " ", "_") 
# AS: 
# Biggest sample size N:
DF_rmaxASbigN <- DF_FULL %>%
  filter(MRtype == 'AS' & (DFtype == 'BigN' | DFtype == 'Both')) %>%
  dplyr::select(-c("MRtype", "DFtype"))
DF_rmaxASbigN$Lifestyle <- as.factor(DF_rmaxASbigN$Lifestyle)
DF_rmaxASbigN$TeleostOrElasmo <- as.factor(DF_rmaxASbigN$TeleostOrElasmo)
DF_rmaxASbigN$phylo <- str_replace_all(DF_rmaxASbigN$ScientificName, " ", "_")
# Closest to mean temperature 15C:
DF_rmaxASmeanT <- DF_FULL %>%
  filter(MRtype == 'AS' & (DFtype == 'MeanT' | DFtype == 'Both')) %>%
  dplyr::select(-c("MRtype", "DFtype"))
DF_rmaxASmeanT$Lifestyle <- as.factor(DF_rmaxASmeanT$Lifestyle)
DF_rmaxASmeanT$TeleostOrElasmo <- as.factor(DF_rmaxASmeanT$TeleostOrElasmo) 
DF_rmaxASmeanT$phylo <- str_replace_all(DF_rmaxASmeanT$ScientificName, " ", "_") 

# ---- Phylogenetic trees ----
# All pruned from the same supertree
# Phylo for RMR models:
rmaxRMR_phy <- ape::read.tree("data/phylogenies/pruned_fish_tree_rmaxRMR_ms.tre")
# Phylo for MMR models:
rmaxMMR_phy <- ape::read.tree("data/phylogenies/pruned_fish_tree_rmaxMMR_ms.tre")
# Phylo for AS models:
rmaxAS_phy <- ape::read.tree("data/phylogenies/pruned_fish_tree_rmaxAS_ms.tre")

# ---- Correlation matrices for brms ----
# Corr matrix for RMR models:
rmaxRMR_corrma <- ape::vcv(rmaxRMR_phy, corr = TRUE)
# Corr matrix for MMR models:
rmaxMMR_corrma <- ape::vcv(rmaxMMR_phy, corr = TRUE)
# Corr matrix for AS models:
rmaxAS_corrma <- ape::vcv(rmaxAS_phy, corr = TRUE)

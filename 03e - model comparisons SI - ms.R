# Sarah Gravel
# 03 Supplementary analyses - model comparisons 

# LOOIC and elpd

# SUPPLEMENTARY ANALYSES ----

# Ecological lifestyle ----
# comparing the models in Q1 and Q2 with ecological lifestyle to models without

# Models WITH ecological lifestyle (same models as presented in main ms, Tables 1 & 2):
# RMR
RMR_loo1 <- add_criterion(brms_rmaxRMRbigN_mtLs, "loo")
RMR_loo1_loo <- brms::loo(RMR_loo1)
RMR_loo2 <- add_criterion(brms_rmaxRMRbigN_mtROLs, "loo")
RMR_loo2_loo <- brms::loo(RMR_loo2)
RMR_loo3 <- add_criterion(brms_rmaxRMRbigN_mtAmatLs, "loo")
RMR_loo3_loo <- brms::loo(RMR_loo3)
RMR_loo4 <- add_criterion(brms_rmaxRMRbigN_mtAmaxLs, "loo")
RMR_loo4_loo <- brms::loo(RMR_loo4)
RMR_loo5 <- add_criterion(brms_rmaxRMRbigN_mtRmaxLs, "loo")
RMR_loo5_loo <- brms::loo(RMR_loo5)
# MMR
MMR_loo1 <- add_criterion(brms_rmaxMMRbigN_mtLs, "loo")
MMR_loo1_loo <- brms::loo(MMR_loo1)
MMR_loo2 <- add_criterion(brms_rmaxMMRbigN_mtROLs, "loo")
MMR_loo2_loo <- brms::loo(MMR_loo2)
MMR_loo3 <- add_criterion(brms_rmaxMMRbigN_mtAmatLs, "loo")
MMR_loo3_loo <- brms::loo(MMR_loo3)
MMR_loo4 <- add_criterion(brms_rmaxMMRbigN_mtAmaxLs, "loo")
MMR_loo4_loo <- brms::loo(MMR_loo4)
MMR_loo5 <- add_criterion(brms_rmaxMMRbigN_mtRmaxLs, "loo")
MMR_loo5_loo <- brms::loo(MMR_loo5)
# AS
AAS_loo1 <- add_criterion(brms_rmaxAASbigN_mtLs, "loo")
AAS_loo1_loo <- brms::loo(AAS_loo1)
AAS_loo2 <- add_criterion(brms_rmaxAASbigN_mtROLs, "loo")
AAS_loo2_loo <- brms::loo(AAS_loo2)
AAS_loo3 <- add_criterion(brms_rmaxAASbigN_mtAmatLs, "loo")
AAS_loo3_loo <- brms::loo(AAS_loo3)
AAS_loo4 <- add_criterion(brms_rmaxAASbigN_mtAmaxLs, "loo")
AAS_loo4_loo <- brms::loo(AAS_loo4)
AAS_loo5 <- add_criterion(brms_rmaxAASbigN_mtRmaxLs, "loo")
AAS_loo5_loo <- brms::loo(AAS_loo5)
# WITHOUT ecological lifestyle:
# RMR
RMR_loo6 <- add_criterion(brms_rmaxRMRbigN_mt, "loo")
RMR_loo6_loo <- brms::loo(RMR_loo6)
RMR_loo7 <- add_criterion(brms_rmaxRMRbigN_mtRO, "loo")
RMR_loo7_loo <- brms::loo(RMR_loo7)
RMR_loo8 <- add_criterion(brms_rmaxRMRbigN_mtAmat, "loo")
RMR_loo8_loo <- brms::loo(RMR_loo8)
RMR_loo9 <- add_criterion(brms_rmaxRMRbigN_mtAmax, "loo")
RMR_loo9_loo <- brms::loo(RMR_loo9)
RMR_loo10 <- add_criterion(brms_rmaxRMRbigN_mtRmax, "loo")
RMR_loo10_loo <- brms::loo(RMR_loo10)
# MMR
MMR_loo6 <- add_criterion(brms_rmaxMMRbigN_mt, "loo")
MMR_loo6_loo <- brms::loo(MMR_loo6)
MMR_loo7 <- add_criterion(brms_rmaxMMRbigN_mtRO, "loo")
MMR_loo7_loo <- brms::loo(MMR_loo7)
MMR_loo8 <- add_criterion(brms_rmaxMMRbigN_mtAmat, "loo")
MMR_loo8_loo <- brms::loo(MMR_loo8)
MMR_loo9 <- add_criterion(brms_rmaxMMRbigN_mtAmax, "loo")
MMR_loo9_loo <- brms::loo(MMR_loo9)
MMR_loo10 <- add_criterion(brms_rmaxMMRbigN_mtRmax, "loo")
MMR_loo10_loo <- brms::loo(MMR_loo10)
# AS
AAS_loo6 <- add_criterion(brms_rmaxAASbigN_mt, "loo")
AAS_loo6_loo <- brms::loo(AAS_loo6)
AAS_loo7 <- add_criterion(brms_rmaxAASbigN_mtRO, "loo")
AAS_loo7_loo <- brms::loo(AAS_loo7)
AAS_loo8 <- add_criterion(brms_rmaxAASbigN_mtAmat, "loo")
AAS_loo8_loo <- brms::loo(AAS_loo8)
AAS_loo9 <- add_criterion(brms_rmaxAASbigN_mtAmax, "loo")
AAS_loo9_loo <- brms::loo(AAS_loo9)
AAS_loo10 <- add_criterion(brms_rmaxAASbigN_mtRmax, "loo")
AAS_loo10_loo <- brms::loo(AAS_loo10)

# Compare models without ecological lifestyle to models with
# RMR
RMR_loo_list <- list(RMR_loo1_loo, RMR_loo2_loo, RMR_loo3_loo, RMR_loo4_loo, RMR_loo5_loo, RMR_loo6_loo, RMR_loo7_loo, RMR_loo8_loo, RMR_loo9_loo, RMR_loo10_loo)
loo_model_weights(RMR_loo_list) # model weights
loo_compare(RMR_loo_list) # elpd_diff
brms::loo(RMR_loo1) # elpd_loo, p_loo, and looic
brms::loo(RMR_loo2)
brms::loo(RMR_loo3)
brms::loo(RMR_loo4)
brms::loo(RMR_loo5)
brms::loo(RMR_loo6)
brms::loo(RMR_loo7)
brms::loo(RMR_loo8)
brms::loo(RMR_loo9)
brms::loo(RMR_loo10)
# MMR
MMR_loo_list <- list(MMR_loo1_loo, MMR_loo2_loo, MMR_loo3_loo, MMR_loo4_loo, MMR_loo5_loo, MMR_loo6_loo, MMR_loo7_loo, MMR_loo8_loo, MMR_loo9_loo, MMR_loo10_loo)
loo_model_weights(MMR_loo_list)
loo_compare(MMR_loo_list)
brms::loo(MMR_loo1)
brms::loo(MMR_loo2)
brms::loo(MMR_loo3)
brms::loo(MMR_loo4)
brms::loo(MMR_loo5)
brms::loo(MMR_loo6) 
brms::loo(MMR_loo7)
brms::loo(MMR_loo8)
brms::loo(MMR_loo9)
brms::loo(MMR_loo10) 
# AS
AAS_loo_list <- list(AAS_loo1_loo, AAS_loo2_loo, AAS_loo3_loo, AAS_loo4_loo, AAS_loo5_loo, AAS_loo6_loo, AAS_loo7_loo, AAS_loo8_loo, AAS_loo9_loo, AAS_loo10_loo) 
loo_model_weights(AAS_loo_list)
loo_compare(AAS_loo_list)
brms::loo(AAS_loo1)
brms::loo(AAS_loo2)
brms::loo(AAS_loo3)
brms::loo(AAS_loo4)
brms::loo(AAS_loo5)
brms::loo(AAS_loo6) 
brms::loo(AAS_loo7)
brms::loo(AAS_loo8)
brms::loo(AAS_loo9)
brms::loo(AAS_loo10) 


# Based on MR temperature dataset ----
# models from Q1 and Q2 but based on temperature dataset (where metabolic rate was selected based on measurement temperature closest to 15C)

# Q1) Do the life history trait components of rmax (age-at-maturity, maximum age, reproductive output) explain variation in metabolic rates? 

# RMR
RMR_loo11 <- add_criterion(brms_rmaxRMRmeanT_mtLs, "loo")
RMR_loo11_loo <- brms::loo(RMR_loo11)
RMR_loo12 <- add_criterion(brms_rmaxRMRmeanT_mtROLs, "loo")
RMR_loo12_loo <- brms::loo(RMR_loo12)
RMR_loo13 <- add_criterion(brms_rmaxRMRmeanT_mtAmatLs, "loo")
RMR_loo13_loo <- brms::loo(RMR_loo13)
RMR_loo14 <- add_criterion(brms_rmaxRMRmeanT_mtAmaxLs, "loo")
RMR_loo14_loo <- brms::loo(RMR_loo14)
# MMR
MMR_loo11 <- add_criterion(brms_rmaxMMRmeanT_mtLs, "loo")
MMR_loo11_loo <- brms::loo(MMR_loo11)
MMR_loo12 <- add_criterion(brms_rmaxMMRmeanT_mtROLs, "loo")
MMR_loo12_loo <- brms::loo(MMR_loo12)
MMR_loo13 <- add_criterion(brms_rmaxMMRmeanT_mtAmatLs, "loo")
MMR_loo13_loo <- brms::loo(MMR_loo13)
MMR_loo14 <- add_criterion(brms_rmaxMMRmeanT_mtAmaxLs, "loo")
MMR_loo14_loo <- brms::loo(MMR_loo14)
# AS
AAS_loo11 <- add_criterion(brms_rmaxAASmeanT_mtLs, "loo")
AAS_loo11_loo <- brms::loo(AAS_loo11)
AAS_loo12 <- add_criterion(brms_rmaxAASmeanT_mtROLs, "loo")
AAS_loo12_loo <- brms::loo(AAS_loo12)
AAS_loo13 <- add_criterion(brms_rmaxAASmeanT_mtAmatLs, "loo")
AAS_loo13_loo <- brms::loo(AAS_loo13)
AAS_loo14 <- add_criterion(brms_rmaxAASmeanT_mtAmaxLs, "loo")
AAS_loo14_loo <- brms::loo(AAS_loo14)

# Compare models
# RMR
RMR_loo_list <- list(RMR_loo11_loo, RMR_loo12_loo, RMR_loo13_loo, RMR_loo14_loo)
loo_model_weights(RMR_loo_list) # model weights
loo_compare(RMR_loo_list) # elpd_diff
brms::loo(RMR_loo11) # elpd_loo, p_loo, and looic
brms::loo(RMR_loo12)
brms::loo(RMR_loo13)
brms::loo(RMR_loo14)
# MMR
MMR_loo_list <- list(MMR_loo11_loo, MMR_loo12_loo, MMR_loo13_loo, MMR_loo14_loo)
loo_model_weights(MMR_loo_list)
loo_compare(MMR_loo_list)
brms::loo(MMR_loo11) 
brms::loo(MMR_loo12)
brms::loo(MMR_loo13)
brms::loo(MMR_loo14)
# AS
AAS_loo_list <- list(AAS_loo11_loo, AAS_loo12_loo, AAS_loo13_loo, AAS_loo14_loo)
loo_model_weights(AAS_loo_list)
loo_compare(AAS_loo_list)
brms::loo(AAS_loo11) 
brms::loo(AAS_loo12)
brms::loo(AAS_loo13)
brms::loo(AAS_loo14)

# Q2) Does rmax explain variation in metabolic rates? 

# RMR
RMR_loo15 <- add_criterion(brms_rmaxRMRmeanT_mtRmaxLs, "loo")
RMR_loo15_loo <- brms::loo(RMR_loo15)
# MMR
MMR_loo15 <- add_criterion(brms_rmaxMMRmeanT_mtRmaxLs, "loo")
MMR_loo15_loo <- brms::loo(MMR_loo15)
# AS
AAS_loo15 <- add_criterion(brms_rmaxAASmeanT_mtRmaxLs, "loo")
AAS_loo15_loo <- brms::loo(AAS_loo15) 

# Compare models
# RMR
RMR_loo_list <- list(RMR_loo13_loo, RMR_loo15_loo)
loo_model_weights(RMR_loo_list) # model weights
loo_compare(RMR_loo_list) # elpd_diff
brms::loo(RMR_loo13) # elpd_loo, p_loo, and looic
brms::loo(RMR_loo15)
# MMR
MMR_loo_list <- list(MMR_loo11_loo, MMR_loo15_loo)
loo_model_weights(MMR_loo_list)
loo_compare(MMR_loo_list)
brms::loo(MMR_loo11)
brms::loo(MMR_loo15)
# AS
AAS_loo_list <- list(AAS_loo11_loo, AAS_loo13_loo, AAS_loo15_loo)
loo_model_weights(AAS_loo_list)
loo_compare(AAS_loo_list)
brms::loo(AAS_loo11)
brms::loo(AAS_loo13)
brms::loo(AAS_loo15)


# Partial endotherm correction factor ----
# Models from Q1 and Q2 but based on data set without the regionally endothermic fishes

# RMR
RMR_loo16 <- add_criterion(brms_rmaxRMRbigN_mtLs_noEndos, "loo")
RMR_loo16_loo <- brms::loo(RMR_loo16) 
RMR_loo17 <- add_criterion(brms_rmaxRMRbigN_mtAmatLs_noEndos, "loo")
RMR_loo17_loo <- brms::loo(RMR_loo17) 
RMR_loo18 <- add_criterion(brms_rmaxRMRbigN_mtRmaxLs_noEndos, "loo")
RMR_loo18_loo <- brms::loo(RMR_loo18) 
# MMR
MMR_loo16 <- add_criterion(brms_rmaxMMRbigN_mtLs_noEndos, "loo")
MMR_loo16_loo <- brms::loo(MMR_loo16)
MMR_loo17 <- add_criterion(brms_rmaxMMRbigN_mtAmatLs_noEndos, "loo")
MMR_loo17_loo <- brms::loo(MMR_loo17) 
MMR_loo18 <- add_criterion(brms_rmaxMMRbigN_mtRmaxLs_noEndos, "loo")
MMR_loo18_loo <- brms::loo(MMR_loo18) 
# AS
AAS_loo16 <- add_criterion(brms_rmaxAASbigN_mtLs_noEndos, "loo")
AAS_loo16_loo <- brms::loo(AAS_loo16) 
AAS_loo17 <- add_criterion(brms_rmaxAASbigN_mtAmatLs_noEndos, "loo")
AAS_loo17_loo <- brms::loo(AAS_loo17)
AAS_loo18 <- add_criterion(brms_rmaxAASbigN_mtRmaxLs_noEndos, "loo")
AAS_loo18_loo <- brms::loo(AAS_loo18)

# Model elpd_loo, p_loo, and looic
# RMR
brms::loo(RMR_loo16) 
brms::loo(RMR_loo17) 
brms::loo(RMR_loo18) 
# MMR
brms::loo(MMR_loo16) 
brms::loo(MMR_loo17) 
brms::loo(MMR_loo18) 
# AAS
brms::loo(AAS_loo16) 
brms::loo(AAS_loo17) 
brms::loo(AAS_loo18) 

# Do teleosts have higher MR than sharks ----
# Compare top model(s) from Q1 & Q2 to a model with fixed factor for taxonomic group (teleost or shark)

# RMR
RMR_loo19 <- add_criterion(brms_rmaxRMRbigN_mtLs_taxon, "loo")
RMR_loo19_loo <- brms::loo(RMR_loo19) 
RMR_loo20 <- add_criterion(brms_rmaxRMRbigN_mtAmatLs_taxon, "loo")
RMR_loo20_loo <- brms::loo(RMR_loo20) 
RMR_loo21 <- add_criterion(brms_rmaxRMRbigN_mtRmaxLs_taxon, "loo")
RMR_loo21_loo <- brms::loo(RMR_loo21) 
# MMR
MMR_loo19 <- add_criterion(brms_rmaxMMRbigN_mtLs_taxon, "loo")
MMR_loo19_loo <- brms::loo(MMR_loo19) 
MMR_loo20 <- add_criterion(brms_rmaxMMRbigN_mtRmaxLs_taxon, "loo")
MMR_loo20_loo <- brms::loo(MMR_loo20) 
# AS
AAS_loo19 <- add_criterion(brms_rmaxAASbigN_mtLs_taxon, "loo")
AAS_loo19_loo <- brms::loo(AAS_loo19) 
AAS_loo20 <- add_criterion(brms_rmaxAASbigN_mtAmatLs_taxon, "loo")
AAS_loo20_loo <- brms::loo(AAS_loo20)
AAS_loo21 <- add_criterion(brms_rmaxAASbigN_mtRmaxLs_taxon, "loo")
AAS_loo21_loo <- brms::loo(AAS_loo21)

# Compare models without taxon factor to models with
# RMR
RMR_loo_list <- list(RMR_loo1_loo, RMR_loo3_loo, RMR_loo5_loo, RMR_loo19_loo, RMR_loo20_loo, RMR_loo21_loo)
loo_model_weights(RMR_loo_list) # model weights
loo_compare(RMR_loo_list) # elpd_diff
brms::loo(RMR_loo1) # elpd_loo, p_loo, and LOOIC
brms::loo(RMR_loo3) 
brms::loo(RMR_loo5) 
brms::loo(RMR_loo19) 
brms::loo(RMR_loo20) 
brms::loo(RMR_loo21) 
# amat model best (no taxon)

# MMR
MMR_loo_list <- list(MMR_loo1_loo, MMR_loo5_loo, MMR_loo19_loo, MMR_loo20_loo)
loo_model_weights(MMR_loo_list)
loo_compare(MMR_loo_list) 
brms::loo(MMR_loo1)
brms::loo(MMR_loo5) 
brms::loo(MMR_loo19) 
brms::loo(MMR_loo20) 
# rmax+taxon model scored highest, but most parsimonious model is rmax model

# AS
AAS_loo_list <- list(AAS_loo1_loo, AAS_loo3_loo, AAS_loo5_loo, AAS_loo19_loo, AAS_loo20_loo, AAS_loo21_loo)
loo_model_weights(AAS_loo_list)
loo_compare(AAS_loo_list) 
brms::loo(AAS_loo1) 
brms::loo(AAS_loo3) 
brms::loo(AAS_loo5) 
brms::loo(AAS_loo19) 
brms::loo(AAS_loo20) 
brms::loo(AAS_loo21) 


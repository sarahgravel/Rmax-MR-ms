# Sarah Gravel
# 03 Main analyses - model comparisons 

# LOOIC and elpd

# Table 1: Q1) Do the life history trait components of rmax (age-at-maturity, maximum age, reproductive output) explain variation in metabolic rates? ----

# RMR
RMR_loo1 <- add_criterion(brms_rmaxRMRbigN_mtLs, "loo")
RMR_loo1_loo <- brms::loo(RMR_loo1)
RMR_loo2 <- add_criterion(brms_rmaxRMRbigN_mtROLs, "loo")
RMR_loo2_loo <- brms::loo(RMR_loo2)
RMR_loo3 <- add_criterion(brms_rmaxRMRbigN_mtAmatLs, "loo")
RMR_loo3_loo <- brms::loo(RMR_loo3)
RMR_loo4 <- add_criterion(brms_rmaxRMRbigN_mtAmaxLs, "loo")
RMR_loo4_loo <- brms::loo(RMR_loo4)
# MMR
MMR_loo1 <- add_criterion(brms_rmaxMMRbigN_mtLs, "loo")
MMR_loo1_loo <- brms::loo(MMR_loo1)
MMR_loo2 <- add_criterion(brms_rmaxMMRbigN_mtROLs, "loo")
MMR_loo2_loo <- brms::loo(MMR_loo2)
MMR_loo3 <- add_criterion(brms_rmaxMMRbigN_mtAmatLs, "loo")
MMR_loo3_loo <- brms::loo(MMR_loo3)
MMR_loo4 <- add_criterion(brms_rmaxMMRbigN_mtAmaxLs, "loo")
MMR_loo4_loo <- brms::loo(MMR_loo4)
# AS
AAS_loo1 <- add_criterion(brms_rmaxAASbigN_mtLs, "loo")
AAS_loo1_loo <- brms::loo(AAS_loo1)
AAS_loo2 <- add_criterion(brms_rmaxAASbigN_mtROLs, "loo")
AAS_loo2_loo <- brms::loo(AAS_loo2)
AAS_loo3 <- add_criterion(brms_rmaxAASbigN_mtAmatLs, "loo")
AAS_loo3_loo <- brms::loo(AAS_loo3)
AAS_loo4 <- add_criterion(brms_rmaxAASbigN_mtAmaxLs, "loo")
AAS_loo4_loo <- brms::loo(AAS_loo4)
 
# Compare mods
# RMR
RMR_loo_list <- list(RMR_loo1_loo, RMR_loo2_loo, RMR_loo3_loo, RMR_loo4_loo)
loo_model_weights(RMR_loo_list) # model weights
loo_compare(RMR_loo_list) # elpd_diff
brms::loo(RMR_loo1) # elpd_loo, p_loo, and looic
brms::loo(RMR_loo2)
brms::loo(RMR_loo3)
brms::loo(RMR_loo4)
# MMR
MMR_loo_list <- list(MMR_loo1_loo, MMR_loo2_loo, MMR_loo3_loo, MMR_loo4_loo)
loo_model_weights(MMR_loo_list)
loo_compare(MMR_loo_list)
brms::loo(MMR_loo1) 
brms::loo(MMR_loo2)
brms::loo(MMR_loo3)
brms::loo(MMR_loo4)
# AS
AAS_loo_list <- list(AAS_loo1_loo, AAS_loo2_loo, AAS_loo3_loo, AAS_loo4_loo)
loo_model_weights(AAS_loo_list)
loo_compare(AAS_loo_list)
brms::loo(AAS_loo1) 
brms::loo(AAS_loo2)
brms::loo(AAS_loo3)
brms::loo(AAS_loo4)

# Table 2: Q2) Does rmax explain variation in metabolic rates? ----

# Compare top model(s) from Q1 to a model with rmax.

# RMR
RMR_loo5 <- add_criterion(brms_rmaxRMRbigN_mtRmaxLs, "loo")
RMR_loo5_loo <- brms::loo(RMR_loo5)
# MMR
MMR_loo5 <- add_criterion(brms_rmaxMMRbigN_mtRmaxLs, "loo")
MMR_loo5_loo <- brms::loo(MMR_loo5)
# AS
AAS_loo5 <- add_criterion(brms_rmaxAASbigN_mtRmaxLs, "loo")
AAS_loo5_loo <- brms::loo(AAS_loo5) 

# Compare mods
# RMR
RMR_loo_list <- list(RMR_loo3_loo, RMR_loo5_loo)
loo_model_weights(RMR_loo_list) # model weights
loo_compare(RMR_loo_list) # elpd_diff
brms::loo(RMR_loo5) # elpd_loo, p_loo, and looic
# MMR
MMR_loo_list <- list(MMR_loo1_loo, MMR_loo5_loo)
loo_model_weights(MMR_loo_list)
loo_compare(MMR_loo_list)
brms::loo(MMR_loo5)
# AS
AAS_loo_list <- list(AAS_loo1_loo, AAS_loo3_loo, AAS_loo5_loo)
loo_model_weights(AAS_loo_list)
loo_compare(AAS_loo_list)
brms::loo(AAS_loo5)

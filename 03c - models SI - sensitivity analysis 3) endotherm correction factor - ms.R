# Sarah Gravel
# 03 Supplementary analyses - models

# Partial endotherm temperature correction factor
# Table S6

# Top three models from Q1 and Q2 (null, age-at-maturity, and rmax), in Tables 1 & 2, but using a subset of data excluding the partially endothermic fishes (i.e., lamnid sharks and tunas).

# Explanation: We used a correction factor of 3.5C for all partial endotherms in our dataset (following a study in sharks from Pardo & Dulvy, 2022), so we verify the sensitivity of our results to the inclusion of these species.

# Remove tuna species from data and trees:

# From DFs:
# RMR
which(transDF_rmaxRMRbigN$ScientificName == "Isurus oxyrinchus") # 28
which(transDF_rmaxRMRbigN$ScientificName == "Katsuwonus pelamis") # 29
which(grepl("Thunnus", transDF_rmaxRMRbigN$ScientificName)) # 74,75,76
transDF_rmaxRMRbigN_noEndos <- transDF_rmaxRMRbigN[-c(28, 29, 74, 75, 76), ]
transDF_rmaxRMRbigN_noEndos <- transDF_rmaxRMRbigN_noEndos %>%
  mutate(stdInvMRTemp = scale(invMRTemp),
         stdLogMRMass = scale(logMRMass),
         stdLogAmat = scale(logAmat),
         stdLogRmax = scale(logRmax))
# MMR
which(transDF_rmaxMMRbigN$ScientificName == "Isurus oxyrinchus") # 17
which(transDF_rmaxMMRbigN$ScientificName == "Katsuwonus pelamis") # 18
which(grepl("Thunnus", transDF_rmaxMMRbigN$ScientificName)) # 47
transDF_rmaxMMRbigN_noEndos <- transDF_rmaxMMRbigN[-c(17, 18, 47),]
transDF_rmaxMMRbigN_noEndos <- transDF_rmaxMMRbigN_noEndos %>%
  mutate(stdInvMRTemp = scale(invMRTemp),
         stdLogMRMass = scale(logMRMass),
         stdLogAmat = scale(logAmat),
         stdLogRmax = scale(logRmax))
# AS
which(transDF_rmaxASbigN$ScientificName == "Isurus oxyrinchus") # 15
which(transDF_rmaxASbigN$ScientificName == "Katsuwonus pelamis") # 16
which(grepl("Thunnus", transDF_rmaxASbigN$ScientificName)) # 43
transDF_rmaxASbigN_noEndos <- transDF_rmaxASbigN[-c(15, 16, 43),]
transDF_rmaxASbigN_noEndos <- transDF_rmaxASbigN_noEndos %>%
  mutate(stdInvMRTemp = scale(invMRTemp),
         stdLogMRMass = scale(logMRMass),
         stdLogAmat = scale(logAmat),
         stdLogRmax = scale(logRmax))

# From trees:
# RMR
RMRtreeSpp <- rmaxRMR_phy$tip.label 
SppToPrune <- setdiff(RMRtreeSpp, transDF_rmaxRMRbigN_noEndos$phylo) 
rmaxRMR_phy_noEndos <- drop.tip(rmaxRMR_phy, SppToPrune) 
rmaxRMR_phy_noEndos$tip.label
rmaxRMR_corrma_noEndos <- ape::vcv(rmaxRMR_phy_noEndos, corr = TRUE)
# MMR
MMRtreeSpp <- rmaxMMR_phy$tip.label # All labels in tree
SppToPrune <- setdiff(MMRtreeSpp, transDF_rmaxMMRbigN_noEndos$phylo) # Those to drop
rmaxMMR_phy_noEndos <- drop.tip(rmaxMMR_phy, SppToPrune) # Prune tree
rmaxMMR_phy_noEndos$tip.label
rmaxMMR_corrma_noEndos <- ape::vcv(rmaxMMR_phy_noEndos, corr = TRUE)
# AS
AStreeSpp <- rmaxAS_phy$tip.label # All labels in tree
SppToPrune <- setdiff(AStreeSpp, transDF_rmaxASbigN_noEndos$phylo) # Those to drop
rmaxAS_phy_noEndos <- drop.tip(rmaxAS_phy, SppToPrune) # Prune tree
rmaxAS_phy_noEndos$tip.label
rmaxAS_corrma_noEndos <- ape::vcv(rmaxAS_phy_noEndos, corr = TRUE)

# check make sure all species in datasets are in corresponding trees:
setdiff(transDF_rmaxRMRbigN_noEndos$phylo, rmaxRMR_phy_noEndos$tip.label)
setdiff(transDF_rmaxMMRbigN_noEndos$phylo, rmaxMMR_phy_noEndos$tip.label)
setdiff(transDF_rmaxASbigN_noEndos$phylo, rmaxAS_phy_noEndos$tip.label)

# MODELS ON STANDARDIZED VARIABLES ----
# Table 9

# RMR ~ meas mass + meas temp + lifestyle ----
brms_rmaxRMRbigN_mtLs_noEndos <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxRMRbigN_noEndos, 
  family = gaussian(),
  control = list(adapt_delta = 0.99),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxRMR_corrma_noEndos),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)
# summary(brms_rmaxRMRbigN_mtLs_noEndos)
# save(brms_rmaxRMRbigN_mtLs_noEndos, file="models/20230314_noEndos/brms_rmaxRMRbigN_mtLs_noEndos.rds")

# RMR ~ meas mass + meas temp + amat + lifestyle ----
brms_rmaxRMRbigN_mtAmatLs_noEndos <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmat + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxRMRbigN_noEndos, 
  family = gaussian(),
  control = list(adapt_delta = 0.99),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxRMR_corrma_noEndos),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)
# summary(brms_rmaxRMRbigN_mtAmatLs_noEndos)
# save(brms_rmaxRMRbigN_mtAmatLs_noEndos, file="models/20230314_noEndos/brms_rmaxRMRbigN_mtAmatLs_noEndos.rds")

# RMR ~ meas mass + meas temp + rmax + lifestyle ----
brms_rmaxRMRbigN_mtRmaxLs_noEndos <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxRMRbigN_noEndos, 
  family = gaussian(),
  control = list(adapt_delta = 0.99),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxRMR_corrma_noEndos),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)
# summary(brms_rmaxRMRbigN_mtRmaxLs_noEndos)
# save(brms_rmaxRMRbigN_mtRmaxLs_noEndos, file="models/20230314_noEndos/brms_rmaxRMRbigN_mtRmaxLs_noEndos.rds")

# MMR ~ meas mass + meas temp + lifestyle ----
brms_rmaxMMRbigN_mtLs_noEndos <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxMMRbigN_noEndos, 
  family = gaussian(),
  control = list(max_treedepth = 15, adapt_delta = 0.999),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxMMR_corrma_noEndos),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)
# summary(brms_rmaxMMRbigN_mtLs_noEndos)
# save(brms_rmaxMMRbigN_mtLs_noEndos, file="models/20230314_noEndos/brms_rmaxMMRbigN_mtLs_noEndos.rds")

# MMR ~ meas mass + meas temp + amat + lifestyle ----
brms_rmaxMMRbigN_mtAmatLs_noEndos <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmat + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxMMRbigN_noEndos, 
  family = gaussian(),
  control = list(max_treedepth = 15, adapt_delta = 0.999),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxMMR_corrma_noEndos),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)
# summary(brms_rmaxMMRbigN_mtAmatLs_noEndos)
# save(brms_rmaxMMRbigN_mtAmatLs_noEndos, file="models/20230314_noEndos/brms_rmaxMMRbigN_mtAmatLs_noEndos.rds")

# MMR ~ meas mass + meas temp + rmax + lifestyle ----
brms_rmaxMMRbigN_mtRmaxLs_noEndos <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxMMRbigN_noEndos, 
  family = gaussian(),
  control = list(max_treedepth = 15, adapt_delta = 0.999),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxMMR_corrma_noEndos),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)
# summary(brms_rmaxMMRbigN_mtRmaxLs_noEndos)
# save(brms_rmaxMMRbigN_mtRmaxLs_noEndos, file="models/20230314_noEndos/brms_rmaxMMRbigN_mtRmaxLs_noEndos.rds")

# AAS ~ meas mass + meas temp + lifestyle ----
brms_rmaxAASbigN_mtLs_noEndos <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxASbigN_noEndos, 
  family = gaussian(),
  control = list(max_treedepth = 15, adapt_delta = 0.999),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxAS_corrma_noEndos),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)
# summary(brms_rmaxAASbigN_mtLs_noEndos)
# save(brms_rmaxAASbigN_mtLs_noEndos, file="models/20230314_noEndos/brms_rmaxAASbigN_mtLs_noEndos.rds")

# AAS ~ meas mass + meas temp + amat + lifestyle ----
brms_rmaxAASbigN_mtAmatLs_noEndos <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmat + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxASbigN_noEndos, 
  family = gaussian(),
  control = list(max_treedepth = 15, adapt_delta = 0.999),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxAS_corrma_noEndos),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)
# summary(brms_rmaxAASbigN_mtAmatLs_noEndos)
# save(brms_rmaxAASbigN_mtAmatLs_noEndos, file="models/20230314_noEndos/brms_rmaxAASbigN_mtAmatLs_noEndos.rds")

# AAS ~ meas mass + meas temp + rmax + lifestyle ----
brms_rmaxAASbigN_mtRmaxLs_noEndos <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxASbigN_noEndos, 
  family = gaussian(),
  control = list(max_treedepth = 15, adapt_delta = 0.999),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxAS_corrma_noEndos),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)
# summary(brms_rmaxAASbigN_mtRmaxLs_noEndos)
# save(brms_rmaxAASbigN_mtRmaxLs_noEndos, file="models/20230314_noEndos/brms_rmaxAASbigN_mtRmaxLs_noEndos.rds")


# # * ----
# 
# # MODELS ON UNSTANDARDIZED VARIABLES ----
# # For model coefficients in Table 10 
# # Only inverse temperature is standardized
# 
# # RMR ~ meas mass + meas temp + lifestyle (UNSTD) ----
# brms_rmaxRMRbigN_mtLs_unstd_noTunas <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxRMRbigN_noTunas, 
#   family = gaussian(),
#   control = list(adapt_delta = 0.99),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxRMR_corrma_noTunas),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# # Warning messages:
# # 1: There were 3 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxRMRbigN_mtLs_unstd_noTunas)
# save(brms_rmaxRMRbigN_mtLs_unstd_noTunas, file="models/20230314_noTunas/brms_rmaxRMRbigN_mtLs_unstd_noTunas.rds")
# 
# # RMR ~ meas mass + meas temp + amat + lifestyle (UNSTD) ----
# brms_rmaxRMRbigN_mtAmatLs_unstd_noTunas <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmat + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxRMRbigN_noTunas, 
#   family = gaussian(),
#   control = list(adapt_delta = 0.99),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxRMR_corrma_noTunas),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# # Warning messages:
# # 1: There were 1 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxRMRbigN_mtAmatLs_unstd_noTunas)
# save(brms_rmaxRMRbigN_mtAmatLs_unstd_noTunas, file="models/20230314_noTunas/brms_rmaxRMRbigN_mtAmatLs_unstd_noTunas.rds")
# 
# # RMR ~ meas mass + meas temp + rmax + lifestyle (UNSTD) ----
# brms_rmaxRMRbigN_mtRmaxLs_unstd_noTunas <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logRmax + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxRMRbigN_noTunas, 
#   family = gaussian(),
#   control = list(adapt_delta = 0.99),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxRMR_corrma_noTunas),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# # Warning messages:
# # 1: There were 9 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxRMRbigN_mtRmaxLs_unstd_noTunas)
# save(brms_rmaxRMRbigN_mtRmaxLs_unstd_noTunas, file="models/20230314_noTunas/brms_rmaxRMRbigN_mtRmaxLs_unstd_noTunas.rds")
# 
# # MMR ~ meas mass + meas temp + lifestyle (UNSTD) ----
# brms_rmaxMMRbigN_mtLs_unstd_noTunas <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxMMRbigN_noTunas, 
#   family = gaussian(),
#   control = list(max_treedepth = 15, adapt_delta = 0.999),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxMMR_corrma_noTunas),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# # Warning messages:
# # 1: There were 1 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See http://mc-stan.org/misc/warnings.html#bfmi-low 
# # 3: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxMMRbigN_mtLs_unstd_noTunas)
# save(brms_rmaxMMRbigN_mtLs_unstd_noTunas, file="models/20230314_noTunas/brms_rmaxMMRbigN_mtLs_unstd_noTunas.rds")
# 
# # MMR ~ meas mass + meas temp + amat + lifestyle (UNSTD) ----
# brms_rmaxMMRbigN_mtAmatLs_unstd_noTunas <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmat + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxMMRbigN_noTunas, 
#   family = gaussian(),
#   control = list(max_treedepth = 15, adapt_delta = 0.999),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxMMR_corrma_noTunas),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# # Warning messages:
# # 1: There were 3 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxMMRbigN_mtAmatLs_unstd_noTunas)
# save(brms_rmaxMMRbigN_mtAmatLs_unstd_noTunas, file="models/20230314_noTunas/brms_rmaxMMRbigN_mtAmatLs_unstd_noTunas.rds")
# 
# # MMR ~ meas mass + meas temp + rmax + lifestyle (UNSTD) ----
# brms_rmaxMMRbigN_mtRmaxLs_unstd_noTunas <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logRmax + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxMMRbigN_noTunas, 
#   family = gaussian(),
#   control = list(max_treedepth = 15, adapt_delta = 0.999),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxMMR_corrma_noTunas),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# # Warning messages:
# # 1: There were 2 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxMMRbigN_mtRmaxLs_unstd_noTunas)
# save(brms_rmaxMMRbigN_mtRmaxLs_unstd_noTunas, file="models/20230314_noTunas/brms_rmaxMMRbigN_mtRmaxLs_unstd_noTunas.rds")
# 
# # AAS ~ meas mass + meas temp + lifestyle (UNSTD) ----
# brms_rmaxAASbigN_mtLs_unstd_noTunas <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxASbigN_noTunas, 
#   family = gaussian(),
#   control = list(max_treedepth = 15, adapt_delta = 0.999),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxAS_corrma_noTunas),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# summary(brms_rmaxAASbigN_mtLs_unstd_noTunas)
# save(brms_rmaxAASbigN_mtLs_unstd_noTunas, file="models/20230314_noTunas/brms_rmaxAASbigN_mtLs_unstd_noTunas.rds")
# 
# # AAS ~ meas mass + meas temp + amat + lifestyle (UNSTD) ----
# brms_rmaxAASbigN_mtAmatLs_unstd_noTunas <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmat + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxASbigN_noTunas, 
#   family = gaussian(),
#   control = list(max_treedepth = 15, adapt_delta = 0.999),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxAS_corrma_noTunas),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# # Warning messages:
# # 1: There were 1 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxAASbigN_mtAmatLs_unstd_noTunas)
# save(brms_rmaxAASbigN_mtAmatLs_unstd_noTunas, file="models/20230314_noTunas/brms_rmaxAASbigN_mtAmatLs_unstd_noTunas.rds")
# 
# # AAS ~ meas mass + meas temp + rmax + lifestyle (UNSTD) ----
# brms_rmaxAASbigN_mtRmaxLs_unstd_noTunas <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logRmax + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxASbigN_noTunas, 
#   family = gaussian(),
#   control = list(max_treedepth = 15, adapt_delta = 0.999),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxAS_corrma_noTunas),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# summary(brms_rmaxAASbigN_mtRmaxLs_unstd_noTunas)
# save(brms_rmaxAASbigN_mtRmaxLs_unstd_noTunas, file="models/20230314_noTunas/brms_rmaxAASbigN_mtRmaxLs_unstd_noTunas.rds")

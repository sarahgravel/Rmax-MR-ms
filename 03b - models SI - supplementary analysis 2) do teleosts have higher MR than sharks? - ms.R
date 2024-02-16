# Sarah Gravel
# 03 Supplementary analyses - models

# Models with 'taxon' factor
# Table S5

# Did teleosts have higher MR than sharks (after accounting for mass, temperature, and ecological lifestyle)?
# Ran top models from Q1 and Q2, but with the addition of fixed 'taxon' factor with 2 levels: teleost or elasmo.

# MODELS ON STANDARDIZED VARIABLES ----
# Table 11

# RMR ~ meas mass + meas temp + lifestyle + taxon ----
brms_rmaxRMRbigN_mtLs_taxon <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)), 
  data =  transDF_rmaxRMRbigN, 
  family = gaussian(),
  control = list(adapt_delta = 0.99),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxRMR_corrma),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)

# RMR ~ meas mass + meas temp + amat + lifestyle + taxon ----
brms_rmaxRMRbigN_mtAmatLs_taxon <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmat + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxRMRbigN, 
  family = gaussian(),
  control = list(adapt_delta = 0.99),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxRMR_corrma),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)

# RMR ~ meas mass + meas temp + rmax + lifestyle + taxon ----
brms_rmaxRMRbigN_mtRmaxLs_taxon <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxRMRbigN, 
  family = gaussian(),
  control = list(adapt_delta = 0.99),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxRMR_corrma),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)

# MMR ~ meas mass + meas temp + lifestyle + taxon ----
brms_rmaxMMRbigN_mtLs_taxon <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)), 
  data =  transDF_rmaxMMRbigN, 
  family = gaussian(),
  control = list(max_treedepth = 15, adapt_delta = 0.999),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxMMR_corrma),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)

# MMR ~ meas mass + meas temp + rmax + lifestyle + taxon ----
brms_rmaxMMRbigN_mtRmaxLs_taxon <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxMMRbigN, 
  family = gaussian(),
  control = list(max_treedepth = 15, adapt_delta = 0.999),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxMMR_corrma),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)

# AAS ~ meas mass + meas temp + lifestyle + taxon ----
brms_rmaxAASbigN_mtLs_taxon <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)), 
  data =  transDF_rmaxASbigN, 
  family = gaussian(),
  control = list(max_treedepth = 15, adapt_delta = 0.999),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxAS_corrma),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)

# AAS ~ meas mass + meas temp + amat + lifestyle + taxon ----
brms_rmaxAASbigN_mtAmatLs_taxon <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmat + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxASbigN, 
  family = gaussian(),
  control = list(max_treedepth = 15, adapt_delta = 0.999),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxAS_corrma),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)

# AAS ~ meas mass + meas temp + rmax + lifestyle + taxon ----
brms_rmaxAASbigN_mtRmaxLs_taxon <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxASbigN, 
  family = gaussian(),
  control = list(max_treedepth = 15, adapt_delta = 0.999),
  iter = 10000,
  warmup = 2000,
  data2 = list(A = rmaxAS_corrma),
  prior = c(
    prior(student_t(3, 0, 10), "b"),
    prior(student_t(3, 0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  chains = 4, cores = 4
)

# # * ----
# 
# # MODELS ON UNSTANDARDIZED VARIABLES ----
# # For model coefficients in Table 12 
# # Only inverse temperature is standardized
# 
# # RMR ~ meas mass + meas temp + lifestyle + taxon (UNSTD) ----
# brms_rmaxRMRbigN_mtLs_taxon_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)), 
#   data =  transDF_rmaxRMRbigN, 
#   family = gaussian(),
#   control = list(adapt_delta = 0.99),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxRMR_corrma),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# # Warning messages:
# # 1: There were 10 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# 
# # RMR ~ meas mass + meas temp + amat + lifestyle + taxon (UNSTD)----
# brms_rmaxRMRbigN_mtAmatLs_taxon_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmat + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxRMRbigN, 
#   family = gaussian(),
#   control = list(adapt_delta = 0.99),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxRMR_corrma),
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
# 
# # RMR ~ meas mass + meas temp + rmax + lifestyle + taxon (UNSTD) ----
# brms_rmaxRMRbigN_mtRmaxLs_taxon_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logRmax + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxRMRbigN, 
#   family = gaussian(),
#   control = list(adapt_delta = 0.99),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxRMR_corrma),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# 
# # MMR ~ meas mass + meas temp + lifestyle + taxon (UNSTD) ----
# brms_rmaxMMRbigN_mtLs_taxon_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)), 
#   data =  transDF_rmaxMMRbigN, 
#   family = gaussian(),
#   control = list(max_treedepth = 15, adapt_delta = 0.999),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxMMR_corrma),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# # Warning messages:
# # 1: There were 13 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: There were 4 chains where the estimated Bayesian Fraction of Missing Information was low. See http://mc-stan.org/misc/warnings.html#bfmi-low 
# # 3: Examine the pairs() plot to diagnose sampling problems
# 
# # MMR ~ meas mass + meas temp + rmax + lifestyle + taxon (UNSTD) ----
# brms_rmaxMMRbigN_mtRmaxLs_taxon_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logRmax + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxMMRbigN, 
#   family = gaussian(),
#   control = list(max_treedepth = 15, adapt_delta = 0.999),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxMMR_corrma),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# # Warning messages:
# # 1: There were 8 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# 
# # AAS ~ meas mass + meas temp + lifestyle + taxon (UNSTD) ----
# brms_rmaxAASbigN_mtLs_taxon_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)), 
#   data =  transDF_rmaxASbigN, 
#   family = gaussian(),
#   control = list(max_treedepth = 15, adapt_delta = 0.999),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxAS_corrma),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# 
# # AAS ~ meas mass + meas temp + amat + lifestyle + taxon (UNSTD) ----
# brms_rmaxAASbigN_mtAmatLs_taxon_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmat + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxASbigN, 
#   family = gaussian(),
#   control = list(max_treedepth = 15, adapt_delta = 0.999),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxAS_corrma),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )
# 
# # AAS ~ meas mass + meas temp + rmax + lifestyle + taxon (UNSTD) ----
# brms_rmaxAASbigN_mtRmaxLs_taxon_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logRmax + lifestyle + TeleostOrElasmo + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxASbigN, 
#   family = gaussian(),
#   control = list(max_treedepth = 15, adapt_delta = 0.999),
#   iter = 10000,
#   warmup = 2000,
#   data2 = list(A = rmaxAS_corrma),
#   prior = c(
#     prior(student_t(3, 0, 10), "b"),
#     prior(student_t(3, 0, 10), "Intercept"),
#     prior(student_t(3, 0, 20), "sd"),
#     prior(student_t(3, 0, 20), "sigma")
#   ),
#   chains = 4, cores = 4
# )

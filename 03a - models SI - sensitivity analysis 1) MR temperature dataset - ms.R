# Sarah Gravel
# 03 Supplementary analyses - models

# All models from Q1 and Q1 but based on MR 'temperature dataset' (where MR was selected based on measurement temperature closest to 15C)
# Tables S2-4

# check make sure all species in dataset are in tree
setdiff(transDF_rmaxRMRmeanT$phylo, rmaxRMR_phy$tip.label)
setdiff(transDF_rmaxMMRmeanT$phylo, rmaxMMR_phy$tip.label)
setdiff(transDF_rmaxASmeanT$phylo, rmaxAS_phy$tip.label)

# MODELS ON STANDARDIZED VARIABLES ----

# RMR ~ meas mass + meas temp + lifestyle ----
brms_rmaxRMRmeanT_mtLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)), 
  data =  transDF_rmaxRMRmeanT, 
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
# summary(brms_rmaxRMRmeanT_mtLs)
# save(brms_rmaxRMRmeanT_mtLs, file="models/20230314/brms_rmaxRMRmeanT_mtLs.rds")

# RMR ~ meas mass + meas temp + repro output + lifestyle ----
brms_rmaxRMRmeanT_mtROLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogReproOutput + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxRMRmeanT, 
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
# summary(brms_rmaxRMRmeanT_mtROLs)
# save(brms_rmaxRMRmeanT_mtROLs, file="models/20230314/brms_rmaxRMRmeanT_mtROLs.rds")

# RMR ~ meas mass + meas temp + amat + lifestyle ----
brms_rmaxRMRmeanT_mtAmatLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmat + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxRMRmeanT, 
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
# summary(brms_rmaxRMRmeanT_mtAmatLs)
# save(brms_rmaxRMRmeanT_mtAmatLs, file="models/20230314/brms_rmaxRMRmeanT_mtAmatLs.rds")

# RMR ~ meas mass + meas temp + amax + lifestyle ----
brms_rmaxRMRmeanT_mtAmaxLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmax + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxRMRmeanT, 
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
# summary(brms_rmaxRMRmeanT_mtAmaxLs)
# save(brms_rmaxRMRmeanT_mtAmaxLs, file="models/20230314/brms_rmaxRMRmeanT_mtAmaxLs.rds")

# RMR ~ meas mass + meas temp + rmax + lifestyle ----
brms_rmaxRMRmeanT_mtRmaxLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxRMRmeanT, 
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
# summary(brms_rmaxRMRmeanT_mtRmaxLs)
# save(brms_rmaxRMRmeanT_mtRmaxLs, file="models/20230314/brms_rmaxRMRmeanT_mtRmaxLs.rds")

# MMR ~ meas mass + meas temp + lifestyle ----
brms_rmaxMMRmeanT_mtLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)), 
  data =  transDF_rmaxMMRmeanT, 
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
# summary(brms_rmaxMMRmeanT_mtLs)  
# save(brms_rmaxMMRmeanT_mtLs, file= "models/20230314/brms_rmaxMMRmeanT_mtLs.rds")

# MMR ~ meas mass + meas temp + repro output + lifestyle ----
brms_rmaxMMRmeanT_mtROLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogReproOutput + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxMMRmeanT, 
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
# summary(brms_rmaxMMRmeanT_mtROLs) 
# save(brms_rmaxMMRmeanT_mtROLs, file="models/20230314/brms_rmaxMMRmeanT_mtROLs.rds")

# MMR ~ meas mass + meas temp + amat + lifestyle ----
brms_rmaxMMRmeanT_mtAmatLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmat + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxMMRmeanT, 
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
# summary(brms_rmaxMMRmeanT_mtAmatLs) # makes temp effect close to nothing
# save(brms_rmaxMMRmeanT_mtAmatLs, file="models/20230314/brms_rmaxMMRmeanT_mtAmatLs.rds")

# MMR ~ meas mass + meas temp + amax + lifestyle ----
brms_rmaxMMRmeanT_mtAmaxLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmax + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxMMRmeanT, 
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
# summary(brms_rmaxMMRmeanT_mtAmaxLs)
# save(brms_rmaxMMRmeanT_mtAmaxLs, file="models/20230314/brms_rmaxMMRmeanT_mtAmaxLs.rds")

# MMR ~ meas mass + meas temp + rmax + lifestyle ----
brms_rmaxMMRmeanT_mtRmaxLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxMMRmeanT, 
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
# summary(brms_rmaxMMRmeanT_mtRmaxLs)
# save(brms_rmaxMMRmeanT_mtRmaxLs, file="models/20230314/brms_rmaxMMRmeanT_mtRmaxLs.rds")

# AAS ~ meas mass + meas temp + lifestyle ----
brms_rmaxAASmeanT_mtLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)), 
  data =  transDF_rmaxASmeanT, 
  family = gaussian(),
  control = list(max_treedepth = 15, adapt_delta = 0.99),
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
# summary(brms_rmaxAASmeanT_mtLs)
# save(brms_rmaxAASmeanT_mtLs, file="models/20230314/brms_rmaxAASmeanT_mtLs.rds")

# AAS ~ meas mass + meas temp + repro output + lifestyle ----
brms_rmaxAASmeanT_mtROLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogReproOutput + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxASmeanT, 
  family = gaussian(),
  control = list(max_treedepth=15, adapt_delta = 0.99),
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
# summary(brms_rmaxAASmeanT_mtROLs) 
# save(brms_rmaxAASmeanT_mtROLs, file="models/20230314/brms_rmaxAASmeanT_mtROLs.rds")

# AAS ~ meas mass + meas temp + amat + lifestyle ----
brms_rmaxAASmeanT_mtAmatLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmat + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxASmeanT, 
  family = gaussian(),
  control = list(max_treedepth=15, adapt_delta = 0.99),
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
# summary(brms_rmaxAASmeanT_mtAmatLs)
# save(brms_rmaxAASmeanT_mtAmatLs, file="models/20230314/brms_rmaxAASmeanT_mtAmatLs.rds")

# AAS ~ meas mass + meas temp + amax + lifestyle ----
brms_rmaxAASmeanT_mtAmaxLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmax + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxASmeanT, 
  family = gaussian(),
  control = list(max_treedepth=15, adapt_delta = 0.99),
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
# summary(brms_rmaxAASmeanT_mtAmaxLs)
# save(brms_rmaxAASmeanT_mtAmaxLs, file="models/20230314/brms_rmaxAASmeanT_mtAmaxLs.rds")

# AAS ~ meas mass + meas temp + rmax + lifestyle ----
brms_rmaxAASmeanT_mtRmaxLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + lifestyle + (1|gr(phylo, cov = A)),
  data =  transDF_rmaxASmeanT, 
  family = gaussian(),
  control = list(max_treedepth=15, adapt_delta = 0.99),
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
# summary(brms_rmaxAASmeanT_mtRmaxLs)
# save(brms_rmaxAASmeanT_mtRmaxLs, file="models/20230314/brms_rmaxAASmeanT_mtRmaxLs.rds")


# # * ----
# 
# # MODELS ON UNSTANDARDIZED VARIABLES ----
# # For model coefficients 
# # Only inverse temperature is standardized
# 
# # RMR ~ meas mass + meas temp + lifestyle UNSTD ----
# brms_rmaxRMRmeanT_mtLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)), 
#   data =  transDF_rmaxRMRmeanT, 
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
# #   1: There were 11 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxRMRmeanT_mtLs_unstd)
# save(brms_rmaxRMRmeanT_mtLs_unstd, file="models/20230314/brms_rmaxRMRmeanT_mtLs_unstd.rds")
# 
# # RMR ~ meas mass + meas temp + repro output + lifestyle UNSTD ----
# brms_rmaxRMRmeanT_mtROLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logReproOutput + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxRMRmeanT, 
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
# # 1: There were 5 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxRMRmeanT_mtROLs_unstd)
# save(brms_rmaxRMRmeanT_mtROLs_unstd, file="models/20230314/brms_rmaxRMRmeanT_mtROLs_unstd.rds")
# 
# # RMR ~ meas mass + meas temp + amat + lifestyle UNSTD ----
# brms_rmaxRMRmeanT_mtAmatLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmat + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxRMRmeanT, 
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
# summary(brms_rmaxRMRmeanT_mtAmatLs_unstd)
# save(brms_rmaxRMRmeanT_mtAmatLs_unstd, file="models/20230314/brms_rmaxRMRmeanT_mtAmatLs_unstd.rds")
# 
# # RMR ~ meas mass + meas temp + amax + lifestyle UNSTD ----
# brms_rmaxRMRmeanT_mtAmaxLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmax + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxRMRmeanT, 
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
# summary(brms_rmaxRMRmeanT_mtAmaxLs_unstd)
# save(brms_rmaxRMRmeanT_mtAmaxLs_unstd, file="models/20230314/brms_rmaxRMRmeanT_mtAmaxLs_unstd.rds")
# 
# # RMR ~ meas mass + meas temp + rmax + lifestyle UNSTD ----
# brms_rmaxRMRmeanT_mtRmaxLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logRmax + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxRMRmeanT, 
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
# summary(brms_rmaxRMRmeanT_mtRmaxLs_unstd)
# save(brms_rmaxRMRmeanT_mtRmaxLs_unstd, file="models/20230314/brms_rmaxRMRmeanT_mtRmaxLs_unstd.rds")
# 
# # MMR ~ meas mass + meas temp + lifestyle UNSTD ----
# 
# brms_rmaxMMRmeanT_mtLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxMMRmeanT, 
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
# # 1: There were 20 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See http://mc-stan.org/misc/warnings.html#bfmi-low 
# # 3: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxMMRmeanT_mtLs_unstd) 
# save(brms_rmaxMMRmeanT_mtLs_unstd, file="models/20230314/brms_rmaxMMRmeanT_mtLs_unstd.rds")
# 
# # MMR ~ meas mass + meas temp + repro output + lifestyle UNSTD ----
# brms_rmaxMMRmeanT_mtROLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logReproOutput + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxMMRmeanT, 
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
# # 1: There were 10 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See http://mc-stan.org/misc/warnings.html#bfmi-low 
# # 3: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxMMRmeanT_mtROLs_unstd) 
# save(brms_rmaxMMRmeanT_mtROLs_unstd, file="models/20230314/brms_rmaxMMRmeanT_mtROLs_unstd.rds")
# 
# # MMR ~ meas mass + meas temp + amat + lifestyle UNSTD ----
# 
# brms_rmaxMMRmeanT_mtAmatLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmat + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxMMRmeanT, 
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
# # 1: There were 5 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxMMRmeanT_mtAmatLs_unstd)
# save(brms_rmaxMMRmeanT_mtAmatLs_unstd, file="models/20230314/brms_rmaxMMRmeanT_mtAmatLs_unstd.rds")
# 
# # MMR ~ meas mass + meas temp + amax + lifestyle UNSTD ----
# 
# brms_rmaxMMRmeanT_mtAmaxLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmax + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxMMRmeanT, 
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
# # 1: There were 16 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxMMRmeanT_mtAmaxLs_unstd)
# save(brms_rmaxMMRmeanT_mtAmaxLs_unstd, file="models/20230314/brms_rmaxMMRmeanT_mtAmaxLs_unstd.rds")
# 
# # MMR ~ meas mass + meas temp + rmax + lifestyle UNSTD ----
# 
# brms_rmaxMMRmeanT_mtRmaxLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logRmax + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxMMRmeanT, 
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
# 
# summary(brms_rmaxMMRmeanT_mtRmaxLs_unstd)
# save(brms_rmaxMMRmeanT_mtRmaxLs_unstd, file="models/20230314/brms_rmaxMMRmeanT_mtRmaxLs_unstd.rds")
# 
# # AAS ~ meas mass + meas temp + lifestyle UNSTD ----
# brms_rmaxAASmeanT_mtLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)), 
#   data =  transDF_rmaxASmeanT, 
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
# summary(brms_rmaxAASmeanT_mtLs_unstd)
# save(brms_rmaxAASmeanT_mtLs_unstd, file="models/20230314/brms_rmaxAASmeanT_mtLs_unstd.rds")
# 
# # AAS ~ meas mass + meas temp + repro output + lifestyle UNSTD ----
# brms_rmaxAASmeanT_mtROLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logReproOutput + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxASmeanT, 
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
# # Warning messages:
# # 1: There were 1 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxAASmeanT_mtROLs_unstd)
# save(brms_rmaxAASmeanT_mtROLs_unstd, file="models/20230314/brms_rmaxAASmeanT_mtROLs_unstd.rds")
# 
# # AAS ~ meas mass + meas temp + amat + lifestyle UNSTD ----
# brms_rmaxAASmeanT_mtAmatLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmat + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxASmeanT, 
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
# summary(brms_rmaxAASmeanT_mtAmatLs_unstd)
# save(brms_rmaxAASmeanT_mtAmatLs_unstd, file="models/20230314/brms_rmaxAASmeanT_mtAmatLs_unstd.rds")
# 
# # AAS ~ meas mass + meas temp + amax + lifestyle UNSTD ----
# brms_rmaxAASmeanT_mtAmaxLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmax + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxASmeanT, 
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
# summary(brms_rmaxAASmeanT_mtAmaxLs_unstd)
# save(brms_rmaxAASmeanT_mtAmaxLs_unstd, file="models/20230314/brms_rmaxAASmeanT_mtAmaxLs_unstd.rds")
# 
# # AAS ~ meas mass + meas temp + rmax + lifestyle UNSTD ----
# brms_rmaxAASmeanT_mtRmaxLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logRmax + lifestyle + (1|gr(phylo, cov = A)),
#   data =  transDF_rmaxASmeanT, 
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
# summary(brms_rmaxAASmeanT_mtRmaxLs_unstd)
# save(brms_rmaxAASmeanT_mtRmaxLs_unstd, file="models/20230314/brms_rmaxAASmeanT_mtRmaxLs_unstd.rds")

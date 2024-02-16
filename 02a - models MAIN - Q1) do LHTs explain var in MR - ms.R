# Sarah Gravel
# 02 Main analyses - models

# Question 1: Do LHTs explain variation in MR?
# (Based on sample size datasets 'bigN')

# Check make sure all species in data sets are in trees:
setdiff(transDF_rmaxRMRbigN$phylo, rmaxRMR_phy$tip.label)
setdiff(transDF_rmaxMMRbigN$phylo, rmaxMMR_phy$tip.label)
setdiff(transDF_rmaxASbigN$phylo, rmaxAS_phy$tip.label)
setdiff(rmaxRMR_phy$tip.label, transDF_rmaxRMRbigN$phylo)
setdiff(rmaxMMR_phy$tip.label, transDF_rmaxMMRbigN$phylo)
setdiff(rmaxAS_phy$tip.label, transDF_rmaxASbigN$phylo)

# MODELS ON STANDARDIZED VARIABLES ----
# Tables 1 & 2

# RMR ~ meas mass + meas temp + lifestyle ----
brms_rmaxRMRbigN_mtLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)), 
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
# Warning messages:
# 1: There were 4 divergent transitions after warmup. See
# http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# to find out why this is a problem and how to eliminate them. 
# 2: Examine the pairs() plot to diagnose sampling problems

# summary(brms_rmaxRMRbigN_mtLs)
# save(brms_rmaxRMRbigN_mtLs, file="models/20230314/brms_rmaxRMRbigN_mtLs.rds")

# RMR ~ meas mass + meas temp + repro output + lifestyle ----
brms_rmaxRMRbigN_mtROLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogReproOutput + lifestyle + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxRMRbigN_mtROLs)
# save(brms_rmaxRMRbigN_mtROLs, file="models/20230314/brms_rmaxRMRbigN_mtROLs.rds")

# RMR ~ meas mass + meas temp + amat + lifestyle ----
brms_rmaxRMRbigN_mtAmatLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmat + lifestyle + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxRMRbigN_mtAmatLs)
# save(brms_rmaxRMRbigN_mtAmatLs, file="models/20230314/brms_rmaxRMRbigN_mtAmatLs.rds")

# RMR ~ meas mass + meas temp + amax + lifestyle ----
brms_rmaxRMRbigN_mtAmaxLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmax + lifestyle + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxRMRbigN_mtAmaxLs)
# save(brms_rmaxRMRbigN_mtAmaxLs, file="models/20230314/brms_rmaxRMRbigN_mtAmaxLs.rds")

# MMR ~ meas mass + meas temp + lifestyle ----
brms_rmaxMMRbigN_mtLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)), 
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
# summary(brms_rmaxMMRbigN_mtLs) 
# save(brms_rmaxMMRbigN_mtLs, file="models/20230314/brms_rmaxMMRbigN_mtLs.rds")

# MMR ~ meas mass + meas temp + repro output + lifestyle ----
brms_rmaxMMRbigN_mtROLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogReproOutput + lifestyle + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxMMRbigN_mtROLs) 
# save(brms_rmaxMMRbigN_mtROLs, file="models/20230314/brms_rmaxMMRbigN_mtROLs.rds")

# MMR ~ meas mass + meas temp + amat + lifestyle ----
brms_rmaxMMRbigN_mtAmatLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmat + lifestyle + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxMMRbigN_mtAmatLs)
# save(brms_rmaxMMRbigN_mtAmatLs, file="models/20230314/brms_rmaxMMRbigN_mtAmatLs.rds")

# MMR ~ meas mass + meas temp + amax + lifestyle ----
brms_rmaxMMRbigN_mtAmaxLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmax + lifestyle + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxMMRbigN_mtAmaxLs)
# save(brms_rmaxMMRbigN_mtAmaxLs, file="models/20230314/brms_rmaxMMRbigN_mtAmaxLs.rds")

# AAS ~ meas mass + meas temp + lifestyle ----
brms_rmaxAASbigN_mtLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)), 
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
# summary(brms_rmaxAASbigN_mtLs)
# save(brms_rmaxAASbigN_mtLs, file="models/20230314/brms_rmaxAASbigN_mtLs.rds")

# AAS ~ meas mass + meas temp + repro output + lifestyle ----
brms_rmaxAASbigN_mtROLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogReproOutput + lifestyle + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxAASbigN_mtROLs)
# save(brms_rmaxAASbigN_mtROLs, file="models/20230314/brms_rmaxAASbigN_mtROLs.rds")

# AAS ~ meas mass + meas temp + amat + lifestyle ----
brms_rmaxAASbigN_mtAmatLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmat + lifestyle + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxAASbigN_mtAmatLs)
# save(brms_rmaxAASbigN_mtAmatLs, file="models/20230314/brms_rmaxAASbigN_mtAmatLs.rds")

# AAS ~ meas mass + meas temp + amax + lifestyle ----
brms_rmaxAASbigN_mtAmaxLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmax + lifestyle + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxAASbigN_mtAmaxLs)
# save(brms_rmaxAASbigN_mtAmaxLs, file="models/20230314/brms_rmaxAASbigN_mtAmaxLs.rds")


# # *----
# 
# # MODELS ON UNSTANDARDIZED VARIABLES ----
# # For model coefficients in Table S1
# # Only inverse temperature is standardized
# 
# # RMR ~ meas mass + meas temp + lifestyle UNSTD ----
# brms_rmaxRMRbigN_mtLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)), 
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
# # 1: There were 2 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# # 3: In doTryCatch(return(expr), name, parentenv, handler) : restarting interrupted promise evaluation
# summary(brms_rmaxRMRbigN_mtLs_unstd)
# save(brms_rmaxRMRbigN_mtLs_unstd, file="models/20230314/brms_rmaxRMRbigN_mtLs_unstd.rds")
# 
# # RMR ~ meas mass + meas temp + repro output + lifestyle UNSTD ----
# brms_rmaxRMRbigN_mtROLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logReproOutput + lifestyle + (1|gr(phylo, cov = A)),
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
# # 1: There were 8 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxRMRbigN_mtROLs_unstd)
# save(brms_rmaxRMRbigN_mtROLs_unstd, file="models/20230314/brms_rmaxRMRbigN_mtROLs_unstd.rds")
# 
# # RMR ~ meas mass + meas temp + amat + lifestyle UNSTD ----
# brms_rmaxRMRbigN_mtAmatLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmat + lifestyle + (1|gr(phylo, cov = A)),
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
# # 1: There were 20 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxRMRbigN_mtAmatLs_unstd)
# save(brms_rmaxRMRbigN_mtAmatLs_unstd, file="models/20230314/brms_rmaxRMRbigN_mtAmatLs_unstd.rds")
# 
# # RMR ~ meas mass + meas temp + amax + lifestyle UNSTD ----
# brms_rmaxRMRbigN_mtAmaxLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmax + lifestyle + (1|gr(phylo, cov = A)),
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
# # 1: There were 4 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxRMRbigN_mtAmaxLs_unstd)
# save(brms_rmaxRMRbigN_mtAmaxLs_unstd, file="models/20230314/brms_rmaxRMRbigN_mtAmaxLs_unstd.rds")
# 
# # MMR ~ meas mass + meas temp + lifestyle UNSTD ----
# brms_rmaxMMRbigN_mtLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)),
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
# # 1: There were 26 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: There were 3 chains where the estimated Bayesian Fraction of Missing Information was low. See http://mc-stan.org/misc/warnings.html#bfmi-low 
# # 3: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxMMRbigN_mtLs_unstd) 
# save(brms_rmaxMMRbigN_mtLs_unstd, file="models/20230314/brms_rmaxMMRbigN_mtLs_unstd.rds")
# 
# # MMR ~ meas mass + meas temp + repro output + lifestyle UNSTD ----
# brms_rmaxMMRbigN_mtROLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logReproOutput + lifestyle + (1|gr(phylo, cov = A)),
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
# #   1: There were 5 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: There were 3 chains where the estimated Bayesian Fraction of Missing Information was low. See http://mc-stan.org/misc/warnings.html#bfmi-low 
# # 3: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxMMRbigN_mtROLs_unstd) 
# save(brms_rmaxMMRbigN_mtROLs_unstd, file="models/20230314/brms_rmaxMMRbigN_mtROLs_unstd.rds")
# 
# # MMR ~ meas mass + meas temp + amat + lifestyle UNSTD ----
# brms_rmaxMMRbigN_mtAmatLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmat + lifestyle + (1|gr(phylo, cov = A)),
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
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxMMRbigN_mtAmatLs_unstd)
# save(brms_rmaxMMRbigN_mtAmatLs_unstd, file="models/20230314/brms_rmaxMMRbigN_mtAmatLs_unstd.rds")
# 
# # MMR ~ meas mass + meas temp + amax + lifestyle UNSTD ----
# brms_rmaxMMRbigN_mtAmaxLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmax + lifestyle + (1|gr(phylo, cov = A)),
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
# # 1: There were 9 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxMMRbigN_mtAmaxLs_unstd)
# save(brms_rmaxMMRbigN_mtAmaxLs_unstd, file="models/20230314/brms_rmaxMMRbigN_mtAmaxLs_unstd.rds")
# 
# # AAS ~ meas mass + meas temp + lifestyle UNSTD ----
# brms_rmaxAASbigN_mtLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + lifestyle + (1|gr(phylo, cov = A)), 
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
# # Warning messages:
# #   1: There were 15 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# summary(brms_rmaxAASbigN_mtLs_unstd)
# save(brms_rmaxAASbigN_mtLs_unstd, file="models/20230314/brms_rmaxAASbigN_mtLs_unstd.rds")
# 
# # AAS ~ meas mass + meas temp + repro output + lifestyle UNSTD ----
# brms_rmaxAASbigN_mtROLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logReproOutput + lifestyle + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxAASbigN_mtROLs_unstd)
# save(brms_rmaxAASbigN_mtROLs_unstd, file="models/20230314/brms_rmaxAASbigN_mtROLs_unstd.rds")
# 
# # AAS ~ meas mass + meas temp + amat + lifestyle UNSTD ----
# brms_rmaxAASbigN_mtAmatLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmat + lifestyle + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxAASbigN_mtAmatLs_unstd)
# save(brms_rmaxAASbigN_mtAmatLs_unstd, file="models/20230314/brms_rmaxAASbigN_mtAmatLs_unstd.rds")
# 
# # AAS ~ meas mass + meas temp + amax + lifestyle UNSTD ----
# brms_rmaxAASbigN_mtAmaxLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logAmax + lifestyle + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxAASbigN_mtAmaxLs_unstd)
# save(brms_rmaxAASbigN_mtAmaxLs_unstd, file="models/20230314/brms_rmaxAASbigN_mtAmaxLs_unstd.rds")

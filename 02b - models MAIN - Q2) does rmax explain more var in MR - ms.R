# Sarah Gravel
# 02 Main analyses - models

# Question 2: Does rmax explain variation in MR?
# (Based on sample size dataset)

# MODELS ON STANDARDIZED VARIABLES ----
# Table 2

# RMR ~ meas mass + meas temp + rmax + lifestyle ----
brms_rmaxRMRbigN_mtRmaxLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + lifestyle + (1|gr(phylo, cov = A)),
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

# MMR ~ meas mass + meas temp + rmax + lifestyle ----
brms_rmaxMMRbigN_mtRmaxLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + lifestyle + (1|gr(phylo, cov = A)),
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

# AAS ~ meas mass + meas temp + rmax + lifestyle ----
brms_rmaxAASbigN_mtRmaxLs <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + lifestyle + (1|gr(phylo, cov = A)),
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
# # For model coefficients in Table 3 
# # Only inverse temperature is standardized
# 
# # RMR ~ meas mass + meas temp + rmax + lifestyle UNSTD ----
# brms_rmaxRMRbigN_mtRmaxLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logRmax + lifestyle + (1|gr(phylo, cov = A)),
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
# # MMR ~ meas mass + meas temp + rmax + lifestyle UNSTD ----
# brms_rmaxMMRbigN_mtRmaxLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logRmax + lifestyle + (1|gr(phylo, cov = A)),
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
# # 1: There were 1 divergent transitions after warmup. See
# # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# 
# # AAS ~ meas mass + meas temp + rmax + lifestyle UNSTD ----
# brms_rmaxAASbigN_mtRmaxLs_unstd <- brm(
#   logMR ~ logMRMass + stdInvMRTemp + logRmax + lifestyle + (1|gr(phylo, cov = A)),
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

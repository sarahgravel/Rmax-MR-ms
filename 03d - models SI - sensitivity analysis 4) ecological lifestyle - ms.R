# Sarah Gravel
# 03 Supplementary analyses - models

# All models from Q1 and Q2 but WITHOUT ecological lifestyle
# Table S8

# RMR ~ meas mass + meas temp ----
brms_rmaxRMRbigN_mt <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + (1|gr(phylo, cov = A)), 
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
summary(brms_rmaxRMRbigN_mt)
save(brms_rmaxRMRbigN_mt, file="models/20230314/brms_rmaxRMRbigN_mt.rds")

# RMR ~ meas mass + meas temp + repro output ----
brms_rmaxRMRbigN_mtRO <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogReproOutput + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxRMRbigN_mtRO)
# save(brms_rmaxRMRbigN_mtRO, file="models/20230314/brms_rmaxRMRbigN_mtRO.rds")

# RMR ~ meas mass + meas temp + amat ----
brms_rmaxRMRbigN_mtAmat <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmat + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxRMRbigN_mtAmat)
# save(brms_rmaxRMRbigN_mtAmat, file="models/20230314/brms_rmaxRMRbigN_mtAmat.rds")

# RMR ~ meas mass + meas temp + amax ----
brms_rmaxRMRbigN_mtAmax <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmax +  (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxRMRbigN_mtAmax)
# save(brms_rmaxRMRbigN_mtAmax, file="models/20230314/brms_rmaxRMRbigN_mtAmax.rds")

# RMR ~ meas mass + meas temp + rmax ----
brms_rmaxRMRbigN_mtRmax <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxRMRbigN_mtRmax)
# save(brms_rmaxRMRbigN_mtRmax, file="models/20230314/brms_rmaxRMRbigN_mtRmax.rds")

# MMR ~ meas mass + meas temp ----
brms_rmaxMMRbigN_mt <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + (1|gr(phylo, cov = A)), 
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
# summary(brms_rmaxMMRbigN_mt) 
# save(brms_rmaxMMRbigN_mt, file="models/20230314/brms_rmaxMMRbigN_mt.rds")

# MMR ~ meas mass + meas temp + repro output ----
brms_rmaxMMRbigN_mtRO <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogReproOutput + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxMMRbigN_mtRO) 
# save(brms_rmaxMMRbigN_mtRO, file="models/20230314/brms_rmaxMMRbigN_mtRO.rds")

# MMR ~ meas mass + meas temp + amat ----
brms_rmaxMMRbigN_mtAmat<- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmat + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxMMRbigN_mtAmat)
# save(brms_rmaxMMRbigN_mtAmat, file="models/20230314/brms_rmaxMMRbigN_mtAmat.rds")

# MMR ~ meas mass + meas temp + amax ----
brms_rmaxMMRbigN_mtAmax <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmax + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxMMRbigN_mtAmax) # SIGMA RHAT 1.01
# save(brms_rmaxMMRbigN_mtAmax, file="models/20230314/brms_rmaxMMRbigN_mtAmax.rds")

# MMR ~ meas mass + meas temp + rmax ----
brms_rmaxMMRbigN_mtRmax <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxMMRbigN_mtRmax)
# save(brms_rmaxMMRbigN_mtRmax, file="models/20230314/brms_rmaxMMRbigN_mtRmax.rds")

# AAS ~ meas mass + meas temp ----
brms_rmaxAASbigN_mt <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + (1|gr(phylo, cov = A)), 
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
# summary(brms_rmaxAASbigN_mt)
# save(brms_rmaxAASbigN_mt, file="models/20230314/brms_rmaxAASbigN_mt.rds")

# AAS ~ meas mass + meas temp + repro output ----
brms_rmaxAASbigN_mtRO <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogReproOutput + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxAASbigN_mtRO)
# save(brms_rmaxAASbigN_mtRO, file="models/20230314/brms_rmaxAASbigN_mtRO.rds")

# AAS ~ meas mass + meas temp + amat ----
brms_rmaxAASbigN_mtAmat <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmat + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxAASbigN_mtAmat)
# save(brms_rmaxAASbigN_mtAmat, file="models/20230314/brms_rmaxAASbigN_mtAmat.rds")

# AAS ~ meas mass + meas temp + amax ----
brms_rmaxAASbigN_mtAmax <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogAmax + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxAASbigN_mtAmax)
# save(brms_rmaxAASbigN_mtAmax, file="models/20230314/brms_rmaxAASbigN_mtAmax.rds")

# AAS ~ meas mass + meas temp + rmax ----
brms_rmaxAASbigN_mtRmax <- brm(
  logMR ~ stdLogMRMass + stdInvMRTemp + stdLogRmax + (1|gr(phylo, cov = A)),
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
# summary(brms_rmaxAASbigN_mtRmax)
# save(brms_rmaxAASbigN_mtRmax, file="models/20230314/brms_rmaxAASbigN_mtRmax.rds")

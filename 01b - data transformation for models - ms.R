# Sarah Gravel
# 01 Transformations

# Transforming final data sets for models:
# Creates columns of logged data for all vars, with exception of inverse temperature 
# Creates standardized columns (centered and scaled) for all covariates

# Data transformation function:
transDF_func <- function(DF=NULL, MRtype =NULL, MRcriterion=NULL) { 
  transDF <- DF %>%
    mutate(stdRmax = scale(Rmax),
           logRmax = log(Rmax),
           stdLogRmax = scale(logRmax),
           stdAmat = scale(Amat),
           logAmat = log(Amat),
           stdLogAmat = scale(logAmat),
           stdAmax = scale(Amax),
           logAmax = log(Amax),
           stdLogAmax = scale(logAmax),
           stdReproOutput = scale(ReproOutput),
           logReproOutput = log(ReproOutput),
           stdLogReproOutput = scale(logReproOutput),
           logMR = log(WholeOrganismMRWatts), 
           invMRTemp = 1/(kb*(MRTempCelcius+273.15)), 
           stdInvMRTemp = scale(c(invMRTemp)), 
           logMRMass = log(MRMassGrams), 
           stdLogMRMass = scale(logMRMass))
  
  # Return transformed DF:
  return(transDF)
  
  # To save the rmax-MR (RMR,MMR,orAS) data set (MR data based on Temperature or Sample Size): 
  # write.csv(transDF, file = paste("data/full_transformed_Rmax_LHT_", MRtype, "_", MRcriterion, "_ms.csv", sep =""), row.names=FALSE)
  
}

# RMR-rmax DF ----
# 82 species
# Biggest sample size N:
transDF_rmaxRMRbigN <- transDF_func(DF = DF_rmaxRMRbigN, MRtype = "RMR", MRcriterion = "BiggestN")
# Closest to mean temperature 15C:
transDF_rmaxRMRmeanT <- transDF_func(DF= DF_rmaxRMRmeanT, MRtype = "RMR", MRcriterion = "ClosestToMeanTemp")

# MMR-rmax DF ----
# 49 species
# Biggest sample size N:
transDF_rmaxMMRbigN <- transDF_func(DF = DF_rmaxMMRbigN, MRtype = "MMR", MRcriterion = "BiggestN")
# Closest to mean temperature 15C:
transDF_rmaxMMRmeanT <- transDF_func(DF= DF_rmaxMMRmeanT, MRtype = "MMR", MRcriterion = "ClosestToMeanTemp")

# AS-rmax DF ----
# 45 species
# Biggest sample size N:
transDF_rmaxASbigN <- transDF_func(DF = DF_rmaxASbigN, MRtype = "AS", MRcriterion = "BiggestN")
# Closest to mean temperature 15C:
transDF_rmaxASmeanT <- transDF_func(DF = DF_rmaxASmeanT, MRtype = "AS", MRcriterion = "ClosestToMeanTemp")

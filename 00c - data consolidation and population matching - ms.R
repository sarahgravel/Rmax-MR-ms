# Sarah Gravel
# 00 Data pre-processing

# Consolidating all data (MR, LHTs, rmax) into one full data set 

# Note: requires packages in script 01a and output from script 00b

# Load metabolic rate data sets ----
# RMR:
RMR_all <- read.csv("data/MRdata/Gravel_all_RMR_ms.csv", header = TRUE, fileEncoding='latin1')  %>% as.data.frame()
RMR_all$ScientificName <- as.factor(RMR_all$ScientificName)
# MMR:
MMR_all <- read.csv("data/MRdata/Gravel_all_MMR_ms.csv", header=TRUE, fileEncoding = 'latin1') %>% as.data.frame()
MMR_all$ScientificName <- as.factor(MMR_all$ScientificName)
# AS:
AS_all <- read.csv("data/MRdata/Gravel_all_AS_ms.csv", header=TRUE, fileEncoding = "latin1") %>% as.data.frame()
AS_all$ScientificName <- as.factor(AS_all$ScientificName)

# Load rmax (+ LHTs) ----
# (output from file 00b)
#fishRmax <- read.csv("data/LHTdata/all_fish_rmax.csv", header = TRUE, fileEncoding="UTF-8-BOM") %>%  as.data.frame()

# Load ecological lifestyles ----
# For all 84 species
lifestyles <- read.csv("data/lifestyles_84species_ms.csv",
                       header = TRUE, fileEncoding="UTF-8-BOM") %>% as.data.frame()
lifestyles$Lifestyle <- as.factor(lifestyles$Lifestyle)

# Data consolidation function ----
consolidateDF_func <- function(MR_DF=NULL, MRtype=NULL, Rmax_DF=fishRmax, LS_DF=lifestyles) { #MRtype =NULL, MRcriterion=NULL
  # Merge rmax & LHT, MR data, and ecological lifestyles
  Rmax_DF <- Rmax_DF %>%
    dplyr::select("ScientificNamePopID", "Amat", "Amax", "ReproOutput" ,"Rmax") 
  fullData <- merge(merge(MR_DF, LS_DF, by= "ScientificName"), 
                    Rmax_DF, by = "ScientificNamePopID")
  # Rename cols
  fullData_renamed <- fullData %>%
    # Keep only desired cols
    dplyr::select("Family", "ScientificName", "ScientificNamePopID", 
                  "CommonName", "TeleostOrElasmo", "Lifestyle",
                  "MROriginLocation", "RawMetabolicRate", "MRRawDataOrMean", 
                  "SampleSize", "RawMetabolicRateUnits", "RawMetabolicRateCitation", 
                  "BodyMass", "BodyMassUnits", "Temperature",  
                  "Amat", "Amax", "ReproOutput", "Rmax")
  
  # Return rmax-LHT-MR dataframe:
  return(fullData_renamed)
  
  # To save the individual dataframes
  # write.csv(fullData_renamed, file = paste("data/MRdata/full_Rmax_",  MRtype, ".csv", sep =""), row.names=FALSE)
  
}

# RMR-rmax DF ----
DF_rmaxRMR <- consolidateDF_func(MR_DF = RMR_all, MRtype = "RMR", Rmax_DF = fishRmax, LS_DF = lifestyles)
# MMR-rmax DF ----
DF_rmaxMMR <- consolidateDF_func(MR_DF = MMR_all, MRtype = "MMR", Rmax_DF = fishRmax, LS_DF = lifestyles)
# AS-rmax DF ----
DF_rmaxAS <- consolidateDF_func(MR_DF = AS_all, MRtype = "AS", Rmax_DF = fishRmax, LS_DF = lifestyles)


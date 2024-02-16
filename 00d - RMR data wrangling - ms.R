# Sarah Gravel
# 00 Data pre-processing

# Processing RMR data
# Creating population-specific RMR species means DF

# Note: requires packages in script 01a and output from script 00c 

# Load RMR DF ----
# DF_rmaxRMR <- read.csv("data/MRdata/full_Rmax_RMR.csv", header = TRUE, fileEncoding = 'UTF-8') %>% as.data.frame()

# MR units conversion into watts (Jl/s) ----
DF_rmaxRMR$RawMetabolicRateUnits <- as.factor(DF_rmaxRMR$RawMetabolicRateUnits)
levels(DF_rmaxRMR$RawMetabolicRateUnits)
DF_rmaxRMR$RawMetabolicRateUnits <- fct_collapse(as.factor(DF_rmaxRMR$RawMetabolicRateUnits), "mg O2 kg^-0.67 h^-1" = c("mg O2 kg^-0.67 h^-1"), "mg O2 g^-0.7 h^-1" = c("mg O2 g^-0.7 h^-1"), "mg O2 kg^-0.718 h^-1" = c("mg O2 kg^-0.718 h^-1"), "mg O2 kg^-0.8 h^-1" = c("mg O2 kg^-0.8 h^-1"), "mg O2 kg^-0.86 h^-1" = c("mg O2 kg^-0.86 h^-1"), "mg O2 kg^-1 h^-1" = c("mg O2 kg^-1 h^-1 ","mg O2 kg^-1 h^-1"), "mg O2 g^-1 h^-1" = c("mg O2 g^-1 h^-1"), "mg O2 h^-1" = c("mg O2 h^-1", "mg O2/h", "mg/h"), "mg O2 kg^-1 min^-1" = c("mg O2 kg^-1 min^-1", "mg O2 min^-1 kg^-1"), "mg O2 min^-1" = c("mg O2 min^-1"), "ml O2 h^-1" = c("ml O2 h^-1", "ml O2/h"), "ml O2 kg^-1 h^-1" = c("cc O2 kg^-1 h^-1", "ml kg^-1 h^-1", "ml O2 kg^-1 h^-1"), "mmol O2 kg^-1 h^-1" = c("mmol O2 kg^-1 h^-1"), "ul O2 g^-1 h^-1" = c("ul O2 g^-1 h^-1","uL O2 g^-1 h^-1"), "ul O2 h^-1" = c("ul/hr"), "umol O2 g^-1 h^-1" = c("umol O2 g^-1 h^-1") ,"umol O2 kg^-1 min^-1" = c("umol O2 kg^-1 min^-1"), "umol O2 min^-1" = c("umol O2 min^-1", "umol O2/min"), "J h^-1" = c("J/h")) # Fix inconsistencies of unit naming
DF_rmaxRMR$WholeOrganismRMRWatts <- NA # Add new column for MR in watts (J/s) 

# Function to turn mass-adjusted (aka "mass-corrected") MRs into whole org MR:
reverse_adjust <- function (b, adjustedMR, M){
  wholeOrg <- adjustedMR*(M^b) # in units O2/ time units^-1
  return(wholeOrg)
}

# Convert all mass units to grams:
DF_rmaxRMR$BodyMassUnits <- as.factor(DF_rmaxRMR$BodyMassUnits)
levels(DF_rmaxRMR$BodyMassUnits)
for(i in 1:length(DF_rmaxRMR$ScientificName)) {
  if (DF_rmaxRMR$BodyMassUnits[i] == "kg"){
    DF_rmaxRMR$BodyMass[i] <- DF_rmaxRMR$BodyMass[i]*1000
    DF_rmaxRMR$BodyMassUnits[i] <- "g"
  }
}
DF_rmaxRMR$BodyMassUnits <- fct_collapse(DF_rmaxRMR$BodyMassUnits, g = c("g", "kg"))

# Convert MR to Watts:
# If in MR in /kg, modify WT as appropriate (all body mass are now in g)
levels(DF_rmaxRMR$RawMetabolicRateUnits)
DF_rmaxRMR$RawMetabolicRateUnits <- as.factor(DF_rmaxRMR$RawMetabolicRateUnits) 
for(i in 1:length(DF_rmaxRMR$ScientificName)){
  
  rawMR <- DF_rmaxRMR$RawMetabolicRate[i] 
  units <- DF_rmaxRMR$RawMetabolicRateUnits[i]
  WT <- DF_rmaxRMR$BodyMass[i]
  
  if (units == "J h^-1") {
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- rawMR/3600
  }
  if (units == "cal g^-1 day^-1"){
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*WT*4.184)/86400 
    # 1 kcal (or 1000 cal) = 4184 J
    # 86400 s / day
  }
  if (units == "mg O2 g^-0.67 h^-1") {
    wholeMR <- reverse_adjust(0.67,rawMR,WT) 
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 g^-0.7 h^-1") {
    # NOTE this one is in grams, so WT should be too
    wholeMR <- reverse_adjust(0.7,rawMR,WT)
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 g^-1 h^-1") {
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*WT*14.1)/3600
  }
  if (units == "mg O2 kg^-0.67 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.67,rawMR,WT) 
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.718 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.718,rawMR,WT)
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.74 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.74,rawMR,WT)
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.79 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.79,rawMR,WT)
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.8 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.8,rawMR,WT)
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.82 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.82,rawMR,WT)
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.84 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.84,rawMR,WT)
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.86 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.86,rawMR,WT)
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.89 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.89,rawMR,WT)
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  # For S. acanthias:
  if (units == "mg O2 kg^-0.81 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.81,rawMR,WT)
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.75 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.75,rawMR,WT)
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.46 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.46,rawMR,WT)
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.43 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.43,rawMR,WT)
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-1 h^-1") {
    WT <- WT/1000 # to kg
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*WT*14.1)/3600
  }
  if (units == "mg O2 kg^-1 min^-1") {
    WT <- WT/1000 # to kg
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*WT*14.1)/60
  }
  if (units == "g O2 day^-1"){
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*1000*14.1)/86400 
    # from g to mg O2 (*1000)
  }
  if (units == "mg O2 h^-1") {
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*14.1)/3600
  }
  if (units == "mg O2 min^-1") {
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*14.1)/60
  }
  if (units == "ml O2 min^-1") {
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*20.1)/60
  }
  if (units == "ml O2 h^-1") {
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*20.1)/3600
  }
  if (units == "ml O2 g^-1 h^-1") {
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*WT*20.1)/3600
  }
  if (units == "ml O2 kg^-1 h^-1") {
    WT <- WT/1000 # to kg
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*WT*20.1)/3600
  }
  if (units == "ml O2 kg^-1 min^-1") {
    WT <- WT/1000 # to kg
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*WT*20.1)/60
  }
  if (units == "mmol O2 kg^-1 h^-1") {
    WT <- WT/1000 # to kg
    # 1 mmol O2 = 22.3916 ml O2
    rawMR <- rawMR*22.3916 # to get it in ml
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*WT*20.1)/3600
  }
  if (units == "ul O2 g^-1 h^-1") {
    # 1 ul O2 = 0.001 ml O2
    rawMR <- rawMR*0.001 # to get it in ml
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*WT*20.1)/3600
  }
  if (units == "ul O2 min^-1") {
    # 1 ul O2 = 0.001 ml O2
    rawMR <- rawMR*0.001 # to get it in ml
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*20.1)/60
  }
  if (units == "ul O2 h^-1") {
    rawMR <- rawMR*0.001
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*20.1)/3600
  }
  if (units == "umol O2 h^-1") {
    # 1 mmol = 1000 umol
    # 1 umol O2 = 0.0223916 ml O2
    rawMR <- rawMR*0.0223916 # to get it in ml
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*20.1)/3600
  }
  if (units == "umol O2 min^-1") {
    # 1 mmol = 1000 umol
    # 1 umol O2 = 0.0223916 ml O2
    rawMR <- rawMR*0.0223916 # to get it in ml
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*20.1)/60
  }
  if (units == "umol O2 g^-1 h^-1") {
    # 1 mmol = 1000 umol
    # 1 umol O2 = 0.0223916 ml O2
    rawMR <- rawMR*0.0223916 # to get it in ml
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*WT*20.1)/3600
  }
  if (units == "umol O2 g^-1 min^-1") {
    # 1 mmol = 1000 umol
    # 1 umol O2 = 0.0223916 ml O2
    rawMR <- rawMR*0.0223916 # to get it in ml
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*WT*20.1)/60
  }
  if (units == "umol O2 kg^-1 min^-1") {
    WT <- WT/1000 # to kg
    # 1 mmol = 1000 umol
    # 1 umol O2 = 0.0223916 ml O2
    rawMR <- rawMR*0.0223916 # to get it in ml
    DF_rmaxRMR$WholeOrganismRMRWatts[i] <- (rawMR*WT*20.1)/60
  }
  
}
# Make sure all converted:
which(is.na(DF_rmaxRMR$WholeOrganismRMRWatts))

# Partiallly endothermic tuna/lamnid temperature correction
DF_rmaxRMR_corr <- DF_rmaxRMR %>%
  dplyr::filter(grepl("Thunnus", ScientificName) | 
                  ScientificName=="Katsuwonus pelamis" | 
                  ScientificName=="Euthynnus affinis" |
                  Family == "Lamnidae") %>%
  mutate(Temperature = Temperature + 3.5) %>%
  bind_rows(DF_rmaxRMR %>%
              dplyr::filter(is.na(Family) | 
                              (!grepl("Thunnus", ScientificName) &  
                                 ScientificName != "Katsuwonus pelamis" &
                                 ScientificName != "Euthynnus affinis" &
                                 Family != "Lamnidae")))

# Mean RMRs (species means) ----

# By biggest sample size N: 
# Select study/temperature group with biggest sample size
# If same N, choose largest body size
# If same body size, choose temperature closest to mean of dataset
# If all above are the same, select lowest MR measurement

# By temperature closest to 15C: 
# Select study/temperature group measured at temp closest to mean of dataset
# If same temp, choose largest body size
# If same body size, choose largest sample size
# If all above are the same, select lowest MR measurement

# Split into raw and mean MR data:
RMR_split <- split(DF_rmaxRMR_corr, 
                       DF_rmaxRMR_corr$MRRawDataOrMean)
raw_RMR <- RMR_split$Raw
mean_RMR <- RMR_split$Mean

# To ID max of a list (or if all NAs, return NA)
my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)

# RAW DATA:

# Group by (binned) temperature:
raw_RMRbyT <- raw_RMR %>%
  group_by(ScientificName, 
           RawMetabolicRateCitation, MROriginLocation, 
           Temperature) %>% 
  mutate_at(c("SampleSize"), sum) %>% 
  mutate_if(is.numeric, mean) %>% 
  dplyr::slice(1)%>%
  ungroup()

# 1) Largest N
rawRMRbigN <- raw_RMRbyT %>% 
  group_by(ScientificName) %>% 
  filter(SampleSize ==(my.max(SampleSize))|is.na(my.max(SampleSize))) %>%
  filter(BodyMass==max(BodyMass)) %>%
  filter(abs(Temperature - 15) == min(abs(Temperature - 15)))

# 2) Temp closest to 15 
rawRMRmeanT <- raw_RMRbyT %>%
  group_by(ScientificName) %>%
  filter(abs(Temperature - 15) == min(abs(Temperature - 15))) %>%
  filter(BodyMass==max(BodyMass))

# MEAN DATA:

# 1) Largest N
meanRMRbigN <- mean_RMR %>%
  group_by(ScientificName) %>% 
  filter(SampleSize ==(my.max(SampleSize))|is.na(my.max(SampleSize))) %>%
  filter(BodyMass==max(BodyMass)) %>%
  filter(abs(Temperature - 15) == min(abs(Temperature - 15))) %>%
  filter(RawMetabolicRate==min(RawMetabolicRate)) # for this one species which has same sample size, body mass and temperature, but two different MR groups
# Chose lowest

# 2) Temp closest to 15 
meanRMRmeanT <- mean_RMR %>% 
  group_by(ScientificName) %>% 
  filter(abs(Temperature - 15) == min(abs(Temperature - 15))) %>%
  filter(BodyMass==max(BodyMass)) %>%
  filter(SampleSize ==(my.max(SampleSize))|is.na(my.max(SampleSize))) %>%
  filter(RawMetabolicRate==min(RawMetabolicRate)) # again, just for that one species

# Combining raw and mean dataset means:

# Full biggest N dataset
RMR_MeanAll_BigN <- rbind(rawRMRbigN, meanRMRbigN)
RMR_MeanAll_BigN <- RMR_MeanAll_BigN %>% 
  group_by(ScientificName) %>%
  filter(SampleSize ==(my.max(SampleSize))|is.na(my.max(SampleSize))) %>%
  filter(BodyMass==max(BodyMass)) %>%
  filter(abs(Temperature - 15) == min(abs(Temperature - 15))) 
length(RMR_MeanAll_BigN$ScientificName)
length(unique(RMR_MeanAll_BigN$ScientificName))

# Rename cols
DF_rmaxRMRbigN <- RMR_MeanAll_BigN %>% as.data.frame() %>%
  dplyr::select("Family", "ScientificName", "CommonName", 
                "TeleostOrElasmo", "Lifestyle", 
                "MROriginLocation", "SampleSize", 
                "RawMetabolicRateCitation", 
                "BodyMass", "Temperature", 
                "WholeOrganismRMRWatts", 
                "Amat", "Amax", "ReproOutput", "Rmax") %>%
  setNames(c("Family", "ScientificName", "CommonName", 
             "TeleostOrElasmo", "Lifestyle",
             "MROriginLocation", "MRSampleSize", 
             "MRCitation",
             "MRMassGrams", "MRTempCelcius", 
             "WholeOrganismMRWatts", 
             "Amat", "Amax", "ReproOutput" , "Rmax"))
# write.csv(DF_rmaxRMRbigN, file = "data/MRdata/meanDF_rmaxRMRbigN.csv", row.names=FALSE, fileEncoding = 'UTF-8')

# Full temp closest to 15 dataset
RMR_MeanAll_MeanT <- rbind(rawRMRmeanT, meanRMRmeanT)
RMR_MeanAll_MeanT <- RMR_MeanAll_MeanT %>% 
  group_by(ScientificName) %>% 
  filter(abs(Temperature - 15) == min(abs(Temperature - 15))) %>%
  filter(BodyMass==max(BodyMass)) %>%
  filter(SampleSize ==(my.max(SampleSize))|is.na(my.max(SampleSize)))
length(RMR_MeanAll_MeanT$ScientificName)
length(unique(RMR_MeanAll_MeanT$ScientificName))
# Rename cols
DF_rmaxRMRmeanT <- RMR_MeanAll_MeanT %>% as.data.frame() %>%
  dplyr::select("Family", "ScientificName", "CommonName", 
                "TeleostOrElasmo", "Lifestyle", 
                "MROriginLocation", "SampleSize", 
                "RawMetabolicRateCitation", 
                "BodyMass", "Temperature", 
                "WholeOrganismRMRWatts", 
                "Amat", "Amax", "ReproOutput", "Rmax") %>%
  setNames(c("Family", "ScientificName", "CommonName", 
             "TeleostOrElasmo", "Lifestyle",
             "MROriginLocation", "MRSampleSize", 
             "MRCitation",
             "MRMassGrams", "MRTempCelcius", 
             "WholeOrganismMRWatts", 
             "Amat", "Amax", "ReproOutput" , "Rmax"))
# write.csv(DF_rmaxRMRmeanT, file = "data/MRdata/meanDF_rmaxRMRmeanT.csv", row.names=FALSE, fileEncoding = 'UTF-8')

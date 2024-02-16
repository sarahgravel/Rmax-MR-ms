# Sarah Gravel
# 00 Data pre-processing

# Processing AS data
# Creating population-specific AS species means DF

# Note: requires packages in script 01a and output from script 00c 

# Load AS DF ----
# DF_rmaxAS <- read.csv("data/MRdata/full_Rmax_AS.csv", header = TRUE, fileEncoding = 'UTF-8') %>% as.data.frame()

# MR units conversion into watts (Jl/s) ----
DF_rmaxAS$RawMetabolicRateUnits<- as.factor(DF_rmaxAS$RawMetabolicRateUnits)
levels(DF_rmaxAS$RawMetabolicRateUnits)
DF_rmaxAS$RawMetabolicRateUnits <- fct_collapse(as.factor(DF_rmaxAS$RawMetabolicRateUnits), "mg O2 kg^-1 h^-1" = c("mg O2 kg^-1 h^-1 ","mg O2 kg^-1 h^-1"))
DF_rmaxAS$WholeOrganismAASWatts <- NA # Add new column for MR in watts (J/s) 

# Function to turn mass-adjusted MRs into whole org MR:
reverse_adjust <- function (b, adjustedMR, M){
  wholeOrg <- adjustedMR*(M^b) # in units O2/ time units^-1
  return(wholeOrg)
}

# Convert all mass units to grams:
levels(as.factor(DF_rmaxAS$BodyMassUnits))
for(i in 1:length(DF_rmaxAS$ScientificName)) {
  if (DF_rmaxAS$BodyMassUnits[i] == "kg"){
    DF_rmaxAS$BodyMass[i] <- DF_rmaxAS$BodyMass[i]*1000
    DF_rmaxAS$BodyMassUnits[i] <- "g"
  }
}

# Convert MR to Watts:
levels(DF_rmaxAS$RawMetabolicRateUnits)
DF_rmaxAS$RawMetabolicRateUnits <- as.factor(DF_rmaxAS$RawMetabolicRateUnits) 
for(i in 1:length(DF_rmaxAS$ScientificName)){
  
  rawMR <- DF_rmaxAS$RawMetabolicRate[i] 
  units <- DF_rmaxAS$RawMetabolicRateUnits[i]
  WT <- DF_rmaxAS$BodyMass[i]
  
  if (units == "mg O2 g^-1 h^-1") { 
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (rawMR*WT*14.1)/3600
  }
  if (units == "mg O2 g^-0.67 h^-1") {
    wholeMR <- reverse_adjust(0.67,rawMR,WT) 
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "cal g^-1 day^-1"){
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (rawMR*WT*4.184)/86400 
    # 1 kcal (or 1000 cal) = 4184 J
    # 86400 s / day
  }
  if (units == "mg O2 kg^-0.718 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.718,rawMR,WT)
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.8 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.8,rawMR,WT)
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.81 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.81,rawMR,WT)
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.82 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.82,rawMR,WT)
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.84 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.84,rawMR,WT)
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.86 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.86,rawMR,WT)
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-0.89 h^-1") {
    WT <- WT/1000 # to kg
    wholeMR <- reverse_adjust(0.89,rawMR,WT)
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (wholeMR*14.1)/3600
  }
  if (units == "mg O2 kg^-1 h^-1") {
    WT <- WT/1000 # to kg
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (rawMR*WT*14.1)/3600
  }
  if (units == "mg O2 kg^-1 min^-1") {
    WT <- WT/1000 # to kg
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (rawMR*WT*14.1)/60
  }
  if (units == "g O2 day^-1"){
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (rawMR*1000*14.1)/86400 
    # from g to mg O2 (*1000)
  }
  if (units == "mg O2 h^-1") {
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (rawMR*14.1)/3600
  }
  if (units == "mg O2 min^-1") {
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (rawMR*14.1)/60
  }
  if (units == "mmol O2 kg^-1 h^-1") {
    WT <- WT/1000 # to kg
    # 1 mmol O2 = 22.3916 ml O2
    rawMR <- rawMR*22.3916 # to get it in ml
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (rawMR*WT*20.1)/3600
  }
  # if (units == "ml O2 h^-1") {
  #   df$WholeOrganismAASWatts[i] <- (rawMR*20.1)/3600
  # }
  if (units == "ul O2 mg^-1 h^-1") {
    # 1 ul O2 = 0.001 ml O2
    WT <- WT*1000 # to mg
    rawMR <- rawMR*0.001 # to get it in ml
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (rawMR*WT*20.1)/3600
  }
  if (units == "umol O2 kg^-1 min^-1") {
    WT <- WT/1000 # to kg
    # 1 mmol = 1000 umol
    # 1 umol O2 = 0.0223916 ml O2
    rawMR <- rawMR*0.0223916 # to get it in ml
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (rawMR*WT*20.1)/60
  }
  if (units == "umol O2 g^-1 h^-1") {
    # 1 mmol = 1000 umol
    # 1 umol O2 = 0.0223916 ml O2
    rawMR <- rawMR*0.0223916 # to get it in ml
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (rawMR*WT*20.1)/3600
  }
  if (units == "umol O2 g^-1 min^-1") {
    # 1 mmol = 1000 umol
    # 1 umol O2 = 0.0223916 ml O2
    rawMR <- rawMR*0.0223916 # to get it in ml
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (rawMR*WT*20.1)/60
  }
  if (units == "umol O2 min^-1") {
    # 1 mmol = 1000 umol
    # 1 umol O2 = 0.0223916 ml O2
    rawMR <- rawMR*0.0223916 # to get it in ml
    DF_rmaxAS$WholeOrganismAASWatts[i] <- (rawMR*20.1)/60
  }
  
}
which(is.na(DF_rmaxAS$WholeOrganismAASWatts)) 

# Couple edits to the whole organism AAS of S. acanthias :
# Used different scaling exponents for RMR and MMR, which also differed with temperature (Table A1 Andres 2022 thesis)
DF_rmaxAS[DF_rmaxAS$ScientificName == "Squalus acanthias" & DF_rmaxAS$Temperature == 10, c("WholeOrganismAASWatts", "FactorialAerobicScope")] <- c(0.6967367-0.2137931,0.6967367/0.2137931)
DF_rmaxAS[DF_rmaxAS$ScientificName == "Squalus acanthias" & DF_rmaxAS$Temperature == 13, c("WholeOrganismAASWatts", "FactorialAerobicScope")] <- c(1.0160744-0.3220440,1.0160744/0.3220440)
DF_rmaxAS[DF_rmaxAS$ScientificName == "Squalus acanthias" & DF_rmaxAS$Temperature == 17, c("WholeOrganismAASWatts", "FactorialAerobicScope")] <- c(0.9662017-0.3579000,0.9662017/0.3579000)
DF_rmaxAS[DF_rmaxAS$ScientificName == "Squalus acanthias" & DF_rmaxAS$Temperature == 21, c("WholeOrganismAASWatts", "FactorialAerobicScope")] <- c(1.3802544-0.3840489,1.3802544/0.3840489)
DF_rmaxAS[DF_rmaxAS$ScientificName == "Squalus acanthias" & DF_rmaxAS$Temperature == 23, c("WholeOrganismAASWatts", "FactorialAerobicScope")] <- c(1.3913783-0.4399522,1.3913783/0.4399522)
# and Pagrus auratus (Nelson NZ):
DF_rmaxAS[DF_rmaxAS$ScientificName == "Pagrus auratus" & DF_rmaxAS$Temperature == 17, c("WholeOrganismAASWatts", "FactorialAerobicScope")] <- c(0.2317288-0.09315200,0.2317288/0.09315200)

# Temperature correction (partial endotherms) ----
DF_rmaxAS_corr <- DF_rmaxAS %>%
  dplyr::filter(grepl("Thunnus", ScientificName) | 
                  ScientificName=="Katsuwonus pelamis" | 
                  ScientificName=="Euthynnus affinis" |
                  Family == "Lamnidae") %>%
  mutate(Temperature = Temperature + 3.5) %>%
  bind_rows(DF_rmaxAS %>%
              dplyr::filter(is.na(Family) | 
                              (!grepl("Thunnus", ScientificName) &  
                                 ScientificName != "Katsuwonus pelamis" &
                                 ScientificName != "Euthynnus affinis" &
                                 Family != "Lamnidae")))

# Mean ASs (species means) ----

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
AS_split <- split(DF_rmaxAS_corr, 
                  DF_rmaxAS_corr$MRRawDataOrMean)
raw_AS <- AS_split$Raw
mean_AS <- AS_split$Mean

# To ID max of a list (or if all NAs, return NA)
my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)

# RAW DATA:

# Group by (binned) temperature:
raw_ASbyT <- raw_AS %>% 
  group_by(ScientificName, 
           RawMetabolicRateCitation, MROriginLocation, 
           Temperature) %>% 
  mutate_at(c("SampleSize"), sum) %>% 
  mutate_if(is.numeric, mean) %>% 
  dplyr::slice(1)%>%
  ungroup()

# 1) Big N
rawASbigN <- raw_ASbyT %>% 
  group_by(ScientificName) %>% 
  filter(SampleSize ==(my.max(SampleSize))|is.na(my.max(SampleSize))) %>%
  filter(BodyMass==max(BodyMass)) %>%
  filter(abs(Temperature - 15) == min(abs(Temperature - 15))) # for T. albacares

# 2) Temp closest to 15 
rawASmeanT <- raw_ASbyT %>%
  group_by(ScientificName) %>%
  filter(abs(Temperature - 15) == min(abs(Temperature - 15))) %>%
  filter(BodyMass==max(BodyMass))

# MEAN DATA:

# 1) Largest N
meanASbigN <- mean_AS %>%
  group_by(ScientificName) %>% 
  filter(SampleSize ==(my.max(SampleSize))|is.na(my.max(SampleSize))) %>%
  filter(BodyMass==max(BodyMass)) %>%
  filter(abs(Temperature - 15) == min(abs(Temperature - 15))) %>%
  filter(RawMetabolicRate==max(RawMetabolicRate)) # for this one species which has same sample size, body mass and temperature, but two different MR groups
# Chose lowest

# 2) Temp closest to 15 
meanASmeanT <- mean_AS %>% 
  group_by(ScientificName) %>% 
  filter(abs(Temperature - 15) == min(abs(Temperature - 15))) %>%
  filter(BodyMass==max(BodyMass)) %>%
  filter(SampleSize ==(my.max(SampleSize))|is.na(my.max(SampleSize))) %>%
  filter(RawMetabolicRate==max(RawMetabolicRate)) 


# Combining raw and mean dataset means:

# Full biggest N dataset
AS_MeanAll_BigN <- rbind(rawASbigN, meanASbigN)
AS_MeanAll_BigN <- AS_MeanAll_BigN %>% 
  group_by(ScientificName) %>%
  filter(SampleSize ==(my.max(SampleSize))|is.na(my.max(SampleSize))) %>%
  filter(BodyMass==max(BodyMass))
length(AS_MeanAll_BigN$ScientificName)
length(unique(AS_MeanAll_BigN$ScientificName))
# Rename cols
DF_rmaxASbigN <- AS_MeanAll_BigN %>% as.data.frame() %>%
  dplyr::select("Family", "ScientificName", "CommonName", 
                "TeleostOrElasmo", "Lifestyle", 
                "MROriginLocation", "SampleSize", 
                "RawMetabolicRateCitation", 
                "BodyMass", "Temperature", 
                "WholeOrganismAASWatts", 
                "Amat", "Amax", "ReproOutput", "Rmax") %>%
  setNames(c("Family", "ScientificName", "CommonName", 
             "TeleostOrElasmo", "Lifestyle",
             "MROriginLocation", "MRSampleSize", 
             "MRCitation",
             "MRMassGrams", "MRTempCelcius", 
             "WholeOrganismMRWatts", 
             "Amat", "Amax", "ReproOutput" , "Rmax"))
# write.csv(DF_rmaxASbigN, file = "data/MRdata/meanDF_rmaxASbigN.csv", row.names=FALSE, fileEncoding = 'UTF-8')

# Full temp closest to 15 dataset
AS_MeanAll_MeanT <- rbind(rawASmeanT, meanASmeanT)
AS_MeanAll_MeanT <- AS_MeanAll_MeanT %>% 
  group_by(ScientificName) %>% 
  filter(abs(Temperature - 15) == min(abs(Temperature - 15))) %>%
  filter(BodyMass==max(BodyMass))
length(AS_MeanAll_MeanT$ScientificName)
length(unique(AS_MeanAll_MeanT$ScientificName))
# Rename cols
DF_rmaxASmeanT <- AS_MeanAll_MeanT %>% as.data.frame() %>%
  dplyr::select("Family", "ScientificName", "CommonName", 
                "TeleostOrElasmo", "Lifestyle", 
                "MROriginLocation", "SampleSize", 
                "RawMetabolicRateCitation", 
                "BodyMass", "Temperature", 
                "WholeOrganismAASWatts", 
                "Amat", "Amax", "ReproOutput", "Rmax") %>%
  setNames(c("Family", "ScientificName", "CommonName", 
             "TeleostOrElasmo", "Lifestyle",
             "MROriginLocation", "MRSampleSize", 
             "MRCitation",
             "MRMassGrams", "MRTempCelcius", 
             "WholeOrganismMRWatts", 
             "Amat", "Amax", "ReproOutput" , "Rmax"))
# write.csv(DF_rmaxASmeanT, file = "data/MRdata/meanDF_rmaxASmeanT.csv", row.names=FALSE, fileEncoding = 'UTF-8')

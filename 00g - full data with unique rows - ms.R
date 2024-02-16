# Sarah Gravel
# 00 Data pre-processing

## Creates final FULL dataset with unique MR rows ##

DF_rmaxRMRbigN$DFtype <- NA
DF_rmaxMMRbigN$DFtype <- NA
DF_rmaxASbigN$DFtype <- NA
DF_rmaxRMRmeanT$DFtype <- NA
DF_rmaxMMRmeanT$DFtype <- NA
DF_rmaxASmeanT$DFtype <- NA

# To compare identical rows, order by spp name & update row names to spp name
DF_rmaxRMRbigN <- DF_rmaxRMRbigN[order(DF_rmaxRMRbigN$ScientificName),] 
DF_rmaxRMRmeanT <- DF_rmaxRMRmeanT[order(DF_rmaxRMRmeanT$ScientificName),] 
DF_rmaxMMRbigN <- DF_rmaxMMRbigN[order(DF_rmaxMMRbigN$ScientificName),] 
DF_rmaxMMRmeanT <- DF_rmaxMMRmeanT[order(DF_rmaxMMRmeanT$ScientificName),] 
DF_rmaxASbigN <- DF_rmaxASbigN[order(DF_rmaxASbigN$ScientificName),] 
DF_rmaxASmeanT <- DF_rmaxASmeanT[order(DF_rmaxASmeanT$ScientificName),] 

rownames(DF_rmaxRMRbigN) <- DF_rmaxRMRbigN$ScientificName
rownames(DF_rmaxRMRmeanT) <- DF_rmaxRMRmeanT$ScientificName
rownames(DF_rmaxMMRbigN) <- DF_rmaxMMRbigN$ScientificName
rownames(DF_rmaxMMRmeanT) <- DF_rmaxMMRmeanT$ScientificName
rownames(DF_rmaxASbigN) <- DF_rmaxASbigN$ScientificName
rownames(DF_rmaxASmeanT) <- DF_rmaxASmeanT$ScientificName

# Combine Temperature and Sample Size DFs into single FULL DF: ----
# RMR:
uniqueRMRrows <- data.frame() # empty dataframe
k <- 1 # counter var
for (i in 1:length(DF_rmaxRMRbigN$ScientificName)) {
  x<-DF_rmaxRMRbigN
  y<-DF_rmaxRMRmeanT
  if (identical(x[i,], y[i,])) { 
    uniqueRMRrows <- rbind(uniqueRMRrows, x[i,]) # if both rows identical, adds 1 row
    uniqueRMRrows[k,c("DFtype")] <- 'Both' # updates DFtype to indicate the data for that species was the same for both BigN and MeanT data set versions
    k <- k+1
  }
  else {
    x[i,c("DFtype")] <- 'BigN'
    y[i,c("DFtype")] <- 'MeanT'
    uniqueRMRrows <- rbind(uniqueRMRrows, x[i,], y[i,]) # adds both unique rows
    k <- k+2
  }
}

# MMR:
uniqueMMRrows <- data.frame() # empty dataframe
k <- 1 # counter var
for (i in 1:length(DF_rmaxMMRbigN$ScientificName)) {
  x<-DF_rmaxMMRbigN
  y<-DF_rmaxMMRmeanT
  if (identical(x[i,], y[i,])) {
    uniqueMMRrows <- rbind(uniqueMMRrows, x[i,]) # if both rows identical, adds 1 row
    uniqueMMRrows[k,c("DFtype")] <- 'Both' # updates DFtype to indicate the data for that species was the same for both BigN and MeanT data set versions
    k <- k+1
  }
  else {
    x[i,c("DFtype")] <- 'BigN'
    y[i,c("DFtype")] <- 'MeanT'
    uniqueMMRrows <- rbind(uniqueMMRrows, x[i,], y[i,]) # adds both unique rows
    k <- k+2
  }
}

# AS:
uniqueASrows <- data.frame() 
k <- 1 
for (i in 1:length(DF_rmaxASbigN$ScientificName)) {
  x<-DF_rmaxASbigN
  y<-DF_rmaxASmeanT
  if (identical(x[i,], y[i,])) {
    uniqueASrows <- rbind(uniqueASrows, x[i,]) # if both rows identical, adds 1 row
    uniqueASrows[k,c("DFtype")] <- 'Both' # updates DFtype to indicate the data for that species was the same for both BigN and MeanT data set versions
    k <- k+1
  }
  else {
    x[i,c("DFtype")] <- 'BigN'
    y[i,c("DFtype")] <- 'MeanT'
    uniqueASrows <- rbind(uniqueASrows, x[i,], y[i,]) # adds both unique rows
    k <- k+2
  }
}

uniqueRMRrows$MRtype <- 'RMR'
uniqueMMRrows$MRtype <- 'MMR'
uniqueASrows$MRtype <- 'AS'

# Combine all: ----
DF_FULL <- rbind(uniqueRMRrows, uniqueMMRrows, uniqueASrows)
rownames(DF_FULL) <- NULL
#write.csv(DF_FULL, file = "data/FULL_Rmax_LHT_MR_ms.csv", row.names=FALSE)

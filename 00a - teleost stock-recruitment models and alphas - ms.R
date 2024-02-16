# Sarah Gravel
# 00 Data pre-processing

# Extraction of slope at the origin 'alpha' for calculation of teleost rmax from Ricker stock-recruitment models fit to spawner-recruit timeseries
# Stock data and timeseries from the RAM Database: https://www.ramlegacy.org/database/

# RAM DATA ----
# Load the DBdata.RData file
# Then load the database data into matrix/dataframe files for the assessment only version of the database.
load("data/RAMdata/DBdata[asmt][v4.491].RData")
load("data/RAMdata/RAMdata.rda")

### List of RAM DFs with stock metadata: ###
# -- metadata -- Summarized metadata
# -- stock -- General stock metadata
# -- assessment -- General assessment metadata
# -- taxonomy -- Taxonomic metadata (can search species by family)
# -- management -- Management authority metadata
# -- assessor -- Stock assessor metadata
# -- assessmethod -- Assessment method metadata
# -- area -- Area metadata

### List of RAM DFs with stock timeseries and traits: ###
# -- bioparams -- Full parameter data listing (has stock LHT values)
# -- biometrics -- Parameter data types with descriptions (has stock LHT descriptions)
# -- ssb.data -- Spawning stock biomass data time series
# -- r.data -- Recruits data time series
# -- timeseries_units_views -- Units corresponding to values in timeseries_values_views (has units for R and SSB)

# Finding teleost fish species stocks with spawner and recruit time series data in RAM ----
# Species:
sppDF <- taxonomy %>%
  filter(classname == "Actinopterygii") %>%
  dplyr::select("scientificname", "classname", "family", "commonname1", 
         "commonname2")
# Stocks:
stockDF <- stock %>%
  filter(scientificname %in% sppDF$scientificname)

# Which of these stocks have ssb (spawning stock biomass) and r (recruits) time series data?
TestR <- as.data.frame(r.data$j) 
TestSSB <- as.data.frame(ssb.data$j)
SRspecies_out <- data.frame(ScientificName = NA,
                            CommonName = NA,
                            StockID = NA,
                            AreaID = NA)
k <- 1 # counter variable
# Loop through all stocks to find species with SR data:
for (j in 1:length(stockDF$stockid)) {
  # want to grab r and ssb of the stock ID at row j, and then test if either has 0 rows
  TestR <- as.data.frame(r.data[[stockDF[j,"stockid"]]]) 
  TestSSB <- as.data.frame(ssb.data[[stockDF[j,"stockid"]]])
  TestR
  TestSSB
  if (nrow(TestR) == 0 | nrow(TestSSB) == 0) {
    next
  }
  # Save spp name and stock ID to SRspecies_out
  SRspecies_out[k,] <- c(stockDF[j, c("scientificname","commonname",
                                      "stockid", "areaid")])
  # Update counter
  k <- k+1
}

# Save teleost species with spawner (ssb) and recruits (r) data:
# write.csv(SRspecies_out, file = "data/fisheries_data/output/list_of_teleosts_with_SR_data_in_RAM.csv", row.names = FALSE)

# r:
idx <- match(SRspecies_out$StockID, names(r.data)) # indices of the r.data cols 
Rdat <- r.data[ ,idx] # subset the rows of the cols specified by idx 
colnames(Rdat) <- paste(colnames(Rdat), "r", sep = "_") # concatenate _r to colnames
# ssb:
idx2 <- match(SRspecies_out$StockID, names(ssb.data)) # indices of the ssb.data cols 
SSBdat <- ssb.data[ ,idx2] # subset the rows of the cols specified by idx2
colnames(SSBdat) <- paste(colnames(SSBdat), "ssb", sep = "_") # concatenate _ssb to colnames
SRdata <- cbind(Rdat, SSBdat)
SRdata <- SRdata[, order(names(SRdata))]
# And save their r and ssb timeseries:
# write.csv(SRdata, file = "data/fisheries_data/output/all_teleosts_SR_data_ms.csv", row.names = FALSE)


# * ----

# STOCK-RECRUITMENT MODELS ----

# Code adapted from FishR vignette (Ogle, 2011) for 3rd parameterization of Ricker model (density-dependent) and linear (density-independent) models fit, which were fit to either the full time series (all years) or the 10 years with lowest spawner biomass (lowest 10 ssb years; see our Methods) for each fish stocks for which we had metabolic rate 'MR' data.

# Import my stockIDs for species for which we have MR data ----
# For matching or closely neighbouring (or whichever was available) stocks to the MR population locations, for all teleost species for which we have MR data
stockIDs <- read.csv("data/RAMdata/Gravel_RAM_stock_IDs_ms.csv", header=TRUE, fileEncoding="UTF-8-BOM")

# When REC (recruitment) age was not listed in RAM: ----
# REC age was required for the calculation for SPRfis0 in life tables.
# For certain species, REC age was not listed on RAM for the stock:

# 1) Peruvian anchovetta PANCH stocks: Because age at maturity ~0.5 years -> REC age set to 0;
# 2) Gag GAGSATLC: REC age for GAGGM stock is 0, and SEDAR stock assessment for GAGSATLC report recruits age 0 in catch -> REC age set to 0;
# 3) Salmon stocks: SPRfis0 is not required for the calculation of rmax -> REC age NA;
# 4) Tuna stocks: due to their very large body sizes -> REC age set to 0;
# (Note: however, models were also run without the regional endothermic fishes - our overall findings did not change)
# 5) Greenland and Atlantic halibut stocks (which had one single stock with timeseries data, no alternative stocks): REC age likely 0 (Note: other flatfishes were usually 0 or 1, and these two species were the largest) -> REC age set to 0;
# 6) For the Pacific sardine: SARDPCOAST would've been a better match with the MR data, but was not used due to unsortable units for SPRfis0 calculation. The alternative SARDWSE stock REC age was NA, so the SARDSA stock was used for this species (for which a REC age was reported);
# 7) NZ snapper: The NZSNAPNZ1BOP-HAGU stock REC age = NA, so taking NZSNAPNZ7 for which we also had MR data and LHTs.

# Filter data:
stockIDs <- stockIDs %>%
  filter(Keep == 1)
# NOTE: stocks with R/SSB in relative units or E00/E00eggs, E00/E00larvae, MT/MT were ignored.
length(stockIDs$StockID) # number of stocks: 78
ordered_stockIDs <- stockIDs[order(stockIDs$StockID),] 

# Recruit time series ----
idx <- match(stockIDs$StockID, names(r.data)) # array of indices of the recruit data in r.data
NewDF1 <- r.data[ ,idx] # subset r.data 
colnames(NewDF1) <- paste(colnames(NewDF1), "r", sep = "_") # concatenate _r to colnames

# Spawner time series ----
idx2 <- match(stockIDs$StockID, names(ssb.data)) # array of indices of the stock (spawner) data in ssb.data
NewDF2 <- ssb.data[ ,idx2] # subset ssb.data 
colnames(NewDF2) <- paste(colnames(NewDF2), "ssb", sep = "_") # concatenate _ssb to colnames

# Full stock-recruitment data for all stocks ----
stockdata <- cbind(NewDF1, NewDF2) # combine subsetted r and ssb data
stockdata <- stockdata[, order(names(stockdata))] # alphabetical ordering to have r and sbb pairs side by side
length(stockdata) # 156 (should be double the number of stocks)

# Create a subset of stockdata with lowest 10 SSB years of each stock ----
stockdata_10 <- data.frame(matrix(ncol = 164, nrow = 10)) # empty data frame (dimensions of # stocks x2 by 10 rows i.e. 10 years)
colnames(stockdata_10) <- colnames(stockdata)
idx.low10 <- data.frame(matrix(ncol = 82, nrow = 10)) # corresponding dataframe of indices associated with the lowest 10 years (can later be used to colour these 10 lowest SSB points in each stock plot)
colnames(idx.low10) <- ordered_stockIDs$StockID 

for (i in 1:length(ordered_stockIDs$StockID)){
  # For ith species stock:
  x <- stockdata[2*i] %>% unlist() %>% as.double() %>% as.vector() # ssb column
  y <- stockdata[2*i-1] %>% unlist() %>% as.double() %>% as.vector() # r column
  tog <- na.omit(cbind(y, x)) %>% as.data.frame() 
  colnames(tog) <- c("r", "ssb")
  stockdata_10[,2*i] <- kit::topn(tog$ssb, 10, decreasing = FALSE, hasna=FALSE, index = FALSE) # Find lowest 10 ssb years
  idx.smallest10 <- kit::topn(tog$ssb, 10, decreasing = FALSE, hasna=FALSE, index = TRUE) # and corresponding indices 
  idx.low10[,i] <- idx.smallest10
  stockdata_10[,2*i-1] <- tog$r[idx.smallest10] # and correponding r
}


# All stock-recruit models ----

# Loops through species to,
# 1) fit stock-recruitment model to time series data (all years in timeseries and lowest 10 ssb years), and 
# 2) plot the four types of stock-recruitment models for each stock:
# a. Density-dependent (Ricker) fit to all years in timeseries
# b. Density-independent (linear) fit to all years in timeseries
# c. Density-dependent (Ricker) fit to 10 years with lowest SSB in timeseries
# d. Density-independent (linear) fit to 10 years with lowest SSB in timeseries

# DFs (to be filled in the loop, further below):
# species/stock information, model starting values, model fit values
m3.out <- data.frame(matrix(ncol = 13, nrow = 82)) # empty dataframe for models based on all years 
colnames(m3.out) <- c("ScientificName", "TeleostOrElasmo", "StockID" , "r3s_a", "r3s_Rp", "r3fit_a", "r3fit_Rp", "r3fit_error", "r0fit_a", "r0fit_error", "r3fit_AIC", "r0fit_AIC", "perc_decline_SSB") 
m3.L10.out <- data.frame(matrix(ncol = 12, nrow = 82)) # empty dataframe for models lowest 10 ssb years
colnames(m3.L10.out) <- c("ScientificName", "TeleostOrElasmo", "StockID", "r3s_a", "r3s_Rp", "r3fit_a", "r3fit_Rp", "r3fit_error", "r0fit_a", "r0fit_error", "r3fit_AIC", "r0fit_AIC")
# r3: Ricker model using 3rd parameterization (see FishR vignette), where r3s are starting values and r3fit are model fit values
# r0: density-independent (linear) model fit
# a: alpha, i.e. the slope at the origin of Ricker curve (or the slope of linear model)
# Rp: other Ricker model parameter -- 'peak level of recruitment'

# Note: _error columns will say Y or N if threw an error

graphics.off() # turn graphics off (saving plots to folder)


for (i in 1:length(ordered_stockIDs$StockID)){
  
  r3 <- srFuns(type="Ricker",param=3) # load 3rd param. of Ricker model from FSA package
  
  # For ith stock, ALL YEARS in timeseries:
  tog <- stockdata[c(2*i-1,2*i)] 
  colnames(tog) <- c("r", "ssb")
  tog <- na.omit(tog)
  tog$logR <- log(tog$r)
  tog$colour <- ifelse(seq_len(nrow(tog)) %in% (idx.low10[i]%>% unlist()), "YES", "NO") 
  # ^ assigns y/n if this datapoint is one of the lowest 10 ssb years for this stock
  
  # Automating plot output:
  mypath <- file.path("plots/RAMplots/All_StockRecruit_Models_ms",
                      paste("plot_R3_", ordered_stockIDs[i, 5], ".jpg", sep = ""))
  jpeg(file=mypath)
  mytitle = paste("Stock-recruitment models for ", 
                  ordered_stockIDs[i, 1], ", \nstock ", ordered_stockIDs[i, 5])
  plot(r ~ ssb, data=tog, pch=19, main = mytitle, 
       col = c("black", "red")[as.factor(tog$colour)], 
       xlim = c(0,max(ssb)+10), ylim = c(0,max(r)+1000)) 
  # ^New plot, coloured by whether YES or NO (part of lowest 10 or not, red being YES)
  
  x_max_allyears <- max(tog$ssb, na.rm=TRUE) + 10 # set upper limit of plot x-axis
  
  r3s <- srStarts(r ~ ssb, data=tog, type="Ricker",param=3) # find starting values 
  unlist(r3s)
  m3.out[i,c(8,10)] <- "N"
  
  # Put following model fit code into tryCatch (in case of error)
  tryCatch(
    {
      if (ordered_stockIDs$StockID[i] == "CODIIIaW-IV-VIId") { 
        # For this stock, I want to fit a 
        r3s <- srStarts( r ~ ssb, data=cod, type="Ricker",param=3,plot=TRUE,
                         fixed=list(Rp=1e09,a=6000))
        unlist(r3s)
        r3fit_allyears <- nls(r ~ r3(ssb,a,Rp),data=tog, start=r3s, 
                              algorithm="port", lower=c(0,0)) 
      } else {
        # The density-dependent model is then fit and saved to an object:
        r3fit_allyears <- nls(logR ~ log(r3(ssb,a,Rp)),
                              data=tog, start=r3s, algorithm="port", lower=c(0,0))
        # Note: port and lower arguments are meant to constrain parameters to be positive
      }
    },
    error = function(e){
      print(i)
      print("R3_all")
      print(warning(e))
      m3.out[i,8] <<- "Y" # Y if error
    }
  )
  r0 <- logR~log(a*ssb) # load density independent (linear) model from FSA package
  r0s <- r3s[1] # use same starting value
  tryCatch(
    {
      # The density-independent model is fit and saved to an object:
      r0fit_allyears <- nls(r0,data=tog,start=r0s,algorithm="port",lower=c(0)) 
    },
    error = function(e){
      m3.out[i,10] <<- "Y" # Y if error
    }
  )
  # Save species name, common name, stock ID and r3 model coefs for ith stock:
  m3.out[i,1:3] <- ordered_stockIDs[i,c(1,4,5)] 
  m3.out[i,4:5] <- r3s
  m3.out[i,13] <- round(((max(tog$ssb)-min(tog$ssb))/max(tog$ssb))*100, 2)
  
  if (m3.out[i,8] == "Y"){
    m3.out[i,6:7] <- NA
  } else {
    m3.out[i,6:7] <- coef(r3fit_allyears)
    # Generates a curve for the model (if there's a model, i.e. there was no error)
    curve(r3(x,coef(r3fit_allyears)[1],
             coef(r3fit_allyears)[2]),from=0, to=x_max_allyears,
          col="black",lwd=2,add=TRUE) # black  = r3 (density-dependent) model, all years
  }
  if (m3.out[i,10] == "Y"){
    m3.out[i,9] <- NA
  } else {
    m3.out[i,9] <- coef(r0fit_allyears)
    curve(coef(r0fit_allyears)[1]*x,from=0,to=x_max_allyears,
          col="blue",lwd=2,add=TRUE) # blue = r0 (density-independent) model, all years
  }
  
  # Comparing r3 and r0 model fits (for spp where both models were fit without errors):
  if (m3.out[i,8] == "N" & m3.out[i,10] == "N") {
    aic <- AIC(r3fit_allyears, r0fit_allyears)
    m3.out[i,11:12] <- aic[c(1,2), 2]
  } else {
    m3.out[i,11:12] <- NA
  }
  
  #**********************************************************
  
  # For ith stock, LOWEST 10 SSB YEARS in timeseries:
  
  tog2 <- stockdata_10[c(2*i-1,2*i)]
  colnames(tog2) <- c("r", "ssb")
  tog2 <- na.omit(tog2)
  tog2$logR <- log(tog2$r)
  
  x_max_low10 <- max(tog2$ssb, na.rm=TRUE) + 10 # set upper limit of plot x-axis
  
  r3s <- srStarts(r ~ ssb, data=tog2, type="Ricker",param=3) # find starting values 
  unlist(r3s)
  m3.L10.out[i,c(8,10)] <- "N"
  
  tryCatch(
    { r3fit_low10 <- nls(logR ~ log(r3(ssb,a,Rp)),
                         data=tog2, start=r3s, algorithm="port", lower=c(0,0)) }, error = function(e){
                           m3.L10.out[i,8] <<- "Y" # Y if error
                           # ^NOTE: In here, needs to use <<- instead of <-
                         }
  )
  r0s <- r3s[1] # Use same starting value
  tryCatch(
    {
      # The density-independent model is fit and saved to an object:
      r0fit_low10 <- nls(r0,data=tog2,start=r0s,algorithm="port",lower=c(0))
    },
    error = function(e){
      print(i)
      print("R0_low10")
      print(warning(e))
      m3.L10.out[i,10] <<- "Y" # Y if error
    }
  )
  # Save species name, common name, stock ID and r3 model coefs for ith stock:
  m3.L10.out[i,1:3] <- ordered_stockIDs[i,c(1,4,5)]
  m3.L10.out[i,4:5] <- r3s
  if (m3.L10.out[i,8] == "Y"){
    m3.L10.out[i,6:7] <- NA # If there was an error, we want to make the "fit" coefs NA
  } else {
    m3.L10.out[i,6:7] <- coef(r3fit_low10)
    curve(r3(x,coef(r3fit_low10)[1],coef(r3fit_low10)[2]),from=0,to=x_max_allyears,
          col="red",lwd=2,add=TRUE) # red = r3 (density-dependent) model, lowest 10 ssb years
  }
  
  if (m3.L10.out[i,10] == "Y"){
    m3.L10.out[i,9] <- NA
  } else {
    m3.L10.out[i,9] <- coef(r0fit_low10)
    curve(coef(r0fit_low10)[1]*x,from=0,to=x_max_allyears,
          col="orange",lwd=2,add=TRUE)# orange =r0 (density-independent), lowest 10 ssb years
  }
  # Comparing r3 and r0 model fits (for spp where both models were fit without errors):
  if (m3.L10.out[i,8] == "N" & m3.L10.out[i,10] == "N") {
    aic <- AIC(r3fit_low10, r0fit_low10)
    m3.L10.out[i,11:12] <- aic[c(1,2), 2]
  } else {
    m3.L10.out[i,11:12] <- NA
  }
  # Add legend of 4 curve types/colours:
  legend("topright",legend=c("density independent ALL","density dependent ALL", "density independent LOW10", "density dependent LOW10"),col=c("blue","black","orange","red"), lwd=2,cex=0.6)
  text(x=0.15*max(tog$ssb), y=0.99*max(tog$r), 
       paste("% decline ssb: ", round(((max(tog$ssb)-min(tog$ssb))/max(tog$ssb))*100, 2), 
             "\n alpha yellow LOW10:", scientific(m3.L10.out$r0fit_a[i], 3),
             "\n alpha black ALL:", scientific(m3.out$r3fit_a[i], 3)),
       cex=0.7)
  
  dev.off() 
}

# Note: generally, density dependent (Ricker) models, as opposed to linear models, were better fit to the data / with less errors.

# Create new DF with sp name, stock ID, and r3 model alphas generated from all years or lowest 10 ssb years:
colnames(m3.out) <- paste(colnames(m3.out), "all", sep = "_")
colnames(m3.L10.out) <- paste(colnames(m3.L10.out), "lowest10", sep = "_")
Ricker_alphas <- cbind(m3.out$ScientificName_all, m3.out$StockID_all, 
                       m3.out$TeleostOrElasmo_all,
                       m3.out$r3fit_a_all, m3.L10.out$r3fit_a_lowest10) %>%
  as.data.frame() %>%
  setNames(c("ScientificName", "StockID", "TeleostOrElasmo", "alphaALL", "alphaLOW10")) 


# Exceptional alpha cases: Semelparous species ----

# Pink salmon (O. gorbuscha) ----
# Alphas were mined opportunistically from the literature for the same populations as MR data  
# (see manuscript)
Ricker_alphas$alphaOTHER <- NA # add col
Ricker_alphas[nrow(Ricker_alphas)+1, c(1, 2, 3)] <- c("Oncorhynchus gorbuscha", "PSALMSETONA", "Teleost") # add rows
Ricker_alphas[nrow(Ricker_alphas)+1, c(1, 2, 3)] <- c("Oncorhynchus gorbuscha", "PSALMHARR", "Teleost")
Ricker_alphas[Ricker_alphas$ScientificName == "Oncorhynchus gorbuscha" & Ricker_alphas$StockID == "PSALMSETONA", "alphaOTHER"] <- 11.2
Ricker_alphas[Ricker_alphas$ScientificName == "Oncorhynchus gorbuscha" & Ricker_alphas$StockID == "PSALMHARR", "alphaOTHER"] <- 7.4

# Capelin (M. villosus) ----
# Barents Sea capelin are semelparous. However, the S-R units on RAM are in E00/MT, and needed to be converted to E00/E00.
# alphaALL (from Ricker model fit to all years in timeseries): 305206.8 recruits/MT spawners
# alphaLOW10 (from Ricker model fit to lowest 10 ssb years in timeseries): 1173653.8 recruits/MT spawners
# 1 MT is 1000000 g
# 50% of capelin are mature at 2 or 3, "surviving to an age of four or five years" where most do not live older than 5, so "that the spawning stock each year is totally dominated by 3 or 4 years old capelin" (Gjosaeter 1998, Skaret et al 2020)
# "Most female spawners are in the length interval 14 to 18 cm" (Gjosaeter, 1998)

# Average mass of 3 and 4 year old capelin:
# Von B equation results in length which match mean documented age-size classes well.
# Now, using least conservative length-weight equation, alpha in E00/E00 would be:
(305206.9 * 58.5)/1000000 # 17.8546

# However, using the above L-W equation leads to 50 g 3 yr-old capelin -- but capelin weight rarely exceeds 50 g. We therefore opted for the more conservative L-W equation.
# Capelin alphaALL in units E00/E00:
(305207 * ((23 + 30)/2))/1000000 # = 8.08799
# can also be calculated as: 305206.8/(1000000/((23 + 30)/2))
# Capelin alphaLOW10 in units E00/E00:
(1173654 * ((23 + 30)/2))/1000000 # = 31.10183

# Note for SPRfis0: Baulier et al. 2012 say that females mature at 2/3 years in the Barents Sea. The average amat (2.5 years) was used.
Ricker_alphas[Ricker_alphas$ScientificName == "Mallotus villosus", c("alphaALL", "alphaLOW10")] <- c(8.08799, 31.10183)

# Save alphas DF ----
#write.csv(Ricker_alphas, file="data/RAMdata/Ricker_alphas.csv", row.names=FALSE, fileEncoding = 'UTF-8') 


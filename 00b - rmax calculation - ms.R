# Sarah Gravel
# 00 Data pre-processing

# The calculation of rmax 
# Using separate methods for sharks and teleosts

# Note: requires packages in script 01a and output from script 00a
# Note: for teleosts, we selected the highest alpha value from the Ricker model fit to all years vs. the lowest 10 SSB years in the time series.
# For a stock where the "highest alpha" resulted from a Ricker model with an estimated Rp (peak recruitment) exceeding the highest observed level of recruitment, the alternative (lower) alpha was preferred.

# LHT data ----

# All fish LHTs:
allfish_LHT <- read.csv("data/LHTdata/Gravel_all_LHTs_best_match_ms.csv", header = TRUE, fileEncoding="UTF-8-BOM")  %>% as.data.frame()
teleost_LHT <- allfish_LHT %>%
  filter(TeleostOrElasmo =="Teleost") %>%
  dplyr::select("ScientificName",	"ScientificNamePopID",	
                "CommonName",	"Family", "TeleostOrElasmo", "StockID",	
                "SPRfis0", "Amat",	"Amax",	"M", "lamat")
elasmo_LHT <- allfish_LHT %>%
  filter(TeleostOrElasmo =="Elasmo") %>%
  dplyr::select("ScientificName",	"ScientificNamePopID",	
                "CommonName",	"Family", "TeleostOrElasmo",	
                "Amat",	"Amax",	"M", "lamat", "b")
# teleost alphas:
# (output from file 00a stock-recruitment models)
# Ricker_alphas <- read.csv("data/RAMdata/Ricker_alphas.csv", header=TRUE, fileEncoding="UTF-8-BOM") %>% as.data.frame()
teleost_alphas <- Ricker_alphas %>% 
  filter(TeleostOrElasmo=="Teleost") %>%
  dplyr::select("StockID", "alphaALL", "alphaLOW10", "alphaOTHER")
  
teleost_alphas$alphaALL <- as.numeric(teleost_alphas$alphaALL)
teleost_alphas$alphaLOW10 <- as.numeric(teleost_alphas$alphaLOW10)
teleost_alphas$alphaOTHER <- as.numeric(teleost_alphas$alphaOTHER)

# combine for full teleost data set to calculate rmax:
setdiff(teleost_LHT$stockID, teleost_alphas$stockID)
teleost_LHTandAlphas <- merge(teleost_LHT, teleost_alphas, by= "StockID")

# Loop through species to calculate rmax ----
# function to get maximum value (to choose between alpha generated from all years or lowest 10 SSB years for each stock):
my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
my.min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)

# loop:
rmax <- function (x, class) {
  
  # x is elasmo_LHT or teleostLHTandAlphas
  # class is teleost or elasmo
  
  # rmax function:
  rmax_func <- function(rmax) (exp(rmax)^amat - exp(-M)*
                                 (exp(rmax)^(amat-1)) - (alpha_til))^2
  
  x$Rmax <- NA
  alpha_til_val <- NA
  M_val <- NA
  
  for (j in 1:length(x$ScientificName)) {
    
    print(x$ScientificName[j])
    
    # FOR TELEOSTS:
    if (class=="teleost") {
      
      # For jth stock:
      M_val <- x$M[j] # Adult mortality
      SPRfis0 <- x$SPRfis0[j] # Spawning biomass per recruit at fishing pressure= 0
      lamat <- x$lamat[j] # Juvenile mortality (cumulative)
      alphas <- c(x$alphaALL[j], x$alphaLOW10[j], x$alphaOTHER[j]) # Alphas 
      # ^ From stock-recruit models fit to all years in timeseries or lowest 10 SSB years,
      # Or alphas mined from literature for pops matching MR data (Pink salmon)
      a <- NA # alpha 'a' var
      b <- NA # reproductive output 'b' var
      if (x$StockID[j] == "GAGSATLC" | 
          x$StockID[j] == "WINFLOUN5Z" | 
          x$StockID[j] == "LSTHORNHPCOAST" | 
          x$StockID[j] == "SBT" | 
          x$StockID[j] == "PACBTUNA") {
        # ^ Stocks where the highest alpha resulted from a Ricker model fit where the estimated Rp (peak recruitment) value was >> than the highest observed recruitment value:
        # In which case, the alternative (lower) alpha was selected.
        a <- my.min(alphas)
        print(a)
      } else {
        # Otherwise choose highest alpha:
        a <- my.max(alphas)
        print(a)
      }
      
      # For semelparous teleosts:
      # From Myers et al 2001: "For semelparous species in which R and S are in the same units, the slope at the origin for the Beverton-Holt and Ricker models (alpha) can be directly interpreted as the maximum annual reproductive rate. For other species, alpha must be standardized."
      # 'a' is ANNUAL, needs to be standardized in iteroparous species 
      # SPRfis0 is LIFETIME spawners (or spawner biomass/recruit)
      # 'â' is maximum LIFETIME reproductive output
      # 'ã' is maximum ANNUAL reproductive output/rate (Myers et al 1997, 1999, 2001)
      # So in semelparous species: ã = a.
      if (x$Family[j] == "Salmonidae" | x$Family[j] == "Osmeridae") {
        alpha_til_val <- a 
        # Salmon alphas mined opportunistically from literature
        print("Semelparous ã:")
        print(alpha_til_val)
        b <- alpha_til_val/lamat 
        # ^NOTE for teleosts: 
        # For teleosts, repro output 'b' is not required to calculate alpha tilde 
        # (i.e., the maximum spawners per spawner; see Methods).
        # But for the repro output models in Question 1, we estimated teleost repro output,where : 
        # b = alpha tilde / lamat
      } else {
        # For iteroparous teleosts:
          alpha_til_val <- (a * SPRfis0 * (1- exp(-M_val))) # Alpha tilde (iteroparous)
          b <- alpha_til_val/lamat # Reproductive output for iteroparous teleosts
      }
      x$MaxSpawnersPerSpawner[j] <- alpha_til_val
      x$ReproOutput[j] <- b # Reproductive output
      
    
      # FOR SHARKS:
    } else if (class =="elasmo") {
      M_val <- x$M[j] # Adult mortality
      x$ReproOutput[j] <- x$b[j] # Repro output
      alpha_til_val <- x$b[j] * x$lamat[j] # Alpha tilde
      
    } else {
      # try to remove any stored values for these, and then will throw an error
      rm(M_val) 
      rm(alpha_til_val)
    }
    
    print("ã used:")
    print(alpha_til_val)
    
    amat = x$Amat[j]
    alpha_til = alpha_til_val
    M = M_val
    
    rmax_val <- nlminb(1/(0.4*x$Amat[j]), rmax_func,
                       # amat = x$amat[j],
                       # alpha_til = alpha_til_val,
                       # M = M_val,
                       lower=0, upper=5)$par
    
    x$Rmax[j] <- rmax_val # save rmax value
  }
  return(x)
}

# Elasmo rmax ----
e_rmax <- rmax(x=elasmo_LHT, class="elasmo")
e_rmax <- e_rmax %>%
  dplyr::select("ScientificName", "ScientificNamePopID", "Family", "TeleostOrElasmo", 
             "Amat", "Amax", "ReproOutput", "Rmax")

# Teleost rmax ----
t_rmax_aHIGHEST <- rmax(x=teleost_LHTandAlphas, class="teleost")
t_rmax <- t_rmax_aHIGHEST %>%
  filter(Rmax != 0)

t_rmax <- t_rmax %>%
  dplyr::select("ScientificName", "ScientificNamePopID", "Family", "TeleostOrElasmo", 
             "Amat", "Amax", "ReproOutput", "Rmax")

# All fish spp rmax DF ----
fishRmax <- rbind(e_rmax, t_rmax)
#write.csv(fishRmax, file = "data/LHTdata/all_fish_rmax.csv", row.names = FALSE)

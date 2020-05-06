# ---- Set working directory ----

# I recommend running this script on the server, but
# for testing on your local computer
local.nodename <- "L00019154" # to figure out your computers nodename,
# type `Sys.info()["nodename"]` into the console. The returned string
# is the nodename of your computer.
local.ed <- "/Volumes/T1000/Analysis/kforthman/COVID19" # The path to the
# working directory on your computer.

# If you run the script locally, it will set the working directory.
if(Sys.info()["nodename"] == local.nodename){setwd(local.ed)}


# Correlate 500 cities features to the factor scores.
library(stringr)

cdc_data_var_filename <- "Data/cdc_data_var.rda"

if(!file.exists(cdc_data_var_filename)){
  cdc_data <- read.csv("../Neighborhood_Analysis/Tract/data/500_Cities__Census_Tract-level_Data__GIS_Friendly_Format_.csv", stringsAsFactors = F)
  cdc_data_names <- as.matrix(read.csv("../Neighborhood_Analysis/Tract/data/500-Cities VAR CODES.csv", header = F))
  
  cdc_data$TractFIPS <- str_pad(as.character(cdc_data$TractFIPS), 11, "left", pad = 0)
  
  cdc_data_var <- cdc_data[,seq(7, 62, by = 2)]
  colnames(cdc_data_var) <- cdc_data_names[,3]
  
  cdc_data_var$tractID <- cdc_data$TractFIPS
  cdc_data_var$stateID <- substr(cdc_data$TractFIPS,1,2)
  cdc_data_var$countyID <- substr(cdc_data$TractFIPS,3,5)
  cdc_data_var$cityID <- cdc_data$PlaceFIPS
  cdc_data_var$state <- cdc_data$X
  cdc_data_var$city <- cdc_data$PlaceName
  cdc_data_var$lat <- as.numeric(unlist(lapply(strsplit(cdc_data$Geolocation, ",|[(]|[)]| "), "[", 2)))
  cdc_data_var$lng <- as.numeric(unlist(lapply(strsplit(cdc_data$Geolocation, ",|[(]|[)]| "), "[", 4)))
  cdc_data_var$population2010 <- as.numeric(unlist(lapply(strsplit(cdc_data$population2010, ","), paste, collapse = "")))
  cdc_data_var <- cdc_data_var[,c(29:ncol(cdc_data_var),1:28)]
  
  countyName <- read.csv("../Neighborhood_Analysis/Tract/data/county_code.csv", header = F, stringsAsFactors = F)[,2:4]
  names(countyName) <- c("stateID", "countyID", "county")
  countyName$countyID <- str_pad(as.character(countyName$countyID), 3, "left", pad = 0)
  countyName$stateID <- str_pad(as.character(countyName$stateID), 2, "left", pad = 0)
  
  cdc_data_var <- merge(cdc_data_var, countyName, by = c("stateID", "countyID"), all.y = F, all.x = T)
  cdc_data_var <- cdc_data_var[,c('tractID', 'stateID', 'countyID', 'cityID', 
                                  'state', 'county', 'city', 'population2010', 
                                  'lat','lng',
                                  cdc_data_names[,3])]
  
  save(cdc_data_var, file = "cdc_data_var.rda")
  
  nrow(unique(cdc_data_var[,c("stateID", "countyID")]))
  length(unique(cdc_data_var[,"cityID"]))
  
  nrow(unique(cdc_data_var[,c("stateID", "countyID", "cityID")]))
  
}else{load(cdc_data_var_filename)}
load("Data/county_factors.rda")

cdc_data_var$statecountyID <- apply(cdc_data_var[,c("stateID", "countyID")], 1, paste, collapse = "")
uniq_statecountyID <- unique(cdc_data_var$statecountyID)
county_factors$statecountyID <- substr(county_factors$ID,1,5)

# ---- Avg by county ----

uniq_statecountyID <- unique(cdc_data_var$statecountyID)
countyCodes <- data.frame(statecountyID = uniq_statecountyID,
                          stateID = substr(uniq_statecountyID, 1,2),
                          countyID = substr(uniq_statecountyID, 3,5),
                          total.pop = NA,
                          cities500.pop = NA, cities500.n.tracts = NA, 
                          cities500.pop.ratio = NA,
                          stringsAsFactors = F)

for(j in 1:length(uniq_statecountyID)){
  data <- cdc_data_var[cdc_data_var$statecountyID == uniq_statecountyID[j], ]
  
  countyCodes$cities500.n.tracts[j] <- nrow(data)
  countyCodes$cities500.pop[j] <- sum(data$population2010)
  
  countyCodes$total.pop[j] <- county_factors$pop[county_factors$statecountyID == uniq_statecountyID[j]]
  countyCodes$cities500.pop.ratio[j] <- countyCodes$cities500.pop[j]/countyCodes$total.pop[j]
}

health_var_names <- names(cdc_data_var)[11:38]
county_avg <- matrix(nrow = length(uniq_statecountyID), ncol = length(health_var_names))
colnames(county_avg) <- health_var_names
for(j in 1:length(uniq_statecountyID)){
  data <- cdc_data_var[cdc_data_var$statecountyID == uniq_statecountyID[j], c("population2010", health_var_names)]
  
  # weight the factor scores by population in county.
  for(k in health_var_names){
    data[,k] <- data[,k] * data[,"population2010"] / countyCodes$cities500.pop[j]
  }
  
  data <- data[,-1]
  
  if(is.null(dim(data))){
    county_avg[j,] <-data
  }else{
    county_avg[j,] <- apply(data, 2, sum, na.rm = T)
  }
  
}

county_500CitiesData <- data.frame(ID = uniq_statecountyID, county_avg, countyCodes$cities500.pop.ratio)

save(county_500CitiesData, file = "Data/county_500CitiesData.rda")
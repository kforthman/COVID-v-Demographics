original <- read.csv(paste0("../Neighborhood_Analysis/Tract/output/2015/useful_ACS_originalData.csv"), row.names = 1)
# If the state code is only a single digit, it is padded.
rownames(original) <- str_pad(rownames(original), 11, "left", pad="0")

# --- Now, begin adding variables to a new matrix and changing them as needed. --- #

# Total population ----
total.pop <- original['Total']
rm(original)

# ---- Load neighborhood data ----

neigh_data <- read.csv("../Neighborhood_Analysis/Tract/output/2015/5 factors/FA_5Factors.csv", row.names = 1)
rownames(neigh_data) <- str_pad(as.character(rownames(neigh_data)), 11, "left", pad = "0")

neigh_data <- merge(neigh_data, total.pop, by = 0)

rownames(neigh_data) <- neigh_data[,1]
neigh_data <- neigh_data[,-1]

# ---- Avg by county ----

countyName <- read.csv("../Neighborhood_Analysis/Tract/data/county_code.csv")
countyCodes <- data.frame(state_code = str_pad(as.character(countyName[,2]), 2, "left", pad = "0"),
                     county_code = str_pad(as.character(countyName[,3]), 3, "left", pad = "0"),
                     pop = NA, stringsAsFactors = F)

for(j in 1:(dim(countyCodes)[1])){
  data <- neigh_data[which(substr(rownames(neigh_data), 1,2) == countyCodes$state_code[j]), ]
  data <- data[which(substr(rownames(data), 3,5) == countyCodes$county_code[j]), ]
  
  countyCodes$pop[j] <- sum(data[,"Total"])
}

county_avg <- matrix(nrow = dim(countyCodes)[1], ncol = 5)
colnames(county_avg) <- colnames(neigh_data)[1:5]
for(j in 1:(dim(countyCodes)[1])){
  data <- neigh_data[which(substr(rownames(neigh_data), 1,2) == countyCodes$state_code[j]), ]
  data <- data[which(substr(rownames(data), 3,5) == countyCodes$county_code[j]), ]
  
  # weight the factor scores by population in county.
  for(k in 1:5){
    data[,k] <- data[,k] * data[,6] / countyCodes$pop[j]
  }
  
  data <- data[,-6]
  
  if(is.null(dim(data))){
    county_avg[j,] <-data
  }else{
    county_avg[j,] <- apply(data, 2, sum)
  }
  
}

county_factors <- data.frame(ID = apply(countyCodes[,1:2], 1, paste, collapse = ""), county_avg,
                             pop = countyCodes$pop)

save(county_factors, file = "Data/county_factors.rda")

# ---- Load neighborhood data ----

neigh_data <- read.csv("../Neighborhood_Analysis/Tract/output/2015/5 factors/FA_5Factors.csv", row.names = 1)
rownames(neigh_data) <- str_pad(as.character(rownames(neigh_data)), 11, "left", pad = "0")

# ---- Avg by county ----

countyName <- read.csv("../Neighborhood_Analysis/Tract/data/county_code.csv")
countyCodes <- cbind(str_pad(as.character(countyName[,2]), 2, "left", pad = "0"),
                     str_pad(as.character(countyName[,3]), 3, "left", pad = "0"))

county_avg <- matrix(nrow = dim(countyCodes)[1], ncol = 5)
colnames(county_avg) <- colnames(neigh_data)
for(j in 1:(dim(countyCodes)[1])){
  data <- neigh_data[which(substr(rownames(neigh_data), 1,2) == countyCodes[j,1]), ]
  data <- data[which(substr(rownames(data), 3,5) == countyCodes[j,2]), ]
  
  if(is.null(dim(data))){
    county_avg[j,] <-data
  }else{
    county_avg[j,] <- apply(data, 2, mean)
  }
  
}

county_factors <- data.frame(ID = apply(countyCodes, 1, paste, collapse = ""), county_avg)

save(county_factors, file = "Data/county_factors.rda")
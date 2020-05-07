#system("cd Data/COVID-19 ; git pull ; cd ../../")
library(stringr)
library(corrplot)
library(shiny)
library(lme4)
library(lmerTest)
library(gtools)
setwd("/Volumes/T1000/Analysis/kforthman/COVID19")

data.path <- "Data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/"

# Read in the data
US.deaths <- read.csv(
  paste0(data.path, "time_series_covid19_deaths_US.csv"), 
  header = T, stringsAsFactors = F)
US.cases <- read.csv(
  paste0(data.path, "time_series_covid19_confirmed_US.csv"), 
  header = T, stringsAsFactors = F)

# Read in the header seprately.
US.cases.head <- read.csv(
  paste0(data.path, "time_series_covid19_confirmed_US.csv"), 
  header = F, stringsAsFactors = F)[1,]
US.deaths.head <- read.csv(
  paste0(data.path, "time_series_covid19_deaths_US.csv"), 
  header = F, stringsAsFactors = F)[1,]

# Correct the dates in the header to be more useable as
# column names.
proper_date <- function(dates){
  dates <- sapply(dates, strsplit, split = "/")
  dates <- lapply(dates, str_pad, width = 2, side = "left", pad = "0")
  dates <- lapply(dates, paste, collapse = "_")
  dates <- unlist(dates)
  
  return(dates)
}

dates.cases <- proper_date(US.cases.head[-c(1:11)])
dates.deaths <- proper_date(US.deaths.head[-c(1:12)])

names(US.cases) <- c(US.cases.head[1,1:11], dates.cases)
names(US.deaths) <- c(US.deaths.head[1,1:12], dates.deaths)

if(sum(US.cases$UID != US.deaths$UID, na.rm = T) > 0){warning("COVID data rows do not match!")}
US.cases$Population <- US.deaths$Population
US.cases <- US.cases[,c(1:11, ncol(US.cases), 12:(ncol(US.cases)-1))]

save(US.cases, file = "Data/US.cases.rda")
save(US.deaths, file = "Data/US.deaths.rda")



## Other stats within the daily reports

data.path <- "Data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports_us/"
daily_filenames <- list.files(data.path)
daily_filenames <- daily_filenames[daily_filenames != "README.md"]

todays_report_filename <- daily_filenames[length(daily_filenames)]
US.todaysReport <- read.csv(
  paste0(data.path, todays_report_filename), 
  header = T, stringsAsFactors = F)

save(daily_filenames, file = "Data/daily_filenames.rda")
save(US.todaysReport, file = "Data/US.todaysReport.rda")



all.states <- c('Alabama', 'Alaska', 'American Samoa', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'Diamond Princess', 'District of Columbia', 'Florida', 'Georgia', 'Grand Princess', 'Guam', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Northern Mariana Islands', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Puerto Rico', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virgin Islands', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
all.states.df <- data.frame(Province_State = all.states)
all.stats <- c("Confirmed", "Deaths", "Recovered", "Active", "Incident_Rate", "People_Tested", "People_Hospitalized", "Mortality_Rate", "Testing_Rate", "Hospitalization_Rate")

compiled.stats <- list()
for(i in 1:length(daily_filenames)){
  day <- substring(daily_filenames[i],1,10)
  data <- read.csv(
    paste0(data.path, daily_filenames[i]),
    header = T, stringsAsFactors = F)
  compiled.stats[[i]] <- merge(all.states.df, data, all.y = F)
  names(compiled.stats)[i] <- day
}
save(compiled.stats, file = "Data/compiled.stats.rda")




# ---- Calculate some useful stats to compare with neighborhood data ----
US.cases.info <- as.matrix(US.cases[,1:12])
US.cases.data <- as.matrix(US.cases[,-c(2:12)])
US.deaths.info <- as.matrix(US.deaths[,1:12])
US.deaths.data <- as.matrix(US.deaths[,-c(2:12)])

rownames(US.cases.info) <- US.cases.info[,1]
US.cases.info <- US.cases.info[,-1]
rownames(US.cases.data) <- US.cases.data[,1]
US.cases.data <- US.cases.data[,-1]
rownames(US.deaths.info) <- US.deaths.info[,1]
US.deaths.info <- US.deaths.info[,-1]
rownames(US.deaths.data) <- US.deaths.data[,1]
US.deaths.data <- US.deaths.data[,-1]


ndays.cases <- ncol(US.cases.data)
ndays.deaths <- ncol(US.deaths.data)

nobs <- nrow(US.cases.data)

US.total <- data.frame(day = colnames(US.cases.data))
US.total$cases.total <- colSums(US.cases.data)
US.total$deaths.total <- colSums(US.deaths.data)

## Average rise in cases per day
US.stats <- data.frame(UID = US.cases$UID)

rise.cases <- matrix(ncol = ndays.cases - 1, nrow = nobs)
colnames(rise.cases) <- colnames(US.cases.data)[-1]
for(i in 1:ncol(rise.cases) + 1){
  rise <- US.cases.data[,i] - US.cases.data[,i-1]
  rise.cases[,i-1] <- rise
}

US.stats$avg.rise.cases <- apply(rise.cases, 1, mean)

US.total$rise.cases.total <- c(NA, colSums(rise.cases))

## Average rise in deaths per day

rise.deaths <- matrix(ncol = ndays.deaths - 1, nrow = nobs)
colnames(rise.deaths) <- colnames(US.deaths.data)[-1]
for(i in 1:ncol(rise.deaths) + 1){
  rise <- US.deaths.data[,i] - US.deaths.data[,i-1]
  rise.deaths[,i-1] <- rise
}

US.stats$avg.rise.deaths <- apply(rise.deaths, 1, mean)

US.total$rise.deaths.total <- c(NA, colSums(rise.deaths))

## Total cases

US.stats$total.cases <- US.cases.data[,ndays.cases]


## Total cases per capita

US.stats$total.cases.percap <- US.stats$total.cases / US.cases$Population
US.stats$total.cases.percap[US.cases$Population == 0] <- NA


## Total deaths
US.stats$total.deaths <- US.deaths.data[,ndays.deaths]


## Total deaths per capita
US.stats$total.deaths.percap <- US.stats$total.deaths / US.deaths$Population
US.stats$total.deaths.percap[US.deaths$Population == 0] <- NA


## Total deaths per case
#*Error in Johns Hopkins data has rows with total.deaths > total.cases.*
  
# pos.case.ind <- US.stats$total.cases > 0
# US.stats$total.deaths.percase[pos.case.ind] <- US.stats$total.deaths[pos.case.ind] / US.stats$total.cases[pos.case.ind]
# US.stats$total.deaths.percase[!pos.case.ind] <- 0
US.stats$total.deaths.percase <- US.stats$total.deaths / US.stats$total.cases
US.stats$total.deaths.percase[US.stats$total.cases == 0] <- NA

save(US.stats, file = "Data/US.stats.rda")
save(US.total, file = "Data/US.total.rda")


load("Data/county_factors.rda")
load("Data/county_500CitiesData.rda")

US.stats$UID <- str_pad(US.stats$UID, 8, "left", pad = "0")
US.stats$UID <- substr(US.stats$UID, 4, 8)

county.Demo_and_Covid.allcounties <- merge(county_factors, US.stats, by.x = "ID", by.y = "UID")
data.merge2 <- merge(county.Demo_and_Covid.allcounties, county_500CitiesData, by = "ID", all.x = F)
US.todaysReport.states <- US.todaysReport[!is.na(US.todaysReport$FIPS) & nchar(US.todaysReport$FIPS)<=2,]
US.todaysReport.states$FIPS <- str_pad(US.todaysReport.states$FIPS, 2, "left", pad = "0")
US.todaysReport.states <- US.todaysReport.states[,c(10,6:9,11:14,17:18)]

data.merge2$stateID <- substr(data.merge2$ID,1,2)

data.merge3 <- merge(data.merge2, US.todaysReport.states, by.x = "stateID", by.y = "FIPS")

county.Demo_and_Covid.500counties <- data.merge3
save(county.Demo_and_Covid.allcounties, file = "Data/county.Demo_and_Covid.allcounties.rda")
save(county.Demo_and_Covid.500counties, file = "Data/county.Demo_and_Covid.500counties.rda")

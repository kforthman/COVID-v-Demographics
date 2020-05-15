# I recommend running this script on the server, but
# for testing on your local computer
local.nodename <- "L00019154" # to figure out your computers nodename,
# type `Sys.info()["nodename"]` into the console. The returned string
# is the nodename of your computer.
local.ed <- "/Volumes/T1000/Analysis/kforthman/COVID19" # The path to the
# working directory on your computer.

# If you run the script locally, it will set the working directory.
if(Sys.info()["nodename"] == local.nodename){setwd(local.ed)}

# Pull new data
#system("cd Data/COVID-19 ; git pull ; cd ../../")

source("Scripts/save_UScasesdeaths.R")
rmarkdown::render("Scripts/index.Rmd", output_file = "../index.html", clean = T)
rsconnect::deployApp("Scripts/interactive_plots.Rmd",
                     appFiles = c("Data/US.cases.rda",
                                  "Data/US.deaths.rda",
                                  "Data/daily_filenames.rda",
                                  "Data/compiled.stats.rda"),
                     appId = "kforthman",
                     launch.browser = F
                     )

#if (!is.null(target$appId) && !is.null(app) && interactive() && !forceUpdate) {

# Push updates
#system("git add index.html Data/. ; git commit -m "Updating data." ; git push origin master")
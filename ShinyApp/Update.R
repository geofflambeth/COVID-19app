##This script needs to be run after pulling down updates from JohnsHopkins origin or upstream...
##Changes need to be committed to master after running

setwd("../csse_covid_19_data/csse_covid_19_daily_reports/")

library(dplyr)

JohnsHopkinsAll <- list.files(pattern = "*.csv", full.names = TRUE) #Collect filenames from the working directory folder
JohnsHopkinsAll <- lapply(JohnsHopkinsAll,function(i){           #Collect all data together into individual data frames
  read.csv(i, header=TRUE)
})
JohnsHopkinsAll <- bind_rows(JohnsHopkinsAll)                       #Bind all data frames together

write.csv(JohnsHopkinsAll, "../../ShinyApp/JohnsHopkinsAll.csv")

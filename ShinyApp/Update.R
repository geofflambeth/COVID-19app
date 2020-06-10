##This script needs to be run after pulling down updates from JohnsHopkins origin or upstream...
##Changes need to be committed to master after running

setwd("~/Google Drive/GitHub/COVID-19app/csse_covid_19_data/csse_covid_19_daily_reports/")

library("dplyr")
library("lubridate")

#Load JohnsHopkinsAll from cloned Johns Hopkins .csv files in /ccse_covid_19_daily_reports/
JohnsHopkinsAll <- list.files(pattern = "*.csv", full.names = TRUE)
JohnsHopkinsAll <- lapply(JohnsHopkinsAll,function(i){
  read.csv(i, header=TRUE)
})
JohnsHopkinsAll <- bind_rows(JohnsHopkinsAll)

###Clean JohnsHopkinsAll###
#Clean Dates
Last.Update <- parse_date_time(JohnsHopkinsAll$Last.Update, orders = c("ymd_HMS", "mdy_HM"))
Last_Update <- parse_date_time(JohnsHopkinsAll$Last_Update, orders = c("ymd_HMS", "mdy_HM"))
Last.Update[is.na(Last.Update)] <- Last_Update[!is.na(Last_Update)]
DateTime <- Last.Update
Date <- format(DateTime, "%Y-%m-%d 00:00:00")
Date <- parse_date_time(Date, orders = c("ymd_HMS"))
#Clean Province/State
Province.State <- JohnsHopkinsAll$Province.State
Province_State <- JohnsHopkinsAll$Province_State
Province.State[is.na(Province.State)] <- Province_State[!is.na(Province_State)]
ProvinceState <- Province.State
#Clean Country/Region
Country.Region <- JohnsHopkinsAll$Country.Region
Country_Region <- JohnsHopkinsAll$Country_Region
Country.Region[is.na(Country.Region)] <- Country_Region[!is.na(Country_Region)]
CountryRegion <- Country.Region
#Clean Province/State
Province.State <- JohnsHopkinsAll$Province.State
Province_State <- JohnsHopkinsAll$Province_State
Province.State[is.na(Province.State)] <- Province_State[!is.na(Province_State)]
ProvinceState <- Province.State
#Collect Regular Attributes (Simpler than dates, location, etc...)
Confirmed <- JohnsHopkinsAll$Confirmed
Deaths <- JohnsHopkinsAll$Deaths
Recovered <- JohnsHopkinsAll$Recovered
Active <- JohnsHopkinsAll$Active
FIPS <- JohnsHopkinsAll$FIPS
Admin2 <- JohnsHopkinsAll$Admin2

#Join into new dataframe
JohnsHopkinsAll <- data.frame(Date, DateTime, ProvinceState, CountryRegion, Admin2, FIPS, Confirmed, Active, Deaths, Recovered)

#Write out as updated JohnsHopkinsAll.csv
#This file will be read in by server.R where filtering and analysis will begin
write.csv(JohnsHopkinsAll, "../../ShinyApp/JohnsHopkinsAll.csv")

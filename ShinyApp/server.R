##V2.0 Beta

# install.packages("plotly")
# install.packages("shiny")
# install.packages("acelerometry")
# install.packages("RCurl")
# install.packages("dplyr")
# install.packages("lubridate")

#Load Packages
library("dplyr")
library("plotly")
library("lubridate")
library("shiny")
library("accelerometry")
library("RCurl")

#Load JohnsHopkinsAll from GitHub
#JohnsHopkinsAll.csv needs to be updated manually using update.R
#Script references the master branch, so remember to update the MASTER branch not just the BETA branch
#FEATURE TO ADD LATER <- Possible to write this update into script automatically? Need to solve github sync issue with large files....

##### NECESSARY FOR WEB PUBLISHING #####
#GitHubURL <- getURL("https://raw.githubusercontent.com/geofflambeth/COVID-19app/master/ShinyApp/JohnsHopkinsAll.csv")
#JohnsHopkinsAll <- read.csv(text = GitHubURL)
#JohnsHopkinsAll$Date <- parse_date_time(JohnsHopkinsAll$Date, orders = c("ymd"))
#JohnsHopkinsAll$DateTime <- parse_date_time(JohnsHopkinsAll$DateTime, orders = c("ymd_HMS"))
                                     

##### FASTER FOR LOCAL USE #####
setwd("./")
JohnsHopkinsAll <- read.csv("JohnsHopkinsAll.csv")
JohnsHopkinsAll$Date <- parse_date_time(JohnsHopkinsAll$Date, orders = c("ymd"))
JohnsHopkinsAll$DateTime <- parse_date_time(JohnsHopkinsAll$DateTime, orders = c("ymd_HMS"))

##FILTER RULES HERE##
CountryRegionFilter <- "US"
ProvinceStateFilter <- "New Mexico"
Admin2Filter <- "Santa Fe"
Exclusion1 <- "2020-04-22"
Exclusion1 <- parse_date_time(Exclusion1, orders = "ymd")
Exclusion2 <- "2020-04-23"
Exclusion2 <- parse_date_time(Exclusion2, orders = "ymd")
Exclusion3 <- "2020-04-13"
Exclusion3 <- parse_date_time(Exclusion3, orders = "ymd")
Exclusion4 <- "2020-04-27"
Exclusion4 <- parse_date_time(Exclusion4, orders = "ymd")
Exclusion5 <- "2020-04-28"
Exclusion5 <- parse_date_time(Exclusion5, orders = "ymd")
Exclusion6 <- "2020-06-29"
Exclusion6 <- parse_date_time(Exclusion6, orders = "ymd")
Exclusion7 <- "2020-06-30"
Exclusion7 <- parse_date_time(Exclusion7, orders = "ymd")


#Filter JohnsHopkinsAll
#FEATURE TO ADD LATER <- Select filters from ShinyApp ui.R
#JohnsHopkinsAll <- filter(JohnsHopkinsAll, Date != Exclusion1)
#JohnsHopkinsAll <- filter(JohnsHopkinsAll, Date != Exclusion2)
#JohnsHopkinsAll <- filter(JohnsHopkinsAll, Date != Exclusion3)
#JohnsHopkinsAll <- filter(JohnsHopkinsAll, Date != Exclusion4)
#JohnsHopkinsAll <- filter(JohnsHopkinsAll, Date != Exclusion5)
#JohnsHopkinsAll <- filter(JohnsHopkinsAll, Date != Exclusion6)
#JohnsHopkinsAll <- filter(JohnsHopkinsAll, Date != Exclusion7)
#JohnsHopkinsAll <- filter(JohnsHopkinsAll, Date != Exclusion)
#JohnsHopkinsAll <- filter(JohnsHopkinsAll, Date != Exclusion)
#JohnsHopkinsAll <- filter(JohnsHopkinsAll, Date != Exclusion)
JohnsHopkinsCountryRegion <- filter(JohnsHopkinsAll, CountryRegion == CountryRegionFilter)
JohnsHopkinsProvinceState <- filter(JohnsHopkinsCountryRegion, ProvinceState == ProvinceStateFilter)
JohnsHopkinsAdmin2 <- filter(JohnsHopkinsProvinceState, Admin2 == Admin2Filter)
today <- max(JohnsHopkinsProvinceState$Date) #sets today as the most recent data point
tomorrow <- max(JohnsHopkinsProvinceState$Date)+86400 #sets tomorrow as one day in the future
onemonthago <- max(JohnsHopkinsProvinceState$Date)-2678400 #removes 31 days from the most recent data point

if(Admin2Filter == "Santa Fe" & ProvinceStateFilter == "New Mexico") JohnsHopkinsAdmin2$Confirmed[262] <- ((JohnsHopkinsAdmin2$Confirmed[263]-JohnsHopkinsAdmin2$Confirmed[261])/2)+JohnsHopkinsAdmin2$Confirmed[261]

#Order ProvinceState by County and add NewCases
JohnsHopkinsProvinceState <- JohnsHopkinsProvinceState[order(JohnsHopkinsProvinceState$Admin2),]
JohnsHopkinsProvinceState$NewCasesByCounty = JohnsHopkinsProvinceState$Confirmed - lag(JohnsHopkinsProvinceState$Confirmed, default = 0)

##### Aggregate data by filter #####
#CountryRegion Aggregate by Date

### Needs input dataframe here
NewData <- JohnsHopkinsCountryRegion
#Aggregate content by date...
# ConfirmedCases <- NewData$Confirmed
# Deaths <- NewData$Deaths
# ActiveCases <- NewData$Active
# NewData <- data.frame(Date, ConfirmedCases, Deaths, ActiveCases)
Data <- aggregate(NewData$Confirmed, by = list(NewData$Date), FUN = sum)
Date <- Data$Group.1
Confirmed <- Data$x
Data <- aggregate(NewData$Deaths, by = list(NewData$Date), FUN = sum)
Deaths <- Data$x
Data <- aggregate(NewData$Active, by = list(NewData$Date), FUN = sum)
Active <- Data$x
Data <- aggregate(NewData$Recovered, by = list(NewData$Date), FUN = sum)
Recovered <- Data$x
NewConfirmed = Confirmed - lag(Confirmed, default = 0)
MovingAves7dayTemp <- c(NA, NA, NA, NA, NA, NA)
MovingAves7day <- movingaves(x = NewConfirmed, window = 7, integer = TRUE)
MovingAves14dayTemp <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
MovingAves14day <- movingaves(x = NewConfirmed, window = 14, integer = TRUE)
PercentChangeDaily = ((MovingAves7day - lag(MovingAves7day, default = 0)) / lag(MovingAves7day, default = 0)) * 100
PercentChangeDaily[which(!is.finite(PercentChangeDaily))] <- 1
MovingAvesPercentChangeTemp <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
MovingAvesPercentChange <- movingaves(x = PercentChangeDaily, window = 7, integer = FALSE)
#Add NA values
MovingAves7day <- append(MovingAves7dayTemp, MovingAves7day)
MovingAves14day <- append(MovingAves14dayTemp, MovingAves14day)
PercentChangeDaily <- append(MovingAves7dayTemp, PercentChangeDaily)
MovingAvesPercentChange <- append(MovingAvesPercentChangeTemp, MovingAvesPercentChange)


#Provide new name for data here:
NewData <- data.frame(Date, Confirmed, NewConfirmed, MovingAves7day, MovingAves14day, PercentChangeDaily, MovingAvesPercentChange, Active, Recovered, Deaths)
CountryRegionAgg <- NewData


#ProvinceState Aggregate by Date

### Needs input dataframe here
NewData <- JohnsHopkinsProvinceState
#Aggregate content by date...
# ConfirmedCases <- NewData$Confirmed
# Deaths <- NewData$Deaths
# ActiveCases <- NewData$Active
# NewData <- data.frame(Date, ConfirmedCases, Deaths, ActiveCases)
Data <- aggregate(NewData$Confirmed, by = list(NewData$Date), FUN = sum)
Date <- Data$Group.1
Confirmed <- Data$x
Data <- aggregate(NewData$Deaths, by = list(NewData$Date), FUN = sum)
Deaths <- Data$x
Data <- aggregate(NewData$Active, by = list(NewData$Date), FUN = sum)
Active <- Data$x
Data <- aggregate(NewData$Recovered, by = list(NewData$Date), FUN = sum)
Recovered <- Data$x
NewConfirmed = Confirmed - lag(Confirmed, default = 0)
MovingAves7dayTemp <- c(NA, NA, NA, NA, NA, NA)
MovingAves7day <- movingaves(x = NewConfirmed, window = 7, integer = TRUE)
PercentChangeDaily = ((MovingAves7day - lag(MovingAves7day, default = 0)) / lag(MovingAves7day, default = 0)) * 100
PercentChangeDaily[which(!is.finite(PercentChangeDaily))] <- 1
MovingAvesPercentChangeTemp <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
MovingAvesPercentChange <- movingaves(x = PercentChangeDaily, window = 7, integer = FALSE)
#Add NA values
MovingAves7day <- append(MovingAves7dayTemp, MovingAves7day)
PercentChangeDaily <- append(MovingAves7dayTemp, PercentChangeDaily)
MovingAvesPercentChange <- append(MovingAvesPercentChangeTemp, MovingAvesPercentChange)

#Provide new name for data here:
NewData <- data.frame(Date, Confirmed, NewConfirmed, MovingAves7day, PercentChangeDaily, MovingAvesPercentChange, Active, Recovered, Deaths)
ProvinceStateAgg <- NewData


#Admin2 Aggregate by Date

### Needs input dataframe here
NewData <- JohnsHopkinsAdmin2
#Aggregate content by date...
# ConfirmedCases <- NewData$Confirmed
# Deaths <- NewData$Deaths
# ActiveCases <- NewData$Active
# NewData <- data.frame(Date, ConfirmedCases, Deaths, ActiveCases)
Data <- aggregate(NewData$Confirmed, by = list(NewData$Date), FUN = sum)
Date <- Data$Group.1
Confirmed <- Data$x
Data <- aggregate(NewData$Deaths, by = list(NewData$Date), FUN = sum)
Deaths <- Data$x
Data <- aggregate(NewData$Active, by = list(NewData$Date), FUN = sum)
Active <- Data$x
Data <- aggregate(NewData$Recovered, by = list(NewData$Date), FUN = sum)
Recovered <- Data$x
NewConfirmed = Confirmed - lag(Confirmed, default = 0)
MovingAves7dayTemp <- c(NA, NA, NA, NA, NA, NA)
MovingAves7day <- movingaves(x = NewConfirmed, window = 7, integer = TRUE)
MovingAves14dayTemp <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
MovingAves14day <- movingaves(x = NewConfirmed, window = 14, integer = TRUE)
PercentChangeDaily = ((MovingAves7day - lag(MovingAves7day, default = 0)) / lag(MovingAves7day, default = 0)) * 100
PercentChangeDaily[which(!is.finite(PercentChangeDaily))] <- 1
MovingAvesPercentChangeTemp <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
MovingAvesPercentChange <- movingaves(x = PercentChangeDaily, window = 7, integer = FALSE)
#Add NA values
MovingAves7day <- append(MovingAves7dayTemp, MovingAves7day)
MovingAves14day <- append(MovingAves14dayTemp, MovingAves14day)

PercentChangeDaily <- append(MovingAves7dayTemp, PercentChangeDaily)
MovingAvesPercentChange <- append(MovingAvesPercentChangeTemp, MovingAvesPercentChange)

#Provide new name for data here:
NewData <- data.frame(Date, Confirmed, NewConfirmed, MovingAves7day, MovingAves14day, PercentChangeDaily, MovingAvesPercentChange, Active, Recovered, Deaths)
Admin2Agg <- NewData








#####PLOT DATA FOR SHINY APP#####

#Plot Admin2 Cases for top of dashboard
PlotTitle <- paste("COVID-19 Cases Over Time in", Admin2Filter, "County,", ProvinceStateFilter)
CasesAdmin2 <- plot_ly(Admin2Agg, x = ~Date, y = ~Confirmed, name = "Confirmed Cases", type = "scatter", mode = "lines", line = list(color = "blue", width = 4))
CasesAdmin2 <- CasesAdmin2 %>% add_trace(y = ~Recovered, name = "Recovered Cases", mode = "lines", line = list(color = "green", width = 4))
CasesAdmin2 <- CasesAdmin2 %>% add_trace(y = ~Deaths, name = "Deaths", mode = "lines", line = list(color = "red", width = 4))
CasesAdmin2 <- CasesAdmin2 %>% layout(title = PlotTitle, xaxis = list(title = "Date"), yaxis = list(title = "Cases"))

#Plot ProvinceState Cases
PlotTitle <- paste("COVID-19 Cases Over Time in", ProvinceStateFilter, ",", CountryRegionFilter)
CasesProvinceState <- plot_ly(ProvinceStateAgg, x = ~Date, y = ~Confirmed, name = "Confirmed Cases", type = "scatter", mode = "lines", line = list(color = "blue", width = 4))
CasesProvinceState <- CasesProvinceState %>% add_trace(y = ~Recovered, name = "Recovered Cases", mode = "lines", line = list(color = "green", width = 4))
CasesProvinceState <- CasesProvinceState %>% add_trace(y = ~Deaths, name = "Deaths", mode = "lines", line = list(color = "red", width = 4))
CasesProvinceState <- CasesProvinceState %>% layout(title = PlotTitle, xaxis = list(title = "Date"), yaxis = list(title = "Cases"))

#Plot CountryRegion Cases
PlotTitle <- paste("COVID-19 Cases Over Time in", CountryRegionFilter)
CasesCountryRegion <- plot_ly(CountryRegionAgg, x = ~Date, y = ~Confirmed, name = "Confirmed Cases", type = "scatter", mode = "lines", line = list(color = "blue", width = 4))
CasesCountryRegion <- CasesCountryRegion %>% add_trace(y = ~Recovered, name = "Recovered Cases", mode = "lines", line = list(color = "green", width = 4))
CasesCountryRegion <- CasesCountryRegion %>% add_trace(y = ~Deaths, name = "Deaths", mode = "lines", line = list(color = "red", width = 4))
CasesCountryRegion <- CasesCountryRegion %>% layout(title = PlotTitle, xaxis = list(title = "Date"), yaxis = list(title = "Cases"))

#Plot Admin2 New Cases
PlotTitle <- paste("7-Day Average New COVID-19 Cases in", Admin2Filter, "County,", ProvinceStateFilter)
NewCasesAdmin2 <- plot_ly(Admin2Agg, x = ~Date, y = ~MovingAves7day, name = "7-Day Average New Confirmed Cases", type = "bar", color = "red")
#NewCasesAdmin2 <- NewCasesAdmin2 %>% add_trace(y = ~MovingAves7day, name = "7-Day Average New Cases", mode = "lines", line = list(color = "red", shape = "spline", width = 4))
#NewCasesAdmin2 <- NewCasesAdmin2 %>% layout(title = PlotTitle, xaxis = list(title = "Date"), yaxis = list(title = "New Confirmed COVID-19 Cases"))
NewCasesAdmin2 <- NewCasesAdmin2 %>% layout(title = PlotTitle, yaxis = list(title = "New COVID-19 Cases"))

#Plot Admin2 Trendline
PlotTitle <- paste("Percentage Change in 7-day Average New Cases in", Admin2Filter, "County,", ProvinceStateFilter)
Admin2Trend <- plot_ly(Admin2Agg, x = ~Date, y = ~PercentChangeDaily, name = "Percent Change", type = "scatter", mode = "markers")
Admin2Trend <- Admin2Trend %>% add_trace(y = ~MovingAvesPercentChange, name = "7-Day Avg of % Change", mode = "lines", line = list(color = "red", shape = "spline", width = 4))
Admin2Trend <- Admin2Trend %>% layout(title = PlotTitle, xaxis = list(title = "Date", range = c(onemonthago, tomorrow)), yaxis = list(title = "Percent Change in New Cases (in %)", range = c(-27, 27)))

#Plot ProvinceState New Cases
PlotTitle <- paste("7-Day Average New COVID-19 Cases in", ProvinceStateFilter, ",", CountryRegionFilter)
NewCasesProvinceState <- plot_ly(ProvinceStateAgg, x = ~Date, y = ~MovingAves7day, type = "bar", color = "blue")
NewCasesProvinceState <- NewCasesProvinceState %>% layout(title = PlotTitle, yaxis = list(title = "New COVID-19 Cases"))

#Plot ProvinceState Trendline
PlotTitle <- paste("Percentage Change in 7-day Average New Cases in", ProvinceStateFilter, ",", CountryRegionFilter)
ProvinceStateTrend <- plot_ly(ProvinceStateAgg, x = ~Date, y = ~PercentChangeDaily, name = "Percent Change", type = "scatter", mode = "markers")
ProvinceStateTrend <- ProvinceStateTrend %>% add_trace(y = ~MovingAvesPercentChange, name = "7-Day Avg of % Change", mode = "lines", line = list(color = "red", shape = "spline", width = 4))
ProvinceStateTrend <- ProvinceStateTrend %>% layout(title = PlotTitle, xaxis = list(title = "Date", range = c(onemonthago, tomorrow)), yaxis = list(title = "Percent Change in New Cases (in %)", range = c(-27, 27)))
ProvinceStateTrend

#Plot CountryRegion New Cases
PlotTitle <- paste("New COVID-19 Cases Over Time in", CountryRegionFilter)
NewCasesCountryRegion <- plot_ly(CountryRegionAgg, x = ~Date, y = ~NewConfirmed, type = "bar")
NewCasesCountryRegion <- NewCasesCountryRegion %>% layout(title = PlotTitle)

#Plot CountryRegion Trendline
PlotTitle <- paste("Percentage Change in 7-day Average New Cases in", CountryRegionFilter)
CountryRegionTrend <- plot_ly(CountryRegionAgg, x = ~Date, y = ~PercentChangeDaily, name = "Percent Change", type = "scatter", mode = "markers")
CountryRegionTrend <- CountryRegionTrend %>% add_trace(y = ~MovingAvesPercentChange, name = "7-Day Avg of % Change", mode = "lines", line = list(color = "red", shape = "spline", width = 4))
CountryRegionTrend <- CountryRegionTrend %>% layout(title = PlotTitle, xaxis = list(title = "Date", range = c(onemonthago, tomorrow)), yaxis = list(title = "Percent Change in New Cases (in %)", range = c(-27, 27)))
CountryRegionTrend

#Plot ProvinceState Counties Pie Chart
PlotTitle <- paste("New Cases by County in", ProvinceStateFilter, "(", today, ")")

ProvinceStateNewCasesLocations <- filter(JohnsHopkinsProvinceState, Date == today)
County <- ProvinceStateNewCasesLocations$Admin2
Date <- ProvinceStateNewCasesLocations$Date
NewCasesByCounty <- ProvinceStateNewCasesLocations$NewCasesByCounty
# NewCases <- (NewCases$Confirmed - lag(NewCases$Confirmed, default = 0))
# NewCasesDate <- JohnsHopkinsProvinceState$Date
# NewCasesDate <- data.frame(NewCasesDate, NewCases)
# NewCasesDate <- filter(NewCasesDate, NewCasesDate == today)
# NewCases <- NewCasesDate$NewCases
ProvinceStateNewCasesLocations <- data.frame(Date, County, NewCasesByCounty)
ProvinceStateNewCasesLocations <- filter(ProvinceStateNewCasesLocations, NewCasesByCounty > 0)
ProvinceStatePieChart <- plot_ly(ProvinceStateNewCasesLocations, labels = ~County, values = ~NewCasesByCounty, type = "pie")
ProvinceStatePieChart <- ProvinceStatePieChart %>% layout(title = PlotTitle,
                                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
)

# Define server logic
shinyServer(function(input, output) {
  
  #Plot needs to go HERE:
  output$CasesAdmin2 <- renderPlotly(CasesAdmin2)
  output$CasesProvinceState <- renderPlotly(CasesProvinceState)
  output$CasesCountryRegion <- renderPlotly(CasesCountryRegion)
  output$NewCasesAdmin2 <- renderPlotly(NewCasesAdmin2)
  output$NewCasesProvinceState <- renderPlotly(NewCasesProvinceState)
  output$NewCasesCountryRegion <- renderPlotly(NewCasesCountryRegion)
  output$ProvinceStatePieChart <- renderPlotly(ProvinceStatePieChart)
  output$Admin2Trend <- renderPlotly(Admin2Trend)
  output$ProvinceStateTrend <- renderPlotly(ProvinceStateTrend)
  output$CountryRegionTrend <- renderPlotly(CountryRegionTrend)

})











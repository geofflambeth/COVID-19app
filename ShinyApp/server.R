# setwd("../csse_covid_19_data/csse_covid_19_daily_reports/")

# install.packages("plyr")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("plotly")
# install.packages("lubridate")
# install.packages("shiny")
# install.packages("accelerometry")

library("plyr")                                                   # Load plyr package
library("dplyr")                                                  # Load dplyr package
library("readr")                                                  # Load readr package
library("plotly")
library("lubridate")
library("shiny")
library("accelerometry")
library("scales")

# JohnsHopkinsAll <- list.files(pattern = "*.csv", full.names = TRUE) #Collect filenames from the working directory folder
# JohnsHopkinsAll <- lapply(JohnsHopkinsAll,function(i){           #Collect all data together into individual data frames
#     read.csv(i, header=TRUE)
# })
# JohnsHopkinsAll <- bind_rows(JohnsHopkinsAll)                       #Bind all data frames together

JohnsHopkinsUS <- filter(JohnsHopkinsAll, Country_Region == "US")
JohnsHopkinsUS_NM <- filter(JohnsHopkinsUS, Province_State == "New Mexico")
JohnsHopkinsUS_NM_SF <- filter(JohnsHopkinsUS_NM, Admin2 == "Santa Fe")



#NEW MEXICO AGGREGATES AND VIZ
#Insert dataframe to start analysis for NM...
NewData <- JohnsHopkinsUS_NM
#Correct date format
a <- ymd_hms(NewData$Last_Update)
b <- mdy_hm(NewData$Last_Update)
a[is.na(a)] <- b[!is.na(b)]
Date <- a
#Add and aggregate content by date...
ConfirmedCases <- NewData$Confirmed
Deaths <- NewData$Deaths
ActiveCases <- NewData$Active
NewData <- data.frame(Date, ConfirmedCases, Deaths, ActiveCases)
Data <- aggregate(NewData$Confirmed, by = list(NewData$Date), FUN = sum)
Date <- Data$Group.1
ConfirmedCases <- Data$x
Data <- aggregate(NewData$Deaths, by = list(NewData$Date), FUN = sum)
Deaths <- Data$x
Data <- aggregate(NewData$Active, by = list(NewData$Date), FUN = sum)
Active <- Data$x
NewConfirmed = ConfirmedCases - lag(ConfirmedCases, default = 0)
MovingAvesTemp <- c(NA, NA, NA, NA, NA, NA)
MovingAves7day <- movingaves(x = NewConfirmed, window = 7)
MovingAves7day <- append(MovingAvesTemp, MovingAves7day)
MovingAvesTemp <- c(NA)
MovingAves2day <- movingaves(x = NewConfirmed, window = 2)
MovingAves2day <- append(MovingAvesTemp, MovingAves2day)
#Create Moving Ave trend for percentage change on 7-day moving average
MovingAvesTemp <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
#Provide new name for data here:
NewData <- data.frame(Date, ConfirmedCases, Deaths, Active, NewConfirmed, MovingAves7day, MovingAves2day, Trendline)
JohnsHopkinsUS_NM_Aggregate <- NewData

#Calculate moving average for new cases...

#Plot Aggregate
CasesUS_NM <- plot_ly(JohnsHopkinsUS_NM_Aggregate, x = ~Date, y = ~ConfirmedCases, name = "Confirmed Cases", type = "scatter", mode = "lines", line = list(color = "blue", width = 4))
CasesUS_NM <- CasesUS_NM %>% add_trace(y = ~Active, name = "Active Cases", mode = "lines", line = list(color = "orange", width = 4))
CasesUS_NM <- CasesUS_NM %>% add_trace(y = ~Deaths, name = "Deaths", mode = "lines", line = list(color = "red", width = 4))
CasesUS_NM <- CasesUS_NM %>% layout(title = "COVID-19 Cases in New Meico", xaxis = list(title = "Date"), yaxis = list(title = "Cases"))

#Plot New Cases
NewCasesUS_NM <- plot_ly(JohnsHopkinsUS_NM_Aggregate, x = ~Date, y = ~NewConfirmed, name = "New Cases", type = "bar")
NewCasesUS_NM <- NewCasesUS_NM %>% layout(title = "New COVID-19 Cases in New Mexico")

#Plot 7-day moving averages of cases
NewCasesUS_NMAves <- plot_ly(JohnsHopkinsUS_NM_Aggregate, x = ~Date, y = ~MovingAves7day, type = "bar", color = I("blue"))
NewCasesUS_NMAves <- NewCasesUS_NMAves %>% layout(title = "7-Day Moving Average of COVID-19 Cases in New Mexico", yaxis = list(title = "Moving Average of New Cases"))

###Aggregate and plot locations of new cases in New Mexico###
NewData <- JohnsHopkinsUS_NM
#Correct Dates
a <- ymd_hms(NewData$Last_Update)
b <- mdy_hm(NewData$Last_Update)
a[is.na(a)] <- b[!is.na(b)]
NewData$Last_Update <- a
#Simplify date line
today <- format(Sys.Date(), "%Y-%m-%d 00:00:00")
today <- ymd_hms(today)
NewData$Last_Update <- format(NewData$Last_Update, "%Y-%m-%d 00:00:00")
NewData$Last_Update <- ymd_hms(NewData$Last_Update)
#Order by county and calculate change in confirmed cases
NewData <- NewData[order(NewData$Admin2),]
NewData$NewConfirmed <- NewData$Confirmed - lag(NewData$Confirmed, default = 0)
#Filter to yesterday's date ONLY
NM_New_Cases_Location <- filter(NewData, Last_Update == today)
County <- NM_New_Cases_Location$Admin2
Date <- NM_New_Cases_Location$Last_Update
NewCases <- NM_New_Cases_Location$NewConfirmed
NM_New_Cases_Location <- data.frame(Date, County, NewCases)
NM_New_Cases_Location <- filter(NM_New_Cases_Location, NewCases > 0)

#Create New Cases Pie Chart with Plotly
NM_New_Cases <- plot_ly(NM_New_Cases_Location, labels = ~County, values = ~NewCases, type = "pie")
NM_New_Cases <- NM_New_Cases %>% layout(title = "New COVID-19 Cases by County, Today",
                                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
)

#Imitate Santa Fe New Meixcan moving averages plot
SFNMdata <- JohnsHopkinsUS_NM_Aggregate
SFNMdata$PChangeTotalCases <- SFNMdata$NewConfirmed / lag(SFNMdata$ConfirmedCases, default = 0) * 100
# SFNMdata$PChangeTotalCases <- label_percent()(SFNMdata$PChangeTotalCases)
SFNMdata$PChangeNewCases <- ((SFNMdata$NewConfirmed - lag(SFNMdata$NewConfirmed, default = 0)) / lag(SFNMdata$NewConfirmed, default = 0)) * 100
SFNMdata$PChangeNewCasesMovingAve <- ((SFNMdata$MovingAves7day - lag(SFNMdata$MovingAves7day, default = 0)) /lag(SFNMdata$MovingAves7day, default = 0)) * 100
Trendline <- movingaves(x = SFNMdata$PChangeNewCasesMovingAve, window = 7)
# SFNMdata$PChangeNewCases <- label_percent()(SFNMdata$PChangeNewCases)
# MovingAvesTemp <- c(NA, NA, NA, NA, NA, NA)
# MovingAves <- movingaves(x = SFNMdata$PChangeTotalCases, window = 7)
# MovingAves <- append(MovingAvesTemp, MovingAves)
# SFNMdata$PchangeTotalCasesMovingAverage <- MovingAves

#Plot
SFNMplot <- plot_ly(SFNMdata, x = ~Date, y = ~PChangeTotalCases, type = "bar", color = I("orange"))
SFNMplot <- SFNMplot %>% layout(title = "Percent Change Compared to Prior Day Total Cases (Imitate the New Mexican)", yaxis = list(title = "Percentage Change (in %)"))
SFNMplot
SFNMRecplot <- plot_ly(SFNMdata, x = ~Date, y = ~PChangeNewCasesMovingAve, type = "bar", color = I("orange"))
SFNMRecplot <- SFNMRecplot %>% add_trace(y = ~Trendline, name = "Trendline", type = "scatter", mode = "lines", line = list(color = "blue", width = 4))
SFNMRecplot <- SFNMRecplot %>% layout(title = "Percent Change of 7-day Moving Averages Compared to Prior Day (Is this better?)", yaxis = list(title = "Percentage Change (in %)"))
SFNMRecplot


#SANTA FE AGGREGATES AND VIZ
#Insert dataframe to start analysis for NM...
NewData <- JohnsHopkinsUS_NM_SF
#Correct date format
a <- ymd_hms(NewData$Last_Update)
b <- mdy_hm(NewData$Last_Update)
a[is.na(a)] <- b[!is.na(b)]
Date <- a
#Add and aggregate content by date...
ConfirmedCases <- NewData$Confirmed
Deaths <- NewData$Deaths
ActiveCases <- NewData$Active
NewData <- data.frame(Date, ConfirmedCases, Deaths, ActiveCases)
Data <- aggregate(NewData$Confirmed, by = list(NewData$Date), FUN = sum)
Date <- Data$Group.1
ConfirmedCases <- Data$x
Data <- aggregate(NewData$Deaths, by = list(NewData$Date), FUN = sum)
Deaths <- Data$x
Data <- aggregate(NewData$Active, by = list(NewData$Date), FUN = sum)
Active <- Data$x
NewConfirmed = ConfirmedCases - lag(ConfirmedCases, defaul = 0)
MovingAvesTemp <- c(NA, NA, NA, NA, NA, NA)
MovingAves <- movingaves(x = NewConfirmed, window = 7)
MovingAves <- append(MovingAvesTemp, MovingAves)
NewData <- data.frame(Date, ConfirmedCases, Deaths, Active, NewConfirmed, MovingAves)
#Provide new name for data here:
JohnsHopkinsUS_NM_SF_Aggregate <- NewData

#Plot Aggregate
CasesUS_NM_SF <- plot_ly(JohnsHopkinsUS_NM_SF_Aggregate, x = ~Date, y = ~ConfirmedCases, name = "Confirmed Cases", type = "scatter", mode = "lines", line = list(color = "blue", width = 4))
CasesUS_NM_SF <- CasesUS_NM_SF %>% add_trace(y = ~Active, name = "Active Cases", mode = "lines", line = list(color = "orange", width = 4))
CasesUS_NM_SF <- CasesUS_NM_SF %>% add_trace(y = ~Deaths, name = "Deaths", mode = "lines", line = list(color = "red", width = 4))
CasesUS_NM_SF <- CasesUS_NM_SF %>% layout(title = "COVID-19 Cases in Santa Fe, NM", xaxis = list(title = "Date"), yaxis = list(title = "Cases"))

#Plot New Cases
NewCasesUS_NM_SF <- plot_ly(JohnsHopkinsUS_NM_SF_Aggregate, x = ~Date, y = ~NewConfirmed, type = "bar")
NewCasesUS_NM_SF <- NewCasesUS_NM_SF %>% layout(title = "New COVID-19 Cases in Santa Fe, NM")

#Plot 7-day moving averages of cases
NewCasesUS_NM_SFAves <- plot_ly(JohnsHopkinsUS_NM_SF_Aggregate, x = ~Date, y = ~MovingAves, type = "bar", color = I("orange"))
NewCasesUS_NM_SFAves <- NewCasesUS_NM_SFAves %>% layout(title = "7-Day Moving Average of COVID-19 Cases in Santa Fe, NM", yaxis = list(title = "Moving Average of New Cases"))
NewCasesUS_NM_SFAves
########## DONE ##########

#UNITED STATES AGGREGATES AND VIZ
#Insert dataframe to start analysis for NM...
NewData <- JohnsHopkinsUS
#Correct date format
a <- ymd_hms(NewData$Last_Update)
b <- mdy_hm(NewData$Last_Update)
a[is.na(a)] <- b[!is.na(b)]
Date <- a
#Add and aggregate content by date...
ConfirmedCases <- NewData$Confirmed
Deaths <- NewData$Deaths
ActiveCases <- NewData$Active
NewData <- data.frame(Date, ConfirmedCases, Deaths, ActiveCases)
Data <- aggregate(NewData$Confirmed, by = list(NewData$Date), FUN = sum)
Date <- Data$Group.1
ConfirmedCases <- Data$x
Data <- aggregate(NewData$Deaths, by = list(NewData$Date), FUN = sum)
Deaths <- Data$x
Data <- aggregate(NewData$Active, by = list(NewData$Date), FUN = sum)
Active <- Data$x
NewConfirmed = ConfirmedCases - lag(ConfirmedCases, defaul = 0)
MovingAvesTemp <- c(NA, NA, NA, NA, NA, NA)
MovingAves <- movingaves(x = NewConfirmed, window = 7)
MovingAves <- append(MovingAvesTemp, MovingAves)
NewData <- data.frame(Date, ConfirmedCases, Deaths, Active, NewConfirmed, MovingAves)
#Provide new name for data here:
JohnsHopkinsUS_Aggregate <- NewData

#Plot Aggregate
CasesUS <- plot_ly(JohnsHopkinsUS_Aggregate, x = ~Date, y = ~ConfirmedCases, name = "Confirmed Cases", type = "scatter", mode = "lines", line = list(color = "blue", width = 4))
CasesUS <- CasesUS %>% add_trace(y = ~Active, name = "Active Cases", mode = "lines", line = list(color = "orange", width = 4))
CasesUS <- CasesUS %>% add_trace(y = ~Deaths, name = "Deaths", mode = "lines", line = list(color = "red", width = 4))
CasesUS <- CasesUS %>% layout(title = "COVID-19 Cases in the U.S.", xaxis = list(title = "Date"), yaxis = list(title = "Cases"))

#Plot New Cases
NewCasesUS <- plot_ly(JohnsHopkinsUS_Aggregate, x = ~Date, y = ~NewConfirmed, type = "bar")
NewCasesUS <- NewCasesUS %>% layout(title = "New COVID-19 Cases in the United States")
########## DONE ##########



# Define server logic
shinyServer(function(input, output) {

#Plot needs to go HERE:
output$CasesUS_NM_SF <- renderPlotly(CasesUS_NM_SF)
output$CasesUS_NM <- renderPlotly(CasesUS_NM)
output$CasesUS <- renderPlotly(CasesUS)
output$NewCasesUS_NM_SF <- renderPlotly(NewCasesUS_NM_SF)
output$NewCasesUS_NM <- renderPlotly(NewCasesUS_NM)
output$NewCasesUS <- renderPlotly(NewCasesUS)
output$NM_New_Cases_Piechart <- renderPlotly(NM_New_Cases)
output$SFNMplot <- renderPlotly(SFNMplot)
output$SFNMRecplot <- renderPlotly(SFNMRecplot)
output$NewCasesUS_NMAves <- renderPlotly(NewCasesUS_NMAves)


})

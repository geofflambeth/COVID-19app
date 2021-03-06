library(shiny)
library(plotly)

shinyUI(fluidPage(
    includeCSS("www/custom.css"),

    # Application title
    titlePanel("COVID-19 Data Dashboard by Geoff Lambeth - Data Courtesy of Johns Hopkins University", windowTitle = "COVID-19"),


    # Show a plot of the generated distribution
    fluidRow(
        plotlyOutput("CasesAdmin2")
    ),
    fluidRow(
        column(
            6,
            plotlyOutput("NewCasesAdmin2")
        ),
        column(
            6,
            plotlyOutput("ProvinceStatePieChart")
        )
    ),
    fluidRow(
        column(
            6,
            plotlyOutput("Admin2Trend")
        ),
        column(
            6,
            plotlyOutput("ProvinceStateTrend")
        ),

    ),
    fluidRow(
        column(
            6,
            plotlyOutput("CasesProvinceState")
        ),
        column(
            6,
            plotlyOutput("NewCasesProvinceState")
        ),
    )
#    fluidRow(
#        column(
#            6,
#            plotlyOutput("CasesUS_NM")
#        ),
#        column(
#            6,
#            plotlyOutput("NewCasesUS_NM")
#        )
#   )
 #   mainPanel(
 #       plotlyOutput("CasesUS_NM_SF")
 #   ),
 #   mainPanel(
 #       plotlyOutput("CasesUS_NM")
 #   )
))

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Geoff's COVID-19 Dashboard", windowTitle = "COVID-19"),

    # Show a plot of the generated distribution
    fluidRow(
        plotlyOutput("CasesUS_NM_SF")
    ),
    fluidRow(
        column(
            6,
            plotlyOutput("NewCasesUS_NM_SF")
        ),
        column(
            6,
            plotlyOutput("NM_New_Cases_Piechart")
        )
    ),
    fluidRow(
        column(
            6,
            plotlyOutput("SFNMplot")
        ),
        column(
            6,
            plotlyOutput("SFNMRecplot")
        )
    ),
    fluidRow(
        column(
            6,
            plotlyOutput("CasesUS_NM")
        ),
        column(
            6,
            plotlyOutput("NewCasesUS_NM")
        )
    )
 #   mainPanel(
 #       plotlyOutput("CasesUS_NM_SF")
 #   ),
 #   mainPanel(
 #       plotlyOutput("CasesUS_NM")
 #   )
))


#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(DBI)
library(RSQLite)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(DT)



# Define UI for application that draws a histogram
shinyUI(fluidPage(navbarPage(
  title = 'HiBC: Human intestinal Bacterial Collection',
  windowTitle = 'Navigation Bar', 
  position = 'fixed-top', 
  collapsible = TRUE, 
  theme = shinytheme('cosmo'), 
  tabPanel(title = 'Cultivation'),
  tabPanel(title = 'Overview')
),





  titlePanel('Overview'),
  fluidRow(
    align = 'center',
    ### well panel 1
    column(
      width = 4,
      wellPanel(
        style = 'background-color: #696969; color: #ffffff;',
        h4('Number of isolates'),
        htmlOutput(
          outputId = 'nIsolates'
        )
      )
    ),
    ### well panel 2
    column(
      width = 4,
      wellPanel(
        style = 'background-color: #696969; color: #ffffff; bold = TRUE',
        h4('Number of phyla'),
        htmlOutput(
          outputId = 'nPhyla'
        )
      )
    ),
    ### well panel 3
    column(
      width = 4,
      wellPanel(
        style = 'background-color: #696969; color: #ffffff;',
        h4('Number of species'),
        htmlOutput(
          outputId = 'nSpecies'
        )
      )
    )
  ),
  hr(),
    mainPanel(      dataTableOutput(
      outputId = 'Overview'
    )
  )






















))


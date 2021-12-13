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
  title = div(img(src="Logo.png", align = "left",height='40px',width='100px'),'Human intestinal Bacterial Collection'),
  
  windowTitle = 'Navigation Bar', 
  position = 'fixed-top', 
  collapsible = TRUE, 
  theme = shinytheme('cosmo'), 
  tabPanel(title = 'Overview',


  titlePanel('Overview'),
  h1(" "),
  h3("About HiBC"),
  h4("HiBC is a collection of bacterial strains, isolated from the human gut for which 16S rRNA, genome and cultivation conditions are available."),
  h4("At current, HiBC contains many novel strains which we aim to describe in our ongoing work."),
  h4("If you make use of HiBC please cite: Masson et al (TBA)"),
  
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





  ),
  
  
  
  
  
  
tabPanel('Cultivation',
         h1(" "),
         h2("Second level title"),
         h3("Cultivation overview"),
         h4("HiBC represents the culumative work of multiple projects, hence a range of cultivation conditions have been used to isolate and maintain these strains."),
         h4("As we hope these strains will be utilised by the community, below we provide information on the current culture conditions used to maintain each strain."),
         fluidRow(
           align = 'center',
           ### well panel 1
           column(
             width = 4,
             wellPanel(
               style = 'background-color: #696969; color: #ffffff;',
               h4('Countries of origin'),
               htmlOutput(
                 outputId = 'nCountries'
               )
             )
           ),
           ### well panel 2
           column(
             width = 4,
             wellPanel(
               style = 'background-color: #696969; color: #ffffff; bold = TRUE',
               h4('Number of addatives'),
               htmlOutput(
                 outputId = 'nAddatives'
               )
             )
           ),
           ### well panel 3
           column(
             width = 4,
             wellPanel(
               style = 'background-color: #696969; color: #ffffff;',
               h4('Number of media'),
               htmlOutput(
                 outputId = 'nMedia'
               )
             )
           )
         ),
         hr(),           
           mainPanel(      dataTableOutput(
             outputId = 'CultivationDB'
           )
           )

),





















tabPanel('Genomes',
         h1(" "),
         h2("Second level title"),
         h3("Sequence overview"),
         h4("The main goal of HiBC was to provide high-quality paired information for a collection of human gut isolates. As such, both full length 16S rRNA gene sequuences and genomes are available for every isolate."),
         h4(""),
         
         fluidRow(
           mainPanel(      dataTableOutput(
             outputId = 'GenomeDB'
           )
           )
           
         ))






)))
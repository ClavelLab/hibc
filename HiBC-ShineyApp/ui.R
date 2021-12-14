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
library(shinyforms)


##### Questionaire

# Questions listed
questions <- list(
  list(id = "name", type = "text", title = "Name", mandatory = TRUE),
  list(id = "association", type = "text", title = "Academic association", mandatory = TRUE),
  list(id = "email", type = "text", title = "Email address", mandatory = TRUE),
  list(id = "isolate", type = "text", title = "Isolate name (Lab-ID)", mandatory = TRUE),
  list(id = "terms", type = "checkbox", title = "I agree to the terms", mandatory = TRUE)
)

# Create the form
formInfo <- list(  password = "Aachen28",
  id = "requestinfo",
  reset = TRUE,
  questions = questions,
  storage = list(
    # Right now, only flat file storage is supported
    type = STORAGE_TYPES$FLATFILE,
    # The path where responses are stored
    path = "responses"
  )
)




#### Create local static version of database for listing taxa
db <- dbConnect(RSQLite::SQLite(), dbname = "hiBC.db")


prodtype <- dbGetQuery(
  db,
  statement = 'SELECT 
  i.Clavel_Lab_ID as "Lab ID",
  i.DSM as "DSMZ ID",
  d.State as "Status at DSMZ",
  
  CASE WHEN  ts.Species  IS NOT NULL
  /* the manually selected family is taken into account */
  THEN  tg.genus  ||" " ||  ts.Species 
  /* the automatically detected family based on the genus is displayed */
  ELSE   tf.family  ||" " ||  tg.genus 
  END AS "Currently assigned name" ,
  
  
  /* Taxonomic information */
  
  CASE WHEN  tp.Phylum IS NOT NULL
  /* the manually selected family is taken into account */
  THEN tp.Phylum
  /* the automatically detected family based on the genus is displayed */
  ELSE  tp2.Phylum
  END AS "Phylum" ,
  
  
  CASE WHEN  tf.family IS NOT NULL
  /* the manually selected family is taken into account */
  THEN tf.family
  /* the automatically detected family based on the genus is displayed */
  ELSE  tf2.Family
  END AS "Family" ,
  
  tg.genus as "Genus",
  ts.Species as "Species", /* Tax species for any reason  */
  
  
  
  
  /* Additional information */
  i.Risk_Group as "Risk group"
  
  
  
  
  
  from ISOLATE i
  left join TAXONOMY t on t.pk = i.FK_TAXONOMY
  left join TAX_FAM tf on tf.pk = t.FK_TAX_FAM
  left join TAX_SPE ts on ts.pk = t.FK_TAX_SPE
  left join TAX_GEN tg on tg.pk = t.FK_TAX_GEN
  left join TAX_PHY tp on tp.pk = tf.FK_TAX_PHY
  left join TAX_FAM tf2 on tf2.pk = tg.FK_TAX_FAM
  left join TAX_PHY tp2 on tp2.pk = tf2.FK_TAX_PHY
  left join GENOME g on g.pk = i.FK_GENOME
  left join True_False true on true.pk = g.FK_GENOME_PRESENT
  left join DSMZ d on d.PK = i.FK_DSMZ
  left join ISOLATION iso on iso.pk = i.FK_ISOLATION 
  left join Location loc on loc.pk = iso.FK_LOCATION
  left join Country c on c.pk = loc.FK_COUNTRY
  left join rRNA rna on rna.pk = i.FK_rRNA
  
  left join ISOLATION_MEDIA iso_med on iso_med.FK_ISOLATION = iso.pk
  left join MEDIA m on m.pk = iso_med.FK_MEDIA
  
  left join MALDI mal on mal.pk = i.FK_MALDI')









##### 
#
#
#
#
#      The start of the interface
#
#
#
#
############################
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
  h3(div(img(src="Banner.png", align = "centre",height='75px',width='1800px')),align = "centre"),

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
  sidebarLayout(
    sidebarPanel("Taxa",
      selectInput(
        inputId = 'Overview',
        label = 'Phyla:',
        choices = prodtype$Phylum,
        selected = ''
      ),
      selectInput(
        inputId = 'Overview',
        label = 'Families:',
        choices = prodtype$Family,
        selected = ''
      ),
      selectInput(
        inputId = 'Overview',
        label = 'Genera:',
        choices = prodtype$Genus,
        selected = ''
      ),
      selectInput(
        inputId = 'Overview',
        label = 'Species:',
        choices = prodtype$"Currently assigned name",
        selected = ''
      ),
      br(),
    ),    mainPanel(      dataTableOutput(
      outputId = 'Overview'

    )
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
         hr(),       align="center",    
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
         
         fluidRow(align="center",
           mainPanel(      dataTableOutput(
             outputId = 'GenomeDB'
           )
           )
           
         )),



tabPanel('Requests',
         h1(" "),
         h2("Second level title"),
         h3("Requesting isolates"),
         h4("We aim to provide access to the isolates within HiBC to the research community. Due to the limited number of stocks avaialble for each isolate as well as the time involved in their maintenance we do not provide access to isolates which have already been deposited at a national culture collection (DSMZ, JCM etc.)."),
         h4("Requests will be limited based on our schedule and the remaining availability of prepared cryo-stocks of each isolate."),
         
         fluidRow(align="center",
           formUI(formInfo)
           
         ))



)))
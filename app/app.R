library(shiny)
library(shinythemes)
library(DT)


# Define UI
ui <- navbarPage(
  lang = "en",
  title = p(icon("bacteria"), "hibc"),
  windowTitle = "hibc: The Human Intestinal Bacterial Collection",
  theme = shinytheme("cosmo"),
  position = "fixed-top",
  footer = list(
    column(hr(),
      p("hibc. Copyright Clavel Lab (2023)", ),
      align = "center", width = 12
    )
  ),
  id = "navbar",
  tabPanel("Overview",
    value = "Overview",
    column(
      width = 12, align = "center",
      tags$style(type = "text/css", "body {padding-top: 70px;}"),
      h1(icon("bacteria"), "hibc: The Human Intestinal Bacterial Collection")
    ),
    column(
      width = 8, offset = 2,
      fluidRow(
        column(
          width = 6,
          p("Placeholder for an large coloured button indicating the number of isolates in the hibc")
        ),
        column(
          width = 6,
          p("Placeholder for an large coloured button indicating the number of species in the hibc")
        )
      ),
      fluidRow(
        h2("About hibc", align = "center"),
        p(
          "This is a collection of bacterial strains, isolated from the human gut for which 16S rRNA gene sequences, genome sequences and cultivation conditions are made available to the research community.",
          "Many of these isolates are novel at different taxonomic rank."
        ),
        h2("How to navigate the resource", align = "center"),
        p("We provide different lenses through which researchers can explore interactively the hibc:"),
        tags$ul(
          tags$li("the taxonomy of isolates"),
          tags$li("the cultivation metadata of the isolates"),
          tags$li("the genomes metadata of the isolates")
        ),
        p(
          "If you make use of the hibc resource, please cite our work as:",
          tags$blockquote("Hitch TCA and Masson J et al.")
        ),
        align = "left"
      ),
    )
  ),
  tabPanel("Taxonomy"),
  tabPanel("Cultivation"),
  tabPanel("Genomes"),
)


# Define server logic
server <- function(input, output, session) {

}

# Run the application
shinyApp(ui = ui, server = server)

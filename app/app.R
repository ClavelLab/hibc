library(shiny)
library(bslib)
library(DT)
library(tidyverse)

# Load the hibc table
hibc_data <- read_delim("2023-01-12.Merged_HiBC.tsv", delim = "\t", show_col_types = FALSE)

# Define UI
ui <- navbarPage(
  lang = "en",
  title = span(icon("bacteria"), "hibc"),
  windowTitle = "hibc: The Human Intestinal Bacterial Collection",
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    base_font = font_google("Source Sans Pro"),
    code_font = font_google("Source Code Pro")
  ) %>%
    bs_add_rules(
      ':target:before { content: "";  display: block;  height: 80px;  margin: -20px 0 0;}'
    ),
  position = "fixed-top",
  footer = list(
    column(hr(),
      p("hibc. Copyright Clavel Lab (2023)", ),
      align = "center", width = 12
    )
  ),
  id = "navbar",
  tabPanel(
    "Overview",
    column(
      width = 12, align = "center",
      tags$style(type = "text/css", "body {padding-top: 70px;}"),
      h1("hibc: The Human Intestinal Bacterial Collection")
    ),
    column(
      width = 8, offset = 2,
      layout_column_wrap(
        width = 1 / 2,
        value_box(
          # showcase = icon("bacteria"),
          showcase = tags$i(class = "fas fa-bacteria", style = "font-size: 96px"),
          title = "hibc contains",
          value = "224",
          theme_color = "primary",
          p("isolates"),
        ),
        value_box(
          showcase = tags$i(class = "fas fa-bugs", style = "font-size: 96px"),
          title = "hibc contains",
          value = "123",
          theme_color = "secondary",
          p("isolates")
        )
      ),
      br(),
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
  tabPanel(
    "Taxonomy",
    column(
      width = 8, offset = 2, align = "center",
      tags$style(type = "text/css", "body {padding-top: 70px;}"),
      h2("List of the hibc isolates and the associated taxonomy", align = "center"),
      br(),
      fluidRow(
        DT::dataTableOutput("taxonomy")
      ),
    )
  ),
  tabPanel("Cultivation"),
  tabPanel("Genomes"),
)


# Define server logic
server <- function(input, output, session) {
  # Taxonomy table
  output$taxonomy <- DT::renderDT(
    DT::datatable(
      hibc_data %>%
        select(StrainID, Species, `DSM no.`, Phylum, Family) %>%
        arrange(Species),
      filter = "top",
      extensions = "Responsive",
      selection = list(
        mode = "single",
        selected = "7",
        target = "row"
      )
    ) %>% formatStyle(columns = "Species", fontStyle = "italic"),
    server = TRUE
  )
}

# Run the application
shinyApp(ui = ui, server = server)

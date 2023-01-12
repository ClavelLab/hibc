library(shiny)
library(bslib)
library(DT)
library(tidyverse)

# Load the hibc table
hibc_data <- read_delim("2023-01-12.Merged_HiBC.tsv", delim = "\t", show_col_types = FALSE) %>%
  arrange(Species)

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
    fluidRow(
      column(
        width = 4, offset = 2,
        tags$style(type = "text/css", "body {padding-top: 70px;}"),
        h4("Taxonomy of the hibc isolates"),
        p("Browse through the complete list of the isolates in the table below."),
        p(
          "If you want to have more information on a specific isolate,",
          "please select your isolate in the table and click on the button on the right.",
        ),
      ),
      column(
        width = 4, align = "center",
        tags$style(type = "text/css", "body {padding-top: 70px;}"),
        h4("An isolate of interest?"),
        br(),
        actionButton(
          inputId = "viewDetail", class = "go-to-top success",
          label = "Select a row and click me",
          width = "150px",
        )
      )
    ),
    column(
      width = 8, offset = 2, align = "center",
      fluidRow(
        DT::dataTableOutput("taxonomy")
      )
    )
  ),
  tabPanel(
    "Cultivation",
    fluidRow(
      column(
        width = 4, offset = 2,
        tags$style(type = "text/css", "body {padding-top: 70px;}"),
        h4("Cultivation of the hibc isolates"),
        p("Browse through the metadata related to the cultivation of the isolates in the table below."),
      ),
      column(
        width = 4, align = "center",
        tags$style(type = "text/css", "body {padding-top: 70px;}"),
        h4("Cultivation media in hibc"),
        br(),
        p("Placeholder for a horizontal bar chart displaying the media used")
      )
    ),
    column(
      width = 8, offset = 2, align = "center",
      fluidRow(
        DT::dataTableOutput("cultivation")
      )
    )
  ),
  tabPanel(
    "Genomes",
    column(
      width = 8, offset = 2,
      tags$style(type = "text/css", "body {padding-top: 70px;}"),
      h4("Assemblies of the hibc isolates"),
      p("Explore the genome assemblies of the isolates via the two interactive plots and the table below."),
    ),
    fluidRow(
      column(
        width = 4, offset = 2, align = "center",
        tags$style(type = "text/css", "body {padding-top: 70px;}"),
        h4("Cultivation media in hibc"),
        br(),
        p("Placeholder for a interactive plot of genome length vs N50")
      ),
      column(
        width = 4, align = "center",
        tags$style(type = "text/css", "body {padding-top: 70px;}"),
        h4("Cultivation media in hibc"),
        br(),
        p("Placeholder for a interactive plot of genome length vs N50")
      )
    ),
    column(
      width = 8, offset = 2, align = "center",
      fluidRow(
        DT::dataTableOutput("genome")
      )
    )
  ),
  tabPanel("Details isolate",
    value = "detail",
    fluidRow(
      column(
        width = 8, offset = 2,
        h2("Details on the hibc isolate", textOutput("isolate_id", inline = T),
          id = "isolate-details", align = "center"
        ),

        # h3("Interaction name:", textOutput("int_name", inline = T), align = "center"),
        h4("Genome assembly"),
        column(width = 10, offset = 1, tableOutput("details_assembly")),
        # h4("Taxonomy and specificity"),
        # tags$ul(
        #   tags$li(textOutput("int_tax")),
        #   tags$li(textOutput("int_specificity"))
        # ),
        # h4("Interaction features"),
        # fluidRow(
        #   column(width = 5, offset = 1, h5("Dependencies"), tableOutput("dependencies_table")),
        #   column(width = 5, offset = 1, h5("Site"), tableOutput("site_table"))
        # ),
        # fluidRow(
        #   column(width = 5, offset = 1, h5("Habitat"), tableOutput("habitat_table")),
        #   column(width = 5, offset = 1, h5("Compounds"), tableOutput("compounds_table"))
        # )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  preview_hibc <- reactive({
    hibc_data
  })
  # Taxonomy table
  output$taxonomy <- DT::renderDT(
    DT::datatable(
      preview_hibc() %>%
        select(StrainID, Species, `DSM no.`, Phylum, Family),
      filter = "top",
      extensions = "Responsive",
      selection = list(
        mode = "single",
        selected = "7",
        target = "row"
      ),
      options = list(
        dom = "rtflp"
      )
    ) %>% formatStyle(columns = "Species", fontStyle = "italic"),
    server = TRUE
  )

  # Cultivation table
  output$cultivation <- DT::renderDT(
    preview_hibc() %>%
      select(StrainID, `Medium for best growth`, `Growth atm.`, `Incubation time`),
    filter = "top",
    extensions = "Responsive",
    options = list(
      dom = "rtflp"
    ),
    server = FALSE
  )

  # Genome table
  output$genome <- DT::renderDT(
    preview_hibc() %>%
      select(StrainID, genome_length, number_contig, N50, coverage, compl_score, contam_score),
    filter = "top",
    extensions = "Responsive",
    options = list(
      dom = "rtflp"
    ),
    server = TRUE
  )

  # Selected isolate
  output$isolate_id <- renderText({
    preview_hibc()[input$taxonomy_rows_selected, ] %>% pull("StrainID")
  })

  observe({
    req(input$taxonomy_rows_selected)
    # The action button needs to be observed to be able to register the different row selection
    updateActionButton(
      session, "viewDetail",
      paste(
        "Details on",
        preview_hibc()[input$taxonomy_rows_selected, ] %>%
          pull("StrainID")
      )
    )
  })

  output$contamination <- renderText({
    preview_hibc()[input$taxonomy_rows_selected, ] %>% pull("contam_score")
  })

  output$details_assembly <- renderTable(
    {
      # This function should not be ran before a row is selected.
      req(input$taxonomy_rows_selected)
      #
      preview_hibc() %>%
        select(genome_length, number_contig, N50, number_contig_below_1kb, max_contig_length) %>%
        .[input$taxonomy_rows_selected, ] %>%
        t()
    },
    rownames = T,
    colnames = F,
    na = "",
    hover = T,
    spacing = "xs",
    digits = 0
  )
}

# Run the application
shinyApp(ui = ui, server = server)

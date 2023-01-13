library(shiny)
library(bslib)
library(DT)
library(tidyverse)
library(cowplot)
library(plotly)

# Load the hibc table
hibc_data <- read_delim("2023-01-12.Merged_HiBC.tsv", delim = "\t", show_col_types = FALSE) %>%
  arrange(Species)

# Define UI
ui <- navbarPage(
  lang = "en",
  title = span(tags$img(src = "hibc.png", height = 40), "hibc"),
  windowTitle = "HiBC: The Human Intestinal Bacterial Collection",
  theme = bs_theme(
    version = 5,
    bootswatch = "sketchy",
    # primary = "#519c00",
    # secondary = "#F2BB05",
    # success = "#F0F0C9",
    # warning = "#CF5C36",
    # info = "#555358",
    # base_font = font_google("Source Sans Pro"),
    # code_font = font_google("Source Code Pro")
  ) %>%
    bs_add_rules(
      ':target:before { content: "";  display: block;  height: 80px;  margin: -20px 0 0;}'
    ),
  position = "fixed-top",
  header = list(
    tags$head(tags$style(
      HTML("
      .table.dataTable tbody td.active, .table.dataTable tbody tr.active td {
            background-color: var(--bs-success)!important;}
      ")
    ))
  ),
  footer = list(
    column(hr(),
      p("hibc. Copyright Clavel Lab (2023)", ),
      align = "center", width = 12
    )
  ),
  id = "navbar",
  tabPanel(
    "Overview",
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    column(
      width = 12, align = "center",
      tags$style(type = "text/css", "body {padding-top: 70px;}"),
      h1(tags$img(src = "hibc.png", height = 80), "HiBC: The Human Intestinal Bacterial Collection")
    ),
    column(
      width = 8, offset = 2,
      layout_column_wrap(
        width = 1 / 2,
        value_box(
          showcase = icon("bacteria", class = "fa-3x"),
          title = "Isolates:",
          value = tags$span(textOutput("no_isolates", inline = T), class = "h2 mb-2"),
          theme_color = "info",
          p("as of 2023-01-13")
        ),
        value_box(
          showcase = icon("bugs", class = "fa-3x"),
          title = "Species:",
          value = tags$span(textOutput("no_species", inline = T), class = "h2 mb-2"),
          theme_color = "warning",
          p("as of 2023-01-13")
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
          inputId = "viewDetail", class = "btn-info go-to-top",
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
        h4("Completion and contamination"),
        br(),
        plotlyOutput("plot_compl_contam", width = "400px", height = "400px")
      ),
      column(
        width = 4, align = "center",
        tags$style(type = "text/css", "body {padding-top: 70px;}"),
        h4("Genome size and assembly fragmentation"),
        br(),
        plotlyOutput("plot_N50_genome_size", width = "400px", height = "400px")
      )
    ),
    br(),
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
  # Numbers on the hibc dataset
  output$no_isolates <- renderText({
    preview_hibc() %>%
      pull(StrainID) %>%
      unique() %>%
      length()
  })
  output$no_species <- renderText({
    preview_hibc() %>%
      pull(Species) %>%
      unique() %>%
      length()
  })
  # Taxonomy table
  output$taxonomy <- DT::renderDT(
    DT::datatable(
      preview_hibc() %>%
        select(StrainID, Phylum, Family, Species, `DSM no.`),
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
  # Genomes plot: completion vs contamination
  output$plot_compl_contam <- renderPlotly({
    checkm <- ggplot(
      preview_hibc(),
      aes(
        x = compl_score, y = contam_score,
        label = StrainID
      )
    ) +
      geom_point(alpha = 0.5, size = 2, color = "#17a2b8") +
      geom_vline(xintercept = 90, linetype = "dashed") +
      geom_hline(yintercept = 5, linetype = "dashed") +
      geom_rug(alpha = 0.3) +
      scale_y_continuous(
        breaks = scales::breaks_extended(n = 7),
        expand = expansion(mult = .08)
      ) +
      scale_x_continuous(expand = expansion(mult = .08)) +
      labs(x = "Completion (CheckM)", y = "Contamination (CheckM)") +
      theme_cowplot()
    ggplotly(checkm)
  })
  # Genomes plot: N50 vs genome size
  output$plot_N50_genome_size <- renderPlotly({
    p_N50 <- ggplot(
      preview_hibc(),
      aes(
        x = genome_length, y = N50,
        label = StrainID
      )
    ) +
      geom_point(alpha = 0.5, size = 2, color = "#ffc107") +
      geom_hline(yintercept = 25000, linetype = "dashed") +
      geom_rug(alpha = 0.3) +
      scale_x_continuous(
        labels = scales::label_bytes(units = "MB"),
        breaks = scales::breaks_extended(n = 7),
        expand = expansion(mult = 0.08)
      ) +
      scale_y_continuous(
        labels = scales::label_bytes(units = "kB"),
        breaks = scales::breaks_extended(n = 7), expand = expansion(mult = 0.08)
      ) +
      labs(x = "Genome size", y = "N50") +
      theme_cowplot()
    ggplotly(p_N50)
  })
  # Selected isolate
  output$isolate_id <- renderText({
    preview_hibc()[input$taxonomy_rows_selected, ] %>% pull("StrainID")
  })

  observe({
    req(input$taxonomy_rows_selected)
    # The action button needs to be observed to be able to register the different row selection
    updateActionButton(
      session, "viewDetail",
      label = paste(
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
  # Navigation
  #
  observeEvent(input$viewDetail, {
    updateNavbarPage(session = session, inputId = "navbar", selected = "detail")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

library(shiny)
library(bslib)
library(DT)
library(tidyverse)
library(cowplot)
library(plotly)
library(thematic)
library(showtext)
library(shinycssloaders)

thematic_shiny(font = "auto")
options(spinner.type = 8, spinner.color = "#519c00")

# Load the hibc table
hibc_data <- read_delim("2023-01-13.Merged_HiBC.tsv", delim = "\t", show_col_types = FALSE) %>%
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
          inputId = "viewDetail", class = "btn-info fw-bold go-to-top",
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
        p(
          "Browse through the metadata related to the cultivation of the isolates",
          "in the table below and in the interactive figure on the left."
        ),
        p(
          "Note that when multiple cultivation media are indicated,",
          "the media for the best growth is the left-most media."
        ),
        p(
          "The figure indicates the most used media for the best growth of the isolates",
          textOutput("no_media", inline = T), "The media used only once are not shown",
          textOutput("no_media_once", inline = T)
        )
      ),
      column(
        width = 4, align = "center",
        tags$style(type = "text/css", "body {padding-top: 70px;}"),
        plotlyOutput("plot_media", height = "400px") %>% withSpinner()
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
      width = 8, offset = 2, align = "center",
      tags$style(type = "text/css", "body {padding-top: 70px;}"),
      h4("Assemblies of the hibc isolates"),
      p("Explore the genome assemblies of the isolates via the two interactive plots and the table below."),
      layout_column_wrap(
        width = 1 / 2,
        card(
          height = 500, full_screen = T,
          card_header("Completion and contamination"),
          card_body_fill(
            plotlyOutput("plot_compl_contam", height = "400px") %>% withSpinner()
          )
        ),
        card(
          height = 500, full_screen = T,
          card_header("Genome size and assembly fragmentation"),
          card_body_fill(
            plotlyOutput("plot_N50_genome_size", height = "400px") %>% withSpinner()
          )
        )
      )
    ),
    br(),
    column(
      width = 8, offset = 2, align = "center",
      fluidRow(
        DT::dataTableOutput("genome") %>% withSpinner()
      )
    )
  ),
  tabPanel("Details isolate",
    value = "detail",
    column(
      width = 8, offset = 2,
      h2("Details on the HiBC isolate", textOutput("isolate_id", inline = T),
        id = "isolate-details", align = "center"
      ),
      h4("Taxonomy"),
      p(
        "The isolate", textOutput("details_dsm_number", inline = T),
        "belongs to:", uiOutput("details_taxonomy", inline = T)
      ),
      h4("Cultivation and isolation metadata"),
      layout_column_wrap(
        width = "300px",
        card(
          class = "border-info", align = "center",
          card_header("Cultivation"),
          card_body(
            tableOutput("details_cultivation")
          )
        ),
        card(
          class = "border-warning", align = "center",
          card_header("Isolation"),
          card_body(
            tableOutput("details_isolation")
          )
        )
      ),
      h4("Genome assembly metadata"),
      layout_column_wrap(
        width = "300px",
        card(
          class = "border-success", align = "center",
          card_header("Genome"),
          card_body(
            tags$ul(
              tags$li("completion, contamination summaarised"),
              tags$li("coverage"),
              tags$li("quality type")
            )
          )
        ),
        card(
          class = "border-danger", align = "center",
          card_header("Assembly"),
          card_body(
            tableOutput("details_assembly")
          )
        ),
        card(
          class = "border-primary", align = "center",
          card_header("Workflow"),
          card_body(
            tags$ul(
              tags$li("workflow version and date"),
              tags$li("assembly software"),
              tags$li("md5sums and download button")
            )
          )
        )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  preview_hibc <- reactive({
    hibc_data
  })
  media <- reactive({
    preview_hibc() %>%
      mutate(best_media = gsub(";.*", "", `Medium for best growth`)) %>%
      count(best_media) %>%
      arrange(n) %>%
      mutate(best_media = factor(best_media, best_media))
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
  output$no_media <- renderText({
    media() %>%
      nrow() %>%
      paste0("(n = ", ., ").")
  })
  output$no_media_once <- renderText({
    media() %>%
      filter(n == 1) %>%
      nrow() %>%
      paste0("(n = ", ., ").")
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

  # Cultivation media plot
  output$plot_media <- renderPlotly({
    p_media <- media() %>%
      filter(!is.na(best_media) & n > 1) %>%
      ggplot(aes(x = best_media, y = n)) +
      geom_segment(aes(x = best_media, xend = best_media, y = 0, yend = n)) +
      geom_point(size = 2, color = "#28a745") +
      coord_flip() +
      labs(x = "", y = "Best media occurrence") +
      theme_cowplot()
    ggplotly(p_media)
  })
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
  output$details_dsm_number <- renderText({
    preview_hibc()[input$taxonomy_rows_selected, ] %>%
      select(StrainID, `DSM no.`) %>%
      mutate(text = if_else(is.na(`DSM no.`),
        StrainID, paste0(StrainID, " (DSM", `DSM no.`, ")")
      )) %>%
      pull(text)
  })
  output$details_taxonomy <- renderUI({
    # This function should not be ran before a row is selected.
    req(input$taxonomy_rows_selected)
    # Taxonomy information as list
    tax_list <- preview_hibc() %>%
      .[input$taxonomy_rows_selected, ] %>%
      select(Phylum, Family, Species, `DSM no.`) %>%
      as.list()
    tags$span(
      tax_list[["Phylum"]], "(Phylum),",
      tax_list[["Family"]], "(Family),",
      tags$em(tax_list[["Species"]], class = "text-success"), "(Species)."
    )
  })
  output$details_cultivation <- renderTable(
    {
      # This function should not be ran before a row is selected.
      req(input$taxonomy_rows_selected)
      #
      preview_hibc() %>%
        .[input$taxonomy_rows_selected, ] %>%
        select(`Growth atm.`, `Medium for best growth`, `Incubation time`, `Risk Group`) %>%
        t()
    },
    rownames = T,
    colnames = F,
    na = "",
    hover = T,
    spacing = "s",
  )
  output$details_isolation <- renderTable(
    {
      # This function should not be ran before a row is selected.
      req(input$taxonomy_rows_selected)
      #
      preview_hibc() %>%
        select(`Geographic location`, `Donor type`, `Gut region`, `Date of isolation (JJJJ-MM-DD)`) %>%
        rename(`Isolation date` = `Date of isolation (JJJJ-MM-DD)`) %>%
        relocate(`Gut region`, `Donor type`, `Isolation date`, `Geographic location`) %>%
        .[input$taxonomy_rows_selected, ] %>%
        t()
    },
    rownames = T,
    colnames = F,
    na = "",
    hover = T,
    spacing = "s",
    digits = 0
  )
  output$details_assembly <- renderTable(
    {
      # This function should not be ran before a row is selected.
      req(input$taxonomy_rows_selected)
      #
      preview_hibc() %>%
        select(genome_length, N50, number_contig, number_contig_below_1kb, max_contig_length) %>%
        .[input$taxonomy_rows_selected, ] %>%
        t()
    },
    rownames = T,
    colnames = F,
    na = "",
    hover = T,
    spacing = "xs",
    digits = 0,
    format.args = list(big.mark = " ")
  )
  # Navigation
  #
  observeEvent(input$viewDetail, {
    updateNavbarPage(session = session, inputId = "navbar", selected = "detail")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

library(shiny)
library(bslib)
library(DT)
library(tidyverse)
library(cowplot)
library(plotly)
library(thematic)
library(showtext)
library(shinycssloaders)
library(rclipboard)
library(aws.s3)
library(conductor)

conductor <- Conductor$
  new(  exitOnEsc = TRUE)$
  step(
  title = "HiBC's guided tour",
  text = "Use the arrows keys or the buttons below to navigate the tour. Press ESC to exit the tour.",
  buttons = list(
    list(
      action = "next",
      text = "Next"
    )
  )
)$
  step(
  title=NULL,
  "Find the list of bacterial isolates there!",
  # "#navbar li:nth-child(2)",
  "[data-value='Taxonomy']",
  tab = "Overview",
  tabId = "navbar"
)$
  step(
  "#tax_tab",
  title = NULL,
  text = "Dive into the taxonomy and filter the table",
  tab = "Taxonomy",
  tabId = "navbar",
  position = "top"
)$
  step(
    "div.dt-buttons", # btn-group flex-wrap",
    title = NULL,
    text = "The current subset table can be exported for later use!",
    tab = "Taxonomy",
    tabId = "navbar"
  )$
  step(
    title=NULL,
    "Explore similarly the cultivation metadata",
    "[data-value='Cultivation']",
    tab = "Taxonomy",
    tabId = "navbar"
  )$
  step(
    title=NULL,
    "And the genome assembly metadata.",
    "[data-value='Genomes']",
    tab = "Taxonomy",
    tabId = "navbar"
  )$
  step(
    "#viewDetail",
    title = NULL,
    text = "Curious about a strain in particular? Select it in the table and click on the button!",
    tab = "Taxonomy",
    tabId = "navbar"
  )$
  step(
    el = "#isolate-details",
    title = NULL,
    text = "Skim through the extra metadata of your selected isolate",
    tab = "detail",
    tabId = "navbar"
  )$
  step(
    "#downloadSequences",
    title = NULL,
    text = "Sequences of this isolate are directly accessible below",
    tab = "detail",
    tabId = "navbar"
  )$
  step(
    "#downloadBulk",
    title = NULL,
    text = "To download all the HiBC, use the buttons below!",
    tab = "Overview",
    tabId = "navbar"
    )$
  step(
    "#disclaimer",
    title = "Thanks for your attention",
    text = "Don't forget to use data responsibly!",
    buttons = list(
      list(
        action = "next",
        text = "Finish"
      )
    )
  )


thematic_shiny(font = "auto")
options(spinner.type = 8, spinner.color = "#28a745")

# Fetch the authentification credentials for Coscine
# src: https://appsilon.github.io/rhino/articles/how-to/manage-secrets-and-environments.html
coscine_read <- Sys.getenv("COSCINE_READ")
coscine_secret <- Sys.getenv("COSCINE_SECRET")
if (coscine_read == "" | coscine_secret == "") {
  warning(
    "No Coscine credentials in .Renviron",
    "Please provide COSCINE_READ_TOKEN and COSCINE_SECRET_TOKEN"
  )
}

# Load the hibc table
hibc_data <- read_delim("2023-04-20.Merged_HiBC.tsv", delim = "\t", show_col_types = FALSE) %>%
  arrange(Species)


translate <- function(vec, translator) {
  return(translator[vec] %>% unname())
}

qual_flags_translation <- c(
  "are_contigs_less_100" = "more than 100 contigs",
  "is_compl_grtr_90" = "completion below 90%",
  "is_contam_less_5" = "contamination above 5%",
  "is_coverage_grtr_10" = "coverage below 5x",
  "is_N50_grtr_25kb" = "N50 below 25kb",
  "is_max_contig_grtr_100kb" = "longest contig is below 100kb",
  "is_trnas_grtr_18" = "tRNAs unique number is below 18",
  "is_SSU_grtr_0" = "16S rRNA gene (SSU) was not detected",
  "is_LSU_grtr_0" = "23S rRNA gene (LSU) was not detected",
  "is_5S_grtr_0" = "5S rRNA gene was not detected"
)

# Define UI
ui <- navbarPage(
  lang = "en",
  title = span(tags$a(
    href = "https://hibc.otc.coscine.dev/",
    class = "text-reset text-decoration-none",
    tags$img(src = "hibc.png", height = 40), "HiBC"
  )),
  windowTitle = "HiBC: Human Intestinal Bacterial Collection",
  theme = bs_theme(
    version = 5,
    bootswatch = "litera",
    primary = "#28a745",
    secondary = "#F0F0C9",
    success = "#28a745",
    warning = "#ffc107",
    danger = "#dc3545",
    info = "#17a2b8",
    base_font = font_google("Atkinson Hyperlegible"),
    code_font = font_google("Source Code Pro")
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
      "),
      HTML("
        .shepherd-button {
          background-color: #28a745 ;
        }
        .shepherd-button:hover {
          background-color: #ffc107 !important ;
        }
        .shepherd-button-secondary {
          background-color: #17a2b8 ;
        }")
      # HTML("
      #   .shepherd-header {
      #     background-color: #17a2b8 !important;
      #   }
      #   .shepherd-element {
      #     background-color: white;
      #   }
      #   .shepherd-title {
      #     color: white
      #   }
      #   .shepherd-text {
      #     color: black
      #   }
      #   .shepherd-button {
      #     background-color: #28a745 ;
      #   }
      #   .shepherd-button:hover {
      #     background-color: #ffc107 !important ;
      #   }
      #   .shepherd-button-secondary {
      #     background-color: #17a2b8 ;
      #   }
      #   .shepherd-arrow:before {
      #     background-color: #17a2b8;
      #   }
      #      ")
    )),
    use_conductor()
  ),
  footer = list(
    column(hr(),
      a(href = "https://hibc.otc.coscine.dev/", "HiBC", .noWS = "after"), ".",
      "Copyright",
      a(href = "https://www.ukaachen.de/en/clinics-institutes/institute-of-medical-microbiology/research/ag-clavel/", "AG Clavel"),
      "(2023)",
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
      h1(tags$img(src = "hibc.png", height = 80), "HiBC: Human Intestinal Bacterial Collection")
    ),
    column(
      width = 8, offset = 2,
      layout_column_wrap(
        id = "hibc_values",
        width = 1 / 2,
        value_box(
          showcase = icon("bacteria", class = "fa-3x"),
          title = "Isolates:",
          value = tags$span(textOutput("no_isolates", inline = T), class = "h2 mb-2"),
          theme_color = "info",
          p("as of 2023-04-20")
        ),
        value_box(
          showcase = icon("bugs", class = "fa-3x"),
          title = "Species:",
          value = tags$span(textOutput("no_species", inline = T), class = "h2 mb-2"),
          theme_color = "warning",
          p("as of 2023-04-20")
        )
      ),
      br(),
      fluidRow(
        column(
          width = 6,
          h2("About HiBC", align = "center"),
          "The Human intestinal Bacterial Collection (HiBC) is a collection of bacterial strains,",
          "isolated from the human gut for which 16S rRNA gene sequences, genome sequences and culture",
          "conditions are made available to the research community.",
          "In addition to previously described bacteria, we include strains that represent novel species",
          "which have been taxonomically described and validly named, or will be in the future.",
          "This collection will be updated regularly."
        ),
        column(
          width = 6,
          h2("How to navigate the resource", align = "center"),
          "We provide different layers of information accessible via corresponding tabs at the top of the page; with this, users can explore the current content HiBC:",
          tags$ul(
            tags$li("taxonomy of the isolates"),
            tags$li("cultivation metadata"),
            tags$li("16S rRNA gene and genome sequences")
          ),
          align = "left",
          div(
            actionButton(
            inputId = "startTour", class = "btn-secondary fw-bold",
            label = "Take a guided tour!", icon = icon("redo"),
            width = "200px",
          ), align = "center")
        ),
      ),
      br(),
      fluidRow(
        column(
          width = 6,
          h2("Strain availability", align = "center"),
          "We strive to have our strains deposited in international collections and corresponding collection",
          "numbers are provided whenever available. However, due to chronic underfunding of culture collections,",
          "this is a continuous process and updates will be released whenever relevant.",
          "In the case of strains not yet available, we do our best to provide our strains on request,",
          "without guarantee of time due to the difficulty of funding staff for service purposes.",
        ),
        column(
          width = 6,
          h2("Data availability", align = "center"),
          "We aim to produce research data that follows the FAIR principles (Findable, Accessible, Interoperable, Reusable;",
          a(href = "http://www.nature.com/articles/sdata201618", "Wilkinson et al. 2016", .noWS = "after"),
          "). Therefore, we collect standardized metadata regarding the culture and isolation, sequencing, genome",
          "assembly process and the biological sequences. We are supported in that process by the",
          a(href = "https://nfdi4microbiota.de", "NFDI4Microbiota"),
          "a German consortium of the National Research Data Infrastructure that supports and train the microbiology",
          "community for better research data production and management.",
          div(id = "downloadBulk",
              h5("Download"),
              downloadButton("dl16S", label = "HiBC 16S rRNA sequences", icon = icon("barcode"), class = "btn-primary"),
             downloadButton("dlGenomes", label = "HiBC genomes sequences", icon = icon("dna"), class = "btn-info"),
             downloadButton("dlMetadata", label = "HiBC metadata",icon = icon("file"),class = "btn-warning")
          #     tags$ul(
          #       tags$li(downloadButton("dl16S", label = "HiBC 16S rRNA sequences", icon = icon("barcode"), class = "btn-primary")),
          #       tags$li(downloadButton("dlGenomes", label = "HiBC genomes sequences", icon = icon("dna"), class = "btn-info")),
          #       tags$li(downloadButton("dlMetadata", label = "HiBC metadata",icon = icon("file"),class = "btn-warning"))
          # )
          )
              # downloadButton("dl16S", label = "Download HiBC 16S rRNA sequences", class = "btn-error",
              #                icon = icon("barcode")),br(),
              # downloadButton("dlGenomes", label = "Download HiBC genomes sequences", class = "btn-primary",
              #                icon = icon("dna")),br(),
              # downloadButton("dlMetadata", label = "Download HiBC metadata",class = "btn-info",
              #                icon = icon("file")),br()
              # )
        )
      ),
      div(id = "disclaimer",
      h2("Disclaimer", align = "center"),
      "If you make use of HiBC, please cite our work as:",
      tags$blockquote(
        "Hitch, T.C.A., Masson J. et al. & Clavel T.",
        "The Human Intestinal Bacterial Collection,",
        "20 Apr. 2023,", a(href = "https://hibc.otc.coscine.dev", "https://hibc.otc.coscine.dev")
      ),
      "By downloading any of the HiBC data, you agree", tags$strong("not"), "to submit the data to any public database",
      "(e.g., NCBI, EBI) on your behalf or on the behalf of AG Clavel, as the ownership of all data on",
      "this website belongs to AG Clavel before submission to a public database."
      )
    )
  ),
  tabPanel(
    "Taxonomy",
    fluidRow(
      column(
        width = 4, offset = 2,
        tags$style(type = "text/css", "body {padding-top: 70px;}"),
        h4("Taxonomy of the HiBC isolates"),
        "Browse through the complete list of the isolates in the table below.",
        "Use the buttons beneath the table to copy or download the", tags$em("displayed"), "values.",
        br(), br(),
        "If you want to have more information on a specific isolate,",
        "please select your isolate in the table and click on the button on the right.",
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
      fluidRow(id = "tax_tab",
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
        h4("Cultivation of the HiBC isolates"),
        "On this tab we detail the cultivation conditions required to best grow each HiBC isolate.",
        "The graph on the right highlights the", textOutput("no_media", inline = T),
        "media most frequently identified as the best media to grow individual strains.",
        "A further", textOutput("no_media_once", inline = T), "media were observed only once.",
        br(), br(),
        "Use the buttons beneath the table to copy or download the", tags$em("displayed"), "values.",
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
      h4("Assemblies of the HiBC isolates"),
      "Explore the genome assemblies of the isolates via the two interactive plots and the table below.",
      "Use the buttons beneath the table to copy or download the", tags$em("displayed"), "values.",
      br(), br(),
      layout_column_wrap(
        width = 1 / 2,
        card(
          height = 500, full_screen = F,
          card_header("Completion and contamination"),
          card_body_fill(
            plotlyOutput("plot_compl_contam", height = "400px") %>% withSpinner()
          )
        ),
        card(
          height = 500, full_screen = F,
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
      "The isolate", textOutput("details_dsm_number", inline = T),
      "belongs to:", uiOutput("details_taxonomy", inline = T),
      h4("Cultivation and isolation metadata"),
      layout_column_wrap(
        width = "300px", fill = F,
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
      h4("Genome assembly and metadata"),
      textOutput("details_genome_quality"),
      br(),
      layout_column_wrap(
        width = "300px", fill = F,
        card(
          class = "border-secondary", align = "center",
          card_header("Genome quality"),
          card_body(
            tableOutput("details_tab_genome_quality")
          )
        ),
        card(
          class = "border-danger", align = "center",
          card_header("Assembly details"),
          card_body(
            tableOutput("details_assembly")
          )
        )
      ),
      br(),
      layout_column_wrap(
        width = "300px", fill = F,
        card(
          class = "border-warning", align = "center",
          card_header("Genomic features"),
          card_body(
            tableOutput("details_features")
          )
        ),
        card(
          class = "border-primary", align = "center",
          card_header(tags$a(href = "https://github.com/ClavelLab/genome-assembly", "Workflow")),
          card_body(
            tableOutput("details_workflow")
          )
        )
      ),
      h4("Genome and 16S rRNA sequences"),
      rclipboardSetup(),
      layout_column_wrap(id = "downloadSequences",
        width = "400px", align = "center",
        card(
          class = "border-danger", align = "center",
          card_header("16S rRNA gene sequence"),
          card_body(
            uiOutput("clip_16S", align = "center"),
            br(),
            div(
              align = "left",
              verbatimTextOutput("details_16S_sequence")
            )
          )
        ),
        card(
          class = "border-info", align = "center",
          card_header("Download FASTA"),
          card_body(
            uiOutput("clip_md5"),
            br(),
            verbatimTextOutput("md5_genome"),
            downloadButton("download_genome", "Download the genome", class = "btn-info")
          )
        )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  observeEvent(input$startTour,{
    conductor$init()$start()
  })
  preview_hibc <- reactive({
    hibc_data
  })
  media <- reactive({
    preview_hibc() %>%
      mutate(best_media = gsub(";.*", "", `Recommended medium for growth`)) %>%
      count(best_media) %>%
      arrange(n) %>%
      mutate(best_media = factor(best_media, best_media)) %>%
      drop_na(best_media)
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
      filter(n > 1) %>%
      nrow()
  })
  output$no_media_once <- renderText({
    media() %>%
      filter(n == 1) %>%
      nrow()
  })
  # Taxonomy table
  output$taxonomy <- DT::renderDT(
    DT::datatable(
      preview_hibc() %>%
        select(StrainID, Phylum, Family, Species, `DSM no.`),
      filter = "top",
      extensions = c("Responsive", "Buttons"),
      selection = list(
        mode = "single",
        selected = "7",
        target = "row"
      ),
      options = list(
        dom = "rtBflp",
        buttons =
          list("copy", "csv", "excel")
      )
    ) %>% formatStyle(columns = "Species", fontStyle = "italic"),
    server = T
  )

  # Cultivation table
  output$cultivation <- DT::renderDT(
    preview_hibc() %>%
      select(StrainID, `Recommended medium for growth`, `Growth atm.`, `Incubation time`),
    filter = "top",
    extensions = c("Responsive", "Buttons"),
    options = list(
      dom = "rtBflp",
      buttons =
        list("copy", "csv", "excel")
    ),
    server = FALSE
  )

  # Genome table
  output$genome <- DT::renderDT(
    preview_hibc() %>%
      select(StrainID, genome_length, number_contig, N50, coverage, compl_score, contam_score),
    filter = "top",
    extensions = c("Responsive", "Buttons"),
    options = list(
      dom = "rtBflp",
      buttons =
        list("copy", "csv", "excel")
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
        label = StrainID, text = paste("Species:", Species)
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
        label = StrainID, text = paste("Species:", Species)
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

  # Hide the tab when no isolate is selected
  observeEvent(0, {
    hideTab(inputId = "navbar", target = "detail")
  })
  observeEvent(input$taxonomy_rows_selected, {
    showTab(inputId = "navbar", target = "detail")
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

  md5_genome <- reactive({
    preview_hibc()[input$taxonomy_rows_selected, ] %>% pull("genome_md5")
  })

  output$md5_genome <- renderText(md5_genome())

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
        select(`Growth atm.`, `Recommended medium for growth`, `Incubation time`, `Risk Group`) %>%
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
        select(
          `Geographic location`, `Host age class`,
          `Sample material`, `Date of isolation (JJJJ-MM-DD)`
        ) %>%
        rename(`Isolation date` = `Date of isolation (JJJJ-MM-DD)`) %>%
        relocate(
          `Host age class`, `Sample material`,
          `Isolation date`, `Geographic location`
        ) %>%
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
  output$details_genome_quality <- renderText({
    # This function should not be ran before a row is selected.
    req(input$taxonomy_rows_selected)
    #
    assembly_qual <- preview_hibc() %>%
      .[input$taxonomy_rows_selected, ] %>%
      pull("assembly_qual") %>%
      stringr::str_split(., "[:,]") %>%
      unlist() %>%
      str_trim()
    # Return the adequate statement
    if_else(
      length(assembly_qual) == 1,
      str_glue("The genome is a high-quality draft"),
      assembly_qual[-1] %>%
        translate(translator = qual_flags_translation) %>%
        str_flatten_comma() %>%
        str_glue("The genome could be a high-quality draft but is has: {flags}.",
          flags = .
        )
    )
  })
  output$details_tab_genome_quality <- renderTable(
    {
      # This function should not be ran before a row is selected.
      req(input$taxonomy_rows_selected)
      #
      preview_hibc() %>%
        select(coverage, compl_score, compl_software, contam_score, contam_software) %>%
        .[input$taxonomy_rows_selected, ] %>%
        t()
    },
    rownames = T,
    colnames = F,
    na = "",
    hover = T,
    spacing = "xs",
    digits = 0,
    align = "lr",
    format.args = list(big.mark = " ")
  )
  output$details_assembly <- renderTable(
    {
      # This function should not be ran before a row is selected.
      req(input$taxonomy_rows_selected)
      #
      preview_hibc() %>%
        select(genome_length, max_contig_length, N50, number_contig, number_contig_below_1kb) %>%
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
  output$details_workflow <- renderTable(
    {
      # This function should not be ran before a row is selected.
      req(input$taxonomy_rows_selected)
      #
      preview_hibc() %>%
        select(workflow_version, assembly_date, sequencer, assembly_software) %>%
        .[input$taxonomy_rows_selected, ] %>%
        t()
    },
    rownames = T,
    colnames = F,
    na = "",
    hover = T,
    spacing = "xs",
    digits = 0,
    align = "lr",
    format.args = list(big.mark = " ")
  )
  output$details_features <- renderTable(
    {
      # This function should not be ran before a row is selected.
      req(input$taxonomy_rows_selected)
      #
      preview_hibc() %>%
        .[input$taxonomy_rows_selected, ] %>%
        select(plasmid_length, trnas, trna_ext_software) %>%
        mutate(plasmid_length = str_split_1(plasmid_length, ";") %>%
          sapply(., function(x) prettyNum(x, big.mark = " "), USE.NAMES = F) %>%
          str_glue("{size} nt", size = .) %>%
          str_flatten_comma(last = " and ")) %>%
        t()
    },
    rownames = T,
    colnames = F,
    na = "",
    hover = T,
    spacing = "xs",
    digits = 0,
    align = "lr",
    format.args = list(big.mark = " ")
  )
  seq16S <- reactive({
    # This function should not be ran before a row is selected.
    req(input$taxonomy_rows_selected)
    #
    preview_hibc() %>%
      .[input$taxonomy_rows_selected, ] %>%
      select(StrainID, Species, `16S sequence`) %>%
      str_glue_data(">{StrainID}_16S_Sanger {Species}\n{`16S sequence`}")
  })
  output$details_16S_sequence <- renderText({
    seq16S()
  })
  # Copy clipboard buttons
  output$clip_16S <- renderUI({
    rclipButton("btn_clip_16S",
      label = "Copy sequence", icon = icon("clipboard"),
      class = "btn-danger", clipText = seq16S()
    )
  })
  output$clip_md5 <- renderUI({
    rclipButton("btn_clip_md5",
      label = "Copy FASTA md5sum", icon = icon("clipboard"),
      class = "btn-secondary", clipText = md5_genome()
    )
  })
  observeEvent(input$btn_clip_16S, {
    showNotification("16S rRNA sequence copied.", type = "default")
  })
  observeEvent(input$btn_clip_md5, {
    showNotification("FASTA md5sum copied.", type = "default")
  })
  genome_filename <- reactive(
    preview_hibc() %>% .[input$taxonomy_rows_selected, ] %>%
      str_glue_data("{StrainID}.genome.fa.gz")
  )
  output$download_genome <- downloadHandler(
    filename = genome_filename(),
    content = function(file) {
      save_object(
        object = genome_filename(), file = file,
        bucket = gsub("read_", "", coscine_read),
        region = "", # because non-AWS
        base_url = "coscine-s3-01.s3.fds.rwth-aachen.de:9021",
        key = coscine_read,
        secret = coscine_secret
      )
    },
    contentType = "text/plain"
  )
  # Navigation
  #
  observeEvent(input$viewDetail, {
    updateNavbarPage(session = session, inputId = "navbar", selected = "detail")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

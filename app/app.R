library(shiny)
library(bslib)
library(DT)
library(tidyverse)
library(cowplot)
library(plotly)
library(thematic)
library(showtext)
library(shinycssloaders)
library(aws.s3)
library(glouton)
library(conductor)

# src: https://cicerone.john-coene.com/examples/glouton.html
js <- "$(document).on('shiny:connected', function(event) {
  Shiny.onInputChange('loaded', true)
});"

conductor <- Conductor$
  new(exitOnEsc = TRUE)$
  step(
  title = "HiBC's guided tour",
  text = "Use the arrows keys or the buttons below to navigate the tour. Press ESC to exit the tour.",
  buttons = list(
    list(
      action = "next",
      text = "Next"
    )
  ),
  cancelIcon = list(enabled = TRUE, NULL)
)$
  step(
  title = NULL,
  text = "Find the list of bacterial isolates here!",
  "[data-value='Taxonomy']",
  tab = "Overview",
  tabId = "navbar"
)$
  step(
  "#tax_tab",
  title = NULL,
  text = "Dive into the taxonomy by using filters and search.",
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
  title = NULL,
  "Likewise, explore the cultivation metadata (with plot).",
  "[data-value='Cultivation']",
  tab = "Taxonomy",
  tabId = "navbar",
  canClickTarget = FALSE
)$
  step(
  title = NULL,
  "And the genome assembly metadata (with plots).",
  "[data-value='Genomes']",
  tab = "Taxonomy",
  tabId = "navbar",
  canClickTarget = FALSE
)$
  step(
  "#viewDetail",
  title = NULL,
  text = "Curious about a strain in particular? Select it in the table and click on the button!",
  tab = "Taxonomy",
  tabId = "navbar",
  canClickTarget = FALSE
)$
  step(
  "[data-value='detail']",
  title = NULL,
  text = "Or select it in the table and click on the tab here!",
  tab = "Taxonomy",
  tabId = "navbar",
  canClickTarget = FALSE
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
  tabId = "navbar",
  canClickTarget = FALSE
)$
  step(
  "#downloadBulk",
  title = NULL,
  text = "If you are interested in all isolates, use the buttons to download all the HiBC datasets",
  tab = "Overview",
  tabId = "navbar",
  canClickTarget = FALSE,
  position = "top"
)$
  step(
  "#disclaimer",
  title = "End of the tour!",
  text = "Thanks for your attention. Don't forget to use data responsibly!",
  position = "top",
  buttons = list(
    list(
      action = "next",
      text = "Finish"
    )
  ),
  cancelIcon = list(enabled = TRUE, NULL)
)


thematic_shiny(font = "auto")
options(spinner.type = 8, spinner.color = "#28a745")

# Fetch the authentification credentials for Coscine
# src: https://appsilon.github.io/rhino/articles/how-to/manage-secrets-and-environments.html
coscine_genome_read <- Sys.getenv("COSCINE_GENOME_READ")
coscine_genome_secret <- Sys.getenv("COSCINE_GENOME_SECRET")
coscine_16S_read <- Sys.getenv("COSCINE_16S_READ")
coscine_16S_secret <- Sys.getenv("COSCINE_16S_SECRET")
if (coscine_genome_read == "" | coscine_genome_secret == "" |
    coscine_16S_read == "" | coscine_16S_secret == "") {
  warning(
    "No Coscine credentials in .Renviron",
    "Please provide COSCINE_GENOME_READ, COSCINE_GENOME_SECRET,",
    "COSCINE_16S_READ, COSCINE_16S_SECRET tokens"
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
  ),
  position = "static-top",
  header = list(
    tags$head(tags$style(
      HTML("
      .table.dataTable tbody td.active, .table.dataTable tbody tr.active td {
            background-color: var(--bs-success)!important;}
      "),
      HTML("
        .shepherd-title {
          padding-left: 2.5rem
        }

        .shepherd-cancel-icon > span {
          border-radius: 50%;
          background-color: none;
          opacity: 0.8;
          display: block;
          width: 2.8rem !important;
        }
        .shepherd-button {
          background-color: #28a745 ;
        }
        .shepherd-button:hover {
          background-color: #ffc107 !important ;
        }
        .shepherd-button-secondary {
          background-color: #17a2b8 ;
        }")
    )),
    use_glouton(),
    use_conductor(),
    tags$script(js)
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
  collapsible = FALSE,
  tabPanel(
    "Overview",
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    column(
      width = 12, align = "center",
      h1(tags$img(src = "hibc.png", height = 80, class = "mx-auto d-none d-md-inline"), "HiBC: Human Intestinal Bacterial Collection")
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
              inputId = "startTour", class = "btn-success fw-bold",
              label = "Follow the guided tour!", icon = icon("redo"),
              width = "200px",
            ),
            align = "center"
          )
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
          "community for better research data production and management."
        )
      ),
      fluidRow(
        column(
          width = 6,
          div(
            id = "downloadBulk",
            h2("Get HiBC datasets", align = "center"), br(), br(),
            layout_column_wrap(
              width = 1 / 3,
              card(
                align = "center",
                card_header(icon("barcode"), "16S rRNA gene sequences"),
                a(href = "https://doi.org/10.5281/zenodo.7966230", "Zenodo", target = "_blank"),br(),br(),
                tags$button(
                  class = "btn btn-warning",
                  icon("download"),
                  a(class = "text-reset text-decoration-none", target = "_blank",
                    href="https://zenodo.org/record/7966230/files/HiBC_16S_rRNA_gene_sequences_20230524.zip?download=1",
                    "Download")
                )
              ),
              card(
                align = "center",
                card_header(icon("dna"), "Genomes sequences"),
                a(href = "https://doi.org/10.5281/zenodo.7966739", "Zenodo", target = "_blank"),br(),br(),
                tags$button(
                  class = "btn btn-warning",
                  icon("download"),
                  a(class = "text-reset text-decoration-none", target = "_blank",
                    href="https://zenodo.org/record/7966739/files/HiBC_Genome_sequences_20230524.zip?download=1",
                    "Download")
                )              ),
              card(
                align = "center",
                card_header(icon("file"), "Isolates and genomes metadata"),
                a(href = "https://doi.org/10.5281/zenodo.7966674", "Zenodo", target = "_blank"),br(),br(),
                downloadButton("download_metadata", class = "btn-warning")
              )
            )
          )
        ),
        column(
          width = 6,
          div(
            id = "disclaimer",
            h2("Disclaimer", align = "center"),
            "If you make use of HiBC, please temporarily cite our work as a website in the", a(href = "https://apastyle.apa.org/", "APA style", .noWS = "after"), ":", br(), br(),
            "Thomas C. A. Hitch, Johannes M. Masson, Charlie Pauvert, Johanna Bosch, Afrizal Afrizal, Nicole Treichel, Jonathan Hartman, Lukas C. Bossert and Thomas Clavel",
            "(n.d.).", tags$em("The Human Intestinal Bacterial Collection Website", .noWS = "after"), ".",
            "Retrieved", format(Sys.Date(), "%b %d, %Y"), "from", a(href = "https://hibc.otc.coscine.dev", "https://hibc.otc.coscine.dev", .noWS = "after"), ".", br(), br(),
            "By downloading any of the HiBC data, you agree", tags$strong("not"), "to submit the data to any public database",
            "(e.g., NCBI, EMBL-EBI) on your behalf or on the behalf of AG Clavel."
          )
        )
      )
    )
  ),
  tabPanel(
    "Taxonomy",
    fluidRow(
      column(
        width = 4, offset = 2,
        h4("Taxonomy of the HiBC isolates"),
        "Browse through the complete list of the isolates in the table below.",
        "Use the buttons beneath the table to copy or download the", tags$em("displayed"), "values.",
        br(), br(),
        "If you want to have more information on a specific isolate,",
        "please select your isolate in the table and click on the button on the right.",
      ),
      column(
        width = 4, align = "center",
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
        id = "tax_tab",
        DT::dataTableOutput("taxonomy")
      )
    )
  ),
  tabPanel(
    "Cultivation",
    fluidRow(
      column(
        width = 4, offset = 2,
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
      h4("Assemblies of the HiBC isolates"),
      "Explore the genome assemblies of the isolates via the two interactive plots and the table below.", br(),
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
      layout_column_wrap(
        id = "downloadSequences",
        width = "400px", align = "center",
        card(
          class = "border-danger", align = "center",
          card_header(icon("barcode"), "16S rRNA gene sequence"),
          downloadButton("download_selected_16S", "Download", class = "btn-danger")
        ),
        card(
          class = "border-info", align = "center",
          card_header(icon("dna"), "Genome sequence"),
          downloadButton("download_genome", "Download", class = "btn-info")
        )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  observeEvent(input$loaded, {
    # get cookie
    visited <- fetch_cookies()
    # if null set cookie
    # otherwise show guide
    if (is.null(visited$visited_site)) {
      add_cookie("visited_site", "yes")
      conductor$init()$start()
    }
  })
  observeEvent(input$startTour, {
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
  output$clip_md5 <- renderUI({
    rclipButton("btn_clip_md5",
      label = "Copy Genome md5sum", icon = icon("clipboard"),
      class = "btn-secondary", clipText = md5_genome()
    )
  })
  observeEvent(input$btn_clip_md5, {
    showNotification("FASTA md5sum copied", type = "default")
  })
  sixteen_s_filename <- reactive(
    preview_hibc() %>% .[input$taxonomy_rows_selected, ] %>%
      str_glue_data("{StrainID}_16S_Sanger.fna")
  )
  output$download_selected_16S <- downloadHandler(
    filename = function() sixteen_s_filename(),
    content = function(file) {
      # Test if file exists
      does_seq_exists<-head_object(
        sixteen_s_filename(),
        bucket = gsub("read_", "", coscine_16S_read),
        region = "", # because non-AWS
        base_url = "coscine-s3-01.s3.fds.rwth-aachen.de:9021",
        key = coscine_16S_read,
        secret = coscine_16S_secret
      )
      if(isTRUE(does_seq_exists)){
        existing_sequence <- sixteen_s_filename()
      } else {
        existing_sequence <- gsub("Sanger","Genome", sixteen_s_filename())
      }
      save_object(
        object = existing_sequence, file = file,
        bucket = gsub("read_", "", coscine_16S_read),
        region = "", # because non-AWS
        base_url = "coscine-s3-01.s3.fds.rwth-aachen.de:9021",
        key = coscine_16S_read,
        secret = coscine_16S_secret
      )
    },
    contentType = "text/plain"
  )
  genome_filename <- reactive(
    preview_hibc() %>% .[input$taxonomy_rows_selected, ] %>%
      str_glue_data("{StrainID}.genome.fa.gz")
  )
  output$download_genome <- downloadHandler(
    filename = function() genome_filename(),
    content = function(file) {
      save_object(
        object = genome_filename(), file = file,
        bucket = gsub("read_", "", coscine_genome_read),
        region = "", # because non-AWS
        base_url = "coscine-s3-01.s3.fds.rwth-aachen.de:9021",
        key = coscine_genome_read,
        secret = coscine_genome_secret
      )
    },
    contentType = "text/plain"
  )
  output$download_metadata <- downloadHandler(
    filename = "20230420_HiBC_metadata.tsv",
    content = function(file) {
      preview_hibc() %>%
        rename(`Isolation date` = `Date of isolation (JJJJ-MM-DD)`) %>%
        select(
          StrainID, Phylum, Family, Species, `DSM no.`,
          `Recommended medium for growth`, `Growth atm.`, `Incubation time`, `Risk Group`,
          `Geographic location`, `Host age class`,
          `Sample material`, `Isolation date`,
          genome_md5, assembly_qual,
          coverage, compl_score, compl_software, contam_score, contam_software,
          genome_length, max_contig_length, N50, number_contig, number_contig_below_1kb,
          plasmid_length, trnas, trna_ext_software,
          workflow_version, assembly_date, sequencer, assembly_software
        ) %>%
        relocate(
          StrainID, Phylum, Family, Species, `DSM no.`,
          `Recommended medium for growth`, `Growth atm.`, `Incubation time`, `Risk Group`,
          `Geographic location`, `Host age class`,
          `Sample material`, `Isolation date`,
          genome_md5, assembly_qual,
          coverage, compl_score, compl_software, contam_score, contam_software,
          genome_length, max_contig_length, N50, number_contig, number_contig_below_1kb,
          plasmid_length, trnas, trna_ext_software,
          workflow_version, assembly_date, sequencer, assembly_software
        ) %>%
        write_tsv(file)
    }, contentType = "text/tsv"
  )
  # Navigation
  #
  observeEvent(input$viewDetail, {
    updateNavbarPage(session = session, inputId = "navbar", selected = "detail")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

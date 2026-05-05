# app.R
library(shiny)
library(dplyr)
library(shinycssloaders)
library(bslib)

source("R/fetch_rates.R")
source("R/optimizer.R")

# Load rates once at startup (not per session)
rates_data <- fetch_usps_rates()

get_rate <- function(id) {
  rates_data$stamps %>%
    filter(id == !!id) %>%
    pull(value)
}

pastel_theme <- bs_theme(
  version        = 5,
  bg             = "#fff0f5",   # Lavender blush — soft pink page background
  fg             = "#4a3040",   # Dark plum — easy to read on light backgrounds
  primary        = "#f48fb1",   # Medium pink — buttons, links, accents
  secondary      = "#ce93d8",   # Soft purple — secondary accents
  success        = "#a5d6a7",   # Pastel green — success states
  info           = "#81d4fa",   # Pastel blue — info states
  warning        = "#ffe082",   # Pastel yellow — warnings
  danger         = "#ef9a9a",   # Pastel red — errors
  base_font      = font_google("Nunito"),       # Soft rounded font
  heading_font   = font_google("Pacifico"),     # Fun cursive headings
  font_scale     = 1.05
)

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = bs_theme(
    version   = 5,
    bg        = "#fff0f5",
    fg        = "#4a3040",
    primary   = "#f48fb1",
    secondary = "#ce93d8",
    success   = "#a5d6a7",
    info      = "#81d4fa",
    warning   = "#ffe082",
    danger    = "#ef9a9a",
    font_scale = 1.05
    # No base_font or heading_font here — handled via CSS below
  ),
  
  tags$head(
    # Google Fonts loaded by the browser, not R — no firewall issues
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Nunito:wght@400;700&display=swap"
    ),
    tags$style(HTML("

      body, .shiny-input-container {
        font-family: 'Nunito', 'Trebuchet MS', Arial, sans-serif;
      }

      h1, h2, h3, h4, h5, h6 {
        font-family: 'Nunito', 'Trebuchet MS', Arial, sans-serif;
      }

      body {
        background-color: #fff0f5;
      }

      .well {
        background-color: #fce4ec;
        border: 1px solid #f48fb1;
        border-radius: 12px;
      }

      .btn-primary {
        background-color: #f48fb1 !important;
        border-color:     #ec407a !important;
        color:            #ffffff !important;
        border-radius:    20px   !important;
        font-weight:      bold   !important;
      }

      .btn-primary:hover {
        background-color: #ec407a !important;
        border-color:     #e91e8c !important;
      }

      thead {
        background-color: #f48fb1;
        color: white;
      }

      tbody tr:nth-child(odd)  { background-color: #fff0f5; }
      tbody tr:nth-child(even) { background-color: #fce4ec; }

      .well p {
        color: #880e4f;
      }

      hr {
        border-color: #f48fb1;
      }

    "))
  ),
  
  titlePanel("📬 Postcrossing USPS Stamp Optimizer"),
  p("Find the optimal combination of stamps you own to cover international letter postage."),
  
  sidebarLayout(
    
    sidebarPanel(width = 4,
                 
                 # -- Current rates display --
                 h4("📋 Current USPS Rates"),
                 uiOutput("rates_display"),
                 
                 hr(),
                 
                 # -- Stamp selection by category --
                 h4("✅ Step 1: Select stamps you own"),
                 p(em("Check every denomination you have on hand:")),
              
                 strong("🇺🇸 Domestic Stamps"),
                 uiOutput("checkboxes_Domestic"),
                 
                 br(),
                 strong("🌍 International Stamps"),
                 uiOutput("checkboxes_International"),
                 
                 br(),
                 strong("💌 Make-Up / Additional Postage Stamps"),
                 uiOutput("checkboxes_Makeup"),
                 
                 hr(),
                 
                 # -- Custom denominations --
                 h4("➕ Step 2: Add any other stamps you own"),
                 p("Have old or unusual stamps not listed above?"),
                 p(em("Enter values in CENTS, separated by commas:")),
                 p(em("e.g. → 20, 34, 37 for 20¢, 34¢, and 37¢ stamps")),
                 textInput("custom_stamps",
                           label = NULL,
                           value = "",
                           placeholder = "e.g. 20, 34, 37"),
                 
                 hr(),
                 
                 # Add this ABOVE the actionButton in the sidebarPanel:
                 
                 hr(),
                 
                 h4("⚙️ Step 3: Options"),
                 numericInput(
                   inputId = "max_stamps",
                   label   = "Maximum number of stamps per combination:",
                   value   = 5,      # Default
                   min     = 1,
                   max     = 10,
                   step    = 1
                 ),
                 p(em("Lower = faster but may miss some combinations. 2–5 is recommended.")),
                 
                 # -- Run button --
                 actionButton("optimize",
                              "🔍 Find Optimal Combinations",
                              class = "btn-primary",
                              width = "100%")
    ),
    
    mainPanel(width = 8,
              
              h4(paste0("🏆 Top Combinations for International Letter Postage ($",
                formatC(get_rate("intl_global_forever"), format = "f", digits = 2), ")")),
              p(em("Ranked by: least overage first, then fewest stamps used")),
              
              withSpinner(tableOutput("results_table"), type = 7, color = "#f48fb1"),
              
              uiOutput("no_results_msg"),
              
              br(),
              wellPanel(
                p("⚠️ ", strong("Rates last verified: July 2025.")),
                p("USPS typically updates rates in January and July. Always confirm at ",
                  a("usps.com", href = "https://www.usps.com/business/postage-rates.htm",
                    target = "_blank"), ".")
              )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # -- Render current rates summary --
output$rates_display <- renderUI({
  tags$ul(
    lapply(seq_len(nrow(rates_data$stamps)), function(i) {
      tags$li(paste0(
        rates_data$stamps$name[i], ": $",
        formatC(rates_data$stamps$value[i], format = "f", digits = 2)
      ))
    })
  )
})
  
  # -- Render checkbox groups per category --
  # Helper function to avoid repeating code three times
make_checkboxes <- function(cat) {
  renderUI({
    cat_stamps <- rates_data$stamps %>%
      filter(category == cat)
    checkboxGroupInput(
      inputId  = paste0("stamps_", cat),
      label    = NULL,
      choices  = setNames(
        cat_stamps$id,
        paste0(cat_stamps$name, " ($",
               formatC(cat_stamps$value, format = "f", digits = 2), ")")
      ),
      selected = NULL
    )
  })
}
  
  output$checkboxes_Makeup        <- make_checkboxes("Makeup")
  output$checkboxes_Domestic      <- make_checkboxes("Domestic")
  output$checkboxes_International <- make_checkboxes("International")
  
  # -- Run optimizer on button click --
  results <- eventReactive(input$optimize, {

  # Collect selected stamp IDs across all three groups
  selected_ids <- c(
    input$stamps_Makeup,
    input$stamps_Domestic,
    input$stamps_International
  )

  if (length(selected_ids) == 0 && nchar(trimws(input$custom_stamps)) == 0) {
    return(NULL)
  }

  # Look up name and value by ID — guaranteed unique, no length mismatch
  selected_stamps <- rates_data$stamps %>%
    filter(id %in% selected_ids) %>%
    select(name, value)

  owned <- setNames(selected_stamps$value, selected_stamps$name)

  # Parse custom stamp inputs (entered in cents, convert to dollars)
  if (nchar(trimws(input$custom_stamps)) > 0) {
    custom_raw   <- strsplit(input$custom_stamps, ",")[[1]]
    custom_cents <- suppressWarnings(as.numeric(trimws(custom_raw)))
    custom_cents <- custom_cents[!is.na(custom_cents) & custom_cents > 0]

    if (length(custom_cents) > 0) {
      custom_dollars <- custom_cents / 100
      custom_named   <- setNames(
        custom_dollars,
        paste0(custom_cents, "¢ (custom)")
      )
      owned <- c(owned, custom_named)
    }
  }

  # Validate max_stamps input
  max_s <- input$max_stamps
  if (is.na(max_s) || max_s < 1) max_s <- 1
  if (max_s > 10) max_s <- 10

  target <- round(get_rate("intl_global_forever") * 100)

  find_combinations(owned_stamps = owned,
                    target_cents = target,
                    max_stamps   = max_s,
                    top_n        = 10)
})
  
  # -- Render results table --
  output$results_table <- renderTable({
    req(results())
    results()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # -- Show helpful message if no combinations found --
  output$no_results_msg <- renderUI({
    req(input$optimize)
    if (is.null(results()) || nrow(results()) == 0) {
      div(
        style = "color: red;",
        "⚠️ No combinations found. Try selecting more stamp denominations,
         or add custom stamps in Step 2."
      )
    } else if (nrow(results()) == 1 &&
               "Message" %in% colnames(results())) {
      div(style = "color: red;", results()$Message[1])
    }
  })
}

# ── Launch ────────────────────────────────────────────────────────────────────

shinyApp(ui, server)

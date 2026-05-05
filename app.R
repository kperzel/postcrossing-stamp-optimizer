# app.R
library(shiny)
library(dplyr)

source("R/fetch_rates.R")
source("R/optimizer.R")

# Load rates once at startup (not per session)
rates_data <- fetch_usps_rates()

get_rate <- function(id) {
  rates_data$stamps %>%
    filter(id == !!id) %>%
    pull(value)
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  
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
              
              tableOutput("results_table"),
              
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
      tags$li(paste0(rates_data$stamps$name[i], ": $",
                     formatC(rates_data$stamps$value[i], format = "f", digits = 2)))
    })
  )
})
  
  # -- Render checkbox groups per category --
  # Helper function to avoid repeating code three times
  make_checkboxes <- function(cat) {
    renderUI({
      cat_stamps <- rates_data$stamps %>% filter(category == cat)
      checkboxGroupInput(
        inputId  = paste0("stamps_", cat),
        label    = NULL,
        choices  = setNames(as.character(cat_stamps$id), cat_stamps$name),
        selected = NULL   # Nothing pre-selected — user picks what they own
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

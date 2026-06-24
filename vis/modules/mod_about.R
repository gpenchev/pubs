mod_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(10, offset = 1,

        br(),

        # --- Title card -------------------------------------------------------
        bslib::card(
          bslib::card_header(
            tags$h3(
              bsicons::bs_icon("shield-shaded"), " ",
              "Interactive Application to: Threat Proximity and Defence Spending in NATO-EU Member States, 1995–2023",
              style = "margin: 0;"
            )
          ),
          bslib::card_body(
            tags$p(
              style = "font-size: 1.05em; line-height: 1.7;",
              "This app accompanies a study of defence spending rationality
               across 22 NATO-EU member states (1995–2023). It asks: do
               European governments respond to proximate military threat in a
               rational, proportional way — and what fiscal and political
               conditions constrain that response? Using a spatially-decayed
               UCDP conflict proximity index and spatial autoregressive panel
               models, we find strong evidence of threat-responsive spending
               disrupted by fiscal austerity (2005–2013) and conditioned by
               the political context of post-2014 rearmament."
            ),
            tags$p(
              style = "color: #666; font-size: 0.95em;",
              "Data: UCDP GED 26.1 · Eurostat · IMF WEO · WDI · ParlGov · Caldara-Iacoviello GPR"
            ),
            tags$p(
              style = "color: #666; font-size: 0.95em;",
              bsicons::bs_icon("github"), " Code: ",
              tags$a("github.com/gpenchev/pubs",
                     href = "https://github.com/gpenchev/pubs",
                     target = "_blank",
                     style = "color: #555;")
            )
          )
        ),

        br(),

        # --- Key findings value boxes -----------------------------------------
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          bslib::value_box(
            title    = "Spatial lag \u03c1 (M5 SAR)",
            value    = "+0.177",
            showcase = bsicons::bs_icon("diagram-3"),
            theme    = "primary",
            p("p < 0.001 \u2014 long-run strategic complementarity")
          ),
          bslib::value_box(
            title    = "Threat coefficient \u03b2 (M5 SAR)",
            value    = "+0.088",
            showcase = bsicons::bs_icon("bullseye"),
            theme    = "success",
            p("p < 0.001 \u2014 significant threat responsiveness")
          ),
          bslib::value_box(
            title    = "\u0394AIC vs GPR (M5 vs M13)",
            value    = "17.6",
            showcase = bsicons::bs_icon("bar-chart-line"),
            theme    = "info",
            p("UCDP outperforms text-based GPR measure")
          ),
          bslib::value_box(
            title    = "Sample",
            value    = "22 countries",
            showcase = bsicons::bs_icon("people"),
            theme    = "warning",
            p("529 obs \u00b7 1995\u20132023 \u00b7 NATO-EU")
          )
        ),

        br(),

        # --- How to use -------------------------------------------------------
        bslib::card(
          bslib::card_header(tags$h5(bsicons::bs_icon("map"), " How to use this app")),
          bslib::card_body(
            fluidRow(
              column(6,
                tags$h6(bsicons::bs_icon("search"), " Open Science"),
                tags$ul(
                  tags$li("Every", tags$b("Tables"), "sub-tab has a Download CSV button"),
                  tags$li(tags$b("Results"), "sub-tabs show the full methodology and findings"),
                  tags$li("All data comes from the published replication pipeline")
                )
              ),
              column(6,
                tags$h6(bsicons::bs_icon("easel"), " Presentation use"),
                tags$ul(
                  tags$li("Navigate to a tab before the session and leave it open"),
                  tags$li("Use the", tags$b("Issues"), "tab for pre-prepared answers to reviewer questions"),
                  tags$li("Every chart is interactive — zoom, hover, filter on demand")
                )
              )
            )
          )
        ),

        br(),

        # --- Navigation guide -------------------------------------------------
        bslib::card(
          bslib::card_header(tags$h5(bsicons::bs_icon("signpost-split"), " Navigation guide")),
          bslib::card_body(
            tags$table(
              class = "table table-sm table-hover",
              tags$thead(tags$tr(
                tags$th("Tab"),
                tags$th("What it covers"),
                tags$th("Open first")
              )),
              tags$tbody(
                tags$tr(
                  tags$td(tags$b("Threat Index")),
                  tags$td("How the UCDP conflict proximity score is built and varies across countries and time"),
                  tags$td("Maps — see where conflict events fall")
                ),
                tags$tr(
                  tags$td(tags$b("Estimation")),
                  tags$td("Regression models M1–M13: coefficients, regime effects, spatial lag, AIC comparison"),
                  tags$td("Figures — coefficient forest plot")
                ),
                tags$tr(
                  tags$td(tags$b("Robustness")),
                  tags$td("All sensitivity checks A–J: BG 2019, immigration, cross-section 2022"),
                  tags$td("Tables — full checks summary")
                ),
                tags$tr(
                  tags$td(tags$b("Issues")),
                  tags$td("Pre-written answers to the four named reviewer weaknesses"),
                  tags$td("Figure panel opens automatically — select issue from sidebar")
                )
              )
            )
          )
        ),

        br(),

        # --- Dynamic brief.md -------------------------------------------------
        uiOutput(ns("brief_text")),

        br(),

        # --- Links and acknowledgement ----------------------------------------
        bslib::card(
          bslib::card_header(tags$h5(bsicons::bs_icon("info-circle"), " Links & acknowledgements")),
          bslib::card_body(
            tags$p(
              bsicons::bs_icon("github"), " ",
              tags$strong("Code repository:"), " ",
              tags$a("github.com/gpenchev/pubs",
                     href   = "https://github.com/gpenchev/pubs",
                     target = "_blank")
            ),
            tags$p(
              bsicons::bs_icon("bar-chart-line"), " ",
              tags$strong("Interactive application:"), " ",
              tags$a("pub.e-dnrs.org/article1",
                     href   = "https://pub.e-dnrs.org/article1/",
                     target = "_blank")
            ),
            tags$hr(style = "margin: 0.75em 0;"),
            tags$p(
              style = "color: #666; font-size: 0.95em;",
              "Research design, data, and all analytical decisions by the author. ",
              "Scripts and interactive application developed with the assistance of ",
              tags$strong("Claude Sonnet 4.5"),
              " accessed through ",
              tags$strong("AiderDesk 0.70.0"),
              "."
            )
          )
        ),

        br()
      )
    )
  )
}

mod_about_server <- function(id, proj_root) {
  moduleServer(id, function(input, output, session) {
    output$brief_text <- renderUI({
      md <- tryCatch(
        read_md_section(proj_root, "methodology/models/brief.md", "full"),
        error = function(e) "*Research framework summary not available.*"
      )
      bslib::card(
        bslib::card_header(
          tags$h5(bsicons::bs_icon("journal-text"), " Research framework")
        ),
        bslib::card_body(shiny::markdown(md))
      )
    })
  })
}

# Generic Results panel — reads one or more .md sections and renders them.
# Each tab passes a list of md_spec entries: list(file, heading, label).

mod_results_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("results_body"))
}

mod_results_server <- function(id, proj_root, md_specs) {
  # md_specs: list of lists, each with:
  #   $label   — accordion panel title (character)
  #   $file    — relative path from proj_root
  #   $heading — "full" or "## N." prefix
  moduleServer(id, function(input, output, session) {
    output$results_body <- renderUI({
      panels <- lapply(md_specs, function(spec) {
        content <- tryCatch(
          read_md_section(proj_root, spec$file, spec$heading),
          error = function(e) paste0("*Could not load: ", spec$file, "*")
        )
        bslib::accordion_panel(
          title = spec$label,
          shiny::markdown(content)
        )
      })

      # First panel open by default for immediate visibility
      first_label <- if (length(md_specs) > 0) md_specs[[1]]$label else NULL

      tagList(
        br(),
        tags$div(
          style = "margin-bottom: 10px;",
          tags$span(
            bsicons::bs_icon("info-circle"),
            " Click on a topic heading to expand the research notes.",
            style = "color: #6c757d; font-size: 0.9em;"
          )
        ),
        do.call(bslib::accordion, c(
          list(open = first_label, multiple = TRUE),
          panels
        ))
      )
    })
  })
}

# ── pop_params_ui.R ───────────────────────────────────────────────────────────
# Accepts ns (the parent popUI namespace function) instead of an id.

popParamsPanelUI <- function(ns) {
  tabPanel("Trends",
           p(""),
           uiOutput(ns("trend_vbs")),
           uiOutput(ns("trend_vbs_spacer")),
           layout_columns(
             card(
               card_header("Parameter Selection"),
               card_body(
                 uiOutput(ns("params")),
                 uiOutput(ns("proportion_toggle")),
                 uiOutput(ns("time_range")),
                 uiOutput(ns("facet_var"))
               )
             ),
             card(
               card_header("Parameter Visualization"),
               card_body(
                 htmlOutput(ns("brush_warn")),
                 plotlyOutput(ns("paramplot")),
                 uiOutput(ns("summary_section"))
               )
             ),
             col_widths = c(3, 9)
           ),
           p(""),
           uiOutput(ns("selected_card"))
  )
}
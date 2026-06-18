# ── pop_litters_ui.R ──────────────────────────────────────────────────────────
popLittersPanelUI <- function(ns) {
  tabPanel("Litters",
           p(""),
           uiOutput(ns("year_filter_badge")),
           layout_columns(
             value_box(title = "Total Litters",    value = uiOutput(ns("vb_lit_total")),
                       showcase = bsicons::bs_icon("collection"),       theme = "primary"),
             value_box(title = "Litters This Year",value = uiOutput(ns("vb_lit_thisyear")),
                       showcase = bsicons::bs_icon("calendar"),         theme = "info"),
             value_box(title = "Mean Litter Size", value = uiOutput(ns("vb_lit_size")),
                       showcase = bsicons::bs_icon("bar-chart"),        theme = "secondary"),
             value_box(title = "Sex Ratio",        value = uiOutput(ns("vb_lit_sexratio")),
                       showcase = bsicons::bs_icon("gender-ambiguous"), theme = "secondary"),
             col_widths = c(3, 3, 3, 3)
           ),
           p(""),
           card(card_header("Litters by Year"),   card_body(plotlyOutput(ns("litterplot")))),
           card(card_header("Litters"),           card_body(dataTableOutput(ns("litters")),      min_height = "500px")),
           card(card_header("Selected Litter"),   card_body(dataTableOutput(ns("selectlitter")), min_height = "700px"))
  )
}
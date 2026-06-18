# ── pop_overview_ui.R ─────────────────────────────────────────────────────────
# Accepts ns (the parent popUI namespace function) instead of an id.

popOverviewPanelUI <- function(ns) {
  tabPanel("Population Overview",
           p(""),
           uiOutput(ns("year_filter_badge")),
           layout_columns(
             value_box(title = "Total Animals",
                       value = uiOutput(ns("vb_animals")),
                       showcase = bsicons::bs_icon("hearts"), theme = "primary"),
             value_box(title = "Total Litters",
                       value = uiOutput(ns("vb_litters")),
                       showcase = bsicons::bs_icon("diagram-3"), theme = "primary"),
             value_box(title = "Mean Litter Size",
                       value = uiOutput(ns("vb_litsize")),
                       showcase = bsicons::bs_icon("bar-chart"), theme = "secondary"),
             value_box(title = "Generation Interval",      value = uiOutput(ns("vb_genint")),
                       showcase = bsicons::bs_icon("hourglass-split"),  theme = "secondary"),
             value_box(title = "Equivalent Complete Generations",
                       value = uiOutput(ns("vb_ecg")),
                       showcase = bsicons::bs_icon("house"), theme = "info"),
             value_box(title = "Effective Population Size (Ne) \u24d8",
                       value = uiOutput(ns("vb_ne")),
                       showcase = bsicons::bs_icon("people"), theme = "info"),
             col_widths = c(2, 2, 2, 2, 2, 2)
           ),
           p(""),
           layout_columns(
             card(card_header("Recommendations"), card_body(uiOutput(ns("advice")))),
             uiOutput(ns("popstatus")),
             col_widths = c(10, 2)
           ),
           p(""),
           card(
             card_header("Birth Statistics by Year"),
             layout_sidebar(
               sidebar = sidebar(
                 selectInput(ns("metric"), "Choose metric",
                             choices = c(
                               "Mean Litter Size"               = "MeanLitterSize",
                               "Number of litters born"         = "Litters",
                               "Number of progeny born"         = "Animals",
                               "Number of females born"         = "Females",
                               "Number of males born"           = "Males",
                               "Number of female breeders born" = "FemaleBreeders",
                               "Number of male breeders born"   = "MaleBreeders"
                             ))
               ),
               plotlyOutput(ns("birthplot"))
             )
           ),
           uiOutput(ns("conditional_table_ui")),
           card(card_header("Overview Table"),
                card_body(dataTableOutput(ns("overview_table"))),
                min_height = "500px")
  )
}
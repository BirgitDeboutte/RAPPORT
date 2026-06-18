# ── pop_dams_ui.R ─────────────────────────────────────────────────────────────
popDamsPanelUI <- function(ns) {
  tabPanel("Dams",
           p(""),
           uiOutput(ns("year_filter_badge")),
           layout_columns(
             value_box(title = "Total Dams",        value = uiOutput(ns("vb_dam_total")),
                       showcase = bsicons::bs_icon("gender-female"),      theme = "primary"),
             value_box(title = "Own Population",    value = uiOutput(ns("vb_dam_own")),
                       showcase = bsicons::bs_icon("house"),              theme = "info"),
             value_box(title = "External",          value = uiOutput(ns("vb_dam_ext")),
                       showcase = bsicons::bs_icon("box-arrow-in-right"), theme = "secondary"),
             value_box(title = "Mean Litters / Dam",value = uiOutput(ns("vb_dam_lit")),
                       showcase = bsicons::bs_icon("diagram-3"),          theme = "secondary"),
             value_box(title = "Mean Progeny / Dam",value = uiOutput(ns("vb_dam_prog")),
                       showcase = bsicons::bs_icon("diagram-3"),          theme = "secondary"),
             col_widths = c(2, 2, 2, 3, 3)
           ),
           p(""),
           checkboxInput(ns("own_dams"), "Show only dams born in own population", value = FALSE),
           card(card_header("Dams by Birthyear"), card_body(plotlyOutput(ns("damplot")))),
           layout_columns(
             card(card_header("Dams"),         card_body(dataTableOutput(ns("dams")),      min_height = "500px")),
             card(card_header("Selected Dam"), card_body(dataTableOutput(ns("selectdam")), min_height = "500px")),
             col_widths = c(6, 6)
           ),
           card(card_header("Litter From Selected Dam"),
                card_body(dataTableOutput(ns("selectdamlitter")), min_height = "700px"))
  )
}
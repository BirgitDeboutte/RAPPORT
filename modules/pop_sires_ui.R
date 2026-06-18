# ── pop_sires_ui.R ────────────────────────────────────────────────────────────
popSiresPanelUI <- function(ns) {
  tabPanel("Sires",
           p(""),
           uiOutput(ns("year_filter_badge")),
           layout_columns(
             value_box(title = "Total Sires",         value = uiOutput(ns("vb_sire_total")),
                       showcase = bsicons::bs_icon("gender-male"),        theme = "primary"),
             value_box(title = "Own Population",      value = uiOutput(ns("vb_sire_own")),
                       showcase = bsicons::bs_icon("house"),              theme = "info"),
             value_box(title = "External",            value = uiOutput(ns("vb_sire_ext")),
                       showcase = bsicons::bs_icon("box-arrow-in-right"), theme = "secondary"),
             value_box(title = "Mean Litters / Sire", value = uiOutput(ns("vb_sire_lit")),
                       showcase = bsicons::bs_icon("diagram-3"),          theme = "secondary"),
             value_box(title = "Mean Progeny / Sire", value = uiOutput(ns("vb_sire_prog")),
                       showcase = bsicons::bs_icon("diagram-3"),          theme = "secondary"),
             col_widths = c(2, 2, 2, 3, 3)
           ),
           p(""),
           checkboxInput(ns("own_sires"), "Show only sires born in own population", value = FALSE),
           card(card_header("Sires by Birthyear"), card_body(plotlyOutput(ns("sireplot")))),
           layout_columns(
             card(card_header("Sires"),          card_body(dataTableOutput(ns("sires")),       min_height = "500px")),
             card(card_header("Selected Sire"),  card_body(dataTableOutput(ns("selectsire")),  min_height = "500px")),
             col_widths = c(6, 6)
           ),
           card(card_header("Litter From Selected Sire"),
                card_body(dataTableOutput(ns("selectsirelitter")), min_height = "700px"))
  )
}
# ── pop_breeders_ui.R ─────────────────────────────────────────────────────────
# Accepts ns (the parent popUI namespace function) instead of an id.

popBreedersPanelUI <- function(ns) {
  tabPanel("Active Breeding Stock",
           p(""),
           uiOutput(ns("no_breeders_warning_ph")),
           layout_columns(
             value_box(title    = "Dams",
                       value    = uiOutput(ns("vb_br_dams")),
                       showcase = bsicons::bs_icon("gender-female"), theme = "primary"),
             value_box(title    = "Sires",
                       value    = uiOutput(ns("vb_br_sires")),
                       showcase = bsicons::bs_icon("gender-male"),   theme = "primary"),
             value_box(title    = "Own Population Breeders",
                       value    = uiOutput(ns("vb_br_pct")),
                       showcase = bsicons::bs_icon("house"),         theme = "info"),
             value_box(title    = "External Dams",
                       value    = uiOutput(ns("vb_br_ext_dams")),
                       showcase = bsicons::bs_icon("gender-female"), theme = "info"),
             value_box(title    = "External Sires",
                       value    = uiOutput(ns("vb_br_ext_sires")),
                       showcase = bsicons::bs_icon("gender-male"),   theme = "info"),
             value_box(title    = "Mean Kinship \u24d8",
                       value    = uiOutput(ns("vb_br_kinship")),
                       showcase = bsicons::bs_icon("diagram-3"),     theme = "secondary"),
             col_widths = c(2, 2, 2, 2, 2, 2)
           ),
           p(""),
           layout_columns(
             card(
               card_header("Filters"),
               card_body(
                 tags$p(style = paste0("font-size:0.78rem; font-weight:700; text-transform:uppercase; ",
                                       "letter-spacing:0.06em; color:#888; margin-bottom:6px"),
                        "Show"),
                 checkboxGroupInput(
                   ns("breeder_ov_sex"),
                   label    = NULL,
                   choices  = c("Dams", "Sires"),
                   selected = c("Dams", "Sires"),
                   inline   = TRUE
                 ),
                 hr(style = "margin: 10px 0"),
                 tags$p(style = paste0("font-size:0.78rem; font-weight:700; text-transform:uppercase; ",
                                       "letter-spacing:0.06em; color:#888; margin-bottom:6px"),
                        "Compare against population mean"),
                 uiOutput(ns("breeder_ov_vars")),
                 checkboxInput(ns("breeder_ov_override_cat"),
                               "Treat selected numeric variables as categorical",
                               value = FALSE)
               )
             ),
             col_widths = c(12)
           ),
           p(""),
           uiOutput(ns("breeder_trait_plot_ui")),
           p(""),
           dataTableOutput(ns("breeder_ov_table"))
  )
}
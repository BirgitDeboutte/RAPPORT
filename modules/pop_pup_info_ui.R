# ── pop_pup_info_ui.R ─────────────────────────────────────────────────────────
# Accepts ns (the parent popUI namespace function) instead of an id.

popPupInfoPanelUI <- function(ns) {
  tabPanel("Animal Info",
           p(""),
           layout_columns(
             card(
               card_body(
                 layout_columns(
                   selectizeInput(ns("selected_id"), "Select Animal",
                                  choices  = NULL,
                                  multiple = FALSE,
                                  options  = list(placeholder = "Type to search\u2026",
                                                  dropdownParent = "body")),
                   selectizeInput(ns("extra_columns"), "Additional Info to Display",
                                  choices  = NULL,
                                  multiple = TRUE,
                                  options  = list(placeholder     = "Select extra columns\u2026",
                                                  plugins         = list("remove_button"),
                                                  dropdownParent  = "body")),
                   col_widths = c(4, 8)
                 )
               )
             ),
             col_widths = c(12)
           ),
           p(""),
           layout_columns(
             value_box(title    = "Sex",
                       value    = uiOutput(ns("vb_sex")),
                       showcase = bsicons::bs_icon("gender-ambiguous"), theme = "primary"),
             value_box(title    = "Date of Birth",
                       value    = uiOutput(ns("vb_dob")),
                       showcase = bsicons::bs_icon("calendar3"),        theme = "primary"),
             value_box(title    = "Dam",
                       value    = uiOutput(ns("vb_dam")),
                       showcase = bsicons::bs_icon("gender-female"),    theme = "info"),
             value_box(title    = "Sire",
                       value    = uiOutput(ns("vb_sire")),
                       showcase = bsicons::bs_icon("gender-male"),      theme = "info"),
             value_box(title    = "Coefficient of Inbreeding (COI) \u24d8",
                       value    = uiOutput(ns("vb_f")),
                       showcase = bsicons::bs_icon("diagram-2"),        theme = "secondary"),
             value_box(title    = "Littermates",
                       value    = uiOutput(ns("vb_littermates")),
                       showcase = bsicons::bs_icon("people"),           theme = "secondary"),
             col_widths = c(2, 2, 2, 2, 2, 2)
           ),
           p(""),
           layout_columns(
             tagList(
               card(
                 card_header("Animal Details"),
                 card_body(
                   uiOutput(ns("breeder_toggle_ui")),
                   hr(style = "margin: 8px 0"),
                   dataTableOutput(ns("pup_overview_table")),
                   uiOutput(ns("extra_columns_section"))
                 )
               )
             ),
             card(
               card_header("Family Browser"),
               card_body(
                 tags$p(style = "font-size:0.82rem; color:#888; margin-bottom:10px;",
                        "Click a row in Animal Details to browse related animals."),
                 uiOutput(ns("table_title")),
                 dataTableOutput(ns("filtered_table"))
               )
             ),
             col_widths = c(4, 8)
           )
  )
}
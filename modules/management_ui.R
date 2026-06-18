# ── management_ui.R ───────────────────────────────────────────────────────────
managementUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Management",
           tabsetPanel(
             
             # ── Population Growth ────────────────────────────────────────────────────
             tabPanel("Population Growth",
                      p(""),
                      uiOutput(ns("management_data_warning")),  
                      uiOutput(ns("no_breeders_warning")),
                      layout_columns(
                        value_box(title = "Target Progeny / Year",        value = uiOutput(ns("vb_goal_pups")),
                                  showcase = bsicons::bs_icon("bullseye"),      theme = "primary"),
                        value_box(title = "Litters Needed / Year",     value = uiOutput(ns("vb_goal_litters")),
                                  showcase = bsicons::bs_icon("diagram-3"),     theme = "primary"),
                        value_box(title = "Active Dams Needed",        value = uiOutput(ns("vb_goal_dams")),
                                  showcase = bsicons::bs_icon("gender-female"), theme = "info"),
                        value_box(title = "Expected Progeny This Year",   value = uiOutput(ns("vb_capacity")),
                                  showcase = bsicons::bs_icon("speedometer2"),  theme = "secondary"),
                        col_widths = c(3, 3, 3, 3)
                      ),
                      p(""),
                      layout_columns(
                        card(
                          card_header("Breeding Goals & Inputs"),
                          card_body(
                            numericInput(ns("goal"), "Desired Yearly Progeny Count", value = 150, min = 1),
                            hr(),
                            layout_columns(
                              tagList(
                                tags$div(style = "font-size:0.8rem; font-weight:700; text-transform:uppercase;
                    letter-spacing:0.06em; color:#888; margin-bottom:8px", "Dams"),
                                numericInput(ns("litdam"), "Max Litters Per Dam", value = 4, min = 1, max = 10),
                                selectizeInput(ns("retire_col"),
                                            label = tagList(
                                              "Early/Planned Retirement Date Column",
                                              tags$span(
                                                `data-bs-toggle`    = "tooltip",
                                                `data-bs-placement` = "right",
                                                title = paste0(
                                                  "Optional. If your data contains a column with early/planned retirement ",
                                                  "dates for dams, select it here. The app uses these dates to determine ",
                                                  "when a dam will no longer be available for breeding, which affects active ",
                                                  "dam counts and production projections. If no such column exists, leave ",
                                                  "this set to None \u2014 the app will estimate retirement from the max ",
                                                  "litters per dam setting instead."
                                                ),
                                                style = "cursor:help; margin-left:4px",
                                                bsicons::bs_icon("info-circle",
                                                                 style = "color:#aaa; font-size:0.8rem")
                                              )
                                            ),
                                            choices = c("None"), selected = "None",
                                            options = list(placeholder = "Type to search\u2026", dropdownParent = "body")
                                            ),
                                uiOutput(ns("retire_col_warning"))
                              ),
                              tagList(
                                tags$div(style = "font-size:0.8rem; font-weight:700; text-transform:uppercase;
                    letter-spacing:0.06em; color:#888; margin-bottom:8px", "Sires"),
                                numericInput(ns("litsire"), "Max Litters Per Sire (Own Pop)", value = 2, min = 1, max = 10),
                                sliderInput(ns("siresext_pct"), "External Sires (% of litters)",
                                            min = 0, max = 50, value = 10, step = 5, post = "%"),
                                uiOutput(ns("ne_recommendation"))
                              ),
                              col_widths = c(6, 6)
                            ),
                            hr(),
                            sliderInput(ns("correction_pct"),
                                        label = tagList(
                                          "Expected Litter Correction(%)",
                                          tags$span("*",
                                                    `data-bs-toggle`    = "tooltip",
                                                    `data-bs-placement` = "right",
                                                    title = paste(
                                                      "A planning buffer that accounts for dams who are expected to be available ",
                                                      "for breeding but do not produce a litter that year — for example due to a ",
                                                      "failed insemination, a health issue, skipped cycles or other, unforseen ",
                                                      "circumstances."
                                                    ),
                                                    style = "color:#0277bd; cursor:help; font-weight:700; margin-left:3px"
                                          )
                                        ),
                                        min = 0, max = 20, value = 5, step = 1, post = "%"),
                            checkboxInput(ns("use_empirical_correction"),
                                          label = tagList(
                                            "Calculate correction rate from data",
                                            tags$span(
                                              "\u26a0",
                                              `data-bs-toggle`    = "tooltip",
                                              `data-bs-placement` = "right",
                                              title = paste0(
                                                "Estimated as as the proportion of dam-years in which an active, non-retired ",
                                                "dam did not produce a litter. If present in your data, the retirement date column ",
                                                "must be selected above. If not, early retired dams may be counted as active. "
                                              ),
                                              style = "color:#e65100; cursor:help; margin-left:4px; font-size:0.85rem"
                                            ),
                                            uiOutput(ns("empirical_correction_hint"), inline = TRUE)
                                          ),
                                          value = FALSE)
                          )
                        ),
                        card(
                          card_header("Programme Breakdown"),
                          card_body(
                            tags$table(
                              class = "table table-sm table-borderless mb-0",
                              style = "font-size:1.05rem; width:100%",
                              tags$colgroup(tags$col(style = "width:70%"), tags$col(style = "width:30%")),
                              tags$tbody(
                                tags$tr(
                                  tags$td(style = "color:#444; padding:7px 8px 7px 0", "Target pups / year"),
                                  tags$td(class = "text-end", style = "font-weight:600; padding:7px 0", uiOutput(ns("pb_goal_pups")))
                                ),
                                tags$tr(
                                  tags$td(style = "color:#444; padding:7px 8px 7px 0", "Litters needed / year"),
                                  tags$td(class = "text-end", style = "font-weight:600; padding:7px 0", uiOutput(ns("pb_goal_litters")))
                                ),
                                tags$tr(tags$td(colspan = "2", style = "padding:4px 0")),
                                tags$tr(
                                  tags$td(style = "color:#444; padding:7px 8px 7px 0", "Active dams needed in total to reach goal"),
                                  tags$td(class = "text-end", style = "font-weight:600; padding:7px 0", uiOutput(ns("pb_goal_dams")))
                                ),
                                tags$tr(
                                  tags$td(style = "color:#444; padding:7px 8px 7px 0", "Dams to select / year"),
                                  tags$td(class = "text-end", style = "font-weight:600; padding:7px 0", uiOutput(ns("pb_dams_replaced")))
                                ),
                                tags$tr(
                                  tags$td(style = "color:#444; padding:7px 8px 7px 0", "Max litters per dam"),
                                  tags$td(class = "text-end", style = "font-weight:600; padding:7px 0", uiOutput(ns("pb_litdam")))
                                ),
                                tags$tr(tags$td(colspan = "2", style = "padding:4px 0")),
                                tags$tr(
                                  tags$td(style = "color:#444; padding:7px 8px 7px 0", "Sires to select / year"),
                                  tags$td(class = "text-end", style = "font-weight:600; padding:7px 0", uiOutput(ns("pb_sires_int")))
                                ),
                                tags$tr(
                                  tags$td(style = "color:#444; padding:7px 8px 7px 0", "Max litters per own-pop sire"),
                                  tags$td(class = "text-end", style = "font-weight:600; padding:7px 0", uiOutput(ns("pb_litsire")))
                                ),
                                tags$tr(
                                  tags$td(style = "color:#444; padding:7px 8px 7px 0", "Litters from own-pop sires"),
                                  tags$td(class = "text-end", style = "font-weight:600; padding:7px 0", uiOutput(ns("pb_litters_int")))
                                ),
                                tags$tr(
                                  tags$td(style = "color:#444; padding:7px 8px 7px 0", "Litters from external sires"),
                                  tags$td(class = "text-end", style = "font-weight:600; padding:7px 0", uiOutput(ns("pb_litters_ext")))
                                )
                              )
                            )
                          )
                        ),
                        col_widths = c(4, 8)
                      ),
                      p(""),
                      layout_columns(
                        card(
                          card_header(layout_columns(
                            "Last Year",
                            div(style = "text-align:right", uiOutput(ns("last_year_badge"))),
                            col_widths = c(8, 4)
                          )),
                          card_body(uiOutput(ns("last_year")))
                        ),
                        card(
                          card_header(layout_columns(
                            "This Year (Projected)",
                            div(style = "text-align:right", uiOutput(ns("this_year_badge"))),
                            col_widths = c(8, 4)
                          )),
                          card_body(uiOutput(ns("current")))
                        ),
                        col_widths = c(6, 6)
                      )
             ),
             
             # ── Dam Planner ──────────────────────────────────────────────────────────
             tabPanel("Dam Planner",
                      p(""),
                      uiOutput(ns("management_data_warning")),  
                      uiOutput(ns("no_breeders_warning_dam")), 
                      uiOutput(ns("retire_col_warning_dam")),
                      layout_columns(
                        value_box(title = "Avg Age at First Litter",  value = uiOutput(ns("vb_age_first")),
                                  showcase = bsicons::bs_icon("calendar2-heart"), theme = "primary"),
                        value_box(title = "Avg Litter Interval",      value = uiOutput(ns("vb_interval")),
                                  showcase = bsicons::bs_icon("arrow-repeat"),    theme = "info"),
                        value_box(title = "Avg Litters / Year / Dam", value = uiOutput(ns("vb_per_year")),
                                  showcase = bsicons::bs_icon("diagram-3"),        theme = "success"),
                        value_box(title = "Mean Litter Size",         value = uiOutput(ns("vb_litter_size")),
                                  showcase = bsicons::bs_icon("bar-chart"),        theme = "secondary"),
                        value_box(title = "Generation Interval",      value = uiOutput(ns("vb_genint")),
                                  showcase = bsicons::bs_icon("hourglass-split"),  theme = "secondary"),
                        col_widths = c(2, 2, 2, 3, 3)
                      ),
                      p(""),
                      layout_columns(
                        card(
                          card_header("Dam Activity Summary"),
                          card_body(
                            layout_columns(
                              uiOutput(ns("dam_status_active")),
                              uiOutput(ns("dam_status_upcoming")),
                              uiOutput(ns("dam_status_retiring")),
                              uiOutput(ns("dam_status_retired")),
                              col_widths = c(3, 3, 3, 3)
                            ),
                            uiOutput(ns("dam_detail_section"))
                          )
                        ),
                        card(
                          card_header("Filter Chart"),
                          card_body(
                            checkboxGroupInput(ns("dam_filter"), label = NULL,
                                               choices  = c("Active" = "Active or Eligible This Year", "Upcoming", "Retired"),
                                               selected = c("Active or Eligible This Year", "Upcoming")),
                            sliderInput(ns("gantt_year_range"), "Year range:",
                                        min = 2015, max = 2032, value = c(2020, 2030), sep = "")
                          )
                        ),
                        col_widths = c(8, 4)
                      ),
                      p(""),
                      h4("Dam Timeline"),
                      tags$p(
                        style = "font-size:0.87rem; color:#555; margin-bottom:6px",
                        tags$span(style = "display:inline-block; width:14px; height:10px; background:lightgrey;
             border-radius:2px; margin-right:4px; vertical-align:middle"),
                        tags$b("Grey"), " = estimated breeding window based on population averages.\u2003",
                        tags$span(style = "display:inline-block; width:14px; height:10px; background:steelblue;
             border-radius:2px; margin-right:4px; vertical-align:middle"),
                        tags$b("Blue"), " = actual litters recorded in the data file."
                      ),
                      plotlyOutput(ns("gantt_chart"), height = "600px")
             ),
             
             # ── Progeny Planner ──────────────────────────────────────────────────────────
             tabPanel("Progeny Planner",
                      p(""),
                      uiOutput(ns("management_data_warning")),  
                      uiOutput(ns("no_breeders_warning_pup")), 
                      uiOutput(ns("retire_col_warning_pup")),
                      layout_columns(
                        value_box(title = "Progeny Born Last Year",  value = uiOutput(ns("vb_pups_last")),
                                  showcase = bsicons::bs_icon("heart-fill"),      theme = "primary"),
                        value_box(title = "Expected Last Year",   value = uiOutput(ns("vb_pups_expected_last")),
                                  showcase = bsicons::bs_icon("calculator"),      theme = "info"),
                        value_box(title = "Coverage Last Year",   value = uiOutput(ns("vb_coverage")),
                                  showcase = bsicons::bs_icon("percent"),         theme = "success"),
                        value_box(title = "Projected This Year",  value = uiOutput(ns("vb_pups_projected")),
                                  showcase = bsicons::bs_icon("graph-up-arrow"),  theme = "secondary"),
                        col_widths = c(3, 3, 3, 3)
                      ),
                      p(""),
                      h4("Progeny Timeline"),
                      p(tags$small(tags$em("Click a year bar to see a detailed dam overview for that year."))),
                      plotlyOutput(ns("pup_projection")),
                      p(""),
                      uiOutput(ns("yearly_dam_table_header")),
                      DT::dataTableOutput(ns("yearly_dam_table"))
             ),
             
             # ── Breeder Selection ────────────────────────────────────────────────────
             tabPanel("Breeder Selection",
                      p(""),
                      uiOutput(ns("management_data_warning")),  
                      uiOutput(ns("no_breeders_warning_bs")), 
                      # ── Row 1: six KPI value boxes ─────────────────────────────────────────
                      layout_columns(
                        value_box(title = "New Dams Needed / Year",        value = uiOutput(ns("vb_bs_dams_needed")),
                                  showcase = bsicons::bs_icon("gender-female"),    theme = "primary"),
                        value_box(title = "New Sires Needed / Year",       value = uiOutput(ns("vb_bs_sires_needed")),
                                  showcase = bsicons::bs_icon("gender-male"),      theme = "primary"),
                        value_box(title = "Dams Taken Back This Year",     value = uiOutput(ns("vb_bs_dams_back")),
                                  showcase = bsicons::bs_icon("check2-circle"),    theme = "info"),
                        value_box(title = "Sires Taken Back This Year",    value = uiOutput(ns("vb_bs_sires_back")),
                                  showcase = bsicons::bs_icon("check2-circle"),    theme = "info"),
                        value_box(title = "Current Active Breeders Passing All Mandatory", value = uiOutput(ns("vb_breeders_passing")),
                                  showcase = bsicons::bs_icon("people-fill"),      theme = "success"),
                        col_widths = c(2, 2, 3, 3, 2)
                      ),
                      
                      p(""),
                      
                      # ── Row 2: criteria (left) | status + compliance (right) ───────────────
                      layout_columns(
                        
                        card(
                          card_header("Selection Criteria / Variables To Show In The Table"),
                          card_body(
                            # Variable selector
                            uiOutput(ns("goal_var_selector")),
                            p(""),
                            # Per-variable: direction symbol + value + priority
                            uiOutput(ns("goal_criteria_inputs")),
                            p(""),
                            layout_columns(
                              uiOutput(ns("breedertimerange")),
                              tags$p(
                                style = "font-size:0.78rem; color:#888; margin-top:-8px",
                                "Filter to animals born within this date range. Widen the range if no candidates appear."
                              ),
                              checkboxGroupInput(ns("toggle_sex"), "Show:",
                                                 choices  = c("Show Sires", "Show Dams"),
                                                 selected = c("Show Sires", "Show Dams")),
                              col_widths = c(6, 6)
                            ),
                            p(""),
                            actionButton(ns("save_goals"),  "Save Goals",
                                         class = "btn btn-primary"),
                            actionButton(ns("breedersbutton"), "Show Potential Breeders",
                                         class = "btn btn-primary"),
                            actionButton(ns("clear_goals"), "Clear Goals",
                                         class = "btn btn-outline-danger ms-2"),
                            p(""),
                            uiOutput(ns("goals_saved_alert"))
                          )
                        ),
                        
                        card(
                          card_header("Status & Compliance"),
                          card_body(
                            tabsetPanel(
                              tabPanel("Recruitment Status",
                                       p(""),
                                       uiOutput(ns("recruitment_status"))
                              ),
                              tabPanel("Compliance Overview",
                                       p(""),
                                       p(tags$small(tags$em(
                                         "How current breeders compare against each saved goal. ",
                                         "Mandatory goals also apply as hard filters in Ranked Mating."
                                       ))),
                                       p(""),
                                       uiOutput(ns("compliance_rank_order_ui")), 
                                       p(""),
                                       tags$div(
                                         style = "font-size:0.82rem; color:#666; margin-top:8px",
                                         tags$p(
                                           style = "margin-bottom:6px",
                                           tags$em("Click any column header to sort.")
                                         )
                                       ),
                                       DT::dataTableOutput(ns("compliance_table")),
                                       p("")
                              )
                            )
                          )
                        ),
                        
                        col_widths = c(6, 6)
                      ),
                      
                      p(""),
                      
                      # ── Row 3: potential breeders ──────────────────────────────────────────
                      tags$hr(),
                      uiOutput(ns("kinship_constraint_panel")),
                      p(""),
                      tags$h5(style = "color:#517066; margin-top:16px; margin-bottom:8px;",
                              "Potential Breeders"),
                      dataTableOutput(ns("breederselection")),
                      
                      # ── Row 4: selected + convert ──────────────────────────────────────────
                      tags$hr(),
                      tags$h5(style = "color:#517066; margin-top:16px; margin-bottom:8px;",
                              "Selected Breeders"),
                      uiOutput(ns("convert_to_breed")),
                      uiOutput(ns("breeders_converted")),
                      p(""),
                      dataTableOutput(ns("selected_new_breeders"))
             )
           )
  )
}
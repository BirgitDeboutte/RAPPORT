# ── mating_ranking_ui.R ───────────────────────────────────────────────────────
rankingUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Ranked Mating",
           p(""),
           
           # ── Parameter input ───────────────────────────────────────────────────
           card(
             card_header("Parameters"),
             card_body(
               layout_columns(
                 
                 # Left col: test parent + inbreeding limits
                 div(
                   uiOutput(ns("testouderout")),
                   p(""),
                   card(
                     card_header("Inbreeding Limit"),
                     card_body(
                       sliderInput(ns("coiinc"), "Max. COI Increase Over Parental Average",
                                   min = 0, max = 0.15, value = 0.01,
                                   step = 0.01, ticks = FALSE),
                       tags$div(
                         style = paste0("background:#f4f7f6; border-left:3px solid #517066; ",
                                        "padding:9px 13px; margin-top:8px; ",
                                        "border-radius:0 4px 4px 0; ",
                                        "font-size:0.78rem; color:#444; line-height:1.45"),
                         tags$b(style = "color:#517066", "Why filter on COI increase?"),
                         tags$br(),
                         "Filtering on the increase over the parental average limits the ",
                         tags$i("rate"), " at which inbreeding accumulates across generations, ",
                         "rather than the absolute level in any individual offspring. ",
                         "Absolute COI is largely a function of pedigree depth; a deep pedigree ",
                         "shows higher absolute COIs even under careful mating, while a shallow one ",
                         "can show low absolute COIs even under poor choices. The increase is the ",
                         "quantity actually under the breeding manager\u2019s control.",
                         tags$br(), tags$br(),
                         "The default of ", tags$b("0.01"), " aligns with the 1% per-generation ",
                         "threshold corresponding to ",
                         tags$i("N"), tags$sub("e"), " < 50 used throughout the app. ",
                         "If no eligible candidates remain, relax the threshold incrementally and ",
                         "prioritise the candidate closest to 0.01."
                       )
                     )
                   )
                 ),
                 
                 # Right col: criteria (left half) + ranking priority (right half)
                 card(
                   card_header("Selection Criteria & Ranking"),
                   card_body(
                     div(
                         uiOutput(ns("selectvars")),
                         p(""),
                         uiOutput(ns("criteria"))
                         )
                     ),
                     
                     p(""),
                     
                     # ── Parameter goals toggle ──────────────────────────────────
                     tags$div(
                       style = "border-top:1px solid #eee; padding-top:10px; margin-top:4px",
                       checkboxInput(
                         ns("use_param_goals"),
                         tagList(
                           bsicons::bs_icon("flag-fill"),
                           tags$span("Apply Parameter Goals from Management",
                                     style = "margin-left:4px; font-size:0.9rem")
                         ),
                         value = FALSE
                       ),
                       uiOutput(ns("param_goals_preview"))
                     ),
                     
                     p(""),
                     actionButton(ns("rankbutton"), "Get Ranking", class = "btn btn-primary")
                   ),
                 col_widths = c(3, 9)
                 )
               )
             ),
           
           p(""),
           
           # ── Test parent ───────────────────────────────────────────────────────
           tags$h5(style = "color:#517066; margin-top:16px; margin-bottom:8px;",
                   "Selected Test Parent"),
           dataTableOutput(ns("testparenttable")),
           
           tags$hr(),
           
           # ── Ranked candidates ─────────────────────────────────────────────────
           tags$h5(style = "color:#517066; margin-top:16px; margin-bottom:8px;",
                   "Ranked Candidates (sorted by COI increase, low to high)"),
           dataTableOutput(ns("ranktable")),
           p(""),
           uiOutput(ns("download_ui")),
           
           tags$hr(),
           
           # ── Pedigree of selected mating ───────────────────────────────────────
           tags$h5(style = "color:#517066; margin-top:16px; margin-bottom:8px;",
                   "Pedigree of Selected Mating"),
           imageOutput(ns("pedtree2"))
  )
}
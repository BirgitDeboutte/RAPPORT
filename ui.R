source("modules/data_input_viewer_ui.R")
source("modules/pop_ui.R")     
source("modules/management_ui.R")
source("modules/mating_ranking_ui.R")

ui <- fluidPage(
  ##### Formatting #######################
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Nunito:wght@400;500;600&display=swap"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Baloo:wght@400;500;600&display=swap"),
    tags$script(HTML("
        $(document).ready(function() {
          const observer = new MutationObserver(() => {
           document.querySelectorAll('[data-bs-toggle=\"tooltip\"]:not([data-bs-original-title])').forEach(el => {
              new bootstrap.Tooltip(el, { trigger: 'hover' });
           });
          });
         observer.observe(document.body, { childList: true, subtree: true });
       });
      ")),
    tags$style(HTML(
      ".shiny-notification {
         position: fixed;
         top: calc(50%);
         left: calc(40%);
       }
       .app-title {
         display: flex;
         align-items: center;
         justify-content: left;
         margin-bottom: 10px;
       }
       .app-title img {
         height: 50px;
         width: 50px;
         margin-top: 10px;
       }
       .app-title h1 {
          font-size: 24px;
          margin: 0;
          color: #517066;
          font-family: 'Baloo', sans-serif;
          letter-spacing: 0.08em;
        }
       .card-equal-height {
         display: flex;
         flex-direction: column;
         height: 100%;
       }
       .card-equal-height .card-body {
         display: flex;
         flex-direction: column;
         flex-grow: 1;
         min-height: 300px;
       }
       .card-footer {
         display: flex;
         justify-content: center;
         margin-top: auto;
       }"
    ))
  ),
  
  theme = bslib::bs_theme(
    version    = 5,
    bootswatch = "minty",
    base_font  = "Nunito",
    primary    = "#517066",
    success    = "#d4edda",
    secondary  = "#f4f7f6",
    font_scale = 1
  ),
  
  div(class = "app-title",
      img(src = "logo.png", alt = "RAPPORT Logo"),
      div(
        h1("RAPPORT", style = "margin: 0;"),
        tags$p("Relatedness Analysis and Population Planning, Organisation, Reporting and Tracking",
               style = "font-size: 11px; color: #517066; opacity: 0.75; margin: 0;
                      font-family: 'Nunito', sans-serif;")
      )),
  
  useShinyjs(),
  
  navbarPage(
    titlePanel(""),
    windowTitle = "RAPPORT",
    
    # ── Data Input ──────────────────────────────────────────────────────
    dataInputViewerUI("datainputviewer"),
    
    # ── Population ───────────────────────────────────────────────────────
    tabPanel("Population",
             popUI("pop")
    ),
    
    # ── Management ───────────────────────────────────────────────────────
    managementUI("management"),
    
    # ── Matings ──────────────────────────────────────────────────────────
    tabPanel("Mating",
             tabsetPanel(
               rankingUI("ranking")
             )
    )
  )
)
# ── pop_overview_server.R ─────────────────────────────────────────────────────
# Panel helper for the "Population Overview" tab.
# Called from popServer(); shares input/output/session namespace.
#
# Reactive args: litter_ov, dam_table, sire_table, selected_year
# Inputs consumed: metric
# Defines outputs: vb_animals, vb_litters, vb_litsize, vb_sexratio, vb_breeders,
#                  vb_ne, advice, popstatus, birthplot, conditional_table_ui,
#                  conditional_table, overview_table

popOverviewHelper <- function(
    input, output, session, rd,
    litter_ov,
    dam_table,
    sire_table,
    selected_year
) {
  ns <- session$ns
  
  # ── KPI reactive ─────────────────────────────────────────────────────────────
  overview_kpis <- reactive({
    req(litter_ov(), rd$ownpop)
    ov       <- litter_ov()
    own_ids  <- rd$ownpop[[rd$datid]]
    own_ov   <- ov[ov$Dam %in% own_ids | ov$Sire %in% own_ids, ]
    sizes    <- own_ov$Littersize
    mean_lit <- mean(sizes, na.rm = TRUE)
    se       <- sd(sizes,   na.rm = TRUE) / sqrt(sum(!is.na(sizes)))
    n_females  <- sum(ov$`# Females`)
    n_males    <- sum(ov$`# Males`)
    total      <- n_females + n_males
    D          <- dam_table()
    S          <- sire_table()
    own_dams   <- D[D$Dam  %in% own_ids, ]
    own_sires  <- S[S$Sire %in% own_ids, ]
    n_breeders <- nrow(own_dams) + nrow(own_sires)
    list(
      total_animals = total,
      total_litters = nrow(ov),
      mean_litter   = round(mean_lit, 1),
      ci_lower      = round(mean_lit - 1.96 * se, 1),
      ci_upper      = round(mean_lit + 1.96 * se, 1),
      sex_ratio     = if (total > 0)
        paste0(round(n_females / total * 100, 0), "% F  /  ",
               round(n_males   / total * 100, 0), "% M")
      else "N/A",
      n_breeders = n_breeders
    )
  })
  
  # ── Value boxes ───────────────────────────────────────────────────────────────
  output$vb_animals <- renderUI({ req(overview_kpis()); h3(overview_kpis()$total_animals) })
  output$vb_litters <- renderUI({ req(overview_kpis()); h3(overview_kpis()$total_litters) })
  
  output$vb_litsize <- renderUI({
    req(overview_kpis())
    kv <- overview_kpis()
    h3(paste0(kv$mean_litter, " [", kv$ci_lower, "\u2013", kv$ci_upper, "]"))
  })
  
  output$vb_genint <- renderUI({
    req(rd$genint)
    tags$div(
      `data-bs-toggle`    = "tooltip",
      `data-bs-placement` = "bottom",
      title = paste0(
        "The average age of dams when their offspring are born, weighted by litter size. ",
        "A shorter generation interval means faster genetic change. ",
        "Current value: ", round(rd$genint, 2), " years."
      ),
      style = "display:inline-block; cursor:help",
      h3(
        paste(round(rd$genint, 2), "yrs"),
        tags$sup("\u24d8", style = "font-size:0.6rem; color:#aaa; vertical-align:super; margin-left:2px")
      )
    )
  })
  
  
  output$vb_ecg <- renderUI({
    req(rd$ecg)
    tags$div(
      `data-bs-toggle`    = "tooltip",
      `data-bs-placement` = "bottom",
      title = paste0(
        "Equivalent number of fully known ancestral generations, averaged across ",
        "the reference population. Higher values indicate more complete pedigree ",
        "records and more reliable inbreeding and Ne estimates. Low values ",
        "means possible under-estimation of kinship and inbreeding, and over-estimation ",
        "of Ne."
      ),
      style = "display:inline-block; cursor:help",
      h3(round(rd$ecg, 1)),
      tags$span(
        paste0("ECG\u24d8"),
        style = "font-size:0.72rem; color:#555;"
      )
    )
  })
  
  output$vb_ne <- renderUI({
    req(rd$Ne)
    Ne    <- round(rd$Ne)
    color <- if (Ne > 200) "#2e7a3a" else if (Ne > 100) "#558b2f" else if (Ne >= 50) "#e65100" else "#c62828"
    label <- if (Ne > 200) "Adequate" else if (Ne > 100) "Acceptable" else if (Ne >= 50) "At risk" else "Critical"
    dF    <- if (!is.null(rd$deltaF) && is.finite(rd$deltaF)) round(rd$deltaF, 4) else NULL
    se_dF <- if (!is.null(rd$se_dF) && is.finite(rd$se_dF)) rd$se_dF else NULL
    se_Ne <- if (!is.null(rd$se_Ne) && is.finite(rd$se_Ne)) rd$se_Ne else NULL
    
    ne_ci <- if (!is.null(se_Ne)) paste0("(95% CI: ", round(Ne - 1.96 * se_Ne), "\u2013", round(Ne + 1.96 * se_Ne), ")") else NULL
    df_ci <- if (!is.null(se_dF) && !is.null(dF)) paste0(" (95% CI: ", round(dF - 1.96 * se_dF, 4), "\u2013", round(dF + 1.96 * se_dF, 4), ")") else NULL
    
    tags$div(
      `data-bs-toggle`    = "tooltip",
      `data-bs-placement` = "bottom",
      title = paste0(
        "Effective Population Size (Ne): estimates how many animals are effectively ",
        "contributing to genetic diversity. ",
        "> 200 = adequate; 101\u2013200 = acceptable; 50\u2013100 = at risk; < 50 = critical. ",
        "The rate of inbreeding (\u0394F) is the average increase in inbreeding per generation; ",
        "values above 0.01 (Ne < 50) indicate critical diversity loss."
      ),
      style = "display:inline-block; cursor:help",
      h3(Ne, style = paste0("color:", color, "; margin-bottom:0;")),
      if (!is.null(ne_ci))
        tags$div(ne_ci, style = paste0("font-size:0.72rem; color:", color, "; margin-bottom:4px;")),
      tags$span(
        paste0(label, "\u24d8"),
        style = paste0("font-size:0.72rem; color:", color, ";")
      ),
      if (!is.null(dF))
        tags$div(
          paste0("\u0394F\u0305 = ", dF, if (!is.null(df_ci)) df_ci),
          style = "font-size:0.78rem; color:#555; margin-top:2px;"
        )
    )
  })
  
  # ── Advice & status ───────────────────────────────────────────────────────────
  output$advice <- renderUI({
    req(rd$Ne)
    Ne <- round(rd$Ne, 1)
    
    if (Ne > 200) {
      color       <- "#4caf50"
      intro       <- "No actions required \u2014 the population is in adequate genetic health."
      show_urgent <- FALSE
      show_light  <- TRUE
    } else if (Ne > 100) {
      color       <- "#7cb342"
      intro       <- "Population size is acceptable but below the adequate threshold. Keep the following in mind:"
      show_urgent <- FALSE
      show_light  <- TRUE
    } else if (Ne >= 50) {
      color       <- "#f57200"
      intro       <- "Genetic diversity may be at risk. Consider the following:"
      show_urgent <- TRUE
      show_light  <- FALSE
    } else {
      color       <- "#f44336"
      intro       <- "Genetic diversity is at risk. Consider the following:"
      show_urgent <- TRUE
      show_light  <- FALSE
    }
    
    make_rec_list <- function(style)
      tags$ul(style = paste0("line-height:1.8; padding-left:18px; color:", style, ";"),
              tags$li(strong("Balance the sex ratio"), " \u2014 limit the number of litters per sire to the same maximum as for dams."),
              tags$li(strong("Increase the number of breeding individuals"), " per generation."),
              tags$li(strong("Avoid inbreeding"), " \u2014 do not breed closely related individuals."),
              tags$li(strong("Introduce new individuals"), " or genetic material from other populations."),
              tags$li(strong("Select for diverse genetic backgrounds"), "."),
              tags$li(strong("Monitor genetic health"), " and adjust strategies as needed."))
    
    tagList(
      tags$p(style = paste0("color:", color, "; font-weight:bold; margin-bottom:8px;"), intro),
      if (show_urgent) make_rec_list(color),
      if (show_light)  tagList(
        tags$p(style = "color:#aaaaaa; font-size:0.85em; margin-bottom:4px;",
               "Keep in mind for the future:"),
        make_rec_list("#aaaaaa")
      ),
      tags$p(style = "margin-top:12px; font-style:italic; color:grey; font-size:0.9em;",
             "Following these strategies will help maintain the long-term genetic health of the population.")
    )
  })
  
  output$popstatus <- renderUI({
    req(rd$Ne)
    Ne <- round(rd$Ne, 1)
    
    if (Ne > 200) {
      svg   <- HTML('<svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg" width="80" height="80">
        <circle cx="50" cy="50" r="45" fill="#e8f5e9" stroke="#4caf50" stroke-width="4"/>
        <polyline points="25,52 42,68 75,35" fill="none" stroke="#4caf50" stroke-width="8"
                  stroke-linecap="round" stroke-linejoin="round"/>
      </svg>')
      color <- "#4caf50"; label <- "Ne adequate (> 200)"
    } else if (Ne > 100) {
      svg   <- HTML('<svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg" width="80" height="80">
        <circle cx="50" cy="50" r="45" fill="#f1f8e9" stroke="#7cb342" stroke-width="4"/>
        <polyline points="25,52 42,68 75,35" fill="none" stroke="#7cb342" stroke-width="8"
                  stroke-linecap="round" stroke-linejoin="round"/>
      </svg>')
      color <- "#7cb342"; label <- "Ne acceptable (101\u2013200)"
    } else if (Ne >= 50) {
      svg   <- HTML('<svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg" width="80" height="80">
        <circle cx="50" cy="50" r="45" fill="#fff8e1" stroke="#ff9800" stroke-width="4"/>
        <line x1="50" y1="25" x2="50" y2="58" stroke="#ff9800" stroke-width="8" stroke-linecap="round"/>
        <circle cx="50" cy="72" r="5" fill="#ff9800"/>
      </svg>')
      color <- "#f57200"; label <- "Ne low (< 100)"
    } else {
      svg   <- HTML('<svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg" width="80" height="80">
        <circle cx="50" cy="50" r="45" fill="#ffebee" stroke="#f44336" stroke-width="4"/>
        <line x1="35" y1="35" x2="65" y2="65" stroke="#f44336" stroke-width="8" stroke-linecap="round"/>
        <line x1="65" y1="35" x2="35" y2="65" stroke="#f44336" stroke-width="8" stroke-linecap="round"/>
      </svg>')
      color <- "#f44336"; label <- "Ne critical (< 50)"
    }
    
    tags$div(
      style = "display:flex; flex-direction:column; align-items:center;
               justify-content:center; text-align:center;",
      svg,
      tags$p(style = paste0("color:", color, "; font-weight:bold; margin-top:8px; font-size:0.95em;"), label)
    )
  })
  
  # ── Birth statistics plot & tables ────────────────────────────────────────────
  observe({
    req(rd$merged)
    stat_data <- rd$merged
    
    ov <- ungroup(summarize(
      group_by(stat_data, BirthYear, !!sym(rd$datdob), !!sym(rd$peddam), !!sym(rd$pedsire)),
      Littersize    = n(),
      Female        = sum(!!sym(rd$datsex) == rd$datF),
      Male          = sum(!!sym(rd$datsex) == rd$datM),
      FemaleBreeder = sum(!!sym(rd$datsex) == rd$datF & !!sym(rd$datid) %in% rd$breeders[[rd$datid]]),
      MaleBreeder   = sum(!!sym(rd$datsex) == rd$datM & !!sym(rd$datid) %in% rd$breeders[[rd$datid]])
    ))
    
    ov1 <- summarize(group_by(ov, BirthYear),
                     Litters        = n(),
                     MeanLitterSize = round(mean(Littersize, na.rm = TRUE), 1),
                     Animals        = sum(Littersize),
                     Females        = sum(Female),
                     `%Females`     = paste(round(sum(Female) / Animals * 100, 0), "%"),
                     Males          = sum(Male),
                     `% Male`       = paste(round(sum(Male)   / Animals * 100, 0), "%"),
                     FemaleBreeders = sum(FemaleBreeder),
                     MaleBreeders   = sum(MaleBreeder))
    
    output$birthplot <- renderPlotly({
      req(input$metric)
      bar_colors <- if (!is.null(selected_year()))
        ifelse(ov1$BirthYear == selected_year(), "lightblue", "steelblue")
      else "steelblue"
      p <- plot_ly(ov1, x = ~as.numeric(BirthYear), source = "birthplot") %>%
        add_bars(y = ov1[[input$metric]],
                 marker    = list(color = bar_colors),
                 hoverinfo = "text",
                 text      = paste0(input$metric, ": ", ov1[[input$metric]],
                                    "<br>Year: ", ov1$BirthYear)) %>%
        layout(title  = paste("Yearly", input$metric),
               xaxis  = list(title = "Birth Year", tickmode = "linear", dtick = 1),
               yaxis  = list(title = input$metric),
               showlegend = FALSE)
      event_register(p, "plotly_click")
      event_register(p, "plotly_doubleclick")
    })
    
    output$conditional_table_ui <- renderUI({
      if (input$metric == "MeanLitterSize" || is.null(selected_year())) return(NULL)
      tagList(card(card_header("Table From Plot Selection"),
                   card_body(dataTableOutput(ns("conditional_table")))))
    })
    
    output$overview_table <- renderDataTable({
      datatable(as.data.frame(ov1), style = "bootstrap", rownames = FALSE,
                options = list(lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                               pageLength = -1,
                               columnDefs = list(list(className = "dt-center", targets = 0:6))))
    })
    
    output$conditional_table <- renderDataTable({
      if (input$metric == "MeanLitterSize" || is.null(selected_year())) return(NULL)
      data <- switch(input$metric,
                     Litters        = ov[ov$BirthYear == selected_year(),
                                         colnames(ov)[colnames(ov) != "BirthYear"]],
                     Animals        = stat_data[stat_data$BirthYear == selected_year(),
                                                colnames(stat_data)[colnames(stat_data) != "BirthYear"]],
                     Females        = stat_data[stat_data$BirthYear == selected_year() &
                                                  stat_data[[rd$datsex]] == rd$datF,
                                                colnames(stat_data)[colnames(stat_data) != "BirthYear"]],
                     Males          = stat_data[stat_data$BirthYear == selected_year() &
                                                  stat_data[[rd$datsex]] == rd$datM,
                                                colnames(stat_data)[colnames(stat_data) != "BirthYear"]],
                     FemaleBreeders = stat_data[stat_data$BirthYear == selected_year() &
                                                  stat_data[[rd$datsex]] == rd$datF &
                                                  stat_data[[rd$datid]] %in% rd$breeders[[rd$datid]],
                                                colnames(stat_data)[colnames(stat_data) != "BirthYear"]],
                     MaleBreeders   = stat_data[stat_data$BirthYear == selected_year() &
                                                  stat_data[[rd$datsex]] == rd$datM &
                                                  stat_data[[rd$datid]] %in% rd$breeders[[rd$datid]],
                                                colnames(stat_data)[colnames(stat_data) != "BirthYear"]],
                     NULL
      )
      if (is.null(data)) return(NULL)
      datatable(as.data.frame(data), style = "bootstrap", rownames = FALSE,
                options = list(lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                               pageLength = -1,
                               columnDefs = list(list(className = "dt-center", targets = 0:6))))
    })
  })
  
  observeEvent(event_data("plotly_click", source = "birthplot"), {
    req(rd$merged)
    click   <- event_data("plotly_click", source = "birthplot")
    clicked <- round(click$x)
    if (clicked %in% unique(rd$merged$BirthYear))
      selected_year(if (!is.null(selected_year()) && selected_year() == clicked) NULL else clicked)
    else
      selected_year(NULL)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(event_data("plotly_doubleclick", source = "birthplot"), {
    selected_year(NULL)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
}
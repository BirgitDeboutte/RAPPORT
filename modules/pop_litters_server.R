# ── pop_litters_server.R ──────────────────────────────────────────────────────
# Panel helper for the "Litters" tab.
# Called from popServer(); shares input/output/session namespace.
#
# Reactive args: litter_ov, selected_year
# Defines outputs: vb_lit_total, vb_lit_size, vb_lit_sexratio, vb_lit_thisyear,
#                  litterplot, litters, selectlitter
# Note: vb_lit_size filters to own-population litters (Dam or Sire in ownpop).
#       vb_litsize in popOverviewHelper uses all litters — intentionally different.

popLittersHelper <- function(
    input, output, session, rd,
    litter_ov,
    selected_year
) {
  
  # ── Value boxes ───────────────────────────────────────────────────────────────
  output$vb_lit_total <- renderUI({
    req(litter_ov())
    h3(nrow(litter_ov()))
  })
  
  output$vb_lit_size <- renderUI({
    req(litter_ov(), rd$ownpop)
    own_ids     <- rd$ownpop[[rd$datid]]
    ov          <- litter_ov()
    own_litters <- ov[ov$Dam %in% own_ids | ov$Sire %in% own_ids, ]
    sizes       <- own_litters$Littersize
    mean_raw    <- mean(sizes, na.rm = TRUE)
    se          <- sd(sizes,   na.rm = TRUE) / sqrt(sum(!is.na(sizes)))
    h3(paste0(round(mean_raw, 1),
              " [", round(mean_raw - 1.96 * se, 1),
              "\u2013", round(mean_raw + 1.96 * se, 1), "]"))
  })
  
  output$vb_lit_sexratio <- renderUI({
    req(litter_ov())
    ov    <- litter_ov()
    nf    <- sum(ov$`# Females`)
    nm    <- sum(ov$`# Males`)
    total <- nf + nm
    tags$span(
      if (total > 0) paste0(round(nf / total * 100), "% F  /  ", round(nm / total * 100), "% M")
      else "N/A",
      style = "font-size:1rem; font-weight:600;"
    )
  })
  
  output$vb_lit_thisyear <- renderUI({
    req(litter_ov())
    n <- sum(litter_ov()$BirthYear == lubridate::year(Sys.Date()))
    h3(n)
  })
  
  # ── Litters plot & table ──────────────────────────────────────────────────────
  observe({
    req(litter_ov())
    ov  <- litter_ov()
    ov1 <- summarize(group_by(ov, BirthYear),
                     Litters        = n(),
                     MeanLitterSize = round(mean(Littersize, na.rm = TRUE), 1))
    
    output$litterplot <- renderPlotly({
      bar_colors <- if (!is.null(selected_year()))
        ifelse(ov1$BirthYear == selected_year(), "lightblue", "steelblue")
      else "steelblue"
      p <- plot_ly(ov1, x = ~as.numeric(BirthYear), source = "litterplot") %>%
        add_bars(y = ov1$Litters,
                 marker    = list(color = bar_colors),
                 hoverinfo = "text",
                 text      = paste0("Litters: ", ov1$Litters, "<br>Year: ", ov1$BirthYear)) %>%
        layout(xaxis = list(title = "Birth Year", tickmode = "linear", dtick = 1),
               yaxis = list(title = "Litters"),
               showlegend = FALSE)
      event_register(p, "plotly_click")
      event_register(p, "plotly_doubleclick")
    })
    
    output$litters <- renderDataTable({
      disp <- if (is.null(selected_year())) ov else ov[ov$BirthYear == selected_year(), ]
      datatable(disp[, colnames(disp)[!colnames(disp) %in% c("dob", "BirthYear")]],
                style = "bootstrap", rownames = FALSE, selection = "single",
                options = list(lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                               pageLength = -1))
    })
  })
  
  observeEvent(event_data("plotly_click", source = "litterplot"), {
    req(rd$merged)
    click   <- event_data("plotly_click", source = "litterplot")
    clicked <- round(click$x)
    if (clicked %in% unique(rd$merged$BirthYear))
      selected_year(if (!is.null(selected_year()) && selected_year() == clicked) NULL else clicked)
    else
      selected_year(NULL)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(event_data("plotly_doubleclick", source = "litterplot"), {
    selected_year(NULL)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$litters_rows_selected, {
    req(rd$merged, litter_ov())
    ov   <- litter_ov()
    disp <- if (is.null(selected_year())) ov else ov[ov$BirthYear == selected_year(), ]
    selected_litter <- disp[input$litters_rows_selected, rd$datdob, drop = TRUE]
    output$selectlitter <- renderDataTable({
      datatable(rd$merged[rd$merged[[rd$datdob]] %in% selected_litter, ],
                style = "bootstrap", rownames = FALSE, selection = "single",
                options = list(lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                               pageLength = -1))
    })
  })
}
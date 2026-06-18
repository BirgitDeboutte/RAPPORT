# ── pop_sires_server.R ────────────────────────────────────────────────────────
# Panel helper for the "Sires" tab.
# Called from popServer(); shares input/output/session namespace.
#
# Reactive args: litter_ov, sire_table, selected_year
# Inputs consumed: own_sires, sires_rows_selected, selectsire_rows_selected
# Defines outputs: vb_sire_total, vb_sire_own, vb_sire_ext, vb_sire_lit,
#                  vb_sire_prog, sireplot, sires, selectsire, selectsirelitter

popSiresHelper <- function(
    input, output, session, rd,
    litter_ov,
    sire_table,
    selected_year
) {
  
  S_filtered <- reactive({
    req(sire_table())
    if (isTRUE(input$own_sires)) sire_table()[sire_table()$Sire %in% rd$ownpop[[rd$datid]], ]
    else sire_table()
  })
  
  # ── Value boxes ───────────────────────────────────────────────────────────────
  output$vb_sire_total <- renderUI({
    req(sire_table()); h3(nrow(sire_table()))
  })
  
  output$vb_sire_own <- renderUI({
    req(sire_table(), rd$ownpop)
    own_ids <- rd$ownpop[[rd$datid]]
    n       <- sum(sire_table()$Sire %in% own_ids)
    tagList(
      h3(n),
      tags$span(paste0(round(100 * n / nrow(sire_table()), 1), "% of sires"),
                style = "font-size:0.72rem;")
    )
  })
  
  output$vb_sire_ext <- renderUI({
    req(sire_table(), rd$ownpop)
    own_ids <- rd$ownpop[[rd$datid]]
    n       <- sum(!sire_table()$Sire %in% own_ids)
    tagList(
      h3(n),
      tags$span(paste0(round(100 * n / nrow(sire_table()), 1), "% of sires"),
                style = "font-size:0.72rem;")
    )
  })
  
  output$vb_sire_lit <- renderUI({
    req(sire_table()); h3(round(mean(sire_table()$`# Litters`, na.rm = TRUE), 1))
  })
  
  output$vb_sire_prog <- renderUI({
    req(sire_table()); h3(round(mean(sire_table()$`# Progeny`, na.rm = TRUE), 1))
  })
  
  # ── Sire plot & table ─────────────────────────────────────────────────────────
  observe({
    req(S_filtered())
    S   <- S_filtered()
    ovs <- summarize(group_by(S, BirthYear), Sires = n())
    
    output$sireplot <- renderPlotly({
      bar_colors <- if (!is.null(selected_year()))
        ifelse(ovs$BirthYear == selected_year(), "lightblue", "steelblue")
      else "steelblue"
      p <- plot_ly(ovs, x = ~as.numeric(BirthYear), source = "sireplot") %>%
        add_bars(y = ovs$Sires,
                 marker    = list(color = bar_colors),
                 hoverinfo = "text",
                 text      = paste0("Sires: ", ovs$Sires, "<br>Year: ", ovs$BirthYear)) %>%
        layout(xaxis = list(title = "Birth Year", tickmode = "linear", dtick = 1),
               yaxis = list(title = "Sires"),
               showlegend = FALSE)
      event_register(p, "plotly_click")
      event_register(p, "plotly_doubleclick")
    })
    
    output$sires <- renderDataTable({
      S    <- S_filtered()
      disp <- if (is.null(selected_year())) S else S[S$BirthYear == selected_year(), ]
      datatable(disp[, colnames(disp)[colnames(disp) != "BirthYear"]],
                rownames = FALSE, style = "bootstrap",
                options = list(lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                               pageLength = -1))
    })
  })
  
  observeEvent(event_data("plotly_click", source = "sireplot"), {
    req(rd$data)
    click   <- event_data("plotly_click", source = "sireplot")
    clicked <- round(click$x)
    if (clicked %in% unique(rd$data$BirthYear))
      selected_year(if (!is.null(selected_year()) && selected_year() == clicked) NULL else clicked)
    else
      selected_year(NULL)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(event_data("plotly_doubleclick", source = "sireplot"), {
    selected_year(NULL)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  output$selectsire <- renderDataTable({
    req(litter_ov(), S_filtered(), input$sires_rows_selected)
    S    <- S_filtered()
    disp <- if (is.null(selected_year())) S else S[S$BirthYear == selected_year(), ]
    selected_sire <- disp[input$sires_rows_selected, "Sire", drop = TRUE]
    filtered_data <- litter_ov()[litter_ov()$Sire %in% selected_sire,
                                 colnames(litter_ov())[!colnames(litter_ov()) %in%
                                                         c("BirthYear", "dob")]]
    datatable(filtered_data, style = "bootstrap", rownames = FALSE,
              options = list(lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                             pageLength = -1))
  })
  
  observeEvent(input$selectsire_rows_selected, {
    req(rd$merged, litter_ov(), S_filtered(), input$sires_rows_selected)
    S    <- S_filtered()
    disp <- if (is.null(selected_year())) S else S[S$BirthYear == selected_year(), ]
    selected_sire <- disp[input$sires_rows_selected, "Sire", drop = TRUE]
    filtered_data <- litter_ov()[litter_ov()$Sire %in% selected_sire,
                                 colnames(litter_ov())[!colnames(litter_ov()) %in%
                                                         c("BirthYear", "dob")]]
    selected_litter <- filtered_data[input$selectsire_rows_selected, rd$datdob, drop = TRUE]
    output$selectsirelitter <- renderDataTable({
      datatable(rd$merged[rd$merged[[rd$datdob]] %in% selected_litter, ],
                style = "bootstrap", rownames = FALSE, selection = "single",
                options = list(lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                               pageLength = -1))
    })
  })
}
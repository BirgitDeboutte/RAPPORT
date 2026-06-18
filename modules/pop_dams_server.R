# ── pop_dams_server.R ─────────────────────────────────────────────────────────
# Panel helper for the "Dams" tab.
# Called from popServer(); shares input/output/session namespace.
#
# Reactive args: litter_ov, dam_table, selected_year
# Inputs consumed: own_dams, dams_rows_selected, selectdam_rows_selected
# Defines outputs: vb_dam_total, vb_dam_own, vb_dam_ext, vb_dam_lit, vb_dam_prog,
#                  damplot, dams, selectdam, selectdamlitter

popDamsHelper <- function(
    input, output, session, rd,
    litter_ov,
    dam_table,
    selected_year
) {
  
  D_filtered <- reactive({
    req(dam_table())
    if (isTRUE(input$own_dams)) dam_table()[dam_table()$Dam %in% rd$ownpop[[rd$datid]], ]
    else dam_table()
  })
  
  # ── Value boxes ───────────────────────────────────────────────────────────────
  output$vb_dam_total <- renderUI({
    req(dam_table()); h3(nrow(dam_table()))
  })
  
  output$vb_dam_own <- renderUI({
    req(dam_table(), rd$ownpop)
    own_ids <- rd$ownpop[[rd$datid]]
    n       <- sum(dam_table()$Dam %in% own_ids)
    tagList(
      h3(n),
      tags$span(paste0(round(100 * n / nrow(dam_table()), 1), "% of dams"),
                style = "font-size:0.72rem;")
    )
  })
  
  output$vb_dam_ext <- renderUI({
    req(dam_table(), rd$ownpop)
    own_ids <- rd$ownpop[[rd$datid]]
    n       <- sum(!dam_table()$Dam %in% own_ids)
    tagList(
      h3(n),
      tags$span(paste0(round(100 * n / nrow(dam_table()), 1), "% of dams"),
                style = "font-size:0.72rem;")
    )
  })
  
  output$vb_dam_lit <- renderUI({
    req(dam_table()); h3(round(mean(dam_table()$`# Litters`, na.rm = TRUE), 1))
  })
  
  output$vb_dam_prog <- renderUI({
    req(dam_table()); h3(round(mean(dam_table()$`# Progeny`, na.rm = TRUE), 1))
  })
  
  # ── Dam plot & table ──────────────────────────────────────────────────────────
  observe({
    req(D_filtered())
    D   <- D_filtered()
    ovd <- summarize(group_by(D, BirthYear), Dams = n())
    
    output$damplot <- renderPlotly({
      bar_colors <- if (!is.null(selected_year()))
        ifelse(ovd$BirthYear == selected_year(), "lightblue", "steelblue")
      else "steelblue"
      p <- plot_ly(ovd, x = ~as.numeric(BirthYear), source = "damplot") %>%
        add_bars(y = ovd$Dams,
                 marker    = list(color = bar_colors),
                 hoverinfo = "text",
                 text      = paste0("Dams: ", ovd$Dams, "<br>Year: ", ovd$BirthYear)) %>%
        layout(xaxis = list(title = "Birth Year", tickmode = "linear", dtick = 1),
               yaxis = list(title = "Dams"),
               showlegend = FALSE)
      event_register(p, "plotly_click")
      event_register(p, "plotly_doubleclick")
    })
    
    output$dams <- renderDataTable({
      D    <- D_filtered()
      disp <- if (is.null(selected_year())) D else D[D$BirthYear == selected_year(), ]
      datatable(disp[, colnames(disp)[!colnames(disp) %in% c("dobmom", "BirthYear")]],
                rownames = FALSE, style = "bootstrap",
                options = list(lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                               pageLength = -1))
    })
  })
  
  observeEvent(event_data("plotly_click", source = "damplot"), {
    req(rd$data)
    click   <- event_data("plotly_click", source = "damplot")
    clicked <- round(click$x)
    if (clicked %in% unique(rd$data$BirthYear))
      selected_year(if (!is.null(selected_year()) && selected_year() == clicked) NULL else clicked)
    else
      selected_year(NULL)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(event_data("plotly_doubleclick", source = "damplot"), {
    selected_year(NULL)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  output$selectdam <- renderDataTable({
    req(litter_ov(), D_filtered(), input$dams_rows_selected)
    D    <- D_filtered()
    disp <- if (is.null(selected_year())) D else D[D$BirthYear == selected_year(), ]
    selected_dam  <- disp[input$dams_rows_selected, "Dam", drop = TRUE]
    filtered_data <- litter_ov()[litter_ov()$Dam %in% selected_dam,
                                 colnames(litter_ov())[!colnames(litter_ov()) %in%
                                                         c("BirthYear", "dob")]]
    datatable(filtered_data, style = "bootstrap", rownames = FALSE,
              options = list(lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                             pageLength = -1))
  })
  
  observeEvent(input$selectdam_rows_selected, {
    req(rd$merged, litter_ov(), D_filtered(), input$dams_rows_selected)
    D    <- D_filtered()
    disp <- if (is.null(selected_year())) D else D[D$BirthYear == selected_year(), ]
    selected_dam  <- disp[input$dams_rows_selected, "Dam", drop = TRUE]
    filtered_data <- litter_ov()[litter_ov()$Dam %in% selected_dam,
                                 colnames(litter_ov())[!colnames(litter_ov()) %in%
                                                         c("BirthYear", "dob")]]
    selected_litter <- filtered_data[input$selectdam_rows_selected, rd$datdob, drop = TRUE]
    output$selectdamlitter <- renderDataTable({
      datatable(rd$merged[rd$merged[[rd$datdob]] %in% selected_litter, ],
                style = "bootstrap", rownames = FALSE, selection = "single",
                options = list(lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                               pageLength = -1))
    })
  })
}
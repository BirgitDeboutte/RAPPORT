# ── management_pup_planner_server.R ──────────────────────────────────────────
# Panel helper for the "Progeny Planner" tab.

managementPupPlannerHelper <- function(
    input, output, session, rd,
    litter_stats,
    retirement_dates,
    pup_projection_data,
    capacity_vals,
    breeders_col_specified
) {
  ns <- session$ns
  
  # ── Warnings ──────────────────────────────────────────────────────────────────
  
  output$no_breeders_warning_pup <- renderUI({
    if (breeders_col_specified()) return(NULL)
    tags$div(
      class = "alert alert-warning",
      style = "font-size:0.9rem; padding:10px 14px; margin-bottom:12px",
      tags$b("\u26a0 No breeding stock column specified. "),
      "The Progeny Planner requires a designated breeding stock column in the data file. ",
      "Please specify this in the Data Input tab."
    )
  })
  
  output$retire_col_warning_pup <- renderUI({  
    req(rd$breeders)
    col <- input$retire_col
    if (is.null(col) || col == "None") {
      tags$div(
        class = "alert alert-warning",
        style = "font-size:0.85rem; padding:8px 12px; margin-bottom:8px",
        tags$b("\u26a0 No retirement date column selected in Population Growth. "),
        "Active dam counts may include retired animals."
      )
    }
  })
  
  # ── Value boxes ───────────────────────────────────────────────────────────────
  
  output$vb_pups_last <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(pup_projection_data())
    n <- pup_projection_data() %>%
      filter(year == lubridate::year(Sys.Date()) - 1) %>% pull(actual_pups)
    h3(if (length(n) == 0 || is.na(n[1])) "\u2014" else n[1])
  })
  
  output$vb_pups_expected_last <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(pup_projection_data())
    n <- pup_projection_data() %>%
      filter(year == lubridate::year(Sys.Date()) - 1) %>% pull(expected_pups)
    h3(if (length(n) == 0 || is.na(n[1])) "\u2014" else n[1])
  })
  
  output$vb_coverage <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(pup_projection_data())
    row <- pup_projection_data() %>%
      filter(year == lubridate::year(Sys.Date()) - 1)
    pct <- if (nrow(row) == 0 || is.na(row$expected_pups) || row$expected_pups == 0)
      "\u2014"
    else
      paste0(round(100 * row$actual_pups / row$expected_pups), "%")
    color <- if (pct != "\u2014") {
      val <- as.numeric(sub("%", "", pct))
      if (val >= 90) "color:darkgreen" else if (val >= 70) "color:orange" else "color:red"
    } else "color:grey"
    h3(pct, style = color)
  })
  
  output$vb_pups_projected <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    cv <- capacity_vals()
    req(cv)
    if (is.na(cv$capacity)) return(h3("\u2014", style = "color:grey"))
    h3(cv$capacity)
  })
  
  # ── Progeny projection chart ──────────────────────────────────────────────────────
  
  selected_year_pup <- reactiveVal(NULL)
  
  output$pup_projection <- renderPlotly({
    if (!breeders_col_specified()) return(plotly_empty())
    req(pup_projection_data())
    cv        <- capacity_vals()
    pup_data  <- pup_projection_data()
    this_year <- lubridate::year(Sys.Date())
    
    # Align current-year expected bar with the value box — both use capacity_vals.
    # pup_projection_data counts active dams via retirement_dates();
    # capacity_vals counts via dam_schedule() — different methods, different numbers.
    if (!is.null(cv) && !is.na(cv$capacity)) {
      if (this_year %in% pup_data$year) {
        pup_data$expected_pups[pup_data$year == this_year] <- cv$capacity
      } else {
        pup_data <- bind_rows(pup_data, data.frame(
          year          = this_year,
          expected_pups = cv$capacity,
          actual_pups   = 0L,
          expected_moms = NA_integer_,
          actual_moms   = 0L
        ))
      }
    }
    pup_data <- arrange(pup_data, year)
    
    p <- plot_ly(pup_data, x = ~year, source = "pup_projection") %>%
      add_bars(y = ~expected_pups, name = "Expected Progeny",
               marker = list(color = "lightgray"), hoverinfo = "text",
               text = ~paste0("Expected progeny: ", expected_pups,
                              "<br>Expected dams: ", expected_moms)) %>%
      add_bars(y = ~actual_pups, name = "Actual Progeny",
               marker = list(color = "steelblue"), hoverinfo = "text",
               text = ~paste0("Actual progeny: ", actual_pups,
                              "<br>Dams with litters: ", actual_moms)) %>%
      layout(barmode  = "overlay",
             title    = "Expected vs Actual Progeny Count per Year",
             xaxis    = list(title = "Year", type = "category"),
             yaxis    = list(title = "Number of Progeny"),
             showlegend = TRUE,
             margin     = list(t = 50, b = 50),
             hovermode  = "x unified")
    event_register(p, "plotly_click")
    p
  })
  
  observeEvent(event_data("plotly_click", source = "pup_projection"), {
    req(rd$litterplan)
    click_data   <- event_data("plotly_click", source = "pup_projection")
    clicked_year <- suppressWarnings(as.integer(round(as.numeric(click_data$x[1]))))
    valid_years  <- sort(unique(lubridate::year(rd$litterplan$dob)))
    selected_year_pup(
      if (!is.na(clicked_year) && clicked_year %in% valid_years) clicked_year else NULL
    )
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # ── Yearly dam table ──────────────────────────────────────────────────────────
  
  output$yearly_dam_table_header <- renderUI({
    req(selected_year_pup())
    tagList(
      p(""),
      layout_columns(
        h4(paste("Dam Overview \u2014", selected_year_pup())),
        actionButton(ns("clear_year"), "\u00d7 Clear", class = "btn btn-sm btn-outline-secondary"),
        col_widths = c(10, 2)
      ),
      p("")
    )
  })
  
  observeEvent(input$clear_year, { selected_year_pup(NULL) })
  
  output$yearly_dam_table <- DT::renderDataTable({
    req(selected_year_pup(), rd$litterplan, litter_stats(), retirement_dates())
    year <- selected_year_pup()
    ls   <- litter_stats()
    
    breeders_female <- retirement_dates() %>%
      mutate(start_breeding_date = dobmom + lubridate::dyears(ls$mean_age_first)) %>%
      filter(start_breeding_date <= as.Date(paste0(year, "-12-31")),
             is.na(retirement_date) | retirement_date > as.Date(paste0(year - 1, "-12-31")))
    
    dams_litters <- rd$litterplan %>%
      filter(lubridate::year(dob) == year) %>%
      select(Dam, LitterDate = dob, Littersize)
    
    dam_litter_counts <- rd$litterplan %>%
      filter(lubridate::year(dob) <= year) %>%
      group_by(Dam) %>% summarise(litter_count = n(), .groups = "drop")
    
    breeders_female %>%
      left_join(dam_litter_counts, by = "Dam") %>%
      mutate(litter_count    = replace_na(litter_count, 0),
             HadLitter       = ifelse(Dam %in% dams_litters$Dam, "Yes", "No"),
             retirement_date = format(retirement_date, "%Y-%m-%d")) %>%
      left_join(dams_litters, by = "Dam") %>%
      arrange(desc(HadLitter), Dam) %>%
      select(Dam, dobmom, litter_count, HadLitter, LitterDate, Littersize, retirement_date) %>%
      datatable(rownames = FALSE, style = "bootstrap", selection = "single",
                colnames = c("Dam", "Date of Birth", "Litters So Far", "Had Litter?",
                             "Litter Date", "Litter Size", "Retirement Date"),
                options = list(lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                               pageLength = -1))
  })
}
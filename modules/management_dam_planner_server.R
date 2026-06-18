# ── management_dam_planner_server.R ──────────────────────────────────────────
# Panel helper for the "Dam Planner" tab.
# Called from managementServer(); shares input/output/session namespace.
#
# Inputs consumed: litdam, dam_filter, gantt_year_range,
#                  show_active, show_retiring, show_upcoming, show_retired
# Defines outputs: no_breeders_warning_dam, vb_age_first, vb_interval,
#                  vb_per_year, vb_litter_size, vb_genint,
#                  dam_status_*, dam_detail_section, dam_detail_table,
#                  gantt_chart

managementDamPlannerHelper <- function(input, output, session, rd,
                                       litter_stats,
                                       dam_schedule,
                                       breeders_col_specified,
                                       active_in_year) {
  ns <- session$ns
  
  # ── Warnings ──────────────────────────────────────────────────────────────────
  output$no_breeders_warning_dam <- renderUI({
    if (breeders_col_specified()) return(NULL)
    tags$div(
      class = "alert alert-warning",
      style = "font-size:0.9rem; padding:10px 14px; margin-bottom:12px",
      tags$b("\u26a0 No breeding stock column specified. "),
      "The Dam Planner requires a designated breeding stock column in the data file. ",
      "Please specify this in the Data Input tab."
    )
  })
  
  output$retire_col_warning_dam <- renderUI({ 
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
  
  # ── Summary value boxes ──────────────────────────────────────────────────────
  output$vb_age_first <- renderUI({
    req(litter_stats())
    h3(paste(round(litter_stats()$mean_age_first, 1), "yrs"))
  })
  output$vb_interval <- renderUI({
    req(litter_stats())
    h3(paste(round(litter_stats()$mean_litter_interval, 1), "yrs"))
  })
  output$vb_per_year <- renderUI({
    req(litter_stats())
    h3(round(litter_stats()$mean_litter_per_year, 1))
  })
  output$vb_litter_size <- renderUI({
    req(litter_stats())
    ls <- litter_stats()
    h3(paste0(round(ls$mean_litter_size, 1),
              " [", round(ls$ci_lower, 1), "\u2013", round(ls$ci_upper, 1), "]"))
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
  
  # ── Dam Activity Summary boxes ───────────────────────────────────────────────
  selected_dam_group <- reactiveVal(NULL)
  
  output$dam_status_active <- renderUI({
    if (!breeders_col_specified()) return(NULL)
    req(dam_schedule())
    n <- nrow(active_in_year(dam_schedule(), lubridate::year(Sys.Date())))
    tagList(
      tags$h4(n, style = "color:steelblue"),
      tags$p("Active or Eligible This Year"),
      actionButton(ns("show_active"), "View Dams",
                   class = "btn btn-sm btn-outline-primary mt-1")
    )
  })
  
  output$dam_status_retiring <- renderUI({
    if (!breeders_col_specified()) return(NULL)
    req(dam_schedule(), input$litdam)
    n     <- dam_schedule() %>% filter(n_litters == input$litdam - 1) %>% nrow()
    color <- if (n > 0) "orange" else "steelblue"
    tagList(
      tags$h4(n, style = paste0("color:", color)),
      tags$p("Due for Last Litter"),
      actionButton(ns("show_retiring"), "View Dams",
                   class = "btn btn-sm btn-outline-warning mt-1")
    )
  })
  
  output$dam_status_upcoming <- renderUI({
    if (!breeders_col_specified()) return(NULL)
    req(dam_schedule())
    n <- dam_schedule() %>% filter(n_litters == 0, breed_start > Sys.Date()) %>% nrow()
    tagList(
      tags$h4(n, style = "color:darkgreen"),
      tags$p("Not Yet Started"),
      actionButton(ns("show_upcoming"), "View Dams",
                   class = "btn btn-sm btn-outline-success mt-1")
    )
  })
  
  output$dam_status_retired <- renderUI({
    if (!breeders_col_specified()) return(NULL)
    req(dam_schedule())
    n <- dam_schedule() %>% filter(breed_end < Sys.Date()) %>% nrow()
    tagList(
      tags$h4(n, style = "color:grey"),
      tags$p("Retired"),
      actionButton(ns("show_retired"), "View Dams",
                   class = "btn btn-sm btn-outline-secondary mt-1")
    )
  })
  
  observeEvent(input$show_active,   { selected_dam_group("active")   })
  observeEvent(input$show_retiring, { selected_dam_group("retiring") })
  observeEvent(input$show_upcoming, { selected_dam_group("upcoming") })
  observeEvent(input$show_retired,  { selected_dam_group("retired")  })
  
  output$dam_detail_section <- renderUI({
    req(selected_dam_group())
    title <- switch(selected_dam_group(),
                    "active"   = "Active or Eligible This Year",
                    "retiring" = "Due for Last Litter",
                    "upcoming" = "Not Yet Started",
                    "retired"  = "Retired")
    tagList(hr(), h5(title), DT::dataTableOutput(ns("dam_detail_table")))
  })
  
  output$dam_detail_table <- DT::renderDataTable({
    req(selected_dam_group(), dam_schedule())
    today     <- Sys.Date()
    this_year <- lubridate::year(today)
    base      <- dam_schedule()
    
    tab <- switch(selected_dam_group(),
                  "active"   = active_in_year(base, this_year) %>%
                    select(Dam, dobmom, n_litters, last_litter, expected_last_litter),
                  "retiring" = base %>% filter(n_litters == input$litdam - 1) %>%
                    select(Dam, dobmom, n_litters, last_litter, expected_last_litter),
                  "upcoming" = base %>% filter(n_litters == 0, breed_start > today) %>%
                    select(Dam, dobmom, expected_start = breed_start, expected_last_litter),
                  "retired"  = base %>% filter(breed_end < today) %>%
                    select(Dam, dobmom, n_litters, last_litter, breed_end)
    )
    
    col_names <- switch(selected_dam_group(),
                        "upcoming" = c("Dam", "Date of Birth", "Expected First Litter", "Expected Last Litter"),
                        "retired"  = c("Dam", "Date of Birth", "Total Litters", "Last Litter Date", "Retired Since"),
                        c("Dam", "Date of Birth", "Litters So Far", "Last Litter Date", "Expected Last Litter")
    )
    
    tab %>%
      mutate(across(where(lubridate::is.Date), ~ format(.x, "%Y-%m-%d"))) %>%
      arrange(Dam) %>%
      datatable(rownames = FALSE, style = "bootstrap", colnames = col_names,
                options = list(pageLength = 10, dom = "tip"))
  })
  
  # ── Gantt chart ──────────────────────────────────────────────────────────────
  observe({
    req(dam_schedule(), rd$litterplan, litter_stats())
    today <- Sys.Date()
    sched <- dam_schedule()
    
    active_mothers <- rd$litterplan %>%
      group_by(Dam, dobmom) %>%
      summarise(start_date = min(dob) %m-% months(2),
                end_date   = max(dob) %m+% months(2),
                .groups    = "drop") %>%
      left_join(
        sched[, c("Dam", "retirement_date", "expected_last_litter",
                  "breed_start", "breed_end", "n_litters")],
        by = "Dam"
      ) %>%
      mutate(
        hypothetical_start = breed_start - months(2),
        hypothetical_end   = breed_end   + months(2),
        end_date = if_else(!is.na(retirement_date) & end_date > retirement_date,
                           retirement_date, end_date)
      )
    
    new_mothers <- sched %>%
      filter(!Dam %in% active_mothers$Dam) %>%
      select(Dam, dobmom, retirement_date, expected_last_litter,
             breed_start, breed_end, n_litters) %>%
      mutate(hypothetical_start = breed_start - months(2),
             hypothetical_end   = breed_end   + months(2),
             start_date = as.Date(NA),
             end_date   = as.Date(NA))
    
    all_mothers <- rbind(as.data.frame(active_mothers), as.data.frame(new_mothers)) %>%
      arrange(desc(dobmom))
    all_mothers$Dam <- factor(all_mothers$Dam, levels = unique(all_mothers$Dam))
    
    output$gantt_chart <- renderPlotly({
      if (!breeders_col_specified()) return(plotly_empty())
      
      years <- seq(
        as.Date(floor_date(min(all_mothers$hypothetical_start, na.rm = TRUE), "year")),
        as.Date(ceiling_date(max(all_mothers$hypothetical_end,  na.rm = TRUE), "year")),
        by = "1 year"
      )
      midpoints   <- years + months(6)
      annotations <- lapply(seq_along(midpoints), function(i) {
        list(x = midpoints[i], y = 1, xref = "x", yref = "paper",
             text = format(years[i], "%Y"), showarrow = FALSE,
             yanchor = "bottom", xanchor = "center", font = list(size = 10))
      })
      
      plot_mothers <- all_mothers %>%
        filter(
          hypothetical_end   >= as.Date(paste0(input$gantt_year_range[1], "-01-01")),
          hypothetical_start <= as.Date(paste0(input$gantt_year_range[2], "-12-31"))
        )
      
      if (!"Retired" %in% input$dam_filter)
        plot_mothers <- plot_mothers %>%
        filter(is.na(retirement_date) | retirement_date >= today)
      if (!"Upcoming" %in% input$dam_filter)
        plot_mothers <- plot_mothers %>% filter(!is.na(start_date))
      if (!"Active or Eligible This Year" %in% input$dam_filter) {
        this_year    <- lubridate::year(today)
        plot_mothers <- plot_mothers %>%
          filter(!(hypothetical_start <= as.Date(paste0(this_year, "-12-31")) &
                     hypothetical_end >= as.Date(paste0(this_year, "-01-01"))))
      }
      
      plot_ly(height = 800) %>%
        add_segments(
          data = plot_mothers,
          x = ~hypothetical_start, xend = ~hypothetical_end,
          y = ~Dam, yend = ~Dam,
          line = list(color = "lightgrey", width = 10),
          hoverinfo = "text",
          text = ~paste("Hypothetical Period", Dam, ": From",
                        format(hypothetical_start, "%Y-%m-%d"),
                        "<br>To", format(hypothetical_end, "%Y-%m-%d")),
          name = "Hypothetical Active Period"
        ) %>%
        add_segments(
          data = plot_mothers[!is.na(plot_mothers$start_date), ],
          x = ~start_date, xend = ~end_date,
          y = ~Dam, yend = ~Dam,
          line = list(color = "steelblue", width = 10),
          hoverinfo = "text",
          text = ~paste0(
            "<b>", Dam, "</b>",
            "<br>Born: ",               format(dobmom,      "%Y-%m-%d"),
            "<br>First litter: ",       format(start_date,  "%Y-%m-%d"),
            "<br>Last litter recorded: ",format(end_date,   "%Y-%m-%d"),
            "<br>Litters so far: ", n_litters,
            dplyr::if_else(!is.na(retirement_date),
                           paste0("<br>Retirement date: ",
                                  format(retirement_date, "%Y-%m-%d")),
                           "")
          ),
          name = "Real Active Period"
        ) %>%
        layout(
          title = list(text = paste0(
            "Active Mother Periods<br>",
            "<sup>Hypothetical Active Periods are based on the average age at first litter ",
            "and remaining litters, adjusted for retirement.</sup>"
          )),
          xaxis = list(title = "Year", type = "date",
                       showticklabels = FALSE, tickvals = years,
                       showgrid = TRUE, gridcolor = "rgba(200,200,200,0.3)"),
          yaxis = list(title = "Dam", categoryorder = "trace"),
          annotations = annotations,
          showlegend  = TRUE,
          margin      = list(t = 100, b = 100, l = 100, r = 100)
        )
    })
  })
}
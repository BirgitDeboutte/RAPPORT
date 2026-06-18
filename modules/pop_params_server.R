# ── pop_params_server.R ───────────────────────────────────────────────────────
# Panel helper for the "Trends" tab.
# Called from popServer(); shares input/output/session namespace.
#
# Inputs consumed: plotvar, override_categorical, remove_na, time_range,
#                  facet_var, show_proportions, summary_table_rows_selected
# Defines outputs: params, facet_var, proportion_toggle, time_range,
#                  paramplot, trend_vbs_spacer, trend_vbs,
#                  summary_section, summary_table, selected_card, param_selected_data
# Depends on global helpers: is_cat_var, is_num_var

popParamsHelper <- function(input, output, session, rd) {
  ns <- session$ns
  
  # ── UI controls ───────────────────────────────────────────────────────────────
  output$params <- renderUI({
    req(rd$merged)
    tagList(
      selectizeInput(ns("plotvar"), "Choose a Parameter to Visualize:",
                     choices  = c("", "Coefficient of Inbreeding (COI)", colnames(rd$merged)),
                     selected = "Coefficient of Inbreeding (COI)",
                     options  = list(placeholder = "Select Parameter")),
      checkboxInput(ns("override_categorical"), "This Is Not A Numeric Parameter",
                    value = FALSE),
      checkboxInput(ns("remove_na"), "Don't Show Missing Data In Plot", value = TRUE)
    )
  })
  
  output$facet_var <- renderUI({
    req(input$plotvar)
    if (input$plotvar %in% c("Coefficient of Inbreeding (COI)", "")) return(NULL)
    selectInput(ns("facet_var"), "Facet by:",
                choices = c("None", rd$datsex, rd$peddam, rd$pedsire), selected = "None")
  })
  
  output$proportion_toggle <- renderUI({
    req(rd$merged, input$plotvar)
    if (isTRUE(input$override_categorical) || is_cat_var(rd$merged[[input$plotvar]]))
      checkboxInput(ns("show_proportions"), "Show proportions instead of counts", value = FALSE)
  })
  
  output$time_range <- renderUI({
    req(rd$merged)
    dates  <- as.Date(as.character(rd$merged$dob))
    dates  <- dates[!is.na(dates)]
    min_yr <- as.integer(format(min(dates), "%Y"))
    max_yr <- as.integer(format(max(dates), "%Y"))
    sliderInput(ns("time_range"), "Birth Year Range:",
                min = min_yr, max = max_yr, value = c(min_yr, max_yr),
                step = 1, sep = "")
  })
  
  # ── Data prep ─────────────────────────────────────────────────────────────────
  param_data <- reactive({
    req(rd$merged, input$plotvar, input$time_range)
    if (input$plotvar == "Coefficient of Inbreeding (COI)") {
      req(rd$inbreeding, rd$gen_depth)
      return(rd$merged)
    }
    d <- rd$merged
    d$dob       <- as.Date(as.character(d$dob))
    d$BirthYear <- factor(as.character(d$BirthYear),
                          levels = sort(unique(as.character(d$BirthYear))))
    d[!is.na(d$dob) &
        as.integer(format(d$dob, "%Y")) >= input$time_range[1] &
        as.integer(format(d$dob, "%Y")) <= input$time_range[2], ]
  })
  
  safe_var <- reactive({
    var  <- input$plotvar
    req(var, var != "")
    data <- param_data()
    req(var == "Coefficient of Inbreeding (COI)" || var %in% names(data))
    var
  })
  
  # ── make_trend helper ─────────────────────────────────────────────────────────
  make_trend <- function(x_date, y_num) {
    x_num <- as.numeric(x_date)
    ok    <- !is.na(x_num) & !is.na(y_num)
    if (sum(ok) < 2) return(NULL)
    fit   <- lm(y ~ x, data = data.frame(x = x_num[ok], y = y_num[ok]))
    x_seq <- seq(min(x_num[ok]), max(x_num[ok]), length.out = 100)
    data.frame(
      x = as.character(as.Date(round(x_seq), origin = "1970-01-01")),
      y = predict(fit, newdata = data.frame(x = x_seq))
    )
  }
  
  # ── Trace info (shared by plot and row-selection highlight) ───────────────────
  trace_info_r <- reactive({
    var  <- safe_var()
    if (var == "Coefficient of Inbreeding (COI)") return(NULL)
    data <- param_data()
    is_numeric <- is_num_var(data[[var]]) && !isTRUE(input$override_categorical)
    facet_var  <- if (!is.null(input$facet_var) && input$facet_var != "None")
      input$facet_var else NULL
    if (is_numeric) {
      if (is.null(facet_var)) return(NULL)
      data[[var]] <- as.numeric(data[[var]])
      panels <- split(data, data[[facet_var]])
      list(type      = "numeric",
           panels    = names(panels),
           has_trend = sapply(names(panels), function(p)
             !is.null(make_trend(panels[[p]]$dob, panels[[p]][[var]]))))
    } else {
      data[[var]] <- as.factor(data[[var]])
      if (is.null(facet_var)) {
        list(type = "categorical_simple", levels = levels(data[[var]]))
      } else {
        panels <- split(data, data[[facet_var]])
        list(type         = "categorical_facet",
             panels       = names(panels),
             panel_levels = lapply(panels, function(d) levels(droplevels(d[[var]]))))
      }
    }
  })
  
  # ── Main plot ─────────────────────────────────────────────────────────────────
  output$paramplot <- renderPlotly({
    var  <- safe_var()
    data <- param_data()
    
    # COI 
    if (var == "Coefficient of Inbreeding (COI)") {
      req(rd$inbreeding, rd$ownpop, rd$datid)
      
      # Per-animal Fi + BirthYear
      ne_df    <- rd$ownpop[, c(rd$datid, "BirthYear"), drop = FALSE]
      ne_df$id <- as.character(ne_df[[rd$datid]])
      ne_df$Fi <- rd$inbreeding[ne_df$id]
      ne_df    <- ne_df[!is.na(ne_df$Fi) & !is.na(ne_df$BirthYear), ]
      
      if (nrow(ne_df) < 5) {
        p <- plot_ly() %>%
          layout(title = "Insufficient data in reference population to plot inbreeding trend.")
        event_register(p, "plotly_click")
        return(p)
      }
      
      # ── Cohort means ───────────────────────────────────────────────────────────
      cohort_f <- ne_df %>%
        group_by(BirthYear) %>%
        summarise(
          mean_Fi = mean(Fi, na.rm = TRUE),
          sd_Fi   = sd(Fi,   na.rm = TRUE),
          n       = n(),
          .groups = "drop"
        ) %>%
        arrange(BirthYear) %>%
        mutate(
          BirthYearNum = suppressWarnings(as.numeric(as.character(BirthYear))),
          hover_text = paste0(
            "Birth year: ",    BirthYear,
            "<br>Mean COI: ",  round(mean_Fi, 4),
            "<br>SD: ",        round(sd_Fi,   4),
            "<br>N animals: ", n
          )
        ) %>%
        filter(!is.na(BirthYearNum))
      
      # ── Years-per-generation: use rd$genint when available, else estimate ──────
      L_years <- NA_real_
      
      if (!is.null(rd$genint) && is.finite(rd$genint) && rd$genint > 0) {
        L_years <- rd$genint
      } else if (!is.null(rd$gen_depth) && nrow(rd$gen_depth) > 0) {
        # Fallback only if genint couldn't be computed (insufficient dam DOBs etc.)
        own_yr <- data.frame(
          id        = as.character(rd$ownpop[[rd$datid]]),
          BirthYear = suppressWarnings(as.numeric(as.character(rd$ownpop$BirthYear))),
          stringsAsFactors = FALSE
        )
        gd <- merge(rd$gen_depth, own_yr, by = "id")
        gd <- gd[!is.na(gd$generation) & !is.na(gd$BirthYear), ]
        if (nrow(gd) >= 10 && length(unique(gd$BirthYear)) >= 2) {
          gen_fit <- tryCatch(lm(generation ~ BirthYear, data = gd),
                              error = function(e) NULL)
          if (!is.null(gen_fit)) {
            slope_gen <- as.numeric(coef(gen_fit)[2])
            if (is.finite(slope_gen) && slope_gen > 0) L_years <- 1 / slope_gen
          }
        }
      }
      
      # ── Linear trend on cohort means (weighted by N), projected forward ────────
      trend_fit <- if (nrow(cohort_f) >= 3)
        tryCatch(lm(mean_Fi ~ BirthYearNum, data = cohort_f, weights = n),
                 error = function(e) NULL) else NULL
      
      yr_min  <- min(cohort_f$BirthYearNum)
      yr_max  <- max(cohort_f$BirthYearNum)
      yr_proj <- yr_max + 5
      
      trend_df <- NULL
      if (!is.null(trend_fit)) {
        x_seq <- seq(yr_min, yr_proj, length.out = 120)
        pred  <- predict(trend_fit, newdata = data.frame(BirthYearNum = x_seq),
                         interval = "confidence", level = 0.95)
        trend_df <- data.frame(
          x   = x_seq,
          y   = pmax(pred[, "fit"], 0),
          lwr = pmax(pred[, "lwr"], 0),
          upr = pmax(pred[, "upr"], 0)
        )
      }
      
      # ── 1%/generation upper limit, anchored at the trend's value at yr_min ─────
      ref_df <- NULL
      if (!is.na(L_years) && L_years > 0 && !is.null(trend_df)) {
        anchor_y      <- trend_df$y[1]
        rate_per_year <- 0.01 / L_years
        x_seq <- seq(yr_min, yr_proj, length.out = 60)
        ref_df <- data.frame(
          x = x_seq,
          y = anchor_y + rate_per_year * (x_seq - yr_min)
        )
      }
      
      # ── Average ΔF line, anchored at the trend's value at yr_min ───────────
      avg_df_line <- NULL
      if (!is.na(L_years) && L_years > 0 && !is.null(trend_df) &&
          !is.null(rd$deltaF) && is.finite(rd$deltaF) && rd$deltaF > 0) {
        anchor_y_avg    <- trend_df$y[1]
        avg_rate_per_yr <- rd$deltaF / L_years
        x_seq_avg       <- seq(yr_min, yr_proj, length.out = 60)
        avg_df_line <- data.frame(
          x = x_seq_avg,
          y = anchor_y_avg + avg_rate_per_yr * (x_seq_avg - yr_min)
        )
      }
      
      y_max <- max(
        max(cohort_f$mean_Fi, na.rm = TRUE),
        if (!is.null(trend_df)) max(trend_df$upr, na.rm = TRUE) else 0,
        if (!is.null(ref_df))   max(ref_df$y,     na.rm = TRUE) else 0,
        if (!is.null(avg_df_line))  max(avg_df_line$y,   na.rm = TRUE) else 0,
        0.03
      ) * 1.18
      
      p <- plot_ly(source = "paramplot")
      
      # Subtle shading on the projection zone
      p <- p %>% layout(shapes = list(
        list(type = "rect", xref = "x", yref = "paper",
             x0 = yr_max, x1 = yr_proj, y0 = 0, y1 = 1,
             fillcolor = "rgba(0,0,0,0.035)", line = list(width = 0))
      ))
      
      # Legend name for observed trend
      obs_trend_name <- "Observed trend"
      if (!is.null(trend_fit)) {
        slope_per_year <- as.numeric(coef(trend_fit)[2])
        obs_dF_per_gen <- if (!is.na(L_years)) slope_per_year * L_years else NA_real_
        if (!is.na(obs_dF_per_gen))
          obs_trend_name <- paste0("Observed trend (\u0394F \u2248 ",
                                   sprintf("%.2f%%", obs_dF_per_gen * 100), " / gen)")
      }
      
      # 95% CI ribbon around the trend
      
      # 95% CI ribbon around the trend
      if (!is.null(trend_df)) {
        p <- p %>% add_ribbons(
          data = trend_df, x = ~x, ymin = ~lwr, ymax = ~upr,
          fillcolor = "rgba(21,101,192,0.15)", line = list(width = 0),
          hoverinfo = "none", showlegend = FALSE
        )
        hist_df <- trend_df[trend_df$x <= yr_max, ]
        proj_df <- trend_df[trend_df$x >= yr_max, ]
        p <- p %>% add_lines(
          data = hist_df, x = ~x, y = ~y,
          line = list(color = "#1565C0", width = 3),
          hoverinfo = "none", showlegend = TRUE,
          name = obs_trend_name, legendgroup = "obs_trend"
        )
        p <- p %>% add_lines(
          data = proj_df, x = ~x, y = ~y,
          line = list(color = "#1565C0", width = 3, dash = "dot"),
          hoverinfo = "none", showlegend = FALSE, legendgroup = "obs_trend"
        )
      }
      
      # 1%/generation upper limit
      if (!is.null(ref_df)) {
        p <- p %>% add_lines(
          data = ref_df, x = ~x, y = ~y,
          line = list(color = "#c62828", width = 2, dash = "dash"),
          hoverinfo = "none", showlegend = TRUE,
          name = paste0("\u0394F = 1% / gen")
        )
      }
      
      # Average ΔF per generation
      if (!is.null(avg_df_line)) {
        p <- p %>% add_lines(
          data = avg_df_line, x = ~x, y = ~y,
          line = list(color = "#65734d", width = 2, dash = "dashdot"),
          hoverinfo = "none", showlegend = TRUE,
          name = paste0("Avg. \u0394F = ", sprintf("%.2f%%", rd$deltaF * 100), " / gen")
        )
      }
      
      # Cohort means — neutral colour, no traffic-light bands
      p <- p %>% add_markers(
        data       = cohort_f,
        x          = ~BirthYearNum,
        y          = ~mean_Fi,
        marker     = list(color = "#7a8a91", size = 7, opacity = 0.75,
                          line = list(color = "#ffffff", width = 1)),
        text       = ~hover_text, hoverinfo = "text",
        showlegend = FALSE
      )
      
      # Generation interval note in legend 
      if (!is.na(L_years) && L_years > 0) {
        p <- p %>% add_markers(
          x = yr_min, y = -1,
          marker     = list(size = 0, opacity = 0, color = "rgba(0,0,0,0)"),
          hoverinfo  = "none", showlegend = TRUE,
          name       = paste0("Generation interval = ", sprintf("%.1f", L_years), " yr")
        )
      }
      
        
      p <- p %>% layout(
        xaxis       = list(title = "Birth Year", tickmode = "linear", dtick = 1,
                           range = c(yr_min - 0.5, yr_proj + 0.5)),
        yaxis       = list(title = "Mean Coefficient of Inbreeding (COI)",
                           tickformat = ".1%", range = c(0, y_max)),
        showlegend  = TRUE,
        margin      = list(l = 70, r = 10, b = 60, t = 20)
      )
      
      event_register(p, "plotly_click")
      return(p)
    }
    
    var_class      <- class(data[[var]])
    is_categorical <- isTRUE(input$override_categorical) || !is_num_var(data[[var]])
    if (input$remove_na)
      data <- data[!is.na(data[[var]]) & !is.na(data$dob), ]
    facet_var <- if (!is.null(input$facet_var) && input$facet_var != "None")
      input$facet_var else NULL
    
    # Categorical
    if (is_categorical) {
      data[[var]]    <- as.factor(data[[var]])
      data$BirthYear <- as.character(data$BirthYear)
      show_prop      <- isTRUE(input$show_proportions)
      group_cols <- c("BirthYear", var, if (!is.null(facet_var)) facet_var)
      counts <- data %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(Count = n(), .groups = "drop") %>%
        group_by(across(all_of(c("BirthYear", if (!is.null(facet_var)) facet_var)))) %>%
        mutate(Total = sum(Count), Proportion = Count / Total) %>%
        ungroup()
      y_col   <- if (show_prop) "Proportion" else "Count"
      y_label <- if (show_prop) "Proportion"  else "Count"
      if (is.null(facet_var)) {
        p <- plot_ly(counts, x = ~BirthYear, y = ~get(y_col), color = ~get(var),
                     source = "paramplot", type = "bar",
                     hovertext = ~paste0("Year: ", BirthYear, "<br>", var, ": ", get(var),
                                         "<br>", y_label, ": ",
                                         if (show_prop) round(Proportion, 3) else Count),
                     hoverinfo = "text", text = "", textposition = "none") %>%
          layout(barmode = "stack", dragmode = FALSE,
                 xaxis   = list(title = "Birth Year"),
                 yaxis   = list(title = y_label,
                                tickformat = if (show_prop) ".0%" else ""),
                 legend  = list(title = list(text = var)))
      } else {
        panels   <- split(counts, counts[[facet_var]])
        n_panels <- length(panels)
        n_rows   <- ceiling(n_panels / min(n_panels, 3))
        plots <- lapply(names(panels), function(panel) {
          d <- panels[[panel]]
          plot_ly(d, x = ~BirthYear, y = d[[y_col]], color = d[[var]],
                  source = "paramplot", type = "bar",
                  showlegend = (panel == names(panels)[1]),
                  hovertext  = ~paste0("Year: ", BirthYear, "<br>", var, ": ", d[[var]],
                                       "<br>", y_label, ": ",
                                       if (show_prop) round(Proportion, 3) else Count),
                  hoverinfo = "text", text = "", textposition = "none") %>%
            layout(barmode  = "stack", dragmode = FALSE,
                   annotations = list(list(
                     text = panel, showarrow = FALSE,
                     x = 0.5, y = 1.05, xref = "paper", yref = "paper",
                     xanchor = "center", yanchor = "top", font = list(size = 11)
                   )))
        })
        p <- subplot(plots, shareY = TRUE, nrows = n_rows, titleX = FALSE,
                     margin = c(0.02, 0.02, 0.06, 0.02)) %>%
          layout(yaxis  = list(title = y_label, tickformat = if (show_prop) ".0%" else ""),
                 height = 300 * n_rows,
                 margin = list(l = 50, r = 10, b = 40, t = 40, pad = 0))
      }
      event_register(p, "plotly_click")
      return(p)
    }
    
    # Numeric
    data[[var]]     <- as.numeric(data[[var]])
    data$dob_char   <- as.character(as.Date(as.character(data$dob)))
    data$hover_text <- paste0(rd$datid, ": ", data[[rd$datid]], "<br>",
                              var,       ": ", data[[var]],      "<br>",
                              rd$datdob, ": ", data$dob_char,    "<br>",
                              rd$datsex, ": ", data[[rd$datsex]])
    if (is.null(facet_var)) {
      trend <- make_trend(data$dob, data[[var]])
      p <- plot_ly(source = "paramplot") %>%
        add_markers(data = data, x = ~dob_char, y = ~get(var),
                    text = ~hover_text, hoverinfo = "text",
                    marker = list(color = "blue", opacity = 0.6), name = var)
      if (!is.null(trend))
        p <- p %>% add_lines(x = trend$x, y = trend$y,
                             line = list(color = "red", width = 1.5),
                             hoverinfo = "none", showlegend = FALSE, name = "Trend")
      p <- p %>% layout(xaxis = list(title = rd$datdob, type = "date"),
                        yaxis = list(title = var), dragmode = "select")
      event_register(p, "plotly_selected")
      return(p)
    }
    panels <- split(data, data[[facet_var]])
    colors <- colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                 "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
                                 "#bcbd22", "#17becf"))(length(panels))
    p <- plot_ly(source = "paramplot")
    for (i in seq_along(names(panels))) {
      panel <- names(panels)[i]; d <- panels[[panel]]; col <- colors[i]
      trend <- make_trend(d$dob, d[[var]])
      p <- p %>%
        add_markers(data = d, x = ~dob_char, y = d[[var]],
                    text = ~hover_text, hoverinfo = "text",
                    marker = list(color = col, opacity = 0.7, size = 7),
                    name = panel, legendgroup = panel, showlegend = TRUE)
      if (!is.null(trend))
        p <- p %>% add_lines(x = trend$x, y = trend$y,
                             line = list(color = col, width = 1.5),
                             hoverinfo = "none", legendgroup = panel, showlegend = FALSE)
    }
    p %>% layout(xaxis  = list(title = rd$datdob, type = "date",
                               tickformat = "%Y", dtick = "M12"),
                 yaxis  = list(title = var), dragmode = "select",
                 legend = list(title = list(text = facet_var),
                               itemclick = "toggleothers", itemdoubleclick = "toggle"),
                 margin = list(l = 50, r = 10, b = 40, t = 20)) %>%
      event_register("plotly_selected")
  })
  # ── Trend KPI value boxes ─────────────────────────────────────────────────────
  output$trend_vbs_spacer <- renderUI({ req(safe_var()); p("") })
  
  output$trend_vbs <- renderUI({
    var  <- safe_var()
    data <- param_data()
    is_num <- var == "Coefficient of Inbreeding (COI)" || (is_num_var(data[[var]]) && !isTRUE(input$override_categorical))
    if (is_num) {
      vals <- if (var == "Coefficient of Inbreeding (COI)") { req(rd$inbreeding); rd$inbreeding }
      else              as.numeric(data[[var]])
      vals   <- vals[!is.na(vals)]
      n_obs  <- length(vals)
      mean_v <- round(mean(vals), 3)
      sd_v   <- round(sd(vals),   3)
      trend_dir <- tryCatch({
        d_trend <- data[!is.na(data[[var]]) & !is.na(data$dob), ]
        if (nrow(d_trend) < 3) return("\u2014")
        fit  <- lm(as.numeric(d_trend[[var]]) ~ as.numeric(d_trend$dob))
        coef <- coef(fit)[2]
        if      (coef >  1e-6) "\u2191 Increasing"
        else if (coef < -1e-6) "\u2193 Decreasing"
        else                    "\u2192 Stable"
      }, error = function(e) "\u2014")
      trend_color <- if (grepl("\u2191", trend_dir)) "#e65100" else if (grepl("\u2193", trend_dir)) "#2e7a3a" else "#888"
      yr_min <- min(as.integer(format(data$dob[!is.na(data$dob)], "%Y")))
      yr_max <- max(as.integer(format(data$dob[!is.na(data$dob)], "%Y")))
      layout_columns(
        value_box(title = "Observations", value = h3(n_obs),
                  showcase = bsicons::bs_icon("list-ol"),       theme = "primary"),
        value_box(title = "Mean",         value = h3(mean_v),
                  showcase = bsicons::bs_icon("calculator"),    theme = "primary"),
        value_box(title = "SD",           value = h3(sd_v),
                  showcase = bsicons::bs_icon("distribute-vertical"), theme = "secondary"),
        value_box(title = "Trend",
                  value = tags$span(trend_dir, style = paste0("font-size:1rem;font-weight:700;color:", trend_color)),
                  showcase = bsicons::bs_icon("graph-up"),      theme = "secondary"),
        value_box(title = "Time Span",
                  value = tags$span(paste0(yr_min, " \u2013 ", yr_max),
                                    style = "font-size:1rem;font-weight:600;"),
                  showcase = bsicons::bs_icon("calendar-range"), theme = "info"),
        col_widths = c(2, 2, 2, 3, 3)
      )
    } else {
      vals    <- data[[var]][!is.na(data[[var]])]
      n_obs   <- length(vals)
      n_cats  <- length(unique(vals))
      top_cat <- names(sort(table(vals), decreasing = TRUE))[1]
      top_pct <- round(100 * sum(vals == top_cat) / n_obs, 1)
      yr_min  <- min(as.integer(format(data$dob[!is.na(data$dob)], "%Y")))
      yr_max  <- max(as.integer(format(data$dob[!is.na(data$dob)], "%Y")))
      layout_columns(
        value_box(title = "Observations", value = h3(n_obs),
                  showcase = bsicons::bs_icon("list-ol"),        theme = "primary"),
        value_box(title = "Categories",   value = h3(n_cats),
                  showcase = bsicons::bs_icon("tags"),           theme = "primary"),
        value_box(title = "Most Common",
                  value = tagList(tags$span(top_cat, style = "font-size:1rem;font-weight:700;"),
                                  tags$br(),
                                  tags$span(paste0(top_pct, "% of records"), style = "font-size:0.75rem;color:#888;")),
                  showcase = bsicons::bs_icon("bar-chart-steps"), theme = "secondary"),
        value_box(title = "Time Span",
                  value = tags$span(paste0(yr_min, " \u2013 ", yr_max),
                                    style = "font-size:1rem;font-weight:600;"),
                  showcase = bsicons::bs_icon("calendar-range"),  theme = "info"),
        col_widths = c(3, 2, 4, 3)
      )
    }
  })
  
  # ── Summary section & table ───────────────────────────────────────────────────
  output$summary_section <- renderUI({
    var <- safe_var()
    if (var == "Coefficient of Inbreeding (COI)") {
      req(rd$deltaF, rd$ecg, rd$Ne)
      tagList(
        hr(style = "margin: 12px 0"),
        layout_columns(
          tags$div(tags$p(style = "font-size:0.78rem;font-weight:700;text-transform:uppercase;letter-spacing:0.06em;color:#888;margin-bottom:4px", "Average Rate of Inbreeding*"),
                   tags$span(paste0(round(rd$deltaF*100, 2), "%"), style = "font-size:1.1rem;font-weight:600;")),
          tags$div(tags$p(style = "font-size:0.78rem;font-weight:700;text-transform:uppercase;letter-spacing:0.06em;color:#888;margin-bottom:4px", "Equiv. Complete Generations"),
                   tags$span(round(rd$ecg, 1), style = "font-size:1.1rem;font-weight:600;")),
          tags$div(tags$p(style = "font-size:0.78rem;font-weight:700;text-transform:uppercase;letter-spacing:0.06em;color:#888;margin-bottom:4px", "Effective Population Size"),
                   tags$span(round(rd$Ne, 1), style = "font-size:1.1rem;font-weight:600;")),
          col_widths = c(4, 4, 4)
        ),
        tags$p(style = "font-style:italic;color:#aaa;font-size:0.8rem;margin-top:6px;",
               "* The trend above measures the current per-generation change in mean COI; ",
               "the average rate of inbreeding here is each animal's lifetime accumulation rate, averaged across animals. ",
               "If the observed trend is higher, the rate of inbreeding is accelerating.")
      )
    } else {
      tagList(
        hr(style = "margin: 12px 0"),
        tags$p(style = "font-size:0.78rem;font-weight:700;text-transform:uppercase;letter-spacing:0.06em;color:#888;margin-bottom:6px", "Summary Statistics"),
        dataTableOutput(ns("summary_table"))
      )
    }
  })
  
  summary_df_r <- reactive({
    var  <- safe_var()
    if (var == "Coefficient of Inbreeding (COI)") return(NULL)
    data <- param_data()
    is_numeric <- class(data[[var]]) %in% c("numeric", "integer") && !isTRUE(input$override_categorical)
    facet_var  <- if (!is.null(input$facet_var) && input$facet_var != "None")
      input$facet_var else NULL
    if (is_numeric) {
      if (!is.null(facet_var) && facet_var %in% names(data)) {
        data |> group_by(!!sym(facet_var)) |>
          filter(sum(!is.na(.data[[var]])) > 0) |>
          summarise(Observations = sum(!is.na(.data[[var]])),
                    Mean   = round(mean  (.data[[var]], na.rm = TRUE), 2),
                    Median = round(median(.data[[var]], na.rm = TRUE), 2),
                    SD     = round(sd    (.data[[var]], na.rm = TRUE), 2),
                    Min    = round(min   (.data[[var]], na.rm = TRUE), 2),
                    Max    = round(max   (.data[[var]], na.rm = TRUE), 2),
                    .groups = "drop")
      } else {
        stats <- c(Observations = sum(!is.na(data[[var]])),
                   Mean   = round(mean  (data[[var]], na.rm = TRUE), 2),
                   Median = round(median(data[[var]], na.rm = TRUE), 2),
                   SD     = round(sd    (data[[var]], na.rm = TRUE), 2),
                   Min    = round(min   (data[[var]], na.rm = TRUE), 2),
                   Max    = round(max   (data[[var]], na.rm = TRUE), 2))
        data.frame(Statistic = names(stats), Value = unname(stats))
      }
    } else {
      if (!is.null(facet_var) && facet_var %in% names(data)) {
        data |>
          group_by(!!sym(facet_var), !!sym(var)) |>
          summarise(Count = n(), .groups = "drop") |>
          group_by(!!sym(facet_var)) |>
          mutate(Total = sum(Count), Percent = round(100 * Count / Total, 1)) |>
          ungroup() |>
          pivot_wider(id_cols = !!sym(facet_var), names_from = !!sym(var),
                      values_from = Count, values_fill = 0) |>
          left_join(data |> group_by(!!sym(facet_var)) |>
                      summarise(Total = n(), .groups = "drop"), by = facet_var)
      } else {
        data |> count(!!sym(var), name = "Count") |>
          mutate(Total = sum(Count), Percent = round(100 * Count / Total, 1))
      }
    }
  })
  
  output$summary_table <- renderDataTable({
    summary_df <- summary_df_r()
    req(summary_df)
    datatable(summary_df, style = "bootstrap", rownames = FALSE,
              selection = list(mode = "multiple", selected = NULL),
              options   = list(lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                               pageLength = -1))
  })
  
  # ── Row-selection opacity highlight ──────────────────────────────────────────
  observeEvent(input$summary_table_rows_selected, {
    ti <- trace_info_r()
    req(ti)
    selected_rows <- input$summary_table_rows_selected
    opacities <- if (ti$type == "numeric") {
      all_names      <- ti$panels
      selected_names <- if (length(selected_rows) == 0) all_names else all_names[selected_rows]
      ops <- c()
      for (i in seq_along(all_names)) {
        is_sel <- all_names[i] %in% selected_names
        ops    <- c(ops, if (is_sel) 0.8 else 0.08)
        if (ti$has_trend[[i]]) ops <- c(ops, if (is_sel) 1.0 else 0.08)
      }
      ops
    } else if (ti$type == "categorical_simple") {
      all_levels      <- ti$levels
      selected_levels <- if (length(selected_rows) == 0) all_levels else all_levels[selected_rows]
      ifelse(all_levels %in% selected_levels, 1.0, 0.08)
    } else if (ti$type == "categorical_facet") {
      all_panels      <- ti$panels
      selected_panels <- if (length(selected_rows) == 0) all_panels else all_panels[selected_rows]
      ops <- c()
      for (panel in all_panels) {
        n   <- length(ti$panel_levels[[panel]])
        ops <- c(ops, rep(if (panel %in% selected_panels) 1.0 else 0.08, n))
      }
      ops
    }
    plotlyProxy(ns("paramplot"), session) %>%
      plotlyProxyInvoke("restyle", list(opacity = as.list(opacities)))
  }, ignoreNULL = FALSE)
  
  observeEvent(list(input$plotvar, input$facet_var), {
    plotlyProxy(ns("paramplot"), session) %>%
      plotlyProxyInvoke("restyle", list(opacity = 1))
    dataTableProxy(ns("summary_table")) %>% selectRows(NULL)
  })
  
  # ── Selected animals card & table ─────────────────────────────────────────────
  output$selected_card <- renderUI({
    var <- safe_var()
    if (var == "Coefficient of Inbreeding (COI)") {
      sel <- event_data("plotly_selected", source = "paramplot")
      if (is.null(sel) || nrow(sel) == 0) return(NULL)
      coi_df <- data.frame(id = names(rd$inbreeding), COI = round(rd$inbreeding, 2),
                           stringsAsFactors = FALSE)
      coi_df <- merge(coi_df, rd$gen_depth, by = "id", all.x = TRUE)
      selected_ids <- unique(unlist(lapply(seq_len(nrow(sel)), function(i) {
        coi_df$id[!is.na(coi_df$generation) &
                    coi_df$generation == round(sel$x[i]) &
                    abs(coi_df$COI - sel$y[i]) < 0.001]
      })))
      if (length(selected_ids) == 0) return(NULL)
      ped_df <- as.data.frame(rd$ped)
      result <- do.call(rbind, lapply(selected_ids, function(animal_id) {
        ped_row <- ped_df[as.character(ped_df[[rd$pedid]]) == animal_id, ]
        dam_id  <- if (nrow(ped_row) > 0) as.character(ped_row[[rd$peddam]])  else NA
        sire_id <- if (nrow(ped_row) > 0) as.character(ped_row[[rd$pedsire]]) else NA
        data.frame(ID = animal_id,
                   Generation = coi_df$generation[coi_df$id == animal_id],
                   COI        = coi_df$COI[coi_df$id == animal_id],
                   Dam        = if (!is.na(dam_id))  dam_id  else "\u2014",
                   Dam_COI    = if (!is.na(dam_id)  && dam_id  %in% names(rd$inbreeding))
                     round(rd$inbreeding[dam_id],  4) else NA,
                   Sire       = if (!is.na(sire_id)) sire_id else "\u2014",
                   Sire_COI   = if (!is.na(sire_id) && sire_id %in% names(rd$inbreeding))
                     round(rd$inbreeding[sire_id], 4) else NA,
                   stringsAsFactors = FALSE)
      }))
      result <- result[order(result$Generation, result$COI, decreasing = c(FALSE, TRUE)), ]
      card(card_header(paste0("Selected Animals (", nrow(result), ")")),
           card_body(datatable(result, style = "bootstrap", rownames = FALSE,
                               options = list(lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                                              pageLength = 10))))
    } else {
      is_cat        <- isTRUE(input$override_categorical) || !is_num_var(param_data()[[var]])
      has_selection <- if (!is_cat) {
        sel <- event_data("plotly_selected", source = "paramplot")
        !is.null(sel) && nrow(sel) > 0
      } else {
        click <- event_data("plotly_click", source = "paramplot")
        !is.null(click) &&
          as.character(click$x) %in% unique(as.character(param_data()$BirthYear))
      }
      if (!has_selection) return(NULL)
      card(card_header("Selected Animals"),
           card_body(dataTableOutput(ns("param_selected_data"))))
    }
  })
  
  output$param_selected_data <- renderDataTable({
    var  <- safe_var()
    data <- param_data()
    data$dob       <- as.Date(as.character(data$dob))
    data$BirthYear <- as.character(data$BirthYear)
    is_categorical <- isTRUE(input$override_categorical) || !is_num_var(data[[var]])
    if (input$remove_na) data <- data[!is.na(data[[var]]) & !is.na(data$dob), ]
    if (!is_categorical) {
      sel <- event_data("plotly_selected", source = "paramplot")
      if (is.null(sel) || nrow(sel) == 0) return(NULL)
      data$dob_char <- as.character(data$dob)
      brushed <- data[as.Date(data$dob_char) %in% as.Date(as.character(sel$x)) &
                        data[[var]] %in% sel$y, ]
      keep <- c(rd$datid, "dob", var,
                names(brushed)[!names(brushed) %in%
                                 c(rd$datid, "dob", var, "dob_char", "hover_text", "BirthYear")])
      keep <- keep[keep %in% names(brushed)]
      out  <- brushed[, keep, drop = FALSE]
      names(out)[names(out) == "dob"] <- rd$datdob
      datatable(out, style = "bootstrap", rownames = FALSE,
                options = list(paging = FALSE, scrollY = "100%",
                               scrollX = "100%", scrollCollapse = TRUE))
    } else {
      click <- event_data("plotly_click", source = "paramplot")
      if (is.null(click)) return(NULL)
      birth_year_clicked <- as.character(click$x)
      if (!birth_year_clicked %in% unique(data$BirthYear)) return(NULL)
      facet_var <- if (!is.null(input$facet_var) && input$facet_var != "None")
        input$facet_var else NULL
      selected <- if (!is.null(facet_var) && facet_var %in% names(data) &&
                      "curveNumber" %in% names(click)) {
        grouped     <- split(data, data[[facet_var]])
        panel_value <- names(grouped)[click$curveNumber %% length(grouped) + 1]
        data[data$BirthYear == birth_year_clicked & data[[facet_var]] == panel_value, ]
      } else {
        data[data$BirthYear == birth_year_clicked, ]
      }
      keep <- c(rd$datid, "BirthYear", var,
                names(selected)[!names(selected) %in%
                                  c(rd$datid, "dob", var, "BirthYear", "dob_char", "hover_text")])
      keep <- keep[keep %in% names(selected)]
      datatable(selected[, keep, drop = FALSE], style = "bootstrap", rownames = FALSE,
                options = list(lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                               pageLength = -1))
    }
  })
}
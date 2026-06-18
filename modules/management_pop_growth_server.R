# ── management_pop_growth_server.R ───────────────────────────────────────────
# Panel helper for the "Population Growth" tab.
# Called from managementServer(); shares input/output/session namespace.
#
# Inputs consumed: goal, correction_pct, litdam, litsire, siresext_pct, retire_col
# Sets on rd:      rd$damsint, rd$siresint
# Defines outputs: management_data_warning (shared across all tabs), no_breeders_warning,
#                  vb_goal_*, pb_*, last_year, ne_recommendation,
#                  vb_capacity (FIXED – expected progeny this year via capacity_vals),
#                  vb_ne, last_year_badge, this_year_badge, current

managementPopGrowthHelper <- function(input, output, session, rd,
                                      litter_stats,
                                      retirement_dates,
                                      dam_schedule,
                                      pup_projection_data,
                                      capacity_vals,
                                      breeders_col_specified,
                                      active_in_year,
                                      ly_row,
                                      empirical_correction_pct) {
  ns <- session$ns
  
  # ── retire_col choices ──────────────────────────────────────────────────────
  observe({
    req(rd$breeders)
    cols <- names(rd$breeders)
    
    detected <- match_col(cols, c(
      "^pensioen$", "^retire", "^retirement$", "^pension$",
      "^uitstroom$", "^afvoer$", "^end[._-]?date$", "^exit$"
    ))
    
    updateSelectInput(inputId = "retire_col",
                      choices  = c("None", cols),
                      selected = detected %||% "None")
  })
  
  # ── No (valid) retire col selected warning ─────────────────
  output$retire_col_warning <- renderUI({
    req(rd$breeders)
    col <- input$retire_col
    if (is.null(col) || col == "None") {
      tags$div(
        class = "text-danger",
        style = "font-size:0.8rem; margin-top:4px",
        tags$b("\u26a0 No retirement column selected."),
        " Early retired dams may be counted as active, making projections overly optimistic."
      )
    } else {
      x       <- rd$breeders[[col]]
      parsed  <- parse_dates_robust(x)
      n_total <- sum(!is.na(x) & as.character(x) != "")
      n_valid <- sum(!is.na(parsed))
      
      # after
      if (n_total > 0 && n_valid == 0) {
        tags$div(
          class = "text-danger",
          style = "font-size:0.8rem; margin-top:4px",
          tags$b("\u26a0 Selected column does not appear to contain recognisable dates."),
          tags$br(),
          tags$span(style = "color:#888",
                    "No values could be parsed as dates. Check the column selection.")
        )
      } else if (n_total > 0 && n_valid / n_total < 0.8) {
        tags$div(
          class = "text-warning",
          style = "font-size:0.8rem; margin-top:4px",
          tags$b("\u26a0 Some retirement dates could not be parsed."),
          tags$br(),
          tags$span(style = "color:#888",
                    paste0(n_valid, " of ", n_total, " values were recognised as dates. ",
                           "Animals with unparseable dates will be treated as still active."))
        )
      }
      # else: NULL — clean parse, no warning
    }
  })
  
  # ── Empirical correction hint (shown inline next to checkbox) ─────────────────
  output$empirical_correction_hint <- renderUI({
    if (!isTRUE(input$use_empirical_correction)) return(NULL)
    pct <- empirical_correction_pct()
    if (is.null(pct))
      tags$span("\u2014 not enough historical data",
                style = "font-size:0.78rem; color:#aaa; margin-left:4px")
    else
      tags$span(paste0("(", pct, "% from data)"),
                style = "font-size:0.78rem; color:#0277bd; margin-left:4px")
  })
  
  # ── No-breeders warning ─────────────────────────────────────────────────────
  output$no_breeders_warning <- renderUI({
    if (breeders_col_specified()) return(NULL)
    tags$div(
      class = "alert alert-warning",
      style = "font-size:0.9rem; padding:10px 14px; margin-bottom:12px",
      tags$b("\u26a0 No breeding stock column specified. "),
      "Population projections require a column in the data file identifying which animals ",
      "are designated breeding stock. Please specify this in the Data Input tab."
    )
  })
  
  # ── Interval warning (shared output – rendered once, shown in all 4 tabs) ──
  observe({
    req(rd$ped, rd$data, litter_stats(), pup_projection_data())
    
    output$management_data_warning <- renderUI({
      ls <- litter_stats()
      if (!is.na(ls$mean_litter_interval)) return(NULL)
      tags$div(
        class = "alert alert-warning",
        style = "font-size:0.9rem; padding:10px 14px; margin-bottom:12px",
        tags$b("\u26a0 Litter interval could not be calculated. "),
        "This happens when all dams in the programme have produced only one litter so far. ",
        "Production projections and the Dam Planner timeline will be unavailable until ",
        "at least some dams have had a second litter."
      )
    })
    
    # ── Goal value boxes + Programme Breakdown + rd$damsint / rd$siresint ────
    ls        <- litter_stats()
    correction <- input$correction_pct / 100
    lit_needed    <- ceiling((input$goal / rd$mls) / (1 - correction))
    dams_needed   <- ceiling(lit_needed / ls$mean_litter_per_year)
    dams_replaced <- ceiling(dams_needed / (input$litdam * ls$mean_litter_per_year))
    pct_ext       <- input$siresext_pct / 100
    n_ext         <- round(lit_needed * pct_ext)
    n_int         <- lit_needed - n_ext
    sires_int     <- ceiling(n_int / input$litsire)
    
    rd$damsint  <- dams_replaced
    rd$siresint <- sires_int
    
    output$vb_goal_pups      <- renderUI({ h3(input$goal) })
    output$vb_goal_litters   <- renderUI({ h3(lit_needed) })
    output$vb_goal_dams      <- renderUI({ h3(dams_needed) })
    
    output$pb_goal_pups        <- renderUI({ span(input$goal) })
    output$pb_goal_litters     <- renderUI({ span(lit_needed) })
    output$pb_goal_dams        <- renderUI({ span(dams_needed) })
    output$pb_sires_int        <- renderUI({ span(sires_int) })
    output$pb_dams_replaced    <- renderUI({ span(dams_replaced) })
    output$pb_litdam           <- renderUI({ span(input$litdam) })
    output$pb_sires_int_detail <- renderUI({ span(sires_int) })
    output$pb_litsire          <- renderUI({ span(input$litsire) })
    output$pb_litters_int      <- renderUI({ span(paste0(n_int, " (", 100 - input$siresext_pct, "%)")) })
    output$pb_litters_ext      <- renderUI({ span(paste0(n_ext, " (", input$siresext_pct, "%)")) })
    
  })
  # ── Last-year summary table ───────────────────────────────────────────────
  output$last_year <- renderUI({
    if (!breeders_col_specified()) return(
      tags$p(style = "color:#999; font-size:0.9rem",
             "Specify a breeding stock column to enable accurate projections."))
    req(pup_projection_data(), litter_stats(), rd$litterplan)
    
    ls        <- litter_stats()
    today     <- Sys.Date()
    last_year <- lubridate::year(today) - 1
    ly        <- rd$litterplan %>% filter(lubridate::year(dob) == last_year)
    
    n_ly_lit    <- nrow(ly)
    n_ly_pups   <- sum(ly$Littersize, na.rm = TRUE)
    
    ly_proj     <- pup_projection_data() %>% filter(year == last_year)
    n_ly_dams   <- if (nrow(ly_proj) > 0) ly_proj$expected_moms else 0L
    exp_ly_pups <- if (nrow(ly_proj) > 0) ly_proj$expected_pups else 0
    exp_ly_lit  <- if (!is.na(ls$mean_litter_size) && ls$mean_litter_size > 0)
      round(exp_ly_pups / ls$mean_litter_size) else 0
    ci_lo       <- exp_ly_lit * ls$ci_lower
    ci_hi       <- exp_ly_lit * ls$ci_upper
    
    tags$table(
      class = "table table-sm table-borderless mb-0",
      style = "font-size:0.92rem",
      tags$tbody(
        ly_row("Active dams",            n_ly_dams),
        ly_row("Expected litters",       floor(exp_ly_lit)),
        ly_row("Actual litters",         n_ly_lit),
        ly_row("Expected pups (95% CI)",
               paste0(floor(exp_ly_pups),
                      " [", floor(ci_lo), "\u2013", floor(ci_hi), "]")),
        ly_row("Actual pups",            n_ly_pups)
      )
    )
  })
  
  # ── Ne recommendation ───────────────────────────────────────────────────────
  output$ne_recommendation <- renderUI({
    Ne <- rd$Ne
    if (is.null(Ne)) return(NULL)
    suggested_pct <- rd$suggested_ext_pct
    risk_color <- if (Ne >= 200) "success" else if (Ne >= 100) "warning" else "danger"
    badge_style <- if (risk_color == "success")
      "background-color:#2e7a3a; color:#fff; font-size:0.82rem" else NULL
    
    tagList(
      tags$div(
        style = "margin-top: 4px",
        if (!is.null(badge_style))
          tags$span(class = "badge", style = badge_style, paste0("Ne = ", round(Ne)))
        else
          tags$span(class = paste0("badge bg-", risk_color), paste0("Ne = ", round(Ne))),
        tags$span(style = "font-size:0.82rem; color:#555; margin-left:6px",
                  paste0("Recommended external: \u2265 ", suggested_pct, "%"))
      ),
      tags$div(
        style = "margin-top:4px; font-size:0.82rem",
        if (input$siresext_pct < suggested_pct)
          tags$span(class = "text-danger",
                    paste0("\u26a0 ", input$siresext_pct, "% is below recommendation"))
        else
          tags$span(style = "color:#2e7a3a; font-weight:500",
                    paste0("\u2713 Meets recommendation for Ne = ", round(Ne)))
      )
    )
  })
  # ── Expected Progeny This Year value box (FIXED) ───────────────────────────────
  # Previously this output was only defined in the Pup Planner section and
  # displayed coverage% — wrong label and wrong value.
  # Now uses capacity_vals(), the same reactive as vb_pups_projected.
  output$vb_capacity <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(capacity_vals())
    cv <- capacity_vals()
    if (is.na(cv$capacity)) return(h3("\u2014", style = "color:grey"))
    tagList(
      h3(paste0(cv$capacity, " [", cv$ci_lo, "\u2013", cv$ci_hi, "]")),
      tags$small(cv$gap_short,
                 style = paste0("color:", cv$color, "; font-size:0.78rem"))
    )
  })

  
  # ── Badge helpers (actual vs expected %) ────────────────────────────────────
  make_labelled_badge <- function(actual, expected) {
    if (is.na(expected) || expected == 0) return(NULL)
    pct   <- round(100 * actual / expected)
    color <- if (pct >= 90) "#2e7a3a" else if (pct >= 70) "warning" else "danger"
    badge <- if (pct >= 90)
      tags$span(class = "badge",
                style = "background-color:#2e7a3a; color:#fff", paste0(pct, "%"))
    else
      tags$span(class = paste0("badge bg-", color), paste0(pct, "%"))
    tagList(badge,
            tags$span(style = "font-size:0.9rem; color:#888; margin-left:4px",
                      "of expected progeny"))
  }
  
  output$last_year_badge <- renderUI({
    req(pup_projection_data())
    row <- pup_projection_data() %>%
      filter(year == lubridate::year(Sys.Date()) - 1)
    if (nrow(row) == 0) return(NULL)
    make_labelled_badge(row$actual_pups, row$expected_pups)
  })
  
  output$this_year_badge <- renderUI({
    req(dam_schedule(), litter_stats())
    ls        <- litter_stats()
    today     <- Sys.Date()
    this_year <- lubridate::year(today)
    n_active  <- nrow(active_in_year(dam_schedule(), this_year))
    expected  <- round(n_active * ls$mean_litter_per_year * ls$mean_litter_size *
                         (1 - input$correction_pct / 100))
    pups_so_far <- rd$litterplan %>%
      filter(lubridate::year(dob) == this_year, dob <= today) %>%
      pull(Littersize) %>% sum(na.rm = TRUE)
    make_labelled_badge(pups_so_far, expected)
  })
  
  # ── This Year detailed card ──────────────────────────────────────────────────
  output$current <- renderUI({
    if (!breeders_col_specified()) return(
      tags$p(style = "color:#999; font-size:0.9rem",
             "Specify a breeding stock column to enable accurate projections."))
    req(dam_schedule(), litter_stats(), capacity_vals())
    ls        <- litter_stats()
    today     <- Sys.Date()
    this_year <- lubridate::year(today)
    cv        <- capacity_vals()
    n_active    <- nrow(active_in_year(dam_schedule(), this_year))
    exp_litters <- round(n_active * ls$mean_litter_per_year *
                           (1 - input$correction_pct / 100))
    exp_pups    <- round(exp_litters * ls$mean_litter_size)
    ci_lo       <- round(exp_litters * ls$ci_lower)
    ci_hi       <- round(exp_litters * ls$ci_upper)
    pups_so_far <- rd$litterplan %>%
      filter(lubridate::year(dob) == this_year, dob <= today) %>%
      pull(Littersize) %>% sum(na.rm = TRUE)
    
    tagList(
      tags$table(
        class = "table table-sm table-borderless mb-0",
        style = "font-size:0.92rem",
        tags$tbody(
          ly_row("Active or eligible dams",         n_active),
          ly_row("Expected litters this year",      exp_litters),
          ly_row("Expected progeny this year (95% CI)",
                 paste0(exp_pups, " [", ci_lo, "\u2013", ci_hi, "]")),
          ly_row("Progeny born so far",                pups_so_far),
          ly_row("Remaining expected",              max(0, exp_pups - pups_so_far))
        )
      ),
      tags$div(
        style = paste0(
          "margin-top:12px; padding:12px 16px; border-radius:6px; ",
          "background-color:", cv$color, "18; border-left:4px solid ", cv$color, ";"
        ),
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:baseline",
          tags$span("Capacity vs Goal",
                    style = "font-size:1.1rem; color:#555; font-weight:500"),
          tags$span(paste0(cv$pct, "%"),
                    style = paste0("font-size:1.5rem; font-weight:700; color:", cv$color))
        ),
        tags$div(cv$gap_short,
                 style = paste0("font-size:1rem; font-weight:600; color:", cv$color,
                                "; margin-top:4px"))
      )
    )
  })
}
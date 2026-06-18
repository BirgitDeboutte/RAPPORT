# ── management_server.R ───────────────────────────────────────────────────────
# Coordinator: owns all shared inputs/reactives and wires up the four panel
# helpers.  Each helper receives input/output/session from this scope so ALL
# output IDs and input IDs stay in the management module namespace.
# management_ui.R requires NO changes.

source("modules/management_pop_growth_server.R")
source("modules/management_dam_planner_server.R")
source("modules/management_pup_planner_server.R")
source("modules/management_breeder_selection_server.R")

managementServer <- function(id, rd) {
  moduleServer(id, function(input, output, session) {
    
    # ── Inter-module dependency note ──────────────────────────────────────────
    # Depends on rd$litterplan / rd$litterage from popOverviewLitterServer.
    # ─────────────────────────────────────────────────────────────────────────
    
    # ══════════════════════════════════════════════════════════════════════════
    # SHARED UTILITIES (passed to helpers as plain objects)
    # ══════════════════════════════════════════════════════════════════════════
    
    ly_row <- function(label, value) {
      tags$tr(style = "line-height:1.5",
              tags$td(style = "padding:5px 12px 5px 0; color:#444", label),
              tags$td(class = "text-end",
                      style = "padding:5px 0; font-weight:600; font-size:1rem", value))
    }
    
    data_var_classes <- reactive({
      req(rd$data)
      sapply(rd$data, class)
    })
    
    active_in_year <- function(sched, yr) {
      sched %>%
        filter(breed_start <= as.Date(paste0(yr, "-12-31")),
               breed_end   >= as.Date(paste0(yr, "-01-01")))
    }
    
    # ══════════════════════════════════════════════════════════════════════════
    # SHARED REACTIVES
    # Inputs consumed here:
    #   input$litdam         – max litters per dam        (Pop Growth UI)
    #   input$retire_col     – optional retirement column (Pop Growth UI)
    #   input$correction_pct  – litter correction %           (Pop Growth UI)
    #   input$goal           – desired yearly pup count   (Pop Growth UI)
    # ══════════════════════════════════════════════════════════════════════════
    
    breeders_col_specified <- reactive({
      req(rd$breeders, rd$data)
      !setequal(rd$breeders[[rd$datid]], rd$data[[rd$datid]])
    })
    
    # ── 1. retirement_dates ──────────────────────────────────────────────────
    retirement_dates <- reactive({
      req(rd$litterplan, rd$breeders, rd$data)
      litters_per_dam <- as.numeric(input$litdam)
      today           <- Sys.Date()
      
      inferred <- rd$litterplan %>%
        mutate(dob = as.Date(dob)) %>%
        group_by(Dam) %>%
        summarise(n_litters   = n(),
                  last_litter = max(dob),
                  .groups     = "drop") %>%
        filter(n_litters == litters_per_dam |
                 last_litter <= today %m-% years(2)) %>%
        mutate(inferred_retirement_date = pmax(
          if_else(n_litters == litters_per_dam, last_litter, as.Date(NA)),
          na.rm = TRUE
        )) %>%
        select(Dam, inferred_retirement_date)
      
      breeders_female_base <- rd$breeders %>%
        filter(.data[[rd$datsex]] == rd$datF) %>%
        select(Dam = !!sym(rd$datid), dobmom = !!sym(rd$datdob), everything()) %>%
        mutate(dobmom = as.Date(dobmom))
      
      actual_dam_ids <- unique(rd$litterplan$Dam)
      
      extra_dams <- rd$data %>%
        filter(
          .data[[rd$datid]] %in% actual_dam_ids,
          .data[[rd$datsex]] == rd$datF,
          !.data[[rd$datid]] %in% breeders_female_base$Dam
        ) %>%
        select(Dam = !!sym(rd$datid), dobmom = !!sym(rd$datdob), everything()) %>%
        mutate(dobmom = as.Date(dobmom))
      
      breeders_female <- bind_rows(breeders_female_base, extra_dams)
      
      # after
      if (!is.null(input$retire_col) && input$retire_col != "None" &&
          input$retire_col %in% names(breeders_female)) {
        breeders_female <- breeders_female %>%
          mutate(user_retirement_date =
                   parse_dates_robust(.data[[input$retire_col]]))
      } else {
        breeders_female$user_retirement_date <- NA
      }
      
      breeders_female %>%
        left_join(inferred, by = "Dam") %>%
        mutate(retirement_date = coalesce(user_retirement_date, inferred_retirement_date))
    })
    
    # ── 2. litter_stats ──────────────────────────────────────────────────────
    litter_stats <- reactive({
      req(rd$litterage, rd$litterplan, rd$ownpop)
      la <- rd$litterage
      
      mean_age_first <- as.numeric(la["AgeAtLitter_Litter1"])
      
      litter_keys <- names(la)[grepl("^AgeAtLitter_Litter", names(la))]
      litter_nums <- sort(as.integer(sub("AgeAtLitter_Litter", "", litter_keys)))
      intervals   <- numeric(0)
      for (i in seq_along(litter_nums[-1])) {
        a <- as.numeric(la[paste0("AgeAtLitter_Litter", litter_nums[i])])
        b <- as.numeric(la[paste0("AgeAtLitter_Litter", litter_nums[i + 1])])
        if (!is.na(a) && !is.na(b) && (b - a) > 0) intervals <- c(intervals, b - a)
      }
      mean_litter_interval <- if (length(intervals) > 0) mean(intervals) else NA_real_
      mean_litter_per_year <- if (!is.na(mean_litter_interval) && mean_litter_interval > 0)
        1 / mean_litter_interval else NA_real_
      
      own_ids  <- rd$ownpop[[rd$datid]]
      own_plan <- rd$litterplan[
        rd$litterplan$Dam %in% own_ids | rd$litterplan$Sire %in% own_ids, ]
      
      sizes            <- as.numeric(as.character(own_plan$Littersize))
      mean_litter_size <- mean(sizes, na.rm = TRUE)
      se_litter_size   <- sd(sizes, na.rm = TRUE) / sqrt(sum(!is.na(sizes)))
      
      list(
        mean_age_first       = mean_age_first,
        mean_litter_interval = mean_litter_interval,
        mean_litter_per_year = mean_litter_per_year,
        mean_litter_size     = mean_litter_size,
        ci_lower             = mean_litter_size - 1.96 * se_litter_size,
        ci_upper             = mean_litter_size + 1.96 * se_litter_size
      )
    })
    
    # ── 3. dam_schedule ──────────────────────────────────────────────────────
    dam_schedule <- reactive({
      req(retirement_dates(), litter_stats(), rd$litterplan, input$litdam)
      ls    <- litter_stats()
      today <- Sys.Date()
      
      litter_counts <- rd$litterplan %>%
        group_by(Dam) %>%
        summarise(n_litters   = n(),
                  last_litter = max(dob),
                  .groups     = "drop")
      
      retirement_dates() %>%
        left_join(litter_counts, by = "Dam") %>%
        mutate(
          n_litters   = replace_na(n_litters,   0),
          last_litter = replace_na(last_litter, as.Date(NA)),
          expected_last_litter = as.Date(case_when(
            n_litters > 0 ~
              last_litter +
              lubridate::dyears((input$litdam - n_litters) * ls$mean_litter_interval),
            n_litters == 0 &
              dobmom + lubridate::dyears(ls$mean_age_first) <= today ~
              today +
              lubridate::dyears((input$litdam - 1) * ls$mean_litter_interval),
            TRUE ~
              dobmom + lubridate::dyears(ls$mean_age_first) +
              lubridate::dyears((input$litdam - 1) * ls$mean_litter_interval)
          )),
          breed_start = as.Date(dobmom + lubridate::dyears(ls$mean_age_first)),
          breed_end   = as.Date(coalesce(retirement_date, expected_last_litter))
        )
    })
    
    # ── 3b. empirical_correction_pct ───────────────────────────────────────────
    # Computes historical litter correction rate from actual vs expected litters
    # across past years.  Used to auto-set the correction slider when the user
    # ticks "Calculate from historical data".
    empirical_correction_pct <- reactive({
      req(rd$litterplan, litter_stats(), retirement_dates())
      ls        <- litter_stats()
      this_year <- lubridate::year(Sys.Date())
      if (is.na(ls$mean_litter_per_year) || ls$mean_litter_per_year == 0) return(NULL)
      
      bf <- retirement_dates() %>%
        mutate(start_date = dobmom + lubridate::dyears(ls$mean_age_first))
      
      actual_by_year <- rd$litterplan %>%
        filter(Dam %in% bf$Dam) %>%                      
        mutate(year = lubridate::year(dob)) %>%
        filter(year < this_year) %>%
        group_by(year) %>%
        summarise(actual_litters = n(), .groups = "drop")
      
      if (nrow(actual_by_year) == 0) return(NULL)
      
      actual_by_year <- actual_by_year %>%
        mutate(
          n_active = sapply(year, function(y) {
            nrow(filter(bf,
                        start_date       <= as.Date(paste0(y, "-12-31")),
                        is.na(retirement_date) |
                          retirement_date > as.Date(paste0(y - 1, "-12-31"))))
          }),
          expected_litters = n_active * ls$mean_litter_per_year,
          correction_y        = 1 - actual_litters / expected_litters
        ) %>%
        filter(expected_litters > 0, is.finite(correction_y))
      
      if (nrow(actual_by_year) == 0) return(NULL)
      max(0, round(mean(actual_by_year$correction_y, na.rm = TRUE) * 100))
    })
    
    # When the checkbox is ticked, push the empirical value into the slider.
    observeEvent(input$use_empirical_correction, {
      req(isTRUE(input$use_empirical_correction))
      pct <- empirical_correction_pct()
      if (!is.null(pct))
        updateSliderInput(session, "correction_pct", value = pct)
    })
    
    # ── 4. pup_projection_data ───────────────────────────────────────────────
    # Covers historical years with recorded litters only.
    # For "this year" projections use capacity_vals() below.
    pup_projection_data <- reactive({
      req(rd$litterplan, litter_stats(), retirement_dates())
      ls <- litter_stats()
      
      breeders_female <- retirement_dates() %>%
        mutate(start_breeding_date = dobmom + lubridate::dyears(ls$mean_age_first))
      
      actual_pups <- rd$litterplan %>%
        mutate(year = lubridate::year(dob)) %>%
        group_by(year) %>%
        summarise(actual_pups = sum(Littersize, na.rm = TRUE), .groups = "drop")
      
      years <- sort(unique(actual_pups$year))
      
      active_dams_per_year <- sapply(years, function(y) {
        breeders_female %>%
          filter(
            start_breeding_date <= as.Date(paste0(y, "-12-31")),
            is.na(retirement_date) |
              retirement_date > as.Date(paste0(y - 1, "-12-31"))
          ) %>%
          nrow()
      })
      
      this_year <- lubridate::year(Sys.Date())
      expected_pups <- data.frame(
        year          = years,
        expected_pups = round(ls$mean_litter_size *
                                ls$mean_litter_per_year *
                                active_dams_per_year *
                                ifelse(years < this_year, 1, 1 - input$correction_pct / 100)),
        expected_moms = active_dams_per_year
      )
      
      actual_dams <- rd$litterplan %>%
        mutate(year = lubridate::year(dob)) %>%
        group_by(year) %>%
        summarise(actual_moms = n_distinct(Dam), .groups = "drop")
      
      full_join(expected_pups, actual_pups,  by = "year") %>%
        full_join(actual_dams, by = "year") %>%
        replace_na(list(expected_pups = 0, actual_pups = 0,
                        actual_moms   = 0, expected_moms = 0))
    })
    
    observe({
      req(pup_projection_data(), litter_stats())
      rd$expected_litters <-
        pup_projection_data()$expected_pups / litter_stats()$mean_litter_size
    })
    
    # ── 5. capacity_vals ─────────────────────────────────────────────────────
    # Single source of truth for "expected progeny this year".
    # Used by BOTH vb_capacity (Pop Growth) AND vb_pups_projected (Pup Planner),
    # guaranteeing they always show the same number.
    capacity_vals <- reactive({
      req(dam_schedule(), litter_stats(), input$goal)
      ls <- litter_stats()
      
      if (is.na(ls$mean_litter_per_year) || is.na(ls$mean_litter_size)) {
        return(list(pct = NA, color = "grey",
                    gap_short = "Cannot compute \u2014 no dams with 2+ litters yet",
                    capacity = NA, ci_lo = NA, ci_hi = NA))
      }
      
      this_year   <- lubridate::year(Sys.Date())
      n_active    <- nrow(active_in_year(dam_schedule(), this_year))
      exp_litters <- round(n_active * ls$mean_litter_per_year *
                             (1 - input$correction_pct / 100))
      capacity    <- round(exp_litters * ls$mean_litter_size)
      ci_lo       <- round(exp_litters * ls$ci_lower)
      ci_hi       <- round(exp_litters * ls$ci_upper)
      pct         <- if (input$goal > 0) round(100 * capacity / input$goal) else 0
      gap         <- capacity - input$goal
      color       <- if (pct >= 100) "#2e7a3a" else if (pct >= 80) "#e65100" else "#c62828"
      gap_short   <- if (gap >= 0) {
        paste0("+", gap, " progeny above goal")
      } else {
        extra_dams    <- ceiling(abs(gap) / (ls$mean_litter_per_year * ls$mean_litter_size))
        extra_litters <- ceiling(abs(gap) / ls$mean_litter_size)
        paste0(abs(gap), " progeny short \u2192 ", extra_litters,
               " more litter(s) / ", extra_dams, " more dam(s) needed")
      }
      list(pct = pct, color = color, gap_short = gap_short,
           capacity = capacity, ci_lo = ci_lo, ci_hi = ci_hi)
    })
    
    # ══════════════════════════════════════════════════════════════════════════
    # CALL PANEL HELPERS
    # ══════════════════════════════════════════════════════════════════════════
    
    managementPopGrowthHelper(
      input, output, session, rd,
      litter_stats              = litter_stats,
      retirement_dates          = retirement_dates,
      dam_schedule              = dam_schedule,
      pup_projection_data       = pup_projection_data,
      capacity_vals             = capacity_vals,
      breeders_col_specified    = breeders_col_specified,
      active_in_year            = active_in_year,
      ly_row                    = ly_row,
      empirical_correction_pct   = empirical_correction_pct
    )
    
    managementDamPlannerHelper(
      input, output, session, rd,
      litter_stats           = litter_stats,
      dam_schedule           = dam_schedule,
      breeders_col_specified = breeders_col_specified,
      active_in_year         = active_in_year
    )
    
    managementPupPlannerHelper(
      input, output, session, rd,
      litter_stats           = litter_stats,
      retirement_dates       = retirement_dates,
      pup_projection_data    = pup_projection_data,
      capacity_vals          = capacity_vals,
      breeders_col_specified = breeders_col_specified
    )
    
    managementBreederSelectionHelper(
      input, output, session, rd,
      litter_stats           = litter_stats,
      retirement_dates       = retirement_dates,
      breeders_col_specified = breeders_col_specified,
      data_var_classes       = data_var_classes
    )
  })
}
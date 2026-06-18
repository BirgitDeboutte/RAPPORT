# ── management_breeder_selection_server.R ────────────────────────────────────
# Panel helper for the "Breeder Selection" tab.


managementBreederSelectionHelper <- function(
    input, output, session, rd,
    litter_stats,
    retirement_dates,
    breeders_col_specified,
    data_var_classes
) {
  ns <- session$ns
  
  # ── Ne status for guidance text ───────────────────────────────────────────
  ne_status <- reactive({
    ne <- rd$Ne
    if (is.null(ne) || !is.finite(ne) || ne <= 0)
      return(list(status = "unknown", ne = NA_real_))
    list(
      status = if (ne < 50) "critical" else if (ne < 100) "vulnerable" else if (ne <= 200) "acceptable" else "adequate",
      ne     = ne
    )
  })
  
  # ── Tertile cutoffs from whole population ───────────────────────
  mk_tertiles <- reactive({
    req(rd$kinship, rd$ownpop, rd$datid)
    K       <- rd$kinship
    pop_ids <- intersect(as.character(rd$ownpop[[rd$datid]]), rownames(K))
    if (length(pop_ids) < 3) return(NULL)
    mk_pop <- vapply(pop_ids, function(id)
      mean(K[id, pop_ids], na.rm = TRUE), numeric(1))
    list(p33 = unname(quantile(mk_pop, 1/3)),
         p67 = unname(quantile(mk_pop, 2/3)))
  })
  output$kinship_constraint_panel <- renderUI({
    req(rd$kinship)
    
    ns_val  <- ne_status()
    ne_disp <- if (is.finite(ns_val$ne)) round(ns_val$ne, 1) else "N/A"
    
    si <- switch(ns_val$status,
                 critical   = list(color = "#c62828", bg = "#ffebee",
                                   icon = "\U0001F534", label = "CRITICAL"),
                 vulnerable = list(color = "#e65100", bg = "#fff8e1",
                                   icon = "\U0001F7E0", label = "AT RISK"),
                 acceptable = list(color = "#558b2f", bg = "#f1f8e9",
                                   icon = "\U0001F7E2", label = "ACCEPTABLE"),
                 adequate   = list(color = "#2e7a3a", bg = "#e8f5e9",
                                   icon = "\U0001F7E2", label = "ADEQUATE"),
                 list(color = "#555",    bg = "#f5f5f5",
                      icon = "\u26AA",    label = "UNKNOWN")
    )
    
    # Tertile info — only available after "Show Potential Breeders" is clicked
    tert   <- mk_tertiles()
    tert_note <- if (!is.null(tert))
      tagList(
        tags$div(
          style = paste0("display:flex; gap:8px; margin-top:10px; ",
                         "font-size:0.78rem; align-items:center"),
          tags$span(
            style = paste0("background:#e8f5e9; border:1px solid #2e7a3a; ",
                           "border-radius:4px; padding:3px 8px; color:#2e7a3a; font-weight:700"),
            paste0("Green \u2264 ", round(tert$p33, 4))
          ),
          tags$span(style = "color:#aaa", "\u2192"),
          tags$span(
            style = paste0("background:#fff8e1; border:1px solid #e65100; ",
                           "border-radius:4px; padding:3px 8px; color:#e65100; font-weight:700"),
            paste0("Orange \u2264 ", round(tert$p67, 4))
          ),
          tags$span(style = "color:#aaa", "\u2192"),
          tags$span(
            style = paste0("background:#ffebee; border:1px solid #c62828; ",
                           "border-radius:4px; padding:3px 8px; color:#c62828; font-weight:700"),
            paste0("Red > ", round(tert$p67, 4))
          )
        )
      )
    else
      tags$p(style = "font-size:0.78rem; color:#999; margin-top:8px; font-style:italic",
             "Colour bands will appear after clicking \u2018Show Potential Breeders\u2019.")
    
    # Ne-status-specific guidance
    guidance <- switch(ns_val$status,
                       critical = tags$div(
                         style = paste0("padding:10px 12px; background:#ffebee; border-radius:4px; ",
                                        "font-size:0.83rem; color:#c62828; margin-top:10px"),
                         tags$b("\U0001F534 Critical (N\u2091 = ", ne_disp, "): "),
                         "Every new breeder should ideally be green. Avoid red candidates entirely. ",
                         "The population is losing genetic diversity at a rate that puts it at short-term ",
                         "risk \u2014 low-kinship recruits are the primary solution."
                       ),
                       vulnerable = tags$div(
                         style = paste0("padding:10px 12px; background:#fff8e1; border-radius:4px; ",
                                        "font-size:0.83rem; color:#e65100; margin-top:10px"),
                         tags$b("\U0001F7E0 At risk (N\u2091 = ", ne_disp, "): "),
                         "Strongly prefer green candidates. Orange is acceptable if health or ",
                         "phenotypic criteria cannot otherwise be met. Avoid selecting two red animals ",
                         "in the same recruitment round, and never pair red \u00d7 red or ",
                         "red \u00d7 orange in mating decisions."
                       ),
                       acceptable = tags$div(
                         style = paste0("padding:10px 12px; background:#f1f8e9; border-radius:4px; ",
                                        "font-size:0.83rem; color:#558b2f; margin-top:10px"),
                         tags$b("\U0001F7E2 Acceptable (N\u2091 = ", ne_disp, "): "),
                         "Population size exceeds the minimum threshold but has not yet reached the ",
                         "adequate level (N\u2091 > 200). Prefer green candidates to continue building ",
                         "diversity. Orange is acceptable if health or phenotypic requirements take priority. ",
                         "Monitor regularly and aim to increase N\u2091 over successive generations."
                       ),
                       adequate = tags$div(
                         style = paste0("padding:10px 12px; background:#e8f5e9; border-radius:4px; ",
                                        "font-size:0.83rem; color:#2e7a3a; margin-top:10px"),
                         tags$b("\U0001F7E2 Adequate (N\u2091 = ", ne_disp, "): "),
                         "Green candidates are preferred and bring the most novel genetics. ",
                         "Orange and red animals are acceptable if they meet your health and ",
                         "phenotypic criteria \u2014 red simply means their genetics are already well ",
                         "represented in the population, not that they are poor breeders. ",
                         "Continue monitoring to maintain this status."
                       ),
                       tags$div(
                         style = paste0("padding:10px 12px; background:#f5f5f5; border-radius:4px; ",
                                        "font-size:0.83rem; color:#555; margin-top:10px"),
                         "N\u2091 could not be estimated \u2014 pedigree may be too shallow. ",
                         "Use the colour bands as a guide: prefer green and orange candidates."
                       )
    )
    
    card(
      card_header(
        layout_columns(
          tags$span(
            tags$span(si$icon, style = "margin-right:5px"),
            tags$b("Mean Kinship \u2014 Genetic Diversity Indicator"),
            style = "font-size:0.95rem"
          ),
          div(style = "text-align:right",
              tags$span(
                style = paste0(
                  "font-size:0.76rem; font-weight:700; color:", si$color,
                  "; text-transform:uppercase; letter-spacing:0.05em;",
                  " background:", si$bg, "; padding:2px 8px; border-radius:4px"
                ),
                paste0("N\u2091 = ", ne_disp, "  \u00b7  ", si$label)
              )
          ),
          col_widths = c(8, 4)
        )
      ),
      card_body(
        layout_columns(
          # Left: what the colours mean + tertile legend
          tags$div(
            tags$p(style = "margin-bottom:6px; font-weight:700; color:#517066",
                   "\U0001F9EC What is mean kinship?"),
            tags$p(style = "font-size:0.83rem; color:#333; margin-bottom:6px",
                   "Mean kinship is the average relatedness of a candidate to your ",
                   "entire own population. It measures how much novel genetic material ",
                   "a candidate brings: a ", tags$b("lower value"), " means their genes are ",
                   "rare in the population and genetically valuable; a ",
                   tags$b("higher value"), " means their genes are already well represented ",
                   "and selecting them adds little diversity."
            ),
            tags$p(style = "font-size:0.83rem; color:#333; margin-bottom:4px",
                   "Candidates are colour-coded into three equal groups (tertiles) ",
                   "relative to the current candidate pool, sorted low-to-high so the ",
                   "most genetically valuable candidates appear first:"
            ),
            tags$ul(
              style = "font-size:0.82rem; padding-left:18px; margin-bottom:6px",
              tags$li(
                tags$span(style = "color:#2e7a3a; font-weight:700", "Green"),
                " \u2014 bottom third: genetically distinct, prioritise these"
              ),
              tags$li(
                tags$span(style = "color:#e65100; font-weight:700", "Orange"),
                " \u2014 middle third: average relatedness, acceptable"
              ),
              tags$li(
                tags$span(style = "color:#c62828; font-weight:700", "Red"),
                " \u2014 top third: most related to population, use cautiously"
              )
            ),
            tert_note
          ),
          # Right: Ne-driven guidance + checkbox
          tags$div(
            guidance,
            tags$div(
              style = "margin-top:14px",
              checkboxInput(
                ns("filter_by_kinship"),
                label = tagList(
                  "Show only green and orange candidates",
                  tags$span(
                    style = "font-size:0.78rem; color:#888; display:block; margin-top:2px",
                    "Hides the top third (red). Health criteria should be applied first ",
                    "(a red candidate who meets all health requirements is not a poor choice,",
                    "just something to take into account)"
                  )
                ),
                value = FALSE
              )
            )
          ),
          col_widths = c(6, 6)
        )
      )
    )
  })
  
  # ── Warning ──────────────────────────────────────────────────────────────────
  
  output$no_breeders_warning_bs <- renderUI({
    if (breeders_col_specified()) return(NULL)
    tags$div(
      class = "alert alert-warning",
      style = "font-size:0.9rem; padding:10px 14px; margin-bottom:12px",
      tags$b("\u26a0 No breeding stock column specified. "),
      "Value boxes and recruitment status require a designated breeding stock column. ",
      "Please specify this in the Data Input tab."
    )
  })
  
  # ── Variable selector ─────────────────────────────────────────────────────────
  
  output$goal_var_selector <- renderUI({
    req(rd$data, data_var_classes())
    vc      <- data_var_classes()
    choices <- colnames(rd$data)[!colnames(rd$data) %in% c(rd$datid, rd$datsex, rd$datdob)]
    selectInput(ns("goal_vars"), "Variables",
                choices  = setNames(choices, paste0(choices, " (", vc[choices], ")")),
                selected = NULL,
                multiple = TRUE)
  })
  
  # ── Per-variable criteria inputs ──────────────────────────────────────────────
  
  output$goal_criteria_inputs <- renderUI({
    req(input$goal_vars, rd$data, data_var_classes())
    vc <- data_var_classes()
    
    lapply(input$goal_vars, function(var_name) {
      var_class   <- vc[var_name]
      dir_choices <- if (var_class %in% c("character", "factor"))
        c("No Goal", "=", "\u2260")
      else
        c("No Goal", "\u2264", "\u2265", "=", "\u2260")
      
      existing <- if (!is.null(rd$param_goals) && nrow(rd$param_goals) > 0)
        rd$param_goals[rd$param_goals$variable == var_name, ]
      else
        data.frame()
      
      pre_dir <- if (nrow(existing) > 0) existing$direction[1] else "No Goal"
      pre_val <- if (nrow(existing) > 0) existing$value[1]     else NULL
      pre_pri <- if (nrow(existing) > 0) existing$priority[1]  else "Mandatory"
      
      tags$div(
        style = paste0("border:1px solid #e0e0e0; border-radius:6px; ",
                       "padding:10px 14px; margin-bottom:8px; background:#fafafa"),
        tags$div(
          style = paste0("font-size:0.82rem; font-weight:700; text-transform:uppercase; ",
                         "letter-spacing:0.06em; color:#555; margin-bottom:8px"),
          paste0(var_name, "  (", var_class, ")")
        ),
        layout_columns(
          selectInput(ns(paste0("goal_dir_", var_name)),
                      "Direction", choices = dir_choices, selected = pre_dir),
          conditionalPanel(
            condition = paste0("input['", ns(paste0("goal_dir_", var_name)), "'] !== 'No Goal'"),
            selectizeInput(ns(paste0("goal_val_", var_name)), "Target Value",
                           choices  = sort(unique(rd$data[[var_name]])),
                           selected = pre_val,
                           options  = list(create = TRUE))
          ),
          selectInput(ns(paste0("goal_pri_", var_name)), "Priority",
                      choices = c("Mandatory", "Preferred"), selected = pre_pri),
          col_widths = c(3, 6, 3)
        )
      )
    })
  })
  
  # ── Current goals reactive ────────────────────────────────────────────────────
  
  current_goals <- reactive({
    req(rd$data, data_var_classes())
    if (is.null(input$goal_vars) || length(input$goal_vars) == 0)
      return(data.frame(variable = character(), var_class = character(),
                        direction = character(), value = character(),
                        priority = character(), stringsAsFactors = FALSE))
    vc <- data_var_classes()
    goals <- lapply(input$goal_vars, function(var_name) {
      dir <- input[[paste0("goal_dir_", var_name)]]
      if (is.null(dir) || length(dir) == 0 || dir == "No Goal") return(NULL)
      val <- input[[paste0("goal_val_", var_name)]]
      if (is.null(val) || length(val) == 0) return(NULL)
      pri <- input[[paste0("goal_pri_", var_name)]]
      if (is.null(pri) || length(pri) == 0) return(NULL)
      data.frame(variable = var_name, var_class = as.character(vc[[var_name]]),
                 direction = as.character(dir), value = as.character(val),
                 priority = as.character(pri), stringsAsFactors = FALSE)
    })
    result <- do.call(rbind, Filter(Negate(is.null), goals))
    if (is.null(result))
      data.frame(variable = character(), var_class = character(),
                 direction = character(), value = character(),
                 priority = character(), stringsAsFactors = FALSE)
    else result
  })
  
  observeEvent(input$save_goals, {
    rd$param_goals <- current_goals()
    output$goals_saved_alert <- renderUI({
      n <- if (!is.null(rd$param_goals)) nrow(rd$param_goals) else 0L
      tags$div(class = "alert alert-success mt-2 mb-0",
               style = "font-size:0.9rem; padding:8px 12px",
               tags$strong("\u2714 Saved: "),
               paste0(n, " goal(s) active \u2014 also applied in Ranked Mating"))
    })
  })
  
  observeEvent(input$clear_goals, {
    rd$param_goals <- data.frame(
      variable = character(), var_class = character(),
      direction = character(), value = character(),
      priority = character(), stringsAsFactors = FALSE
    )
    output$goals_saved_alert <- renderUI({
      tags$div(class = "alert alert-warning mt-2 mb-0",
               style = "font-size:0.9rem; padding:8px 12px",
               tags$strong("\u21b6 Cleared all goals"))
    })
  })
  
  # ── Date range ────────────────────────────────────────────────────────────────
  
  output$breedertimerange <- renderUI({
    req(rd$merged)
    date_range  <- range(as.Date(rd$merged[[rd$datdob]]), na.rm = TRUE)
    default_min <- max(date_range[1], min(date_range[2], Sys.Date() - lubridate::years(2)))
    default_max <- date_range[2]
    sliderInput(ns("date_range"), "Birth Year Range:",
                min        = date_range[1],
                max        = date_range[2],
                value      = c(default_min, default_max),
                timeFormat = "%Y-%m-%d")
  })
  
  ranktab <- reactiveVal(NULL)
  displayed_ranktab <- reactiveVal(NULL) 
  newbr   <- reactiveVal(NULL)
  
  # ── Show Potential Breeders ───────────────────────────────────────────────────
  
  observeEvent(input$breedersbutton, {
    req(rd$ped, rd$data, input$date_range)
    
    goal_vars <- if (!is.null(input$goal_vars) && length(input$goal_vars) > 0)
      input$goal_vars else character(0)
    
    stat_data <- as.data.frame(rd$data)
    tab <- stat_data[
      stat_data[[rd$datdob]] >= input$date_range[1] &
        stat_data[[rd$datdob]] <= input$date_range[2],
      c(rd$datid, rd$datsex, rd$datdob, goal_vars),
      drop = FALSE
    ]
    
    goals <- current_goals()
    if (!is.null(goals) && nrow(goals) > 0) {
      for (i in seq_len(nrow(goals))) {
        g <- goals[i, ]
        if (g$priority != "Mandatory")       next
        if (!g$variable %in% colnames(tab))  next
        tab <- tab[apply_goal_filter(tab[[g$variable]], g$direction, g$value), , drop = FALSE]
      }
      for (i in seq_len(nrow(goals))) {
        g <- goals[i, ]
        if (g$priority != "Preferred")       next
        if (!g$variable %in% colnames(tab))  next
        tab[[paste0(".pref_fail_", g$variable)]] <-
          !apply_goal_filter(tab[[g$variable]], g$direction, g$value)
      }
      pref_cols <- grep("^\\.pref_fail_", colnames(tab), value = TRUE)
      if (length(pref_cols) > 0) {
        n_fails          <- rowSums(tab[, pref_cols, drop = FALSE])
        tab              <- tab[order(n_fails, na.last = TRUE), ]
        tab[, pref_cols] <- NULL
      }
    }
    
    opts <- input$toggle_sex
    if ("Show Sires" %in% opts && !"Show Dams" %in% opts)
      tab <- tab[tab[[rd$datsex]] == rd$datM, ]
    else if ("Show Dams" %in% opts && !"Show Sires" %in% opts)
      tab <- tab[tab[[rd$datsex]] == rd$datF, ]
    
    if (breeders_col_specified()) {
      existing_ids <- rd$breeders[[rd$datid]]
      tab <- tab[!tab[[rd$datid]] %in% existing_ids, ]
    }
    
    # ── Mean kinship to own population ────────────────────────────────────────
    if (!is.null(rd$kinship) && nrow(tab) > 0) {
      K       <- rd$kinship
      ref_ids <- intersect(as.character(rd$ownpop[[rd$datid]]), rownames(K))
      
      tab$mean_kinship <- vapply(as.character(tab[[rd$datid]]), function(cid) {
        if (!cid %in% rownames(K) || length(ref_ids) == 0) return(NA_real_)
        mean(K[cid, ref_ids], na.rm = TRUE)
      }, numeric(1))
      
      id_sex_dob <- intersect(c(rd$datid, rd$datsex, rd$datdob), names(tab))
      rest_cols  <- setdiff(names(tab), c(id_sex_dob, "mean_kinship"))
      tab        <- tab[, c(id_sex_dob, "mean_kinship", rest_cols), drop = FALSE]
    }
    
    ranktab(tab)
  })
  
  # ── Compliance reactives ──────────────────────────────────────────────────────
  
  goal_compliance <- reactive({
    req(rd$breeders, rd$param_goals)
    goals <- rd$param_goals
    if (is.null(goals) || nrow(goals) == 0) return(NULL)
    breeders <- as.data.frame(rd$breeders)
    n_total  <- nrow(breeders)
    rows <- lapply(seq_len(nrow(goals)), function(i) {
      g      <- goals[i, ]
      col    <- breeders[[g$variable]]
      if (is.null(col)) return(NULL)
      passes <- apply_goal_filter(col, g$direction, g$value)
      data.frame(variable = g$variable, direction = g$direction, value = g$value,
                 priority = g$priority, n_pass = sum(passes, na.rm = TRUE),
                 n_total  = n_total,
                 pct = if (n_total > 0) round(100 * sum(passes, na.rm = TRUE) / n_total) else 0L,
                 stringsAsFactors = FALSE)
    })
    do.call(rbind, Filter(Negate(is.null), rows))
  })
  
  output$vb_breeders_passing <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    comp <- goal_compliance()
    req(comp, rd$breeders)
    mand <- comp[comp$priority == "Mandatory", ]
    if (nrow(mand) == 0) return(h3(nrow(rd$breeders)))
    breeders <- as.data.frame(rd$breeders)
    mask     <- rep(TRUE, nrow(breeders))
    for (i in seq_len(nrow(mand))) {
      g    <- mand[i, ]
      mask <- mask & apply_goal_filter(breeders[[g$variable]], g$direction, g$value)
    }
    n     <- sum(mask, na.rm = TRUE)
    total <- nrow(breeders)
    pct   <- round(100 * n / total)
    color <- if (pct >= 80) "#2e7a3a" else if (pct >= 50) "#e65100" else "#c62828"
    tagList(
      h3(n, style = paste0("color:", color)),
      tags$small(paste0(pct, "% of ", total),
                 style = paste0("color:", color, "; font-size:0.78rem; font-weight:500"))
    )
  })
  
  # ── Compliance rank-order drag list ───────────────────────────────────────────
  
  output$compliance_rank_order_ui <- renderUI({
    req(rd$param_goals)
    goals <- rd$param_goals
    if (nrow(goals) == 0) return(NULL)
    tagList(
      tags$div(
        style = paste0("font-size:0.82rem; font-weight:700; text-transform:uppercase; ",
                       "letter-spacing:0.06em; color:#555; margin-bottom:6px"),
        bsicons::bs_icon("grip-vertical"),
        tags$span("Ranking Priority", style = "margin-left:4px"),
        tags$span(
          "\u2014 drag to reorder \u2022 direction from saved goal",
          style = paste0("font-size:0.75rem; font-weight:400; color:#999; ",
                         "text-transform:none; letter-spacing:0; margin-left:6px")
        )
      ),
      sortable::rank_list(
        text     = NULL,
        labels   = goals$variable,
        input_id = ns("compliance_rank_order"),
        class    = "default-sortable"
      )
    )
  })
  
  # ── Compliance DT ─────────────────────────────────────────────────────────────
  
  output$compliance_table <- DT::renderDataTable({
    if (!breeders_col_specified()) return(datatable(
      data.frame(Message = "Specify a breeding stock column to enable compliance tracking."),
      rownames = FALSE, options = list(dom = "t")))
    req(rd$param_goals, rd$breeders, data_var_classes())
    goals <- rd$param_goals
    if (is.null(goals) || nrow(goals) == 0)
      return(datatable(data.frame(Message = "No goals saved yet."),
                       rownames = FALSE, options = list(dom = "t")))
    
    vars_present <- goals$variable[goals$variable %in% names(rd$breeders)]
    if (length(vars_present) == 0)
      return(datatable(data.frame(Message = "Goal variables not found in breeders data."),
                       rownames = FALSE, options = list(dom = "t")))
    
    df <- as.data.frame(rd$breeders)[, c(rd$datid, vars_present), drop = FALSE]
    
    # ── Ranking ───────────────────────────────────────────────────────────────
    rank_order <- if (!is.null(input$compliance_rank_order) &&
                      length(input$compliance_rank_order) > 0)
      input$compliance_rank_order
    else
      vars_present
    
    vc        <- data_var_classes()
    sort_keys <- list()
    for (var_name in rank_order) {
      if (!var_name %in% colnames(df))          next
      g_dir <- goals$direction[goals$variable == var_name]
      if (length(g_dir) == 0)                   next
      if (!g_dir[1] %in% c("\u2264", "\u2265")) next
      if (!vc[var_name] %in% c("numeric", "integer")) next
      col <- df[[var_name]]
      sort_keys[[length(sort_keys) + 1]] <-
        if (g_dir[1] == "\u2265") -col else col
    }
    if (length(sort_keys) > 0)
      df <- df[do.call(order, c(sort_keys, list(na.last = TRUE))), ]
    
    # ── fake_input for transform_to_delta ────────────────────────────────────
    fake_input <- list()
    for (i in seq_len(nrow(goals))) {
      g      <- goals[i, ]
      longfm <- dir_to_long[g$direction]
      if (!is.na(longfm)) {
        fake_input[[paste0("crit_",  g$variable)]] <- longfm
        fake_input[[paste0("value_", g$variable)]] <- g$value
      }
    }
    
    # ── Per-variable goal reference values (numeric only) ────────────────────
    ref_vals <- setNames(
      lapply(vars_present, function(v) {
        gv <- goals$value[goals$variable == v]
        if (length(gv) == 0) return(NULL)
        if (vc[[v]] %in% c("numeric", "integer")) as.numeric(gv[1]) else NULL
      }),
      vars_present
    )
    ref_vals <- Filter(Negate(is.null), ref_vals)
    
    df <- transform_to_delta(df, vars_present, fake_input,
                             as.data.frame(rd$data), data_var_classes(),
                             ref_vals  = if (length(ref_vals) > 0) ref_vals else NULL,
                             ref_label = "goal")
    make_delta_datatable(df, vars_present, selection = "none")
  })
  
  # ── Potential breeders table ──────────────────────────────────────────────────
  
  output$breederselection <- renderDataTable({
    req(ranktab())
    
    goal_vars  <- if (!is.null(input$goal_vars) && length(input$goal_vars) > 0)
      input$goal_vars else character(0)
    goals      <- rd$param_goals
    fake_input <- list()
    if (!is.null(goals) && nrow(goals) > 0) {
      for (i in seq_len(nrow(goals))) {
        g      <- goals[i, ]
        longfm <- dir_to_long[g$direction]
        if (!is.na(longfm)) {
          fake_input[[paste0("crit_",  g$variable)]] <- longfm
          fake_input[[paste0("value_", g$variable)]] <- g$value
        }
      }
    }
    
    tab     <- ranktab()
    tert    <- mk_tertiles()
    has_kin <- "mean_kinship" %in% names(tab)
    
    if (isTRUE(input$filter_by_kinship) && has_kin && !is.null(tert))
      tab <- tab[is.na(tab$mean_kinship) | tab$mean_kinship <= tert$p67, , drop = FALSE]
    
    if (has_kin)
      tab <- tab[order(tab$mean_kinship, na.last = TRUE), , drop = FALSE]
    
    if (has_kin)
      names(tab)[names(tab) == "mean_kinship"] <- "Mean Kinship"
    
    displayed_ranktab(tab)
    
    df <- transform_to_delta(tab, goal_vars, fake_input, rd$data, data_var_classes())
    dt <- make_delta_datatable(df, goal_vars, selection = "multiple")
    
    if ("Mean Kinship" %in% names(df)) {
      dt <- dt %>% DT::formatRound("Mean Kinship", digits = 4)
      if (!is.null(tert)) {
        dt <- dt %>% DT::formatStyle(
          "Mean Kinship",
          backgroundColor = DT::styleInterval(
            c(tert$p33, tert$p67),
            c("#e8f5e9", "#fff8e1", "#ffebee")
          ),
          fontWeight = "bold"
        )
      }
    }
    
    dt
  })
  
  selected_breeders <- reactive({
    rows <- input$breederselection_rows_selected
    if (is.null(rows) || length(rows) == 0) return(NULL)
    rt <- displayed_ranktab()
    if (is.null(rt)) return(NULL)
    rt[rows, , drop = FALSE]
  })
  
  output$selected_new_breeders <- renderDataTable({
    sel <- selected_breeders()
    df  <- if (is.null(sel)) data.frame() else sel
    
    goal_vars  <- if (!is.null(input$goal_vars) && length(input$goal_vars) > 0)
      input$goal_vars else character(0)
    goals      <- rd$param_goals
    fake_input <- list()
    if (!is.null(goals) && nrow(goals) > 0) {
      for (i in seq_len(nrow(goals))) {
        g      <- goals[i, ]
        longfm <- dir_to_long[g$direction]
        if (!is.na(longfm)) {
          fake_input[[paste0("crit_",  g$variable)]] <- longfm
          fake_input[[paste0("value_", g$variable)]] <- g$value
        }
      }
    }
    
    df <- transform_to_delta(df, goal_vars, fake_input, rd$data, data_var_classes())
    make_delta_datatable(df, goal_vars, selection = "single")
  })
  
  output$convert_to_breed <- renderUI({
    req(selected_breeders())
    tagList(
      actionButton(ns("convert_button"), "Convert To Breeders", class = "btn btn-primary"),
      actionButton(ns("undo_convert"),   "Undo",                class = "btn btn-warning")
    )
  })
  
  observeEvent(input$convert_button, {
    req(selected_breeders(), rd$breeders, rd$data)
    new_ids <- selected_breeders()[[rd$datid]][
      !selected_breeders()[[rd$datid]] %in% rd$breeders[[rd$datid]]
    ]
    if (length(new_ids) == 0) return()
    newbr(new_ids)
    rd$breeders <- rbind(rd$breeders,
                         rd$data[rd$data[[rd$datid]] %in% new_ids, , drop = FALSE])
    output$breeders_converted <- renderUI({
      tags$div(class = "alert alert-success mt-2 mb-0",
               style = "font-size:0.9rem; padding:8px 12px",
               tags$strong("\u2714 Added: "), paste(new_ids, collapse = ", "))
    })
  })
  
  observeEvent(input$undo_convert, {
    req(newbr(), rd$breeders)
    rd$breeders <- rd$breeders[!rd$breeders[[rd$datid]] %in% newbr(), , drop = FALSE]
    output$breeders_converted <- renderUI({
      tags$div(class = "alert alert-warning mt-2 mb-0",
               style = "font-size:0.9rem; padding:8px 12px",
               tags$strong("\u21b6 Removed: "), paste(newbr(), collapse = ", "))
    })
  })
  
  # ── Recruitment counts + same-litter warning ──────────────────────────────────
  
  recruitment_counts <- reactive({
    req(rd$breeders)
    last_year  <- lubridate::year(Sys.Date()) - 1
    dams_back  <- rd$breeders %>%
      filter(.data[[rd$datsex]] == rd$datF,
             lubridate::year(as.Date(.data[[rd$datdob]])) == last_year) %>% nrow()
    sires_back <- rd$breeders %>%
      filter(.data[[rd$datsex]] == rd$datM,
             lubridate::year(as.Date(.data[[rd$datdob]])) == last_year) %>% nrow()
    sel       <- selected_breeders()
    dams_sel  <- if (!is.null(sel)) sum(sel[[rd$datsex]] == rd$datF, na.rm = TRUE) else 0L
    sires_sel <- if (!is.null(sel)) sum(sel[[rd$datsex]] == rd$datM, na.rm = TRUE) else 0L
    list(last_year = last_year, dams_back = dams_back, sires_back = sires_back,
         dams_sel = dams_sel, sires_sel = sires_sel)
  })
  
  same_litter_warning <- reactive({
    sel <- selected_breeders()
    if (is.null(sel) || nrow(sel) < 2) return(NULL)
    req(rd$ped)
    ped     <- as.data.frame(rd$ped)
    ped_low <- tolower(colnames(ped))
    id_idx  <- which(ped_low %in% c("indiv", "id", "animal", "ind", tolower(rd$datid)))[1]
    dam_idx <- which(ped_low %in% c("dam", "mother", "mum", "moeder", "daam", "mere"))[1]
    if (is.na(id_idx) || is.na(dam_idx)) return(NULL)
    id_col  <- colnames(ped)[id_idx]
    dam_col <- colnames(ped)[dam_idx]
    sel_info <- sel %>%
      transmute(id = .data[[rd$datid]], sex = .data[[rd$datsex]],
                dob = as.Date(.data[[rd$datdob]])) %>%
      left_join(ped[, c(id_col, dam_col)] %>%
                  rename(id = !!sym(id_col), dam_of = !!sym(dam_col)), by = "id") %>%
      filter(!is.na(dam_of), dam_of != "0", dam_of != "")
    if (nrow(sel_info) < 2) return(NULL)
    conflicts <- sel_info %>% group_by(dam_of, dob) %>% filter(n() > 2) %>% ungroup()
    if (nrow(conflicts) == 0) return(NULL)
    warnings <- character(0)
    for (g in (conflicts %>% filter(sex == rd$datF) %>% group_by(dam_of, dob) %>% group_split()))
      if (nrow(g) >= 3)
        warnings <- c(warnings, paste0("Dams \u2018", paste(g$id, collapse = "\u2019 and \u2018"),
                                       "\u2019 are litter mates \u2014 consider selecting only two."))
    for (g in (conflicts %>% filter(sex == rd$datM) %>% group_by(dam_of, dob) %>% group_split()))
      if (nrow(g) >= 3)
        warnings <- c(warnings, paste0("Sires \u2018", paste(g$id, collapse = "\u2019 and \u2018"),
                                       "\u2019 are litter mates \u2014 consider selecting only two."))
    if (length(warnings) == 0) NULL else warnings
  })
  
  # ── Recruitment value boxes ───────────────────────────────────────────────────
  
  output$vb_bs_dams_needed <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(rd$damsint);  h3(rd$damsint)
  })
  
  output$vb_bs_sires_needed <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(rd$siresint); h3(rd$siresint)
  })
  
  output$vb_bs_dams_back <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(recruitment_counts())
    rc    <- recruitment_counts()
    goal  <- if (!is.null(rd$damsint)) rd$damsint else 0L
    total <- rc$dams_back + rc$dams_sel
    color <- if (total >= goal) "#2e7a3a" else if (rc$dams_back > 0) "#e65100" else "inherit"
    tagList(h3(rc$dams_back, style = paste0("color:", color)),
            tags$small(paste0("+ ", rc$dams_sel, " selected"),
                       style = "color:#0277bd; font-size:0.8rem"))
  })
  
  output$vb_bs_sires_back <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(recruitment_counts())
    rc    <- recruitment_counts()
    goal  <- if (!is.null(rd$siresint)) rd$siresint else 0L
    total <- rc$sires_back + rc$sires_sel
    color <- if (total >= goal) "#2e7a3a" else if (rc$sires_back > 0) "#e65100" else "inherit"
    tagList(h3(rc$sires_back, style = paste0("color:", color)),
            tags$small(paste0("+ ", rc$sires_sel, " selected"),
                       style = "color:#0277bd; font-size:0.8rem"))
  })
  
  # ── Recruitment status panel ──────────────────────────────────────────────────
  
  output$recruitment_status <- renderUI({
    if (!breeders_col_specified()) return(tags$p(
      style = "color:#999; font-size:0.9rem",
      "Specify a breeding stock column to enable recruitment tracking."))
    req(recruitment_counts(), rd$damsint, rd$siresint)
    rc           <- recruitment_counts()
    dams_needed  <- rd$damsint;  sires_needed <- rd$siresint
    dams_done    <- rc$dams_back  + rc$dams_sel
    sires_done   <- rc$sires_back + rc$sires_sel
    dams_remain  <- max(0L, dams_needed  - dams_done)
    sires_remain <- max(0L, sires_needed - sires_done)
    
    color_for <- function(pct)
      if (pct >= 100) "#2e7a3a" else if (pct >= 50) "#e65100" else "#c62828"
    
    make_progress_bar <- function(taken, pending, needed, col) {
      done      <- taken + pending
      pct_total <- if (needed > 0) min(100, round(100 * done  / needed)) else 0
      pct_taken <- if (needed > 0) min(100, round(100 * taken / needed)) else 0
      pct_pend  <- pct_total - pct_taken
      tags$div(
        style = "display:flex; align-items:center; gap:10px; margin:6px 0 10px",
        tags$div(
          style = "flex:1; background:#e9ecef; border-radius:5px; overflow:hidden; height:20px; position:relative",
          tags$div(style = paste0("position:absolute; left:0; top:0; height:100%; width:", pct_taken,
                                  "%; background:", col, "; transition:width 0.4s ease")),
          if (pct_pend > 0)
            tags$div(style = paste0("position:absolute; left:", pct_taken,
                                    "%; top:0; height:100%; width:", pct_pend,
                                    "%; background:", col, "55; border-left:2px dashed ", col)),
          tags$div(
            style = paste0("position:absolute; inset:0; display:flex; align-items:center; ",
                           "justify-content:center; font-size:0.75rem; font-weight:700; ",
                           "color:", if (pct_total >= 35) "white" else "#555"),
            paste0(done, " / ", needed)
          )
        ),
        tags$span(paste0(pct_total, "%"),
                  style = paste0("font-size:0.88rem; font-weight:700; color:", col,
                                 "; min-width:36px; text-align:right"))
      )
    }
    
    row_item <- function(icon, label, value, value_style = "font-weight:600") {
      tags$div(
        style = "display:flex; justify-content:space-between; align-items:baseline; font-size:0.87rem; line-height:1.9",
        tags$span(style = "color:#555", paste(icon, label)),
        tags$span(style = value_style, value)
      )
    }
    
    section <- function(title, taken, pending, needed, remain, last_yr) {
      done <- taken + pending
      col  <- color_for(if (needed > 0) min(100, round(100 * done / needed)) else 0)
      tags$div(
        style = "margin-bottom:18px",
        tags$div(style = paste0("font-size:0.75rem; font-weight:700; text-transform:uppercase; ",
                                "letter-spacing:0.06em; color:#888; margin-bottom:2px"), title),
        make_progress_bar(taken, pending, needed, col),
        row_item("\u2713", paste0("Taken back (born ", last_yr, ")"), taken),
        if (pending > 0)
          row_item("+", "Selected, pending conversion", pending,
                   "font-weight:600; color:#0277bd"),
        row_item(if (remain == 0) "\u2714" else "\u2192", "Still needed", remain,
                 paste0("font-weight:700; color:", if (remain == 0) "#2e7a3a" else "#c62828"))
      )
    }
    
    conflicts     <- same_litter_warning()
    warning_block <- if (!is.null(conflicts))
      tagList(
        hr(style = "margin:12px 0 10px"),
        lapply(conflicts, function(w)
          tags$div(
            style = paste0("display:flex; gap:8px; align-items:flex-start; ",
                           "background:#fff3e0; border-left:4px solid #e65100; ",
                           "border-radius:0 4px 4px 0; padding:8px 10px; ",
                           "margin-bottom:6px; font-size:0.84rem; color:#bf360c"),
            tags$span("\u26a0\ufe0f", style = "flex-shrink:0; margin-top:1px"),
            tags$span(tags$strong("Litter conflict: "), w)
          )
        )
      )
    
    tagList(
      section("Dams",  rc$dams_back,  rc$dams_sel,  dams_needed,  dams_remain,  rc$last_year),
      section("Sires", rc$sires_back, rc$sires_sel, sires_needed, sires_remain, rc$last_year),
      tags$div(
        style = "font-size:0.75rem; color:#999; margin-top:-6px",
        tags$span(style = "display:inline-flex; align-items:center; gap:4px; margin-right:12px",
                  tags$span(style = "width:12px; height:10px; background:#0277bd; border-radius:2px; display:inline-block"),
                  "Taken back"),
        tags$span(style = "display:inline-flex; align-items:center; gap:4px",
                  tags$span(style = "width:12px; height:10px; background:#0277bd55; border:1px dashed #0277bd; border-radius:2px; display:inline-block"),
                  "Selected (pending)")
      ),
      warning_block
    )
  })
}
# ── pop_breeders_server.R ─────────────────────────────────────────────────────
# Panel helper for the "Active Breeding Stock" tab.
# Called from popServer(); shares input/output/session namespace.
#
# Reactive args: breeders_col_specified
# Defines outputs: no_breeders_warning_ph, vb_br_dams, vb_br_sires, vb_br_pct,
#                  vb_br_ext, vb_br_ext_dams, vb_br_ext_sires, vb_br_kinship,
#                  breeder_ov_vars, breeder_trait_plot_ui, breeder_trait_plot,
#                  breeder_ov_table
# Note: rd$dams and rd$sires (for the litter-count lookup in breeder_ov_table)
#       are written by the coordinator before this helper runs.

popBreedersHelper <- function(
    input, output, session, rd,
    breeders_col_specified
) {
  ns <- session$ns
  
  output$no_breeders_warning_ph <- renderUI({
    if (breeders_col_specified()) return(NULL)
    tags$div(
      class = "alert alert-warning",
      style = "font-size:0.9rem; padding:10px 14px; margin-bottom:12px",
      tags$b("\u26a0 No breeding stock column specified. "),
      "Breeder statistics require a designated breeding stock column in the data file. ",
      "Please specify this in the Data Input tab."
    )
  })
  
  # ── KPIs ──────────────────────────────────────────────────────────────────────
  breeder_kpis <- reactive({
    req(rd$breeders, rd$data, rd$ownpop)
    br      <- rd$breeders
    own_ids <- rd$ownpop[[rd$datid]]
    n_dams  <- sum(br[[rd$datsex]] == rd$datF, na.rm = TRUE)
    n_sires <- sum(br[[rd$datsex]] == rd$datM, na.rm = TRUE)
    n_own      <- sum(br[[rd$datid]] %in% own_ids)
    n_external <- nrow(br) - n_own
    pct_pop    <- round(100 * n_own / nrow(rd$ownpop), 1)
    pct_ext    <- round(100 * n_external / nrow(br), 1)
    ext_ids       <- br[[rd$datid]][!br[[rd$datid]] %in% own_ids]
    n_ext_dams    <- sum(br[[rd$datid]] %in% ext_ids & br[[rd$datsex]] == rd$datF, na.rm = TRUE)
    n_ext_sires   <- sum(br[[rd$datid]] %in% ext_ids & br[[rd$datsex]] == rd$datM, na.rm = TRUE)
    pct_ext_dams  <- round(100 * n_ext_dams  / n_dams,  1)
    pct_ext_sires <- round(100 * n_ext_sires / n_sires, 1)
    list(n_dams = n_dams, n_sires = n_sires, n_own = n_own, pct_pop = pct_pop,
         n_external = n_external, pct_ext = pct_ext,
         n_ext_dams = n_ext_dams, pct_ext_dams = pct_ext_dams,
         n_ext_sires = n_ext_sires, pct_ext_sires = pct_ext_sires)
  })
  
  # ── Value boxes ───────────────────────────────────────────────────────────────
  output$vb_br_dams  <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(breeder_kpis()); h3(breeder_kpis()$n_dams)
  })
  output$vb_br_sires <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(breeder_kpis()); h3(breeder_kpis()$n_sires)
  })
  
  output$vb_br_pct <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(breeder_kpis())
    kv <- breeder_kpis()
    tagList(h3(kv$n_own),
            tags$span(paste0(kv$pct_pop, "% of own population"), style = "font-size:0.8rem"))
  })
  
  output$vb_br_ext <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(breeder_kpis())
    kv          <- breeder_kpis()
    recommended <- rd$suggested_ext_pct
    color <- if (is.null(recommended)) "#888"
    else if (kv$pct_ext >= recommended) "#2e7a3a"
    else if (kv$pct_ext >= recommended / 2) "#e65100"
    else "#c62828"
    tags$div(
      `data-bs-toggle`    = "tooltip",
      `data-bs-placement` = "bottom",
      title = if (!is.null(recommended))
        paste0(" \u2265", recommended, "% recommended based on current Effective Population Size (Ne = ",
               round(rd$Ne, 0), ")"),
      style = "display:inline-block; cursor:help",
      h3(kv$n_external, style = paste0("color:", color)),
      tags$span(paste0(kv$pct_ext, "% of breeders ( \u2265 ", recommended, "%\u24d8"),
                style = paste0("font-size:0.8rem; color:", color, ";"))
    )
  })
  
  output$vb_br_ext_dams <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(breeder_kpis())
    kv <- breeder_kpis()
    tagList(h3(kv$n_ext_dams),
            tags$span(paste0(kv$pct_ext_dams, "% of dams"), style = "font-size:0.72rem;"))
  })
  
  output$vb_br_ext_sires <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(breeder_kpis())
    kv <- breeder_kpis()
    tagList(h3(kv$n_ext_sires),
            tags$span(paste0(kv$pct_ext_sires, "% of sires"), style = "font-size:0.72rem;"))
  })
  
  output$vb_br_kinship <- renderUI({
    if (!breeders_col_specified()) return(h3("\u2014", style = "color:grey"))
    req(rd$kinship, rd$breeders, rd$ownpop)
    br_ids  <- intersect(as.character(rd$breeders[[rd$datid]]), rownames(rd$kinship))
    if (length(br_ids) < 2) return(h3("\u2014"))
    kin_br       <- rd$kinship[br_ids, br_ids]
    mean_kinship <- round(mean(kin_br[upper.tri(kin_br)]), 4)
    pop_ids      <- intersect(as.character(rd$ownpop[[rd$datid]]), rownames(rd$kinship))
    kin_pop      <- rd$kinship[pop_ids, pop_ids]
    mean_kin_pop <- round(mean(kin_pop[upper.tri(kin_pop)]), 4)
    diff  <- mean_kinship - mean_kin_pop
    color <- if (diff <= 0) "#2e7a3a" else if (diff <= 0.005) "#e65100" else "#c62828"
    status_text <- if (diff <= 0)
      "<span style='color:#2e7a3a'><b>&#10003; Good:</b> your breeder selection is actively helping diversity.</span> "
    else if (diff <= 0.005)
      "<span style='color:#e65100'><b>&#9651; Slightly elevated:</b> your breeders are somewhat more related to each other than the population average. Consider introducing less related animals into the breeding stock.</span> "
    else
      "<span style='color:#c62828'><b>&#9651; Elevated:</b> your breeders are substantially more related to each other than the population average. It is recommended to swap out some breeders for less related animals.</span> "
    tags$div(
      `data-bs-toggle`    = "tooltip",
      `data-bs-placement` = "bottom",
      `data-bs-html`      = "true",
      title = paste0(
        "Mean kinship among breeders estimates the expected COI of the next generation under <b>random mating</b>. ",
        "Lower is better. ",
        "Comparison with own-population mean (animals in your program only, not the full pedigree): ",
        status_text,
        "Current: breeders = ", mean_kinship, ", own population = ", mean_kin_pop, "."
      ),
      style = "display:inline-block; cursor:help",
      tags$span(mean_kinship, style = paste0("font-size:1.4rem; font-weight:700; color:", color)),
      tags$span(" vs ", style = "font-size:0.85rem; color:#888;"),
      tags$span(mean_kin_pop, style = "font-size:1.4rem; font-weight:700; color:#888;"),
      tags$br(),
      tags$span("breeders vs population", style = "font-size:0.72rem; color:#aaa;")
    )
  })
  
  # ── Breeder trait comparison ───────────────────────────────────────────────────
  output$breeder_ov_vars <- renderUI({
    req(rd$data)
    choices <- colnames(rd$data)[
      !colnames(rd$data) %in% c(rd$datid, rd$datsex, rd$datdob, "BirthYear", "dob")
    ]
    vc <- sapply(rd$data[, choices, drop = FALSE], class)
    tagList(
      tags$div(
        style = "min-height: 120px",
        selectizeInput(ns("breeder_ov_cols"), label = NULL,
                       choices  = setNames(choices, paste0(choices, " (", vc, ")")),
                       selected = NULL, multiple = TRUE,
                       options  = list(placeholder = "Select variables\u2026",
                                       plugins = list("remove_button"), maxItems = NULL,
                                       dropdownParent = "body"))
      ),
      tags$p(style = "font-size:0.78rem; color:#aaa; margin-top:2px",
             "\u25b2\u25bc shows deviation from the population mean for each animal.")
    )
  })
  
  filtered_breeders <- reactive({
    req(rd$breeders)
    sex_filter <- c(if ("Dams"  %in% input$breeder_ov_sex) rd$datF,
                    if ("Sires" %in% input$breeder_ov_sex) rd$datM)
    rd$breeders[rd$breeders[[rd$datsex]] %in% sex_filter, ]
  })
  
  output$breeder_trait_plot_ui <- renderUI({
    if (!breeders_col_specified()) return(NULL)
    if (is.null(input$breeder_ov_cols) || length(input$breeder_ov_cols) == 0) return(NULL)
    tagList(p(""), plotlyOutput(ns("breeder_trait_plot")))
  })
  
  output$breeder_trait_plot <- renderPlotly({
    req(filtered_breeders(), rd$data, input$breeder_ov_cols)
    cols     <- input$breeder_ov_cols
    if (length(cols) == 0) return(NULL)
    vc       <- sapply(rd$data, class)
    br       <- filtered_breeders()
    override <- isTRUE(input$breeder_ov_override_cat)
    plots <- lapply(seq_along(cols), function(ci) {
      col       <- cols[[ci]]
      var_class <- vc[[col]]
      show_leg  <- ci == 1
      is_cat    <- override || var_class %in% c("character", "factor")
      if (!is_cat) {
        pop_vals <- as.numeric(rd$ownpop[[col]])
        br_vals  <- as.numeric(br[[col]])
        plot_ly() %>%
          add_trace(x = pop_vals, type = "box", name = "Population",
                    legendgroup = "pop", showlegend = show_leg,
                    marker = list(color = "#cccccc"), line = list(color = "#999999"),
                    fillcolor = "rgba(200,200,200,0.4)") %>%
          add_trace(x = br_vals, type = "box", name = "Breeders",
                    legendgroup = "br", showlegend = show_leg,
                    marker = list(color = "steelblue"), line = list(color = "steelblue"),
                    fillcolor = "rgba(70,130,180,0.4)") %>%
          layout(annotations = list(list(
            text = col, showarrow = FALSE, xref = "paper", yref = "paper",
            x = 0.5, y = 1.05, xanchor = "center", yanchor = "bottom",
            font = list(size = 12)
          )))
      } else {
        pop_col  <- as.character(rd$ownpop[[col]])
        br_col   <- as.character(br[[col]])
        all_cats <- sort(union(pop_col, br_col))
        pop_pct  <- as.numeric(table(factor(pop_col, levels = all_cats))) /
          sum(!is.na(pop_col)) * 100
        br_pct   <- as.numeric(table(factor(br_col,  levels = all_cats))) /
          sum(!is.na(br_col))  * 100
        # Tableau-10 palette, cycling if more than 10 categories
        pal <- c("#4e79a7","#f28e2b","#e15759","#76b7b2","#59a14f",
                 "#edc948","#b07aa1","#ff9da7","#9c755f","#bab0ac")
        p <- plot_ly()
        for (i in seq_along(all_cats)) {
          col_i <- pal[((i - 1) %% length(pal)) + 1]
          p <- p %>% add_bars(
            x             = c("Population", "Breeders"),
            y             = c(pop_pct[i], br_pct[i]),
            name          = all_cats[i],
            legendgroup   = all_cats[i],
            showlegend    = show_leg,
            marker        = list(color = col_i),
            hovertemplate = paste0(all_cats[i], ": %{y:.1f}%<extra></extra>")
          )
        }
        p %>% layout(
          barmode = "stack",
          yaxis   = list(title = "%", ticksuffix = "%", range = c(0, 101)),
          annotations = list(list(
            text = col, showarrow = FALSE, xref = "paper", yref = "paper",
            x = 0.5, y = 1.05, xanchor = "center", yanchor = "bottom",
            font = list(size = 12)
          ))
        )
      }
    })
    n_rows <- ceiling(length(plots) / min(2L, length(plots)))
    subplot(plots, nrows = n_rows, shareX = FALSE, shareY = FALSE,
            titleX = TRUE, titleY = TRUE, margin = 0.08) %>%
      layout(height = 280 * n_rows, showlegend = TRUE,
             legend = list(orientation = "h", x = 0, y = -0.05))
  })
  
  output$breeder_ov_table <- renderDataTable({
    if (!breeders_col_specified()) return(datatable(
      data.frame(Message = "Specify a breeding stock column to enable this table."),
      rownames = FALSE, options = list(dom = "t")))
    req(filtered_breeders(), rd$data)
    vc   <- sapply(rd$data, class)
    cols <- input$breeder_ov_cols
    df   <- filtered_breeders()
    if (!is.null(rd$inbreeding)) {
      df$F <- round(rd$inbreeding[as.character(df[[rd$datid]])], 4)
      df$F[is.na(df$F)] <- 0
    } else df$F <- NA_real_
    litter_lookup <- c(
      if (!is.null(rd$dams))  setNames(rd$dams[["# Litters"]],  rd$dams$Dam)   else c(),
      if (!is.null(rd$sires)) setNames(rd$sires[["# Litters"]], rd$sires$Sire) else c()
    )
    df$`Litters so far` <- litter_lookup[as.character(df[[rd$datid]])]
    df$`Litters so far`[is.na(df$`Litters so far`)] <- 0L
    compare_cols <- if (!is.null(cols) && length(cols) > 0) cols[cols %in% names(df)] else character(0)
    fixed <- c(rd$datid, rd$datsex, rd$datdob, "F", "Litters so far")
    df    <- df[, c(fixed, compare_cols), drop = FALSE]
    if (length(compare_cols) > 0) {
      null_crits <- setNames(rep(list("No Criterium"), length(compare_cols)),
                             paste0("crit_", compare_cols))
      df <- transform_to_delta(df, compare_cols, null_crits, rd$data, vc)
    }
    make_delta_datatable(df, compare_cols, selection = "none")
  })
}
# ── mating_ranking_server.R ───────────────────────────────────────────────────
rankingServer <- function(id, rd) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ══════════════════════════════════════════════════════════════════════════
    # HELPERS
    # ══════════════════════════════════════════════════════════════════════════
    
    data_var_classes <- reactive({
      req(rd$data)
      sapply(rd$data, class)
    })

    
    # ── Rank table ────────────────────────────────────────────────────────────
    # rank_tab() holds the final sorted result from rankbutton.
    # Nothing re-sorts it afterwards — what rankbutton produces is what renders.
    rank_tab <- reactiveVal(data.frame())
    
    # ══════════════════════════════════════════════════════════════════════════
    # UI OUTPUTS
    # ══════════════════════════════════════════════════════════════════════════
    
    output$testouderout <- renderUI({
      req(rd$breeders)
      selectizeInput(ns("testparent"), "Test Parent",
                     choices = unique(rd$breeders[[rd$datid]]))
    })
    
    output$selectvars <- renderUI({
      req(rd$data, data_var_classes())
      vc      <- data_var_classes()
      choices <- colnames(rd$data)[!colnames(rd$data) %in% c(rd$datid, rd$datsex)]
      selectizeInput(ns("colsofintvars"), "Variables",
                  choices  = setNames(choices, paste0(choices, " (", vc[choices], ")")),
                  options = list(dropdownParent = "body"),
                  selected = NULL,
                  multiple = TRUE)
    })
    
    # ── Per-variable criteria cards ───────────────────────────────────────────
    output$criteria <- renderUI({
      req(input$colsofintvars, rd$data, data_var_classes())
      vc <- data_var_classes()
      
      lapply(input$colsofintvars, function(var_name) {
        var_class   <- vc[var_name]
        dir_choices <- if (var_class %in% c("character", "factor"))
          c("No Goal", "=", "\u2260")
        else
          c("No Goal", "\u2264", "\u2265", "=", "\u2260")
        
        tags$div(
          style = paste0("border:1px solid #e0e0e0; border-radius:6px; ",
                         "padding:10px 14px; margin-bottom:8px; background:#fafafa"),
          tags$div(
            style = paste0("font-size:0.82rem; font-weight:700; text-transform:uppercase; ",
                           "letter-spacing:0.06em; color:#555; margin-bottom:8px"),
            paste0(var_name, "  (", var_class, ")")
          ),
          layout_columns(
            selectizeInput(ns(paste0("crit_", var_name)),
                        "Direction", choices = dir_choices,
                        options = list(dropdownParent = "body")),
            conditionalPanel(
              condition = paste0("input['", ns(paste0("crit_", var_name)), "'] !== 'No Goal'"),
              selectizeInput(ns(paste0("value_", var_name)), "Target Value",
                             choices = sort(unique(rd$data[[var_name]])),
                             options = list(create = TRUE, dropdownParent = "body"))
            ),
            col_widths = c(4, 8)
          )
        )
      })
    })
    
    # ── Parameter goals preview ───────────────────────────────────────────────
    output$param_goals_preview <- renderUI({
      req(input$use_param_goals, rd$param_goals)
      goals <- rd$param_goals
      if (!input$use_param_goals || nrow(goals) == 0) return(NULL)
      mand <- goals[goals$priority == "Mandatory", ]
      pref <- goals[goals$priority == "Preferred", ]
      make_pill <- function(g, col)
        tags$span(style = paste0(
          "display:inline-block; background:", col, "18; border:1px solid ", col,
          "; color:", col, "; border-radius:12px; padding:2px 8px; ",
          "font-size:0.78rem; margin:2px"
        ), paste0(g$variable, " ", g$direction, " ", g$value))
      tagList(
        if (nrow(mand) > 0) tags$div(
          style = "margin-top:6px",
          tags$span(style = "font-size:0.75rem; color:#0277bd; font-weight:700", "Mandatory: "),
          lapply(seq_len(nrow(mand)), function(i) make_pill(mand[i, ], "#0277bd"))
        ),
        if (nrow(pref) > 0) tags$div(
          style = "margin-top:4px",
          tags$span(style = "font-size:0.75rem; color:#888; font-weight:700", "Preferred: "),
          lapply(seq_len(nrow(pref)), function(i) make_pill(pref[i, ], "#888"))
        )
      )
    })
    
    # ══════════════════════════════════════════════════════════════════════════
    # RANK BUTTON
    # ══════════════════════════════════════════════════════════════════════════
    
    observeEvent(input$rankbutton, {
      req(rd$ped, rd$breeders, input$testparent)
      
      stat_data  <- as.data.frame(rd$breeders)
      vars       <- if (is.null(input$colsofintvars)) character(0) else input$colsofintvars
      
      testsex <- stat_data[stat_data[[rd$datid]] == input$testparent, rd$datsex]
      
      base_cols <- c(rd$datid, rd$datsex)
      tab <- stat_data[as.character(stat_data[[rd$datsex]]) != testsex,
                       c(base_cols, vars), drop = FALSE]
      
      # ── COI computation ───────────────────────────────────────────────────────
      K     <- rd$kinship
      F_vec <- rd$inbreeding
      
      cand_ids   <- as.character(tab[[rd$datid]])
      test_id    <- as.character(input$testparent)
      
      # For each candidate: who is dam, who is sire?
      dam_ids  <- if (testsex == rd$datM) cand_ids else rep(test_id, length(cand_ids))
      sire_ids <- if (testsex == rd$datM) rep(test_id, length(cand_ids)) else cand_ids
      
      # Offspring COI = kinship between parents (vectorised row/col lookup)
      tab$COI <- mapply(function(d, s) {
        if (d %in% rownames(K) && s %in% rownames(K)) K[d, s] else NA_real_
      }, dam_ids, sire_ids)
      
      # Parental COI = individual inbreeding from the same computation
      tab$COI_Mother <- unname(F_vec[dam_ids])
      tab$COI_Father <- unname(F_vec[sire_ids])
      
      # Individuals not in pedigree get F = 0 (founders)
      tab$COI_Mother[is.na(tab$COI_Mother)] <- 0
      tab$COI_Father[is.na(tab$COI_Father)] <- 0
      
      # ── COI increase computation and hard limit ───────────────────────────────
      tab$COI_Increase <- tab$COI - (tab$COI_Mother + tab$COI_Father) / 2
      tab$COI_Increase[is.na(tab$COI_Increase)] <- 0
      tab <- tab[!is.na(tab$COI) & tab$COI_Increase <= input$coiinc, ]
      
      # ── Apply symbol-based hard filters ──────────────────────────────────────
      for (col_name in vars) {
        if (!col_name %in% colnames(tab)) {
          showNotification(paste("Column", col_name, "not available!"), type = "error")
          next
        }
        dir <- input[[paste0("crit_", col_name)]]
        if (is.null(dir) || dir == "No Goal") next
        val <- input[[paste0("value_", col_name)]]
        if (is.null(val) || length(val) == 0) next
        tab <- tab[apply_goal_filter(tab[[col_name]], dir, val), , drop = FALSE]
      }
      
      # ── Optionally apply parameter goals from Management ──────────────────────
      if (isTRUE(input$use_param_goals) &&
          !is.null(rd$param_goals) && nrow(rd$param_goals) > 0) {
        for (i in seq_len(nrow(rd$param_goals))) {
          g <- rd$param_goals[i, ]
          if (!g$variable %in% colnames(tab)) next
          mask <- apply_goal_filter(tab[[g$variable]], g$direction, g$value)
          if (g$priority == "Mandatory") {
            tab <- tab[mask, , drop = FALSE]
          } else {
            tab[[paste0(".pref_fail_", g$variable)]] <- !mask
          }
        }
        penalty_cols <- grep("^\\.pref_fail_", colnames(tab), value = TRUE)
        if (length(penalty_cols) > 0) {
          n_fails             <- rowSums(tab[, penalty_cols, drop = FALSE])
          tab                 <- tab[order(n_fails, tab$COI_Increase, na.last = TRUE), ]
          tab[, penalty_cols] <- NULL
        }
      }
      
      tab <- tab[order(tab$COI_Increase, na.last = TRUE), ]
      
      rank_tab(tab)
    })
    
    # ══════════════════════════════════════════════════════════════════════════
    # TABLE OUTPUTS
    # ══════════════════════════════════════════════════════════════════════════
    
    output$ranktable <- renderDataTable({
      if (nrow(rank_tab()) == 0) {
        return(datatable(
          data.frame(" " = "Select a test parent above and click \u2018Get Ranking\u2019 to see ranked candidates here.",
                     check.names = FALSE),
          rownames = FALSE,
          options  = list(dom = "t", ordering = FALSE)
        ))
      }
      df <- rank_tab()
      df <- rank_tab()
      df$COI <- NULL
      df$COI_Mother   <- NULL
      df$COI_Father   <- NULL
      df$COI_Increase <- round(df$COI_Increase, 4)
      display_names <- colnames(df)
      display_names[display_names == "COI_Increase"] <- "COI Increase"
      datatable(df, style = "bootstrap", rownames = FALSE, selection = "single",
                colnames = display_names,
                options = list(paging = FALSE, scrollY = "100%",
                               scrollX = "100%", scrollCollapse = TRUE))
    })
    
    output$testparenttable <- renderDataTable({
      req(rd$breeders, input$testparent)   
      vars     <- if (is.null(input$colsofintvars)) character(0) else input$colsofintvars
      show_cols <- c(rd$datid, rd$datsex, vars)
      datatable(
        rd$breeders[rd$breeders[[rd$datid]] == input$testparent,
                    show_cols, drop = FALSE],
        style   = "bootstrap",
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    # ══════════════════════════════════════════════════════════════════════════
    # PEDIGREE IMAGE
    # ══════════════════════════════════════════════════════════════════════════
    
    # Selected row comes from rank_tab() directly (no sorted_rank() middleman)
    selected_pair <- reactive({
      req(rank_tab(), input$ranktable_rows_selected, rd$ped)
      row      <- input$ranktable_rows_selected
      req(row >= 1, row <= nrow(rank_tab()))
      cand     <- as.character(rank_tab()[row, rd$datid])
      cand_sex <- as.character(rank_tab()[row, rd$datsex])
      req(!is.na(cand), !is.na(cand_sex))
      if (cand_sex == rd$datF) list(dam = cand, sire = input$testparent)
      else                     list(dam = input$testparent, sire = cand)
    })
    
    output$pedtree2 <- renderImage({
      req(selected_pair(), rd$ped, rd$kinship, rd$inbreeding, rd$breeders)
      pair    <- selected_pair()
      outfile <- tempfile(fileext = ".png")
      png(outfile, width = 1500, height = 1000, res = 200)
      pedtree(rd$ped, rd$pedid, rd$peddam, rd$pedsire, rd$pedsex, rd$pedF, rd$pedM,
              pair$dam, pair$sire,
              inbreeding_vec = rd$inbreeding,
              kinship_mat    = rd$kinship)
      dev.off()
      list(src = outfile, contentType = "image/png",
           width = 1500, height = 1000, alt = "TestPup Pedigree")
    }, deleteFile = TRUE)
    
    output$download_ui <- renderUI({
      req(selected_pair())
      downloadButton(ns("downloadPedtree2"), "Download Pedigree image")
    })
    
    output$downloadPedtree2 <- downloadHandler(
      filename = function() {
        pair <- selected_pair()
        paste0("Pedigree_", pair$sire, "_", pair$dam, ".png")
      },
      content = function(file) {
        pair <- selected_pair()
        png(file, width = 1500, height = 1000, res = 200)
        pedtree(rd$ped, rd$pedid, rd$peddam, rd$pedsire, rd$pedsex, rd$pedF, rd$pedM,
                pair$dam, pair$sire,
                inbreeding_vec = rd$inbreeding,
                kinship_mat    = rd$kinship)
        dev.off()
      }
    )
    
  })
}
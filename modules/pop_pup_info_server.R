# ── pop_pup_info_server.R ─────────────────────────────────────────────────────
# Panel helper for the "Animal Info" tab.
# Called from popServer(); shares input/output/session namespace.
#
# Reactive args: breeders_col_specified
# Inputs consumed: selected_id, extra_columns, make_pup_breeder,
#                  pup_overview_table_rows_selected
# Defines outputs: vb_sex, vb_dob, vb_dam, vb_sire, vb_f, vb_littermates,
#                  breeder_toggle_ui, pup_overview_table, extra_columns_section,
#                  extra_columns_table, table_title, filtered_table

popPupInfoHelper <- function(
    input, output, session, rd,
    breeders_col_specified
) {
  ns <- session$ns
  
  # ── Selectors ─────────────────────────────────────────────────────────────────
  observe({
    req(rd$merged)
    updateSelectizeInput(session, "selected_id",
                         choices = rd$merged[[rd$datid]],
                         server  = TRUE)
    extra_choices <- setdiff(names(rd$merged),
                             c(rd$datid, rd$datdob, rd$peddam,
                               rd$pedsire, rd$datsex, "BirthYear", "dob"))
    updateSelectizeInput(session, "extra_columns",
                         choices = extra_choices,
                         server  = TRUE)
  })
  
  selected_row <- reactive({
    req(input$selected_id, rd$merged)
    rd$merged[rd$merged[[rd$datid]] == input$selected_id, , drop = FALSE]
  })
  
  # ── Value boxes ───────────────────────────────────────────────────────────────
  output$vb_sex <- renderUI({
    req(selected_row())
    h3(as.character(selected_row()[[rd$datsex]]))
  })
  
  output$vb_dob <- renderUI({
    req(selected_row())
    h3(as.character(selected_row()[[rd$datdob]]))
  })
  
  output$vb_dam <- renderUI({
    req(selected_row())
    val <- as.character(selected_row()[[rd$peddam]])
    tags$span(val, style = "font-size:0.9rem; font-weight:600; word-break:break-all;")
  })
  
  output$vb_sire <- renderUI({
    req(selected_row())
    val <- as.character(selected_row()[[rd$pedsire]])
    tags$span(val, style = "font-size:0.9rem; font-weight:600; word-break:break-all;")
  })
  
  output$vb_f <- renderUI({
    req(selected_row(), rd$inbreeding)
    id_val <- as.character(selected_row()[[rd$datid]])
    f_val  <- rd$inbreeding[id_val]
    f_val  <- if (is.na(f_val)) 0 else round(f_val, 4)
    tags$div(
      `data-bs-toggle`    = "tooltip",
      `data-bs-placement` = "bottom",
      title = paste0(
        "Coefficient of Inbreeding (COI): probability this animal inherited identical gene copies ",
        "from a common ancestor. "
      ),
      style = "display:inline-block; cursor:help",
      tags$span(f_val, style = paste0("font-size:1.4rem; font-weight:700"))
    )
  })
  
  output$vb_littermates <- renderUI({
    req(selected_row(), rd$merged)
    dob_val <- selected_row()[[rd$datdob]]
    dam_val <- selected_row()[[rd$peddam]]
    n <- nrow(rd$merged[rd$merged[[rd$datdob]] == dob_val &
                          rd$merged[[rd$peddam]]  == dam_val, ]) - 1L
    h3(max(n, 0L))
  })
  
  # ── Breeder toggle ────────────────────────────────────────────────────────────
  output$breeder_toggle_ui <- renderUI({
    req(input$selected_id)
    if (!breeders_col_specified()) return(NULL)
    checkboxInput(ns("make_pup_breeder"),
                  label = tagList(bsicons::bs_icon("star-fill"),
                                  " Designate as Active Breeder"),
                  value = FALSE)
  })
  
  observe({
    req(input$selected_id, rd$breeders)
    updateCheckboxInput(session, "make_pup_breeder",
                        value = input$selected_id %in% rd$breeders[[rd$datid]])
  })
  
  observeEvent(input$make_pup_breeder, {
    req(input$selected_id, rd$breeders, rd$data)
    current <- rd$breeders
    if (input$make_pup_breeder) {
      if (!(input$selected_id %in% current[[rd$datid]])) {
        new_row     <- rd$data[rd$data[[rd$datid]] == input$selected_id, , drop = FALSE]
        rd$breeders <- rbind(current, new_row)
      }
    } else {
      rd$breeders <- current[current[[rd$datid]] != input$selected_id, , drop = FALSE]
    }
  }, ignoreInit = TRUE)
  
  # ── Animal details table ──────────────────────────────────────────────────────
  output$pup_overview_table <- renderDataTable({
    req(selected_row())
    row <- selected_row()
    df  <- data.frame(
      Label = c(rd$datdob, rd$peddam, rd$pedsire, rd$datsex),
      Value = c(as.character(row[[rd$datdob]]),
                as.character(row[[rd$peddam]]),
                as.character(row[[rd$pedsire]]),
                as.character(row[[rd$datsex]])),
      stringsAsFactors = FALSE
    )
    datatable(df, style = "bootstrap", selection = "single", rownames = FALSE,
              colnames = c("Field", "Value"),
              options  = list(dom = "t", paging = FALSE, searching = FALSE,
                              ordering = FALSE, info = FALSE))
  })
  
  # ── Extra columns ─────────────────────────────────────────────────────────────
  output$extra_columns_section <- renderUI({
    req(input$extra_columns, length(input$extra_columns) > 0)
    tagList(
      hr(style = "margin: 8px 0"),
      dataTableOutput(ns("extra_columns_table"))
    )
  })
  
  output$extra_columns_table <- renderDataTable({
    req(selected_row(), input$extra_columns, rd$data)
    cols <- input$extra_columns[input$extra_columns %in% names(rd$data)]
    if (length(cols) == 0) return(NULL)
    vc         <- sapply(rd$data, class)
    df         <- selected_row()[, cols, drop = FALSE]
    null_crits <- setNames(rep(list("No Criterium"), length(cols)), paste0("crit_", cols))
    df <- transform_to_delta(df, cols, null_crits, rd$data, vc)
    make_delta_datatable(df, cols, selection = "none")
  })
  
  # ── Family browser ────────────────────────────────────────────────────────────
  observeEvent(input$pup_overview_table_rows_selected, {
    req(input$pup_overview_table_rows_selected, selected_row(), rd$merged)
    idx    <- input$pup_overview_table_rows_selected
    row    <- selected_row()
    cols   <- c(rd$datdob, rd$peddam, rd$pedsire, rd$datsex)
    values <- c(as.character(row[[rd$datdob]]),
                as.character(row[[rd$peddam]]),
                as.character(row[[rd$pedsire]]),
                as.character(row[[rd$datsex]]))
    selected_col <- cols[idx]
    selected_val <- values[idx]
    label_map    <- c("Date of Birth", "Dam", "Sire", "Sex")
    
    output$table_title <- renderUI({
      tags$p(style = "font-weight:600; margin-bottom:8px;",
             paste0("Showing animals sharing the same ", label_map[idx], ": "),
             tags$em(selected_val))
    })
    
    output$filtered_table <- renderDataTable({
      req(rd$merged)
      if (selected_col %in% c(rd$datdob, rd$datsex)) {
        fd <- rd$merged[rd$merged[[selected_col]] == selected_val, , drop = FALSE]
      } else {
        related <- rd$merged[rd$merged[[selected_col]] == selected_val, , drop = FALSE]
        parent  <- rd$merged[rd$merged[[rd$datid]]     == selected_val, , drop = FALSE]
        fd      <- rbind(parent, related)
      }
      display_cols <- c(rd$datid, rd$datdob, rd$peddam, rd$pedsire, rd$datsex,
                        if (!is.null(input$extra_columns)) input$extra_columns)
      display_cols <- display_cols[display_cols %in% names(fd)]
      fd           <- fd[, display_cols, drop = FALSE]
      compare_cols <- input$extra_columns[input$extra_columns %in% names(fd)]
      if (length(compare_cols) > 0) {
        vc         <- sapply(rd$data, class)
        null_crits <- setNames(rep(list("No Criterium"), length(compare_cols)),
                               paste0("crit_", compare_cols))
        fd <- transform_to_delta(fd, compare_cols, null_crits, rd$data, vc)
      }
      make_delta_datatable(fd, compare_cols, selection = "single")
    })
  })
}
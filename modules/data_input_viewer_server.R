# ── data_input_viewer_server.R ───────────────────────────────────────────
dataInputViewerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    tip <- function(text) tags$span(
      "?",
      `data-bs-toggle`    = "tooltip",
      `data-bs-placement` = "right",
      title = text,
      style = "color:#0277bd; cursor:help; font-weight:700; font-size:0.82rem; margin-left:5px"
    )
    
    # ── Raw file reads (populated by upload buttons) ───────────────────────────
    rped  <- reactiveVal()
    rdata <- reactiveVal()
    data_ad <- reactiveVal(list())   # stores autodetected data column names
    ped_ad  <- reactiveVal(list())   # stores autodetected pedigree column names
    
    # ── Core reactive values (single source of truth) ─────────────────────────
    ped      <- reactiveVal()
    data     <- reactiveVal()
    ownpop   <- reactiveVal()
    breeders <- reactiveVal()
    merged   <- reactiveVal()
    mls      <- reactiveVal()
    
    # ── Sex label stores (set on submit, used in downstream computations) ──────
    pedsex  <- reactiveValues(fem = NULL, male = NULL)
    datasex <- reactiveValues(fem = NULL, male = NULL)
    
    # ── rd: shared state passed to other modules ──────────────────────────────
    rd <- reactiveValues(
      ped = NULL, data = NULL, ownpop = NULL, merged = NULL, breeders = NULL,
      kinship = NULL, inbreeding = NULL, gen_depth = NULL,
      # column name slots — filled after submit, not hardcoded
      pedid = NULL, peddam = NULL, pedsire = NULL,
      pedsex = NULL, pedF   = NULL, pedM    = NULL,
      datid = NULL, datdob = NULL, datsex = NULL,
      datF  = NULL, datM   = NULL,
      Ne = NULL, deltaF = NULL, ecg = NULL, mls = NULL,
      suggested_ext_pct = NULL,
      param_goals = data.frame(
        variable  = character(),
        var_class = character(),
        direction = character(),
        value     = character(),
        priority  = character(),
        stringsAsFactors = FALSE
      )
    )
    
    # Keep rd data slots in sync with their reactiveVals
    observe({ rd$ped      <- ped()      })
    observe({ rd$data     <- data()     })
    observe({ rd$ownpop   <- ownpop()   })
    observe({ rd$merged   <- merged()   })
    observe({ rd$breeders <- breeders() })
    observe({ rd$mls      <- mls()      })
    
    # ── File upload handlers ───────────────────────────────────────────────────
    observeEvent(input$upload_data, {
      req(input$data_file)
      sheet <- if (is.null(input$datasheet) || trimws(input$datasheet) == "")
        NULL else trimws(input$datasheet)
      tryCatch({
        df <- if (is.null(sheet))
          read_excel(input$data_file$datapath, na = NA_STRINGS)
        else
          read_excel(input$data_file$datapath, sheet = sheet, na = NA_STRINGS)
        rdata(df)
        data_ad(autodetect_data_cols(df))        
        output$data_upload_error <- renderText("")
      }, error = function(e) {
        output$data_upload_error <- renderText(
          paste0("Error: sheet '", sheet, "' not found in the file."))
      })
    })
    
    observeEvent(input$upload_ped, {
      req(input$ped_file)
      sheet <- if (is.null(input$pedsheet) || trimws(input$pedsheet) == "")
        NULL else trimws(input$pedsheet)
      tryCatch({
        df <- if (is.null(sheet))
          read_excel(input$ped_file$datapath, na = NA_STRINGS)
        else
          read_excel(input$ped_file$datapath, sheet = sheet, na = NA_STRINGS)
        rped(df)
        ped_ad(autodetect_ped_cols(df))
        output$ped_upload_error <- renderText("")
      }, error = function(e) {
        output$ped_upload_error <- renderText(
          paste0("Error: sheet '", sheet, "' not found in the file."))
      })
    })
    
    # ── Raw data previews ─────────────────────────────────────────────────────
    output$datahead <- renderTable({ req(rdata()); head(rdata(), 5) })
    output$pedhead  <- renderTable({ req(rped());  head(rped(),  5) })
    
    # ══════════════════════════════════════════════════════════════════════════
    # Dynamic column-selection UI (renders once the file is uploaded)
    # ══════════════════════════════════════════════════════════════════════════
    
    # ── Data column selectors ─────────────────────────────────────────────────
    output$datavars <- renderUI({
      req(rdata())
      cols     <- colnames(rdata())
      ad   <- data_ad()                              # autodetected suggestions
      
      card(card_header("Select Data Column Names"),
           card_body(
             if (isTRUE(ad$any_detected))
               tags$div(
                 class = "alert alert-info alert-dismissible fade show",
                 style = "font-size:0.82rem; padding:8px 12px; margin-bottom:12px",
                 bsicons::bs_icon("magic"), " ",
                 "Column names were auto-detected from your file. ",
                 "Check that the selections below are correct before submitting.",
                 tags$button(
                   type = "button",
                   class = "btn-close",
                   `data-bs-dismiss` = "alert",
                   `aria-label` = "Close"
                 )
               ),
             fluidRow(
               column(12,
                      selectizeInput(ns("iddatacol"),
                                     label = tagList("Animal ID", tip("A unique code or name for each animal. 
                                                                      Must match the Animal ID column in your pedigree file exactly — 
                                                                      same spelling, same capitalisation.")),
                                     choices = cols,
                                     selected = ad$id %||% cols[1])
               )
             ),
             fluidRow(
               column(6,
                      selectizeInput(ns("dobdatacol"), "Date of Birth", choices = cols,
                                     selected = ad$dob %||% cols[1])
               ),
               column(6,
                      selectizeInput(ns("dobformat"), "Date Format",
                                     choices = c(
                                       "YYYY-MM-DD  (2024-12-25)" = "%Y-%m-%d",
                                       "DD/MM/YYYY  (25/12/2024)" = "%d/%m/%Y",
                                       "MM/DD/YYYY  (12/25/2024)" = "%m/%d/%Y"
                                     ),
                                     selected = "%Y-%m-%d"),
                      tags$p(
                        style = "font-size:0.78rem; color:#888; margin-top:-10px; margin-bottom:8px",
                        "Check a cell in your date column — is the year first or last? ",
                        "Pick the format that matches."
                      )
               )
             ),
             fluidRow(
               column(4,
                      selectizeInput(ns("sexdatacol"), "Sex", choices = cols,
                                     selected = ad$sex %||% cols[1])
               ),
               column(4, uiOutput(ns("data_female_ui"))),
               column(4, uiOutput(ns("data_male_ui")))
             ),
             fluidRow(
               column(6,
                      selectizeInput(ns("owndatacol"),
                                     label = tagList("Animals in breeding program (optional)",
                                                     tip("A column identifying animals that belong to your own 
                                                         program. If left blank, all animals in the data file 
                                                         are treated as your own population.")),
                                     choices = c("None", cols),
                                     options = list(dropdownParent = "body"),
                                     selected = ad$own %||% "None")
               ),
               column(6, uiOutput(ns("data_own_Y_ui")))
             ),
             fluidRow(
               column(6,
                      selectizeInput(ns("breeddatacol"),
                                     label = tagList("Active Breedingstock column (optional)",
                                                     tip("A column flagging which animals are active breeders. 
                                                         Without this, the Dam Planner, Pup Planner, and Breeder 
                                                         Selection panels will not work.")),
                                     choices = c("None", cols),
                                     options = list(dropdownParent = "body"),
                                     selected = ad$breed %||% "None")
               ),
               column(6, uiOutput(ns("data_breed_Y_ui")))
             )
           )
      )
    })
    
    # Dependent value selectors for data sex / ownpop / breeders
    output$data_female_ui <- renderUI({
      req(input$sexdatacol, rdata())
      if (input$sexdatacol == input$iddatacol ||
          input$sexdatacol == input$dobdatacol) {
        return(tags$div(
          class = "alert alert-warning",
          style = "font-size:0.82rem; padding:6px 10px",
          "Select a valid Sex column first."
        ))
      }
      vals <- sort(unique(na.omit(as.character(rdata()[[input$sexdatacol]]))))
      if (length(vals) < 2) {
        return(tags$div(
          class = "alert alert-warning",
          style = "font-size:0.82rem; padding:6px 10px",
          "The selected Sex column has fewer than 2 distinct values."
        ))
      }
      if (length(vals) > 2) {
        return(tags$div(
          class = "alert alert-warning",
          style = "font-size:0.82rem; padding:6px 10px",
          paste0("The selected Sex column has ", length(vals),
                 " distinct values — expected 2. Select the correct column first.")
        ))
      }
      sl   <- detect_sex_labels(rdata(), input$sexdatacol)
      selectizeInput(ns("femdata"), "Female label",
                     choices = vals, selected = sl$fem)
    })
    
    output$data_male_ui <- renderUI({
      req(input$sexdatacol, rdata())
      if (input$sexdatacol == input$iddatacol ||
          input$sexdatacol == input$dobdatacol) {
        return(tags$div(
          class = "alert alert-warning",
          style = "font-size:0.82rem; padding:6px 10px",
          "Select a valid Sex column first."
        ))
      }
      vals <- sort(unique(na.omit(as.character(rdata()[[input$sexdatacol]]))))
      if (length(vals) < 2) {
        return(tags$div(
          class = "alert alert-warning",
          style = "font-size:0.82rem; padding:6px 10px",
          "The selected Sex column has fewer than 2 distinct values."
        ))
      }
      if (length(vals) > 2) {
        return(tags$div(
          class = "alert alert-warning",
          style = "font-size:0.82rem; padding:6px 10px",
          paste0("The selected Sex column has ", length(vals),
                 " distinct values — expected 2. Select the correct column first.")
        ))
      }
      sl   <- detect_sex_labels(rdata(), input$sexdatacol)
      selectizeInput(ns("maledata"), "Male label", choices = vals,
                     selected = sl$male)
    })
    
    output$data_own_Y_ui <- renderUI({
      req(input$owndatacol, rdata())
      if (is.null(input$owndatacol) || input$owndatacol == "None") return(NULL)
      vals <- sort(unique(na.omit(as.character(rdata()[[input$owndatacol]]))))
      selectizeInput(ns("ownY"), "Value for 'in program'", choices = vals,
                     options = list(dropdownParent = "body"),
                     selected = vals[1])
    })
    
    output$data_breed_Y_ui <- renderUI({
      req(input$breeddatacol, rdata())
      if (is.null(input$breeddatacol) || input$breeddatacol == "None") return(NULL)
      vals <- sort(unique(na.omit(as.character(rdata()[[input$breeddatacol]]))))
      selectizeInput(ns("breedY"), "Value for 'breedingstock'", choices = vals,
                     options = list(dropdownParent = "body"),
                     selected = vals[1])
    })
    
    # ── Pedigree column selectors ─────────────────────────────────────────────
    output$pedvars <- renderUI({
      req(rped())
      cols <- colnames(rped())
      ad   <- ped_ad()
      
      card(card_header("Select Pedigree Column Names"),
           card_body(
             if (isTRUE(ad$any_detected))
               tags$div(
                 class = "alert alert-info alert-dismissible fade show",
                 style = "font-size:0.82rem; padding:8px 12px; margin-bottom:12px",
                 bsicons::bs_icon("magic"), " ",
                 "Column names were auto-detected from your file. ",
                 "Check that the selections below are correct before submitting.",
                 tags$button(
                   type = "button",
                   class = "btn-close",
                   `data-bs-dismiss` = "alert",
                   `aria-label` = "Close"
                 )
               ),
             fluidRow(
               column(12,
                      selectizeInput(ns("idcol"),
                                     label = tagList("Animal ID", tip("Must match the Animal ID column in your 
                                                                      data file exactly — same spelling, same 
                                                                      capitalisation.")),
                                     choices = cols,
                                     selected = ad$id %||% cols[1])
               )
             ),
             fluidRow(
               column(6,
                      selectizeInput(ns("sirecol"), "Sire (Father)", choices = cols,
                                     selected = ad$sire %||% cols[1])
               )
             ),
             fluidRow(
               column(6,
                      selectizeInput(ns("damcol"), "Dam (Mother)", choices = cols,
                                     selected = ad$dam %||% cols[1])
               )
             ),
             fluidRow(
               column(4,
                      selectizeInput(ns("sexcol"), "Sex", choices = cols,
                                     options = list(dropdownParent = "body"),
                                     selected = ad$sex %||% cols[1])
               ),
               column(4, uiOutput(ns("ped_female_ui"))),
               column(4, uiOutput(ns("ped_male_ui")))
             )
           )
      )
    })
    
    output$ped_female_ui <- renderUI({
      req(input$sexcol, rped())
      if (input$sexcol == input$idcol ||
          input$sexcol == input$damcol ||
          input$sexcol == input$sirecol) {
        return(tags$div(
          class = "alert alert-warning",
          style = "font-size:0.82rem; padding:6px 10px",
          "Select a valid Sex column first."
        ))
      }
      vals <- sort(unique(na.omit(as.character(rped()[[input$sexcol]]))))
      if (length(vals) < 2) {
        return(tags$div(
          class = "alert alert-warning",
          style = "font-size:0.82rem; padding:6px 10px",
          "The selected Sex column has fewer than 2 distinct values."
        ))
      }
      if (length(vals) > 2) {
        return(tags$div(
          class = "alert alert-warning",
          style = "font-size:0.82rem; padding:6px 10px",
          paste0("The selected Sex column has ", length(vals),
                 " distinct values — expected 2. Select the correct column first.")
        ))
      }
      sl   <- detect_sex_labels(rped(), input$sexcol)
      selectizeInput(ns("femped"), "Female label", choices = vals,
                     selected = sl$fem)
    })
    
    output$ped_male_ui <- renderUI({
      req(input$sexcol, rped())
      if (input$sexcol == input$idcol ||
          input$sexcol == input$damcol ||
          input$sexcol == input$sirecol) {
        return(tags$div(
          class = "alert alert-warning",
          style = "font-size:0.82rem; padding:6px 10px",
          "Select a valid Sex column first."
        ))
      }
      vals <- sort(unique(na.omit(as.character(rped()[[input$sexcol]]))))
      if (length(vals) < 2) {
        return(tags$div(
          class = "alert alert-warning",
          style = "font-size:0.82rem; padding:6px 10px",
          "The selected Sex column has fewer than 2 distinct values."
        ))
      }
      if (length(vals) > 2) {
        return(tags$div(
          class = "alert alert-warning",
          style = "font-size:0.82rem; padding:6px 10px",
          paste0("The selected Sex column has ", length(vals),
                 " distinct values — expected 2. Select the correct column first.")
        ))
      }
      sl   <- detect_sex_labels(rped(), input$sexcol)
      selectizeInput(ns("maleped"), "Male label", choices = vals,
                     selected = sl$male)
    })
    
    # ══════════════════════════════════════════════════════════════════════════
    # Submit handlers
    # ══════════════════════════════════════════════════════════════════════════
    
    # ── Submit data ────────────────────────────────────────────────────────────
    observeEvent(input$update_data, {
      req(rdata(), input$iddatacol, input$sexdatacol, input$dobdatacol, input$dobformat)
      
      # ── Validate columns exist ───────────────────────────────────────────────
      required <- c(input$iddatacol, input$sexdatacol, input$dobdatacol)
      missing  <- setdiff(required, colnames(rdata()))
      if (length(missing) > 0) {
        output$data_submitted <- renderText("")
        output$data_double    <- renderText(
          paste0("<b style='color:red'>Error:</b> Column(s) not found in data file: ",
                 paste("<b>", missing, "</b>", collapse = ", ")))
        data(NULL)
        return()
      }
      
      # ── Validate columns are distinct ────────────────────────────────────────
      if (anyDuplicated(required)) {
        output$data_submitted <- renderText("")
        output$data_double    <- renderText(
          "<b style='color:red'>Error:</b> ID, Sex, and Date of Birth must each be a different column.")
        data(NULL)
        return()
      }
      
      output$data_double <- renderText("")  # clear any previous error
      
      clean <- cleandata(as.data.frame(rdata()),
                         input$iddatacol, input$sexdatacol,
                         input$dobdatacol, input$dobformat)
      clean$BirthYear <- format(clean$dob, "%Y")
      
      # ── Validate DOB column parsed correctly ─────────────────────────────────
      n_total   <- nrow(clean)
      n_valid   <- sum(!is.na(clean$dob))
      n_invalid <- n_total - n_valid
      
      if (n_valid == 0) {
        output$data_submitted      <- renderText("")
        output$data_submit_summary <- renderUI(NULL)
        output$data_double <- renderText(paste0(
          "<b style='color:red'>Error:</b> The selected date of birth column <b>",
          input$dobdatacol, "</b> could not be recognized as dates — 0 out of ",
          n_total, " values were recognised. ",
          "Check that you selected the correct column and the correct date format."
        ))
        data(NULL)
        return()
      }
      
      if (n_invalid > 0) {
        pct <- round(100 * n_invalid / n_total)
        showNotification(
          id = "dob_partial_parse",
          ui = tagList(
            tags$b(paste0("DOB warning: ", pct, "% of dates could not be recognized.")),
            tags$br(),
            paste0(n_invalid, " of ", n_total, " values in column <b>",
                   input$dobdatacol, "</b> were not recognised as dates and were set to NA. ",
                   "These animals will be excluded from any time-based calculations ",
                   "(generation interval, age at litter, projections). ",
                   "Check the selected date format matches your data.")
          ),
          type = "warning", duration = 15
        )
      }
      
      # ── Validate ID column has no all-NA or constant value ───────────────────
      if (all(is.na(clean[[input$iddatacol]])) || length(unique(clean[[input$iddatacol]])) == 1) {
        output$data_submitted      <- renderText("")
        output$data_submit_summary <- renderUI(NULL)
        output$data_double <- renderText(paste0(
          "<b style='color:red'>Error:</b> The selected Animal ID column <b>",
          input$iddatacol, "</b> appears to contain no usable unique identifiers. ",
          "Check that you selected the correct column."
        ))
        data(NULL)
        return()
      }
      
      # ── Warn about ambiguous columns (after conversion) ───────────────────
      skip_cols <- c(input$iddatacol, input$sexdatacol, input$dobdatacol, "dob", "BirthYear")
      ambiguous <- sapply(setdiff(names(clean), skip_cols), function(col) {
        if (is.numeric(clean[[col]])) return(FALSE)
        x      <- as.character(clean[[col]])
        non_na <- x[!is.na(x) & x != ""]
        if (length(non_na) == 0) return(FALSE)
        parsed <- suppressWarnings(as.numeric(gsub(",", ".", non_na, fixed = TRUE)))
        pct    <- mean(!is.na(parsed))
        pct > 0.2 && pct < 0.8
      })
      if (any(ambiguous)) {
        showNotification(
          id = "ambiguous_cols",
          paste0("Column(s) ", paste(names(ambiguous)[ambiguous], collapse = ", "),
                 " contain a mix of numeric and text values and were kept as text. ",
                 "Check these columns for unexpected values."),
          type = "warning", duration = 10
        )
      }
      
      data(clean)
      
      # Store sex labels
      datasex$fem  <- input$femdata
      datasex$male <- input$maledata
      
      # Propagate column-name metadata to rd immediately
      rd$datid  <- input$iddatacol
      rd$datdob <- input$dobdatacol
      rd$datsex <- input$sexdatacol
      rd$datF   <- input$femdata
      rd$datM   <- input$maledata
      
      # Duplicate check
      dups <- rdata()[[input$iddatacol]][duplicated(rdata()[[input$iddatacol]])]
      output$data_double <- renderText({
        if (length(dups) > 0)
          paste("The following individuals appear more than once in the data file:",
                paste("<B>", dups, "</B>", collapse = ", "))
      })
      
      output$data_submitted <- renderText({ "Data Successfully Submitted!" })
      
      output$data_submit_summary <- renderUI({
        n_animals <- nrow(clean)
        n_dates   <- length(unique(clean[[input$dobdatacol]]))
        tags$p(
          style = "font-size:0.82rem; color:#2e7a3a; margin-top:2px",
          tags$b(n_animals), " animals loaded \u00b7 ",
          tags$b(n_dates), " unique birth dates \u00b7 ",
          "sex labels: ", tags$b(input$femdata), " / ", tags$b(input$maledata)
        )
      })
      
      output$dataprev <- renderDataTable({
        datatable(data(), style = "bootstrap",
                  options = list(paging = FALSE, scrollY       = "800px",
                                 scrollX = "100%", scrollCollapse = TRUE))
      })
      
      output$view_data_link <- renderUI({
        actionLink(ns("go_to_data_table"), "View Data Table")
      })
      
      # Own-population subset
      if (!is.null(input$owndatacol) && input$owndatacol != "None" && !is.null(input$ownY)) {
        ownpop(clean[!is.na(clean[[input$owndatacol]]) &
                       clean[[input$owndatacol]] == input$ownY, ])
      } else {
        ownpop(clean)
      }
      
      output$ownprev <- renderDataTable({
        datatable(ownpop(), style = "bootstrap",
                  options = list(paging = FALSE, scrollY       = "800px",
                                 scrollX = "100%", scrollCollapse = TRUE))
      })
      
      # Breeders subset
      if (!is.null(input$breeddatacol) && input$breeddatacol != "None" && !is.null(input$breedY)) {
        breeders(clean[!is.na(clean[[input$breeddatacol]]) &
                         clean[[input$breeddatacol]] == input$breedY, ])
      } else {
        breeders(clean)
      }
      
      output$breedprev <- renderDataTable({
        datatable(breeders(), style = "bootstrap",
                  options = list(paging = FALSE, scrollY       = "800px",
                                 scrollX = "100%", scrollCollapse = TRUE))
      })
    })
    
    # ── Submit pedigree ────────────────────────────────────────────────────────
    observeEvent(input$update_ped, {
      req(rped(), input$idcol, input$damcol, input$sirecol, input$sexcol)
      
      # ── Validate columns exist ───────────────────────────────────────────────
      required <- c(input$idcol, input$damcol, input$sirecol, input$sexcol)
      missing  <- setdiff(required, colnames(rped()))
      if (length(missing) > 0) {
        output$error_message <- renderText(
          paste0("<b style='color:red'>Error:</b> Column(s) not found in pedigree file: ",
                 paste("<b>", missing, "</b>", collapse = ", ")))
        ped(NULL)
        return()
      }
      
      # ── Validate ID / Dam / Sire are distinct (duplicate = cascading crashes) ─
      key_cols <- c(input$idcol, input$damcol, input$sirecol)
      if (anyDuplicated(key_cols)) {
        dupes <- key_cols[duplicated(key_cols)]
        output$error_message <- renderText(
          paste0("<b style='color:red'>Error:</b> ID, Dam, and Sire must each be a different column. ",
                 "Duplicate selection: ", paste("<b>", dupes, "</b>", collapse = ", ")))
        ped(NULL)
        return()
      }
      
      # ── Validate dam/sire columns are not swapped ────────────────────────────
      ped_df   <- as.data.frame(rped())
      dam_ids  <- unique(na.omit(ped_df[[input$damcol]]))
      sire_ids <- unique(na.omit(ped_df[[input$sirecol]]))
      
      known_sex <- c(input$femped, input$maleped)
      
      dam_lookup  <- ped_df[ped_df[[input$idcol]] %in% dam_ids,
                            c(input$idcol, input$sexcol)]
      sire_lookup <- ped_df[ped_df[[input$idcol]] %in% sire_ids,
                            c(input$idcol, input$sexcol)]
      
      dam_lookup  <- dam_lookup[!is.na(dam_lookup[[input$sexcol]])  &
                                  dam_lookup[[input$sexcol]]  %in% known_sex, ]
      sire_lookup <- sire_lookup[!is.na(sire_lookup[[input$sexcol]]) &
                                   sire_lookup[[input$sexcol]] %in% known_sex, ]
      
      male_dams   <- dam_lookup[[input$idcol]][dam_lookup[[input$sexcol]]  == input$maleped]
      female_sires <- sire_lookup[[input$idcol]][sire_lookup[[input$sexcol]] == input$femped]
      
      pct_male_as_dam    <- if (nrow(dam_lookup)  > 0) mean(dam_lookup[[input$sexcol]]  == input$maleped) else 0
      pct_female_as_sire <- if (nrow(sire_lookup) > 0) mean(sire_lookup[[input$sexcol]] == input$femped)  else 0
      
      if (pct_male_as_dam > 0.5 || pct_female_as_sire > 0.5) {
        male_dam_str    <- if (length(male_dams)    > 0)
          paste0("<br><b>Males recorded as dam:</b> ",   paste(male_dams,    collapse = ", "))
        else ""
        female_sire_str <- if (length(female_sires) > 0)
          paste0("<br><b>Females recorded as sire:</b> ", paste(female_sires, collapse = ", "))
        else ""
        output$error_message <- renderText(paste0(
          "<b style='color:red'>Error:</b> Dam and sire columns may be swapped — ",
          round(pct_male_as_dam    * 100), "% of dams are male, ",
          round(pct_female_as_sire * 100), "% of sires are female.",
          male_dam_str, female_sire_str
        ))
        ped(NULL)
        return()
      }
      
      output$error_message <- renderText("")  # clear any previous error
      
      # ── QC pass: capture all issues from the raw upload BEFORE cleaning ───
      ped_raw <- as.data.frame(rped())
      
      # 1. Duplicate IDs — keep first occurrence, record which rows were dropped
      dup_ids <- ped_raw[[input$idcol]][duplicated(ped_raw[[input$idcol]])]
      ped_raw <- ped_raw[!duplicated(ped_raw[[input$idcol]]), ]
      
      # 2. Sex-role mismatches — capture before cleanped() silently corrects them
      sex_lkp      <- setNames(as.character(ped_raw[[input$sexcol]]),
                               as.character(ped_raw[[input$idcol]]))
      dam_ids_raw  <- unique(na.omit(as.character(ped_raw[[input$damcol]])))
      sire_ids_raw <- unique(na.omit(as.character(ped_raw[[input$sirecol]])))
      
      male_as_dam    <- dam_ids_raw[
        !is.na(sex_lkp[dam_ids_raw]) & sex_lkp[dam_ids_raw] == input$maleped]
      female_as_sire <- sire_ids_raw[
        !is.na(sex_lkp[sire_ids_raw]) & sex_lkp[sire_ids_raw] == input$femped]
      
      pedsex$fem  <- input$femped
      pedsex$male <- input$maleped
      
      # Propagate column-name metadata to rd immediately
      rd$pedid   <- input$idcol
      rd$peddam  <- input$damcol
      rd$pedsire <- input$sirecol
      rd$pedsex  <- input$sexcol
      rd$pedF    <- input$femped
      rd$pedM    <- input$maleped
      
      output$ped_complete <- renderText({
        if (!is.null(rdata()) && !is.null(input$iddatacol)) {
          missing_ids <- rdata()[[input$iddatacol]][
            !rdata()[[input$iddatacol]] %in% ped_raw[[input$idcol]]]
          if (length(missing_ids) == 0)
            "All individuals are in the pedigree"
          else
            paste("Note: The following individuals are not in the pedigree:",
                  paste("<B>", missing_ids, "</B>", collapse = ", "))
        }
      })
      
      # Pass the already-deduplicated ped_raw to cleanped
      tryCatch({
        output$error_message <- renderText({ })
        clean_ped <- cleanped(ped_raw, input$idcol, input$damcol, input$sirecol,
                              input$sexcol, pedsex$fem, pedsex$male)
        ped(clean_ped[, c(input$idcol, input$damcol, input$sirecol, input$sexcol)])
      }, error = function(e) {
        output$error_message <- renderText({
          paste("<B>Error:", e$message)
        })
        ped(NULL)
      })
      
      req(ped())
      output$ped_submitted <- renderText({ "Ped Successfully Submitted!" })
      
      # ── QC report rendered directly below the submit confirmation ─────────
      output$ped_qc_report <- renderUI({
        
        qc_item <- function(colour, title, body_html) {
          tags$div(
            style = paste0(
              "border-left:3px solid ", colour, "; background:#fafafa; ",
              "padding:7px 11px; margin-bottom:7px; ",
              "border-radius:0 4px 4px 0; font-size:0.82rem"
            ),
            tags$b(style = paste0("color:", colour), title),
            tags$br(),
            HTML(body_html)
          )
        }
        
        if (length(dup_ids) == 0 &&
            length(male_as_dam) == 0 &&
            length(female_as_sire) == 0) {
          return(tags$div(
            style = "font-size:0.82rem; color:#2e7a3a; margin-top:4px",
            "\u2705 No pedigree inconsistencies detected."
          ))
        }
        
        items <- list()
        
        if (length(dup_ids) > 0)
          items <- c(items, list(qc_item(
            "#e65100",
            paste0("\u26a0\ufe0f Duplicate ID",
                   if (length(dup_ids) > 1) "s" else "",
                   " removed (", length(dup_ids), ")"),
            paste0(
              if (length(dup_ids) > 1) "These IDs appeared" else "This ID appeared",
              " more than once. The first occurrence was kept; ",
              "all later rows were dropped:<br>",
              paste("<b>", dup_ids, "</b>", collapse = ", ")
            )
          )))
        
        if (length(male_as_dam) > 0)
          items <- c(items, list(qc_item(
            "#b06000",
            paste0("\u26a0\ufe0f Sex corrected \u2014 ",
                   length(male_as_dam), " dam",
                   if (length(male_as_dam) > 1) "s" else "",
                   " recorded as ", input$maleped),
            paste0(
              if (length(male_as_dam) > 1) "These animals appear" else "This animal appears",
              " in the <b>dam</b> column but ",
              if (length(male_as_dam) > 1) "were" else "was",
              " recorded as <b>", input$maleped, "</b>. ",
              "Sex has been set to <b>", input$femped, "</b>. ",
              "If the parentage is actually swapped rather than the sex being wrong, ",
              "correct the source file and re-upload.<br>",
              paste("<b>", male_as_dam, "</b>", collapse = ", ")
            )
          )))
        
        if (length(female_as_sire) > 0)
          items <- c(items, list(qc_item(
            "#b06000",
            paste0("\u26a0\ufe0f Sex corrected \u2014 ",
                   length(female_as_sire), " sire",
                   if (length(female_as_sire) > 1) "s" else "",
                   " recorded as ", input$femped),
            paste0(
              if (length(female_as_sire) > 1) "These animals appear" else "This animal appears",
              " in the <b>sire</b> column but ",
              if (length(female_as_sire) > 1) "were" else "was",
              " recorded as <b>", input$femped, "</b>. ",
              "Sex has been set to <b>", input$maleped, "</b>. ",
              "If the parentage is actually swapped rather than the sex being wrong, ",
              "correct the source file and re-upload.<br>",
              paste("<b>", female_as_sire, "</b>", collapse = ", ")
            )
          )))
        
        tags$div(
          style = "margin-top:8px",
          tags$div(
            style = paste0("font-size:0.78rem; font-weight:700; text-transform:uppercase; ",
                           "letter-spacing:0.06em; color:#888; margin-bottom:6px"),
            "Pedigree QC report"
          ),
          tagList(items)
        )
      })
      
      output$view_ped_link <- renderUI({
        actionLink(ns("go_to_pedigree_table"), "View Pedigree Table")
      })
      
      output$pedprev <- renderDataTable({
        datatable(apply(ped(), 2, rev), style = "bootstrap",
                  options = list(paging = FALSE, scrollY = "800px",
                                 scrollX = "100%", scrollCollapse = TRUE))
      })
    })
    
    # ── Navigation links (registered once, not inside observeEvent) ───────────
    observeEvent(input$go_to_data_table, {
      updateTabsetPanel(session, "main_tabs",      selected = "View Data")
      updateTabsetPanel(session, "view_data_tabs", selected = "Data Table")
    })
    
    observeEvent(input$go_to_pedigree_table, {
      updateTabsetPanel(session, "main_tabs",      selected = "View Data")
      updateTabsetPanel(session, "view_data_tabs", selected = "Pedigree Table")
    })
    
    # ══════════════════════════════════════════════════════════════════════════
    # Downstream computations (all triggered reactively once data/ped are ready)
    # ══════════════════════════════════════════════════════════════════════════
    
    # ── Merge ownpop + pedigree parents ───────────────────────────────────────
    observe({
      req(ownpop(), ped(), input$idcol, input$damcol, input$sirecol, input$iddatacol)
      
      req(input$iddatacol %in% colnames(ownpop()),
          input$idcol     %in% colnames(ped()),
          input$sirecol   %in% colnames(ped()),
          input$damcol    %in% colnames(ped()))
      
      result <- tryCatch({
        m <- merge(
          ownpop()[, setdiff(colnames(ownpop()), c(input$sirecol, input$damcol))],
          ped()[, c(input$idcol, input$sirecol, input$damcol)],
          by.x = input$iddatacol, by.y = input$idcol,
          all.x = TRUE, all.y = FALSE
        )
        # Duplicate column names (e.g. data already had a "dam" column) would
        # crash every downstream tibble/group_by — deduplicate with a warning.
        if (anyDuplicated(colnames(m))) {
          dupes <- colnames(m)[duplicated(colnames(m))]
          showNotification(
            paste0("Column name conflict after merging: ",
                   paste(dupes, collapse = ", "),
                   ". Check that your data file does not already contain columns ",
                   "named the same as the pedigree's dam/sire columns."),
            type = "warning", duration = 10
          )
          colnames(m) <- make.unique(colnames(m))
        }
        m
      }, error = function(e) {
        showNotification("Error during merging: ensure column names match.", type = "error")
        NULL
      })
      
      merged(result)
    })
    
    # ── Kinship / generation depth / inbreeding / Ne / ECG / deltaF  (pedigree) ─────────────────────────────────
    observe({
      req( ped(), input$idcol   %in% colnames(ped()),
          input$sirecol %in% colnames(ped()),
          input$damcol  %in% colnames(ped()),
          input$sexcol  %in% colnames(ped()))
      
      sp <- ped()
      
      # ── Generation depth + Ne ─────────────────────────────────────────────────────
      gen_df  <- compute_gen_depth(sp, id_col   = input$idcol,
                                   sire_col = input$sirecol,
                                   dam_col  = input$damcol)
      Ne_res  <- compute_Ne_deltaF(
        setNames(2 * diag(rd$kinship) - 1, rownames(rd$kinship)),
        gen_df
      )

      # ── Write to rd ───────────────────────────────────────────────────────────
      rd$kinship    <- compute_kinship(sp, id_col   = input$idcol,
                                       sire_col = input$sirecol,
                                       dam_col  = input$damcol)
      rd$inbreeding <- setNames(2 * diag(rd$kinship) - 1, rownames(rd$kinship))
      rd$gen_depth  <- gen_df
      
      if (!is.null(ownpop())) {
        ref_ids  <- ownpop()[[input$iddatacol]]
        ref_F    <- rd$inbreeding[names(rd$inbreeding) %in% ref_ids]
        ref_gen  <- gen_df[gen_df$id %in% ref_ids, ]
        rd$ecg   <- as.numeric(mean(ref_gen$generation, na.rm = TRUE))
      } else {
        ref_F    <- rd$inbreeding
        ref_gen  <- gen_df
        rd$ecg   <- NA_real_
      }
      
      Ne_res  <- compute_Ne_deltaF(ref_F, ref_gen)
      
      rd$deltaF            <- as.numeric(Ne_res$deltaF)
      rd$se_dF             <- as.numeric(Ne_res$se_dF)
      rd$Ne                <- as.numeric(Ne_res$Ne)
      rd$se_Ne             <- as.numeric(Ne_res$se_Ne)
      
      
      rd$suggested_ext_pct <- if (is.finite(Ne_res$Ne)) {
        ne <- Ne_res$Ne
        as.integer(
          if      (ne < 25)              50L   # critical
          else if (ne < 50)              30L   # endangered  
          else if (ne < 100)             20L   # below recommended minimum
          else if (ne < 200)             10L   # adequate but not complacent
          else                            5L   # ok
        )
      } else NA_integer_
      
    })
    
    # ── Mean Litter Size ──────────────────────────────────────────────
    observe({
      req(ownpop(), input$dobdatacol, input$sexdatacol)
      
      req(input$dobdatacol %in% colnames(ownpop()),
          input$sexdatacol %in% colnames(ownpop()))
      
      sd <- ownpop()
      
      litters <- dplyr::summarize(dplyr::group_by_at(sd, input$dobdatacol), Littersize = dplyr::n())
      mls_val <- mean(litters$Littersize, na.rm = TRUE)
      mls(mls_val)
      
    })
    
    
    observeEvent(input$reset_session, {
      showModal(modalDialog(
        title = "Reset Session",
        "This will clear all uploaded data and settings. You will need to re-upload your files. Are you sure?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("reset_confirm"), "Yes, reset", class = "btn btn-danger")
        )
      ))
    })
    
    observeEvent(input$reset_confirm, {
      removeModal()
      ped(NULL);      rped(NULL)
      data(NULL);     rdata(NULL)
      ownpop(NULL);   breeders(NULL)
      merged(NULL);   mls(NULL)
      rd$ped         <- NULL;  rd$data        <- NULL
      rd$ownpop      <- NULL;  rd$breeders    <- NULL
      rd$merged      <- NULL;  rd$kinship     <- NULL
      rd$inbreeding  <- NULL;  rd$gen_depth   <- NULL
      rd$Ne          <- NULL;  rd$deltaF      <- NULL
      rd$ecg         <- NULL;  rd$mls         <- NULL
      rd$litters     <- NULL;  rd$dams        <- NULL
      rd$sires       <- NULL;  rd$litterplan  <- NULL
      rd$litterage   <- NULL;  rd$genint      <- NULL
      rd$pedid       <- NULL;  rd$peddam      <- NULL
      rd$pedsire     <- NULL;  rd$pedsex      <- NULL
      rd$pedF        <- NULL;  rd$pedM        <- NULL
      rd$datid       <- NULL;  rd$datdob      <- NULL
      rd$datsex      <- NULL;  rd$datF        <- NULL
      rd$datM        <- NULL;
      rd$suggested_ext_pct <- NULL
      rd$param_goals <- data.frame(
        variable = character(), var_class = character(),
        direction = character(), value = character(),
        priority = character(), stringsAsFactors = FALSE
      )
      output$data_submitted     <- renderText("")
      output$ped_submitted      <- renderText("")
      output$data_submit_summary <- renderUI(NULL)
      output$ped_qc_report      <- renderUI(NULL)
      output$dataprev           <- renderDataTable(NULL)
      output$pedprev            <- renderDataTable(NULL)
      showNotification("Session reset. Please re-upload your files.", 
                       type = "message", duration = 4)
    })
    
    return(rd)
  })
}
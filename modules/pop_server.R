# ── pop_server.R ──────────────────────────────────────────────────────────────
# Coordinator: owns all shared reactives and wires up the 8 panel helpers.
#
# Replaces (in server.R):
#   popOverviewLitterServer("popOverviewLitter", rd)
#   pupInfoServer("pupInfo", rd)
#   popParamsHealthServer("popParamsHealth", rd)
#   pedtreeServer("pedtree", rd)
#
# New call in server.R:
#   popServer("pop", rd)
#
# Shared reactives (computed once, passed to helpers):
#   selected_year      – reactiveVal for bar-chart click sync (Overview/Litters/Dams/Sires)
#   breeders_col_specified – was duplicated in pupInfoServer & popParamsHealthServer
#   litter_ov          – single source of truth for per-litter records
#   dam_table          – derived from litter_ov; also written to rd$dams
#   sire_table         – derived from litter_ov; also written to rd$sires
# Also writes: rd$litters, rd$litterplan, rd$litterage, rd$genint

source("modules/pop_overview_server.R")
source("modules/pop_litters_server.R")
source("modules/pop_dams_server.R")
source("modules/pop_sires_server.R")
source("modules/pop_pup_info_server.R")
source("modules/pop_params_server.R")
source("modules/pop_breeders_server.R")
source("modules/pop_pedtree_server.R")

popServer <- function(id, rd) {
  moduleServer(id, function(input, output, session) {
    
    # ══════════════════════════════════════════════════════════════════════════
    # SHARED STATE
    # ══════════════════════════════════════════════════════════════════════════
    
    # Year highlighted by clicking any bar chart (Overview / Litters / Dams / Sires).
    # Passed to all four helpers so clicking in one tab syncs all others.
    selected_year <- reactiveVal(NULL)
    
    # ── breeders_col_specified ────────────────────────────────────────────────
    # Was independently defined in both pupInfoServer and popParamsHealthServer.
    breeders_col_specified <- reactive({
      req(rd$breeders, rd$data)
      !setequal(rd$breeders[[rd$datid]], rd$data[[rd$datid]])
    })
    
    # ── litter_ov ─────────────────────────────────────────────────────────────
    # Single source of truth for per-litter records.
    # All mean-litter-size and sex-ratio KPIs derive from this reactive,
    # guaranteeing consistency across Overview, Litters, Dams, Sires panels.
    litter_ov <- reactive({
      req(rd$merged)
      ov <- ungroup(summarize(
        group_by(rd$merged, !!sym(rd$datdob), BirthYear, dob,
                 !!sym(rd$peddam), !!sym(rd$pedsire)),
        Littersize  = n(),
        `# Females` = sum(!!sym(rd$datsex) == rd$datF),
        `# Males`   = sum(!!sym(rd$datsex) == rd$datM)
      ))
      colnames(ov)[colnames(ov) == rd$peddam]  <- "Dam"
      colnames(ov)[colnames(ov) == rd$pedsire] <- "Sire"
      ov
    })
    
    observe({ req(litter_ov()); rd$litters <- litter_ov() })
    
    # ── dam_table ─────────────────────────────────────────────────────────────
    dam_table <- reactive({
      req(litter_ov(), rd$data)
      ov <- litter_ov()
      D <- summarize(group_by(ov, Dam),
                     `# Litters`          = n(),
                     `Average Littersize` = round(mean(Littersize), 2),
                     `# Progeny`          = sum(Littersize),
                     `# Females`          = sum(`# Females`),
                     `# Males`            = sum(`# Males`))
      D <- merge(D, rd$data[, c(rd$datid, rd$datdob, "BirthYear", "dob")],
                 by.x = "Dam", by.y = rd$datid, all.x = TRUE, all.y = FALSE)
      D <- D[, c("Dam", rd$datdob, setdiff(colnames(D), c("Dam", rd$datdob)))]
      colnames(D)[colnames(D) == "dob"] <- "dobmom"
      D
    })
    
    observe({ req(dam_table()); rd$dams <- dam_table() })
    
    # ── sire_table ────────────────────────────────────────────────────────────
    sire_table <- reactive({
      req(litter_ov(), rd$data)
      ov <- litter_ov()
      S <- summarize(group_by(ov, Sire),
                     `# Litters`          = n(),
                     `Average Littersize` = round(mean(Littersize), 2),
                     `# Progeny`          = sum(Littersize),
                     `# Females`          = sum(`# Females`),
                     `# Males`            = sum(`# Males`))
      S <- merge(S, rd$data[, c(rd$datid, rd$datdob, "BirthYear")],
                 by.x = "Sire", by.y = rd$datid, all.x = TRUE, all.y = FALSE)
      S[, c("Sire", rd$datdob, setdiff(colnames(S), c("Sire", rd$datdob)))]
    })
    
    observe({ req(sire_table()); rd$sires <- sire_table() })
    
    # ── Generation interval ───────────────────────────────────────────────────
    
    genint_notified <- reactiveVal(FALSE)               # reset on new data
    observeEvent(rd$data, { genint_notified(FALSE) })
    
    observe({
      req(litter_ov(), dam_table(), rd$ownpop)
      all_ids         <- rd$data[[rd$datid]]
      own_ids         <- rd$ownpop[[rd$datid]]
      ownpop_filtered <- !setequal(own_ids, all_ids)
      
      ov <- if (ownpop_filtered)
        litter_ov()[litter_ov()$Dam %in% own_ids | litter_ov()$Sire %in% own_ids, ]
      else
        litter_ov()
      
      D         <- dam_table()
      ov_merged <- merge(ov, D[, c("Dam", "dobmom")], by = "Dam")
      
      rd$litterplan <- ov_merged[!is.na(ov_merged$dobmom), ]
      
      n_litters     <- nrow(ov_merged)
      n_missing_dob <- sum(is.na(ov_merged$dobmom))
      
      # ── Fire at most one notification per data submission ─────────────────
      if (!genint_notified()) {
        if (n_litters == 0) {
          showNotification(
            id = "genint_no_dob",
            ui = tagList(
              tags$b("Generation interval could not be computed."),
              tags$br(),
              "No litters could be matched between the data file and the pedigree. ",
              "Check that the Animal ID column in the data file matches the pedigree exactly ",
              "(same spelling, same capitalisation)."
            ),
            type = "warning", duration = 15
          )
          genint_notified(TRUE)
          rd$litterage <- NULL; rd$genint <- NA_real_
          return()
        }
        
        if (n_missing_dob == n_litters) {
          unmatched_dams <- unique(ov_merged$Dam[is.na(ov_merged$dobmom)])
          n_show     <- min(5, length(unmatched_dams))
          example_ids <- paste(head(unmatched_dams, n_show), collapse = ", ")
          more_str   <- if (length(unmatched_dams) > n_show)
            paste0(" … and ", length(unmatched_dams) - n_show, " more") else ""
          showNotification(
            id = "genint_non_dob",
            ui = tagList(
              tags$b("Generation interval could not be computed."),
              tags$br(),
              "None of the dams could be matched to a date of birth in the data file.",
              tags$ul(
                tags$li("Check that Animal ID columns match between files (spelling, capitalisation)."),
                tags$li("Check that dams in the pedigree are present in the data file.")
              ),
              tags$b("Example unmatched dam IDs: "),
              tags$code(paste0(example_ids, more_str))
            ),
            type = "warning", duration = 20
          )
          genint_notified(TRUE)
          rd$litterage <- NULL; rd$genint <- NA_real_
          return()
        }
        
        if (n_missing_dob > 0) {
          pct <- round(100 * n_missing_dob / n_litters)
          showNotification(
            id = "genint_partial_dob",
            ui = tagList(
              tags$b(paste0("Dam Planner: ", pct, "% of litters have no dam DOB.")),
              tags$br(),
              paste0(n_missing_dob, " of ", n_litters, " litters could not be linked to a dam ",
                     "date of birth and are excluded from planning calculations. ",
                     "Check that all breeding dams are present in the data file with a valid date of birth.")
            ),
            type = "warning", duration = 12
          )
          genint_notified(TRUE)
        }
      }
      
      ov_age <- ov_merged %>%
        filter(!is.na(dobmom)) %>%
        mutate(AgeAtLitter = as.numeric(difftime(dob, dobmom, units = "days")) / 365.25)
      
      ov_num <- ov_age %>%
        arrange(Dam, dob) %>%
        select(Dam, Littersize, AgeAtLitter) %>%
        group_by(Dam) %>%
        mutate(LitterNumber = row_number())
      
      max_litter <- max(ov_num$LitterNumber, na.rm = TRUE)
      
      if (!is.finite(max_litter) || max_litter < 1L) {
        rd$litterage <- NULL; rd$genint <- NA_real_
        return()
      }
      
      ov_fin <- ov_num %>%
        pivot_wider(names_from  = LitterNumber,
                    values_from = c(Littersize, AgeAtLitter),
                    names_glue  = "{.value}_Litter{LitterNumber}")
      
      ov_av        <- colMeans(replace(ov_fin[, -1], ov_fin[, -1] == 0, NA), na.rm = TRUE)
      rd$litterage <- ov_av
      
      litter_ids <- paste0("Litter", seq_len(max_litter))
      L <- sum(ov_av[paste0("AgeAtLitter_", litter_ids)] *
                 ov_av[paste0("Littersize_",  litter_ids)]) /
        sum(ov_av[paste0("Littersize_",  litter_ids)])
      rd$genint <- L
    })
    observeEvent(input$clear_year_filter, { selected_year(NULL) })
    
    # ══════════════════════════════════════════════════════════════════════════
    # CALL PANEL HELPERS
    # ══════════════════════════════════════════════════════════════════════════
    
    popOverviewHelper(
      input, output, session, rd,
      litter_ov     = litter_ov,
      dam_table     = dam_table,
      sire_table    = sire_table,
      selected_year = selected_year
    )
    
    popLittersHelper(
      input, output, session, rd,
      litter_ov     = litter_ov,
      selected_year = selected_year
    )
    
    popDamsHelper(
      input, output, session, rd,
      litter_ov     = litter_ov,
      dam_table     = dam_table,
      selected_year = selected_year
    )
    
    popSiresHelper(
      input, output, session, rd,
      litter_ov     = litter_ov,
      sire_table    = sire_table,
      selected_year = selected_year
    )
    
    popPupInfoHelper(
      input, output, session, rd,
      breeders_col_specified = breeders_col_specified
    )
    
    popParamsHelper(
      input, output, session, rd
    )
    
    popBreedersHelper(
      input, output, session, rd,
      breeders_col_specified = breeders_col_specified
    )
    
    popPedtreeHelper(
      input, output, session, rd
    )
  })
}
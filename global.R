## Load Libraries 
library(shiny)
library(shinyjs)
library(shinyFeedback)
library(shinyWidgets)
library(bslib)
library(bsicons) 
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(DT)
library(thematic)
library(pedigree)
library(kinship2)
library(ggplot2)
library(plotly)
library(lubridate)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(sortable) 
library(ragg)



# Handling Windows encoding issue
if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", locale = "English_United States.UTF-8")
  options(encoding = "UTF-8")
}


#####################################################
#### FUNCTIONS ######################################
#####################################################

# NA synonyms for read_excel
NA_STRINGS <- c(
  "", " ", "  ", "\t", "\r", "\n", "\r\n",
  "\u00A0", "\u200B", "\u200C", "\u200D", "\uFEFF",
  "NA", "N/A", "n/a", "na", "NA ", " NA", "N A",
  "NULL", "Null", "null",
  "#N/A", "#N/A N/A", "#VALUE!",
  "none", "None", "NONE",
  "missing", "Missing", "MISSING"
)

# ── autodetect_cols.R ────────────────────────────────────────────────────────

# NULL-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Match first column name against regex patterns (case-insensitive)
match_col <- function(cols, patterns, from = cols) {
  from_norm <- gsub("[^a-zA-Z0-9]", "_", from)  # spaces/dots/etc → underscore
  for (pat in patterns) {
    hit_idx <- grep(pat, from_norm, ignore.case = TRUE)
    if (length(hit_idx) > 0) return(from[hit_idx[1]])  # return original name
  }
  NULL
}

# Find first column whose values look like dates
detect_date_col <- function(df, exclude = character(0)) {
  candidates <- setdiff(colnames(df), exclude)
  for (col in candidates) {
    x <- df[[col]]
    # Already a Date/POSIXct class (readxl will do this automatically)
    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return(col)
    # Try parsing a sample of character values
    sample_vals <- na.omit(as.character(x))[seq_len(min(20, length(na.omit(as.character(x)))))]
    if (length(sample_vals) == 0) next
    parsed <- suppressWarnings(
      as.Date(sample_vals,
              tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y",
                             "%Y/%m/%d", "%d-%m-%Y"))
    )
    if (mean(!is.na(parsed)) > 0.5) return(col)
  }
  NULL
}

# Detect sex labels (female / male) within the unique values of a sex column
detect_sex_labels <- function(df, sex_col) {
  vals <- sort(unique(na.omit(as.character(df[[sex_col]]))))
  if (length(vals) < 2) return(list(fem = vals[1], male = vals[2]))
  
  fem_patterns  <- c("^f$", "^female$", "^v$", "^vrouw$", "^teef$", "^females$", "^f\\b")
  male_patterns <- c("^m$", "^male$", "^h$", "^hond$", "^reu$",   "^males$",  "^m\\b")
  
  fem_hit  <- match_col(vals, fem_patterns,  from = vals)
  male_hit <- match_col(vals, male_patterns, from = vals)
  
  list(
    fem  = fem_hit  %||% vals[1],
    male = male_hit %||% vals[min(2, length(vals))]
  )
}

# ── Data file autodetect ─────────────────────────────────────────────────────
autodetect_data_cols <- function(df) {
  cols <- colnames(df)
  
  id_col <- match_col(cols, c(
    "^animal[._-]?id$", "^dog[._-]?id$", "^id$",
    "^name$", "^animal$", "^code$"
  )) %||% cols[1]
  
  dob_col <-
    match_col(cols, c(
      "^dob$", "birth[ ._-]?date", "date[ ._-]?of[ ._-]?birth",
      "geboortedatum", "^born$", "^birthdate$"
    )) %||%
    detect_date_col(df, exclude = id_col) %||%
    cols[1]
  
  sex_col <- match_col(cols, c(
    "^sex$", "^gender$", "^geslacht$", "^m[ /_-]?f$"
  )) %||% cols[1]
  
  own_col <- match_col(cols, c(
    "^own[ ._-]?pop", "in[ ._-]?program", "^population$",
    "^program$", "^eigen$", "^own$"
  )) %||% "None"
  
  breed_col <- match_col(cols, c(
    "^breed", "^stud$", "breedingstock", "^fokker", "^fok$",
    "^active[ ._-]?breed", "^breeding$"
  )) %||% "None"
  
  retire_col <- match_col(cols, c(
    "^pensioen$", "^retire", "^retirement$", "^pension$",
    "^uitstroom$", "^afvoer$", "^end[ ._-]?date$", "^exit$"
  )) %||% "None"
  
  list(id = id_col, dob = dob_col, sex = sex_col,
       own = own_col, breed = breed_col, retire = retire_col,
       any_detected = !is.null(match_col(cols, c(
         "^animal[ ._-]?id$","^dog[ ._-]?id$","^id$","^name$","^animal$","^code$",
         "^dob$","birth[ ._-]?date","date[ ._-]?of[ ._-]?birth",
         "^sex$","^gender$","^geslacht$", "^pensioen$", "^retire"
       ))))
}

# ── Pedigree file autodetect ─────────────────────────────────────────────────
autodetect_ped_cols <- function(df) {
  cols <- colnames(df)
  
  id_col <- match_col(cols, c(
    "^animal[._-]?id$", "^dog[._-]?id$", "^id$",
    "^name$", "^animal$", "^code$"
  )) %||% cols[1]
  
  sire_col <- match_col(cols, c(
    "^sire$", "^father$", "^vader$", "^dad$",
    "^sire[._-]?id$", "^father[._-]?id$"
  )) %||% cols[1]
  
  dam_col <- match_col(cols, c(
    "^dam$", "^mother$", "^moeder$", "^mom$",
    "^dam[._-]?id$", "^mother[._-]?id$"
  )) %||% cols[1]
  
  sex_col <- match_col(cols, c(
    "^sex$", "^gender$", "^geslacht$", "^m[/_-]?f$"
  )) %||% cols[1]
  
  list(id = id_col, sire = sire_col, dam = dam_col, sex = sex_col,
       any_detected = !is.null(match_col(cols, c(
         "^animal[._-]?id$","^dog[._-]?id$","^id$","^name$","^animal$","^code$",
         "^sire$","^father$","^vader$","^dam$","^mother$","^moeder$",
         "^sex$","^gender$","^geslacht$"
       ))))
}

# ── parse_dates_robust ────────────────────────────────────────────────────────
parse_dates_robust <- function(x, user_format = "%Y-%m-%d") {
  
  # ── Already a Date ─────────────────────────────────────────────────────────
  if (inherits(x, "Date")) return(x)
  
  # ── POSIXct / POSIXlt ─────────────────────────────────────────────────────
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))
  
  # ── Numeric ───────────────────────────────────────────────────────────────
  if (is.numeric(x)) {
    non_na <- x[!is.na(x)]
    if (length(non_na) == 0) return(as.Date(rep(NA_character_, length(x))))
    
    med <- stats::median(non_na)
    
    if (med < 100000) {
      # Excel date serial: days since 1899-12-30
      return(as.Date(x, origin = "1899-12-30"))
      
    } else if (med < 1e10) {
      # Unix timestamp in seconds (e.g. 1372550400 ≈ 2013-06-30)
      return(as.Date(
        as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
      ))
      
    } else {
      # Unix timestamp in milliseconds
      return(as.Date(
        as.POSIXct(x / 1000, origin = "1970-01-01", tz = "UTC")
      ))
    }
  }
  
  # ── Character / factor ────────────────────────────────────────────────────
  x <- as.character(x)
  n_valid <- sum(!is.na(x) & x != "")
  
  # Helper: try a format, return result only if it parses ≥80% of non-NA rows
  try_fmt <- function(fmt) {
    r <- suppressWarnings(as.Date(x, format = fmt))
    if (n_valid > 0 && sum(!is.na(r)) / n_valid >= 0.8) r else NULL
  }
  
  # Try user format first, then common fallbacks
  fallbacks <- c(
    user_format,
    "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y",
    "%d-%m-%Y", "%Y/%m/%d", "%d.%m.%Y",
    "%Y%m%d",   "%d %b %Y", "%d %B %Y"
  )
  for (fmt in unique(fallbacks)) {
    res <- try_fmt(fmt)
    if (!is.null(res)) return(res)
  }
  
  # Last resort: lubridate (handles many ambiguous formats)
  suppressWarnings(
    as.Date(lubridate::parse_date_time(
      x,
      orders = c("ymd", "dmy", "mdy", "ymd HM", "ymd HMS", "dmy HM")
    ))
  )
}

looks_numeric <- function(x) {
  x <- as.character(x)
  x_clean <- gsub("[\\s]", "", x, perl = TRUE)
  x_clean <- gsub("^[+-]", "", x_clean)
  
  # Detect separator pattern across the whole column
  has_period_comma <- any(grepl("\\d\\.\\d{3},", x_clean), na.rm = TRUE)  # 1.500,25 → EU thousands
  has_comma_period <- any(grepl("\\d,\\d{3}\\.", x_clean), na.rm = TRUE)  # 1,500.25 → US thousands
  
  if (has_period_comma) {
    x_clean <- gsub("\\.", "", x_clean, fixed = TRUE)   # strip EU thousands
    x_clean <- gsub(",", ".", x_clean, fixed = TRUE)    # decimal comma → period
  } else if (has_comma_period) {
    x_clean <- gsub(",", "", x_clean, fixed = TRUE)     # strip US thousands
  } else {
    # Only comma, no period → comma is decimal separator
    x_clean <- gsub(",", ".", x_clean, fixed = TRUE)
  }
  
  non_na <- x_clean[!is.na(x) & x != ""]
  if (length(non_na) == 0) return(FALSE)
  parsed <- suppressWarnings(as.numeric(non_na))
  mean(!is.na(parsed)) >= 0.8
}

# clean data 
cleandata <- function(data, idcol, sexcol, dobcol, dobformat) {
  data[[sexcol]] <- as.factor(data[[sexcol]])
  
  parsed           <- parse_dates_robust(data[[dobcol]], dobformat)
  data[[dobcol]]   <- as.character(parsed)   # store as ISO string for consistency
  data$dob         <- parsed                 # keep a true Date column too
  
  data <- data[!is.na(data[[idcol]]) & data[[idcol]] != "", ]
  data <- unique(data)
  
  skip_cols <- c(idcol, sexcol, dobcol)
  
  for (col in setdiff(names(data), skip_cols)) {
    if (!is.numeric(data[[col]]) && looks_numeric(data[[col]])) {
      x <- as.character(data[[col]])
      x <- gsub("[\\s]", "", x, perl = TRUE)
      
      has_period_comma <- any(grepl("\\d\\.\\d{3},", x), na.rm = TRUE)
      has_comma_period <- any(grepl("\\d,\\d{3}\\.", x), na.rm = TRUE)
      
      if (has_period_comma) {
        x <- gsub("\\.", "", x, fixed = TRUE)
        x <- gsub(",", ".", x, fixed = TRUE)
      } else if (has_comma_period) {
        x <- gsub(",", "", x, fixed = TRUE)
      } else {
        x <- gsub(",", ".", x, fixed = TRUE)
      }
      
      data[[col]] <- suppressWarnings(as.numeric(x))
    }
  }
  
  return(data)
}

# Clean pedigree 
cleanped <- function(ped, id, dam, sire, sex, fem, male) {
  
  # Trim whitespace
  for (col in c(id, dam, sire, sex)) {
    ped[[col]] <- trimws(ped[[col]], whitespace = "[\\h\\v]")
  }
  
  ped         <- unique(ped)
  ped[[dam]]  <- sub("Unknown", NA, ped[[dam]])
  ped[[sire]] <- sub("Unknown", NA, ped[[sire]])
  
  # Replace empty strings with NA safely
  ped[[dam]][!is.na(ped[[dam]])  & ped[[dam]]  == ""] <- NA
  ped[[sire]][!is.na(ped[[sire]]) & ped[[sire]] == ""] <- NA
  
  # Null out single parents 
  single_parent <- xor(is.na(ped[[dam]]), is.na(ped[[sire]]))
  ped[[dam]][single_parent]  <- NA
  ped[[sire]][single_parent] <- NA
  
  # remove rows where the ID itself is missing
  ped1 <- ped[!is.na(ped[[id]]), ]
  ped1 <- unique(ped1)
  
  ped11 <- add.Inds(as.data.frame(ped1))
  
  # Assign sex based on parent roles
  for (i in seq_len(nrow(ped11))) {
    if (ped11[i, id] %in% ped11[[dam]]) {
      ped11[i, sex] <- fem
    } else if (ped11[i, id] %in% ped11[[sire]]) {
      ped11[i, sex] <- male
    }
  }
  
  ped11[order(orderPed(ped11)), ]
}


compute_kinship <- function(ped, id_col = "Indiv", sire_col = "Sire", dam_col = "Dam") {
  ids   <- as.character(ped[[id_col]])
  n     <- length(ids)
  idx   <- setNames(seq_len(n), ids)
  sires <- as.character(ped[[sire_col]])
  dams  <- as.character(ped[[dam_col]])
  
  sires[is.na(sires) | sires == "" | sires == "0"] <- NA
  dams [is.na(dams)  | dams  == "" | dams  == "0"] <- NA
  
  K <- matrix(0, n, n, dimnames = list(ids, ids))
  
  for (i in seq_len(n)) {
    si <- if (!is.na(sires[i]) && sires[i] %in% ids) idx[[sires[i]]] else NA_integer_
    di <- if (!is.na(dams[i])  && dams[i]  %in% ids) idx[[dams[i]]]  else NA_integer_
    
    K[i, i] <- (1 + if (!is.na(si) && !is.na(di)) K[si, di] else 0) / 2
    
    if (i > 1L) {
      js       <- seq_len(i - 1L)
      K[i, js] <- ((if (!is.na(si)) K[si, js] else 0) +
                     (if (!is.na(di)) K[di, js] else 0)) / 2
      K[js, i] <- K[i, js]
    }
  }
  K
}

compute_gen_depth <- function(ped, id_col, sire_col, dam_col) {
  ids   <- as.character(ped[[id_col]])
  sires <- as.character(ped[[sire_col]])
  dams  <- as.character(ped[[dam_col]])
  t     <- setNames(numeric(length(ids)), ids)
  
  for (i in seq_along(ids)) {
    s_known  <- !is.na(sires[i]) && sires[i] %in% ids
    d_known  <- !is.na(dams[i])  && dams[i]  %in% ids
    t[ids[i]] <- (if (s_known) (1 + t[sires[i]]) / 2 else 0) +
      (if (d_known) (1 + t[dams[i]])  / 2 else 0)
  }
  data.frame(id = ids, generation = t, stringsAsFactors = FALSE)
}

compute_Ne_deltaF <- function(inbreeding, gen_depth_df) {
  ids    <- gen_depth_df$id
  F_vals <- inbreeding[ids]
  t_vals <- gen_depth_df$generation
  
  valid  <- !is.na(F_vals) & !is.na(t_vals) & t_vals > 0
  if (sum(valid) < 20)
    return(list(Ne = NA_real_, deltaF = NA_real_, se_Ne = NA_real_))
  
  dF     <- 1 - (1 - F_vals[valid])^(1 / t_vals[valid])
  mean_dF <- mean(dF, na.rm = TRUE)
  se_dF   <- sd(dF, na.rm = TRUE) / sqrt(sum(valid))
  
  list(
    deltaF = mean_dF,
    se_dF  = se_dF,
    Ne     = 1 / (2 * mean_dF),
    se_Ne  = se_dF / (2 * mean_dF^2)
  )
}

is_num_var  <- function(x) is.numeric(x) || is.integer(x)

is_cat_var  <- function(x) is.character(x) || is.factor(x) || inherits(x, c("Date","POSIXct"))


dir_to_long <- c(
  "\u2264" = "Lower Than Or Equal To",
  "\u2265" = "Higher Than Or Equal To",
  "="      = "Equals",
  "\u2260" = "Does Not Equal"
)


apply_goal_filter <- function(col, direction, value) {
  switch(direction,
         "\u2264" = !is.na(col) & as.numeric(col) <= as.numeric(value),
         "\u2265" = !is.na(col) & as.numeric(col) >= as.numeric(value),
         "="      = !is.na(col) & as.character(col) == value,
         "\u2260" = !is.na(col) & as.character(col) != value,
         rep(TRUE, length(col))
  )
}


# table with distance from the mean
transform_to_delta <- function(df, colsofintvars, crit_inputs, data, var_classes,
                               ref_vals = NULL, ref_label = "\u03bc") {
  if (is.null(df) || nrow(df) == 0 ||
      is.null(colsofintvars) || length(colsofintvars) == 0 ||
      is.null(data)) return(df)
  
  transform_cols <- colsofintvars[colsofintvars %in% names(df) &
                                    colsofintvars %in% names(data)]
  
  for (col in transform_cols) {
    var_class <- var_classes[[col]]
    crit      <- crit_inputs[[paste0("crit_", col)]]
    crit_val  <- crit_inputs[[paste0("value_", col)]]
    
    if (var_class %in% c("numeric", "integer")) {
      pop_mean <- if (!is.null(ref_vals) && !is.null(ref_vals[[col]]))
        as.numeric(ref_vals[[col]])
      else
        mean(as.numeric(data[[col]]), na.rm = TRUE)
      df[[paste0(col, "_sort")]] <- as.numeric(df[[col]])
      higher_better <- !is.null(crit) && crit == "Higher Than Or Equal To"
      lower_better  <- !is.null(crit) && crit == "Lower Than Or Equal To"
      directional   <- higher_better || lower_better
      
      df[[col]] <- vapply(df[[col]], function(v) {
        if (is.na(v)) return("<span style='color:#aaa'>\u2014</span>")
        v     <- as.numeric(v)
        delta <- v - pop_mean
        arrow <- if (delta > 0) "\u25b2" else if (delta < 0) "\u25bc" else "\u25a0"
        color <- if (!directional) "#555"
        else if (delta == 0) "#888"
        else if ((higher_better && delta > 0) || (lower_better && delta < 0)) "#2e7a3a"
        else "#c62828"
        sprintf(
          paste0("<span style='font-weight:600; color:%s'>%s %+.2f</span>",
                 "<br><span style='color:#999; font-size:0.76rem'>",
                 "val&nbsp;%.2f &nbsp;%s&nbsp;%.2f</span>"),
          color, arrow, delta, v, ref_label, pop_mean 
        )
      }, character(1))
      
    } else if (var_class %in% c("character", "factor")) {
      has_crit <- !is.null(crit) && crit != "No Criterium" && !is.null(crit_val)
      
      df[[col]] <- vapply(df[[col]], function(v) {
        if (is.na(v)) return("<span style='color:#aaa'>\u2014</span>")
        v <- as.character(v)
        if (has_crit) {
          meets <- switch(crit,
                          "Equals"         = v == crit_val,
                          "Does Not Equal" = v != crit_val,
                          FALSE
          )
          color <- if (meets) "#2e7a3a" else "#c62828"
          icon  <- if (meets) "\u2714" else "\u2718"
          sprintf("<span style='color:%s; font-weight:600'>%s</span>&nbsp;%s",
                  color, icon, v)
        } else {
          sprintf("<span style='color:#555'>%s</span>", v)
        }
      }, character(1))
    }
  }
  df
}


make_delta_datatable <- function(df, colsofintvars, selection = "single") {
  col_names <- names(df)
  
  sort_col_names    <- col_names[grepl("_sort$", col_names)]
  display_col_names <- sub("_sort$", "", sort_col_names)
  
  sort_idx    <- match(sort_col_names,    col_names) - 1L
  display_idx <- match(display_col_names, col_names) - 1L
  
  col_defs <- list(
    list(targets = as.list(sort_idx), visible = FALSE)
  )
  
  for (i in seq_along(display_idx)) {
    col_defs <- c(col_defs, list(
      list(targets = display_idx[[i]], orderData = sort_idx[[i]])
    ))
  }
  
  cat_cols <- setdiff(colsofintvars[colsofintvars %in% col_names], display_col_names)
  cat_idx  <- match(cat_cols, col_names) - 1L
  if (length(cat_idx) > 0)
    col_defs <- c(col_defs, list(list(targets = as.list(cat_idx), orderable = FALSE)))
  
  # All transformed column indices for header tooltip
  all_delta_idx <- c(display_idx, cat_idx)
  delta_idx_js  <- paste0("[", paste(all_delta_idx, collapse = ","), "]")
  
  datatable(
    df,
    style     = "bootstrap",
    rownames  = FALSE,
    escape    = FALSE,
    selection = selection,
    options   = list(
      paging         = FALSE,
      scrollY        = "100%",
      scrollX        = "100%",
      scrollCollapse = TRUE,
      columnDefs     = col_defs
    )
  )
}

# full pedigree tree
generate_pedigree_kinship2 <- function(data, id, mother, father, sex,
                                       female_label, breeder = NULL,
                                       inbreeding_vec = NULL) {
  data <- data[!is.na(data[[id]]), c(id, mother, father, sex)]
  names(data) <- c("id", "mother", "father", "sex")
  
  data$sex    <- ifelse(data$sex == female_label, 2, 1)
  data$id     <- as.character(data$id)
  data$mother <- as.character(data$mother)
  data$father <- as.character(data$father)
  
  data$mother[data$mother %in% c("0", "")] <- NA
  data$father[data$father %in% c("0", "")] <- NA
  
  parent_ids      <- unique(c(data$mother, data$father))
  parent_ids      <- parent_ids[!is.na(parent_ids)]
  missing_parents <- setdiff(parent_ids, data$id)
  
  if (length(missing_parents) > 0) {
    extra <- data.frame(
      id     = missing_parents,
      mother = NA, father = NA,
      sex    = ifelse(missing_parents %in% data$mother, 2, 1)
    )
    data <- rbind(data, extra)
  }
  
  data$sex[data$id %in% data$mother] <- 2
  data$sex[data$id %in% data$father] <- 1
  
  single_parent <- xor(is.na(data$mother), is.na(data$father))
  data$mother[single_parent] <- NA
  data$father[single_parent] <- NA
  
  # ── Labels ─────────────────────────────────────────────────────────────────
  short_names <- substr(data$id, 1, 10)
  if (!is.null(inbreeding_vec)) {
    f_vals <- inbreeding_vec[data$id]
    labels <- ifelse(!is.na(f_vals),
                     paste0(short_names, "\nF", sprintf("%.2f", f_vals)),
                     short_names)
  } else {
    labels <- short_names
  }
  
  # ── Colors ─────────────────────────────────────────────────────────────────
  is_breeder <- if (!is.null(breeder)) data$id %in% breeder else rep(FALSE, nrow(data))
  is_mother  <- data$id %in% data$mother
  is_father  <- data$id %in% data$father
  
  colors <- ifelse(is_mother & is_breeder,  "#AD1457",   # deep pink  – active dam
                   ifelse(is_mother,              "#F48FB1",   # light pink – dam
                          ifelse(is_father & is_breeder, "#1565C0",   # deep blue  – active sire
                                 ifelse(is_father,              "#90CAF9",   # light blue – sire
                                        ifelse(data$sex == 2,          "#FCE4EC",   # pale pink  – other female
                                               "#E3F2FD"))))) # pale blue – other male
  
  ped <- pedigree(id       = data$id,
                  momid    = data$mother,
                  dadid    = data$father,
                  sex      = data$sex,
                  affected = rep(1, nrow(data)))
  
  op <- par(mar = c(2, 1, 2, 1), bg = "#FAFAFA")
  on.exit(par(op), add = TRUE)
  
  plot(ped,
       id         = labels,
       col        = colors,
       font       = 1,
       cex        = 0.18,
       symbolsize = 0.35,
       branch     = 0.5)
  
  legend("bottomleft",
         legend    = c("Active dam", "Active sire", "Dam", "Sire", "Female", "Male"),
         fill      = c("#AD1457", "#1565C0", "#F48FB1", "#90CAF9", "#FCE4EC", "#E3F2FD"),
         border    = "#cccccc",
         bty       = "n",
         cex       = 0.4,
         title     = "Legend",
         title.col = "#555555",
         xpd    = TRUE)
}


getAncestors <- function(ped, indname, id_col, dam_col, sire_col, gen) {
  if (gen == 0 || is.na(indname) || !(indname %in% ped[[id_col]])) return(c())
  parent_sire <- ped[[sire_col]][ped[[id_col]] == indname]
  parent_dam  <- ped[[dam_col]][ped[[id_col]]  == indname]
  ancestors   <- c(parent_sire, parent_dam)
  ancestors   <- c(ancestors,
                   getAncestors(ped, parent_sire, id_col, dam_col, sire_col, gen - 1),
                   getAncestors(ped, parent_dam,  id_col, dam_col, sire_col, gen - 1))
  unique(ancestors[!is.na(ancestors)])
}



plotSubPed <- function(sub_ped, id, dam, sire, sex, fem, male,
                       highlight_ids  = character(0),
                       inbreeding_vec = NULL) {
  
  # Save F values BEFORE add.Inds drops them
  saved_f <- if (!is.null(inbreeding_vec)) inbreeding_vec else setNames(
    rep(0, nrow(sub_ped)), as.character(sub_ped[[id]])
  )
  
  sub_ped <- add.Inds(as.data.frame(sub_ped))
  colnames(sub_ped)[colnames(sub_ped) == id]   <- "id"
  colnames(sub_ped)[colnames(sub_ped) == sire] <- "sire"
  colnames(sub_ped)[colnames(sub_ped) == dam]  <- "dam"
  sub_ped <- sub_ped[order(orderPed(sub_ped)), ]
  
  for (i in seq_len(nrow(sub_ped))) {
    if (sub_ped[i, "id"] %in% sub_ped[["dam"]]) {
      sub_ped[i, sex] <- fem
    } else if (sub_ped[i, "id"] %in% sub_ped[["sire"]]) {
      sub_ped[i, sex] <- male
    }
  }
  
  sub_ped[[sex]] <- as.numeric(ifelse(sub_ped[[sex]] == male, 0, 1))
  sub_ped$full   <- 1L

  f_lookup <- saved_f[as.character(sub_ped[["id"]])]
  f_lookup[is.na(f_lookup)] <- 0
  sub_ped$Inbreeding <- f_lookup
  
  # ── Labels ──────────────────────────────────────
  f_num  <- suppressWarnings(as.numeric(sub_ped$Inbreeding))
  short_names <- substr(sub_ped[["id"]], 1, 20)
  
  labels <- ifelse(
    !is.na(f_num),
    paste0(short_names, "\nCOI ", sprintf("%.2f", f_num)),   # 3 decimals saves space
    short_names
  )
  
  # ── Color scheme ───────────────────────────────────────────────────────────
  is_test    <- sub_ped[["id"]] == "TestPup"
  is_common  <- sub_ped[["id"]] %in% highlight_ids
  is_female  <- sub_ped[[sex]] == 1
  is_founder <- (is.na(sub_ped[["dam"]])  | sub_ped[["dam"]]  %in% c("0", "")) &
    (is.na(sub_ped[["sire"]]) | sub_ped[["sire"]] %in% c("0", ""))
  
  ind_col <- ifelse(is_test,    "#FFC107",   # amber        – test pup
                    ifelse(is_common, "#F01825",   #   – common ancestor
                           ifelse(is_female, "#FCD4DB",   # – females
                                  "#DFF1F2"))) #    – males
  
  # ── Pedigree object & plot ─────────────────────────────────────────────────
  sub_pedtree <- pedigree(
    id       = sub_ped[["id"]],
    dadid    = sub_ped[["sire"]],
    momid    = sub_ped[["dam"]],
    sex      = sub_ped[[sex]],
    affected = sub_ped$full
  )
  
  op <- par(mar = c(5, 1, 2, 1), bg = "#FAFAFA")
  on.exit(par(op), add = TRUE)
  
  plot(sub_pedtree,
       id         = labels,
       col        = ind_col,
       cex        = 0.28,
       font       = 1,
       symbolsize = 0.45,
       branch     = 0.5)
  
  # ── Legend ─────────────────────────────────────────────────────────────────
  has_test   <- any(is_test)
  has_common <- length(highlight_ids) > 0
  
  leg_lab <- c(
    if (has_test)   "Test pup",
    if (has_common) "Common ancestor",
    "Female",
    "Male"
  )
  leg_col <- c(
    if (has_test)   "#FFC107",
    if (has_common) "#F01825",
    "#FCD4DB",
    "#DFF1F2"
  )
  
  legend("bottomleft",
         legend = leg_lab,
         fill   = leg_col,
         border = "#cccccc",
         bty    = "n",
         cex    = 0.4,
         title     = "Legend",
         title.col = "#555555",
         xpd    = TRUE)  
}



pedtree <- function(ped, id, dam, sire, sex, fem, male, testdam, testsire,
                    inbreeding_vec = NULL, kinship_mat = NULL) {
  
  ped1 <- cleanped(ped, id, dam, sire, sex, fem, male)
  ped1 <- ped1[!is.na(ped1[[dam]]) & !is.na(ped1[[sire]]) & !is.na(ped1[[id]]), ]
  
  test    <- c("TestPup", testdam, testsire, male)
  pedtest <- rbind(as.data.frame(ped1), test)
  pedtest <- add.Inds(as.data.frame(pedtest))
  pedtest <- pedtest[order(orderPed(pedtest)), ]
  pedtest$Inbreeding <- 0 
  
  # ── Override with consistent values from rd ────────────────────────────────
  if (!is.null(inbreeding_vec)) {
    ids_vec <- as.character(pedtest[[id]])
    for (i in seq_along(ids_vec)) {
      aid <- ids_vec[i]
      if (aid %in% names(inbreeding_vec))
        pedtest$Inbreeding[i] <- round(inbreeding_vec[[aid]], 4)
    }
    # TestPup: offspring COI = kinship between parents
    if (!is.null(kinship_mat) &&
        testdam  %in% rownames(kinship_mat) &&
        testsire %in% rownames(kinship_mat)) {
      pedtest$Inbreeding[as.character(pedtest[[id]]) == "TestPup"] <-
        round(kinship_mat[testdam, testsire], 4)
    }
  }
  
  ancestors_sire <- getAncestors(pedtest, testsire, id, dam, sire, gen = 4)
  ancestors_dam  <- getAncestors(pedtest, testdam,  id, dam, sire, gen = 4)
  
  sub_ped <- pedtest[pedtest[[id]] %in%
                       c("TestPup", testsire, ancestors_sire,
                         testdam,   ancestors_dam), ]
  
  common <- intersect(c(testdam,  ancestors_dam),
                      c(testsire, ancestors_sire))
  
  # Extend inbreeding_vec with TestPup's COI so plotSubPed doesn't zero it out
  inbreeding_vec_ext <- inbreeding_vec
  if (!is.null(kinship_mat) &&
      testdam  %in% rownames(kinship_mat) &&
      testsire %in% rownames(kinship_mat)) {
    inbreeding_vec_ext["TestPup"] <- round(kinship_mat[testdam, testsire], 4)
  }
  
  plotSubPed(sub_ped, id, dam, sire, sex, fem, male,
             highlight_ids  = common,
             inbreeding_vec = inbreeding_vec_ext)
}



pedtree2 <- function(ped, id, dam, sire, sex, fem, male, pedid,
                     inbreeding_vec = NULL) {
  
  ped1 <- cleanped(ped, id, dam, sire, sex, fem, male)
  ped1 <- ped1[!is.na(ped1[[dam]]) & !is.na(ped1[[sire]]) & !is.na(ped1[[id]]), ]
  
  ped1 <- add.Inds(as.data.frame(ped1))
  ped1 <- ped1[order(orderPed(ped1)), ]
  ped1$Inbreeding <- 0 
  
  # ── Override with consistent values from rd ────────────────────────────────
  if (!is.null(inbreeding_vec)) {
    ids_vec <- as.character(ped1[[id]])
    for (i in seq_along(ids_vec)) {
      aid <- ids_vec[i]
      if (aid %in% names(inbreeding_vec))
        ped1$Inbreeding[i] <- round(inbreeding_vec[[aid]], 4)
    }
  }
  
  idsire <- ped1[ped1[[id]] == pedid, sire]
  iddam  <- ped1[ped1[[id]] == pedid, dam]
  
  ancestors_sire <- getAncestors(ped1, idsire, id, dam, sire, gen = 4)
  ancestors_dam  <- getAncestors(ped1, iddam,  id, dam, sire, gen = 4)
  
  sub_ped <- ped1[ped1[[id]] %in%
                    c(pedid, idsire, ancestors_sire,
                      iddam,  ancestors_dam), ]
  
  common <- intersect(c(iddam,  ancestors_dam),
                      c(idsire, ancestors_sire))
  
  plotSubPed(sub_ped, id, dam, sire, sex, fem, male,
             highlight_ids  = common,
             inbreeding_vec = inbreeding_vec)
}
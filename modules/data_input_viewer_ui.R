# ── data_input_viewer_ui_PPD.R ────────────────────────────────────────────────
dataInputViewerUI <- function(id) {
  ns <- NS(id)
  
  # ── Reusable helpers (mirrors management UI style) ────────────────────────
  section_label <- function(text)
    tags$div(
      style = paste0(
        "font-size:0.8rem; font-weight:700; text-transform:uppercase; ",
        "letter-spacing:0.06em; color:#888; margin-bottom:8px"
      ),
      text
    )
  
  tabPanel("Data Input",
           tabsetPanel(
             id = ns("main_tabs"),
             
             # ── Upload tab ─────────────────────────────────────────────────
             tabPanel("Upload Data",
                      p(""),
                      
                      tags$div(
                        style = "font-size:0.95rem; font-weight:700; color:#517066; 
                    margin-bottom:8px",
                        bsicons::bs_icon("upload"), " Step 1 — Upload your files"
                      ),
                      tags$p(
                        style = "font-size:0.85rem; color:#666; margin-bottom:12px",
                        "Select your Excel files below. A preview of the first rows will 
            appear after uploading. Both files must be in ", 
                        tags$code(".xlsx"), " format."
                      ),
                      tags$div(
                        style = paste0(
                          "background:#fff8e1; border-left:4px solid #e65100; ",
                          "padding:10px 14px; margin:0 0 16px 0; border-radius:0 4px 4px 0; ",
                          "font-size:0.85rem;"
                        ),
                        tags$b("\u26a0\ufe0f Garbage in, garbage out."),
                        tags$br(),
                        "RAPPORT cannot detect factual errors in your files — incorrect parentage, ",
                        "wrong dates, miscoded sex labels, or inconsistent IDs will silently propagate ",
                        "through every calculation. Double-check both files before uploading."
                      ),
                      
                      # ── Row 1: file upload + preview ──────────────────────
                      layout_columns(
                        
                        card(
                          card_header("Data File"),
                          card_body(
                            section_label("Source"),
                            fileInput(ns("data_file"), NULL, accept = ".xlsx"),
                            textInput(ns("datasheet"), "Sheet name (optional)", value = ""),
                            tags$p(
                              style = "font-size:0.85rem; color:#666; margin:0 0 12px",
                              tags$b("Animal IDs must match the pedigree exactly"),
                              " (case-sensitive)."
                            ),
                            actionButton(ns("upload_data"), "Upload Data",
                                         class = "btn btn-outline-primary btn-sm"),
                            htmlOutput(ns("data_upload_error")),
                            hr(),
                            section_label("Preview"),
                            div(style = "overflow-x:auto; max-height:160px",
                                tableOutput(ns("datahead")))
                          )
                        ),
                        
                        card(
                          card_header("Pedigree File"),
                          card_body(
                            section_label("Source"),
                            fileInput(ns("ped_file"), NULL, accept = ".xlsx"),
                            textInput(ns("pedsheet"), "Sheet name (optional)", value = ""),
                            tags$p(
                              style = "font-size:0.85rem; color:#666; margin:0 0 12px",
                              tags$b("Animal IDs must match the data file exactly"),
                              " (case-sensitive)."
                            ),
                            actionButton(ns("upload_ped"), "Upload Pedigree",
                                         class = "btn btn-outline-primary btn-sm"),
                            htmlOutput(ns("ped_upload_error")),
                            hr(),
                            section_label("Preview"),
                            div(style = "overflow-x:auto; max-height:160px",
                                tableOutput(ns("pedhead")))
                          )
                        ),
                        
                        col_widths = c(6, 6)
                      ),
                      
                      p(""),
                      tags$hr(),
                      tags$div(
                        style = "font-size:0.95rem; font-weight:700; color:#517066; 
                    margin-bottom:8px",
                        bsicons::bs_icon("list-check"), " Step 2 — Map the columns"
                      ),
                      tags$p(
                        style = "font-size:0.85rem; color:#666; margin-bottom:12px",
                        "Tell the app which column in each file contains which piece of 
            information. Check the previews above if you are unsure."
                      ),
                      
                      
                      # ── Row 2: column mapping ──────────────────────────────
                      # datavars / pedvars already render full card() elements
                      # from the server, so they sit directly here without re-wrapping.
                      layout_columns(
                        uiOutput(ns("datavars")),
                        uiOutput(ns("pedvars")),
                        col_widths = c(6, 6)
                      ),
                      
                      p(""),
                      tags$hr(),
                      tags$div(
                        style = "font-size:0.95rem; font-weight:700; color:#517066; 
                    margin-bottom:8px",
                        bsicons::bs_icon("check2-circle"), " Step 3 — Submit"
                      ),
                      tags$p(
                        style = "font-size:0.85rem; color:#666; margin-bottom:12px",
                        "Once the columns are mapped correctly, submit each file. 
            Green confirmation messages will appear when the app is ready."
                      ),
                      
                      # ── Row 3: submit + feedback ───────────────────────────
                      layout_columns(
                        
                        card(
                          card_header("Submit Data"),
                          card_body(
                            actionButton(ns("update_data"), "Submit Data",
                                         class = "btn btn-primary"),
                            p(""),
                            htmlOutput(ns("data_submitted")),
                            uiOutput(ns("data_submit_summary")),
                            htmlOutput(ns("data_double")),
                            uiOutput(ns("view_data_link"))
                          )
                        ),
                        
                        card(
                          card_header("Submit Pedigree"),
                          card_body(
                            actionButton(ns("update_ped"), "Submit Pedigree",
                                         class = "btn btn-primary"),
                            p(""),
                            htmlOutput(ns("ped_submitted")),
                            uiOutput(ns("ped_qc_report")),
                            htmlOutput(ns("ped_complete")),
                            htmlOutput(ns("ped_double")),
                            htmlOutput(ns("error_message")),
                            uiOutput(ns("view_ped_link"))
                          )
                        ),
                        
                        col_widths = c(6, 6)
                      ),
                      
                      p(""),
                      
                      # ── Row 4: reset submit ───────────────────────────────────
                      layout_columns(
                        card(
                          card_body(
                            style = "padding: 10px 16px",
                            actionButton(ns("reset_session"), 
                                         tagList(bsicons::bs_icon("arrow-counterclockwise"), " Reset Session"),
                                         class = "btn btn-outline-danger btn-sm"),
                            tags$span(
                              style = "font-size:0.8rem; color:#888; margin-left:10px",
                              "Clears all uploaded data so you can start over."
                            )
                          )
                        ),
                        col_widths = c(12)
                      ),
                      
                      p(""),
                      
             ),
             
             # ── View Data tab ──────────────────────────────────────────────
             tabPanel("View Data",
                      tabsetPanel(
                        id = ns("view_data_tabs"),
                        tabPanel("Data Table",      dataTableOutput(ns("dataprev"))),
                        tabPanel("Own Population Subset", dataTableOutput(ns("ownprev"))),
                        tabPanel("Breeders Subset", dataTableOutput(ns("breedprev"))),
                        tabPanel("Pedigree Table",  dataTableOutput(ns("pedprev")))
                      )
             )
           )
  )
}
# ── pop_pedtree_server.R ──────────────────────────────────────────────────────
# Panel helper for the "Pedigree" tab.
# Called from popServer(); shares input/output/session namespace.
# Self-contained — no shared reactive args needed beyond rd.

popPedtreeHelper <- function(input, output, session, rd) {
  ns <- session$ns
  
  output$pedfull <- renderUI({
    req(rd$ped)
    tagList(
      if (!is.null(rd$merged))
        tags$div(style = "margin-bottom:12px",
                 radioButtons(ns("ped_scope"), label = NULL,
                              choices  = c("Own population" = "merged",
                                           "Full pedigree"  = "full"),
                              selected = "merged", inline = TRUE)),
      uiOutput(ns("ped_scope_msg")),
      fluidRow(column(12, downloadButton(ns("download_fullped"), "Download Full Pedigree"))),
      imageOutput(ns("full_pedigree_chart"))
    )
  })
  
  output$ped_scope_msg <- renderUI({
    req(rd$ped)
    scope <- if (!is.null(rd$merged) && !is.null(input$ped_scope)) input$ped_scope else "full"
    msg <- if (scope == "merged")
      tagList(bsicons::bs_icon("info-circle"),
              tags$span(style = "font-size:0.85rem; color:#0277bd; margin-left:6px",
                        "Showing animals from the data file only. ",
                        "Switch to 'Full pedigree' to include all ancestors."))
    else
      tagList(bsicons::bs_icon("info-circle"),
              tags$span(style = "font-size:0.85rem; color:#555; margin-left:6px",
                        "Showing the complete pedigree file. ",
                        if (!is.null(rd$merged))
                          "Switch to 'Own population' to show only animals in the data file."))
    tags$div(style = "margin-bottom:10px", msg)
  })
  
  ped_plot_args <- reactive({
    req(rd$ped)
    scope <- if (!is.null(rd$merged) && !is.null(input$ped_scope)) input$ped_scope else "full"
    if (scope == "merged" && !is.null(rd$merged))
      list(plot_data = rd$merged, id_col = rd$datid, sex_col = rd$datsex,
           fem_label = rd$datF,
           breeder   = if (!is.null(rd$breeders)) rd$breeders[[rd$datid]] else character(0))
    else
      list(plot_data = rd$ped, id_col = rd$pedid, sex_col = rd$pedsex,
           fem_label = rd$pedF, breeder = character(0))
  })
  
  output$full_pedigree_chart <- renderImage({
    req(rd$ped)
    args    <- ped_plot_args()
    outfile <- tempfile(fileext = ".png")
    png(outfile, width = 7000, height = 2000, res = 600)
    generate_pedigree_kinship2(args$plot_data,
                               id           = args$id_col,
                               mother       = rd$peddam,
                               father       = rd$pedsire,
                               sex          = args$sex_col,
                               female_label = args$fem_label,
                               breeder      = args$breeder,
                               inbreeding_vec = rd$inbreeding)
    dev.off()
    list(src = outfile, contentType = "image/png", width = 4000, height = 1100)
  }, deleteFile = TRUE)
  
  output$download_fullped <- downloadHandler(
    filename = function() paste0("Pedigree_", Sys.Date(), ".png"),
    content  = function(file) {
      req(rd$ped)
      args <- ped_plot_args()
      png(file, width = 7000, height = 2000, res = 3000)
      generate_pedigree_kinship2(args$plot_data,
                                 id           = args$id_col,
                                 mother       = rd$peddam,
                                 father       = rd$pedsire,
                                 sex          = args$sex_col,
                                 female_label = args$fem_label,
                                 breeder      = args$breeder,
                                 inbreeding_vec = rd$inbreeding)
      dev.off()
    }
  )
  
  output$pedextract <- renderUI({
    if (is.null(rd$ped) && is.null(rd$merged)) {
      return(tags$div(class = "alert alert-info", style = "margin-top:16px; font-size:0.9rem",
                      tags$b("\u2139 No data loaded. "),
                      "Upload and submit at least a pedigree file in the ",
                      tags$b("Data Input"), " tab to use this panel."))
    }
    choices <- if (!is.null(rd$ped)) c("", rev(rd$ped[[rd$pedid]]))
    else if (!is.null(rd$merged)) c("", rd$merged[[rd$datid]])
    else c("")
    tagList(
      fluidRow(
        tags$p(tags$b("Choose an animal for a visual pedigree going back 6 generations")),
        selectizeInput(ns("ped_id_select"), NULL, choices = choices, selected = "",
                       options = list(placeholder = "Select Animal"))
      ),
      fluidRow(layout_columns(
        actionButton(ns("pedbutton"), "Generate Pedigree"),
        p(""),
        downloadButton(ns("downloadPedtreeExtr"), "Download Pedigree"),
        col_widths = c(3, 1, 3)
      )),
      fluidRow(imageOutput(ns("pedextr")))
    )
  })
  
  output$pedextr <- renderImage({
    req(input$pedbutton, input$ped_id_select)
    outfile <- tempfile(fileext = ".png")
    png(outfile, width = 2000, height = 1000, res = 300)
    if (!is.null(rd$ped))
      pedtree2(rd$ped,    rd$pedid,  rd$peddam, rd$pedsire,
               rd$pedsex, rd$pedF,   rd$pedM,   input$ped_id_select,
               inbreeding_vec = rd$inbreeding)
    else if (!is.null(rd$merged))
      pedtree2(rd$merged, rd$datid,  rd$peddam, rd$pedsire,
               rd$datsex, rd$datF,   rd$datM,   input$ped_id_select,
               inbreeding_vec = rd$inbreeding)
    dev.off()
    list(src = outfile, contentType = "image/png",
         width = 2000, height = 1000, alt = "Pedigree Extract")
  }, deleteFile = TRUE)
  
  output$downloadPedtreeExtr <- downloadHandler(
    filename = function() paste0("Pedigree_", input$ped_id_select, ".png"),
    content  = function(file) {
      png(file, width = 2000, height = 1000, res = 300)
      if (!is.null(rd$ped))
        pedtree2(rd$ped,    rd$pedid,  rd$peddam, rd$pedsire,
                 rd$pedsex, rd$pedF,   rd$pedM,   input$ped_id_select,
                 inbreeding_vec = rd$inbreeding)
      else if (!is.null(rd$merged))
        pedtree2(rd$merged, rd$datid,  rd$peddam, rd$pedsire,
                 rd$datsex, rd$datF,   rd$datM,   input$ped_id_select,
                 inbreeding_vec = rd$inbreeding)
      dev.off()
    }
  )
}
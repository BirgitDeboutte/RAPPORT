# ── pop_pedtree_ui.R ──────────────────────────────────────────────────────────
# Accepts ns (the parent popUI namespace function) instead of an id.

popPedtreePanelUI <- function(ns) {
  tabPanel("Pedigree Visualisation",
           tabsetPanel(
             tabPanel("Full Pedigree",
                      uiOutput(ns("pedfull"))
             ),
             tabPanel("Individual Pedigree",
                      uiOutput(ns("pedextract"))
             )
           )
  )
}
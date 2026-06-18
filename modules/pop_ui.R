# ── pop_ui.R ──────────────────────────────────────────────────────────────────
# Single entry point for the entire Population section.
# All panels share the "pop" namespace → output IDs are "pop-vb_animals" etc.


source("modules/pop_overview_ui.R")
source("modules/pop_litters_ui.R")
source("modules/pop_dams_ui.R")
source("modules/pop_sires_ui.R")
source("modules/pop_pup_info_ui.R")
source("modules/pop_params_ui.R")
source("modules/pop_breeders_ui.R")
source("modules/pop_pedtree_ui.R")

popUI <- function(id) {
  ns <- NS(id)
  tabPanel("Population",
           tabsetPanel(
             popOverviewPanelUI(ns),
             popLittersPanelUI(ns),
             popDamsPanelUI(ns),
             popSiresPanelUI(ns),
             popPupInfoPanelUI(ns),
             popParamsPanelUI(ns),
             popBreedersPanelUI(ns),
             popPedtreePanelUI(ns)
           )
  )
}
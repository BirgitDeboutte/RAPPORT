source("modules/data_input_viewer_server.R")
source("modules/pop_server.R")
source("modules/mating_ranking_server.R")
source("modules/management_server.R")

server <- function(input, output, session) {
  
  rd <- dataInputViewerServer("datainputviewer")
  
  popServer("pop", rd)
  rankingServer("ranking", rd)
  managementServer("management", rd)
  
}

# Load Global Variables & Functions
source("global.R")

# Load UI and Server Components
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

# Run the Shiny App
shinyApp(ui, server)

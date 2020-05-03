source("R/ui.R", encoding = "UTF-8")
source("R/server.R", encoding = "UTF-8")

shiny::shinyApp(ui = ui, server = server)
#BioAnalyst::runApp
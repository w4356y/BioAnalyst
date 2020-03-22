source("ui.R")
source("server.R")

runApp <- function(){
  shinyApp(ui = ui, server = server)
}

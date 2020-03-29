source("R/ui.R", encoding = "UTF-8")
source("R/server.R", encoding = "UTF-8")
#shinyApp(ui = ui, server = server)

#' Run the Shiny Application
#'
#' @export
runApp <- function(){
   shiny::shinyApp(ui = ui, server = server)
 }

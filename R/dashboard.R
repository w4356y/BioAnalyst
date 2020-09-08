dashboard_mainbar <- tagList(
  actionButton("clickLog","Click here to get passport"),
  hr(),
  tags$img(src="diamond.jpeg", style = 'width: 100%;'),
  p('Contact: w4356y@163.com(+86 15011008186)'),
)

create_clickLog <- function(session, input, output){
  event <- observeEvent(input$clickLog, {
      shinyWidgets::sendSweetAlert(session, 
                                   title = "Welcome!",
                                   "account: messi, password: test, verifycode: Virus", 
                                   type = "message"
      )
  })
  
  
}
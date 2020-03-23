create_login_confirm <- function(session, input, output){
  event <- observeEvent(input$confirmLogin, {
    if(input$login.user == "" | input$login.password == "" | input$anhao == ""){
      showNotification("Warnings!",
                       "Please confirm username, password and passcode are specifued.", 
                       type ="message")
    }
    req(input$login.user)
    #browser()
    req(input$login.password)
    req(input$anhao)
    #browser()
    if(input$login.user %in% c('messi','manning')){
      if(input$login.password == "test"){
        if(input$anhao == "Virus"){
          shinyWidgets::sendSweetAlert(session, 
                                       title = "Wow! Congrats!", 
                                       text = "Login successfully.", 
                                       type = "success"
                                       )
          shinyjs::show(selector = "a[data-value = 'BioMeta']")
          shinyjs::show(selector = "a[data-value = 'BioAnalysis']")
          shinyjs::show(selector = "a[data-value = 'BioAI']")
        }else{
          shinyWidgets::sendSweetAlert(session, 
                                       title = "Sorry! Wrong!",
                                       "Please check your passcode.",
                                       type = "error"
                                       )
        }
      }else{
        shinyWidgets::sendSweetAlert(session, 
                                     title = "Sorry! Wrong!", 
                                     "Please check your password.", 
                                     type = "error"
                                     )
      }
    }else{
      shinyWidgets::sendSweetAlert(session, 
                                   title = "Sorry! Wrong!",
                                   "Please check your username.", 
                                   type = "error"
                                   )
    }
    
    
  })
  
  
}
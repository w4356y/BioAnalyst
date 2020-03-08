library(dplyr)
library(caret)
library(pROC)

ai_mainbar <- mainPanel(width = 12,
                        fluidRow(column(4, 
                                        box("",
                                            title = "Data Source", 
                                            width = NULL,
                                        radioButtons("data_source", "Data from?", choices = c("New Data", "Local data")),
                                        radioButtons("ai_class","Regression or Classification?", choices = c("Classify","Regression")),
                                        fileInput("input_x","Feature Data",multiple = F, 
                                                  accept = c(".csv",
                                                             ".txt",
                                                             ".xlsx")),
                                        fileInput("input_y","Label", multiple = F, 
                                                  accept = c(".csv",
                                                             ".txt",
                                                             ".xlsx")),
                                        selectInput("select_label","Please select a label.", choices = NA),
                                        actionButton("confirm_ai_data", "Confirm.")
                                        ),
                                        box("",
                                            title = "Configuration", 
                                            width = NULL,
                                            textInput("seed","Input a Seed."),
                                            radioButtons("split_strategy","Specify the data split Method", choices = c("Proportion", "Number","Number by group","Proportion by group")),
                                            sliderInput("proportion_train","Proportion of train data.", min = 0, max = 100, value = 80),
                                            textInput("number_train","Number of train data."),
                                            selectInput("group_train_class_A","Select a group as A", choices = NA),
                                            textInput("number_train_A","Number of group A."),
                                            sliderInput("proportion_train_A","Proportion of group A.", min = 0, max = 100, value = 80),
                                            selectInput("group_train_class_B","Select a group as B", choices = NA),
                                            textInput("number_train_B","Number of group B."),
                                            sliderInput("proportion_train_B","Proportion of train data.", min = 0, max = 100, value = 80), 
                                            actionButton("confirm_config","Confirm")
                                            
                                        )),
                                 column(8, 
                                        fluidRow(column(4,
                                                        box( title = "Model Configuration", 
                                                             width = NULL,
                                                             selectInput("train_method","Select a method.", choices = c("cv","LOOCV","none","boot","boot632","optimism_boot","boot_all","repeatedcv","LGOCV")),
                                                             selectInput("cv_numver","Select a CV number", choices = c(1,3,5,8,10)),
                                                             selectInput("metric","Select a metric", choices = c("Accuracy","Kappa", "RMSE","Rsquared","ROC"))
                                                        ),
                                                        ),
                                                 column(8,
                                                        tabBox(
                                                          title = NULL,
                                                          id = "ai_option", width = NULL,
                                                          tabPanel( "Logistic", 
                                                                    width = NULL,
                                                                    box( title = "Tuning Parameter", 
                                                                         width = NULL,
                                                                         selectInput("lr_lambda","L2 penality.", choices = c(0, 0.0001,0.001, 0.01, 0.1,0.5,1), multiple = T),
                                                                         selectInput("lr_cp","Complexity Parameter", choices = c("aic","bic"), multiple = T, selectize = T),
                                                                         actionButton("confirm_logreg","Confirm")
                                                                    )
                                                          ),
                                                          tabPanel( "KNN", 
                                                                    width = NULL,
                                                                    box( title = "Tuning Parameter", 
                                                                         width = NULL,
                                                                         selectInput("knn_k","Specify the neighbor number.", choices = c(1:20), multiple = T),
                                                                         actionButton("confirm_knn","Confirm")
                                                                    )
                                                          ),
                                                          tabPanel( "sPLS-DA", 
                                                                    width = NULL,
                                                                    box( title = "Tuning Parameter", 
                                                                         width = NULL,
                                                                         selectInput("ncomp_splsda","Specify the component.", choices = c(1:10), multiple = T),
                                                                         selectInput("eta_splada", "Eta.", choices = seq(0,1,0.1), multiple = T),
                                                                         actionButton("confirm_splsda","Confirm")
                                                                    )
                                                          ),
                                                          tabPanel( "SVM_Linear", 
                                                                    width = NULL,
                                                                    box( title = "Tuning Parameter", 
                                                                         width = NULL,
                                                                         selectInput("svmlinear_C","Specify parameter C.", 
                                                                                     choices = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5),
                                                                                     multiple = T),
                                                                         actionButton("confirm_svmLinear","Confirm")
                                                                    )
                                                          ),
                                                          tabPanel( "Random_Forest", 
                                                                    width = NULL,
                                                                    box( title = "Tuning Parameter", 
                                                                         width = NULL,
                                                                         selectInput("rf_mtry","Specify the component.", choices = seq(2,30,2), multiple = T),
                                                                         selectInput("rf_ntree","Specify num of tree.", 
                                                                                     choices = c(100,500,800,1000,1200,1500,2000)),
                                                                         actionButton("confirm_rf","Confirm")
                                                                    )
                                                          ),
                                                          tabPanel( "Linear_Regression", 
                                                                    width = NULL,
                                                                    box( title = "Tuning Parameter", 
                                                                         width = NULL,
                                                                         actionButton("confirm_lr","Confirm")
                                                                    )
                                                          )
                                                        )
                                                        
                                                        )),
                                        
                                        box( title = "Data", 
                                             width = NULL,
                                             div(style = 'overflow-x: scroll; overflow-y: scroll;',
                                                 DT::dataTableOutput("ai_table")),
                                             fluidRow(column(4,
                                                             box( title = "Data", 
                                                                  width = NULL,
                                                                  verbatimTextOutput("textai",placeholder = FALSE),
                                                                  # div(style = 'overflow-x: scroll; overflow-y: scroll;',
                                                                  DT::dataTableOutput("confusion_matrix")
                                                             )
                                                             ),
                                                      column(8,
                                                             plotOutput("plot_ai",width = "auto", height = "400px")
                                                             ))
                                             
                                        )
                                        )
                        )
)


create_obs_dataSource <- function(input, output){
  event <- observe({
    if(input$data_source == "New Data"){
      shinyjs::hide("select_label")
      shinyjs::show("input_x")
      shinyjs::show("input_y")
    }else{
      shinyjs::show("select_label")
      shinyjs::hide("input_x")
      shinyjs::hide("input_y")
    }
  })
  return(event)
}

create_obs_splitChoice <- function(input, output,session){
  event <- observe({
    if(input$ai_class == "Regression"){
      shinyjs::hide("group_train_class_A")
      shinyjs::hide("group_train_class_B")
      shinyjs::hide("number_train_A")
      shinyjs::hide("number_train_B")
      shinyjs::hide("proportion_train_A")
      shinyjs::hide("proportion_train_B")
      updateRadioButtons(session = session, "split_strategy", choices = c("Proportion", "Number"))
    }else{
      updateRadioButtons(session = session, "split_strategy", choices = c("Proportion", "Number","Number by group","Proportion by group"))
      shinyjs::hide("group_train_class_A")
      shinyjs::hide("group_train_class_B")
      shinyjs::hide("number_train_A")
      shinyjs::hide("number_train_B")
      shinyjs::hide("proportion_train_A")
      shinyjs::hide("proportion_train_B")
    }
  })
  return(event)
}

create_obs_splitMethod <- function(input, output){
  event <- observe({
    if(input$split_strategy == "Proportion"){
      shinyjs::show("proportion_train")
      shinyjs::hide("number_train")
      shinyjs::hide("group_train_class_A")
      shinyjs::hide("group_train_class_B")
      shinyjs::hide("number_train_A")
      shinyjs::hide("number_train_B")
      shinyjs::hide("proportion_train_A")
      shinyjs::hide("proportion_train_B")
    }else if(input$split_strategy == "Number"){
      shinyjs::hide("proportion_train")
      shinyjs::show("number_train")
      shinyjs::hide("group_train_class_A")
      shinyjs::hide("group_train_class_B")
      shinyjs::hide("number_train_A")
      shinyjs::hide("number_train_B")
      shinyjs::hide("proportion_train_A")
      shinyjs::hide("proportion_train_B")
    }else if(input$split_strategy == "Number by group"){
      shinyjs::hide("proportion_train")
      shinyjs::hide("number_train")
      shinyjs::show("group_train_class_A")
      shinyjs::show("group_train_class_B")
      shinyjs::show("number_train_A")
      shinyjs::show("number_train_B")
      shinyjs::hide("proportion_train_A")
      shinyjs::hide("proportion_train_B")
    }else{
      shinyjs::hide("proportion_train")
      shinyjs::hide("number_train")
      shinyjs::show("group_train_class_A")
      shinyjs::show("group_train_class_B")
      shinyjs::hide("number_train_A")
      shinyjs::hide("number_train_B")
      shinyjs::show("proportion_train_A")
      shinyjs::show("proportion_train_B")
    }
  })
  return(event)
}


create_ai_confirm_ai_data <- function(input = input, output = output, rv = rv, rv_meta = rv_meta,
                                      rv_feature = rv_feature){
  event <- observeEvent(input$confirm_ai_data,{
    if(input$data_source == "New Data"){
      req(!is.null(input$input_x))
      req(!is.null(input$input_y))
      df_x = read_delimKB(input$input_x$datapath)  %>% as.data.frame()
      row.names(df_x) = df_x$na
      df_x$na = NULL
      df_y = read_delimKB(input$input_y$datapath) %>% as.data.frame()
      row.names(df_y) = df_y$na
      df_y$na = NULL
      if(ncol(df_y) >1){
        showNotification("More than one label.","Only one column of label is allowed.")
      }
      req(ncol(df_y) == 1)
      colnames(df_y) = "label"
      df_merge = base::merge(df_x, df_y, by = "row.names")
      df_merge$Row.names = NULL
    }else{
      req(rv_meta$data)
      req(rv_feature$data)
      overlap_id = intersect(rv_meta$data$id, row.names(rv_feature$data))
      if(length(overlap_id) == 0 ){
        showNotification("Merge Error!","Please check the id for meta and feature.")
      }
      req(!is.null(overlap_id))
      df_meta = as.data.frame(rv_meta$data)
      row.names(df_meta) = df_meta$id
      #browser()
      df_merge = as.data.frame(rv_feature$data)
      df_merge$id = row.names(rv_feature$data)
      df_merge = df_merge %>% left_join(rv_meta$data[,c("id",input$select_label)], by = c("id" = "id"))
      df_merge$id = NULL
      colnames(df_merge)[which(colnames(df_merge) == input$select_label)] = "label"
    }
      rv$data = df_merge
      output$ai_table <- DT::renderDataTable({
      #browser()
      DT::datatable(rv$data, 
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = 8,
                      dom = 'Bfrtip',
                      buttons = c('print','excel')
                    )
      )
    }
    )
    #browser()
  })
  
  
}

create_ai_confirm_config <- function(input = input, output = output, rv = rv){
  event <- observeEvent(input$confirm_config, {
      req(input$seed)
      set.seed(as.numeric(input$seed))
      req(rv$data)
      if(input$split_strategy == "Number"){
        req(input$number_train)
        id_train = sample(c(1:nrow(rv$data)), as.numeric(input$number_train))
      }else if(input$split_strategy == "Proportion"){
        req(input$proportion_train)
        id_train = sample(c(1:nrow(rv$data)), round(nrow(rv$data)*as.numeric(input$proportion_train)/100))
        #browser()
      }else if(input$split_strategy == "Number by group"){
        req(input$number_train_A)
        req(input$number_train_B)
        req(input$group_train_class_A)
        req(input$group_train_class_B)
        #browser()
        id_train = c(sample(which(rv$data$label == input$group_train_class_A), 
                          as.numeric(input$number_train_A)),
                          sample(which(rv$data$label == input$group_train_class_B), 
                            as.numeric(input$number_train_B))
                          )
      }else{
        req(input$proportion_train_A)
        req(input$proportion_train_B)
        req(input$group_train_class_A)
        req(input$group_train_class_B)
        num_A = round(sum(rv$data$label == input$group_train_class_A)*as.numeric(input$proportion_train_A)/100)
        num_B = round(sum(rv$data$label == input$group_train_class_B)*as.numeric(input$proportion_train_B)/100)
        id_train = c(sample(which(rv$data$label == input$group_train_class_A), 
                            num_A),
                          sample(which(rv$data$label == input$group_train_class_B), 
                            num_B)
        )
      }
    rv$train = rv$data %>% slice(c(id_train))
    rv$test = rv$data %>% slice(-c(id_train))
    output$ai_table <- DT::renderDataTable({
      #browser()
      DT::datatable(rv$train, 
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = 8,
                      dom = 'Bfrtip',
                      buttons = c('print','excel')
                    )
      )
    }
    )
    
    }
  
  )

}



create_obs_updateGroup <- function(input, output, session, rv ){
  event <- observe({
      
        if(input$ai_class == "Classify"){
          updateSelectInput(session = session, "group_train_class_A", choices = unique(rv$data$label))
          updateSelectInput(session = session, "group_train_class_B", choices = unique(rv$data$label))
        }
    
    
    })
}

create_obs_TrainMethod <- function(input, output){
  event <- observe({
    if(input$train_method == "cv"){
      shinyjs::show("cv_numver")
    }else{
      shinyjs::hide("cv_numver")
    }
    
    
  })
}

create_ai_confirm_logreg <- function(input = input, output = output, rv = rv){
  event <- observeEvent(input$confirm_logreg, {
        if(input$confirm_logreg == "cv"){
          cctr = caret::trainControl(method = input$train_method, number = input$cv_number,classProbs = TRUE, summaryFunction = twoClassSummary
          )
        }else{
          cctr = caret::trainControl(method = input$train_method, classProbs = TRUE, summaryFunction = twoClassSummary
                            )
        }
    #browser()
    req(rv$train)
    req(!is.null(input$lr_lambda))
    req(!is.null(input$lr_cp))
    lrGrid <- expand.grid(lambda = as.numeric(input$lr_lambda), cp = input$lr_cp)
    test_class_cv_model <- caret::train(label ~ .,data = rv$train, 
                                 method = "plr", tuneGrid = lrGrid,
                                 trControl = cctr, metric = input$metric
                                  )
    #browser()
    pred=predict(test_class_cv_model, newdata = rv$test, type = "prob")
    #browser()
    confuseM = caret::confusionMatrix(predict(test_class_cv_model, rv$test), as.factor(rv$test$label))
    output$textai<- renderPrint({
      confuseM$table
    })
    output$confusion_matrix <- DT::renderDataTable({
      d_model = as.data.frame(confuseM$byClass)
      colnames(d_model) = "value"
      d_model$value  = round(d_model$value, 4)
      DT::datatable(d_model, 
                    
                    options = list(
                      pageLength = 6
                      
                      
                    )
      )
    })
    output$ai_table <- DT::renderDataTable({
      #browser()
      DT::datatable(test_class_cv_model$results  %>% mutate(ROC = round(ROC,4),
                                                            Sens = round(Sens,4),
                                                            Spec = round(Spec,4),
                                                            ROCSD = round(ROCSD,4),
                                                            SensSD = round(SensSD,4),
                                                            SpecSD = round(SpecSD, 4)
      ), 
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = 8,
                      dom = 'Bfrtip',
                      buttons = c('print','excel')
                    )
      )
    }
    )
    output$plot_ai <-   renderPlot({
      roc(rv$test$label,
          pred[,1],
          plot=TRUE,
          print.thres = TRUE,
          print.auc = TRUE,
          percent = T,
          asp = NA,
          grid = TRUE)
      })
    #browser()
    
    }
  )
}


create_ai_confirm_knn <- function(input = input, output = output, rv = rv){
  event <- observeEvent(input$confirm_knn, {
    if(input$confirm_logreg == "cv"){
      cctr = caret::trainControl(method = input$train_method, number = input$cv_number,classProbs = TRUE, summaryFunction = twoClassSummary
      )
    }else{
      cctr = caret::trainControl(method = input$train_method, classProbs = TRUE, summaryFunction = twoClassSummary
      )
    }
    #browser()
    req(rv$train)
    req(!is.null(input$knn_k))
    kGrid <- expand.grid(k = as.numeric(input$knn_k))
    test_class_cv_model <- caret::train(label ~ .,data = rv$train, 
                                        method = "knn", tuneGrid = kGrid,
                                        trControl = cctr, metric = input$metric
    )
    #browser()
    pred=predict(test_class_cv_model, newdata = rv$test, type = "prob")
    confuseM = caret::confusionMatrix(predict(test_class_cv_model, rv$test), as.factor(rv$test$label))
    output$textai<- renderPrint({
      confuseM$table
    })
    output$confusion_matrix <- DT::renderDataTable({
      d_model = as.data.frame(confuseM$byClass)
      colnames(d_model) = "value"
      d_model$value  = round(d_model$value, 4)
      DT::datatable(d_model, 
                    
                    options = list(
                      pageLength = 6
                      
                      
                    )
      )
    })
    output$ai_table <- DT::renderDataTable({
      #browser()
      DT::datatable(test_class_cv_model$results %>% mutate(ROC = round(ROC,4),
                                                           Sens = round(Sens,4),
                                                           Spec = round(Spec,4),
                                                           ROCSD = round(ROCSD,4),
                                                           SensSD = round(SensSD,4),
                                                           SpecSD = round(SpecSD, 4)
      ), 
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = 8,
                      dom = 'Bfrtip',
                      buttons = c('print','excel')
                    )
      )
    }
    )
    output$plot_ai <-   renderPlot({
      roc(rv$test$label,
          pred[,1],
          plot=TRUE,
          print.thres = TRUE,
          print.auc = TRUE,
          percent = T,
          asp = NA,
          grid = TRUE)
    })
    
  }
  )
}


create_ai_confirm_splsda <- function(input = input, output = output, rv = rv){
  event <- observeEvent(input$confirm_splsda, {
    if(input$confirm_logreg == "cv"){
      cctr = caret::trainControl(method = input$train_method, number = input$cv_number,classProbs = TRUE, summaryFunction = twoClassSummary
      )
    }else{
      cctr = caret::trainControl(method = input$train_method, classProbs = TRUE, summaryFunction = twoClassSummary
      )
    }
    Grid <- expand.grid(K = as.numeric(input$ncomp_splsda),
                         eta = as.numeric(input$eta_splada),
                        kappa = .5)   
    
    test_class_cv_model <- caret::train(label ~ .,data = rv$train, 
                                        method = "spls", tuneGrid = Grid,
                                        trControl = cctr, metric = input$metric
    )
    #browser()
    pred=predict(test_class_cv_model, newdata = rv$test, type = "prob")
    confuseM = caret::confusionMatrix(predict(test_class_cv_model, rv$test), as.factor(rv$test$label))
    output$textai<- renderPrint({
      confuseM$table
    })
    output$confusion_matrix <- DT::renderDataTable({
      d_model = as.data.frame(confuseM$byClass)
      colnames(d_model) = "value"
      d_model$value  = round(d_model$value, 4)
      DT::datatable(d_model, 
                    
                    options = list(
                      pageLength = 6
                      
                      
                    )
      )
    })
    output$ai_table <- DT::renderDataTable({
      #browser()
      DT::datatable(test_class_cv_model$results  %>% mutate(ROC = round(ROC,4),
                                                            Sens = round(Sens,4),
                                                            Spec = round(Spec,4),
                                                            ROCSD = round(ROCSD,4),
                                                            SensSD = round(SensSD,4),
                                                            SpecSD = round(SpecSD, 4)
      ), 
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = 8,
                      dom = 'Bfrtip',
                      buttons = c('print','excel')
                    )
      )
    }
    )
    output$plot_ai <-   renderPlot({
      roc(rv$test$label,
          pred[,1],
          plot=TRUE,
          print.thres = TRUE,
          print.auc = TRUE,
          percent = T,
          asp = NA,
          grid = TRUE)
    })
  }
  )
}


create_ai_confirm_svmLinear <- function(input = input, output = output, rv = rv){
  event <- observeEvent(input$confirm_svmLinear, {
    if(input$confirm_logreg == "cv"){
      cctr = caret::trainControl(method = input$train_method, number = input$cv_number,classProbs = TRUE, summaryFunction = twoClassSummary
      )
    }else{
      cctr = caret::trainControl(method = input$train_method, classProbs = TRUE, summaryFunction = twoClassSummary
      )
    }
 
    Grid <- expand.grid(C = as.numeric(input$svmlinear_C))
    test_class_cv_model <- caret::train(label ~ .,data = rv$train, 
                                        method = "svmLinear", tuneGrid = Grid,
                                        trControl = cctr, metric = input$metric
    )
    #browser()
    pred=predict(test_class_cv_model, newdata = rv$test, type = "prob")
    confuseM = caret::confusionMatrix(predict(test_class_cv_model, rv$test), as.factor(rv$test$label))
    output$textai<- renderPrint({
      confuseM$table
    })
    output$confusion_matrix <- DT::renderDataTable({
      d_model = as.data.frame(confuseM$byClass)
      colnames(d_model) = "value"
      d_model$value  = round(d_model$value, 4)
      DT::datatable(d_model, 
                    
                    options = list(
                      pageLength = 6
                      
                      
                    )
      )
    })
    output$ai_table <- DT::renderDataTable({
      #browser()
      DT::datatable(test_class_cv_model$results %>% mutate(ROC = round(ROC,4),
                                                           Sens = round(Sens,4),
                                                           Spec = round(Spec,4),
                                                           ROCSD = round(ROCSD,4),
                                                           SensSD = round(SensSD,4),
                                                           SpecSD = round(SpecSD, 4)
      ), 
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = 8,
                      dom = 'Bfrtip',
                      buttons = c('print','excel')
                    )
      )
    }
    )
    output$plot_ai <-   renderPlot({
      roc(rv$test$label,
          pred[,1],
          plot=TRUE,
          print.thres = TRUE,
          print.auc = TRUE,
          percent = T,
          asp = NA,
          grid = TRUE)
    })
    
       
  })
}



create_ai_confirm_rf <- function(input = input, output = output, rv = rv){
  event <- observeEvent(input$confirm_rf, {
    if(input$confirm_logreg == "cv"){
      cctr = caret::trainControl(method = input$train_method, number = input$cv_number,classProbs = TRUE, summaryFunction = twoClassSummary
      )
    }else{
      cctr = caret::trainControl(method = input$train_method, classProbs = TRUE, summaryFunction = twoClassSummary
      )
    }
    Grid <- expand.grid(mtry = as.numeric(input$rf_mtry))
    test_class_cv_model <- caret::train(label ~ .,data = rv$train, 
                                        method = "rf", tuneGrid = Grid,
                                        ntree = as.numeric(input$rf_ntree),
                                        trControl = cctr, metric = input$metric
    )
    #browser()
    pred=predict(test_class_cv_model, newdata = rv$test, type = "prob")
    confuseM = caret::confusionMatrix(predict(test_class_cv_model, rv$test), as.factor(rv$test$label))
    output$textai<- renderPrint({
      confuseM$table
    })
    output$confusion_matrix <- DT::renderDataTable({
      d_model = as.data.frame(confuseM$byClass)
      colnames(d_model) = "value"
      d_model$value  = round(d_model$value, 4)
      DT::datatable(d_model, 
                    
                    options = list(
                      pageLength = 6
                      
                      
                    )
      )
    })
    output$ai_table <- DT::renderDataTable({
      #browser()
      DT::datatable(test_class_cv_model$results %>% mutate(ROC = round(ROC,4),
                                                           Sens = round(Sens,4),
                                                           Spec = round(Spec,4),
                                                           ROCSD = round(ROCSD,4),
                                                           SensSD = round(SensSD,4),
                                                           SpecSD = round(SpecSD, 4)
                                                           ), 
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = 8,
                      dom = 'Bfrtip',
                      buttons = c('print','excel')
                    )
      )
    }
    )
    output$plot_ai <-   renderPlot({
      roc(rv$test$label,
          pred[,1],
          plot=TRUE,
          print.thres = TRUE,
          print.auc = TRUE,
          percent = T,
          asp = NA,
          grid = TRUE)
    })
    
    })
}

create_obs_aiTab <- function(input, output, session){
  event <- observe({
    
    if(input$ai_class == "Regression"){
      hideTab(inputId = "ai_option", target =  "Logistic", session = session)
      hideTab(inputId = "ai_option", target =  "Random_Forest", session = session)
      hideTab(inputId = "ai_option", target =  "KNN", session = session)
      hideTab(inputId = "ai_option", target =  "sPLS-DA", session = session)
      hideTab(inputId = "ai_option", target =  "SVM_Linear", session = session)
      showTab(inputId = "ai_option", target =  "Linear_Regression", session = session)
      updateSelectInput(session = session, "metric", choice = c("RMSE","Rsquared") )
    }else{
      hideTab(inputId = "ai_option", target =  "Linear_Regression", session = session)
      showTab(inputId = "ai_option", target =  "Logistic", session = session)
      showTab(inputId = "ai_option", target =  "Random_Forest", session = session)
      showTab(inputId = "ai_option", target =  "KNN", session = session)
      showTab(inputId = "ai_option", target =  "sPLS-DA", session = session)
      showTab(inputId = "ai_option", target =  "SVM_Linear", session = session)
      updateSelectInput(session = session, "metric", choice = c("ROC","Accuracy","Kappa"))
    }
    
    
  })
}


create_ai_confirm_lr <- function(input = input, output = output, rv = rv){
  event <- observeEvent(input$confirm_lr, {
    if(input$confirm_logreg == "cv"){
      cctr = caret::trainControl(method = input$train_method, number = input$cv_number,
      )
    }else{
      cctr = caret::trainControl(method = input$train_method
      )
    }
    #Grid <- expand.grid(mtry = as.numeric(input$rf_mtry))
    dataset = sapply(colnames(rv$train), function(x) rv$train[[x]] = as.numeric(rv$train[[x]])) %>% as.data.frame()
    
    test_class_cv_model <- caret::train(label ~ .,data =dataset, 
                                        method = "lm",  
                                        trControl = cctr, metric = input$metric
    )
    output$ai_table <- DT::renderDataTable({
      #browser()
      DT::datatable(test_class_cv_model$results,
      extensions = 'Buttons', 
      options = list(
        pageLength = 8,
        dom = 'Bfrtip',
        buttons = c('print','excel')
      )
      )
    }
    )
    test_set = sapply(colnames(rv$test), function(x) rv$test[[x]] = as.numeric(rv$test[[x]])) %>% as.data.frame()
    #browser()
    pred =predict(test_class_cv_model, newdata = test_set)
    output$plot_ai <-   renderPlot({
      plot(x= pred, y = rv$test$label)
      abline(a = 0, b =1)
    })
    output$confusion_matrix <- DT::renderDataTable({
      d = data.frame(pred = round(pred,4), ref = round(rv$test$label,4))
      DT::datatable(d, 
                    
                    options = list(
                      pageLength = 6
                      
                      
                    )
      )
    })
    
  }
  )
}

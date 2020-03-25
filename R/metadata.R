source("R/functions.R", 
       encoding = "UTF-8"
       )
metadata_sidebar <- sidebarPanel(
  h1("Metadata"),
  hr(),
  br(),
  shinydashboard::box(title = "", 
                      width = 20,
                      shiny::fileInput("metadata_file",
                                       "Choose a file",
                                       multiple = F, 
                                       accept = c(".csv",
                                                  ".txt",
                                                  ".xlsx"
                                                  )
                                       ),
                      fluidRow(
                        column(5, 
                               actionButton("confirmMetaFile",
                                            "Confirm"
                                            )
                               ),
                        column(5, 
                               actionButton("metaDetection", 
                                            'Detection')
                               )
                        ),
                      shiny::selectInput("metadata_id", 
                                         "Please specify the column as sample id", 
                                         choices = NA
                                         ),
                      fluidRow(
                        column(5, 
                               actionButton("summaryMetaFile",
                                            'Summary')
                               ),
                        column(5, 
                               actionButton("test", 
                                            'test'
                                            )
                               )
                        )
                      ),
  shiny::helpText("Any problem, please contact Wei(W4356y@163.com)."),
  shiny::verbatimTextOutput("textMeta", 
                     placeholder = FALSE
                     ),
                     tags$head(tags$style("#textMeta{color: red;
                                 font-size: 15px;
                                 font-style: bold;
                                 }"
                        )
                      ),
  shinydashboard::box(title = "", 
                      width = 20,
                      shiny::selectInput("remove_ColNA", 
                                         "Select columns to remove NA values.", 
                                         choice = NA,
                                         selectize = TRUE, 
                                         multiple = TRUE
                                         ),
                      shiny::actionButton("confirmRemoveNA", 
                                          "Remove"
                                          ),
                      shiny::selectInput("fillna_ColMeta", 
                                         "Select a column.", 
                                         choice = NA
                                         ),
                      shiny::textInput("fillna_ValueMeta", 
                                       "Type in a value to fill NA."
                                       ),
                      shiny::actionButton("confirmFillna", 
                                          "Confirm"
                                          )
                      )
  )



metadata_mainbar <- mainPanel(
  shinydashboard::box(
    title = "", 
    width = NULL, 
    status = "primary",
    shinydashboard::tabBox(
      title = NULL,
      id = "tabset1", 
      width = NULL,
      shiny::tabPanel(
        "DataTable_Meta", 
        width = NULL,
        div(style = 'overflow-x: scroll; overflow-y: scroll;', 
            DT::dataTableOutput("meta_table")
            )
        ),
      shiny::tabPanel(
        "DataTable_Continuous",
        shinydashboard::box(
          title = "Numeric variable", 
          width= NULL,
          id = "meta1",
          fluidRow(column(7,
                          DT::dataTableOutput("meta_stat_continuous")
                          ),
                  column(5,
                          fluidRow(
                          column(8,
                                 selectInput(
                                   "select_continuousVariable",
                                    "Select a variable", 
                                    choice = NA
                                      )
                                    ),
                          column(3,
                                 actionButton(
                                   "confirmHist",
                                   "Hist"
                                   )
                                 )
                          ),
                                  # selectInput("select_continuousVariable","Select a variable", 
                                  #             choice = NA),
                                  # actionButton("confirmHist","Hist"),
                         shiny::plotOutput("continuous_plot_meta", 
                                           width = "250px", 
                                           height = "230px")
                         )
                  )
          )
        ),
      shiny::tabPanel(
        "DataTable_Discrete",
        shinydashboard::box(
          title = "Categorical variable", 
          width= NULL,
          id = "meta2",
          fluidRow(
            column(7, 
                   DT::dataTableOutput("meta_stat_discrete")
                   ),
            column(5,
                   fluidRow(column(8,
                                   shiny::selectInput("select_discreteVariable",
                                                      "Select a variable", 
                                                       choice = NA
                                                      )
                                   ),
                            column(3,
                                   shiny::actionButton("confirmPie","Pie")
                                   )
                            ),
                   shiny::plotOutput("discrete_plot_meta", 
                                     width = "250px", 
                                     height = "230px"
                                     )
                   )
            )
          )
        )
      )
    ),
  fluidRow(column(4,
                  shinydashboard::box(title = "Regression Analysis", 
                                      width = 20,
                                      shinydashboard::tabBox(
                                        title = NULL,
                                        id = "meta3",
                                        width = NULL,
                                        shiny::tabPanel("Corelation", 
                                                        width = NULL,
                                                        shiny::selectInput("metadata_corelation_xs", 
                                                                          "Please select a variable.", 
                                                                          choices = NA,
                                                                          selectize = TRUE, 
                                                                          multiple = TRUE
                                                                      ),
                                                        shiny::actionButton("confirmCorelationMeta", 
                                                                            "Confirm"),
                                                        shiny::downloadButton("downloadDataCorelation", 
                                                                            "Download")
                                        ),
                                        shiny::tabPanel("Regression", 
                                                        width = NULL,
                                                        shiny::selectInput("metadata_regression_y", 
                                                                           "Please select a variable.", 
                                                                           choices = NA),
                                                        shiny::selectInput("metadata_regression_xs", 
                                                                           "Please select a variable.", 
                                                                           choices = NA,
                                                                           selectize = TRUE
                                                                           ),
                                                        shiny::actionButton("confirmRegressMeta", "Confirm"
                                                                            ),
                                                        shiny::downloadButton("downloadData", "Download")
                                                        ),
                                        shiny::tabPanel("Chi-Square Analysis", 
                                                        width = NULL,
                                                        shiny::selectInput("chi_square_xs", 
                                                                           "Please select a variable.", 
                                                                           choices = NA,
                                                                           selectize = TRUE, 
                                                                           multiple = TRUE
                                                                           ),
                                                        shiny::actionButton("confirmChi_test", 
                                                                            "Confirm"),
                                                        hr(),
                                                        shiny::verbatimTextOutput("chi_result", 
                                                                                  placeholder = FALSE),
                                                        shiny::verbatimTextOutput("chi_p", 
                                                                                  placeholder = FALSE)
                               #downloadButton("downloadDataCorelation", "Download")
                               )
                               )
                               )
                  ),
           column(8,
                  shinydashboard::box(title = "", 
                                      width = NULL,
                                      shiny::plotOutput("plot1", 
                                                        width = "auto", 
                                                        height = "400px"
                                                        )
                                      )
                  )
           )
  )





create_Metadata_ReadConfirm <- function(input, output, session, rv){
  event <- observeEvent(input$confirmMetaFile,{
    
    #browser()
    if(is.null(input$metadata_file)){
      showNotification("No file uploaded.","Please upload a file.")
    }
    req(input$metadata_file)
    df = read_delimKB(input$metadata_file$datapath)
    if("na" %in% colnames(df)){
      df$na = NULL
    }
    duplicated_list = lapply(colnames(df), function(x) sum(duplicated(df[[x]]))) %>% unlist()
    if(!(0 %in% duplicated_list)){
      showNotification("Not unique column found counld be ID.","Please check that.",
                       type = "warning")
    }
    req(0 %in% duplicated_list) 
    output$meta_table <- DT::renderDataTable({
      #browser()
      DT::datatable(df, 
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = 8,
                      dom = 'Bfrtip',
                      buttons = c('print','excel')
                    )
      )
    }
    )
    unique_id = colnames(df)[which(duplicated_list == 0 )]
    updateSelectInput(session, "metadata_id", choices = unique_id)
    updateSelectInput(session, "fillna_ColMeta", choices = colnames(df))
    updateSelectInput(session, "remove_ColNA", choices = colnames(df))
    updateSelectInput(session, "filterCondition1", choices = colnames(df))
    updateSelectInput(session, "filterCondition2", choices = colnames(df))
    #updateSelectInput(session, "metadata_id", choices = unique_id)
    rv$data = df
    rv$unique_id = unique_id
  })
  return(event)
}


create_Metadata_Detection <- function(input, output, session, rv){
  event <- observeEvent(input$metaDetection,{
    #continuous_test = lapply(setdiff(colnames(rv$data), rv$unique_id), function(x) length(table(rv$data[[x]])) > 10) %>% unlist()
    continuous_test = lapply(base::setdiff(colnames(rv$data), rv$unique_id), function(x) !is.na(mean(as.numeric(rv$data[[x]])[!is.na(as.numeric(rv$data[[x]]))]))) %>% unlist()
    continuous_id = base::setdiff(colnames(rv$data), rv$unique_id)[which(continuous_test == TRUE)]
    
    #browser()
    max_v = lapply(continuous_id, function(x) max(as.numeric(rv$data[[x]])[!is.na(as.numeric(rv$data[[x]]))])) %>% unlist()
    min_v = lapply(continuous_id, function(x) min(as.numeric(rv$data[[x]])[!is.na(as.numeric(rv$data[[x]]))])) %>% unlist()
    mean_v = lapply(continuous_id, function(x) mean(as.numeric(rv$data[[x]])[!is.na(as.numeric(rv$data[[x]]))])) %>% unlist() 
    req(!is.null(continuous_id))
    df_continuous = data.frame(name = continuous_id, 
                    max = max_v,
                    min = min_v,
                    avg = round(mean_v,2))
    #browser()
    discrete_id = base::setdiff(colnames(rv$data), rv$unique_id)[which(continuous_test == FALSE)]
    max_id = lapply(discrete_id, function(x) names(which.max(table(rv$data[[x]])))) %>% unlist()
    min_id = lapply(discrete_id, function(x) names(which.min(table(rv$data[[x]])))) %>% unlist()
    min_v_d = lapply(discrete_id, function(x) min(table(rv$data[[x]]))) %>% unlist()
    max_v_d = lapply(discrete_id, function(x) max(table(rv$data[[x]]))) %>% unlist()
    df_discrete = data.frame(
      name = discrete_id,
      most = paste0(max_id,"(", max_v_d,")"),
      least = paste0(min_id,"(",min_v_d,")")
    )
    output$meta_stat_continuous = DT::renderDataTable({
      DT::datatable(df_continuous, extensions = 'Responsive',options = list(pageLength = 5, autoWidth = TRUE),
                    rownames= FALSE)
    })
    output$meta_stat_discrete = DT::renderDataTable({
      DT::datatable(df_discrete, extensions = 'Responsive',options = list(pageLength = 5, autoWidth = TRUE),
                    rownames= FALSE)
    })
    updateSelectInput(session = session, "select_continuousVariable", choice = continuous_id)
    rv$continuous_id = continuous_id
    rv$continuous_stat = df_continuous
    showTab(inputId = "tabset1", target =  "DataTable_Continuous", session = session)
    showTab(inputId = "tabset1", target =  "DataTable_Discrete", session = session)
    updateSelectInput(session = session, "select_discreteVariable", choice = discrete_id)
    updateSelectInput(session = session, "pca_color", choice = c(discrete_id,"NA"))
    updateSelectInput(session = session, "pca_shape", choice = c(discrete_id,"NA"))
    updateSelectInput(session = session, "heatmap_discrete", choice = discrete_id)
    updateSelectInput(session = session, "diff_group", choice = discrete_id)
    updateSelectInput(session = session, "chi_square_xs", choice = discrete_id)
    updateSelectInput(session = session, "heatmap_continuous", choice = continuous_id)
    updateSelectInput(session = session, "metadata_corelation_xs", choices = continuous_id)
    updateSelectInput(session = session, "metadata_regression_xs", choices = continuous_id)
    updateSelectInput(session = session, "metadata_regression_y", choices = continuous_id)
    rv$discrete_id =  discrete_id
    rv$discrete_stat = df_discrete
    updateSelectInput(session = session, "select_label", choices = discrete_id)
    
  })
  
  }

create_Metadata_confirmHist <- function(input, output, session, rv){
  event <- observeEvent(input$confirmHist,{
    req(input$select_continuousVariable)
    output$continuous_plot_meta = renderPlot({   
      hist(as.numeric(rv$data[[input$select_continuousVariable]]), main = "", xlab = "")
  })
  }
  )
}

create_Metadata_confirmPie <- function(input, output, session, rv){
  event <- observeEvent(input$confirmPie,{
    req(input$select_discreteVariable)
    output$discrete_plot_meta = renderPlot({   
      df_plot = rv$data[[input$select_discreteVariable]]  %>%  tidyr::replace_na("NA") %>% table() %>% as.data.frame() %>% arrange(Freq)
      colnames(df_plot)[1] = "Category"
      df_plot = df_plot %>% group_by(Category) %>% mutate(prop = Freq/nrow(rv$data))  
      df_plot$y1 = c(0, cumsum(df_plot$prop)[-nrow(df_plot)])
      df_plot$pos = df_plot$prop/2 + df_plot$y1 
      #browser()
      library(ggplot2)
      ggplot(df_plot %>% arrange(pos), 
             aes(x =  "", y = prop, 
                 fill = factor(Category, levels = df_plot %>% arrange(desc(pos)) %>% pull(Category)))) + 
        geom_bar(stat = "identity") + coord_polar("y", start = 0) + 
        ggrepel::geom_label_repel(aes(y= pos,label = paste(Category,round(100*prop, 2))), size=5, show.legend = F, nudge_x = 1) + labs(fill = "Gender") + theme_bw() + 
        theme(legend.position = "none", axis.title = element_blank(), axis.text = element_blank())
    })
  }
  )
}

create_Metadata_confirmCorelationMeta <- function(input, output, session, rv){
  event <- observeEvent(input$confirmCorelationMeta, {
    if(length(input$metadata_corelation_xs) <= 1){
      showNotification("2 columns are needed.","Please add another column.")
    }
    req(length(input$metadata_corelation_xs) > 1)
    #browser()
    output$plot1 = renderPlot({
      PerformanceAnalytics::chart.Correlation(sapply(rv$data[,input$metadata_corelation_xs], as.numeric), histogram=TRUE, pch=19)
    })
        
  })
  
  
}

create_Metadata_confirmRegressMeta <- function(input, output, session, rv){
  event <- observeEvent(input$confirmRegressMeta, {
    req(length(input$metadata_regression_xs) > 0)
    #browser()
    output$plot1 = renderPlot({
      #library("ggpubr")
      ggpubr::ggscatter(sapply(rv$data[,unique(c(input$metadata_regression_xs, input$metadata_regression_y))], as.numeric) %>% 
                          as.data.frame(), 
                        x = input$metadata_regression_xs, 
                        y = input$metadata_regression_y, 
                        add = "reg.line", 
                        conf.int = TRUE, 
                        cor.coef = TRUE, xlab = input$metadata_regression_xs,
                        ylab = input$metadata_regression_y)
    })
    
  })
}


create_Metadata_summaryMetaFile <- function(input, output, session, rv){
  event <- observeEvent(input$summaryMetaFile, {
    req(!is.null(rv$data))
    n_row = nrow(rv$data)
    n_col = ncol(rv$data)
    colname_list = colnames(rv$data)
    #browser()
    na_detection = lapply(colname_list, function(x) sum( rv$data[[x]] == "NA" ))
    na_col = colname_list[which(na_detection == TRUE)]
    updateSelectInput(session, "fillna_ColMeta", choices = na_col)
    updateSelectInput(session, "remove_ColNA", choices = na_col)
    output$textMeta = renderText({
      paste0("There are ", n_row, " rows and ", n_col, " columns.\n",
            "Columns: ", paste(colname_list, collapse = ", "),".\n",
            "Columns with NA values: ",paste(na_col,collapse = ", "))
             
    })
    
  })
  
  
}


create_Metadata_confirmFillna <- function(input, output, session, rv){
  event <- observeEvent(input$confirmFillna, {
    req(!is.null(rv$data))
    rv$data[[input$fillna_ColMeta]][rv$data[[input$fillna_ColMeta]] == "NA"] = input$fillna_ValueMeta
    #browser()
    output$meta_table <- DT::renderDataTable({
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
    
  })
  
  
}
create_Metadata_Bookmark <- function(input, output, session, rv){
  event <- observeEvent(input$Bookmark,{
    session$doBookmark()    
  })
  return(event)
  
}


create_Metadata_confirmRemoveNA <- function(input, output, session, rv){
  event <- observeEvent(input$confirmRemoveNA,{
    req(!is.null(input$remove_ColNA))
    #browser()
    for(x in input$remove_ColNA){
      rv$data =  rv$data[rv$data[[x]] != "NA" & !is.na(rv$data[[x]]) & !is.null(rv$data[[x]]) & rv$data[[x]] != "",]
    }
    output$meta_table <- DT::renderDataTable({
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
  })
  return(event)
  
}

create_Metadata_filterConfirm <- function(input, output, session, rv){
  event <- observeEvent(input$filterConfirm ,{
    req(!is.null(input$valueCondition1))
    #browser()
    flag1 = is.na(as.numeric(input$valueCondition1)) & input$valueCondition1 != "" & input$relationCondition1 != "in"
    if(flag1){
      showNotification("Wrong relation 1.", "Please select another relation")
    }
    #browser()
    req(flag1 == FALSE)
    flag2 = is.na(as.numeric(input$valueCondition2)) & input$valueCondition2 != "" & input$relationCondition2 != "in"
    if(flag2){
      showNotification("Wrong relation 2.", "Please select another relation")
    }
    req(flag2 == FALSE)
    rv$data_bak = rv$data
    if(input$valueCondition2 == ""){
      if(input$relationCondition1 == "in"){
        cmd = paste0("rv$data %>% dplyr::filter(", 
                     input$filterCondition1, " %",
                     input$relationCondition1,"% c('",
                     paste(stringr::str_split(input$valueCondition1,",", simplify = T),
                           collapse = "','")
                     ,"'))")
      }else{
        cmd = paste0("rv$data %>% dplyr::filter(", 
                     input$filterCondition1,
                     input$relationCondition1,
                     input$valueCondition1,")")
      }
      #browser()
      rv$data = eval(parse(text = cmd))
      #browser()
    }else{
      if(input$joinRelation == "And"){join_sign = "&"}else{join_sign = "|"}
        
      if(input$relationCondition1 == "in"){
          cmd = paste0("rv$data %>% dplyr::filter(", 
                       input$filterCondition1, " %",
                       input$relationCondition1,"% c('",
                       paste(stringr::str_split(input$valueCondition1,",", simplify = T),
                             collapse = "','")
                       ,"') ",join_sign," ")
          if(input$relationCondition2 == "in"){
            cmd1 = paste0(cmd, 
                          input$filterCondition2, " %",
                          input$relationCondition2,"% c('",
                          paste(stringr::str_split(input$valueCondition2,",", simplify = T),
                                collapse = "','")
                          ,"'))")
          }else{
            cmd1 = paste0(cmd, 
                          input$filterCondition2,
                          input$relationCondition2,
                          input$valueCondition2,")")
          }
          
        }else{
          cmd = paste0("rv$data %>% dplyr::filter(", 
                       input$filterCondition1,
                       input$relationCondition1,
                       input$valueCondition1," ",join_sign," ")
          if(input$relationCondition2 == "in"){
            cmd1 = paste0(cmd, 
                          input$filterCondition2, " %",
                          input$relationCondition2,"% c('",
                          paste(stringr::str_split(input$valueCondition2,",", simplify = T),
                                collapse = "','")
                          ,"'))")
          }else{
            cmd1 = paste0(cmd, 
                          input$filterCondition2,
                          input$relationCondition2,
                          input$valueCondition2,")")
          }
          
        }
      #browser()
      rv$data = eval(parse(text = cmd1))
    }
    output$meta_table <- DT::renderDataTable({
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
  })
  return(event)
  
}

create_Metadata_offsetFilter <- function(input, output, session, rv){
  event <- observeEvent(input$offsetFilter ,{
      req(rv$data_bak)    
      rv$data = rv$data_bak
      output$meta_table <- DT::renderDataTable({
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
    })
  
}




create_Metadata_confirmChi_test <- function(input, output, rv){
  event <- observeEvent(input$confirmChi_test, {
    if(length(input$chi_square_xs) != 2){
      showNotification("Column number is not 2.","2 column should be specified.")
    }
    req(length(input$chi_square_xs) == 2)
    #browser()    
    df = rv$data[,input$chi_square_xs] %>% table()
    chi_test = rv$data[,input$chi_square_xs] %>% table() %>% chisq.test()
    output$chi_result = renderPrint({
      df
    })
    output$chi_p = renderText({
      paste0("p value = ",
             round(chi_test$p.value, 4))
    })
    })
}

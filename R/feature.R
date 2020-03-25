
feature_mainbar <- mainPanel(width = 12,
  fluidRow(
    column(width = 7,
           shinydashboard::tabBox(
             title = NULL,
             id = "tabset2", width = NULL,
             shiny::tabPanel( "DataTable_Feature",
                       width = NULL,
                       shinydashboard::box( title = "Data/Plot",
                            width = NULL,
                            shiny::div(style = 'overflow-x: scroll; overflow-y: scroll;',
                                DT::dataTableOutput("feature_table")),
                            shiny::plotOutput("plot2",width = "auto", height = "400px"),
                            shinyjs::useShinyjs(),
                            fluidRow(column(6,
                                            shiny::actionButton("previousPage","Previous")),
                                     column(6,
                                            shiny::actionButton("nextPage","Next")))

                        )
                       ),
             shiny::tabPanel( "Stat_sample",
                       width = "100%",
                       shinydashboard::box( title = "",
                            width = NULL,
                                     plotOutput("feature_plot_sample",width = "585px", height = "450px"),
                                     div(style = 'overflow-x: scroll; overflow-y: scroll;',
                                     DT::dataTableOutput("feature_stat_sample"))
                                            )


             ),
             shiny::tabPanel( "Stat_feature",
                       width = "100%",
                       shinydashboard::box(title = "", 
                                  width = NULL,
                                  shiny::plotOutput("feature_plot_feature",
                                                    width = "585px", 
                                                    height = "450px"),
                       div(style = 'overflow-x: scroll; overflow-y: scroll;',
                                       DT::dataTableOutput("feature_stat_feature"))
             )
             )

                    )
           ),
    column(width = 5,
           shinydashboard::box(
             title = "Import/PreProcessing",
             width = NULL,
             shiny::fileInput("feature_file",
                              "Choose a file",
                              multiple = F,
                              accept = c(".csv",
                                         ".txt",
                                         ".xlsx")
                              ),
             fluidRow(
               column(5, shiny::actionButton("confirmFeatureFile",
                                             "Confirm")
                      ),
               column(5, shiny::selectInput("id_feature", 
                                            "Please specify the sample ID.", 
                                            choice = NA)
                      )
             ),
             fluidRow(
               column(5,  
                  shiny::actionButton("Prepare", 
                                      'Prepare'
                                      ),
                  br(),
                  br(),
                  br(),
                  shiny::actionButton("CancelPrepare", 
                                      'Cancel Prepare'
                                      )
                  ),
               column(5,
                      shiny::radioButtons("transpose_or_not", 
                                          "Transpose?", 
                                          choices = c(TRUE,
                                                      FALSE
                                                      ), 
                                          selected = FALSE, 
                                          inline = T
                                          ),
                      shiny::selectInput("cols_to_discard",
                                         "Select columns to be discarded.",
                                         choices = NA, 
                                         selectize = TRUE,
                                         multiple = T)
                      )
               ),
             column(5, 
                    shiny::actionButton("featureDetection", 
                                        'Detection')
                    )
             ),
          fluidRow(column(6, 
                          shinydashboard::box(title = "NA Filtering",
                                              width = 20,
                                              shiny::sliderInput("proportion_feature",
                                                                 "Proportion of feature NA",
                                                                 min = 0,
                                                                 max = 100,
                                                                 value = 20
                                                                 ),
                                              fluidRow(column(6, 
                                                              shiny::actionButton("confirmRemoveFeatureNA", 
                                                                                  "Remove")
                                                              ),
                                                       column(6, shiny::actionButton("cancelRemoveFeatureNA", 
                                                                                     "Cancel")
                                                              )
                                                       ),
                              shiny::sliderInput("proportion_sample",
                                                 "Proportion of sample NA",
                                                 min = 0,
                                                 max = 100,
                                                 value = 20
                                                 ),
                              fluidRow(column(6, 
                                              shiny::actionButton("confirmRemoveSampleNA", 
                                                                  "Remove"
                                                                  )
                                              ),
                                       column(6, 
                                              shiny::actionButton("cancelRemoveSampleNA",
                                                                  "Cancel"
                                                                  )
                                              )
                                       )
                              )
                          ),
                   column(6, 
                          shinydashboard::box(title = "Data Transform",
                                              width = 20,
                                              shiny::selectInput("fillNAmethod", 
                                                                 "Select a method to fill NA.", 
                                                                 choices = c("row_mean",
                                                                             "col_mean",
                                                                             "row_min",
                                                                             "col_min",
                                                                             "constant"
                                                                             )
                                                                 ),
                                              shiny::textInput("fillFeatureNA",
                                                               "Type in a value."
                                                               ),
                                              shiny::helpText("Type in r for row mean, c for column mean"),
                                              fluidRow(column(6, 
                                                              shiny::actionButton("confirmFillNA", 
                                                                                  "Confirm"
                                                                                  )
                                                              ),
                                                       column(6, 
                                                              shiny::actionButton("cancelFillNA", 
                                                                                  "Cancel"
                                                                                  )
                                                              )
                                                       ),
                                              shiny::selectInput("transformMethod", 
                                                                 "Transform method",
                                                                 choices = c("log10",
                                                                             "log",
                                                                             "log2",
                                                                             "clr")
                                                                 ),
                                              fluidRow(column(6, 
                                                              shiny::actionButton("confirmTransform", 
                                                                                  "Confirm"
                                                                                  )
                                                              ),
                                                       column(6, 
                                                              shiny::actionButton("cancelTransform", 
                                                                                  "Cancel"
                                                                                  )
                                                              )
                                                       )
                                              )
                          )
                   ),
          shinydashboard::box(title = "Data Transform", 
                              width = 20,
                              shinydashboard::tabBox(
                                title = NULL,
                                id = "tabset1", 
                                width = NULL,
                                shiny::tabPanel( "PCA w/z meta & Co-linear-Analysis",
                                                 shinydashboard::box( 
                                                   title = "",
                                                   width = NULL,
                                                   fluidRow(column(4, 
                                                                   shiny::radioButtons("axis",
                                                                                       "Which axis?", 
                                                                                       choices = c("X","Y")
                                                                                       )
                                                                   ),
                                                            column(4, 
                                                                   shiny::radioButtons("center_or_not", 
                                                                                       "Center or not?", 
                                                                                       choices = c("TRUE",
                                                                                                   "FALSE"
                                                                                                   )
                                                                                       )
                                                                   ),
                                                            column(4, 
                                                                   shiny::radioButtons("scale_or_not", 
                                                                                       "Scale or not?", 
                                                                                       choices = c("TRUE",
                                                                                                   "FALSE"
                                                                                                   )
                                                                                       )
                                                                   )
                                                            )
                                                   ),
                                                 shiny::actionButton("confirmPCA","PCA")
                                                 ),
                shiny::tabPanel( "PCA with meta",
                                 shinydashboard::box( title = "",
                                                      width = NULL,
                                                      shiny::selectInput("pca_color",
                                                                         "Select a color variable.", 
                                                                         choices = NA
                                                                         ),
                                                      shiny::selectInput("pca_shape", 
                                                                         "Select a shape variable", 
                                                                         choices = NA
                                                                         ),
                                                      shiny::actionButton("Join_meta_and_feature",
                                                                          "detect_merge")
                                                      )
                                 ),
                shiny::tabPanel( "Complex_Heatmap",
                                 shinydashboard::box( title = "",
                                                      width = NULL,
                                                      fluidRow(column(4, 
                                                                      shiny::selectInput("heatmap_discrete",
                                                                                         "Select categorical variables.", 
                                                                                         choices = NA, 
                                                                                         selectize = T)
                                                                      ),
                                                               column(4, 
                                                                      shiny::selectInput("heatmap_continuous",
                                                                                         "Select continuous variables.", 
                                                                                         choices = NA, 
                                                                                         selected = T
                                                                                         )
                                                                      )
                                                               ),
                                                      fluidRow(column(4, 
                                                                      shiny::radioButtons("cluster_row",
                                                                                          "Cluster row?", 
                                                                                          choices = c("TRUE", 
                                                                                                      "FALSE"
                                                                                                      )
                                                                                          )
                                                                      ),
                                                               column(4, 
                                                                      shiny::radioButtons("cluster_col",
                                                                                          "Cluster column?", 
                                                                                          choices = c("TRUE",
                                                                                                      "FALSE"
                                                                                                      )
                                                                                          )
                                                                      )
                                                               ),
                                                      fluidRow(column(4, 
                                                                      shiny::radioButtons("dend_row",
                                                                                          "Dend row?", 
                                                                                          choices = c("TRUE", "FALSE")
                                                                                   )
                                                                      ),
                                                               column(4, 
                                                                      shiny::radioButtons("dend_col",
                                                                                          "Dend column?", 
                                                                                          choices = c("TRUE","FALSE")
                                                                                          )
                                                                      )
                                                               ),
                                                      fluidRow(column(4, 
                                                                      shiny::radioButtons("name_row",
                                                                                          "Name row?", 
                                                                                          choices = c("TRUE", 
                                                                                                      "FALSE")
                                                                                          )
                                                                      ),
                                                               column(4, 
                                                                      shiny::radioButtons("name_col",
                                                                                          "Name column?", 
                                                                                          choices = c("TRUE",
                                                                                                      "FALSE"
                                                                                                      )
                                                                                          )
                                                                      )
                                                               ),
                                                      fluidRow(column(4, 
                                                                      shiny::selectInput("method_row",
                                                                                  "Method row?", 
                                                                                  choices = c("ward.D", 
                                                                                              "ward.D2",
                                                                                              "single",
                                                                                              "complete",
                                                                                              "average",
                                                                                              "mcquitty",
                                                                                              "median",
                                                                                              "centroid"
                                                                                              )
                                                                                  )
                                                                      ),
                                                               column(4, 
                                                                      shiny::selectInput("method_col",
                                                                                         "Method column?", 
                                                                                         choices = c("ward.D", 
                                                                                                     "ward.D2",
                                                                                                     "single",
                                                                                                     "complete",
                                                                                                     "average",
                                                                                                     "mcquitty",
                                                                                                     "median",
                                                                                                     "centroid"
                                                                                                     )
                                                                                         )
                                                                      )
                                                               ),
                                                      fluidRow(column(4, 
                                                                      shiny::selectInput("distance_row",
                                                                                  "Diatance row?", 
                                                                                  choices = c("euclidean", 
                                                                                              "maximum",
                                                                                              "manhattan",
                                                                                              "canberra",
                                                                                              "binary",
                                                                                              "minkowski",
                                                                                              "pearson",
                                                                                              "spearman",
                                                                                              "kendall"
                                                                                              )
                                                                                  )
                                                                      ),
                                                               column(4, 
                                                                      shiny::selectInput("distance_col",
                                                                                         "Distance column?",
                                                                                         choices = c("euclidean",
                                                                                                     "maximum",
                                                                                                     "manhattan",
                                                                                                     "canberra",
                                                                                                     "binary",
                                                                                                     "minkowski",
                                                                                                     "pearson",
                                                                                                     "spearman",
                                                                                                     "kendall"
                                                                                                     )
                                                                                         )
                                                                      )
                                                               ),
                                                      shiny::actionButton("confirmHeatmap",
                                                                          "Heatmap")
                                                      )
                                 ),
                shiny::tabPanel( "Differential_Analysis",
                                 shinydashboard::box( title = "", 
                                                      width = NULL,
                                                      shiny::selectInput("diff_group",
                                                                         "Select a group.", 
                                                                         choices = NA
                                                                         ),
                                                      fluidRow(column(5, 
                                                                      shiny::selectInput("p_signif",
                                                                                         "Select a sig level.", 
                                                                                         choices = c(0.05,
                                                                                                     0.01, 
                                                                                                     0.001
                                                                                                     )
                                                                                         )
                                                                      ),
                                                               column(5, 
                                                                      shiny::selectInput("diff_method",
                                                                                         "Select a method.",
                                                                                         choices = c("K-W", 
                                                                                                     "Wilcox"
                                                                                                     )
                                                                                         )
                                                                      )
                                                               ),
                                                      shiny::selectInput("paired_method",
                                                                         "Paired or not.", 
                                                                         choices = c("TRUE",
                                                                                     "FALSE"
                                                                                     )
                                                                         ),
                                                      shiny::actionButton("confirmDifferential",
                                                                          "Differential"
                                                                          )
                                                      )
                                 )
                )
                )
          )
  )
)


create_Feature_confirmFeatureFile <- function(input = input, 
                                              output = output, 
                                              rv = rv, 
                                              session = session){
  event <- shiny::observeEvent(input$confirmFeatureFile, {
    if(is.null(input$feature_file)){
      showNotification("No file uploaded.","Please upload a file.")
    }
    req(!is.null(input$feature_file))
    df = read_delimKB(input$feature_file$datapath)
    rv$data = df
    output$feature_table <- DT::renderDataTable({
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
    shiny::updateSelectInput(session = session, 
                             "id_feature", 
                             choice = colnames(df))
    shiny::updateSelectInput(session = session, 
                             "cols_to_discard", 
                             choice = colnames(df))
  }
  )
  
  
}


create_Feature_featureDetection <- function(input = input, 
                                            output = output,
                                            rv = rv, 
                                            session = session){
  event <- shiny::observeEvent(input$featureDetection, {
    req(rv$data)
    #sample_id = rv$data[[input$id_feature]]
    #req(sample_id)
    #if(sum(duplicated(sample_id)) > 0){
    #  showNotification("Sample ID wrong!","Please select a unique id.")
    #}
    #req(sum(duplicated(sample_id)) == 0)
    #row.names(rv$data) = sample_id
    #rv$data[[input$id_feature]] = NULL
    #browser()
    shiny::showTab(inputId = "tabset2", 
                   target =  "Stat_sample", 
                   session = session)
    shiny::showTab(inputId = "tabset2", 
                   target =  "Stat_feature", 
                   session = session)
    x_max = apply(rv$data, 
                  1, 
                  function(x) max(as.numeric(x[x != "NA"])[!is.na(as.numeric(x[x != "NA"]))]))
    x_min = apply(rv$data, 
                  1, 
                  function(x) min(as.numeric(x[x != "NA"])[!is.na(as.numeric(x[x != "NA"]))]))
    x_mean = apply(rv$data, 
                   1, 
                   function(x) mean(as.numeric(x[x != "NA"])[!is.na(as.numeric(x[x != "NA"]))]))
    x_med = apply(rv$data, 
                  1, 
                  function(x) median(as.numeric(x[x != "NA"])[!is.na(as.numeric(x[x != "NA"]))]))
    x_na = apply(rv$data, 
                 1, 
                 function(x)   length(x[x == "NA"])/ncol(rv$data))
    x_stat = data.frame(samples = row.names(rv$data),
                       min = x_min,
                       max = x_max,
                       mean = round(x_mean, 2),
                       med = x_med,
                       na = x_na)
    output$feature_stat_sample <- DT::renderDataTable({
      #browser()
      DT::datatable(x_stat, 
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = 6,
                      dom = 'Bfrtip',
                      buttons = c('print','excel')
                    )
      )
    }
    )
    output$feature_plot_sample = renderPlot({   
      hist(x_na, 
           main = "", 
           xlab = "")
    })
    #browser()
    y_max = apply(rv$data, 
                  2, 
                  function(x) max(as.numeric(x[x != "NA"])[!is.na(as.numeric(x[x != "NA"]))]))
    y_min = apply(rv$data, 
                  2, 
                  function(x) min(as.numeric(x[x != "NA"])[!is.na(as.numeric(x[x != "NA"]))]))
    y_mean = apply(rv$data, 
                   2, 
                   function(x) mean(as.numeric(x[x != "NA"])[!is.na(as.numeric(x[x != "NA"]))]))
    y_med = apply(rv$data, 
                  2, 
                  function(x) median(as.numeric(x[x != "NA"])[!is.na(as.numeric(x[x != "NA"]))]))
    y_na = apply(rv$data, 
                 2, 
                 function(x)  length(x[x == "NA"])/nrow(rv$data))
    y_stat = data.frame(features = colnames(rv$data),
                        min = y_min,
                        max = y_max,
                        mean = round(y_mean,2),
                        med = y_med,
                        na = y_na)
    output$feature_stat_feature <- DT::renderDataTable({
      #browser()
      DT::datatable(y_stat, 
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = 6,
                      dom = 'Bfrtip',
                      buttons = c('print','excel')
                    )
      )
    }
    )
    output$feature_plot_feature = renderPlot({   
      hist(y_na, 
           main = "", 
           xlab = "")
    })
    rv$sample_stat <- x_stat
    rv$feature_stat <- y_stat
    shiny::updateSelectInput(session = session, 
                      "id_feature", 
                      choice = colnames(df))
  })
  
  
}



create_Feature_confirmRemoveFeatureNA <- function(input = input, 
                                                  output = output, 
                                                  rv = rv, 
                                                  session = session){
  event <- shiny::observeEvent(input$confirmRemoveFeatureNA,{
    req(rv$feature_stat)
    rv$feature_bak = rv$data
    #browser()
    featureid = filter(rv$feature_stat, 
                       na < input$proportion_feature/100) %>% pull(features)
    #browser()
    rv$data = rv$data  %>% select(featureid)
    output$feature_table <- DT::renderDataTable({
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

create_Feature_cancelRemoveFeatureNA <- function(input = input, 
                                                 output = output,
                                                 rv = rv, 
                                                 session = session){
  event <- shiny::observeEvent(input$cancelRemoveFeatureNA, {
    req(rv$feature_bak)
    rv$data = rv$feature_bak
    output$feature_table <- DT::renderDataTable({
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



create_Feature_confirmRemoveSampleNA <- function(input = input, 
                                                 output = output, 
                                                 rv = rv, 
                                                 session = session){
  event <- observeEvent(input$confirmRemoveSampleNA, {
    req(rv$sample_stat)
    rv$sample_bak = rv$data
    #browser()
    sampleid = which(rv$sample_stat$na < input$proportion_sample/100)
    #browser()
    rv$data = rv$data  %>% dplyr::slice(sampleid)
    row.names(rv$data) = rv$sample_stat$samples[which(rv$sample_stat$na < input$proportion_sample/100)]
    output$feature_table <- DT::renderDataTable({
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



create_Feature_cancelRemoveSampleNA <- function(input = input, 
                                                output = output, 
                                                rv = rv, 
                                                session = session){
  event <- shiny::observeEvent(input$cancelRemoveSampleNA, {
    req(rv$sample_bak)
    rv$data = rv$sample_bak
    output$feature_table <- DT::renderDataTable({
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


create_Feature_confirmFillNA <- function(input = input, 
                                         output = output, 
                                         rv = rv, 
                                         session){
  event <- shiny::observeEvent(input$confirmFillNA, {
    req(rv$data)
    rv$fill_bak = rv$data
    rv$data = sapply(rv$data, 
                     function(x) suppressWarnings(as.numeric(as.character(x)))) %>% as.data.frame()
    row.names(rv$data) = row.names(rv$fill_bak)
    #browser()
    if(input$fillNAmethod == "constant"){
      req(input$fillFeatureNA)
      rv$data[is.na(rv$data) | rv$data == "NA"] = suppressWarnings(as.numeric(input$fillFeatureNA))
      #browser()
    }else if(input$fillNAmethod == "row_mean"){
      #browser()
      rv$data = apply(rv$data, 
                      1, 
                      function(x) ifelse(x == "NA" | is.na(x), mean(as.numeric(x[x != "NA" & !is.na(x)])), x)) %>% t() %>% as.data.frame()
    }else if(input$fillNAmethod == "col_mean"){
      rv$data = apply(rv$data, 
                      2, 
                      function(x) ifelse(x == "NA" | is.na(x), mean(as.numeric(x[x != "NA" & !is.na(x)])), x))
    }else if(input$fillNAmethod == "col_min"){
      rv$data = apply(rv$data, 
                      2, 
                      function(x) ifelse(x == "NA" | is.na(x), min(as.numeric(x[x != "NA" & !is.na(x)])), x))
    }else if(input$fillNAmethod == "col_max"){
      rv$data = apply(rv$data, 
                      2, 
                      function(x) ifelse(x == "NA" | is.na(x), max(as.numeric(x[x != "NA" & !is.na(x)])), x))
    }else if(input$fillNAmethod == "row_max"){
      rv$data = apply(rv$data, 
                      1, 
                      function(x) ifelse(x == "NA" | is.na(x), max(as.numeric(x[x != "NA" & !is.na(x)])), x)) %>% t() %>% as.data.frame()
    }else if(input$fillNAmethod == "row_min"){
      rv$data = apply(rv$data, 
                      1, 
                      function(x) ifelse(x == "NA" | is.na(x), min(as.numeric(x[x != "NA" & !is.na(x)])), x)) %>% t() %>% as.data.frame()
    }else{}
    #rv$data[rv$data == "NA"] = as.numeric(input$fillFeatureNA)
    output$feature_table <- DT::renderDataTable({
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
    rv$fillNA_status  = 1 
  })
  
  
  
}


create_Feature_cancelFillNA <- function(input = input, 
                                        output = output, 
                                        rv = rv, 
                                        session = session){
  event <- shiny::observeEvent(input$cancelFillNA, {
    req(rv$fill_bak)
    rv$data = rv$fill_bak
    output$feature_table <- DT::renderDataTable({
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
    rv$fillNA_status  = 0 
  })
  
  
}


create_Feature_confirmTransform <- function(input = input,
                                            output = output, 
                                            rv = rv, 
                                            session = session){
  event <- observeEvent(input$confirmTransform, {
    req(rv$data)
    req(sum(rv$data == "NA")  == 0)
    rv$transform_bak = rv$data
    #browser()
    if(input$transformMethod == "log"){
      rv$data = apply(rv$data, 
                      2, 
                      function(x) log(as.numeric(x) + 1)) %>% as.data.frame()
    }else if(input$transformMethod == "log2"){
      rv$data = apply(rv$data, 
                      2, 
                      function(x) log2(as.numeric(x) + 1)) %>% as.data.frame()
    }else if(input$transformMethod == "log10"){
      rv$data = apply(rv$data, 
                      2, 
                      function(x) log10(as.numeric(x) + 1)) %>% as.data.frame()
    }else{
      
    }
    row.names(rv$data) = row.names(rv$transform_bak)
    output$feature_table <- DT::renderDataTable({
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

create_Feature_cancelTransform <- function(input = input, 
                                           output = output, 
                                           rv = rv, 
                                           session = session){
  event <- shiny::observeEvent(input$cancelTransform, {
    req(rv$transform_bak)
    rv$data = rv$transform_bak
    output$feature_table <- DT::renderDataTable({
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


create_Feature_confirmPCA <- function(input = input, 
                                      output = output, 
                                      rv = rv, 
                                      session = session){
  event <- observeEvent(input$confirmPCA, {
    #browser()
    req(rv$data)
    if(is.null(rv$fillNA_status)){
      showNotification("NA values detected.", 
                       "NA filling is needed.")
    }
    #browser()
    if(!is.null(rv$fillNA_status)){
      if(rv$fillNA_status  == 0){
        showNotification("NA values detected.", 
                         "NA filling is needed."
                         )
      }else{
        
      }
      
    }
    req(rv$fillNA_status  == 1)
    #browser()
    #rv$data = apply(rv$data, 2, as.numeric)  %>% as.data.frame()
    if(input$axis == "X"){
      pca = prcomp(rv$data, 
                   center = as.logical(input$center_or_not), 
                   scale. = as.logical(input$scale_or_not)
                   )
    }else{
      pca = prcomp(t(rv$data), 
                   center = as.logical(input$center_or_not), 
                   scale. = as.logical(input$scale_or_not)
                   )
    }
    
    library(ggplot2)
    output$plot2 = renderPlot({
      #library("ggpubr")
      ggplot(pca$x %>% as.data.frame(), 
             aes(x = PC1,
                 y = PC1, 
                 label = row.names(pca$x)
                 )
             ) + 
        geom_point(size = 5) + ggrepel::geom_text_repel() + theme_bw() + 
        theme(axis.title = element_text(size=16),
              axis.text = element_text(size= 12))
    })
    
  })
  
  
  
  
}



create_Feature_Join_meta_and_feature <- function(input = input, 
                                                 output = output, 
                                                 rv_meta = rv_meta, 
                                                 rv_feature = rv_feature, 
                                                 session = session){
  event <- shiny::observeEvent(input$Join_meta_and_feature, {
     #browser()
    if(is.null(rv_meta$data)){
      showNotification("Metadata is needed.",
                       "Please upload metadata.")
    }
    if(is.null(rv_feature$data)){
      showNotification("Feature data is needed.",
                       "Please upload feature data."
                       )
    }
    req(rv_meta$data)
    req(rv_feature$data)
    #  req(rv_meta$data)
     overlap_id = intersect(rv_meta$data$id, 
                            row.names(rv_feature$data)
                            )
     if(length(overlap_id) == 0 ){
       showNotification("Merge Error!", 
                        "Please check the id for meta and feature.")
     }
     req(!is.null(overlap_id))
     showNotification("Merge successfully!", 
                      paste0("There are ", 
                             length(overlap_id), 
                             " samples that merged."
                             ), 
                      type = "message")
     rv_feature$overlap_id = overlap_id
     pca = prcomp(rv_feature$data[overlap_id,], 
                  center =T, 
                  scale. = T)
     #browser()
     if(input$pca_color == "NA"){
       if(input$pca_shape == "NA"){
         p =  ggplot(pca$x %>% as.data.frame(), 
                     aes(x = PC1, 
                         y = PC1, 
                         label = row.names(pca$x)
                         )
                     ) + 
           geom_point(size = 5) + ggrepel::geom_text_repel() + theme_bw() + 
           theme(axis.title = element_text(size=16),
                 axis.text = element_text(size= 12))
       }else{
         df_merge = as.data.frame(pca$x)
         df_merge$id = row.names(pca$x)
         df_merge = df_merge %>% 
           left_join(rv_meta$data, 
                     by = c("id" = "id")
                     )
         p =  ggplot(df_merge, 
                     aes(x = PC1, 
                         y = PC1, 
                         label = row.names(pca$x)
                         )
                     ) + 
           geom_point(size = 5, 
                      aes(shape = !!as.name(input$pca_shape)
                          )
                      ) + theme_bw() + 
           theme(axis.title = element_text(size=16),
                 axis.text = element_text(size= 12)) + labs(color = input$pca_shape)
       }
     }else{
       if(input$pca_shape == "NA"){
         df_merge = as.data.frame(pca$x)
         df_merge$id = row.names(pca$x)
         df_merge = df_merge %>% 
           left_join(rv_meta$data, 
                     by = c("id" = "id")
                     )
         p =  ggplot(df_merge, 
                     aes(x = PC1, 
                         y = PC1, 
                         label = row.names(pca$x)
                         )
                     ) + 
           geom_point(size = 5, 
                      aes(color = !!as.name(input$pca_color))
                      )  + theme_bw() + 
           theme(axis.title = element_text(size=16),
                 axis.text = element_text(size= 12)) + labs(color = input$pca_color )
       }else{
         df_merge = as.data.frame(pca$x)
         df_merge$id = row.names(pca$x)
         #browser()
         df_merge = df_merge %>% 
           left_join(rv_meta$data, 
                     by = c("id" = "id")
                     )
         p =  ggplot(df_merge, 
                     aes(x = PC1, 
                         y = PC1, 
                         label = row.names(pca$x)
                         )
                     ) + 
           geom_point(size = 5, 
                      aes(shape = !!as.name(input$pca_shape), 
                          color = !!as.name(input$pca_color)
                          )
                      ) + theme_bw() + 
           theme(axis.title = element_text(size=16),
                 axis.text = element_text(size= 12)) + labs(color = input$pca_color, 
                                                            shape = input$pca_shape) 
       }
     }
     
     output$plot2 = renderPlot({
       #library("ggpubr")
       p
     })
    
  })
  
  
  
  
}

create_Feature_confirmHeatmap <- function(input = input, 
                                          output = output, 
                                          session = session, 
                                          rv_meta = rv_meta, 
                                          rv_feature = rv_feature){
  event <- shiny::observeEvent(input$confirmHeatmap, {
    if(is.null(rv_meta$data)){
      showNotification("Metadata is needed.",
                       "Please upload metadata.")
    }
    if(is.null(rv_feature$data)){
      showNotification("Feature data is needed.", 
                       "Please upload feature data.")
    }
    req(rv_meta$data)
    req(rv_feature$data)
    overlap_id = intersect(rv_meta$data$id, 
                           row.names(rv_feature$data))
    if(length(overlap_id) == 0 ){
      showNotification("Merge Error!",
                       "Please check the id for meta and feature."
                       )
    }
    req(!is.null(overlap_id))
    showNotification("Merge successfully!", 
                     paste0("There are ", 
                            length(overlap_id), 
                            " samples that merged."
                            ), 
                     type = "message")
    rv_feature$overlap_id = overlap_id
    df_meta = as.data.frame(rv_meta$data)
    row.names(df_meta) = df_meta$id
    #x1 = as.name(input$heatmap_discrete)
    #x2 = as.name(input$heatmap_continuous)
    #browser()
    ha1 = ComplexHeatmap::HeatmapAnnotation(Var1 = df_meta[overlap_id,] %>% 
                                              pull(as.name(input$heatmap_discrete)),
                            Var2 = df_meta[overlap_id,] %>% 
                              pull(as.name(input$heatmap_continuous)) %>% as.numeric())
    #Heatmap(rv_feature$data[overlap_id,], name = "rnorm", col = col_rnorm, top_annotation = ha1)
    #browser()
    #ha1 = ComplexHeatmap::HeatmapAnnotation(as.name(input$heatmap_discrete) = c())
    output$plot2 = renderPlot({
      #library("ggpubr")
      #p
      #browser()
      ComplexHeatmap::Heatmap(t(rv_feature$data[overlap_id,]), 
                              name = "value", 
                              top_annotation = ha1,
                              show_row_names = as.logical(input$name_row),
                              show_column_names = as.logical(input$name_col),
                              show_row_dend = as.logical(input$dend_row),
                              show_column_dend = as.logical(input$dend_col),
                              cluster_rows = as.logical(input$cluster_row),
                              cluster_columns = as.logical(input$cluster_col),
                              clustering_method_rows =  input$method_row,
                              clustering_method_columns =   input$method_col,
                              clustering_distance_rows = input$distance_row,
                              clustering_distance_columns = input$distance_col
                              )
    })
    
  })
  
  
}


create_Feature_confirmDifferential <- function(input = input, 
                                               output = output, 
                                               session = session, 
                                               rv_meta = rv_meta, 
                                               rv_feature = rv_feature){
  event <- shiny::observeEvent(input$confirmDifferential, {
    if(is.null(rv_meta$data)){
      showNotification("Metadata is needed.", 
                       "Please upload metadata.")
    }
    if(is.null(rv_feature$data)){
      showNotification("Feature data is needed.", 
                       "Please upload feature data.")
    }
    req(rv_meta$data)
    req(rv_feature$data)
    overlap_id = intersect(rv_meta$data$id, 
                           row.names(rv_feature$data))
    if(length(overlap_id) == 0 ){
      showNotification("Merge Error!",
                       "Please check the id for meta and feature.")
    }
    req(!is.null(overlap_id))
    showNotification("Merge successfully!",
                     paste0("There are ", 
                            length(overlap_id), 
                            " samples that merged."
                            ), 
                     type = "message"
                     )
    rv_feature$overlap_id = overlap_id
    df_meta = as.data.frame(rv_meta$data)
    row.names(df_meta) = df_meta$id
    #browser()
    df_merge = as.data.frame(rv_feature$data)
    df_merge$id = row.names(rv_feature$data)
    df_merge = df_merge %>% 
      left_join(rv_meta$data[,c("id",input$diff_group)], 
                by = c("id" = "id")
                )
    stat_p = NULL
    if(input$diff_method == "K-W"){
      stat_p = lapply(c(1:ncol(rv_feature$data)), 
                      function(x) kruskal.test(df_merge[,x], df_merge[[input$diff_group]])$p.value) %>% unlist()
    }else{
      if(length(unique(df_merge[[input$diff_group]])) != 2){
        showNotification("Method selection wrong!", 
                         "When use wilcox test, the group have be 2."
                         )
      }else{
        categories = df_merge[[input$diff_group]] %>% unique()
        bin_value = ifelse(df_merge[[input$diff_group]] == categories[1], 0, 1)
        if(sum(bin_value == 0) != sum(bin_value == 1) & input$paired_method == "TRUE"){
          showNotification("Wrong selection!", 
                           "Not paired.")
        }else{
        #browser()
        stat_p = lapply(c(1:ncol(rv_feature$data)), 
                        function(x) wilcox.test(df_merge[,x] ~ bin_value, paired = as.logical(input$paired_method))$p.value) %>% unlist()
        }#browser()
        }
    }
    req(!is.null(stat_p))
    diff_stat = data.frame(feature = colnames(rv_feature$data),
                           p_value = stat_p)
    diff_sig = diff_stat %>% 
      filter(p_value < as.numeric(input$p_signif))
    if(nrow(diff_sig) < 1){
      shiny::showNotification("No feature detected.",
                              "Plese couble check.")
    }
    req(nrow(diff_sig) >0)
    rv_feature$diff_status = 1
    rv_feature$diff_data = df_merge
    rv_feature$diff_sig = diff_sig
    output$plot2 = renderPlot({
      ggplot(data = NULL,aes(x = df_merge[[input$diff_group]], 
                             y = df_merge[[diff_sig$feature[1]]])) + 
        geom_jitter(width = 0.3) + xlab(input$diff_group) + 
        ylab(diff_sig$feature[1]) + theme_bw() + 
        theme(axis.title = element_text(size= 18), 
              axis.text = element_text(size= 15))
      
      
    })
    
    output$feature_table <- DT::renderDataTable({
      #browser()
      DT::datatable(diff_sig, 
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = 8,
                      dom = 'Bfrtip',
                      buttons = c('print','excel')
                    )
      )
    })
    
  })
  
  
}


create_obs_nextPage <- function(input,
                                output, 
                                rv){
  event <- observe({
    shinyjs::hide("nextPage")
    if(!is.null(rv$diff_status))
      shinyjs::show("nextPage")
  })
  return(event)
}

create_obs_previousPage <- function(input,
                                    output, 
                                    rv){
  event <- observe({
    shinyjs::hide("previousPage")
    if(!is.null(rv$diff_status)){
      if(rv$diff_status > 1){
        shinyjs::show("previousPage")
      }
    }
  })
  return(event)
}


create_Feature_nextPage <- function(input, 
                                    output, 
                                    rv){
  event <- observeEvent(input$nextPage,{
    df_merge = rv$diff_data
    diff_sig = rv$diff_sig
    rv$diff_status = rv$diff_status + 1
    output$plot2 = renderPlot({
      ggplot(data = NULL,aes(x = df_merge[[input$diff_group]], 
                             y = df_merge[[diff_sig$feature[rv$diff_status]]])) + 
        geom_jitter(width = 0.3) + xlab(input$diff_group) + 
        ylab(diff_sig$feature[rv$diff_status]) + theme_bw() + 
        theme(axis.title = element_text(size= 18), 
              axis.text = element_text(size= 15))
    })
    
  })
  
}

create_Feature_previousPage <- function(input, 
                                        output, 
                                        rv){
  event <- observeEvent(input$previousPage, {
    df_merge = rv$diff_data
    diff_sig = rv$diff_sig
    rv$diff_status = rv$diff_status - 1
    output$plot2 = renderPlot({
      ggplot(data = NULL,aes(x = df_merge[[input$diff_group]], 
                             y = df_merge[[diff_sig$feature[rv$diff_status]]])) + 
        geom_jitter(width = 0.3) + xlab(input$diff_group) + 
        ylab(diff_sig$feature[rv$diff_status]) + theme_bw() + 
        theme(axis.title = element_text(size= 18), 
              axis.text = element_text(size= 15))
      
      
    })
    
  })
  
}

create_obs_kwPairedMethod <- function(input, 
                                      output){
  event <- observe({
    #browser()
    if(input$diff_method == "K-W"){
      shinyjs::hide("paired_method")
    }else{
      shinyjs::show("paired_method")
    }
  })
  return(event)
}

create_obs_fillNA <- function(input, 
                              output){
  event <- observe({
    if(input$fillNAmethod == "constant"){
      shinyjs::show("fillFeatureNA")
    }else{
      shinyjs::hide("fillFeatureNA")
    }
  })
  return(event)
}


create_Feature_Prepare <- function(input, 
                                   output, 
                                   rv){
  event <- observeEvent(input$Prepare, {
    req(rv$data)
    rv$data_bak = rv$data
    sample_id = rv$data[[input$id_feature]]
    if(sum(duplicated(sample_id)) > 0){
      showNotification("Sample ID wrong!",
                       "Please select a unique id.")
    }
    if(input$cols_to_discard != ""){
      for(col in input$cols_to_discard){
        rv$data[[col]] = NULL
      }
    }
    req(sum(duplicated(sample_id)) == 0)
    if(input$transpose_or_not == TRUE){
      row.names(rv$data) = sample_id
      rv$data[[input$id_feature]] = NULL
      rv$data = t(rv$data) %>% as.data.frame()
      colnames(rv$data) = sample_id
    }else{
      row.names(rv$data) = rv$data[[input$id_feature]]
      rv$data[[input$id_feature]] = NULL
    }
    
    #diff_sig = rv$diff_sig
    #rv$diff_status = rv$diff_status - 1
    output$feature_table <- DT::renderDataTable({
      #browser()
      DT::datatable(rv$data, 
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = 8,
                      dom = 'Bfrtip',
                      buttons = c('print','excel')
                    )
      )
    })
    
  })
  
}

create_Feature_CancelPrepare <- function(input, 
                                         output, 
                                         rv){
  event <- observeEvent(input$CancelPrepare,{
    req(rv$data_bak)
    rv$data = rv$data_bak
    output$feature_table <- DT::renderDataTable({
      #browser()
      DT::datatable(rv$data, 
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = 8,
                      dom = 'Bfrtip',
                      buttons = c('print','excel')
                    )
      )
    })
  })
  
}


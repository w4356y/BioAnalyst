library(shiny)
source('R/configuration.R', 
       encoding = "UTF-8")
source('R/login.R', 
       encoding = "UTF-8")
server <- function(input, 
                   output, 
                   session) {
  #setBookmarkExclude(c("Bookmark"))
  #enableBookmarking(store = "url")
  rv_meta <- reactiveValues()
  rv_feature <- reactiveValues()
  rv_ai <- reactiveValues()
  rv <- reactiveValues()
  
  shiny::hideTab(inputId = "tabset1", 
                 target =  "DataTable_Continuous", 
                 session = session)
  shiny::hideTab(inputId = "tabset1", 
                 target =  "DataTable_Discrete", 
                 session = session)
  shiny::hideTab(inputId = "tabset2", 
                 target =  "Stat_sample",
                 session = session)
  shiny::hideTab(inputId = "tabset2", 
                 target =  "Stat_feature", 
                 session = session)
  shinyjs::hide(selector = "a[data-value = 'BioMeta']")
  shinyjs::hide(selector = "a[data-value = 'BioAnalysis']")
  shinyjs::hide(selector = "a[data-value = 'BioAI']")
  
  create_login_confirm(session = session,
                       input = input,
                       output = output)
  
  create_Metadata_ReadConfirm(input = input,
                              output = output,
                              session = session,
                              rv = rv_meta)
  create_Metadata_Detection(input = input,
                              output = output,
                              session = session,
                              rv = rv_meta)

  create_Metadata_confirmHist(input = input,
                              output = output,
                              session = session,
                              rv = rv_meta)

  create_Metadata_confirmPie(input = input,
                              output = output,
                              session = session,
                              rv = rv_meta)


  create_Metadata_confirmCorelationMeta(input = input,
                             output = output,
                             session = session,
                             rv = rv_meta)
  #create_hover
  create_Metadata_confirmRegressMeta(input = input,
                                     output = output,
                                     session = session,
                                     rv = rv_meta)

  create_Metadata_summaryMetaFile(input = input,
                                  output = output,
                                  session = session,
                                  rv = rv_meta)

  create_Metadata_confirmFillna(input = input,
                                output = output,
                                session = session,
                                rv = rv_meta)

  create_Metadata_Bookmark(input = input,
                           output = output,
                           session = session,
                           rv = rv_meta)

  create_Metadata_confirmRemoveNA(input = input,
                                  output = output,
                                  session = session,
                                  rv = rv_meta)

  create_Metadata_filterConfirm(input = input,
                                output = output,
                                session = session,
                                rv = rv_meta)

  create_Metadata_offsetFilter(input = input,
                               output = output,
                               session = session,
                               rv = rv_meta)

  create_Metadata_confirmChi_test(input = input,
                                  output = output,
                                  rv = rv_meta)
  
  


#  ----------------------------- Feature----------------

  create_Feature_confirmFeatureFile(input = input,
                                     output = output,
                                     session = session,
                                     rv = rv_feature)

  create_Feature_featureDetection(input = input,
                                    output = output,
                                    session = session,
                                    rv = rv_feature)

  create_Feature_confirmRemoveFeatureNA(input = input,
                                        output = output,
                                        session = session,
                                        rv = rv_feature)

  create_Feature_cancelRemoveFeatureNA(input = input,
                                       output = output,
                                       session = session,
                                       rv = rv_feature)

  create_Feature_confirmRemoveSampleNA(input = input,
                                       output = output,
                                       session = session,
                                       rv = rv_feature)

  create_Feature_cancelRemoveSampleNA(input = input,
                                      output = output,
                                      session = session,
                                      rv = rv_feature)

  create_Feature_confirmFillNA(input = input,
                               output = output,
                               session = session,
                               rv = rv_feature)

  create_Feature_cancelFillNA(input = input,
                              output = output,
                              session = session,
                              rv = rv_feature)

  create_Feature_confirmTransform(input = input,
                                  output = output,
                                  session = session,
                                  rv = rv_feature)

  create_Feature_cancelTransform(input = input,
                                 output = output,
                                 session = session,
                                 rv = rv_feature)

  create_Feature_confirmPCA(input = input,
                            output = output,
                            session = session,
                            rv = rv_feature)

  create_Feature_Join_meta_and_feature(input = input,
                                       output = output,
                                       session = session,
                                       rv_meta = rv_meta,
                                       rv = rv_feature)
  create_Feature_confirmHeatmap(input = input,
                                output = output,
                                session = session,
                                rv_meta = rv_meta,
                                rv = rv_feature)

  create_Feature_confirmDifferential(input = input,
                                     output = output,
                                     session = session,
                                     rv_meta = rv_meta,
                                     rv = rv_feature)
  create_Feature_Prepare(input = input,
                            output = output,
                            rv = rv_feature)
  
  create_Feature_CancelPrepare(input = input,
                                  output = output,
                                  rv = rv_feature)

  create_obs_nextPage(input = input,
                      output = output,
                      rv = rv_feature)

  create_obs_previousPage(input = input,
                      output = output,
                      rv = rv_feature)

  create_Feature_nextPage(input = input,
                          output = output,
                          rv = rv_feature)

  create_Feature_previousPage(input = input,
                          output = output,
                          rv = rv_feature)

  # create_obs_kwPairedMethod(input = input,
  #                           output = output)


  create_obs_fillNA(input = input,
                    output = output)


## -------------------------AI module-------------------------
  create_obs_dataSource(input = input,
                        output = output)

  create_obs_splitChoice(input = input,
                        output = output,
                        session = session)

  create_obs_splitMethod(input = input,
                         output = output)

  create_ai_confirm_ai_data(input = input,
                                 output = output,
                                 rv = rv_ai,
                            rv_meta = rv_meta,
                            rv_feature = rv_feature)

  create_ai_confirm_config(input = input,
                           output = output,
                           rv = rv_ai)

  create_obs_updateGroup(input = input,
                         output = output,
                         session = session,
                         rv = rv_ai)

  create_obs_TrainMethod(input = input,
                         output = output)


  create_ai_confirm_logreg(input = input,
                           output = output,
                           rv = rv_ai)

  create_ai_confirm_knn(input = input,
                        output = output,
                        rv = rv_ai)


  create_ai_confirm_splsda(input = input,
                           output = output,
                           rv = rv_ai)

  create_ai_confirm_svmLinear(input = input,
                              output = output,
                              rv = rv_ai)

  create_ai_confirm_rf(input = input,
                              output = output,
                              rv = rv_ai)

  create_obs_aiTab(input = input,
                   output = output,
                   session = session)

  create_ai_confirm_lr(input = input,
                       output = output,
                       rv = rv_ai)
  create_clickLog(input = input,
                  output = output,
                  session = session)




}



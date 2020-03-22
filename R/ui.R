library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
source("R/metadata.R", encoding = "UTF-8")
source("R/siderbar_func.R", encoding = "UTF-8")
source("R/ai.R", encoding = "UTF-8")
source("R/dashboard.R", encoding = "UTF-8")
source("R/feature.R", encoding = "UTF-8")

ui <-  dashboardPagePlus(skin  = "green-light",
  header =  create_header(),
  sidebar =   shinydashboard::dashboardSidebar(disable = FALSE, collapsed = FALSE,
     sidebarMenu(id = 'tabs', icon = icon("home"),
                            menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
                            menuItem("BioMeta", tabName = "BioMeta", icon = icon("moon")),
                            menuItem('BioAnalysis', tabName = "BioAnalysis", icon = icon("signal")),
                            menuItem("BioAI", tabName = "BioAI", icon = icon("plane"))
                 #bookmarkButton(id = "BookmarK")
    )),
  rightsidebar = bs4Dash::dashboardControlbar(skin = 'dark', inputId = "controlBar",
                                              title = h2("Filter meta"),disable = TRUE,
                                              br(),
                                              box(title = "Condition 1",width = "100%",
                                              selectInput("filterCondition1","Please select a column.",choices = NA),
                                              selectInput("relationCondition1","Please select a relation",
                                                          choices = c(">","<","==","<>","<=",">=","in")),
                                              textInput("valueCondition1", "Please type in a value.")
                                              ),
                                              selectInput("joinRelation","Select a relation.", choices = c("And","Or")),
                                              box(title = "Condition 2",width = "100%",
                                                selectInput("filterCondition2","Please select a column.",choices = NA),
                                                selectInput("relationCondition2","Please select a relation",
                                                            choices = c(">","<","==","<>","<=",">=","in")),
                                                textInput("valueCondition2", "Please type in a value.")
                                              ),
                                              fluidRow(column(3, actionButton("filterConfirm","Confirm")
                                                              ),
                                                       column(3,  actionButton("offsetFilter","Offset")
                                                              ))

          ),
  #browser(),
  body =   dashboardBody(#useShinyjs(),
                          #useShinyjs(),
      tabItems(
        tabItem(tabName = "dashboard",
                dashboard_mainbar
        ),

        tabItem(tabName = "BioMeta",
                metadata_sidebar,
                metadata_mainbar
                            ),
        tabItem(tabName = 'BioAnalysis',
                feature_mainbar
                            ),
        tabItem(tabName = 'BioAI',
                ai_mainbar
                )
      )

  )
)
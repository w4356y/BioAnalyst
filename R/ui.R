library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
source("R/metadata.R", 
       encoding = "UTF-8")
source("R/siderbar_func.R", 
       encoding = "UTF-8")
source("R/ai.R", 
       encoding = "UTF-8")
source("R/dashboard.R", 
       encoding = "UTF-8")
source("R/feature.R", 
       encoding = "UTF-8")

ui <-  shinydashboardPlus::dashboardPagePlus(
  shinythemes::themeSelector(),
  skin  = "green-light",
  header =  create_header(),
  sidebar =   shinydashboard::dashboardSidebar(
    disable = FALSE, 
    collapsed = FALSE,
    shinydashboard::sidebarMenu(
      id = 'tabs', 
      icon = icon("home"),
      shinydashboard::menuItem(
        "Dashboard", 
        tabName = "dashboard", 
        icon = icon("home")
        ),
      shinydashboard::menuItem(
        "BioMeta", 
        tabName = "BioMeta", 
        icon = icon("moon")
        ),
      shinydashboard::menuItem(
        'BioAnalysis', 
        tabName = "BioAnalysis", 
        icon = icon("signal")
        ),
      shinydashboard::menuItem(
        "BioAI", 
        tabName = "BioAI", 
        icon = icon("plane")
        )
                 #bookmarkButton(id = "BookmarK")
      )
    ),
  rightsidebar = bs4Dash::dashboardControlbar(
    skin = 'dark', 
    inputId = "controlBar",
    title = h2("Filter meta"), 
    disable = TRUE,
    br(),
    shinydashboard::box(
      title = "Condition 1", 
      width = "100%",
      shiny::selectInput(
        "filterCondition1", 
        "Please select a column.", 
        choices = NA
        ),
      shiny::selectInput(
        "relationCondition1", 
        "Please select a relation",
        choices = c(">","<","==","<>","<=",">=","in")
        ),
      shiny::textInput(
        "valueCondition1", 
        "Please type in a value."
        )
      ),
    shiny::selectInput(
      "joinRelation", 
      "Select a relation.", 
      choices = c("And","Or")
      ),
    shinydashboard::box(
      title = "Condition 2", 
      width = "100%",
      shiny::selectInput(
        "filterCondition2", 
        "Please select a column.", 
        choices = NA
        ),
      shiny::selectInput(
        "relationCondition2", 
        "Please select a relation",
        choices = c(">","<","==","<>","<=",">=","in")
        ),
      shiny::textInput(
        "valueCondition2", 
        "Please type in a value."
        )
      ),
    fluidRow(
      column(3, 
             shiny::actionButton("filterConfirm","Confirm")
             ),
      column(3, 
             shiny::actionButton("offsetFilter","Offset")
             )
      )
    ),
  #browser(),
  body =   shinydashboard::dashboardBody(
    #useShinyjs(),
    #useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      shinyjs::useShinyjs(),
      tags$script(src = "script.js")
    ),
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "dashboard",
        dashboard_mainbar
        ),
      shinydashboard::tabItem(
        tabName = "BioMeta",
        metadata_sidebar,
        metadata_mainbar
        ),
      shinydashboard::tabItem(
        tabName = 'BioAnalysis',
        feature_mainbar
        ),
      shinydashboard::tabItem(
        tabName = 'BioAI',
        ai_mainbar
        )
      )
    )
  )
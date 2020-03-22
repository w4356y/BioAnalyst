source("R/metadata.R", encoding = "UTF-8")
source("R/feature.R", encoding = "UTF-8")
navBar_ui <- function(){
  tagList(
  #golem_add_external_resources(),
  bs4Dash::bs4DashPage(
    tagList(
      bs4Dash::bs4DashNavbar(
        skin = "light",
        title = 'Research Analysis',
        id = "sidebar_bra",
          bs4Dash::menuItem(
            tabName = "parameter_review",
            metadata_sidebar,
            icon = 'heartbeat',
            "Parameter Review"
          ),
          bs4Dash::menuItem(
            tabName = "user_input",
            icon = 'keyboard',
            "Manual Input"
          )
        )
      )   
    )
)
}



body_ui <- function() {
  bs4Dash::bs4DashBody(
  bs4Dash::bs4TabItems( #add new tabItems in the sidebar -> rstudioapi::navigateToFile('R/mod_sidebar.R')
    bs4Dash::bs4TabItem(
      tabName = "dashboard"
    ),
    bs4Dash::bs4TabItem(
      tabName = 'parameter_review',
      metadata_mainbar
      #mod_parameter_review_ui("parameter_review_ui_1")
    )
  )
) 
}

create_header <- function(){
  header = dashboardHeaderPlus(
    enable_rightsidebar = TRUE, title =  "BioAnalyst",
    rightSidebarIcon = "gears" ,
    left_menu = shiny::tagList(
      # dropdownBlock(
      #   id = "mydropdown",
      #   title = "Dropdown 1",
      #   icon = "sliders",
      #   sliderInput(
      #     inputId = "n",
      #     label = "Number of observations",
      #     min = 10, max = 100, value = 30
      #   ),
      #   prettyToggle(
      #     inputId = "na",
      #     label_on = "NAs keeped",
      #     label_off = "NAs removed",
      #     icon_on = icon("check"),
      #     icon_off = icon("remove")
      #   )
      # ),
      dropdownBlock(id = "login",title = "Login",
                    #taskItem(value = 20, color = "aqua", "Refactor code"),
                    textInput("login.user","User ID"),
                    textInput("login.password","Password"),
                    textInput("anhao","暗号")
      )
      # dropdownBlock(
      #   id = "mydropdown2",
      #   title = "Dropdown 2",
      #   icon = "sliders",
      #   prettySwitch(
      #     inputId = "switch4",
      #     label = "Fill switch with status:",
      #     fill = TRUE, 
      #     status = "primary"
      #   ),
      #   prettyCheckboxGroup(
      #     inputId = "checkgroup2",
      #     label = "Click me!", 
      #     thick = TRUE,
      #     choices = c("Click me !", "Me !", "Or me !"),
      #     animation = "pulse", 
      #     status = "info"
      #   )
      # )
    )
    # dropdownMenu(
    #   type = "tasks", 
    #   badgeStatus = "danger",
    #   taskItem(value = 20, color = "aqua", "Refactor code"),
    #   taskItem(value = 40, color = "green", "Design new layout"),
    #   taskItem(value = 60, color = "yellow", "Another task"),
    #   taskItem(value = 80, color = "red", "Write documentation")
    # )
  )
  return(header)
}
   
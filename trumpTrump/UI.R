###################
#####  UI.R  ######
###################

library(shinydashboard)
library(shiny)
library(shinyjs)



#---------------------------------------
# HEADER
#---------------------------------------
header <- dashboardHeader(
  title = "Love Trumps Hate"
)



#---------------------------------------
# SIDEBAR
#---------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("TAB ONE", tabName = "tab1", icon = icon("snowflake-o")),
    menuItem("TAB TWO", tabName = "tab2", icon = icon("snowflake-o")),
    menuItem("TAB THREE", tabName = "tab3", icon = icon("snowflake-o"))
    
  )
)



#---------------------------------------
# BODY
#---------------------------------------
body <- dashboardBody(
  # -------------------
  # LOADING SCREEN
  # -------------------
  useShinyjs(),
  div(
    id = "loading_page",
    h1("Loading...")
  ),
  # -------------------
  # MAIN CONTENT
  # -------------------
  hidden(
    div(
      id = "main_content",
      
      tabItems(
        
        # -------------------
        # TAB ONE
        # -------------------
        tabItem(
          tabName = "tab1",
          fluidPage(
            h1("Matching Day of Week"),
            hr()
          ),
          fluidRow(
            hr(),
            
            box(
              title = "Choose Timeframe",
              # Copy the line below to make a select box 
              selectInput("tab1_timeFrame", label = h3("Select box"), 
                          choices = list("Year" = 1, "Month" = 2, "Day" = 3), 
                          selected = 1)
            ),
            box(
              title = "Choose Crime Type",
              # Copy the line below to make a select box 
              selectInput("tab1_crimeType", label = h3("Select box"), 
                          choices = list("Assault" = 1, "Battery" = 2, "Both" = 3), 
                          selected = 1)
            )
          ),
          fluidPage(
            h1("Crime Graph"),
            br(),
            plotOutput("tab1_plot")
          )
        ),
        # -------------------
        # SECOND TAB
        # -------------------
        tabItem(
          tabName = "tab2"
        ),
        # -------------------
        # THIRD TAB
        # -------------------
        tabItem(
          tabName = "tab3"
        )
      )
    )
  )
)




#---------------------------------------
# THE APP
#---------------------------------------
dashboardPage(
  header,
  sidebar,
  body
)







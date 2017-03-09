###################
#####  UI.R  ######
###################

library(shinydashboard)
library(shiny)


# -------------------
# HEADER
# -------------------
header <- dashboardHeader(title = "Ski School Forecasting")



# -------------------
# SIDEBAR
# -------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("TAB ONE", tabName = "tab1", icon = icon("snowflake-o")),
    menuItem("TAB TWO", tabName = "tab2", icon = icon("snowflake-o")),
    menuItem("TAB THREE", tabName = "tab3", icon = icon("snowflake-o"))
    
  )
)



# -------------------
# BODY
# -------------------
body <- dashboardBody(
  tabItems(
    # FIRST TAB
    tabItem(tabName = "tab1",
      fluidPage(
        titlePanel("Matching Day of Week"),
        h4("Check how accurate predicitons are when matching the same day of the week from the previous year")
      ),
      hr(),
      br(),
      fluidRow(
        plotOutput("plot1")
      ),
      fluidRow(
        h4("Choose Days"),
        checkboxGroupInput(
          "daysOfWeek", 
          label = NULL,#"Choose Days",
          choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
          selected = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
          inline =  TRUE
        ),
        box(
          title = "Correlation",
          actionButton("abButton1", label = "Add Line"),
          h4(textOutput("summaryTxt"))
        )
      )
    ),
    #SECOND TAB
    tabItem(
      tabName = "tab2"
    ),
    #THIRD TAB
    tabItem(
      tabName = "tab3"
    )
  )
)




# -------------------
# THE APP
# -------------------
dashboardPage(
	header,
	sidebar,
	body
)


###################
#####  UI.R  ######
###################

library(shinydashboard)
library(shinyjs)

#---------------------------------------
# HEADER
#---------------------------------------
header <- dashboardHeader(
  title = "Love Trumps Hate"
  #title = tags$img(src='MSc-Logo.jpg',height='60',width='200')
)





#---------------------------------------
# SIDEBAR
#---------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Intro", tabName = "tab1", icon = icon("play")),
    menuItem("Descriptive Statistics", tabName = "tab2", icon = icon("sort-numeric-desc")),
    menuItem("Inferential Statistics", tabName = "tab3", icon = icon("search-plus")),
    menuItem("Forecasting", tabName = "tab4", icon = icon("arrows-alt")),
    menuItem("BONUS", tabName = "tab5", icon = icon("free-code-camp"))
  )
)

#---------------------------------------
# LOADING SCREEN
#---------------------------------------
loadingScreen <-   div(
  id = "loading_page",
  img(src = "squares.gif"),
  h1("Loading...")
)

#---------------------------------------
# TAB PANELS
#---------------------------------------
# -------------------
# TAB ONE
# -------------------
tab1 <- tabItem(
  tabName = "tab1",
  fluidPage(
    titlePanel("Trump's Twitter Feed"),
    hr(),
    box(status = "primary",
        p("Trump has consistently criticised the city of Chicago for being a violent and dangerous place. He's suggested that violent crime is on the rise and implied that it is predominantly an inner city problem. He's also implied these crime disproportionately affect people of colour, and that he will help solve these issues of violence.")
    ),
    box(status = "primary", id = "trump-quotes-container",
        div(id = "trump-quotes-container",
            tags$iframe(id = "tweet-frame", src = "trumpTweets.html", frameBorder = 0)
        )
    )
  )
)

# -------------------
# SECOND TAB
# -------------------
tab2 <- tabItem(
  tabName = "tab2",
  fluidPage(
    h1("Heading Title"),
    hr()
  ),
  fluidRow(
    hr(),
    box(
      title = "Choose Timeframe",
      # Copy the line below to make a select box 
      selectInput("tab1_timeFrame", 
                  label = h3("Select box"), 
                  choices = list("Year" = 1, "Month" = 2, "Day" = 3), 
                  selected = 1)
    ),
    box(
      title = "Choose Crime Type",
      # Copy the line below to make a select box 
      selectInput("tab1_crimeType", 
                  label = h3("Select box"), 
                  choices = list("Assault" = 1, "Battery" = 2, "Both" = 3), 
                  selected = 1)
    )
  ),
  fluidPage(
    h1("Crime Graph"),
    br(),
    plotOutput("tab1_plot")
  )
)

# -------------------
# THIRD TAB
# -------------------
tab3 <- tabItem(
  tabName = "tab3"
)
# -------------------
# FOURTH TAB
# -------------------
tab4 <- tabItem(
  tabName = "tab4"
)
# -------------------
# FITH TAB
# -------------------
tab5 <- tabItem(
  tabName = "tab5"
)




#---------------------------------------
# MAIN CONTENT
#---------------------------------------
mainScreen <- div(
  id = "main_content",
  tabItems(
    tab1,
    tab2,
    tab3,
    tab4,
    tab5
  )
)


#---------------------------------------
# BODY
#---------------------------------------
body <- dashboardBody(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(
    id = "container",
    loadingScreen,
    hidden(mainScreen)
  )
)




#---------------------------------------
# THE APP
#---------------------------------------
dashboardPage(
  #skin = "purple",
  #skin = "black",
  header,
  sidebar,
  body
)




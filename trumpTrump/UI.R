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
  title = div(style = "display:block; background-color:#ffffff; text-align:center", tags$img(src='MSc-Logo.jpg',height='60',width='200')),
  sidebarMenu(
    menuItem("Intro", tabName = "tab1", icon = icon("play")),
    menuItem("Inputs", tabName = "tab2", icon = icon("sort-numeric-desc")),
    menuItem("Descriptive Statistics", tabName = "tab3", icon = icon("search-plus")),
    menuItem("Inferential Statistics", tabName = "tab4", icon = icon("wpexplorer")),
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
    #theme = "bootstrap.css",
    titlePanel("Trump's Twitter Feed"),
    hr(),
    column(5,
           box(width = 12,
               height = "inherit",
               status = "primary",
               p("Trump has consistently criticised the city of Chicago for being a violent and dangerous place. He's suggested that violent crime is on the rise and implied that it is predominantly an inner city problem. He's also implied these crimes disproportionately affect people of colour, and that he will help solve these issues of violence.")
           ),
           infoBox(title = "Question:", value = "Is Chicago at Risk?", subtitle = "Lets find out...",
                   icon = shiny::icon("search"), color = "aqua", width = 12,
                   href = NULL, fill = FALSE),
           valueBox(value = "Chicago", subtitle = "Is it at risk?", icon = icon("search"), color = "aqua", width = 12,
                    href = NULL)
    ),
    
    box(width = 7,
        status = "primary", id = "trump-quotes-container",
        div(id = "trump-quotes-container",
            tags$iframe(id = "tweet-frame", src = "trumpTweets.html", frameBorder = 0)
        )
    )
  )
)




# -------------------
# SECOND TAB (INPUTS)
# -------------------
tab2 <- tabItem(
  tabName = "tab2",
  fluidPage(
    titlePanel("Inputs"),
    hr(),
    column(12,
           box(width = 12,
               title = "Type of Crime",
               status = "danger",
               h5("Choose a types of crimes to investigate"),
               div(class = "multicol",
                   checkboxGroupInput("tab2_crimeTypes",
                                      #label = h5("Choose a types of crimes to investigate"),
                                      label = "",
                                      selected = "HOMICIDE",
                                      choices = c("ARSON","ASSAULT","BATTERY","BURGLARY","CONCEALED CARRY LICENSE VIOLATION","CRIM SEXUAL ASSAULT","CRIMINAL DAMAGE","CRIMINAL TRESPASS","DECEPTIVE PRACTICE","DOMESTIC VIOLENCE","GAMBLING","HOMICIDE","HUMAN TRAFFICKING","INTERFERENCE WITH PUBLIC OFFICER","INTIMIDATION","KIDNAPPING","LIQUOR LAW VIOLATION","MOTOR VEHICLE THEFT","NARCOTICS","NON - CRIMINAL","NON-CRIMINAL","NON-CRIMINAL (SUBJECT SPECIFIED)","OBSCENITY","OFFENSE INVOLVING CHILDREN","OTHER NARCOTIC VIOLATION","OTHER OFFENSE","PROSTITUTION","PUBLIC INDECENCY","PUBLIC PEACE VIOLATION","RITUALISM","ROBBERY","SEX OFFENSE","STALKING","THEFT","WEAPONS VIOLATION")
                   )
               )
           ),
           box(width = 12,
               title = "Time Frame",
               status = "danger",
               dateRangeInput("tab2_datesRange", 
                              label = h5("Choose range to investigate"),
                              start = "2001/1/1",
                              end = Sys.Date(),
                              min = "2001/1/1",
                              max = Sys.Date()
               )
           ),
           box(width = 12,
               title = "Location",
               status = "danger",
               leafletOutput("tab2_map")#, width = "100%", height = "400px")
           )
    )
  )
)





# -------------------
# THIRD TAB
# -------------------
tab3 <- tabItem(
  tabName = "tab3",
  titlePanel("Descriptive Statistics"),
  hr(),
  fluidRow(
    box(width = 12,
        title = "Selected Inputs",
        status = "warning",
        fluidRow(
          valueBoxOutput("tab3_timeRange_start", width = 6),
          valueBoxOutput("tab3_location_lat", width = 6)
        ),
        fluidRow(
          valueBoxOutput("tab3_timeRange_end", width = 6),
          valueBoxOutput("tab3_location_lng", width = 6)
        ),
        fluidRow(
          valueBox(value = "Crime Types", subtitle = "Selected:", icon = icon("superpowers"), width = 5),
          box(width = 7, htmlOutput("tab3_crimeTypes"), status = "warning")
        )
    )
  ),
  fluidRow(
    box(width = 12,
        title = "Crime Map",
        status = "warning",
        leafletOutput("tab3_map")
    )
  ),
  fluidRow(
    box(width = 8,
        title = "Time of Day",
        status = "warning",
        plotlyOutput("tab3_timeOfDay_hist")
        
    ),
    column(4,
           wellPanel(
             fluidRow(infoBoxOutput("tab3_timeOfDay_mean")),
             br(),
             fluidRow(infoBoxOutput("tab3_timeOfDay_stdev"))
           )
    )
  ),
  fluidRow(
    box(width = 8,
        title = "Time Series",
        status = "warning",
        dygraphOutput(
          "tab3_timeSeries", 
          width = "100%",
          height = "400px"
        )
    ),
    column(4,
           wellPanel(
             h5("Annual Mean"),
             fluidRow(infoBoxOutput("tab3_timeSeries_mean_y")),
             h5("Annual Standard Deviation"),
             fluidRow(infoBoxOutput("tab3_timeSeries_stdev_y")),
             h5("Annual Growth Rate"),
             fluidRow(infoBoxOutput("tab3_timeSeries_growthRate_y"))
           ),
           wellPanel(
             h5("Monthly Mean"),
             fluidRow(infoBoxOutput("tab3_timeSeries_mean_m")),
             h5("Monthly Standard Deviation"),
             fluidRow(infoBoxOutput("tab3_timeSeries_stdev_m")),
             h5("Monthly Growth Rate"),
             fluidRow(infoBoxOutput("tab3_timeSeries_growthRate_m"))
           )
    )
  )
)









# -------------------
# FOURTH TAB
# -------------------
tab4 <- tabItem(
  tabName = "tab4",
  titlePanel("Forecasting"),
  hr(),
  fluidRow(
    box(width = 12,
        title = textOutput("tab4_forecast_title"),
        status = "info",
        dygraphOutput("tab4_forecast",        
                      width = "100%",
                      height = "400px")
    )
  ),
  fluidRow(
    box(width = 6,
        title = "ACF",
        status = "info",
        plotOutput("tab4_plot_acf")
    ),
    box(width = 6,
        title = "Residuals",
        status = "info",
        plotOutput("tab4_plot_res")
    )
  ),
  fluidRow(
    box(width = 6,
        title = "Decomposistion",
        status = "info",
        plotOutput("tab4_plot_decomp")
    )
  )
  
)








# -------------------
# FITH TAB
# -------------------
tab5 <- tabItem(
  tabName = "tab5",
  titlePanel("Has anything changed?"),
  box(width = 12, htmlOutput("tab5_text"))
)






#---------------------------------------
# MAIN CONTENT
#---------------------------------------
mainScreen <- div(
  #div(id = "theme-selector-wrapper", shinythemes::themeSelector()),
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
    #tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
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
  title = "Love Trumps Hate | Business Stats Project",
  #skin = "purple",
  #skin = "black",
  header,
  sidebar,
  body
)




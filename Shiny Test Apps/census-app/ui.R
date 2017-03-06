# UI
#---------------

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(position = "left",
    sidebarPanel( 
      helpText("Create demographic maps with information fromm the 2010 US Census"),
      selectInput("ethnicitySelector",
                  label = h3("Choose a variable"),
                  choices = c("Percent White", "Percent Black",
                              "Percent Hispanic", "Percent Asian"),
                  selected = "Percent White"
                  ),
      sliderInput("rangeSelector", label = h3("Range of interest"),
                  min = 0, max = 100, value = c(0, 100)
                  )

      
      ),
    
    mainPanel(
      plotOutput("myMap")
    )
  )
))


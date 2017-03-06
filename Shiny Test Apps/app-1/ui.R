# UI
#---------------

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(#position = "right",
    sidebarPanel( 
      p("Create demographic maps with information fromm the 2010 US Census"),
      h3("Choose a variable to display"),
      selectInput("select",
                  label = h3("Choose a variable"),
                  choices = list("Percent White" = 1, "Percent Black" = 2, "Percent Hispanic" = 3, "Percent Asian" = 4)
                  ),
      sliderInput("s1", label = h3("Range of interest"),
                  min = 0, max = 100, value = c(0, 100)
                  )
      
      ),
    mainPanel(
      img(src="bigorb.png", height = 400, width = 400)
    )
  )
))
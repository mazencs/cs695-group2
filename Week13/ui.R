# This is the user-interface scriptn of your Shiny web application. 
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/
#

# Create a dashboard page

dashboardPage(
  dashboardHeader(title = "Row layout"),
  dashboardSidebar(),
  dashboardBody(

# Create text panel
  fluidRow(
    box(
      # width = 4, 
      background = "olive",
      "A text box with a solid olive background. This section can be used as the introdution to your project"
    )
  ),
  
# Create word cloud panel with two boxes, one for input and one for output 
  fluidRow(
    box(
      title = "Select number of words to show on the graph ",
      sliderInput("bins",
                  "Number of words:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    box(
      title = "Word cloud of Brooklyn Tech Triangle",     
      plotOutput("distPlot1")
    
    ) 
  ),

# Create sentiment analysis panel with two boxes, one for input and one for output   
  fluidRow(
    box(
      title = "Select number of bars to show on the graph ",
      sliderInput("bins2",
                  "Number of bars:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    box(
      title = "Sentiment of Brooklyn Tech Triangle",     
      plotOutput("distPlot2")
      
    ) 
  )  
)
)
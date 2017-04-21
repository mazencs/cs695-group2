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
      "CS695 Term Project: This section can be used as the introdution to your project"
    )
  ),
  
# Create word cloud panel with three boxes, one for input and one for output and one for discussion 
  fluidRow(
    
     box(
       width = 4,
        title = "Select number of words to show on the graph ",
      sliderInput("bins",
                  "Number of words:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    box(
      width = 4,
     title = "Word cloud of Brooklyn Tech Triangle",     
      plotOutput("distPlot1")
    
    ),
    
    box(
       width = 4, 
      background = "olive",
      "This text box is for your to discuss the results. 
      You are required to provide a deep discussion of the chart/graph. 
      What is the chart telling about the business?
      What insights can you discover from the results?
      What recommendation you can make to the company?"
    )
 
     ),

# Create sentiment analysis panel with two boxes, one for input and one for output and one for discussion    
  fluidRow(
    box(
      width = 4, 
      title = "Select number of bars to show on the graph ",
      sliderInput("bins2",
                  "Number of bars:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    box(
      width = 4, 
     title = "Sentiment of Brooklyn Tech Triangle",     
      plotOutput("distPlot2")
      
    ) ,
    box(
      width = 4, 
      background = "olive",
      "This text box is for your to discuss the results. 
      You are required to provide a deep discussion of the chart/graph. 
      What is the chart telling about the business?
      What insights can you discover from the results?
      What recommendation you can make to the company?"
    )
  ), 

# Create network graph panel with two boxes, one for output and one for discussion    
fluidRow(
  
  box(
    width = 8, 
      title = "Network Graph of Brooklyn Tech Triangle",     
   plotOutput("distPlot3")
    
  ) ,
  box(
    width = 4, 
    background = "olive",
    "This text box is for your to discuss the results. 
    You are required to provide a deep discussion of the chart/graph. 
    What is the chart telling about the business?
    What insights can you discover from the results?
    What recommendation you can make to the company?"
  )
  ), 

# Create customer profile panel with two boxes, one for output and one for discussion    
fluidRow(
  
  box(
    width = 8, 
    title = "user profile of Brooklyn Tech Triangle",     
    plotOutput("distPlot4")
    
  ) ,
  box(
    width = 4, 
    background = "olive",
    "This text box is for your to discuss the results. 
    You are required to provide a deep discussion of the chart/graph. 
    What is the chart telling about the business?
    What insights can you discover from the results?
    What recommendation you can make to the company?"
  )
), 

# Create topic analysis panel with two boxes, one for output and one for discussion    
fluidRow(
  
  box(
    width = 8, 
    title = "topic analysis of Brooklyn Tech Triangle",     
    plotOutput("distPlot5")
    
  ) ,
  box(
    width = 4, 
    background = "olive",
    "This text box is for your to discuss the results. 
    You are required to provide a deep discussion of the chart/graph. 
    What is the chart telling about the business?
    What insights can you discover from the results?
    What recommendation you can make to the company?"
  )
  ) 


)

)


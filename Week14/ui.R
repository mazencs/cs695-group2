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
      "CS695 Term Project:Merola Tile is a leading tile distributor on the East Coast. Located in Amityville, NY, this family owned and operated company has been importing quality tile products from around the world since 1988. The Merola Tile New Jersey Division, located in Manalapan, NJ, was created as a National Distributor for The Home Depot in 1999. Merola tile selections are sold online at homedepot.com and in hundreds of The Home Depot stores located across the United States. 
The goal of this term project is to use data visualization to identify trends and patterns that would otherwise be unclear or difficult to see in a tabular format.this data presentation offers the  executives at merola a new approaches to dramatically improve their ability to grasp information hiding in their data so that to make decision that will optimize their performance and increase profit."
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
     title = "Word cloud of merola tiles",     
      plotOutput("distPlot1")
    
    ),
    
    box(
       width = 4, 
      background = "olive",
      "Word clouds are a method for visually presenting text data. They are popular for text analysis because they make it easy to spot word frequencies. The more frequent the work is used, the larger and bolder it is displayed.
In case of merola company , we can notice that the top 4 text with high frequency are : merola tile ny , http , thanks , interior design . Given that merola has two locations new york and new jersey, the fact that new york location has the highest frequency indicates that most of the customers are located in new york .
      As for the HTTP word frequency , it indicates that the web address of the company has been mentioned with high frequency between customers ,therefore more attention should be paid to the design and the products on the website.
      Thanks and interior design also had high frequency ,therefore , more attention should be paid on the interior design products."
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
     title = "Sentiment of merola tile",     
      plotOutput("distPlot2")
      
    ) ,
    box(
      width = 4, 
      background = "olive",
      "The chart illustrates the public sentiment of merola tile , scores >0 denotes positive attitude , scores <0 denotes negative attitude toward the company and scores =0 denotes neutral attitude toward the company . From the graph we can see that the majority of the sentiments about the company are neutral followed by positive attitude toward the company (frequency = 24) , the negative attitude toward the company constitutes less than 4 frequencies.we can conclude from the chart that most of the public have not dealt with the company that why they have neutral attitude about it , therefore more effort could be done to attract more customer to try the company products."
    )
  ), 

# Create network graph panel with two boxes, one for output and one for discussion    
fluidRow(
  
  box(
    
    HTML('<center><img src="n.png" width="400"></center>')
  
    
  ) ,
  box(
    width = 4, 
    background = "olive",
    "still in progress"
  )
  ), 

# Create customer profile panel with two boxes, one for output and one for discussion    
fluidRow(
  
  box(
    width = 8, 
    title = "user profile of Merola tile",     
    plotOutput("distPlot4")
    
  ) ,
  box(
    width = 4, 
    background = "olive",
    "The graph indicates that most of the users didn’t specify their gender , nevertheless , male posts were more than female posts during the week.due to the shortage of enough data about the users we can’t generalize that men are more interested in the company’s products than women."
  )
), 

# Create topic analysis panel with two boxes, one for output and one for discussion    
fluidRow(
  
  box(
    width = 8, 
    title = "topic analysis of merola tile",     
    plotOutput("distPlot5")
    
  ) ,
  box(
    width = 4, 
    background = "olive",
    "the graph illustrates that 25% of the products listed in the dictionary file were mentioned"
  )
  ) 


)

)


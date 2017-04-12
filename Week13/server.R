#
# This is the server script of your Shiny web application. 
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

function (input, output) {
  
# *** Word Cloud Panel********  
  
  output$distPlot1 <- renderPlot({
    
    bins <- input$bins + 1
    wordcloud(head(dm$word, bins), head(dm$freq, bins), random.order=FALSE, colors=brewer.pal(8, "Dark2"))
    
  })
  
# **** Sentiment Analysis Panel *****  
  
  output$distPlot2 <- renderPlot({
    
    score= sentiment.scores$score
  
    x <- score
    bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  
}

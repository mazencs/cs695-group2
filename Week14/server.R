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
 
  # **** Network Graph Panel *****  
  
  output$distPlot3 <- renderPlot({
    
    set.seed(1)
    PlotGraph(m2, 
              colors = central$color,
              sizes = central$size,
              labels = central$label
    )
    
  })
  
  # ******* customer profile ************
  output$distPlot4 <- renderPlot({
    
   
    barplot(dfrm, main="Number of posts by Days and Gender",
            xlab="Days", col=c("darkblue","red"),
            legend = rownames(dfrm), beside=TRUE)
    
  })
  
  # ******* Topic Analysis ************
  output$distPlot5 <- renderPlot({
    
    # Pie Chart with Percentages
    slices <- c(Nmentioned,N-Nmentioned)
    lbls <- c("Mentioned", "Not Mentioned")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(slices,labels = lbls, col=rainbow(length(lbls)),
        main="Pie Chart of Tweets on Sports")
    
  })
  
}

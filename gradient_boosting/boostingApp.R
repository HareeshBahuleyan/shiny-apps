setwd("/home/hareesh/RShiny/apps")

library("shiny")
library("gbm")
library("ggplot2")

ui <- pageWithSidebar(
  headerPanel('Select Parameters For GBM'),
  sidebarPanel(
    sliderInput(inputId = "numTrees", label = "Number of decision trees", min = 1, max = 200, value = 10),
    selectInput(inputId = "bagFrac", label = "Sub-sample train data size for each tree", choices = list(0.5,0.6,0.7,0.8,0.9,"1.0" = 1.0)),
    sliderInput(inputId = "depth", label = "Depth to which each tree should be grown", min = 1, max = 5, value = 1),
    selectInput(inputId = "shrinkage", label = "Shrinkage parameter", choices = list(1,0.1,0.01,0.001))
  ),
  mainPanel(
    plotOutput(outputId = "predictionPlot")
  )
)



server <- function(input, output){
  
  output$predictionPlot <- renderPlot({
    # Creating the data
    set.seed(100)
    x = runif(100, min = 0, max = 7)
    x = sort(x, decreasing = F)
    df = data.frame(x = x,y = sin(x))
    
    # Fitting the model
    fit <- gbm(y~x, data=df, distribution="gaussian", n.trees = input$numTrees, shrinkage = as.numeric(input$shrinkage), interaction.depth = input$depth, bag.fraction = as.numeric(input$bagFrac))
    # Make predictions on the train data itself
    predictions <- predict(fit, df, n.trees = input$numTrees)
    df$pred = predictions
    
    # Plotting Actual vs Predicted
    ggplot(df, aes(x)) + 
      geom_line(aes(y = y, colour = "Actual"), size=1) + 
      geom_line(aes(y = pred, colour = "Predicted"), size=1) + 
      xlab("Input Variable (x)") + ylab("Output Variable (y)") +  
      theme(
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="maroon", size=14, face="bold"),
        axis.text.x= element_text(size=14),
        axis.text.y= element_text(size=14),
        legend.text = element_text(size = 16),
        legend.position = "right",
        legend.title = element_blank()
      )
    
  }, height = 500, width = 800)
  
}

shinyApp(ui = ui, server = server)





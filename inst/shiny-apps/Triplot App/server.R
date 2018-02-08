#' This is a shiny app for use in the elemSource package.

# Server logic ----
servertri <- function(input, output) {

  output$caption <- renderText("Triplot/Ternary")

  output$plotly1 <- renderPlotly({

    # Assign correct data frame for radio button selected
    if(input$source1 == 1) {plotDF <- myTempDF1010}
    if(input$source1 == 2) {plotDF <- rbind.data.frame(myTempDF1010[assigned,],
                                                       myTempDF1010[sources,])}
    if(input$source1 == 3) {plotDF <- rbind.data.frame(myTempDF1010[unAssigned,],
                                                       myTempDF1010[sources,])}

    plot_ly(data = plotDF,
                 a = plotDF[,as.numeric(input$elem1)],
                 b = plotDF[,as.numeric(input$elem2)],
                 c = plotDF[,as.numeric(input$elem3)],
                 color = ~get(input$label1),

                 type = 'scatterternary',
                 symbol = ~Type,
                 mode = 'markers',
                 text = ~paste("ANID: ",
                               ANID, '<br>Source:',
                               Source, '<br>Closest Source:',
                               Mahalanobis)) %>%
      layout(ternary = list(aaxis = list(title = names(plotDF)[as.numeric(input$elem1)]),
                            baxis = list(title = names(plotDF)[as.numeric(input$elem2)]),
                            caxis = list(title = names(plotDF)[as.numeric(input$elem3)]),
                            margin = list(t = 500,
                                          l = 500,
                                          r = 500,
                                          bottom = 500,
                                          pad = 500)))
  })
}


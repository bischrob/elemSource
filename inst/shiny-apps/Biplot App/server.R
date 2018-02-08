#' This is a shiny app for use in the elemSource package.

# Server logic ----
serverbi <- function(input, output) {

  output$caption <- renderText("Biplot")

  output$plotly1 <- renderPlotly({

    # Assign correct data frame for radio button selected
    if(input$source1 == 1) {plotDF <- .temp.dataset}
    if(input$source1 == 2) {plotDF <- rbind.data.frame(.temp.dataset[.temp.assigned,],
                                                       .temp.dataset[.temp.Source,])}
    if(input$source1 == 3) {plotDF <- rbind.data.frame(.temp.dataset[.temp.unassigned,],
                                                       .temp.dataset[.temp.Source,])}
    plotS <- plotDF[.temp.Source,]
    colS <- plotS[,as.character(input$label1)]
    shapeS <- plotS$Type
    plotA <- plotDF[.temp.Artifact,]
    colA <- plotA[,as.character(input$label1)]
    shapeA <- plotA$Type

    g <- ggplot() +
      geom_point(data = plotS,
                 aes(x = plotS[,as.numeric(input$elem1)],
                     y = plotS[,as.numeric(input$elem2)],
                     color = colS,
                     shape = shapeS,
                     text = plotS[,1])) +
      geom_point(data = plotA,
                 aes(x = plotA[,as.numeric(input$elem1)],
                     y = plotA[,as.numeric(input$elem2)],
                     color = colA,
                     shape = shapeA,
                     text = plotA[,1])) +
      xlab(names(plotDF)[as.numeric(input$elem1)]) +
      ylab(names(plotDF)[as.numeric(input$elem2)]) +
      theme_minimal() +
      theme(legend.title=element_blank()) +
      stat_ellipse(data = plotS,
                   aes(x = plotS[,as.numeric(input$elem1)],
                       y = plotS[,as.numeric(input$elem2)],
                       color = colS),
                   type = "norm",
                   level = .9,
                   lwd = .5) # this ellipse is based off the multivariate
                             # normal distribution
    ggplotly(g)
  })
}

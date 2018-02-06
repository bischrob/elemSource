#' This is a shiny app for use in the elemSource package.

# User interface ----
uitri <- fluidPage(
  titlePanel("Obsidian Sourcing Interactive Plots"),
      sidebarLayout(
        sidebarPanel(
          radioButtons("source1", label = "Data Source",
                       choices = list("All Data" = 1,
                                      "Assigned" = 2,
                                      "Unassigned" = 3),
                       selected = 1, inline = F),
          selectInput("label1", label = h5("Legend Label"),
                      choices = list( "Original" = "Source",
                                      "Discriminant" = "Discriminant",
                                      "Mahalanobis" = "Mahalanobis"),
                      selected = 12),
          selectInput("elem1", label = h5("Element 1"),
                      choices = list("Rb" = 7,
                                     "Sr" = 8,
                                     "Y" = 9,
                                     "Zr" = 10,
                                     "Nb" = 11),
                      selected = 7),
               selectInput("elem2", label = h5("Element 2"),
                           choices = list("Rb" = 7,
                                          "Sr" = 8,
                                          "Y" = 9,
                                          "Zr" = 10,
                                          "Nb" = 11),
                           selected = 8),
          selectInput("elem3", label = h5("Element 3"),
                      choices = list("Rb" = 7,
                                     "Sr" = 8,
                                     "Y" = 9,
                                     "Zr" = 10,
                                     "Nb" = 11),
                      selected = 9)
      ),
        mainPanel(
          h3(textOutput("caption")),
          hr(),
          plotlyOutput("plotly1", height = "800px", width = "950px")
        )
    )
)

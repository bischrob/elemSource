#' Run Biplot Shiny App
#'
#' Function to run Biplot shiny app
#'
#' @param None dataframe to source
#'
#' @return None
#'
#' @examples
#' runBiplotApp(df)
#'
#' @export
runBiplotApp <- function (df)
{ ... }

# Function
runBiplotApp <- function(df) {
  #load packages
  options(warn = -1)
  suppressMessages(library(shiny))
  suppressMessages(library(ggplot2))
  suppressMessages(library(plotly))
  options(warn = 0)

  # Create variables for subsetting
  myTempDF1010 <<- df
  assigned <<- which(df$Status == "assigned")
  unAssigned <<- which(df$Status == "unassigned")
  artifacts <<- which(df$Type == "Artifact")
  sources <<- c(which(df$Type == "Source"),which(df$Type == "Source Flake"))


  # find directory
  appDir <- system.file("shiny-apps", "Biplot App", package = "elemSource")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `elemSource`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
  on.exit(rm(myTempDF1010,assigned,unAssigned,artifacts,sources))
}

#' Run Triplot Shiny App
#'
#' Function to run Triplot shiny app
#'
#' @param None dataframe to source
#'
#' @return None
#'
#' @examples
#' runBiplotApp(df)
#'
#' @export
runBiplotApp <- function ()
{ ... }

# Function
runBiplotApp <- function() {
  #load packages
  options(warn = -1)
  suppressMessages(library(shiny))
  suppressMessages(library(ggplot2))
  suppressMessages(library(plotly))
  options(warn = 0)

  # Create variables for subsetting
  assigned <- which(df$Status == "assigned")
  unAssigned <- which(df$Status == "unassigned")
  artifacts <- which(df$Type == "Artifact")
  sources <- c(which(df$Type == "Source"),which(df$Type == "Source Flake"))


  # find directory
  appDir <- system.file("shiny-apps", "Biplot App", package = "elemSource")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `elemSource`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
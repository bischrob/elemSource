#' Run Triplot Shiny App
#'
#' Function to run Triplot shiny app
#'
#' @param None
#'
#' @return None
#'
#' @examples
#' runTriplotApp())
#'
#' @export
runTriplotApp <- function (df)
{ ... }

# Function
runTriplotApp <- function(df) {
  #load packages
  options(warn = -1)
  suppressMessages(library(shiny))
  suppressMessages(library(plotly))
  options(warn = 0)

  # Create variables for subsetting
  myTempDF1010 <<- df
  assigned <<- which(df$Status == "assigned")
  unAssigned <<- which(df$Status == "unassigned")
  artifacts <<- which(df$Type == "Artifact")
  sources <<- c(which(df$Type == "Source"),which(df$Type == "Source Flake"))


  # find directory
  appDir <- system.file("shiny-apps", "Triplot App", package = "elemSource")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `elemSource`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
  on.exit(rm(myTempDF1010,assigned,unAssigned,artifacts,sources))
}

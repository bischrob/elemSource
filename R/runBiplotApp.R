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
  .GlobalEnv$.temp.dataset <- df
  on.exit(rm(.temp.dataset, envir=.GlobalEnv))
  .GlobalEnv$.temp.assigned <- which(df$Status == "assigned")
  on.exit(rm(.temp.assigned, envir=.GlobalEnv), add = T)
  .GlobalEnv$.temp.unassigned <- which(df$Status == "unassigned")
  on.exit(rm(.temp.unassigned, envir=.GlobalEnv), add = T)
  .GlobalEnv$.temp.Artifact <- which(df$Type == "Artifact")
  on.exit(rm(.temp.Artifact, envir=.GlobalEnv), add = T)
  .GlobalEnv$.temp.Source <- c(which(df$Type == "Source"),which(df$Type == "Source Flake"))
  on.exit(rm(.temp.Source, envir=.GlobalEnv), add = T)


  # find directory
  appDir <- system.file("shiny-apps", "Biplot App", package = "elemSource")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `elemSource`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

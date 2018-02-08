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

  # Create variables for subsetting and remove after function
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
  appDir <- system.file("shiny-apps", "Triplot App", package = "elemSource")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `elemSource`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

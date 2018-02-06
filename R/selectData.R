#' Function to source data
#'
#' Runs function to source data
#'
#' @param df dataframe to source
#' @param saveResults logical-save results
#' @param prob = probability to accept sourcing
#'
#' @return None
#'
#' @examples
#' elemSource(df)
#'
#' @export
elemSource <- function(df, saveResults = T, prob = .9)
{ ... }

selectData <- function(){
  options(warn = -1)
  suppressMessages(library(rio))
  suppressMessages(library(svDialogs))
  options(warn = 0)
  # first have user select dataframes
  svDialogs::msgBox("Select up to two files to use in sourcing")
  files <- choose.files(caption = "Select Up to two files to use in sourcing")
  
  # load data
  df1 <- rio::import(files[1])
  names(df1)[1:13] <- c("ANID","Mn", "Fe", "Zn", "Ga", "Th", "Rb",
                        "Sr", "Y", "Zr", "Nb", "Source", "Type")
  if(length(files) > 1){
    # load data
    df2 <- import(files[2])
    names(df2)[1:13] <- c("ANID","Mn", "Fe", "Zn", "Ga", "Th", "Rb",
                          "Sr", "Y", "Zr", "Nb", "Source", "Type")
    # Choose which sources to keep
    svDialogs::msgBox("Choose which sources to use")
    list1 <- unique(df1$Source) # unique sources
    list2 <- unique(df2$Source) # unique sources
    grp1 <- dlgList(list1, multiple = TRUE)$res
    grp2 <- dlgList(list2, multiple = TRUE)$res
    
    # Subset and combine data
    df <- bind_rows(df1[which(df1$Source %in% grp1),],
                    df2[which(df2$Source %in% grp2),] ) 
  } else {
    # Choose which sources to keep
    svDialogs::msgBox("Choose which sources to use")
    list1 <- unique(df1$Source) # unique sources
    grp1 <- svDialogs::dlgList(list1, multiple = TRUE)$res
    
    #subset data
    df <- df1[which(df1$Source %in% grp1),]
  }
  if(! "Source" %in% df$Type){
    stop("There are no sources. Ensure data contains a column with the label 'Source'.")
  } 
  if(! "Artifact" %in% df$Type){
    stop("There are no artifacts. Ensure data contains a column with the label 'Artifact'.")
  } 
  return(df)
}

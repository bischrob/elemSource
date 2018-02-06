#' Create all Biplots
#'
#' Function to create all possible biplots
#'
#' @param df dataframe to source
#' @param groupName Name of column to group by
#' @param mColors optional list of colors to use in plot
#'
#' @return None
#'
#' @examples
#' scatterPlots(df)
#'
#' @export
scatterPlots <- function(df = "dataframe", groupName = NULL, mColors = NULL)
{ ... }

# Function
scatterPlots <- function(df = "dataframe", groupName = NULL, mColors = NULL){ 
  if(!is.data.frame(df)){
    stop("Please include a dataframe object.")  
  }
  if (missing("groupName")) {
    groupName <- "Source"
  } 
  if (missing("mColors")) {
    myColors <- readRDS("Data/Colors.Rds")
    mColors <-  myColors$Hex[1:length(unique(df$Source))]
  } 
  artifacts <- which(df$Type == "Artifact")
  sources <- c(which(df$Type == "Source"),which(df$Type == "Source Flake"))
  # load packages
  myPackages <- c("ggplot2", "grid","gridExtra")
  options(warn = -1)
  suppressMessages(library(ggplot2))
  suppressMessages(library(grid))
  suppressMessages(library(gridExtra))
  options(warn = 0)
  
  # get all plots into a list
  # run all plots
  mPlots <- list()  # new empty list
  k <<- 1
  for (i in 7:11)
    for (j in i:11)
      local({
        i <- i
        j <- j
        if(!identical(i,j)){
          (max(df[,j])-min(df[,j]))
          g <- ggplot() +
            geom_point(data = df[artifacts,], aes(x = df[artifacts,i],
                                                  y = df[artifacts,j],
                                                  color = df$Source[artifacts])) +
            xlab(names(df)[i]) +
            ylab(names(df)[j]) +
            theme_minimal() +
            theme(legend.title=element_blank()) +
            scale_color_manual(values = mColors) + # used for manual colors
            stat_ellipse(data = df[sources,], aes(x = df[sources,i],
                                                  y = df[sources,j],
                                                  color = df$Source[sources]),
                         type = "norm",
                         level = .9,
                         lwd = .5) # this ellipse is based off the multivariate normal distribution
          
          mPlots[[k]] <<- g  # add each plot into plot list
          k <<- k + 1
        }
      })
  
  
  # create a function to create one shared plot
  grid_arrange_shared_legend <- function(plots) {
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs 
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    grid.arrange(
      do.call(arrangeGrob, lapply(plots, function(x)
        x + theme(legend.position="none"))),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight))
  }
  g <- grid_arrange_shared_legend(mPlots)
  
  return(g)
}


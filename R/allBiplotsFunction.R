#' Save all Biplots
#'
#' Function to run and save all possible combinations of biplots
#'
#' @param df dataframe from the elemSource function
#' @param mColors optional list of colors to use in plot
#' @param showSources logical- display the sources along with the sourced artifacts
#' @param onlySources logical- only run the sources
#'
#' @return None
#'
#' @examples
#' allBiplots(df)
#'
#' @export
allBiplots <- function(df, mColors, showSources = F, onlySources = F)
  { ... }

# Create all possible combination of biplots
allBiplots <- function(df,mColors, showSources = F, onlySources = F){
  if(showSources == F & onlySources == T){
    stop("Cannot choose showSources = F and onlySources = T")
  }
  stopifnot(is.data.frame(df))
  options(warn = -1)
  suppressMessages(library(ggplot2))
  options(warn = 0)
  artifacts <- which(df$Type == "Artifact")
  sources <- c(which(df$Type == "Source"),which(df$Type == "Source Flake"))
  if(missing(mColors)){
    myColors <- readRDS(system.file('Colors', 'Colors.Rds', package='elemSource'))
    mColors <-  myColors$Hex[1:length(unique(df$Source))]
  }
  dName <- unlist(strsplit(as.character(Sys.time())," ")) # Gives directory today's date
  dName <- paste0("Figures/Plots--", dName[1])
  if (dir.exists(dName) == F) dir.create(dName)

  if(onlySources == F){
  # run all plots
    for(i in 7:11){
    for(j in i:11){
      if(!identical(i,j)){
        ratio.display <- 4/3
        ratio.values <- (max(df[,i])-min(df[,i]))/
            (max(df[,j])-min(df[,j]))
        g <- ggplot() +
          geom_point(data = df[artifacts,], aes(x = df[artifacts,i],
                                     y = df[artifacts,j],
                                     color = df$Source[artifacts])) +
          coord_fixed(ratio.values/ratio.display) +
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
        if(showSources == T){
          g <- g + geom_point(data = df[sources,], aes(x = df[sources,i],
                                      y = df[sources,j],
                                      color = df$Source[sources],
                                      shape = df$Type[sources]))
        }
         ggsave(filename = paste0(dName,"/",names(df)[i], "-",names(df)[j],".png"),
              dpi = 300, plot = g, width = 6.5, units = "in")
      }}}
} else {
  # run all plots
  for(i in 7:11){
    for(j in i:11){
      if(!identical(i,j)){
        ratio.display <- 4/3
        ratio.values <- (max(df[,i])-min(df[,i]))/
          (max(df[,j])-min(df[,j]))
        g <- ggplot() +
          geom_point(data = df[sources,], aes(x = df[sources,i],
                                                       y = df[sources,j],
                                                       color = df$Source[sources],
                                                       shape = df$Type[sources])) +
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
        ggsave(filename = paste0(dName,"/OnlySources",names(df)[i], "-",names(df)[j],".png"),
               dpi = 300, plot = g, width = 6.5, units = "in")
      }
    }
  }
}
}

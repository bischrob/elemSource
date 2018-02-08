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

#############################################################################################
elemSource <- function(df, saveResults = T, prob = .9){
  stopifnot(is.data.frame(df))
  suppressMessages(library(MASS))
  suppressMessages(library(svDialogs))
  suppressMessages(library(rJava))
  suppressMessages(library(xlsx))
    # Create key for rows that are either artifacts or sources
  artifacts <- which(df$Type == "Artifact")
  sources <- c(which(df$Type == "Source"),which(df$Type == "Source Flake"))

  # log and clean values
  # all values below zero or infinite will be removed
  notlogged <- df[,2:11]
  mLog <- df[,2:11]
  mLog[get("mLog") < 0] <- 0
  mLog <- log(mLog)
  mLog <- apply(mLog,2, function(x) replace(x, is.infinite(x),0)) # Replace infinite values

  df[,2:11] <- mLog

#############################################################################################
  # Discriminant Analysis
  # Run analysis on sources
  fitLDA <- MASS::lda(df$Source[sources] ~ ., data = df[sources,7:11],
           prior = rep(1,length(unique(df$Source[sources])))/
                         length(unique(df$Source[sources])))

  # Assess the accuracy of the prediction
  testLDA <- predict(fitLDA, newdata = df[sources,7:11])
  ct <- table(as.character(df$Source[sources]), as.character(testLDA$class))
  testAccuracy <- diag(prop.table(ct, 1))
  testAccuracyTotal <-sum(diag(prop.table(ct)))
  print("Discriminant source accuracy")
  print(testAccuracyTotal)

  # Predict source for artifacts and assign to  dataframe
  predictLDA <- predict(fitLDA, newdata = df[,7:11])
  df$Discriminant <- as.character(predictLDA$class)
  df$Posterior <- round(apply(predictLDA$posterior, 1, function(x) max(x)),3)
  LDAPosterior <- cbind.data.frame(df[,1],df$Source,predictLDA$posterior)
  names(LDAPosterior)[1:2] <- c("ANID", "Source")

#############################################################################################
  # Mahalanobis Distance
  # Calculate Mahalanobis distance from each source sample to each source group.
  mGroups <- unique(sort(df$Source[sources]))

  # Source mean and covariance
  sourcesMdist <- NULL
  for(i in 1:length(mGroups)){
    sMean <- colMeans(df[df$Source == mGroups[i],7:11])
    sCov <- cov(df[df$Source == mGroups[i],7:11])
    mDist <- mahalanobis(df[sources,7:11],sMean,sCov, tol = 1e-20)
    sourcesMdist <- cbind(sourcesMdist,mDist)
    colnames(sourcesMdist)[i] <- mGroups[i]
  }

  # assess accuracy of sourced data
  sourcesMdistMin <- cbind.data.frame(df$Source[sources],
                                    colnames(sourcesMdist)[apply(sourcesMdist,1,which.min)],
                                    round(apply(sourcesMdist, 1, function(x) min(x)),3),
                                    sourcesMdist)
  colnames(sourcesMdistMin)[1:3] <- c("OriginalSource","ClosestSource", "MinimumDistance")
  mDistAccuracy <- sum(sourcesMdistMin$ClosestSource %in% sourcesMdistMin$OriginalSource)/
                  nrow(sourcesMdistMin)
  print("Mahalanobis source accuracy")
  print(mDistAccuracy) # should = 1

  # Determine source for all
  # Source mean and covariance
  allMdist <- NULL
  for(i in 1:length(mGroups)){
    sMean <- colMeans(df[df$Source == mGroups[i],7:11])
    sCov <- cov(df[df$Source == mGroups[i],7:11])
    mDist <- mahalanobis(df[,7:11],sMean,sCov, tol = 1e-20)
    allMdist <- cbind(allMdist,mDist)
    colnames(allMdist)[i] <- mGroups[i]
  }

  # Obtain chi-squared probabilities
  mProbs <- NULL
  for (i in 1:ncol(allMdist)){
    p <- 1 - pchisq(allMdist[,i],length(allMdist[,i]))
    mProbs <- cbind(mProbs,p)
    colnames(mProbs)[i] <- colnames(allMdist)[i]
  }

# combine the mahalanobis distances with the probabilities
  allMdistC <- cbind(allMdist,mProbs)
  allMdistC <- allMdistC[,order(colnames(allMdistC))]
  allMdistC <- cbind.data.frame(df$ANID, df$Source, allMdistC)
  names(allMdistC)[1:2] <- c("ANID", "Source")

  sNames <- colnames(allMdist) [apply(allMdist,1,which.min)]
  minDist <- round(apply(allMdist, 1, function(x) min(x)),3)
  maxProb <- round(apply(mProbs, 1, function(x) max(x)),3)

# Assign Mahalanobis info to original dataframe
  df$Mahalanobis <- sNames
  df$MahalDistance <- minDist
  df$MahalProb <- maxProb

#############################################################################################
# Determines sources that are confidently sourced
# artifacts.

# Sourced
  assigned <- which(df$MahalProb[artifacts] >= prob
                & df$Posterior[artifacts] >= prob & df$Mahalanobis[artifacts] == df$Discriminant[artifacts])
# Unsourced
  unAssigned <- setdiff(artifacts,assigned)
  df$Status <- NA
  df$Status[assigned] <- "assigned"
  df$Status[unAssigned] <- "unassigned"
  df[,2:11] <- notlogged

################################################################################
# Save results
  if(saveResults == T){
    svDialogs::msgBox("Select name and directory to save results (as an xlsx file)")
    fileName <- svDialogs::dlgSave()$res
    xlsx::write.xlsx(df,fileName,
             sheetName = "Results",
             row.names = F)
    xlsx::write.xlsx(LDAPosterior[artifacts,],fileName,
             sheetName = "LDA Posterior Probabilities", row.names = F,
             append = T)
    xlsx::write.xlsx(allMdistC[artifacts,],fileName,
             sheetName = "Mahalanobis distances", row.names = F,
             append = T)
 }

################################################################################
  # Return results
  df[,2:11] <- mLog
  return(df)
  }


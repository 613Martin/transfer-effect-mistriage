#' CalculateStats
#' 
#' Uses the results.data.frames to calculate median and IQR for each entry in all samples.
#' Outputs a collapsed data set with this data.
#' @param results.data.frames List of data.frames containing results.
CalculateStats <- function(results.data.frames) {
  
  ## Setup function for median and IQR
  MedianIQR <- function(num.vector) {
    medi <- median(num.vector)
    medi <- round(medi, digits = 2)
    qu <- quantile(num.vector)
    iqr <- paste(round(qu[2], digits = 2) , round(qu[4], digits = 2), sep = "-")
    mediqr <- paste(medi,"[",iqr,"]")
    return(mediqr)
  }
  ## Calculate median and IQR
  stats <- lapply(results.data.frames, function(sample) lapply(sample, function(x) {
    ## Setup sample name place holder and calculate per row
    Sample.name <- as.character("xy")
    Total.imputations <- max(x[[1]])
    Validation.mistriage.median.IQR <- MedianIQR(x[[2]])
    Validation.undertriage.median.IQR <- MedianIQR(x[[3]])
    Validation.overtriage.median.IQR <- MedianIQR(x[[4]])
    Transfer.mistriage.median.IQR <- MedianIQR(x[[5]])
    Transfer.undertriage.median.IQR <- MedianIQR(x[[6]])
    Transfer.overtriage.median.IQR <- MedianIQR(x[[7]])
    Transferred.mistriage.minus.buddy.local.mistriage.median.IQR <-  MedianIQR(x[[8]])
    Transferred.undertriage.minus.buddy.local.undertriage.median.IQR <-  MedianIQR(x[[9]])
    Transferred.overtriage.minus.buddy.local.overtriage.median.IQR <-  MedianIQR(x[[10]])
    ## Create data frame output
    x <- data.frame(Sample.name,
                    Total.imputations,
                    Validation.mistriage.median.IQR,
                    Validation.undertriage.median.IQR,
                    Validation.overtriage.median.IQR,
                    Transfer.mistriage.median.IQR,
                    Transfer.undertriage.median.IQR,
                    Transfer.overtriage.median.IQR,
                    Transferred.mistriage.minus.buddy.local.mistriage.median.IQR,
                    Transferred.undertriage.minus.buddy.local.undertriage.median.IQR,
                    Transferred.overtriage.minus.buddy.local.overtriage.median.IQR)
    return(x)
  }))
  ## Input correct names into previous place holder
  stats[[1]][[1]][[1]] <- "High volume"
  stats[[1]][[2]][[1]] <- "Low volume"
  stats[[2]][[1]][[1]] <- "Metropolitan"
  stats[[2]][[2]][[1]] <- "Non-metropolitan"
  stats[[3]][[1]][[1]] <- "Multi centre"
  stats[[3]][[2]][[1]] <- "Single centre"
  ## Collapse all sample data frames into one data frame
  final.stats <- rbind(stats[[1]][[1]],
                       stats[[1]][[2]],
                       stats[[2]][[1]],
                       stats[[2]][[2]],
                       stats[[3]][[1]],
                       stats[[3]][[2]])
  ## Return output
  return(final.stats)  
}

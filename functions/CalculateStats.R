#' CalculateStats
#' 
#' Uses the results.data.frames to calculate median and IQR for each entry in all samples.
#' Outputs a collapsed data set with this data.
#' @param list.of.results List of data.frames containing results.
CalculateStats <- function(list.of.results) {
  
  ## Error handling
  
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
  stats <- lapply(list.of.results, function(sample) lapply(sample, function(x) {
    sample.name <- as.character("xy")
    total.imputations <- max(x[[1]])
    local.mistriage.median.IQR <- MedianIQR(x[[2]])
    transfer.mistriage.median.IQR <- MedianIQR(x[[3]])
    transfer.performance.minus.local.performance.median.IQR <-  MedianIQR(x[[4]])
    transferred.performance.minus.buddy.local.performance.median.IQR <- MedianIQR(x[[5]])
    x <- data.frame(sample.name, 
                    total.imputations, 
                    local.mistriage.median.IQR, 
                    transfer.mistriage.median.IQR, 
                    transfer.performance.minus.local.performance.median.IQR, 
                    transferred.performance.minus.buddy.local.performance.median.IQR)
    return(x)
  }))
  
  ## Fix names
  stats[[1]][[1]][[1]] <- "High volume"
  stats[[1]][[2]][[1]] <- "Low volume"
  stats[[2]][[1]][[1]] <- "Metropolitan"
  stats[[2]][[2]][[1]] <- "Non-metropolitan"
  stats[[3]][[1]][[1]] <- "Multi centre"
  stats[[3]][[2]][[1]] <- "Single centre"
  
  ## Collapse data frames
  final.stats <- rbind(stats[[1]][[1]],
                stats[[1]][[2]],
                stats[[2]][[1]],
                stats[[2]][[2]],
                stats[[3]][[1]],
                stats[[3]][[2]])
  
  ## Return output
  return(final.stats)  
}

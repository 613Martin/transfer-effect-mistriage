#' CalculateStats
#' 
#' Uses the results.data.frames to calculate median and IQR for each entry in all samples.
#' Outputs a collapsed data set with this data.
#' @param results.data.frames List of data.frames containing results.
CalculateStats <- function(results.data.frames) {   

    ## Setup function for median and IQR
    MedianIQR <- function(num.vector) {
        medi <- median(num.vector)
        qu <- quantile(num.vector)
        mediqr <- setNames(round(c(medi, qu[2], qu[4]), digits = 2), c("median", "lb", "ub"))
        return(mediqr)
    }
    ## Calculate median and IQR
    stats <- lapply(results.data.frames, function(sample) lapply(sample, function(x) {
        ## Setup sample name place holder and calculate per row
        output <- list(
            Total.imputations = max(x[[1]]),
            Validation.mistriage.median = MedianIQR(x[[2]])["median"],
            Validation.mistriage.lb = MedianIQR(x[[2]])["lb"],
            Validation.mistriage.ub = MedianIQR(x[[2]])["ub"],
            Validation.undertriage.median = MedianIQR(x[[3]])["median"],
            Validation.undertriage.lb = MedianIQR(x[[3]])["lb"],
            Validation.undertriage.ub = MedianIQR(x[[3]])["ub"],
            Validation.overtriage.median = MedianIQR(x[[4]])["median"],
            Validation.overtriage.lb = MedianIQR(x[[4]])["lb"],
            Validation.overtriage.ub = MedianIQR(x[[4]])["ub"],
            Transfer.mistriage.median = MedianIQR(x[[5]])["median"],
            Transfer.mistriage.lb = MedianIQR(x[[5]])["lb"],
            Transfer.mistriage.ub = MedianIQR(x[[5]])["ub"],                
            Transfer.undertriage.median = MedianIQR(x[[6]])["median"],
            Transfer.undertriage.lb = MedianIQR(x[[6]])["lb"],
            Transfer.undertriage.ub = MedianIQR(x[[6]])["ub"],
            Transfer.overtriage.median = MedianIQR(x[[7]])["median"],
            Transfer.overtriage.lb = MedianIQR(x[[7]])["lb"],
            Transfer.overtriage.ub = MedianIQR(x[[7]])["ub"],       
            Transferred.mistriage.minus.buddy.local.mistriage.median =  MedianIQR(x[[8]])["median"],
            Transferred.mistriage.minus.buddy.local.mistriage.lb =  MedianIQR(x[[8]])["lb"],
            Transferred.mistriage.minus.buddy.local.mistriage.ub =  MedianIQR(x[[8]])["ub"],       
            Transferred.undertriage.minus.buddy.local.undertriage.median =  MedianIQR(x[[9]])["median"],
            Transferred.undertriage.minus.buddy.local.undertriage.lb =  MedianIQR(x[[9]])["lb"],
            Transferred.undertriage.minus.buddy.local.undertriage.ub =  MedianIQR(x[[9]])["ub"],        
            Transferred.overtriage.minus.buddy.local.overtriage.median =  MedianIQR(x[[10]])["median"],
            Transferred.overtriage.minus.buddy.local.overtriage.lb =  MedianIQR(x[[10]])["lb"],
            Transferred.overtriage.minus.buddy.local.overtriage.ub =  MedianIQR(x[[10]])["ub"]
        )
        output <- lapply(output, function(item) {
            attr(item, "names") <- NULL
            return(item)
        })
        return(unlist(output))
    }))
    ## Collapse all sample data frames into one data frame
    final.stats <- data.frame((do.call(rbind, lapply(stats, function(stat) do.call(rbind, stat)))))
    final.stats <- cbind(rownames(final.stats), final.stats)
    colnames(final.stats)[1] <- "Sample.name"
    rownames(final.stats) <- NULL
    ## Return output
    return(final.stats)  

}

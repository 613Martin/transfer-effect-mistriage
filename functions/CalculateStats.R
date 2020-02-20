#' CalculateStats
#' 
#' Uses the results.data.frames to calculate median and IQR for each entry in all samples.
#' Outputs a collapsed data set with this data.
#' @param results.data.frames List of data.frames containing results.
CalculateStats <- function(results.data.frames) {   

    ## Setup function for median and IQR
    MeanMedianIQR <- function(num.vector) {
        mean <- mean(num.vector)
        medi <- median(num.vector)
        qu <- quantile(num.vector)
        mediqr <- setNames(round(c(mean, medi, qu[2], qu[4]), digits = 2), c("mean", "median", "lb", "ub"))
        return(mediqr)
    }
    ## Calculate median and IQR
    stats <- lapply(results.data.frames, function(sample) lapply(sample, function(x) {
        ## Setup sample name place holder and calculate per row
        output <- list(
            Total.imputations = max(x[[1]]),
            Validation.mistriage.mean = MeanMedianIQR(x[[2]])["mean"],
            Validation.mistriage.median = MeanMedianIQR(x[[2]])["median"],
            Validation.mistriage.lb = MeanMedianIQR(x[[2]])["lb"],
            Validation.mistriage.ub = MeanMedianIQR(x[[2]])["ub"],
            Validation.undertriage.mean = MeanMedianIQR(x[[3]])["mean"],
            Validation.undertriage.median = MeanMedianIQR(x[[3]])["median"],
            Validation.undertriage.lb = MeanMedianIQR(x[[3]])["lb"],
            Validation.undertriage.ub = MeanMedianIQR(x[[3]])["ub"],
            Validation.overtriage.mean = MeanMedianIQR(x[[4]])["mean"],
            Validation.overtriage.median = MeanMedianIQR(x[[4]])["median"],
            Validation.overtriage.lb = MeanMedianIQR(x[[4]])["lb"],
            Validation.overtriage.ub = MeanMedianIQR(x[[4]])["ub"],
            Transfer.mistriage.mean = MeanMedianIQR(x[[5]])["mean"],            
            Transfer.mistriage.median = MeanMedianIQR(x[[5]])["median"],
            Transfer.mistriage.lb = MeanMedianIQR(x[[5]])["lb"],
            Transfer.mistriage.ub = MeanMedianIQR(x[[5]])["ub"],
            Transfer.undertriage.mean = MeanMedianIQR(x[[6]])["mean"],            
            Transfer.undertriage.median = MeanMedianIQR(x[[6]])["median"],
            Transfer.undertriage.lb = MeanMedianIQR(x[[6]])["lb"],
            Transfer.undertriage.ub = MeanMedianIQR(x[[6]])["ub"],
            Transfer.overtriage.mean = MeanMedianIQR(x[[7]])["mean"],            
            Transfer.overtriage.median = MeanMedianIQR(x[[7]])["median"],
            Transfer.overtriage.lb = MeanMedianIQR(x[[7]])["lb"],
            Transfer.overtriage.ub = MeanMedianIQR(x[[7]])["ub"],
            Transferred.mistriage.minus.buddy.local.mistriage.mean =  MeanMedianIQR(x[[8]])["mean"],            
            Transferred.mistriage.minus.buddy.local.mistriage.median =  MeanMedianIQR(x[[8]])["median"],
            Transferred.mistriage.minus.buddy.local.mistriage.lb =  MeanMedianIQR(x[[8]])["lb"],
            Transferred.mistriage.minus.buddy.local.mistriage.ub =  MeanMedianIQR(x[[8]])["ub"],
            Transferred.undertriage.minus.buddy.local.undertriage.mean =  MeanMedianIQR(x[[9]])["mean"],            
            Transferred.undertriage.minus.buddy.local.undertriage.median =  MeanMedianIQR(x[[9]])["median"],
            Transferred.undertriage.minus.buddy.local.undertriage.lb =  MeanMedianIQR(x[[9]])["lb"],
            Transferred.undertriage.minus.buddy.local.undertriage.ub =  MeanMedianIQR(x[[9]])["ub"],        
            Transferred.overtriage.minus.buddy.local.overtriage.mean =  MeanMedianIQR(x[[10]])["mean"],
            Transferred.overtriage.minus.buddy.local.overtriage.median =  MeanMedianIQR(x[[10]])["median"],
            Transferred.overtriage.minus.buddy.local.overtriage.lb =  MeanMedianIQR(x[[10]])["lb"],
            Transferred.overtriage.minus.buddy.local.overtriage.ub =  MeanMedianIQR(x[[10]])["ub"]
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

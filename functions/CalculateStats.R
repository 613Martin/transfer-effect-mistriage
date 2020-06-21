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
            Transferred.overtriage.minus.buddy.local.overtriage.ub =  MeanMedianIQR(x[[10]])["ub"],
            
            Validation.sensitivity.mean = MeanMedianIQR(x[[11]])["mean"],
            Validation.sensitivity.median = MeanMedianIQR(x[[11]])["median"],
            Validation.sensitivity.lb = MeanMedianIQR(x[[11]])["lb"],
            Validation.sensitivity.ub = MeanMedianIQR(x[[11]])["ub"],
            Validation.specificity.mean = MeanMedianIQR(x[[12]])["mean"],
            Validation.specificity.median = MeanMedianIQR(x[[12]])["median"],
            Validation.specificity.lb = MeanMedianIQR(x[[12]])["lb"],
            Validation.specificity.ub = MeanMedianIQR(x[[12]])["ub"],
            Validation.PPV.mean = MeanMedianIQR(x[[13]])["mean"],
            Validation.PPV.median = MeanMedianIQR(x[[13]])["median"],
            Validation.PPV.lb = MeanMedianIQR(x[[13]])["lb"],
            Validation.PPV.ub = MeanMedianIQR(x[[13]])["ub"],
            Validation.NPV.mean = MeanMedianIQR(x[[14]])["mean"],
            Validation.NPV.median = MeanMedianIQR(x[[14]])["median"],
            Validation.NPV.lb = MeanMedianIQR(x[[14]])["lb"],
            Validation.NPV.ub = MeanMedianIQR(x[[14]])["ub"],
            Validation.AUC.mean = MeanMedianIQR(x[[15]])["mean"],
            Validation.AUC.median = MeanMedianIQR(x[[15]])["median"],
            Validation.AUC.lb = MeanMedianIQR(x[[15]])["lb"],
            Validation.AUC.ub = MeanMedianIQR(x[[15]])["ub"],
            Validation.calibration.intercept.mean = MeanMedianIQR(x[[16]])["mean"],
            Validation.calibration.intercept.median = MeanMedianIQR(x[[16]])["median"],
            Validation.calibration.intercept.lb = MeanMedianIQR(x[[16]])["lb"],
            Validation.calibration.intercept.ub = MeanMedianIQR(x[[16]])["ub"],
            Validation.calibration.slope.mean = MeanMedianIQR(x[[17]])["mean"],
            Validation.calibration.slope.median = MeanMedianIQR(x[[17]])["median"],
            Validation.calibration.slope.lb = MeanMedianIQR(x[[17]])["lb"],
            Validation.calibration.slope.ub = MeanMedianIQR(x[[17]])["ub"],            
            Transfer.sensitivity.mean = MeanMedianIQR(x[[18]])["mean"],            
            Transfer.sensitivity.median = MeanMedianIQR(x[[18]])["median"],
            Transfer.sensitivity.lb = MeanMedianIQR(x[[18]])["lb"],
            Transfer.sensitivity.ub = MeanMedianIQR(x[[18]])["ub"],
            Transfer.specificity.mean = MeanMedianIQR(x[[19]])["mean"],            
            Transfer.specificity.median = MeanMedianIQR(x[[19]])["median"],
            Transfer.specificity.lb = MeanMedianIQR(x[[19]])["lb"],
            Transfer.specificity.ub = MeanMedianIQR(x[[19]])["ub"],
            Transfer.PPV.mean = MeanMedianIQR(x[[20]])["mean"],
            Transfer.PPV.median = MeanMedianIQR(x[[20]])["median"],
            Transfer.PPV.lb = MeanMedianIQR(x[[20]])["lb"],
            Transfer.PPV.ub = MeanMedianIQR(x[[20]])["ub"],
            Transfer.NPV.mean = MeanMedianIQR(x[[21]])["mean"],            
            Transfer.NPV.median = MeanMedianIQR(x[[21]])["median"],
            Transfer.NPV.lb = MeanMedianIQR(x[[21]])["lb"],
            Transfer.NPV.ub = MeanMedianIQR(x[[21]])["ub"],
            Transfer.AUC.mean = MeanMedianIQR(x[[22]])["mean"],            
            Transfer.AUC.median = MeanMedianIQR(x[[22]])["median"],
            Transfer.AUC.lb = MeanMedianIQR(x[[22]])["lb"],
            Transfer.AUC.ub = MeanMedianIQR(x[[22]])["ub"],
            Transfer.calibration.intercept.mean = MeanMedianIQR(x[[23]])["mean"],            
            Transfer.calibration.intercept.median = MeanMedianIQR(x[[23]])["median"],
            Transfer.calibration.intercept.lb = MeanMedianIQR(x[[23]])["lb"],
            Transfer.calibration.intercept.ub = MeanMedianIQR(x[[23]])["ub"],            
            Transfer.calibration.slope.mean = MeanMedianIQR(x[[24]])["mean"],            
            Transfer.calibration.slope.median = MeanMedianIQR(x[[24]])["median"],
            Transfer.calibration.slope.lb = MeanMedianIQR(x[[24]])["lb"],
            Transfer.calibration.slope.ub = MeanMedianIQR(x[[24]])["ub"],                        
            Transferred.sensitivity.minus.buddy.local.sensitivity.mean =  MeanMedianIQR(x[[25]])["mean"],            
            Transferred.sensitivity.minus.buddy.local.sensitivity.median =  MeanMedianIQR(x[[25]])["median"],
            Transferred.sensitivity.minus.buddy.local.sensitivity.lb =  MeanMedianIQR(x[[25]])["lb"],
            Transferred.sensitivity.minus.buddy.local.sensitivity.ub =  MeanMedianIQR(x[[25]])["ub"],
            Transferred.specificity.minus.buddy.local.specificity.mean =  MeanMedianIQR(x[[26]])["mean"],            
            Transferred.specificity.minus.buddy.local.specificity.median =  MeanMedianIQR(x[[26]])["median"],
            Transferred.specificity.minus.buddy.local.specificity.lb =  MeanMedianIQR(x[[26]])["lb"],
            Transferred.specificity.minus.buddy.local.specificity.ub =  MeanMedianIQR(x[[26]])["ub"],
            Transferred.PPV.minus.buddy.local.PPV.mean =  MeanMedianIQR(x[[27]])["mean"],
            Transferred.PPV.minus.buddy.local.PPV.median =  MeanMedianIQR(x[[27]])["median"],
            Transferred.PPV.minus.buddy.local.PPV.lb =  MeanMedianIQR(x[[27]])["lb"],
            Transferred.PPV.minus.buddy.local.PPV.ub =  MeanMedianIQR(x[[27]])["ub"],
            Transferred.NPV.minus.buddy.local.NPV.mean =  MeanMedianIQR(x[[28]])["mean"],            
            Transferred.NPV.minus.buddy.local.NPV.median =  MeanMedianIQR(x[[28]])["median"],
            Transferred.NPV.minus.buddy.local.NPV.lb =  MeanMedianIQR(x[[28]])["lb"],
            Transferred.NPV.minus.buddy.local.NPV.ub =  MeanMedianIQR(x[[28]])["ub"],
            Transferred.AUC.minus.buddy.local.AUC.mean =  MeanMedianIQR(x[[29]])["mean"],            
            Transferred.AUC.minus.buddy.local.AUC.median =  MeanMedianIQR(x[[29]])["median"],
            Transferred.AUC.minus.buddy.local.AUC.lb =  MeanMedianIQR(x[[29]])["lb"],
            Transferred.AUC.minus.buddy.local.AUC.ub =  MeanMedianIQR(x[[29]])["ub"],
            Transferred.calibration.intercept.minus.buddy.local.calibration.intercept.mean =  MeanMedianIQR(x[[30]])["mean"],            
            Transferred.calibration.intercept.minus.buddy.local.calibration.intercept.median =  MeanMedianIQR(x[[30]])["median"],
            Transferred.calibration.intercept.minus.buddy.local.calibration.intercept.lb =  MeanMedianIQR(x[[30]])["lb"],
            Transferred.calibration.intercept.minus.buddy.local.calibration.intercept.ub =  MeanMedianIQR(x[[30]])["ub"],
            Transferred.calibration.slope.minus.buddy.local.calibration.slope.mean =  MeanMedianIQR(x[[31]])["mean"],            
            Transferred.calibration.slope.minus.buddy.local.calibration.slope.median =  MeanMedianIQR(x[[31]])["median"],
            Transferred.calibration.slope.minus.buddy.local.calibration.slope.lb =  MeanMedianIQR(x[[31]])["lb"],
            Transferred.calibration.slope.minus.buddy.local.calibration.slope.ub =  MeanMedianIQR(x[[31]])["ub"]                        
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

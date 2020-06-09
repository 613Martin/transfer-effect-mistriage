#' ResultsCompiler
#'
#' Takes the list of values and calculates and prepares results,
#' @param combined.split.datasets List with combination of required values.
ResultsCompiler <- function(combined.split.datasets){

    ## Convert all imutations to data frames
    combined.split.datasets.data.frames <- lapply(combined.split.datasets, function(sample) lapply(sample, function(imp) (lapply(imp, function(x) {
        x <- data.frame(x)
    }))))
    ## Combine all data frames in a sample using rbind
    sample.datasets <- lapply(combined.split.datasets.data.frames, function(sample) lapply(sample, function(df) {
        df1 <- do.call(rbind, df)
        return(df1)
    }))
    ## Calculate difference, local performance vs transferred model, transferred
    ## model performance minus local model performance
    sample.datasets.with.comparison <- lapply(sample.datasets, function(x) {
        ## Top sample (i.e. High volume, or metropolitan)
        x[[1]]$Transferred.mistriage.minus.buddy.local.mistriage <- x[[1]][["Transfer.mistriage"]] - x[[2]][["Validation.mistriage"]]
        x[[1]]$Transferred.undertriage.minus.buddy.local.undertriage <- x[[1]][["Transfer.undertriage"]] - x[[2]][["Validation.undertriage"]]
        x[[1]]$Transferred.overtriage.minus.buddy.local.overtriage <- x[[1]][["Transfer.overtriage"]] - x[[2]][["Validation.overtriage"]]
        x[[1]]$Transferred.sensitivity.minus.buddy.local.sensitivity <- x[[1]][["Transfer.sensitivity"]] - x[[2]][["Validation.sensitivity"]]
        x[[1]]$Transferred.specificity.minus.buddy.local.specificity <- x[[1]][["Transfer.specificity"]] - x[[2]][["Validation.specificity"]]
        x[[1]]$Transferred.PPV.minus.buddy.local.PPV <- x[[1]][["Transfer.PPV"]] - x[[2]][["Validation.PPV"]]
        x[[1]]$Transferred.NPV.minus.buddy.local.NPV <- x[[1]][["Transfer.NPV"]] - x[[2]][["Validation.NPV"]]
        
        ## Bottom sample (i.e. Low volume, or non-metropolitan)
        x[[2]]$Transferred.mistriage.minus.buddy.local.mistriage <- x[[2]][["Transfer.mistriage"]] - x[[1]][["Validation.mistriage"]]
        x[[2]]$Transferred.undertriage.minus.buddy.local.undertriage <- x[[2]][["Transfer.undertriage"]] - x[[1]][["Validation.undertriage"]]
        x[[2]]$Transferred.overtriage.minus.buddy.local.overtriage <- x[[2]][["Transfer.overtriage"]] - x[[1]][["Validation.overtriage"]]
        x[[2]]$Transferred.sensitivity.minus.buddy.local.sensitivity <- x[[2]][["Transfer.sensitivity"]] - x[[1]][["Validation.sensitivity"]]
        x[[2]]$Transferred.specificity.minus.buddy.local.specificity <- x[[2]][["Transfer.specificity"]] - x[[1]][["Validation.specificity"]]
        x[[2]]$Transferred.PPV.minus.buddy.local.PPV <- x[[2]][["Transfer.PPV"]] - x[[1]][["Validation.PPV"]]
        x[[2]]$Transferred.NPV.minus.buddy.local.NPV <- x[[2]][["Transfer.NPV"]] - x[[1]][["Validation.NPV"]]
        return(x)
    })
    
    ## Rearrange colums in each imputation data frame
    col_order <- c("Imputation", 
                   "Validation.mistriage", 
                   "Validation.undertriage", 
                   "Validation.overtriage", 
                   "Transfer.mistriage", 
                   "Transfer.undertriage", 
                   "Transfer.overtriage", 
                   "Transferred.mistriage.minus.buddy.local.mistriage",
                   "Transferred.undertriage.minus.buddy.local.undertriage",
                   "Transferred.overtriage.minus.buddy.local.overtriage",
                   "Validation.sensitivity",
                   "Validation.specificity",
                   "Validation.PPV",
                   "Validation.NPV",
                   "Transfer.sensitivity",
                   "Transfer.specificity",
                   "Transfer.PPV",
                   "Transfer.NPV",
                   "Transferred.sensitivity.minus.buddy.local.sensitivity",
                   "Transferred.specificity.minus.buddy.local.specificity",
                   "Transferred.PPV.minus.buddy.local.PPV",
                   "Transferred.NPV.minus.buddy.local.NPV"
                   )
    sample.datasets.with.comparison <- lapply(sample.datasets.with.comparison, function(sample) lapply(sample, function(df) {
      df <- df[, col_order]
      return(df)
    }))
    
    ## Return output
    return(sample.datasets.with.comparison)

}

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
        ## Bottom sample (i.e. Low volume, or non-metropolitan)
        x[[2]]$Transferred.mistriage.minus.buddy.local.mistriage <- x[[2]][["Transfer.mistriage"]] - x[[1]][["Validation.mistriage"]]
        x[[2]]$Transferred.undertriage.minus.buddy.local.undertriage <- x[[2]][["Transfer.undertriage"]] - x[[1]][["Validation.undertriage"]]
        x[[2]]$Transferred.overtriage.minus.buddy.local.overtriage <- x[[2]][["Transfer.overtriage"]] - x[[1]][["Validation.overtriage"]]
        return(x)
    })
    ## Return output
    return(sample.datasets.with.comparison)

}

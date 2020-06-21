#' CombineClean
#'
#' Takes the two lists and combines to a single list.
#' Removes values noot needed for future results processing
#' @param split.datasets. Split data sets
#' @param comparison.split.datasets. Comparison data
CombineClean <- function(split.datasets, comparison.split.datasets){

    ## Setup output variable
    split.datasets.output <- split.datasets
    ## Combine on correct level
    for (x in seq_along(split.datasets.output)) {
        for (y in seq_along(split.datasets.output[[x]])) {
            for (z in seq_along(split.datasets.output[[x]][[y]])) {
                split.datasets.output[[x]][[y]][[z]] <- c(split.datasets[[x]][[y]][[z]], comparison.split.datasets[[x]][[y]][[z]], z)
            }
        }
    }
    ## Clean to only show values needed for next steps
    split.datasets.output <- lapply(split.datasets.output, function(sample) lapply(sample, function(imp) (lapply(imp, function(df) {
        df <- list("Imputation" = df[[27]],
                   "Validation mistriage" = df[[7]],
                   "Validation undertriage" = df[[8]],
                   "Validation overtriage" = df[[9]],
                   "Transfer mistriage" = df[[17]],
                   "Transfer undertriage" = df[[18]],
                   "Transfer overtriage" = df[[19]],
                   
                   "Validation sensitivity" = df[[10]],
                   "Validation specificity" = df[[11]],
                   "Validation PPV" = df[[12]],
                   "Validation NPV" = df[[13]],
                   "Validation AUC" = df[[14]],
                   "Validation calibration intercept" = df[[15]],
                   "Validation calibration slope" = df[[16]],
                   "Transfer sensitivity" = df[[20]],
                   "Transfer specificity" = df[[21]],
                   "Transfer PPV" = df[[22]],
                   "Transfer NPV" = df[[23]],
                   "Transfer AUC" = df[[24]],
                   "Transfer calibration intercept" = df[[25]],
                   "Transfer calibration slope" = df[[26]])
    })))) 
    ## Return output
    return(split.datasets.output)

}

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
        df <- list("Imputation" = df[[21]],
                   "Validation mistriage" = df[[7]],
                   "Validation undertriage" = df[[8]],
                   "Validation overtriage" = df[[9]],
                   "Transfer mistriage" = df[[14]],
                   "Transfer undertriage" = df[[15]],
                   "Transfer overtriage" = df[[16]],
                   
                   "Validation sensitivity" = df[[10]],
                   "Validation specificity" = df[[11]],
                   "Validation PPV" = df[[12]],
                   "Validation NPV" = df[[13]],
                   "Transfer sensitivity" = df[[17]],
                   "Transfer specificity" = df[[18]],
                   "Transfer PPV" = df[[19]],
                   "Transfer NPV" = df[[20]],
                   "Transfer AUC" = df[[21]],
                   "Transfer calibration intercept" = df[[22]],
                   "Transfer calibration slope" = df[[23]])
    })))) 
    ## Return output
    return(split.datasets.output)

}

#' CombineClean
#'
#' Takes the two lists and combines to a single list.
#' Removes values noot needed for future results processing
#' @param split.data.sets. Split data sets
#' @param comparison.split.data.sets. Comparison data
CombineClean <- function(split.data.sets, comparison.split.data.sets){

    ## Setup output variable
    split.data.sets.output <- split.data.sets
    ## Combine on correct level
    for (x in seq_along(split.data.sets.output)) {
        for (y in seq_along(split.data.sets.output[[x]])) {
            for (z in seq_along(split.data.sets.output[[x]][[y]])) {
                split.data.sets.output[[x]][[y]][[z]] <- c(split.data.sets[[x]][[y]][[z]], comparison.split.data.sets[[x]][[y]][[z]], z)
            }
        }
    }
    ## Clean to only show values needed for next steps
    split.data.sets.output <- lapply(split.data.sets.output, function(sample) lapply(sample, function(imp) (lapply(imp, function(df) {
        df <- list("Imputation" = df[[13]],
                   "Validation mistriage" = df[[7]],
                   "Validation undertriage" = df[[8]],
                   "Validation overtriage" = df[[9]],
                   "Transfer mistriage" = df[[10]],
                   "Transfer undertriage" = df[[11]],
                   "Transfer overtriage" = df[[12]])
    })))) 
    ## Return output
    return(split.data.sets.output)

}

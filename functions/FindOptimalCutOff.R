#' FindOptimalCutOff
#' 
#' Finds the optimal cutoff for the development model.
#' Outputs this value and adds it to the list for each respective sample(=model)
#' Dependant on the function: FindCutOff
#' @param df.list List of dataframes.
FindOptimalCutOff <- function(df.list) {

    ## Get grid
    grid <- df.list[["Development grid"]]
    ## Create list of probabilities
    prob.list <- as.list(unique(grid$probs))
    ## Search for, and select the optimal cutoff
    optimal.cutoff <- FindCutOff(prob.list = prob.list, grid = grid)
    ## Create output
    output <- df.list
    output[["Optimal CutOff"]] = optimal.cutoff
    ## Return output
    return(output)

}
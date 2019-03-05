#' NACounterDataSet
#'
#' Inputs a data set in list form
#' Ex. data.sets$high.volume.vs.low.volume
#' Counts the number of NA in the inputted data set and calculates the percentage.
#' Outputs a data frame with the total number of NA and the percentage of NA in that data frame
#' (Meant to run over each data set)
#' @param list List. A data set, containing two or more samples. No default.
NACounterDataSet <- function(list) {
  
    ## Error handling
    if (!is.list(list))
        stop ("Input has to be a list")
    ## Count number of NA in the data set
    y <- sapply(list, function(x) sum(is.na(x)))
    total_number_of_na <- sum(y) 
    ## Percentage of NA
    total_data_points <- sapply(list, function(x) ncol(x) * nrow(x))
    total_data_points <- sum(total_data_points)
    percent_NA <- total_number_of_na / total_data_points
    percent_NA <- percent_NA * 100
    percent_NA <- round(percent_NA, 2)
    ## Combine to output
    output <- as.data.frame(total_number_of_na)
    output$percent_NA <- percent_NA
    ## Return new data.frame with NA data from data set
    return(output)
}

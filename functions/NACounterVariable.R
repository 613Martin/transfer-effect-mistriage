#' NACounterVariable
#'
#' Inputs a sample, in the form of a data fram
#' Ex. data.sets$high.volume.vs.low.volume$high.volume
#' Returns a new data frame with the number of NA per column and the
#' percentage of NA in that column.
#' 
#' (Meant to run over each of the samples in the data.sets list)
#' (Eg. high.volume then low.volume then metropolitan...)
#' 
#' @param df Dataframe. The study sample raw data. No default.
NACounterVariable <- function(df) {
  
    ## Error handling
    if (!is.data.frame(df))
        stop ("Input has to be a data.frame")
    ## Count number of NA per column
    number_of_na <- colSums(is.na(df))
    number_of_na <- as.data.frame(number_of_na)
    ## Percent
    number_of_na$percentage <- number_of_na$number_of_na / nrow(df)
    number_of_na$percentage <- number_of_na$percentage * 100
    number_of_na$percentage <- round(number_of_na$percentage, 2)
    ## Return new data data.frame with NA data
    return(number_of_na)
}

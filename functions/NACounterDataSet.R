#' NACounterDataSet
#'
#' Counts the number of NA in the inputted data set and calculates the
#' percentage.  Outputs a data frame with the total number of NA and the
#' percentage of NA in that data frame (Meant to run over each data set)
#' @param df A dataframe. No default.
NACounterDataSet <- function(df) {
  
    ## Error handling
    if (!is.data.frame(df))
        stop ("Input has to be a data.frame")
    ## Count number of NA in the data set
    y <- sapply(df, function(x) sum(is.na(x)))
    total.number.of.na <- sum(y) 
    ## Percentage of NA
    total.data.points <- sapply(df, function(x) ncol(x) * nrow(x))
    total.data.points <- sum(total.data.points)
    percent.NA <- total.number.of.na / total.data.points
    percent.NA <- percent.NA * 100
    percent.NA <- round(percent.NA, 2)
    ## Combine to output
    output <- as.data.frame(total.number.of.na)
    output$percent.NA <- percent.NA
    ## Return new data.frame with NA data from data set
    return(output)
}

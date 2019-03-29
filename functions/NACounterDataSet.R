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
    ## Calculate number of observations with missing values
    total.number.of.na <- nrow(df) - nrow(df[complete.cases(df), ])
    ## Combine to output
    output <- as.data.frame(total.number.of.na)
    output$percent.NA <- round(total.number.of.na/nrow(df) * 100)
    
    ## Set ouput file name
    # df.name <- deparse(substitute(df))
    # file.name <- print(paste("NACountDataSet of ", df.name, ".txt"))
    ## Export NA data as a txt file
    # write.table(output, file = file.name, sep="\t")
    ## Return new data.frame with NA data from data set
    return(output)
}

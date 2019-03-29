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
    number.of.na <- colSums(is.na(df))
    number.of.na <- as.data.frame(number.of.na)
    ## Percent
    number.of.na$percentage <- number.of.na$number.of.na / nrow(df)
    number.of.na$percentage <- number.of.na$percentage * 100
    number.of.na$percentage <- round(number.of.na$percentage, 2)
    
    ## Set ouput file name
    # df.name <- deparse(substitute(df))
    # file.name <- print(paste("NACountVariable of ", df.name, ".txt"))
    ## Export NA data as a txt file
    # write.table(number.of.na, file = file.name, sep="\t")  
    ## Return new data data.frame with NA data
    return(number.of.na)
}

#' IndividualSampleCreator
#'
#' Uses input "Multi.Centre.Sample"
#' (As "Multi.Centre.Sample" already contains all valid individual centres)
#' Splits inputtet data frame into individual centres
#' Outputs several data frames: "Individual.Centre.Sample 1:n", where n is the total 
#' number of valid individual samples
#' 
#' @param df Dataframe. Multi.Centre.Sample
IndividualSampleCreator <- function(df) {
  
    ## Error handling
    if (!is.data.frame(df))
        stop ("Input has to be a data.frame")
    ## Turn each individual centre into list element
    x <- split(df, df$Sjukhuskod)
    ## Name each individual centre
    n = length(x)
    prefix <- "Individual.Sample."
    suffix <- seq(1:n)
    ind.names <- paste(prefix, suffix, sep = "")
    names(x) <- ind.names
    ## Extract each sample as separate a data frame to the global enviroment
    list2env(x, .GlobalEnv)
    ## Return data.frame
    return(df)
}

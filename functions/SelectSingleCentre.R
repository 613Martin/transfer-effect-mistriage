#' SelectSingleCentre
#'
#'  
#' @param centre.id A character vector of length 1. The centre id. No default.
#' @param df A dataframe. Must contain a column "Sjukhuskod". No default.
#' 
SelectSingleCentre <- function(centre.id, df) {
    ## Error handling
    if (!is.data.frame(df))
        stop ("Input has to be a data.frame")
    if (!("Sjukhuskod" %in% colnames(df)))
        stop ("df has to include the column Sjukhuskod")
    if (!("Valid_Individual_Centre" %in% colnames(df)))
        stop ("df has to include the column Valid_Individual_Centre")    

    ## Select single centre
    centre.sample <- df[df$Sjukhuskod == centre.id, ]
    return.object <- NULL
    if (all(centre.sample$Valid_Individual_Centre == "Yes"))
        return.object <- centre.sample
    return(return.object)
}

#' IndividualCentreCheck
#'
#' Creates a table from a data.frame, based on number of deaths within 30 days of trauma.
#' "Sjukhusnummer" with > 170 deaths within 30 days of trauma are considered valid individual centres.
#' Adds a new colum called Valid_Individual_Centre
#' If a "Sjukhusnummer" is a valid individual centre, adds Yes to the new colum, otherwise No.
#' 
#' @param df A dataframe. Must contain a column "Sjukhuskod". No default.
IndividualCentreCheck <- function(df) {
  
    ## Error handling
    if (!is.data.frame(df))
        stop ("Input has to be a data.frame")
    if (!("Sjukhuskod" %in% colnames(df)))
        stop ("df has to include the column Sjukhuskod")
  
    ## Evaluates if Sjukhuskod is valid individual centre
    x <- as.data.frame(table(df$Sjukhuskod, df$res_survival, exclude = c(2, NA, NaN, 999, "Alive")))
    y <- as.vector(x$Var1[x$Freq >= 170])
  
    df$Valid_Individual_Centre <- ifelse(df$Sjukhuskod %in% y, "Yes", "No")
  
    return(df)
}

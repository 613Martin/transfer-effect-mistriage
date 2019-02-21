#' HighVolumeCheck
#'
#' Sorts a data.frame into a list, based on number of entries by each "Sjukhusnummer".
#' "Sjukhusnummer" at the top quartile of this list is a High Volume Centre
#' Adds a new colum called High volume Centre.
#' If a "Sjukhusnummer" is a High Volume Centre, add Yes to the new colum, otherwise No.
#' 
#' @param df A dataframe. Must contain a column "Sjukhuskod". No default.
#' 
HighVolumeCheck <- function(df) {
  ## Error handling
  if (!is.data.frame(df))
    stop ("Input has to be a data.frame")
  if (!("Sjukhuskod" %in% colnames(df)))
    stop ("df has to include the column Sjukhuskod")
  
  ## Assign High_Volume variable
 x <- table(df$Sjukhuskod)
 x <- sort(x, decreasing = TRUE)
 y <- length(x)
 z <- head(x, y/4)
 l <- as.vector(names(z))
 
 df$High_Volume_Centre <- ifelse(df$Sjukhuskod == l, "Yes", "No")
 
  return(df)
}

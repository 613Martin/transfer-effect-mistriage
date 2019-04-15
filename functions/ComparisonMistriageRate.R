#' ComparisonMistriageRate
#' 
#' Obtains mistriage rate when transferring model and cutoff to "buddy sample".
#' Does this for all imputations.
#' Outputs a list with the rates of mistriage for transfers, for each imputation.
#' Reference:
#' df[[1]] = Top subsample i.e. High vol, Metropolitan, or Multi centre
#' df[[2]] = Bottom subsample i.e. Low vol, Non-metropolitan, or Single centre
#' @param df Dataframe.
ComparisonMistriageRate <- function(df) {

  ## Error handling
  #
  
  ## Top to Bottom
  TtB <- lapply(df[[1]], function(x) {
   imp <- mean(x[[2]][[1]])
   CalculateMistriage(data = df[[2]][[imp]][[2]], model = x[[3]], cutoff = x[[6]])
  })
  ## Bottom to Top
  BtT <- lapply(df[[2]], function(x) {
    imp <- mean(x[[2]][[1]])
    CalculateMistriage(data = df[[1]][[imp]][[2]], model = x[[3]], cutoff = x[[6]])
  })
  ## Create output
  output <- list("Top to Bottom" = TtB, "Bottom to Top" = BtT)
  ## Return output
  return(output)
}

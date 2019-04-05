#' ComparisonMistriageRate
#' 
#' Obtains mistriage rate when transferring model and cutoff to "buddy sample"
#' Outputs a list with the rates of mistriage for transfers
#' Reference:
#' df[[1]] = Top subsample i.e. High vol, Metropolitan, or Multi centre
#' df[[2]] = Bottom subsample i.e. Low vol, Non-metropolitan, or Single centre
#' df[[1]][[x]] = Top subsample / x
#' x =
#´ [[1]] = Development data in each sample
#´ [[2]] = Validation data in each sample
#´ [[3]] = Coeffs
#´ [[4]] = Dev grid
#´ [[5]] = Val grid
#´ [[6]] = Optimal CutOff
#' @param df Dataframe.
ComparisonMistriageRate <- function(df) {

  ## Top to bottom
  toptobottom <- CalculateMistriage(data = df[[2]][[2]], model = df[[1]][[3]], cutoff = df[[1]][[6]])
  ## Bottom to top
  bottomtotop <- CalculateMistriage(data = df[[1]][[2]], model = df[[2]][[3]], cutoff = df[[2]][[6]])
  ## Create output
  output <- list("Top to Bottom" = toptobottom, "Bottom to Top" = bottomtotop)
  ## Return output
  return(output)
}

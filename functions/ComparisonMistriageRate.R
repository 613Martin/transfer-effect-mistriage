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
  
  ## Top to Bottom
  TtB <- lapply(df[[1]], function(x) {
    imp <- mean(x[[2]][[1]])
    CalculateUndertriageOvertriage (data = df[[2]][[imp]]$Validation, model = x[["Model coefficients"]], cutoff = x[["Optimal CutOff"]])
  })
  ## Bottom to Top
  BtT <- lapply(df[[2]], function(x) {
    imp <- mean(x[[2]][[1]])
    CalculateUndertriageOvertriage (data = df[[1]][[imp]]$Validation, model = x[["Model coefficients"]], cutoff = x[["Optimal CutOff"]])
  })
  ## Obtain and label correct values
  TtB <- lapply(TtB, function(x) {
    list("Transfer mistriage" = setNames(x["undertriage"]+x["overtriage"], "mistriage"), 
         "Transfer undertriage" = x["undertriage"],
         "Transfer overtriage" = x["overtriage"])
  })
  BtT <- lapply(BtT, function(x) {
    list("Transfer mistriage" = setNames(x["undertriage"]+x["overtriage"], "mistriage"), 
         "Transfer undertriage" = x["undertriage"],
         "Transfer overtriage" = x["overtriage"])
  })  
  ## Create output
  output <- list("Top to Bottom" = TtB, "Bottom to Top" = BtT)
  ## Return output
  return(output)
}

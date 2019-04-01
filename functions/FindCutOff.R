#' FindCutOff
#' 
#' ## WORK IN PROGRESS ## 
#'
#' Inputs a data.frame. Usees grid data frame.
#' Finds the cut off value which provides the best triage rate.
#' @param df Dataframe. 
FindCutOff <- function(df) {
  
  ## Error handling
  if (!is.data.frame(df))
    stop ("Input has to be a data.frame")
  
  ## Setup valid cutoff vector
  valid.cutoffs <- vector()
  
  ## Set cutoff to test
  # Will change to loop over all probabilities produced by model
  tested.cut.off <- 0.8
  
  ## Set tested data (all major traumas according to cutoff)
  tested.data.major <- df[df$prob >= tested.cut.off,]
 
  ## Set tested data (all minor traumas according to cutoff)
  tested.data.minor <- df[df$prob < tested.cut.off,]
  
  ## Estimate undertriage in tested data (1 = Yes)
  num.of.undertriage <- sum(tested.data.minor$ISS_over_15 == 1)      
  undertriage.rate <- num.of.undertriage / nrow(df)
  
  ## Estimate overtriage in tested data
  num.of.overtriage <- sum(tested.data.major$ISS_over_15 == 2)
  overtriage.rate <- num.of.overtriage / nrow(df)
  
  ## Estimate mistriage rate
  mistriage.rate <- undertriage.rate + overtriage.rate
  
  ## Evaluate undertriage and if valid add to valid.cutoffs
  if (undertriage.rate <= 0.5) {
    valid.cutoffs <- c(valid.cutoffs, tested.cut.off)
  }
  
  ## Test all valid cutoffs for the one that is best in terms of overtriage
  # ...
  
  return(valid.cutoffs)
  }

#' FindCutOff
#' 
#' Inputs a data.frame. Use Grid data frame.
#' Finds the cut off value which provides the best triage rate.
#' @param df Dataframe. 
FindCutOff <- function(df) {
  
  ## Error handling
  if (!is.data.frame(df))
   stop ("Input has to be a data.frame")
  
  ## Setup valid cutoff vector
  valid.cutoffs <- vector()
  ## Setup best cutoff vector, and set to 1
  best.cutoff <- vector()
  best.cutoff <- 1
  
  ## Finding all valid cutoffs (undertriage rate <0.05)
  ## Set cutoff to test
  for (i in unique(df$prob)) {
   tested.cut.off <- i
   ## Set tested data (all major traumas according to cutoff)
   tested.data.major <- df[df$prob >= tested.cut.off,]
   ## Set tested data (all minor traumas according to cutoff)
   tested.data.minor <- df[df$prob < tested.cut.off,]
   ## Estimate undertriage in tested data (ISS_over_15 = 1 = Yes)
   num.of.undertriage <- sum(tested.data.minor$ISS_over_15 == 1)      
   undertriage.rate <- num.of.undertriage / nrow(df)
   ## Estimate overtriage in tested data
   num.of.overtriage <- sum(tested.data.major$ISS_over_15 == 2)
   overtriage.rate <- num.of.overtriage / nrow(df)
   ## Estimate mistriage rate
   mistriage.rate <- undertriage.rate + overtriage.rate
   ## Evaluate undertriage and if valid add to valid.cutoffs
   if (undertriage.rate <= 0.05) {
    valid.cutoffs <- c(valid.cutoffs, tested.cut.off)
   }
  }
  
  ## Finding best cutoff (optimal overtriage)
  ## Test all valid cutoffs for the one that is best in terms of overtriage
  for (x in unique(valid.cutoffs)) {
    tested.valid.cut.off <- x
    ## Set tested data (all major traumas according to cutoff)
    tested.data.major <- df[df$prob >= tested.valid.cut.off,]
    ## Set tested data (all minor traumas according to cutoff)
    tested.data.minor <- df[df$prob < tested.valid.cut.off,]
    ## Estimate undertriage in tested data (ISS_over_15 = 1 = Yes)
    num.of.undertriage <- sum(tested.data.minor$ISS_over_15 == 1)      
    undertriage.rate <- num.of.undertriage / nrow(df)
    ## Estimate overtriage in tested data
    num.of.overtriage <- sum(tested.data.major$ISS_over_15 == 2)
    overtriage.rate <- num.of.overtriage / nrow(df)
    ## Estimate mistriage rate
    mistriage.rate <- undertriage.rate + overtriage.rate
    ## Evaluate overtriage and select best cutoff
    if (overtriage.rate <= best.overtriage.rate) {
      best.cutoff <- tested.valid.cut.off
    } 
  }
  ## Return the best cutoff!
  return(best.cutoff)
  }

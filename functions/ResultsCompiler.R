#' ResultsCompiler
#'
#' Takes the list of values and calculates and prepares results,
#' @param combined.split.data.sets List with combination of required values.
ResultsCompiler <- function(combined.split.data.sets){
  
  ## Convert all imutations to data frames
  combined.split.data.sets.data.frames <- lapply(combined.split.data.sets, function(sample) lapply(sample, function(imp) (lapply(imp, function(x) {
    x <- data.frame(x)
  }))))
  ## Combine all data frames in a sample using rbind
  sample.data.sets <- lapply(combined.split.data.sets.data.frames, function(sample) lapply(sample, function(df) {
    df1 <- df[[1]]
    for (x in 2:length(df)) {
      df1 <- rbind(df1, df[[x]])
    }
    return(df1)
  }))
  ## Calculate difference, local performance vs transferred model, transferred model performance minus local model performance
  sample.data.sets.with.comparison <- lapply(sample.data.sets, function(x) {
    ## Top sample (i.e. High volume, or metropolitan)
    x[[1]]$Transferred.mistriage.minus.buddy.local.mistriage <- x[[1]][[5]] - x[[2]][[2]]
    x[[1]]$Transferred.undertriage.minus.buddy.local.undertriage <- x[[1]][[6]] - x[[2]][[3]]
    x[[1]]$Transferred.overtriage.minus.buddy.local.overtriage <- x[[1]][[7]] - x[[2]][[4]]
    ## Bottom sample (i.e. Low volume, or non-metropolitan)
    x[[2]]$Transferred.mistriage.minus.buddy.local.mistriage <- x[[2]][[5]] - x[[1]][[2]]
    x[[2]]$Transferred.undertriage.minus.buddy.local.undertriage <- x[[2]][[6]] - x[[1]][[3]]
    x[[2]]$Transferred.overtriage.minus.buddy.local.overtriage <- x[[2]][[7]] - x[[1]][[4]]
    return(x)
  })
  ## Return output
  return(sample.data.sets.with.comparison)
}

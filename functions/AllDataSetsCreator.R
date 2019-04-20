#' AllDataSetsCreator
#'
#' Inputs imputed data sets and the number of imputations.
#' Original imputation (.imp = 0) already removed from data sets.
#' Outputs the data sets in list form separeted by imputation.
#' @param data.sets The data sets.
#' @param imputations The number of imputations used.
AllDataSetsCreator <- function(data.sets, imputations) {

  ## Setup variables and list
  ds <- data.sets
  imp <- imputations
  set.list <- as.list(c(1:imp))
  ## Setup for-loop
  for (i in 1:imp) {
  ## Create data set x
  data.set.x <- lapply(ds, function(sample) lapply(sample, function(x) {
    x <- x[x$.imp == i,]
  }))
  ## Input into list
  set.list[[i]] <- data.set.x
  }
  ## Setup list names
  names(set.list) <- paste("data.set", 1:imp, sep = ".") 
  ## Create output list
  output <- set.list
  ## Return list with development and validation sample
  return(output)
}

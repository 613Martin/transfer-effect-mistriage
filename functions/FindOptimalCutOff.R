#' FindOptimalCutOff
#' 
#' Finds the optimal cutoff for the development model.
#' Outputs this value and adds it to the list for each respective sample(=model)
#' Dependant on the function: FindCutOff
#' Reference:
#´ df[[1]] = Development data in each sample
#´ df[[2]] = Validation data in each sample
#´ df[[3]] = Coeffs
#´ df[[4]] = Dev grid
#´ df[[5]] = Val grid
#' @param df Dataframe.
FindOptimalCutOff <- function(df) {

  ## Create list of probabilities
  data.sets$high.volume.vs.low.volume$high.volume$probs <- as.list(unique(prob))
  list.of.probs <- as.list(unique(df[[4]]$probs))
  ## Search for, and select the optimal cutoff
  optimal.cutoff <- FindCutOff(prob.list = list.of.probs, grid = df[[4]])
  ## Create output
  output <- list("Development" = df[[1]], "Validation" = df[[2]],  "Model coefficients" = df[[3]], "Development grid" = df[[4]], "Validation grid" = df[[5]], "Optimal CutOff" = optimal.cutoff)
  ## Return output
  return(output)
}

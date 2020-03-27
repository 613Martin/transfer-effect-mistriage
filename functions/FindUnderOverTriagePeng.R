#' FindUnderOverTriagePeng
#' 
#' Inputs a grid with probabilities and outcomes
#' Finds overtriage and undertriage based on grid, according to Peng et al.
#' @param grid. A dataframe with "probs" and "ISS_over_15"
#' @param cutoff. The cutoff to be used for calculations
FindUnderOverTriagePeng <- function(grid, cutoff) {
  
  ## Create Cribari matrix
  cribari.matrix <- as.matrix(table(grid$probs >= cutoff, grid$ISS_over_15))
  colnames(cribari.matrix) <- c("Minor trauma", "Major trauma")
  rownames(cribari.matrix) <- c("Predicted minor trauma", "Predicted major trauma")
  cribari.matrix <- cribari.matrix[c("Predicted major trauma", "Predicted minor trauma"), ]
  cribari.elements <- list(a = cribari.matrix["Predicted major trauma", "Minor trauma"],
                           b = cribari.matrix["Predicted major trauma", "Major trauma"],
                           c = cribari.matrix["Predicted minor trauma", "Minor trauma"],
                           d = cribari.matrix["Predicted minor trauma", "Major trauma"])
  ## Obtain undertriage rate according to Peng et al, 10.1016/j.ajem.2016.08.061
  undertriage.rate <- with(cribari.elements, d/(b + d))
  ## Obtain overtriage rate according to Peng et al, 10.1016/j.ajem.2016.08.061
  overtriage.rate <- with(cribari.elements, a/(a + b))
  
  ## Prepare output data frame
  undertriageANDovertriage <- data.frame(undertriage.rate, overtriage.rate)

  ## Return ...  
  return(undertriageANDovertriage)
}
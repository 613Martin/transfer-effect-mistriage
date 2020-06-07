#' FindUnderOverTriagePeng
#' 
#' Inputs a grid with probabilities and outcomes
#' Finds overtriage and undertriage based on grid, according to Peng et al. + Traditional values
#' @param grid. A dataframe with "probs" and "ISS_over_15"
#' @param cutoff. The cutoff to be used for calculations
FindUnderOverTriagePeng <- function(grid, cutoff) {
  
  ## Create prediction matrix
  ISS_over_15 <- grid$ISS_over_15
  probs <- grid$probs
  cutoff <- cutoff
  observed <- factor(ISS_over_15, levels = c("No", "Yes"), labels = c("Minor trauma", "Major trauma"))
  predicted <- factor(probs >= cutoff, levels = c(FALSE, TRUE), labels = c("Predicted minor trauma", "Predicted major trauma"))
  prediction.matrix <- as.matrix(table(predicted, observed))
  
  ## Create cribari elements
  cribari.elements <- list(a = prediction.matrix["Predicted major trauma", "Minor trauma"],
                           b = prediction.matrix["Predicted major trauma", "Major trauma"],
                           c = prediction.matrix["Predicted minor trauma", "Minor trauma"],
                           d = prediction.matrix["Predicted minor trauma", "Major trauma"])
  
  ## Obtain undertriage rate according to Peng et al, 10.1016/j.ajem.2016.08.061
  undertriage.rate <- with(cribari.elements, d/(a + b + c + d))
  ## Obtain overtriage rate according to Peng et al, 10.1016/j.ajem.2016.08.061
  overtriage.rate <- with(cribari.elements, a/(a + b + c + d))
  ## Obtain Sensitivity according to Peng et al, 10.1016/j.ajem.2016.08.061
  sensitivity <- with(cribari.elements, b/(b + d))
  ## Obtain Specificity according to Peng et al, 10.1016/j.ajem.2016.08.061
  specificity <- with(cribari.elements, c/(a + c))
  ## Obtain Positive Predictive Value(PPV) according to Peng et al, 10.1016/j.ajem.2016.08.061
  PPV <- with(cribari.elements, b/(a + b))
  ## Obtain Negative Predictive Value(NPV) according to Peng et al, 10.1016/j.ajem.2016.08.061
  NPV <- with(cribari.elements, c/(c + d))
  
  ## Prepare output data frame
  undertriageANDovertriageANDtraditional <- data.frame(undertriage.rate, overtriage.rate, sensitivity, specificity, PPV, NPV)
  ## Return
  return(undertriageANDovertriageANDtraditional)

  ## OLD CALCULATION METHOD
  # ## Create Cribari matrix
  # cribari.matrix <- as.matrix(table(grid$probs >= cutoff, grid$ISS_over_15))
  # colnames(cribari.matrix) <- c("Minor trauma", "Major trauma")
  # rownames(cribari.matrix) <- c("Predicted minor trauma", "Predicted major trauma")
  # cribari.matrix <- cribari.matrix[c("Predicted major trauma", "Predicted minor trauma"), ]
  # cribari.elements <- list(a = cribari.matrix["Predicted major trauma", "Minor trauma"],
  #                          b = cribari.matrix["Predicted major trauma", "Major trauma"],
  #                          c = cribari.matrix["Predicted minor trauma", "Minor trauma"],
  #                          d = cribari.matrix["Predicted minor trauma", "Major trauma"])
  # ## Obtain undertriage rate according to Peng et al, 10.1016/j.ajem.2016.08.061
  # undertriage.rate <- with(cribari.elements, d/(b + d))
  # ## Obtain overtriage rate according to Peng et al, 10.1016/j.ajem.2016.08.061
  # overtriage.rate <- with(cribari.elements, a/(a + b))  
  
  # ## Obtain undertriage rate according to Peng et al, 10.1016/j.ajem.2016.08.061
  # undertriage.rate <- with(cribari.elements, d/(b + d))
  # ## Obtain overtriage rate according to Peng et al, 10.1016/j.ajem.2016.08.061
  # overtriage.rate <- with(cribari.elements, a/(a + b))
  
}

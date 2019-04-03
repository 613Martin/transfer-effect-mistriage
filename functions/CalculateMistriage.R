#' CalculateMistriage
#' 
#' Inputs a data frame on which to calculate the mistriage rate
#' Also needs input of model(coefficient) to use and cutoff.
#' Outputs the mistriage rate of the data.
#' @param data. A data frame, the data to be tested.
#' @param model. Shrunk model coefficients.
#' @param cutoff. The cutoff to use for testing
CalculateMistriage  <- function(data, model, cutoff) {
  ## Error handling
  if (!is.data.frame(data))
    stop ("Data input has to be a data.frame")
  if (!is.numeric(model))
    stop("Model coefficients input has to be numberic")
  if (!is.numeric(cutoff))
    stop ("Cutoff input has to be numeric")
  
  ## Predict 30-day mortality in data
  # Create dummy model (as predict function only accepts glm. and not "pure" coefficients)
  dummy.model <- glm(res_survival ~ ed_gcs_sum + 
                       ed_sbp_value + 
                       ed_rr_value + 
                       ed_sbp_value_spline_1 +
                       ed_sbp_value_spline_2 +
                       ed_rr_value_spline_1,
                     data = data, 
                     family = "binomial")
  #Apply inputted model coefficients to dummy.model
  dummy.model$coefficients <- model
  # Calculate product of coefficiants in all entries in data
  sum.coef <- predict(dummy.model, newdata = data)
  # Calculates probability of event in each entry
  prob <- exp(sum.coef)/(1+exp(sum.coef))
  ## Create grid on which to test the cutoff
  grid <- data.frame(cbind(prob, data$ISS_over_15))
  names(grid) <- c("probs", "ISS_over_15")
  ## Setup test data (major or minor trauma according to cutoff)
  tested.data.major <- grid[grid$probs >= cutoff,]
  tested.data.minor <- grid[grid$probs < cutoff,]
  ## Obtain undertriage (ISS_over_15 = 1 = Yes)
  num.of.undertriage <- sum(tested.data.minor$ISS_over_15 == 1)      
  undertriage.rate <- num.of.undertriage / nrow(grid)
  ## Obtain overtriage
  num.of.overtriage <- sum(tested.data.major$ISS_over_15 == 2)
  overtriage.rate <- num.of.overtriage / nrow(grid)
  ## Obtain mistriage
  mistriage.rate <- undertriage.rate + overtriage.rate
  ## Return mistriage rate
  return(mistriage.rate)
}

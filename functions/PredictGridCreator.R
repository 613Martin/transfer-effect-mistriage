#' PredictGridCreator
#' 
#' Caclulates coefficiants in development and validation,
#' then calculates probabilities.
#' Outputs a data frame with predictions and outcomes for 
#' future calculations (cutoff).
#' Reference:
#´ df[[1]] = Development data in each sample
#´ df[[2]] = Validation data in each sample
#´ df[[3]] = Coeffs
#' @param df Dataframe.
PredictGridCreator <- function(df) {
  ## Creates dummy model (as predict function only accepts glm. and not "pure" coefficients)
  dummy.model <- glm(res_survival ~ ed_gcs_sum + 
                       ed_sbp_value + 
                       ed_rr_value + 
                       ed_sbp_value_spline_1 +
                       ed_sbp_value_spline_2 +
                       ed_rr_value_spline_1,
                     data = df[[1]], 
                     family = "binomial")
  ## Apply shrunk coefficients to dummy.model
  dummy.model$coefficients <- as.numeric(df[[3]])
  ## Calculate product and sum of coefficiants in all entries in development sample
  sum.coef <- predict(dummy.model, newdata=df[[1]])
  ## Calculates probability of event in each entry in development sample
  prob <- exp(sum.coef)/(1+exp(sum.coef))
  ## Create grid from probabilities and ISS dichotomized (development sample)
  grid.development <- data.frame(cbind(prob, df[[1]]$ISS_over_15))
  names(grid.development) <- c("probs", "ISS_over_15")
  ## Calculate product and sum of coefficiants in all entries in validation sample
  sum.coef <- predict(dummy.model, newdata=df[[2]])
  ## Calculates probability of event in each entry in validation sample
  prob <- exp(sum.coef)/(1+exp(sum.coef))
  ## Create grid from probabilities and ISS dichotomized (validation sample)
  grid.validation <- data.frame(cbind(prob, df[[2]]$ISS_over_15))
  names(grid.validation) <- c("probs", "ISS_over_15")
  ## Create output
  output <- list("Development" = df[[1]], "Validation" = df[[2]],  "Model coefficients" = df[[3]], "Development grid" = grid.development, "Validation grid" = grid.validation)
  ## Return output
  return(output)
}

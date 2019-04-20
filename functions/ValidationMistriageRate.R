#' ValidationMistriageRate
#' 
#' Finds mistriage rate in each sample using the "local" model and cutoff.
#' Outputs this value and adds it to the original list
#' Dependant on the function: CalculateMistriage.R
#' Reference:
#´ df[[1]] = Development data in each sample
#´ df[[2]] = Validation data in each sample
#´ df[[3]] = Coeffs
#´ df[[4]] = Dev grid
#´ df[[5]] = Val grid
#´ df[[6]] = Optimal CutOff
#' @param df Dataframe.
ValidationMistriageRate <- function(df) {
  
  ## Find mistriage in each sample
  undertriage.overtriage <- CalculateUndertriageOvertriage(data = df[[2]], model = df[[3]], cutoff = df[[6]])
  undertriage <- undertriage.overtriage[[1]]
  overtriage <- undertriage.overtriage[[2]]
  mistriage.rate <- undertriage + overtriage
  
  ## Create output
  output <- list("Development" = df[[1]], 
                 "Validation" = df[[2]],  
                 "Model coefficients" = df[[3]], 
                 "Development grid" = df[[4]], 
                 "Validation grid" = df[[5]], 
                 "Optimal CutOff" = df[[6]], 
                 "Validation mistriage" = mistriage.rate,
                 "Validation undertriage" = undertriage,
                 "Validation overtriage" = overtriage)
  
  ## Return output
  return(output)
}

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
mistriage.rate <- CalculateMistriage(data = df[[2]], model = df[[3]], cutoff = df[[6]])
  ## Create output
  output <- list("Development" = df[[1]], 
                 "Validation" = df[[2]],  
                 "Model coefficients" = df[[3]], 
                 "Development grid" = df[[4]], 
                 "Validation grid" = df[[5]], 
                 "Optimal CutOff" = df[[6]], 
                 "Validation mistriage" = mistriage.rate )

  ## Return output
  return(output)
}

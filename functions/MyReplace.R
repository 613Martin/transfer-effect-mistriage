#' MyReplace
#'
#' Replaces values in a given variable
#' @param variable.data A vector. The variable data. No default.
#' @param current.value A vector of length 1. The value to be replaced. No
#'     default.
#' @param replacement.value A vector of length 1. The value to replace
#'     current.value with. Defaults to NA.
MyReplace <- function(variable.data, current.value, replacement.value = NA) {
  
  if (is.na(current.value)) {
    variable.data[is.na(variable.data)] <- replacement.value    
  } else {
    variable.data[variable.data == current.value] <- replacement.value
  }
  return (variable.data)
}

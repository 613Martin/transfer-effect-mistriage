#' RCRemover
#' 
#' Inputs a data.frame. 
#' Removes all forms of restricted cubic splines in data.
#' @param df Dataframe. 
RCRemover <- function(df) {
  
  ## Error handling
  if (!is.data.frame(df))
    stop ("Input has to be a data.frame")
  df[, grep("^[a-z_]*_spline_[0-9]*$", colnames(df))] <- NULL
  return(df)
}

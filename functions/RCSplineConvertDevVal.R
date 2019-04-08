#' RCSplineConvertDevVal
#' 
#' Uses rcspline.eval to convert SBP and RR to restricted cubic splines.
#' Input must be a dataframe and must contain ed_sbp_value and ed_rr_value.
#' Creates restricted cubic splines from the data in development and validation sample.
#' Uses knot locations from development sample when createing RCS in validation.
#' Reference:
#´ df[[1]] = Development data frame
#´ df[[2]] = Validation data frame
#' @param df A dataframe. No default.
RCSplineConvertDevVal <- function(df) {
  
  ## Setting number of knots(nk) to 4
  nk <- 4
  ## DEVELOPMENT DATA
  ## Converting SBP to Restricted Cubic Splines
  sbp.splines <- rcspline.eval(df[[1]]$ed_sbp_value, 
                               nk = nk)
  ## Saving SBP knot locations
  sbp.knot.locations <- attr(sbp.splines, "knots")
  ## Insert SBP spline function as new columns into inputted data frme
  sbp.splines <- as.data.frame(sbp.splines)
  colnames(sbp.splines) <- paste0("ed_sbp_value_spline_", 1:ncol(sbp.splines))
  df[[1]] <- cbind(df[[1]], sbp.splines)
  ## Converting RR to Restricted Cubic Splines
  rr.splines <- rcspline.eval(df[[1]]$ed_rr_value, 
                              nk = nk)
  ## Saving RR knot locations
  rr.knot.locations <- attr(rr.splines, "knots")
  ## Insert RR spline function as new columns into inputted data frme
  rr.splines <- as.data.frame(rr.splines)
  colnames(rr.splines) <- paste0("ed_rr_value_spline_", 1:ncol(rr.splines))
  df[[1]] <- cbind(df[[1]], rr.splines)
  ## VALIDATION DATA
  ## Converting SBP to Restricted Cubic Splines
  sbp.splines <- rcspline.eval(df[[2]]$ed_sbp_value, 
                               nk = nk, 
                               knots = sbp.knot.locations)
  ## Insert SBP spline function as new columns into inputted data frme
  sbp.splines <- as.data.frame(sbp.splines)
  colnames(sbp.splines) <- paste0("ed_sbp_value_spline_", 1:ncol(sbp.splines))
  df[[2]] <- cbind(df[[2]], sbp.splines)
  ## Converting RR to Restricted Cubic Splines
  rr.splines <- rcspline.eval(df[[2]]$ed_rr_value, 
                              nk = n, 
                              knots = rr.knot.locations)
  ## Insert RR spline function as new columns into inputted data frme
  rr.splines <- as.data.frame(rr.splines)
  colnames(rr.splines) <- paste0("ed_rr_value_spline_", 1:ncol(rr.splines))
  df[[2]] <- cbind(df[[2]], rr.splines)
  ## Create output
  output <- list("Development" = df[[1]], 
                 "Validation" = df[[2]])
  ## Return output
  return(output)
}

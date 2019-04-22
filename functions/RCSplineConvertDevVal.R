#' RCSplineConvertDevVal
#' 
#' Uses rcspline.eval to convert SBP and RR to restricted cubic splines.
#' Input must be a dataframe and must contain ed_sbp_value and ed_rr_value.
#' Creates restricted cubic splines from the data in development and validation sample.
#' Uses knot locations from development sample when createing RCS in validation.
#' @param df.list A list of dataframes. No default.
RCSplineConvertDevVal <- function(df.list) {

  ## Setting number of knots(nk) to 4
  nk <- 4
  ## DEVELOPMENT DATA
  ## Converting SBP to Restricted Cubic Splines
  sbp.splines <- rcspline.eval(df.list$Development$ed_sbp_value, 
                               nk = nk)
  ## Saving SBP knot locations
  sbp.knot.locations <- attr(sbp.splines, "knots")
  ## Insert SBP spline function as new columns into inputted data frme
  sbp.splines <- as.data.frame(sbp.splines)
  colnames(sbp.splines) <- paste0("ed_sbp_value_spline_", 1:ncol(sbp.splines))
  df.list$Development <- cbind(df.list$Development, sbp.splines)
  ## Converting RR to Restricted Cubic Splines
  rr.splines <- rcspline.eval(df.list$Development$ed_rr_value, 
                              nk = nk)
  ## Saving RR knot locations
  rr.knot.locations <- attr(rr.splines, "knots")
  ## Insert RR spline function as new columns into inputted data frme
  rr.splines <- as.data.frame(rr.splines)
  colnames(rr.splines) <- paste0("ed_rr_value_spline_", 1:ncol(rr.splines))
  df.list$Development <- cbind(df.list$Development, rr.splines)
  ## VALIDATION DATA
  ## Converting SBP to Restricted Cubic Splines
  sbp.splines <- rcspline.eval(df.list$Validation$ed_sbp_value, 
                               knots = sbp.knot.locations)
  ## Insert SBP spline function as new columns into inputted data frme
  sbp.splines <- as.data.frame(sbp.splines)
  colnames(sbp.splines) <- paste0("ed_sbp_value_spline_", 1:ncol(sbp.splines))
  df.list$Validation <- cbind(df.list$Validation, sbp.splines)
  ## Converting RR to Restricted Cubic Splines
  rr.splines <- rcspline.eval(df.list$Validation$ed_rr_value, 
                              knots = rr.knot.locations)
  ## Insert RR spline function as new columns into inputted data frme
  rr.splines <- as.data.frame(rr.splines)
  colnames(rr.splines) <- paste0("ed_rr_value_spline_", 1:ncol(rr.splines))
  df.list$Validation <- cbind(df.list$Validation, rr.splines)
  ## Create output
  output <- list("Development" = df.list$Development, 
                 "Validation" = df.list$Validation)
  ## Return output
  return(output)
}

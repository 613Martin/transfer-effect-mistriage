#' RCSplineConvert
#'
#' Uses rcspline.eval to convert SBP and RR to restricted cubic splines.
#' Input must be a dataframe.
#' Input must contain ed_sbp_value and ed_rr_value.
#' @param df A dataframe. No default.
RCSplineConvert <- function(df) {
  
    ## Error handling
    if (!is.data.frame(df))
        stop ("Input has to be a data.frame")
    if (!("ed_sbp_value" %in% colnames(df)))
        stop ("df has to include the column ed_spb_value")
    if (!("ed_rr_value" %in% colnames(df)))
        stop ("df has to include the column ed_rr_value")
  
    ## Setting number of knots(nk) to 4
    nk <- 4
  
    ## Converting SBP to Restricted Cubic Splines
    sbp.splines <- rcspline.eval(df$ed_sbp_value, nk = nk)
    ## Saving SBP knot locations, and save the variable to global enviroment
    sbp.knot.locations <- attr(sbp.splines, "knots")
    assign("sbp.knot.locations", sbp.knot.locations, .GlobalEnv)
    ## Insert SBP spline function as new columns into inputted data frme
    sbp.splines <- as.data.frame(sbp.splines)
    colnames(sbp.splines) <- paste0("ed_sbp_value_spline_", 1:ncol(sbp.splines))
    df <- cbind(df, sbp.splines)
  
    ## Converting RR to Restricted Cubic Splines
    rr.splines <- rcspline.eval(df$ed_rr_value, nk = nk)
    ## Saving RR knot locations, and save the varible to global enviroment
    rr.knot.locations <- attr(rr.splines, "knots")
    assign("rr.knot.locations", rr.knot.locations, .GlobalEnv)
    ## Insert RR spline function as new columns into inputted data frme
    rr.splines <- as.data.frame(rr.splines)
    colnames(rr.splines) <- paste0("ed_rr_value_spline_", 1:ncol(rr.splines))
    df <- cbind(df, rr.splines)
  
    return(df)
}

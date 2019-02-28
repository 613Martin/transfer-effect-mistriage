#' DataCleaning
#'
#' Replaces certiain entries, using the function MyReplace.
#' Please see provided study plan, Swedish SweTrau manual and Utstein manual for the rationale behind theese changes.
#' res_survival 999 is replaced with NA.
#' GCS 999 is raplaced with NA.
#' GCS 99 is replaced with 3.
#' @param df Dataframe. The study sample. No default.
DataCleaning <- function(df) {
  
    ## Error handling
    if (!is.data.frame(df))
        stop ("Input has to be a data.frame")
    ## Entry replacements
    df$res_survival <- MyReplace(df$res_survival, 999)
    df$ed_gcs_sum <- MyReplace(df$ed_gcs_sum, 999)
    df$ed_gcs_sum <- MyReplace(df$ed_gcs_sum, 99, 3)
    return(df)
}

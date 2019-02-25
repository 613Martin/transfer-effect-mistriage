#' VariableSelection
#'
#' Outputs a dataframe with the preselected variables: 
#' Sjukhuskod, patient age, patient gender, GCS on arrival to the ER, SBP on arrival to the ER, 
#' RR on arrival to the ER, 30-day survival, ISS, NISS and the date of trauma.
#' @param df Dataframe. The study sample raw data. No default.
VariableSelection <- function(df) {
  
    ## Error handling
    if (!is.data.frame(df))
        stop ("Input has to be a data.frame")
    ## Selects variables
    df <- df[,c("Sjukhuskod", "pt_age_yrs", "pt_Gender", "ed_gcs_sum", "ed_sbp_value", "ed_rr_value", "res_survival", "ISS", "NISS", "DateTime_Of_Trauma")]
  
    ## Return data.frame
    return(df)
}

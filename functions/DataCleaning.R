#' DataCleaning
#'
#' Replaces certiain entries, using the function MyReplace.
#' Please see provided study plan, Swedish SweTrau manual and Utstein manual for the rationale behind theese changes.
#' res_survival 999 is replaced with NA.
#' GCS 999 is raplaced with NA.
#' GCS 99 is replaced with 3.
#' DateTime_Of_Trauma "" is replaced with NA.
#' Patient sex converted to factor w/ 2 levels
#' Patient 30-day survival converted to factor w/ 2 levels
#' New colum is created: ISS_over_15, dichotomized and converted to factor
#' @param df Dataframe. The study sample. No default.
DataCleaning <- function(df) {
  
    ## Error handling
    if (!is.data.frame(df))
        stop ("Input has to be a data.frame")
    ## Entry replacements
    df$res_survival <- MyReplace(df$res_survival, 999, NA)
    df$ed_gcs_sum <- MyReplace(df$ed_gcs_sum, 999)
    df$ed_gcs_sum <- MyReplace(df$ed_gcs_sum, 99, 3)
    df$DateTime_Of_Trauma <- MyReplace(df$DateTime_Of_Trauma, "", NA)
    df$DateTime_Of_Trauma <- as.Date(as.character(df$DateTime_Of_Trauma, "%Y%m%d"))
    ## Convert patient sex to factor
    df$pt_Gender <- factor(df$pt_Gender, levels = c(1, 2), labels = c("Male", "Female"))
    ## Convert patient 30-day survival to factor
    df$res_survival <- MyReplace(df$res_survival, 2, 0)
    df$res_survival <- factor(df$res_survival, levels = c(0, 1), labels = c("Alive", "Dead"))
    ## Create column ISS>15, dichotomize and convert to factor
    df$ISS_over_15 <- df$ISS
    df$ISS_over_15 <- ifelse(df$ISS_over_15 > 15, 1, 2)
    df$ISS_over_15 <- factor(df$ISS_over_15, levels = c(1, 2), labels = c("Yes", "No"))
    return(df)
}

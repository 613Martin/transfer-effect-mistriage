#' InclusionSelection
#'
#' Selects only cases with age > 15, or cases where age is registerd as NA.
#' @param df Dataframe. The study sample. Must contain pt_age_yrs No default.
InclusionSelection <- function(df) {
  
    ## Error handling
    if (!is.data.frame(df))
        stop ("Input has to be a data.frame")
    if (!("pt_age_yrs" %in% colnames(df)))
        stop ("df has to include the column pt_age_yrs")  
    ## Case selection
    df <- df[ which(df$pt_age_yrs > 15 | is.na(df$pt_age_yrs)), ]
    return(df)
}

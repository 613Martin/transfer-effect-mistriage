#' PredictGridCreator
#' 
#' Caclulates coefficiants in development and validation,
#' then calculates probabilities.
#' Outputs a data frame with predictions and outcomes for 
#' future calculations (cutoff).
#' @param df.list List of dataframes.
PredictGridCreator <- function(df.list) {
    ## Creates dummy model (as predict function only accepts glm. and not "pure" coefficients)
    dummy.model <- glm(res_survival ~ ed_gcs_sum + ed_sbp_value + ed_rr_value,
                     data = df.list$Development,
                     family = "binomial")
    ## Apply shrunk coefficients to dummy.model
    dummy.model$coefficients <- as.numeric(df.list[["Model coefficients"]])
    ## Calculate product and sum of coefficiants in all entries in development sample
    sum.coef <- predict(dummy.model, newdata=df.list$Development)
    ## Calculates probability of event in each entry in development sample
    prob <- exp(sum.coef)/(1+exp(sum.coef))
    ## Create grid from probabilities and ISS dichotomized (development sample)
    grid.development <- data.frame(probs = prob, ISS_over_15 = df.list$Development$ISS_over_15)
    ## Calculate product and sum of coefficiants in all entries in validation sample
    sum.coef <- predict(dummy.model, newdata=df.list$Validation)
    ## Calculates probability of event in each entry in validation sample
    prob <- exp(sum.coef)/(1+exp(sum.coef))
    ## Create grid from probabilities and ISS dichotomized (validation sample)
    grid.validation <- data.frame(probs = prob, ISS_over_15 = df.list$Validation$ISS_over_15)
    ## Create output
    output <- list("Development" = df.list$Development, "Validation" = df.list$Validation,  "Model coefficients" = df.list[["Model coefficients"]], "Development grid" = grid.development, "Validation grid" = grid.validation)
    ## Return output
    return(output)
}

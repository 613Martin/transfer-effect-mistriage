#' DevelopmentModelCreator
#' 
#' Creates model coefficients from development data, and saves in corresponding list entry.
#' @param df Dataframe.
DevelopmentModelCreator <- function(df) {
    
    ## Create model function
    log.reg.model <- function(model.data) {
        glm(res_survival ~ ed_gcs_sum + 
                ed_sbp_value + 
                ed_rr_value + 
                ed_sbp_value_spline_1 +
                ed_sbp_value_spline_2 +
                ed_rr_value_spline_1,
            data = model.data, 
            family = "binomial")
    }
    ## Create development model in development sample
    development.model <- coef(log.reg.model(df[[1]]))
    ## Estimate linear shrinkage factor
    get.prediction.slope <- function(original.data, indices) {  
        model.data <- original.data[indices,]  
        model.fit <- log.reg.model(model.data)
        prediction <- predict(model.fit, newdata = original.data)
        calibration.model <- glm(original.data$res_survival ~ prediction, family = "binomial")
        slope <- coef(calibration.model)["prediction"]
        return(slope)  
    }
    linear.shrinkage.factor <- mean(boot(  
        data = df[[1]], 
        statistic = get.prediction.slope, 
        R = 1000
    )$t)
    ## Apply bootstrap results to shrink model coefficients
    shrunk.development.model <- development.model * linear.shrinkage.factor
    ## Create output
    output <- list("Development" = df[[1]], "Validation" = df[[2]],  "Model coefficients" = shrunk.development.model)
    ## Return output
    return(output)
}

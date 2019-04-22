#' CalculateUndertriageOvertriage
#' 
#' Inputs a data frame on which to calculate the mistriage rate
#' Also needs input of model(coefficient) to use and cutoff.
#' Outputs a vector with undertriage and overtriage
#' @param data. A data frame, the data to be tested.
#' @param model. Shrunk model coefficients.
#' @param cutoff. The cutoff to use for testing
CalculateUndertriageOvertriage  <- function(data, model, cutoff) {

    ## Error handling
    if (!is.data.frame(data))
        stop ("Data input has to be a data.frame")
    if (!is.numeric(model))
        stop("Model coefficients input has to be numberic")
    if (!is.numeric(cutoff))
        stop ("Cutoff input has to be numeric")
    
    ## Predict 30-day mortality in data
    ## Create dummy model (as predict function only accepts glm. and not "pure" coefficients)
    model.formula <- paste0("res_survival ~ ",
                            paste0(names(model)[-1],
                                   collapse = " + "))
    dummy.model <- glm(as.formula(model.formula),
                       data = data, 
                       family = "binomial")
    ## Apply inputted model coefficients to dummy.model
    dummy.model$coefficients <- model
    ## Calculate product of coefficiants in all entries in data
    sum.coef <- predict(dummy.model, newdata = data)
    ## Calculates probability of event in each entry
    prob <- exp(sum.coef)/(1+exp(sum.coef))
    ## Create grid on which to test the cutoff
    grid <- data.frame(probs = prob, ISS_over_15 = data$ISS_over_15)
    ## Setup test data (major or minor trauma according to cutoff)
    tested.data.major <- grid[grid$probs >= cutoff,]
    tested.data.minor <- grid[grid$probs < cutoff,]
    ## Obtain undertriage (ISS_over_15 = 1 = Yes)
    num.of.undertriage <- sum(tested.data.minor$ISS_over_15 == "Yes")      
    undertriage.rate <- num.of.undertriage / nrow(grid)
    ## Obtain overtriage
    num.of.overtriage <- sum(tested.data.major$ISS_over_15 == "No")
    overtriage.rate <- num.of.overtriage / nrow(grid)
    ## Obtain mistriage
    ## Mistriage.rate <- undertriage.rate + overtriage.rate
    ## Return mistriage rate
    undertriage.overtriage <- setNames(c(undertriage.rate, overtriage.rate), c("undertriage", "overtriage"))
    return(undertriage.overtriage)

}
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
    ## Create Cribari matrix
    cribari.matrix <- as.matrix(table(grid$probs >= cutoff, grid$ISS_over_15))
    colnames(cribari.matrix) <- c("Minor trauma", "Major trauma")
    rownames(cribari.matrix) <- c("Predicted minor trauma", "Predicted major trauma")
    cribari.matrix <- cribari.matrix[c("Predicted major trauma", "Predicted minor trauma"), ]
    cribari.elements <- list(a = cribari.matrix["Predicted major trauma", "Minor trauma"],
                             b = cribari.matrix["Predicted major trauma", "Major trauma"],
                             c = cribari.matrix["Predicted minor trauma", "Minor trauma"],
                             d = cribari.matrix["Predicted minor trauma", "Major trauma"])
    ## Obtain undertriage rate according to Peng et al, 10.1016/j.ajem.2016.08.061
    undertriage.rate <- with(cribari.elements, d/(b + d))
    ## Obtain overtriage rate according to Peng et al, 10.1016/j.ajem.2016.08.061
    overtriage.rate <- with(cribari.elements, a/(a + b))
    ## Obtain mistriage
    ## Mistriage.rate <- undertriage.rate + overtriage.rate
    ## Return mistriage rate
    undertriage.overtriage <- setNames(c(undertriage.rate, overtriage.rate), c("undertriage", "overtriage"))
    return(undertriage.overtriage)

}

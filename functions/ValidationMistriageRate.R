#' ValidationMistriageRate
#' 
#' Finds mistriage rate in each sample using the "local" model and cutoff.
#' Also calculates traditional epidemiological values and presents these.
#' Outputs this value and adds it to the original list
#' Dependant on the function: CalculateMistriage.R

#' @param df.list List of dataframes.
ValidationMistriageRate <- function(df.list) {
    
    ## Find mistriage in each sample
    undertriage.overtriage.traditional <- CalculateUndertriageOvertriage(data = df.list$Validation, model = df.list[["Model coefficients"]], cutoff = df.list[["Optimal CutOff"]])
    undertriage <- undertriage.overtriage.traditional["undertriage"]
    overtriage <- undertriage.overtriage.traditional["overtriage"]
    mistriage.rate <- undertriage + overtriage
    
    ## Create output
    output <- df.list
    output[["Validation mistriage"]] <- mistriage.rate
    output[["Validation undertriage"]] <- undertriage
    output[["Validation overtriage"]] <- overtriage
    
    ## Add traditional values to output
    sensitivity <- undertriage.overtriage.traditional["sensitivity"]
    specificity <- undertriage.overtriage.traditional["specificity"]
    PPV <- undertriage.overtriage.traditional["PPV"]
    NPV <- undertriage.overtriage.traditional["NPV"]
    AUC <- undertriage.overtriage.traditional["AUC"]
    calibration.intercept <- undertriage.overtriage.traditional["calibration.intercept"]
    calibration.slope <- undertriage.overtriage.traditional["calibration.slope"]
    output[["Validation sensitivity"]] <- sensitivity
    output[["Validation specificity"]] <- specificity
    output[["Validation PPV"]] <- PPV
    output[["Validation NPV"]] <- NPV
    output[["Validation AUC"]] <- AUC
    output[["Validation calibration intercept"]] <- calibration.intercept
    output[["Validation calibration slope"]] <- calibration.slope
    
    ## Return output
    return(output)

}

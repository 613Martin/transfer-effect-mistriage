#' ValidationMistriageRate
#' 
#' Finds mistriage rate in each sample using the "local" model and cutoff.
#' Outputs this value and adds it to the original list
#' Dependant on the function: CalculateMistriage.R

#' @param df.list List of dataframes.
ValidationMistriageRate <- function(df.list) {
    
    ## Find mistriage in each sample
    undertriage.overtriage <- CalculateUndertriageOvertriage(data = df.list$Validation, model = df.list[["Model coefficients"]], cutoff = df.list[["Optimal CutOff"]])
    undertriage <- undertriage.overtriage["undertriage"]
    overtriage <- undertriage.overtriage["overtriage"]
    mistriage.rate <- undertriage + overtriage
    ## Create output
    output <- df.list
    output[["Validation mistriage"]] <- mistriage.rate
    output[["Validation undertriage"]] <- undertriage
    output[["Validation overtriage"]] <- overtriage
    ## Return output
    return(output)

}
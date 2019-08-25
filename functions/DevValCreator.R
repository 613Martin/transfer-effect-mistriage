#' DevValCreator
#'
#' Inputs a data.frame (e.g. "high.volume")
#' Splits the data frame based on the date of trauma to development 
#' sample (early) and validation sample (late).
#' Development sample will contain 75 events(res_survival == "Dead").
#' Validation sample will contain the rest of the events.
#' @param df Dataframe. A study sample.
DevValCreator <- function(df) {
  
    ## Error handling
    if (!is.data.frame(df))
        stop ("Input has to be a data.frame")
    ## Selecting events only
    events.only <- df[which(df$res_survival == "Dead"),]
    
    ## Sorting by date 
    sorted.events.only <- tryCatch(expr = events.only[order(events.only$DateTime_Of_Trauma, decreasing = FALSE),],
                                   error = function(e) {
                                       .imp <- unique(df$.imp)
                                       filename <- paste0("output/failed.imputation.", .imp, ".", gsub(":|-| ", "", format(Sys.time())), ".Rds")
                                       saveRDS(df, filename)
                                       stop(paste0("Error on .imp ", .imp, ", saved as ", filename, ": ", e))
                                   })
   
    ## Saving cut-off date
    cut.off.date <- sorted.events.only$DateTime_Of_Trauma[75]
    ##Split data frame
    dev.sample <- df[df$DateTime_Of_Trauma <= cut.off.date, ]
    val.sample <- df[df$DateTime_Of_Trauma > cut.off.date, ]
    ## Create output list
    output <- list("Development" = dev.sample, "Validation" = val.sample)
    ## Return list with development and validation sample
    return(output)
}

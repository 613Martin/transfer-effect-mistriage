#' DevValCreator
#'
#' Inputs a data.frame (e.g. "high.volume")
#' Splits the data frame based on the date of trauma to development 
#' sample (early) and validation sample (late).
#' Development sample will contain 70 events(res_survival == "Dead").
#' Validation sample will contain the rest of the events.
#' @param df Dataframe. A study sample.
DevValCreator <- function(df) {
  
    ## Error handling
    if (!is.data.frame(df))
        stop ("Input has to be a data.frame")
    ## Selecting events only
    events.only <- df[which(df$res_survival == "Dead"),]
    
    ## Sorting by date (new line 190815)
    sorted.events.only <- events.only[order(as.Date(as.character(events.only$DateTime_Of_Trauma, "%Y%m%d")), decreasing = FALSE),]
    ## Sorting by date (old line)
    # sorted.events.only <- events.only[order(as.Date(events.only$DateTime_Of_Trauma), decreasing = FALSE),]
    
    ## Saving cut-off date
    cut.off.date <- sorted.events.only$DateTime_Of_Trauma[70]
    ##Split data frame
    dev.sample <- df[df$DateTime_Of_Trauma <= cut.off.date, ]
    val.sample <- df[df$DateTime_Of_Trauma > cut.off.date, ]
    ## Create output list
    output <- list("Development" = dev.sample, "Validation" = val.sample)
    ## Return list with development and validation sample
    return(output)
}

#' MICEImplement
#'
#' Inputs a data set in list form
#' Ex. data.sets$high.volume.vs.low.volume
#' Ex. to run:
#' data.sets$high.volume.vs.low.volume <- MICEImplement(data.sets$high.volume.vs.low.volume)
#' 
#' Runs mice on each sample in the data set. 
#' With the number of imputations = % NA in data set.
#' Returns a list with all imputations numbered 0 to n, where n is the total number of imputations, 
#' and 0 being the original data.
#' This list REPLACES then the original sample if the above example to run i used!
#' 
#' @param df.list A list of dataframes. No default.
#' @param test Logical. If TRUE only 3 imputed datasets are created. Defaults to
#'     FALSE.
MICEImplement <- function(df.list, test = FALSE) {
  
    ## Error handling
    if (!is.list(df.list))
        stop ("Input has to be a list")
  
    ## Calculate NA% in data set
    percents.NA <- lapply(df.list, function(df) NACounterDataSet(df)$percent.NA)
    
    ## Set number of imputations
    number.of.imputations <- as.integer(max(unlist(percents.NA)))
    maxit <- 5
    if (test)
        number.of.imputations <- maxit <- 3
    ## number.of.imputations <- 5
    
    ## Run mice
    df.list <- lapply(df.list, function(df) {
        temporary.data <- mice(df, m = number.of.imputations, maxit = maxit) 
        df <- complete(temporary.data, action = "long", include = TRUE)
        return(df)
    })

    return(df.list)
}

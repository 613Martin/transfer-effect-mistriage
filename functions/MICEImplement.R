#' MICEImplement
#'
#' Inputs a data set in list form
#' Ex. datasets$high.volume.vs.low.volume
#' Ex. to run:
#' datasets$high.volume.vs.low.volume <- MICEImplement(datasets$high.volume.vs.low.volume)
#' 
#' Runs mice on each sample in the data set. 
#' With the number of imputations = % NA in data set.
#' Returns a list with all imputations numbered 0 to n, where n is the total number of imputations, 
#' and 0 being the original data.
#' This list REPLACES then the original sample if the above example to run i used!
#' 
#' @param dataset.pair A list of lists. No default.
#' @param test Logical. If TRUE only 3 imputed datasets are created. Defaults to
#'     FALSE.
MICEImplement <- function(dataset.pair, test = FALSE) {
  
    ## Error handling
    if (!is.list(dataset.pair))
        stop ("Input has to be a list")
  
    ## Calculate NA% in data set
    percents.NA <- lapply(dataset.pair, function(dataset) lapply(dataset, function(sample) NACounterDataSet(sample)$percent.NA))
    
    ## Set number of imputations
    number.of.imputations <- as.integer(max(unlist(percents.NA)))
    maxit <- 5
    if (test)
        number.of.imputations <- maxit <- 3

    ## Run mice
    dataset.pair <- lapply(dataset.pair, function(dataset) lapply(dataset, function(sample) {
        sample$ISS_over_15 <- NULL
        sample$Sjukhuskod <- as.factor(sample$Sjukhuskod)
        temporary.data <- mice(sample, m = number.of.imputations, maxit = maxit)
        print(temporary.data$loggedEvents)
        sample <- complete(temporary.data, action = "long", include = TRUE)
        sample$ISS_over_15 <- factor(sample$ISS > 15, labels = c("No", "Yes"))
        print(sum(is.na(sample[sample[, ".imp"] != 0, ])))
        return(sample)
    }))

    return(dataset.pair)
}

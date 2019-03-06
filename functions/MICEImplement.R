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
#' @param list List. A data set, containing two or more samples. No default.
MICEImplement <- function(list) {
  
    ## Error handling
    if (!is.list(list))
        stop ("Input has to be a list")
  
    ## Calculate NA% in data set
    y <- sapply(list, function(x) sum(is.na(x)) )
    total_number_of_na <- sum(y) 
    total_data_points <- sapply(list, function(x) ncol(x) * nrow(x) )
    total_data_points <- sum(total_data_points)
    percent_NA <- total_number_of_na / total_data_points
    percent_NA <- percent_NA * 100
    percent_NA <- round(percent_NA, 2)
    ## Set number of imputations
    number_of_imputations <- as.integer(percent_NA)
  
    ## Run mice
    list <- lapply(list, function(x) { 
        temporary.data <- mice(x, m = number_of_imputations, maxit = 5, meth = 'pmm') 
        x <- complete(temporary.data, action = "long", include = TRUE, mild = TRUE)
        })

    return(list)
}

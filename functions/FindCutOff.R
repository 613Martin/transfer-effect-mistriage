#' FindCutOff
#' 
#' Inputs a list with probabilities, and a grid against which to test them.
#' Finds the cut off value which provides the best triage rate.
#' @param prob.list. A list of all unique probabilities to be tested.
#' @param grid. A data frame. Data frame with probabilities and outcomes for all entries.
FindCutOff <- function(prob.list, grid) {
    
    ## Create data frame which to save triage results
    probs.triage.value <- as.data.frame(unlist(prob.list))
    names(probs.triage.value) <- c("probs")
    
    ## Calculate undertriage for each probability
    probs.triage.value$undertriage <- unlist(lapply(prob.list, function(x) {
      underANDover<- FindUnderOverTriagePeng(grid = grid, cutoff = x)
      return(underANDover$undertriage.rate)
    }
    ))
    ## Calculate overtriage for each probability
    probs.triage.value$overtriage <- unlist(lapply(prob.list, function(x) {
      underANDover<- FindUnderOverTriagePeng(grid = grid, cutoff = x)
      return(underANDover$overtriage.rate)
    }
    )) 
    ## Find best cutoff
    valid.cutoffs <- probs.triage.value[probs.triage.value$undertriage <= 0.05,]
    best.cutoff <- valid.cutoffs[valid.cutoffs$overtriage == min(valid.cutoffs$overtriage),]
    ## Return optimal cutoff    
    return(mean(best.cutoff$probs))

    ## OLD CODE - PREVIOUS METHOD
    # ## Calculate undertriage for each probability
    # probs.triage.value$undertriage <- unlist(lapply(prob.list, function(x) {
    #     ## Set tested data (all major traumas according to cutoff)
    #     tested.data.major <- grid[grid$probs >= x,]
    #     ## Set tested data (all minor traumas according to cutoff)
    #     tested.data.minor <- grid[grid$probs < x,]
    #     ## Calculate undertriage in tested data 
    #     num.of.undertriage <- sum(tested.data.minor$ISS_over_15 == "Yes")
    #     ## Obtain undertriage rate according to Peng et al 2016 (number of undertriage divided by total # of major trauma)
    #     undertriage.rate <- num.of.undertriage / sum(grid$ISS_over_15 == "Yes")
    #     ## Return to reults data frame
    #     return(undertriage.rate)     
    # }
    # ))
    # 
    # ## Calculate overtriage for each probability
    # probs.triage.value$overtriage <- unlist(lapply(prob.list, function(x) {
    #     ## Set tested data (all major traumas according to cutoff)
    #     tested.data.major <- grid[grid$probs >= x,]
    #     ## Set tested data (all minor traumas according to cutoff)
    #     tested.data.minor <- grid[grid$probs < x,]
    #     ## Calculate overtriage in tested data
    #     num.of.overtriage <- sum(tested.data.major$ISS_over_15 == "No")
    #     ## Obtain overtriage rate according to Peng et al 2016 (number of overtriage divided by total # of trauma)
    #     overtriage.rate <- num.of.overtriage / nrow(grid)
    #     ## Return to reults data frame
    #     return(overtriage.rate)     
    # }
    # ))
}

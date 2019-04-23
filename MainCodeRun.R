## Effects of transfer on mistriage study main R code file 
## 
## This file was created using RStudio 1.1.463
## Cloned from GitHub 190422 23:00
MainCodeRun <- function(test = FALSE) {
    
    ## INITIALIZING
    ## Load required packages and source functions
    ## FuncPack() first needs to be sourced to run
    source("./functions/FuncPack.R")
    FuncPack()
    ## Set random seed
    set.seed(-41892)
    ## Create Results enviroment
    Results <- new.env()  
    ## Import data
    raw.data <- ImportStudyData("simulated-swetrau-data.csv")
    ## Create study sample from selected variables
    selected.data <- VariableSelection(raw.data)
    ## Run study
    stats.calculated <- RunStudy(data = selected.data , boot = FALSE, test = test)
    ## Create bootstrap samples
    number.of.bootstrap.samples = 1000
    if (test)
        number.of.bootstrap.samples = 5
    bootstrap.samples <- bengaltiger::CreateBootstrapSamples(selected.data,
                                                             save.to.disk = FALSE,
                                                             return.samples = TRUE,
                                                             number.of.bootstrap.samples = number.of.bootstrap.samples)
    ## Run bootstrap analysis
    study.cluster <- makeCluster(detectCores())
    registerDoParallel(study.cluster)
    bootstrap.results <- foreach(sample = bootstrap.samples,
                                 .packages = FuncPack(return.only = TRUE)$packages) %dopar%
        RunStudy(data = selected.data, boot = TRUE, test = test)
    stopCluster(study.cluster)
    ## Save results
    saveRDS(list(original.results = stats.calculated, bootstrap.results = bootstrap.results), "test.results.Rds")
    message("results saved to disk")
}
start.time <- Sys.time()
MainCodeRun()
end.time <- Sys.time()
message("This run took ", difftime(end.time, start.time, units = "hours"), " hours")



## Effects of transfer on mistriage study main R code file 
## 
## This file was created using RStudio 1.1.463
## Cloned from GitHub 190422 23:00
MainCodeRun <- function(test = FALSE, clean.start = TRUE) {
    
    ## INITIALIZING
    ## Load required packages and source functions
    ## FuncPack() first needs to be sourced to run
    source("./functions/FuncPack.R")
    FuncPack()
    ## Delete output directory if clean.start is TRUE
    if (clean.start)
        unlink("output", recursive = TRUE)
    ## Set random seed
    set.seed(-41892)
    ## Import data
    raw.data <- ImportStudyData("simulated-swetrau-data.csv")
    ## Create study sample from selected variables
    selected.data <- VariableSelection(raw.data)
    ## Select cases with age > 15 or age = NA
    selected.data <- InclusionSelection(selected.data)
    ## Data Cleaning
    selected.data <- DataCleaning(selected.data)
    ## DATA SETS AND SAMPLES
    ## Mark entries as High Volume or Low Volume
    selected.data <- HighVolumeCheck(selected.data)
    ## Mark entries as Metropolitan or Non-Metropolitan
    selected.data <- Metrocheck(selected.data)
    ## Mark entries as valid (>170 events) individual centres
    selected.data <- IndividualCentreCheck(selected.data)
    
    ## Get original results
    RunStudy(selected.data = selected.data , boot = FALSE, test = test)
    ## Create bootstrap samples
    number.of.bootstrap.samples = 1000
    if (test)
        number.of.bootstrap.samples = 5
    bootstrap.samples <- bengaltiger::CreateBootstrapSamples(selected.data,
                                                             strata = "res_survival",
                                                             save.to.disk = TRUE,
                                                             return.samples = TRUE,
                                                             number.of.bootstrap.samples = number.of.bootstrap.samples)
        ## Get bootstrap results
    study.cluster <- makeCluster(detectCores())
    registerDoParallel(study.cluster)
    foreach(bootstrap.sample = bootstrap.samples,
            .packages = FuncPack(return.only = TRUE)$packages,
            .export = FuncPack(return.only = TRUE)$functions) %dopar%
        RunStudy(selected.data = bootstrap.sample, boot = TRUE, test = test)
    stopCluster(study.cluster)
    ## Report all analyses completed
    message("All analyses completed")
    ## Compile results
    CompileResults()
    ## Render results document
    rmarkdown::render("Results.Rmd")

}
start.time <- Sys.time()
MainCodeRun(test = TRUE)
end.time <- Sys.time()
message("This run took ", difftime(end.time, start.time, units = "hours"), " hours")



#' RunStudy
#'
#' Inputs a data frame with the data required to run the study.  Potentially
#' runs with boostraps, augemented by foreach.  Outputs a data frame with
#' medians and IQR.  If bootstraps are used, the function will NOT return values
#' to the Results.env or pring sample characteristics tables.
#' @param selected.data Data used to run study. No default.
#' @param codebook Codebook for pretty table printing. Defaults to NULL.
#' @param boot Logical. If TRUE, the study will run with 1000
#'     bootstraps. Defaults to FALSE.
#' @param test Logical. If TRUE only multiple imputed datasets are created and 5
#'     bootstraps are used to estimate the linear shrinkage factor. Passed to
#'     MICEImplement and DevelopmentModelCreator. Defaults to FALSE.
#' @param copy.results.to.path Character or NULL. The path to which the results
#'     should be copied. Defaults to NULL.
RunStudy <- function(selected.data, codebook = NULL boot = FALSE, test = FALSE, copy.results.to.path = NULL) {


    ## Error handling
    if (!is.data.frame(selected.data))
        stop ("Input has to be a data frame")
    if (!is.logical(test))
        stop ("test has to be logical")
    if (!is.null(copy.results.to.path) & !is.character(copy.results.to.path))
        stop ("copy.results.to.path has to be character or NULL")

    ## Create results list
    Results <- list()

    ## RUN STUDY
    ## Create High Volume Sample and Low Volume Sample
    High.Volume.Sample <- selected.data[selected.data$High_Volume_Centre == "Yes", ]
    Low.Volume.Sample <- selected.data[selected.data$High_Volume_Centre == "No", ]
    ## Create Metropolitan Sample and Non-Metropolitan Sample
    Metropolitan.Sample <- selected.data[selected.data$metropolitan == "Yes", ]
    Non.Metropolitan.Sample <- selected.data[selected.data$metropolitan == "No", ]
    ## Create Multi Centre Sample (All data)
    Multi.Centre.Sample <- selected.data
    ## Create Single Centre Samples (valid individual centres)
    centre.ids <- unique(selected.data$Sjukhuskod) # Identify unique IDs
    centre.ids <- setNames(centre.ids, nm = paste0("single.centre.", centre.ids)) # Name IDs
    Single.Centre.Samples <- lapply(centre.ids, SelectSingleCentre, df = selected.data)
    Single.Centre.Samples <- Single.Centre.Samples[-which(sapply(Single.Centre.Samples, is.null))]
    
    ## CREATE DATA SETS LIST  
    ## Add samples to list
    data.sets <- list(high.volume.vs.low.volume = list(high.volume = High.Volume.Sample,
                                                       low.volume = Low.Volume.Sample),
                      metropolitan.vs.non.metropolitan = list(metropolitan = Metropolitan.Sample,
                                                              non.metropolitan = Non.Metropolitan.Sample),
                      multi.centre.vs.single.centre = c(list(multi.centre = Multi.Centre.Sample),
                                                        Single.Centre.Samples))
    
    ## RESTRICTED CUBIC SPLINES AND MISSING DATA 
    ## If not a bootstrap run, obtain NA-data and send to results
    if (boot == FALSE) {
        ## Extract missing data information from each sample
        NA.info.sample <- lapply(data.sets, function(sample) lapply(sample, NACounterDataSet))
        ## Extraxt missing data information from each variable in each sample
        NA.info.variable <- lapply(data.sets, function(sample) lapply(sample, NACounterVariable))                 
        ## Save information to Results enviroment
        Results$data.sets.before.imputations <- data.sets
        Results$NA.info.sample <- NA.info.sample
        Results$NA.info.variable <- NA.info.variable
        rm(NA.info.sample, NA.info.variable)
    }
    ## Create restricted cubic splines
    data.sets <- lapply(data.sets, function(sample) lapply(sample, RCSplineConvert))
    ## Impute missing data
    data.sets <- lapply(data.sets, MICEImplement, test = test)
    ## If not a bootstrap run, create table one
    if (boot == FALSE) {
        ## Create sample characteristics tables, and save to Results
        Results$table.one.list <- TableOneCreator(data.sets, codebook = codebook)
    }
    ## Removal of original data (.imp = 0) from data.sets 
    data.sets <- lapply(data.sets, function(sample) lapply(sample, function(x) {
        no.imp.zero <- x[!(x$.imp == "0"),]
        return(no.imp.zero)
    }))
   
    ## SPLIT DATA SETS BASED ON IMPUTATION
    split.data.sets <- lapply(data.sets, function(sample) lapply(sample, function(x) {
        x <- split(x, x$.imp)
    }))
    
    ## DEVELOPMENT AND VALIDATION
    ## Create Development and validation sample, for each imputation
    split.data.sets <- lapply(split.data.sets, function(sample) lapply(sample, function(imp) (lapply(imp, DevValCreator)))) 
    
    ## CLINICAL PREDICTION MODEL
    ## MODEL DEVELOPMENT
    ## Remove all restricted cubic splines
    split.data.sets <- lapply(split.data.sets, function(sample) lapply(sample, function(imp) (lapply(imp, function(devval) lapply(devval, RCRemover)))))
    ## Recreate restricted cubic splines using knot locations from development sample in validation sample
    split.data.sets <- lapply(split.data.sets, function(sample) lapply(sample, function(imp) (lapply(imp, RCSplineConvertDevVal)))) 
    ## Create model and apply shrinkage factor for each development sample, save as Development model coefficients
    split.data.sets <- lapply(split.data.sets, function(sample) lapply(sample, function(imp) (lapply(imp, DevelopmentModelCreator, test = test)))) 
    ## Predict 30-day mortality in development sample and create development grid + prediction grid
    split.data.sets <- lapply(split.data.sets, function(sample) lapply(sample, function(imp) (lapply(imp, PredictGridCreator)))) 
    ## Find optimal cutoff for each model.
    split.data.sets <- lapply(split.data.sets, function(sample) lapply(sample, function(imp) (lapply(imp, FindOptimalCutOff)))) 
    
    ## MODEL VALIDATION
    ## Obtain mistriage rate in the sample which the model was created, i.e. local model performance.
    split.data.sets <- lapply(split.data.sets, function(sample) lapply(sample, function(imp) (lapply(imp, ValidationMistriageRate)))) 
    
    ## MODEL COMPARISON
    ## Obtain mistriage rate in "buddy sample" in each data set using transferred model and cutoff
    comparison.split.data.sets <- lapply(split.data.sets, ComparisonMistriageRate)
    ## Combine data sets and clean variables
    combined.split.data.sets <- CombineClean(split.data.sets = split.data.sets, comparison.split.data.sets = comparison.split.data.sets)
    
    ## COMPILE RESULTS
    ## Create list of data frames with local mistriage, transfer mistriage and differance
    results.data.frames <- ResultsCompiler(combined.split.data.sets = combined.split.data.sets)
    ## Calculate medians and IQR for each sample
    stats.calculated <- CalculateStats(results.data.frames)
    ## Save results to disk
    if (!dir.exists("output"))
        dir.create("output")
    if (boot == FALSE) {
        ## Send to results
        Results$results.data.frames <- results.data.frames
        Results$original.stats <- stats.calculated
        results.specifier <- "original"
    } else {
        Results$bootstrap.stats <- stats.calculated
        results.specifier <- paste0("bootstrap.sample.", mean(selected.data[, ".boot.id"]))
    }
    file.name <- paste0("output/", results.specifier, ".results.Rds") 
    saveRDS(Results, file.name)
    if (!is.null(copy.results.to.path))
        saveRDS(Results, paste0(path, "/", results.specifier, ".results.Rds"))
    return(paste0(file.name, " saved to disk"))

}

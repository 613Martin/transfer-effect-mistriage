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
RunStudy <- function(selected.data, codebook = NULL, boot = FALSE, test = FALSE, copy.results.to.path = NULL) {


    ## Error handling
    if (!is.data.frame(selected.data))
        stop ("Input has to be a data frame")
    if (!is.logical(test))
        stop ("test has to be logical")
    if (!is.null(copy.results.to.path) & !is.character(copy.results.to.path))
        stop ("copy.results.to.path has to be character or NULL")

    ## Log
    if (boot)
        write(paste0("Bootstrap sample ", unique(selected.data[, ".boot.id"]), "started \n"), "log.out", append = TRUE)
    
    ## Create results list
    Results <- list()

    ## RUN STUDY
    ## Exclude observations with missing date and time of trauma
    if (!boot)
        Results$n.missing.date.time.of.trauma <- sum(is.na(selected.data$DateTime_Of_Trauma))
    selected.data <- selected.data[!is.na(selected.data$DateTime_Of_Trauma), ]
    
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
    
    ## Create data sets list  
    dataset.pairs <- list(high.volume.vs.low.volume = list(high.volume = High.Volume.Sample,
                                                            low.volume = Low.Volume.Sample),
                           metropolitan.vs.non.metropolitan = list(metropolitan = Metropolitan.Sample,
                                                                   non.metropolitan = Non.Metropolitan.Sample),
                           multi.centre.vs.single.centre = c(list(multi.centre = Multi.Centre.Sample),
                                                             Single.Centre.Samples))
    
    ## If not a bootstrap run, obtain NA-data and send to results
    if (boot == FALSE) {
        ## Extract missing data information from each sample
        NA.info.sample <- lapply(dataset.pairs, function(sample) lapply(sample, NACounterDataSet))
        ## Extraxt missing data information from each variable in each sample
        NA.info.variable <- lapply(dataset.pairs, function(sample) lapply(sample, NACounterVariable))                 
        ## Save information to Results enviroment
        Results$dataset.pairs.before.imputations <- dataset.pairs
        Results$NA.info.sample <- NA.info.sample
        Results$NA.info.variable <- NA.info.variable
        rm(NA.info.sample, NA.info.variable)
    }

    ## Create Development and validation sample
    dataset.pairs <- lapply(dataset.pairs, function(dataset) lapply(dataset, DevValCreator)) 

    ## Impute missing data
    imputed.dataset.pairs <- lapply(dataset.pairs, MICEImplement, test = test)
    
    ## If not a bootstrap run, create table one
    if (boot == FALSE) {
        ## Create sample characteristics tables, and save to Results
        Results$table.one.list <- TableOneCreator(imputed.dataset.pairs, codebook = codebook)
    }

    ## Restructure data   
    restructured.dataset.pairs <- lapply(imputed.dataset.pairs, function(dataset.pair) {
        lapply(dataset.pair, function(dataset) {
            dataset <- lapply(names(dataset), function(sample.name) {
                sample <- dataset[[sample.name]]
                sample$sample <- sample.name
                return(sample)
            })
            combined.samples <- do.call(rbind, dataset)
            ## Remove original data
            combined.samples <- combined.samples[combined.samples$.imp != 0, ]
            ## Split on imputation
            imp.splits <- split(combined.samples, combined.samples$.imp)
            ## Then split on sample
            imp.sample.splits <- lapply(imp.splits, function(imp.split) split(imp.split, imp.split$sample))
            return(imp.sample.splits)
        })
    })

    ## CLINICAL PREDICTION MODEL
    ## MODEL DEVELOPMENT
    ## Create model and apply shrinkage factor for each development sample, save as Development model coefficients
    cpm.data <- lapply(restructured.dataset.pairs, function(dataset.pair) lapply(dataset.pair, function(dataset) lapply(dataset, DevelopmentModelCreator, test = test)))
    ## Predict 30-day mortality in development dataset.pair and create development grid + prediction grid
    cpm.data <- lapply(cpm.data, function(dataset.pair) lapply(dataset.pair, function(dataset) lapply(dataset, PredictGridCreator))) 
    ## Find optimal cutoff for each model.
    cpm.data <- lapply(cpm.data, function(dataset.pair) lapply(dataset.pair, function(dataset) lapply(dataset, FindOptimalCutOff))) 
    
    ## MODEL VALIDATION
    ## Obtain mistriage rate in the dataset.pair which the model was created, i.e. local model performance.
    cpm.data <- lapply(cpm.data, function(dataset.pair) lapply(dataset.pair, function(dataset) lapply(dataset, ValidationMistriageRate)))
    
    ## MODEL COMPARISON
    ## Obtain mistriage rate in "buddy sample" in each data set using transferred model and cutoff
    comparison.cpm.data <- lapply(cpm.data, ComparisonMistriageRate)
    ## Combine data sets and clean variables
    combined.cpm.data <- CombineClean(split.datasets = cpm.data, comparison.split.datasets = comparison.cpm.data)
    
    ## COMPILE RESULTS
    ## Create list of data frames with local mistriage, transfer mistriage and differance
    results.data.frames <- ResultsCompiler(combined.split.datasets = combined.cpm.data)
    ## Calculate means, medians and IQR for each sample
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
        saveRDS(Results, paste0(copy.results.to.path, file.name))
    message(paste0(file.name, " saved to disk"))

    ## Log
    if (boot)
        write(paste0("Bootstrap sample ", unique(selected.data[, ".boot.id"]), "completed \n"), "log.out", append = TRUE)
}

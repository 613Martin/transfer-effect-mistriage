## Effects of transfer on mistriage study main R code file 
## 
## This file was created using RStudio 1.1.463
MainCodeRun <- function() {
  
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
    ## Select cases with age > 15 or age = NA
    selected.data <- InclusionSelection(selected.data)
    ## Data Cleaning
    selected.data <- DataCleaning(selected.data)
  
    ## DATA SETS AND SAMPLES
    ## Mark entries as High Volume or Low Volume
    selected.data.vol.mark <- HighVolumeCheck(selected.data)
    ## Create High Volume Sample and Low Volume Sample
    High.Volume.Sample <- selected.data.vol.mark[selected.data.vol.mark$High_Volume_Centre == "Yes", ]
    Low.Volume.Sample <- selected.data.vol.mark[selected.data.vol.mark$High_Volume_Centre == "No", ]
    rm(selected.data.vol.mark)
    ## Mark entries as Metropolitan or Non-Metropolitan
    selected.data.metro.mark <- Metrocheck(selected.data)
    ## Create Metropolitan Sample and Non-Metropolitan Sample
    Metropolitan.Sample <- selected.data.metro.mark[selected.data.metro.mark$metropolitan == "Yes", ]
    Non.Metropolitan.Sample <- selected.data.metro.mark[selected.data.metro.mark$metropolitan == "No", ]
    rm(selected.data.metro.mark)
    ## Mark entries as valid (>170 events) individual centres
    selected.data.ind.mark <- IndividualCentreCheck(selected.data)
    ## Create Multi Centre Sample (All data)
    Multi.Centre.Sample <- selected.data.ind.mark
    ## Create Single Centre Samples (valid individual centres)
    centre.ids <- unique(selected.data.ind.mark$Sjukhuskod) # Identify unique IDs
    centre.ids <- setNames(centre.ids, nm = paste0("single.centre.", centre.ids)) # Name IDs
    Single.Centre.Samples <- lapply(centre.ids, SelectSingleCentre, df = selected.data.ind.mark)
    Single.Centre.Samples <- Single.Centre.Samples[-which(sapply(Single.Centre.Samples, is.null))]
    rm(selected.data.ind.mark) 

    ## CREATE DATA SETS LIST  
    ## Add samples to list
    data.sets <- list(high.volume.vs.low.volume = list(high.volume = High.Volume.Sample,
                                                       low.volume = Low.Volume.Sample),
                      metropolitan.vs.non.metropolitan = list(metropolitan = Metropolitan.Sample,
                                                              non.metropolitan = Non.Metropolitan.Sample),
                      multi.centre.vs.single.centre = c(list(multi.centre = Multi.Centre.Sample),
                                                        Single.Centre.Samples))
    
    ## RESTRICTED CUBIC SPLINES AND MISSING DATA
    ## Create restricted cubic splines
    data.sets <- lapply(data.sets, function(sample) lapply(sample, RCSplineConvert))
    ## Extract missing data information from each sample
    NA.info.sample <- lapply(data.sets, function(sample) lapply(sample, NACounterDataSet))
    ## Extraxt missing data information from each variable in each sample
    NA.info.variable <- lapply(data.sets, function(sample) lapply(sample, NACounterVariable))                 
    ## Save information to Results enviroment
    Results$data.sets.before.imputations <- data.sets
    Results$NA.info.sample <- NA.info.sample
    Results$NA.info.variable <- NA.info.variable
    rm(NA.info.sample, NA.info.variable)                                              
    ## Impute missing data
    data.sets <- lapply(data.sets, MICEImplement)
    Results$data.sets.after.imputations <- data.sets                    
    
    ## TABLE ONE CREATION                  
    ## Create sample characteristics tables, and save to disk
    TableOneCreator(data.sets)

    ## DEVELOPMENT AND VALIDATION
    ## Create Development and validation sample, for each sample
    data.sets <- lapply(data.sets, function(sample) lapply(sample, DevValCreator))
    Results$data.sets.after.imputations.dev.val <- data.sets
    
    ## CLINICAL PREDICTION MODEL
    ## MODEL DEVELOPMENT                 
    ## Remove all restricted cubic splines 
    data.sets <- lapply(data.sets, function(sample) lapply(sample, function(devval) lapply(devval, RCRemover)))

    ## Recreate restricted cubic splines using appropriate knot locations for each sample
    # Does not currently use corrent knot locations. Each validation sample RCS-procedure should import apprppriate knot locations
    # from corresponding development sample. Will fix later.
    data.sets <- lapply(data.sets, function(sample) lapply(sample, function(devval) lapply(devval, RCSplineConvert)))
  
    ## Create model and apply shrinkage factor for each development sample, save as Development model coefficients
    data.sets <- lapply(data.sets, function(sample) lapply(sample, DevelopmentModelCreator))
  
    ## Predict 30-day mortality in development sample and create development grid + prediction grid
    data.sets <- lapply(data.sets2, function(sample) lapply(sample, PredictGridCreator))
  
    ## Find optimal cutoff for each model.
    data.sets <- lapply(data.sets, function(sample) lapply(sample, FindOptimalCutOff))
  
                        
    ## MODEL VALIDATION
    ## Obtain mistriage rate in the sample which the model was created, i.e. local model performance.
    data.sets <- lapply(data.sets, function(sample) lapply(sample, ValidationMistriageRate))
    Results$data.sets.with.local.model.performance <- data.sets
  
    ## MODEL COMPARISON, 
    ## Obtain mistriage rate in "buddy sample" in each data set using transferred model and cutoff
    transfer.mistriage.rate <- lapply(data.sets, ComparisonMistriageRate)
  
    ## Apply correct names to transfer mistriage list
    names(transfer.mistriage.rate[[1]]) <- c("High Vol to Low Vol", "Low Vol to High Vol")
    names(transfer.mistriage.rate[[2]]) <- c("Metropolitan to Non-metropolitan", "Non-metropolitan to Metropolitan")
    names(transfer.mistriage.rate[[3]]) <- c("Multi centre to Single centre", "Single centre to Multi centre")
  

    ## COMPILE RESULTS
    ## Create list with local mistriage results as well as transfer mistriage reuslts, for ease of viewing
    final.results.list <- list("High volume and Low volume" = list("High vol local" = data.sets[[1]][[1]][[7]], 
                                                                   "Low vol local" = data.sets[[1]][[2]][[7]],
                                                                   "High vol to Low vol" = transfer.mistriage.rate[[1]][[1]],
                                                                   "Low vol to High vol" = transfer.mistriage.rate[[1]][[2]]),
                               "Metropolitan and Non-metropolitan" = list("Metropolitan local" = data.sets[[2]][[1]][[7]],
                                                                          "Non-metropolitan local" = data.sets[[2]][[2]][[7]],
                                                                          "Metropolitan to Non-metropolitan" = transfer.mistriage.rate[[2]][[1]],
                                                                          "Non-metropolitan to Metropolitan" = transfer.mistriage.rate[[2]][[2]]),
                               "Multi centre and Single centre" = list("Multi centre local" = data.sets[[3]][[1]][[7]],
                                                                       "Single centre local" = data.sets[[3]][[2]][[7]],
                                                                       "Multi centre to Single centre" = transfer.mistriage.rate[[3]][[1]],
                                                                       "Single centre to Multi centre" = transfer.mistriage.rate[[3]][[2]]))
    Results$final.mistriage.list <- final.results.list
}

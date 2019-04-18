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
    ## Create sample characteristics tables, and save to disk
    TableOneCreator(data.sets)
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
    Results$data.sets.after.imputations.dev.val <- data.sets
  
    ## CLINICAL PREDICTION MODEL
    ## MODEL DEVELOPMENT
    ## Remove all restricted cubic splines
    split.data.sets <- lapply(split.data.sets, function(sample) lapply(sample, function(imp) (lapply(imp, function(devval) lapply(devval, RCRemover)))))
    ## Recreate restricted cubic splines using knot locations from development sample in validation sample
    split.data.sets <- lapply(split.data.sets, function(sample) lapply(sample, function(imp) (lapply(imp, RCSplineConvertDevVal)))) 
    ## Create model and apply shrinkage factor for each development sample, save as Development model coefficients
    split.data.sets <- lapply(split.data.sets, function(sample) lapply(sample, function(imp) (lapply(imp, DevelopmentModelCreator)))) 
    ## Predict 30-day mortality in development sample and create development grid + prediction grid
    split.data.sets <- lapply(split.data.sets, function(sample) lapply(sample, function(imp) (lapply(imp, PredictGridCreator)))) 
    ## Find optimal cutoff for each model.
    split.data.sets <- lapply(split.data.sets, function(sample) lapply(sample, function(imp) (lapply(imp, FindOptimalCutOff)))) 
  
   ## MODEL VALIDATION
   ## Obtain mistriage rate in the sample which the model was created, i.e. local model performance.
   split.data.sets <- lapply(split.data.sets, function(sample) lapply(sample, function(imp) (lapply(imp, ValidationMistriageRate)))) 
   Results$data.sets.with.local.model.performance <- data.sets
  
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
  ## Send to results
  Results$results.data.frames <- results.data.frames
  Results$stats.calculated <- stats.calculated
}

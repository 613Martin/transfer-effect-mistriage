## Effects of transfer on mistriage study main R code file 
## 
## This file was created using RStudio 1.1.463
MainCodeRun <- function() {
  
    ## Load required packages and source functions
    FuncPack()
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
    ## Convert SBP and RR to restricted cubic splines
    selected.data <- RCSplineConvert(selected.data)
  
    ## DATA SETS AND SAMPLES
    ## Mark entries as High Volume or Low Volume
    selected.data.vol.mark <- HighVolumeCheck(selected.data)
    ## Create High Volume Sample and Low Volume Sample
    High.Volume.Sample <- selected.data.vol.mark[ which(selected.data.vol.mark$High_Volume_Centre == "Yes"), ]
    Low.Volume.Sample <- selected.data.vol.mark[ which(selected.data.vol.mark$High_Volume_Centre == "No"), ]
    rm(selected.data.vol.mark)
    ## Mark entries as Metropolitan or Non-Metropolitan
    selected.data.metro.mark <- Metrocheck(selected.data)
    ## Create Metropolitan Sample and Non-Metropolitan Sample
    Metropolitan.Sample <- selected.data.metro.mark[ which(selected.data.metro.mark$metropolitan == "Yes"), ]
    Non.Metropolitan.Sample <- selected.data.metro.mark[ which(selected.data.metro.mark$metropolitan == "No"), ]
    rm(selected.data.metro.mark)
    ## Mark entries as valid (>170 events) individual centres
    selected.data.ind.mark <- IndividualCentreCheck(selected.data)
    ## Create Multi Centre Sample (= A combination of all valid individual centres)
    Multi.Centre.Sample <- selected.data.ind.mark[ which(selected.data.ind.mark$Valid_Individual_Centre == "Yes"), ]
    ## Create Individual Centre Sample 1-n, where n is the total number of valid individual centres 
    IndividualSampleCreator(Multi.Centre.Sample)
    rm(selected.data.ind.mark) 
  
    ## DEVELOPMENT AND VALIDATION
    ## Create High Volume Sample Development and Validation
   
    ## Create Low Volume Sample Development and Validation
   
    ## Create Metropolitan Sample Development and Validation
   
    ## Create Non-Metropolitan Development and Validation
   
    ## Create Multi Centre Sample Development and Validation
   
    ## Create Individual Sample Development and Validation
   
    ## MISSING DATA MANAGEMENT
   
    ## CLINICAL PREDICTION MODEL
    ## Model development
   
    ## Model Validation
   
    ## Model comparison
   
    ## COMPILE RESULTS
   
}

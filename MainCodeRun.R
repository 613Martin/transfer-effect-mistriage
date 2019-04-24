## Effects of transfer on mistriage study main R code file 
## 
## This file was created using RStudio 1.1.463
## Cloned from GitHub 190422 23:00
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
  raw.data <- ImportStudyData(data.file.name =  "simulated-swetrau-data 2.csv", data.path = "C:/Users/Martin/transfer-effect-mistriage/code/data/")
  ## Create study sample from selected variables
  selected.data <- VariableSelection(raw.data)
  
  ## Run study
  stats.calculated <- RunStudy(data = selected.data , boot = FALSE)
  
  ## Save Results enviroment to working directory
  save(Results, file = "Results.RData")

}

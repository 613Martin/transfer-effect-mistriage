#' Loads packages and functions
#' 
#' No parameters or variables needed to run.
FuncPack <- function() {
  
    ## Load required packages
    library(data.table)
    library(mice)
    library(Hmisc)
    library(boot)
  
    ## Source functions
    invisible(lapply(list.files("./Functions", full.names = TRUE, pattern = ".R$"), source))
}

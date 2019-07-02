#' Loads or return packages and functions
#' 
#' @param return.only Logical. If TRUE packages and functions are not loaded,
#'     but returned. Defaults to FALSE.
#' @export
FuncPack <- function(return.only = FALSE) {
    ## Get specific bengaltiger version
    library(devtools)
    install_github("martingerdin/bengaltiger@c360801126f95d1573ed5e766d743648b51cd8f9")
    
    ## Load required packages
    packages <- c("data.table",
                  "mice",
                  "Hmisc",
                  "boot",
                  "bengaltiger",
                  "doParallel",
                  "foreach",
                  "knitr",
                  "rmarkdown")

    if (!return.only)
        for(package in packages) library(package, character.only = TRUE)
   
    ## Source functions
    files <- list.files("./Functions", full.names = TRUE, pattern = ".R$")
    functions <- gsub("^./Functions/|.R$", "", files)
    if (!return.only)
        invisible(lapply(files, source))

    ## Return packages and functions
    return.object <- list(packages = packages,
                          functions = functions)
    if (return.only)
        return(return.object)
}

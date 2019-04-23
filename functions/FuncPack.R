#' Loads or return packages and functions
#' 
#' @param return.only Logical. If TRUE packages and functions are not loaded,
#'     but returned. Defaults to FALSE.
#' @export
FuncPack <- function(return.only = FALSE) {
    
    ## Load required packages
    packages <- c("data.table",
                  "mice",
                  "Hmisc",
                  "boot",
                  "bengaltiger",
                  "doParallel",
                  "foreach")

    if (!return.only)
        for(package in packages) library(package, character.only = TRUE)
    
    ## Source functions
    files <- list.files("./Functions", full.names = TRUE, pattern = ".R$")
    functions <- gsub("./Functions/|.R", "", files)
    if (!return.only)
        invisible(lapply(files, source))

    ## Return packages and functions
    return.object <- list(packages = packages,
                          functions = functions)
    if (return.only)
        return(return.object)
}

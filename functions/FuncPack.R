#' Loads or return packages and functions
#' 
#' @param return.only Logical. If TRUE packages and functions are not loaded,
#'     but returned. Defaults to FALSE.
#' @export
FuncPack <- function(return.only = FALSE) {
    ## Get specific bengaltiger version
    library(devtools)
    ## install_github("martingerdin/bengaltiger@aea49457aa68a4d00c0045cb22db582f09727a18")
    library(bengaltiger)
    
    ## Load required packages
    packages <- c("data.table",
                  "mice",
                  "Hmisc",
                  "boot",
                  "foreach",
                  "knitr",
                  "rmarkdown")

    if (!return.only) {
        for(package in packages) {
            tryCatch(expr = library(package, character.only = TRUE),
                     error = function(e) install.packages(package, repos = "https://cloud.r-project.org"),
                     finally = library(package, character.only = TRUE))
        }
        
    }
   
    ## Source functions
    files <- list.files("./functions", full.names = TRUE, pattern = ".R$")
    functions <- gsub("^./functions/|.R$", "", files)
    if (!return.only)
        invisible(lapply(files, source))

    ## Return packages and functions
    return.object <- list(packages = packages,
                          functions = functions)
    if (return.only)
        return(return.object)
}

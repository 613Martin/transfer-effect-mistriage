#' ImportStudyData 
#' 
#' Imports the study data from a csv file.
#' Uses the fread function, so remember to initialize the Hmisc package before running!
#' @param data.file.name Character vector of length 1. The name of the study
#'     data file. Defaults to NULL.
#' @param data.path Character vector of length 1. The path to the data
#'     directory. Defaults to "../data/"
ImportStudyData <- function(data.file.name = NULL, data.path = "../data/") {

    ## Error handling
    if (is.null(data.file.name)) 
        stop("You have to supply a data file name") 
    if (!is.character(data.file.name)) 
        stop("The data file name has to be a character string")
    if (length(data.file.name) > 1) 
        stop("The data file name has to be a character vector of length 1")
    ## Combine data_path and data_file_name into file_path
    file.path <- paste0(data.path, data.file.name)
    ## Import study data from a csv file
    study.data <- fread(file = file.path, data.table = FALSE, stringsAsFactors = FALSE)
    ## Return study data
    return(study.data)
}

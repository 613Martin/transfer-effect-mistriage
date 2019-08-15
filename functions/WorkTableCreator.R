#' WorkTableCreator
#'
#' Creates a data frame to work with for presentation in rmd.
WorkTableCreator <- function(result.tables.data.frame) {
  ## Create data frme
  results.all.data <- as.data.frame(result.tables.data.frame)
  ## Remove duplicate columns
  results.all.data$low.volume.Measure <- NULL
  results.all.data$metropolitan.Measure <- NULL
  results.all.data$non.metropolitan.Measure <- NULL
  results.all.data$multi.centre.Measure <- NULL
  results.all.data$single.centre.11001.Measure <- NULL
  ## Set column names
  colnames(results.all.data) <- c("Measure", "High volume", "Low volume", "Metropolitan", "Non-metropolitan", "Multiple centres", "Single centre")
  ## Retrive names to use for rows
  measure.names <- results.all.data$Measure
  measure.names <- as.vector(measure.names)
  ## Set row names
  row.names(results.all.data) <- measure.names
  ## Remove duplicate column
  results.all.data$Measure <- NULL
  ## Create output
  return(results.all.data)
}

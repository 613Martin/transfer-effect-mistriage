#' TableOneCreator
#' 
#' Creates table one, i.e. a sample characteristics table
#' @param data.sets The data.set list containing the samples.
#' @param codebook Codebook for pretty table printing. Defaults to NULL.
TableOneCreator <- function(data.sets, codebook = NULL) {
  
       Table.Variables <- names(codebook)
       table.list <- lapply(names(data.sets), function(data.set.name) {
                            data.set <- data.sets[[data.set.name]]
                            data.set <- lapply(names(data.set), function(sample.name) {
                                sample <- data.set[[sample.name]]
                                sample$group <- sample.name
                                return(sample)
                            })
                            combined.data.set <- do.call(rbind, data.set)
                            table.name <- print(paste0("Characteristics_table_of_", data.set.name))
                            bengaltiger::CreateSampleCharacteristicsTable(study.sample = combined.data.set,
                                                                          variables = Table.Variables,
                                                                          codebook = codebook,
                                                                          save.to.disk = FALSE,
                                                                          save.to.results = FALSE,
                                                                          table.name = table.name,
                                                                          include.overall = FALSE,
                                                                          group = "group") 
  })
  names(table.list) <- c("Characteristics_table_of_high.volume.vs.low.volume",
                         "Characteristics_table_of_metropolitan.vs.non.metropolitan",
                         "Characteristics_table_of_multi.centre.vs.single.centre")
  return(table.list)
}

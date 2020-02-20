#' TableOneCreator
#' 
#' Creates table one, i.e. a sample characteristics table
#' @param imputed.dataset.pairs The dataset list containing the samples.
#' @param codebook Codebook for pretty table printing. Defaults to NULL.
TableOneCreator <- function(imputed.dataset.pairs, codebook = NULL) {
    
    Table.Variables <- names(codebook)
    imputed.data.names <- names(imputed.dataset.pairs)
    table.names <- setNames(paste0("Characteristics_table_of_", imputed.data.names), nm = imputed.data.names)
    table.list <- lapply(imputed.data.names, function(dataset.pair.name) {
        dataset.pair <- imputed.dataset.pairs[[dataset.pair.name]]
        dataset.pair <- lapply(names(dataset.pair), function(dataset.name) {
            dataset <- do.call(rbind, dataset.pair[[dataset.name]])
            dataset$group <- dataset.name
            return(dataset)
        })
        combined.dataset.pair <- do.call(rbind, dataset.pair)
        table.name <- table.names[dataset.pair.name]
        sample.characteristics.table <- bengaltiger::CreateSampleCharacteristicsTable(study.sample = combined.dataset.pair,
                                                                                      variables = Table.Variables,
                                                                                      codebook = codebook,
                                                                                      save.to.disk = FALSE,
                                                                                      save.to.results = FALSE,
                                                                                      table.name = table.name,
                                                                                      include.overall = FALSE,
                                                                                      group = "group")
        return(sample.characteristics.table)
    })
    names(table.list) <- table.names
    return(table.list)
}

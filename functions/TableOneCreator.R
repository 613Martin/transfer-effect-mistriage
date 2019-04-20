#' TableOneCreator
#' 
#' Creates table one, i.e. a sample characteristics table
#' @param data.sets The data.set list containing the samples.
TableOneCreator <- function(data.sets) {
  
       Table.Variables <- c("pt_age_yrs", "pt_Gender", "ed_gcs_sum", "ed_sbp_value", "ed_rr_value", "res_survival", "ISS", "NISS", "ISS_over_15", "group")
       lapply(names(data.sets), function(data.set.name) {
           data.set <- data.sets[[data.set.name]]
           data.set <- lapply(names(data.set), function(sample.name) {
               sample <- data.set[[sample.name]]
               sample$group <- sample.name
               return(sample)
           })
           combined.data.set <- do.call(rbind, data.set)
           table.name <- print(paste0("Characteristics_table_of_", data.set.name))
           CreateSampleCharacteristicsTable(study.sample = combined.data.set,
                                            variables = Table.Variables,
                                            save.to.disk = TRUE,
                                            save.to.results = FALSE,
                                            table.name = table.name,
                                            include.overall = FALSE,
                                            group = "group") 
  })
}

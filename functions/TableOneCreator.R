#' TableOneCreator
#' 
#' Creates table one, i.e. a sample characteristics table
#' @param data.sets The data.set list containing the samples.
TableOneCreator <- function(data.sets) {
  
  Table.Variables <- c("pt_age_yrs", "pt_Gender", "ed_gcs_sum", "ed_sbp_value", "ed_rr_value", "res_survival", "ISS", "NISS", "ISS_over_15", "group")
  table.list <- lapply(names(data.sets), function(data.set.name) {
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
                                     codebook = list(pt_age_yrs = list(full.label = "Patient age",
                                                                       abbreviated.label = "Age"),
                                                     pt_Gender = list(full.label = "Patient gender", 
                                                                      abbreviated.label = "Gender"),
                                                     ed_gcs_sum = list(full.label = "Glasgow coma scale", 
                                                                       abbreviated.label = "GCS"),
                                                     ed_sbp_value = list(full.label = "Systolic blood pressure", 
                                                                         abbreviated.label = "SBP"), 
                                                     ed_rr_value = list(full.label = "Respiratory rate", 
                                                                        abbreviated.label = "RR"),
                                                     res_survival = list(full.label = "30 day survival", 
                                                                         abbreviated.label = "30DS"),
                                                     ISS = list(full.label = "Injury severity score", 
                                                                abbreviated.label = "ISS"),
                                                     NISS = list(full.label = "New injury severity score", 
                                                                 abbreviated.label = "NISS"),
                                                     ISS_over_15 = list(full.label = "Injury severity score over 15", 
                                                                        abbreviated.label = "ISS>15"),
                                                     group = list(full.label = "", 
                                                                  abbreviated.label = "")),
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

#' Compile original and bootstrap results
#'
#' Imports original and bootstrap results, estimate 95% confidence intervals and
#' saves results to disk.
#' @export
CompileResults <- function() {
    ## Get all files
    files <- list.files("output", pattern = ".Rds$", full.names = TRUE)
    ## Import results
    all.results <- lapply(files, readRDS)
    ## Name results 
    names(all.results) <- gsub("output/|.Rds", "", files)
    ## Extract stats
    original.stats <- all.results$original.results$original.stats
    bootstrap.stats <- all.results[grep("bootstrap\\.sample\\.[0-9]*\\.results", names(all.results))]
    ## Extract samples names
    sample.names <- original.stats$Sample.name
    ## Keep only numbers
    original.stats <- original.stats[, -grep("Sample\\.name|Total\\.imputations", colnames(original.stats))]
    bootstrap.stats <- lapply(bootstrap.stats, function(stats) {
        stats <- stats[[1]][, -grep("Sample\\.name|Total\\.imputations", colnames(stats[[1]]))]
        return(stats)
    })
    ## Calculate differences between original and bootstrap estimates
    bootstrap.differences <- lapply(bootstrap.stats, function(stats) {
        difference <- original.stats - stats
        difference$Sample.name <- sample.names
        return(difference)
    })
    ## Combine differences into data frame
    bootstrap.differences.data <- do.call(rbind, bootstrap.differences)
    ## Name original stats again
    original.stats$Sample.name <- sample.names
    ## Separate stats into list elements based on sample name
    original.stats.list <- split(original.stats, f = original.stats$Sample.name)
    bootstrap.differences.list <- split(bootstrap.differences.data, f = bootstrap.differences.data$Sample.name)
    ## Remove sample names again
    RemoveSampleNames <- function(stats) {
        index <- grep("Sample.name", colnames(stats))
        if (length(index) > 0)
            stats <- stats[, -index]
        return(stats)
    }
    original.stats.list <- lapply(original.stats.list, RemoveSampleNames)
    bootstrap.differences.list <- lapply(bootstrap.differences.list, RemoveSampleNames)
    ## Calculate quantiles
    bootstrap.quantiles.lists <- lapply(bootstrap.differences.list, function(bootstrap.differences) {
        bootstrap.quantiles <- lapply(bootstrap.differences, quantile, probs = c(0.025, 0.975))
        return(bootstrap.quantiles)
    })
    ## Calculate confidence intervals
    bootstrap.confidence.intervals <- lapply(sample.names, function(sample.name) {
        point.estimates <- original.stats.list[[sample.name]]
        point.estimates <- rbind(point.estimates, point.estimates)
        quantiles <- data.frame(do.call(cbind, bootstrap.quantiles.lists[[sample.name]]))
        bounds <- point.estimates - quantiles
        bounds[] <- lapply(bounds, function(bound) c(min(bound), max(bound)))
        rownames(bounds) <- c("lb", "ub")
        return(bounds)
    })
    names(bootstrap.confidence.intervals) <- sample.names
    ## Define final uncertainty as the 2.5% percentile of the 25% percentile and
    ## 97.5% percentile of the 75% percentile
    point.estimate.with.uncertainty <- lapply(sample.names, function(sample.name) {
        original <- original.stats.list[[sample.name]]
        point.estimates <- original[, grep("\\.median$", names(original))]
        colnames <- sub(".median", "", colnames(point.estimates))
        cis <- bootstrap.confidence.intervals[[sample.name]]
        lbs <- cis["lb", grep("\\.lb$", names(cis))]
        ubs <- cis["ub", grep("\\.ub$", names(cis))]
        colnames(point.estimates) <- colnames(lbs) <- colnames(ubs) <- colnames
        combined <- data.frame(t(rbind(point.estimates, lbs, ubs)))
        colnames(combined) <- c("pe", "lb", "ub")
        combined[] <- lapply(combined, function(column) sprintf("%.2f", column))
        rownames <- rownames(combined)
        combined <- data.frame(with(combined, paste0(pe, " (", lb, " - ", ub, ")")))
        combined <- cbind(rownames, combined)
        colnames(combined) <- c("Measure", "Point estimate (95% UI)")
        return(combined)
    })
    names(point.estimate.with.uncertainty) <- sample.names
    ## Create result tables
    result.tables.data.frame <- point.estimate.with.uncertainty
    # result.tables <- lapply(point.estimate.with.uncertainty, function(result) {
    #     kable(result, format = "markdown")
    # })
    ## Save result tables
    saveRDS(result.tables.data.frame , "result.tables.data.frame.Rds")
    # saveRDS(result.tables, "result.tables.Rds")
    message("Result tables saved")
}

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
    ## Keep only means
    mean.columns <- grep("\\.mean$", names(original.stats))
    original.stats <- original.stats[, mean.columns]
    bootstrap.stats <- lapply(bootstrap.stats, function(stats) stats[, mean.columns])
    ## Name original and bootstrap stats again
    original.stats$Sample.name <- sample.names
    bootstrap.stats <- lapply(bootstrap.stats, function(stats) {
        stats$Sample.name <- sample.names
        return(stats)
    })
    ## Combine bootstrap data
    bootstrap.data <- do.call(rbind, bootstrap.stats)
    ## Separate stats into list elements based on sample name
    original.stats.list <- split(original.stats, f = original.stats$Sample.name)
    bootstrap.stats.list <- split(bootstrap.data, f = bootstrap.data$Sample.name)
    ## Remove sample names again
    RemoveSampleNames <- function(stats) {
        index <- grep("Sample.name", colnames(stats))
        if (length(index) > 0)
            stats <- stats[, -index]
        return(stats)
    }
    original.stats.list <- lapply(original.stats.list, RemoveSampleNames)
    bootstrap.stats.list <- lapply(bootstrap.stats.list, RemoveSampleNames)
    ## Calculate percentiles
    bootstrap.percentiles.lists <- lapply(bootstrap.stats.list, function(bootstrap.stats) {
        bootstrap.percentiles <- lapply(bootstrap.stats, quantile, probs = c(0.025, 0.975))
        return(bootstrap.percentiles)
    })
    ## Calculate confidence intervals
    bootstrap.confidence.intervals <- lapply(sample.names, function(sample.name) {
        bounds  <- data.frame(do.call(cbind, bootstrap.percentiles.lists[[sample.name]]))
        bounds[] <- lapply(bounds, function(bound) c(min(bound), max(bound)))
        rownames(bounds) <- c("lb", "ub")
        return(bounds)
    })
    names(bootstrap.confidence.intervals) <- sample.names
    ## Define point estimate with confidence interval
    point.estimate.with.confidence.interval <- lapply(sample.names, function(sample.name) {
        original <- original.stats.list[[sample.name]]
        point.estimates <- original[, grep("\\.mean$", names(original))]
        colnames <- sub(".mean", "", colnames(point.estimates))
        cis <- bootstrap.confidence.intervals[[sample.name]]
        lbs <- cis["lb", ]
        ubs <- cis["ub", ]
        colnames(point.estimates) <- colnames(lbs) <- colnames(ubs) <- colnames
        combined <- data.frame(t(rbind(point.estimates, lbs, ubs)))
        colnames(combined) <- c("pe", "lb", "ub")
        combined[] <- lapply(combined, function(column) sprintf("%.2f", column))
        rownames <- rownames(combined)
        combined <- data.frame(with(combined, paste0(pe, " (", lb, " - ", ub, ")")))
        combined <- cbind(rownames, combined)
        colnames(combined) <- c("Measure", "Point estimate (95% CI)")
        return(combined)
    })
    names(point.estimate.with.confidence.interval) <- sample.names
    ## Create result tables
    result.tables.data.frame <- point.estimate.with.confidence.interval
    # result.tables <- lapply(point.estimate.with.uncertainty, function(result) {
    #     kable(result, format = "markdown")
    # })
    ## Save result tables
    saveRDS(result.tables.data.frame , "result.tables.data.frame.Rds")
    # saveRDS(result.tables, "result.tables.Rds")
    message("Result tables saved")
}

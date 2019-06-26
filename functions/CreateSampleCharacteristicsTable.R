#' Create sample characteristics table
#'
#' Creates the sample characteristics table. Wrapper of TableOne.
#' @param study.sample Data frame. The study sample. No default.
#' @param data.dictionary Not currently used.
#' @param group Character vector of length 1. The grouping variable. If NULL the
#'     table is not grouped. Defaults to NULL.
#' @param variables Character vector. The names of variables to include in the
#'     table. If NULL all variables in data.dictionary is included. Defaults to
#'     NULL.
#' @param exclude.variables Character vector. The names of variables to exclude
#'     from the table. If NULL no variables are excluded. Defaults to NULL.
#' @param include.overall Logical vector of length 1. If TRUE an overall column
#'     is included in the tables. Used only if group is not NULL. Defaults to
#'     TRUE.
#' @param include.missing Not currently used. Logical vector of length 1. If
#'     TRUE a column with the number (%) of missing values in each variable is
#'     included. Defaults to TRUE.
#' @param include.complete.data Logical vector of length 1. If TRUE the final
#'     table has two columns, one with complete cases only and one with multiple
#'     imputed data. Only used if the data is detected as multiple imputed,
#'     i.e. includes the variables ".imp" AND ".id". Overrides group and
#'     include.overall.
#' @param digits Numeric vector of length 1 greater than or equal to 0. Number
#'     of digits to use when rounding table entries. Defaults to 1.
#' @param save.to.results Logical vector of length 1. If TRUE the table object
#'     is saved to a results file on disk using SaveToResults. Defaults to TRUE.
#' @param table.name Character vector of length 1. The name of the table when
#'     passed to SaveToResults and saved to disk.. Deafults to
#'     "sample.characteristics.table".
#' @param table.caption Character vector of length 1. The table
#'     caption. Deafults to "Sample characteristics".
#' @param save.to.disk Logical vector of length 1. If TRUE the table object is
#'     saved to disk. Defaults to FALSE.
#' @param file.format Character vector of length 1. The format in which to save
#'     the table to disk. Has to be one of c("pdf", "rmd", "docx"). Defaults to
#'     "docx".
#' @export
CreateSampleCharacteristicsTable <- function(study.sample,
                                             data.dictionary = NULL,
                                             group = NULL,
                                             variables = NULL,
                                             exclude.variables = NULL,
                                             include.overall = TRUE,
                                             include.missing = TRUE,
                                             include.complete.data = FALSE,                                             
                                             digits = 1,
                                             save.to.results = TRUE,
                                             table.name = "sample.characteristics.table",
                                             table.caption = "Sample characteristics",
                                             save.to.disk = FALSE,
                                             file.format = "docx") {
    ## Load required packages
    library("tableone")
    library("knitr")
    ## Error handling
    if (!is.data.frame(study.sample))
        stop ("study.sample has to be a data.frame")
    if (!is.null(data.dictionary))
        stop ("data.dictionary has to be NULL")
    if ((!is.character(group) | !IsLength1(group)) & !is.null(group))
        stop ("group has to be a character vector of length 1 or NULL")
    if (!is.character(variables) & !is.null(variables))
        stop ("variables has to be a character vector or NULL")
    if (!is.character(exclude.variables) & !is.null(exclude.variables))
        stop ("exclude.variables has to be a character vector or NULL")
    if (!is.logical(include.overall) | !IsLength1(include.overall))
        stop ("include.overall has to be a character vector of length 1")
    if (!is.logical(include.missing) | !IsLength1(include.missing))
        stop ("include.missing has to be a character vector of length 1")
    if (!is.logical(include.complete.data) | !IsLength1(include.complete.data))
        stop ("include.complete.data has to be a character vector of length 1")    
    if (!is.numeric(digits) | !IsLength1(digits) | digits < 0)
        stop ("digits has to be a numeric vector of length 1")
    if (!is.logical(save.to.results) | !IsLength1(save.to.results))
        stop ("save.to.results has to be a character vector of length 1")
    if (!is.character(table.name) | !IsLength1(table.name))
        stop ("table.name has to be a character vector of length 1")
    if (!is.character(table.caption) | !IsLength1(table.caption))
        stop ("table.caption has to be a character vector of length 1")        
    if (!is.logical(save.to.disk) | !IsLength1(save.to.disk))
        stop ("save.to.disk has to be a character vector of length 1")
    if (!(file.format %in% c("docx", "rmd", "pdf")) | !IsLength1(file.format))
        stop ("file.format has to be one of docx, rmd, or pdf")
    ## Find out if data.frame is multiple imputed data
    mi <- FALSE
    if (all(c(".imp", ".id") %in% colnames(study.sample))) {
        mi <- TRUE
        exclude.variables <- c(exclude.variables, ".imp", ".id")
        if (table.caption == "Sample characteristics")
            table.caption <- "Sample characteristics of multiple imputed data"
        message ("Data is detected as multiple imputed and will be treated as such. \n")
    }
    ## Modify study sample if complete data should be reported with multiple
    ## imputed data
    if (mi & include.complete.data) {
        if (!any(study.sample$.imp == 0))
            stop ("study.sample does not include any original data as indicated by .imp == 0. Please run this function again with include.complete.data to FALSE")
        study.sample$.complete <- factor(as.numeric(study.sample$.imp != 0), c(0,1 ), c("Complete", "Imputed"))
        study.sample <- study.sample[complete.cases(study.sample), ]
        group = ".complete"
        if (!is.null(variables))
            variables <- c(variables, group)
        include.overall = FALSE
        if (table.caption == "Sample characteristics of multiple imputed data")
            table.caption <- "Sample characteristics of complete and multiple imputed data"
    }
    ## Remove original data from study sample if it should not be reported
    if (mi & !include.complete.data & any(study.sample$.imp == 0))
        study.sample <- study.sample[!study.sample$.imp == 0, ]
    ## Define variables
    if (is.null(variables)) variables <- colnames(study.sample)
    if (!is.null(exclude.variables)) variables <- variables[!(variables %in% exclude.variables)]
    if (!is.null(group)) 
        if (!(group %in% variables))
            stop ("group has to be one of the variables to be in the table")
    ## Define table data
    table.data <- study.sample[, variables]
    ## Make a list that will hold the individual tables
    table.list <- list()
    ## Create the grouped table if there should be one
    if (!is.null(group)) {
        ## Remove the group variable from the list of variables to be put in the
        ## table
        variables <- variables[!(variables %in% group)]
        ## Create the grouped table
        table.list$grouped.table <- tableone::CreateTableOne(vars = variables,
                                                             strata = group,
                                                             data = table.data,
                                                             test = FALSE) 
    }
    ## Create the overall table if there should be one
    if (is.null(group) | include.overall) table.list$overall.table <- tableone::CreateTableOne(vars = variables, data = table.data)
    ## Define variables to be treated as non-normally distributed, i.e. so that
    ## they are reported using medians and IQR
    nonormal.variables <- sapply(table.data, is.numeric)
    ## Format the tables in table.list
    formatted.tables <- lapply(table.list, tableone:::print.TableOne,
                               nonnormal = names(nonormal.variables)[nonormal.variables],
                               noSpaces = TRUE,
                               catDigits = digits,
                               contDigits = digits,
                               showAllLevels = TRUE,
                               printToggle = FALSE)
    ## Combine the formatted tables into one
    raw.table <- do.call(cbind, formatted.tables)
    ## Generate format based on number of digits
    fmt <- paste0("%.", digits, "f") 
    ## If data is imputed, replace counts with count/number of imputed datasets
    if (mi) {
        ns <- as.numeric(raw.table["n", ]) # Get row with n in each strata
        m <- length(unique(study.sample$.imp)) # Get number of imputations
        new.ns <- ns/m # Set new n to the original divided by the number of imputations
        # If the second column include complete data then it should not be replaced
        if (include.complete.data)
            new.ns[2] <- ns[2] 
        raw.table["n", ] <- new.ns # Replace ns with the new numbers
        raw.table.copy <- raw.table # Make a copy of raw.table
        par.index <- grep("\\(", raw.table.copy) # Find index of cells with percentages
        par.data <- raw.table.copy[par.index] # Get those cells
        ## Format cells
        par.fmt <- unlist(lapply(par.data, function(x) {
            numbers <- unlist(strsplit(x, " ")) # Split element on space
            n <- round(as.numeric(numbers[1])) # Get the count as the first element in the numbers vector
            new.n <- sprintf(fmt, n/m) # Divide that number by the number of imputations
            cell <- paste(new.n, numbers[2]) # Paste together to form new cell
            return(cell)
        }))
        raw.table[par.index] <- par.fmt # Replace cells with old counts with new counts
    }
    ## Remove duplicate level columns
    level.indices <- grep("level", colnames(raw.table)) # Find the indices of columns named level
    if (length(level.indices) > 1) raw.table <- raw.table[, -level.indices[2]] # Remove the second level column
    ## Rename level column
    colnames(raw.table)[1] <- "Level"
    ## Modify the first raw.table row with n to also include percentages
    ni <- grep("^n$", rownames(raw.table)) # Get index of row with n
    if (!is.null(group) & !include.complete.data) {
        nnum <- as.numeric(raw.table[ni, ]) # Make numeric
        ps <- round(nnum/nrow(table.data) * 100, digits = digits) # Estimate percentages
        nn <- paste0(nnum, " (", sprintf(fmt, ps), ")") # Format numbers with percentages
        raw.table[ni, ] <- nn # Put back in raw.table
        rownames(raw.table)[ni] <- "n (%)" # Modify name of n row
        raw.table["n (%)", "Level"] <- ""
    }
    ## Move n to column header if only overall data is reported
    if (is.null(group)) {
        n <- raw.table[ni, "Overall"] # Get n
        raw.table <- raw.table[-ni, ] # Remove n row from raw.table
        colnames(raw.table)[2] <- paste0(colnames(raw.table)[2], ", n = ", n)
    }
    ## Move (median [IQR]) to column header if there are only quantitative
    ## variables
    pattern <- "\\(median \\[IQR\\]\\)"
    if (length(grep(pattern, rownames(raw.table))) == nrow(raw.table)) {
        rownames(raw.table) <- gsub(pattern, "", rownames(raw.table))
        colnames(raw.table)[2] <- paste0(colnames(raw.table)[2], " (median [IQR])")
    }
    ## Replace any NA with ""
    raw.table[is.na(raw.table)] <- ""
    ## Add rownames as column
    raw.table <- cbind(rownames(raw.table), raw.table)
    colnames(raw.table)[1] <- "Characteristic"
    rownames(raw.table) <- NULL
    ## Remove level column if empty
    if (all(raw.table[, "Level"] == ""))
        raw.table <- raw.table[, -grep("Level", colnames(raw.table))]
    ## The code below is currently not implemented
    ##
    ## ## Replace variable names with labels
    ## nrns <- orns <- rownames(table) # Get current rownames
    ## abbr <- list() # Genderate vector to hold abbreviations
    ## for (x in variables) {
    ##     vdd <- data_dictionary[[x]] # Get variable specific data dictionary
    ##     l <- vdd$al # Get abbreviated as label
    ##     if (l == "") l <- vdd$l else abbr[[x]] <- paste0(vdd$al, ", ", vdd$l) # If there is no abbreviated label get full label, else store full label to use in explanatory note
    ##     i <- grep(paste0("^", x), rownames(table)) # Get position of old rowname
    ##     nrns[i] <- sub(paste0("^", x), l, rownames(table)[i]) # Put new rowname there
    ## }
    ## table <- cbind(nrns, table) # Add rownames as column
    ## colnames(table)[1] <- "Characteristic" # Name that column
    ## ## Add missing values column
    ## if (include_missing_column) {
    ##     missing_column <- rep("", nrow(table))
    ##     for (variable in variables) {
    ##         missing_variable <- grep(paste0(variable, "_missing"), colnames(full_table.data), value = TRUE)
    ##         missing_entry <- "0 (0)"
    ##         if (length(missing_variable) != 0) {
    ##             missing_data <- full_table.data[, missing_variable]
    ##             n_missing <- sum(missing_data == 0)
    ##             p_missing <- round(100 - mean(missing_data) * 100, digits = digits)
    ##             missing_entry <- paste0(n_missing, " (", p_missing, ")")
    ##         }
    ##         index <- grep(paste0(variable, " "), rownames(table))
    ##         missing_column[index] <- missing_entry
    ##     }
    ##     missing_cols <- full_table.data[, grep("_missing", colnames(full_table.data))]
    ##     total_n_missing <- nrow(full_table.data) - nrow(missing_cols[-unique(which(missing_cols == 0, arr.ind = TRUE)[, 1]), ])
    ##     total_p_missing <- round(total_n_missing/nrow(full_table.data) * 100, digits = digits)
    ##     missing_column[1] <- paste0(total_n_missing, " (", total_p_missing, ")*")
    ##     missing_column <- matrix(missing_column, ncol = 1)
    ##     colnames(missing_column) <- "Missing values, n (%)"
    ##     rownames(missing_column) <- NULL
    ##     rownames(table) <- NULL
    ##     table <- cbind(table, missing_column)
    ## }
    ## rownames(table) <- NULL # Remove rownames
    ## ## Save raw table object
    ## tables <- list(raw = table)
    ## rownames(tables$raw) <- orns # Add old rownames back, for easy access
    ## ## Make abbreviations and explanations text
    ## abbrv <- paste0("Abbreviations and explanations: ", paste0(sort(unlist(abbr)), collapse = "; ")) # Make abbreviation string
    ## ## Format the table using xtable
    ## formatted_table <- print.xtable(xtable(table,
    ##                                        caption = "\\bf Characteristics of the samples analysed in this study",
    ##                                        label = "tab:sample-characteristics"),
    ##                                 type = "latex",
    ##                                 table.placement = "!ht",
    ##                                 include.rownames = FALSE,
    ##                                 include.colnames = TRUE,
    ##                                 caption.placement = "top",
    ##                                 print.results = FALSE)
    ## star_caption <- abbr
    ## if (include_missing_column) star_caption <- paste0("*The total number (\\%) of observations with missing data. ", abbrv)
    ## formatted_table <- add.star.caption(formatted_table, star_caption) # add caption*
    ## Put formatted table in tables
    ## tables$formatted <- formatted_table
    ## Save formatted table to results file
    if (save.to.results) {
        formatted.table <- paste0(kable(raw.table, caption = table.caption, format = "markdown"), collapse = "\n")
        SaveToResults(formatted.table, table.name)
    }
    ## Save formatted table to disk 
    if (save.to.disk) {
        ## Create R markdown code
        table.file <- paste0("```{r echo = FALSE, results = 'asis'} \n",
                             "kable(raw.table, caption = \"", table.caption, "\") \n",
                             "```")
        ## Write to disk
        file.name <- paste0(table.name, ".Rmd")
        write(table.file, file.name)
        ## Render to desired format
        if (file.format != "rmd") {
            output.format.list <- list(docx = "word_document",
                                       pdf = "pdf_document",
                                       html = "html_document")
            rmarkdown::render(file.name,
                              output_format = output.format.list[[file.format]])
            file.remove(file.name)
        }
    }
    ## Return table
    return(raw.table)
}

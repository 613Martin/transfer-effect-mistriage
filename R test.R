## Load required packages
library(data.table)

## Read CSV to testsample
testsample <- fread("../data/simulated-swetrau-data.csv", data.table = FALSE, stringsAsFactors = FALSE)

## Selecting variables

## subtestsample <- testsample[,c("Deceased", "ed_gcs_sum", "ed_sbp_value", "ed_rr_value", "ISS")]
## finalsample <- subtestsample

## The two lines above can be written as:
finalsample <- subtestsample <- testsample[,c("Deceased", "ed_gcs_sum", "ed_sbp_value", "ed_rr_value", "ISS")]

## And you can get a quick summary of your data using str()
str(finalsample)

## DATA CLEANING

## You can define a Replace function, so that you get a feeling for writing
## functions. We generally try to keep functions in separate files, to avoid
## having to read and work with too much code at once. We also try to adhere to
## the principle that one function does one thing and if it does two things then
## it should be split into two separate functions.

#' Replace
#'
#' Replaces values in a given variable
#' @param variable.data A vector. The variable data. No default.
#' @param current.value A vector of length 1. The value to be replaced. No
#'     default.
#' @param replacement.value A vector of length 1. The value to replace
#'     current.value with. Defaults to NA.
Replace <- function(variable.data, current.value, replacement.value = NA) {
    
    if (is.na(current.value)) {
        variable.data[is.na(variable.data)] <- replacement.value    
    } else {
        variable.data[variable.data == current.value] <- replacement.value
    }
    return (variable.data)
}

# Replacing Deceased NA with FALSE
finalsample$Deceased <- Replace(finalsample$Deceased, NA, FALSE)

# Replacing GCS 99 with 3
finalsample$ed_gcs_sum <- Replace(finalsample$ed_gcs_sum, 99, 3)

# Replace GCS 999 with NA
finalsample$ed_gcs_sum <- Replace(finalsample$ed_gcs_sum, 999)

# Implement R mice package
library(mice)

# Handle missing data with mice package using pmm
tempData <- mice(finalsample,m=5,maxit=3,meth='pmm',seed=500)
completedData <- complete(tempData,1)

# Constructing the model in completed data sample
model <- glm(Deceased ~ed_gcs_sum + ed_sbp_value + ed_rr_value,family=binomial(link='logit'),data=completedData)

# Viewing model
Summary(model)

# Calculating "Pseudo R-squared" and its p-value
ll.null <- model$null.deviance/-2
ll.proposed <- model$deviance/-2

# McFadden's Pseudo R^2
(ll.null - ll.proposed) / ll.null

# The p-value for the R^2
1 - pchisq(2*(ll.proposed - ll.null), df=(length(model$coefficients)-1))

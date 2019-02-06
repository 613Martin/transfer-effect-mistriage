# Read CSV to testsample
testsample <- read.csv("C:/Users/Martin/Desktop/R testing/simulated-swetrau-data.csv")

# Selecting variables
subtestsample <- testsample[,c("Deceased", "ed_gcs_sum", "ed_sbp_value", "ed_rr_value", "ISS")]

finalsample <- subtestsample

# DATA CLEANING
# Replacing Deceased NA with FALSE
finalsample$Deceased[is.na(finalsample$Deceased)] <- FALSE

# Replacing GCS 99 with 3
finalsample$ed_gcs_sum[finalsample$ed_gcs_sum == "99"] <- 3

# Replace GCS 999 with NA
finalsample$ed_gcs_sum[finalsample$ed_gcs_sum == "999"] <- NA

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

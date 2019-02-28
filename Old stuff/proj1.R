## Load required packages
library(data.table)
library(mice)
library(Hmisc)

## Source functions
invisible(lapply(list.files("./Functions", full.names = TRUE, pattern = ".R$"), source))

## Read CSV

## I suggest that you move your data to a project folder and use a relative path
## to point to the data. If you work in RStudio you can use setwd() first to
## make sure that you are in the correct working directory. For example:
## setwd("C:/Users/Martin/transfer-effect-mistriage/code/")
## data.path <- "../data/simulated-swetrau-data.csv"
data.path <- "C:/Users/Martin/Desktop/R testing/simulated-swetrau-data.csv"
## Suggest you use . to separate object names, so raw.data instead of rawdata
raw.data <- fread(data.path, data.table = FALSE, stringsAsFactors = FALSE) 

## Selecting variables
selected.data <- raw.data[,c("Sjukhuskod", "ed_gcs_sum", "ed_sbp_value", "ed_rr_value", "res_survival", "ISS", "DateTime_Of_Trauma")]

### INITAL DATA MANAGEMENT AND CLEANING ###

## Replace Survival 999 with NA
selected.data$res_survival <- MyReplace(selected.data$res_survival, 999)

## Replaced Survival NA with 1(=DEAD). 
# NOTE!!! This will not be done in the final version, 
# but since the simulated sample containde no cases with death within 
# 30 days of trauma this was done in order to provide some data to work with.
selected.data$res_survival <- MyReplace(selected.data$res_survival, NA, 1)

## Replace GCS 999 with NA
selected.data$ed_gcs_sum <- MyReplace(selected.data$ed_gcs_sum, 999)

## Replacing GCS 99 with 3
selected.data$ed_gcs_sum <- MyReplace(selected.data$ed_gcs_sum, 99, 3)

## Replace Sjukhuskod with Hospital name
selected.data$Sjukhuskod <- MyReplace(selected.data$Sjukhuskod, 11001, "Karolinska_sjukhuset")
selected.data$Sjukhuskod <- MyReplace(selected.data$Sjukhuskod, 11002, "Huddinge_sjukhus")
selected.data$Sjukhuskod <- MyReplace(selected.data$Sjukhuskod, 12001, "Akademiska_sjukhuset")
selected.data$Sjukhuskod <- MyReplace(selected.data$Sjukhuskod, 22010, "Ryhov_länssjukhus")
selected.data$Sjukhuskod <- MyReplace(selected.data$Sjukhuskod, 41001, "Universitetssjukhuset_Lund")
selected.data$Sjukhuskod <- MyReplace(selected.data$Sjukhuskod, 41012, "Helsingborgs_lasarett")
selected.data$Sjukhuskod <- MyReplace(selected.data$Sjukhuskod, 42010, "Hallands_sjukhus")
selected.data$Sjukhuskod <- MyReplace(selected.data$Sjukhuskod, 51001, "Sahlgrenska_universitetssjukhuset")
selected.data$Sjukhuskod <- MyReplace(selected.data$Sjukhuskod, 51013, "NU_sjukvården")

## Replace column name Sjukhuskod with Sjukhus
names(selected.data)[names(selected.data) == 'Sjukhuskod'] <- 'Sjukhus'

## Convert SBP and RR to restrictec cubic splines
## I´m not sure how to use rcspline.eval(x) properly.
## The times i´ve tried it wont turn out right...

## rcspline.eval returns a matrix. The number of rows is the same as in the
## original data and the number of columns is equal to nk - 2. These two are the
## spline basis functions, and together with the original variable they form the
## spline transformation.
nk <- 4
sbp.splines <- rcspline.eval(selected.data$ed_sbp_value, nk = nk)
## Remember that you need to save the know locations, so that you can use the
## same locations when you generate spline basis functions in the validation
## data later.
sbp.knot.locations <- attr(sbp.splines, "knots")
## There are several ways to put the spline basis functions into the data. You
## could allocate to columns in selected.data and then put the data there, or
## you can make the matrix into a data frame and bind it to selected.data using
## cbind.
sbp.splines <- as.data.frame(sbp.splines)
colnames(sbp.splines) <- paste0("ed_sbp_value_spline_", 1:(nk - 2))
selected.data <- cbind(selected.data, sbp.splines)

### MISSING DATA ###

## You want to first create the datasets, i.e. high vs low volume, metropolitan
## vs non-metropolitan etc and then impute. Otherwise the datasets will pollute
## one another. But we can discuss that more when we get to that stage.

## Manage missing data

temporary.data <- mice(selected.data,m=5,maxit=3,meth='pmm',seed=500)

# Combine all imputations in one dataframe including original data
all.imputed.data <- complete(temporary.data, action = "long", include = TRUE)

# Combine all imputations into one dataframe excluding the original data
# Which is the dataframe used henceforth (not sure if this is the correct way to do it?)
completed.data <- complete(temporary.data, action = "long", include = FALSE)


### DATA SETS AND SAMPLES ###

### HIGH VOLUME SAMPLE ###

## Create high volume sample of top quartile (Which in this simulated sample is acually the top 1/3)
high_volume_centre <- subset.data.frame(completedData, Sjukhus == "NU_sjukvården" | Sjukhus =="Karolinska_sjukhuset" | Sjukhus == "Hallands_sjukhus")

## Sort high volume sample by date of trauma
high_volume_centre <- high_volume_centre[order(high_volume_centre$DateTime_Of_Trauma, decreasing = TRUE), ]

## Pick the top rows until number of dead = 70
# Need to create a function or loop checking each row from the top in high_volume_centre until the number of dead = 70.
# That will consititute the high_volume_centre_dev_sample.


### LOW VOLUME SAMPLE ###

## Create low volume sample
low_volume_centre <- subset.data.frame(completedData, Sjukhus != "NU_sjukvården" & Sjukhus !="Karolinska_sjukhuset" & Sjukhus != "Hallands_sjukhus")

## Sort low volume sample by date of trauma
low_volume_centre <- low_volume_centre[order(high_volume_centre$DateTime_Of_Trauma, decreasing = TRUE), ]

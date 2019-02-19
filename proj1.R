## Load required packages
library(data.table)
library(mice)
library(Hmisc)

## Read CSV
rawdata <- fread("C:/Users/Martin/Desktop/R testing/simulated-swetrau-data.csv", data.table = FALSE, stringsAsFactors = FALSE)

## Selecting variables
selecteddata <- rawdata[,c("Sjukhuskod", "ed_gcs_sum", "ed_sbp_value", "ed_rr_value", "res_survival", "ISS", "DateTime_Of_Trauma")]


### INITAL DATA MANAGEMENT AND CLEANING ###

## Replace Survival 999 with NA
selecteddata$res_survival <- My_Replace(selecteddata$res_survival, 999)

## Replaced Survival NA with 1(=DEAD). 
# NOTE!!! This will not be done in the final version, 
# but since the simulated sample containde no cases with death within 
# 30 days of trauma this was done in order to provide some data to work with.
selecteddata$res_survival <- My_Replace(selecteddata$res_survival, NA, 1)

## Replace GCS 999 with NA
selecteddata$ed_gcs_sum <- My_Replace(selecteddata$ed_gcs_sum, 999)

## Replacing GCS 99 with 3
selecteddata$ed_gcs_sum <- My_Replace(selecteddata$ed_gcs_sum, 99, 3)

## Replace Sjukhuskod with Hospital name
selecteddata$Sjukhuskod <- My_Replace(selecteddata$Sjukhuskod, 11001, "Karolinska_sjukhuset")
selecteddata$Sjukhuskod <- My_Replace(selecteddata$Sjukhuskod, 11002, "Huddinge_sjukhus")
selecteddata$Sjukhuskod <- My_Replace(selecteddata$Sjukhuskod, 12001, "Akademiska_sjukhuset")
selecteddata$Sjukhuskod <- My_Replace(selecteddata$Sjukhuskod, 22010, "Ryhov_länssjukhus")
selecteddata$Sjukhuskod <- My_Replace(selecteddata$Sjukhuskod, 41001, "Universitetssjukhuset_Lund")
selecteddata$Sjukhuskod <- My_Replace(selecteddata$Sjukhuskod, 41012, "Helsingborgs_lasarett")
selecteddata$Sjukhuskod <- My_Replace(selecteddata$Sjukhuskod, 42010, "Hallands_sjukhus")
selecteddata$Sjukhuskod <- My_Replace(selecteddata$Sjukhuskod, 51001, "Sahlgrenska_universitetssjukhuset")
selecteddata$Sjukhuskod <- My_Replace(selecteddata$Sjukhuskod, 51013, "NU_sjukvården")

## Replace column name Sjukhuskod with Sjukhus
names(selecteddata)[names(selecteddata) == 'Sjukhuskod'] <- 'Sjukhus'

## Convert SBP and RR to restrictec cubic splines
# I´m not sure how to use rcspline.eval(x) properly.
# The times i´ve tried it wont turn out right...


### MISSING DATA ###

# Manage missing data
tempData <- mice(selecteddata,m=5,maxit=3,meth='pmm',seed=500)

# Combine all imputations in one dataframe including original data
all_imp_data <- complete(tempData, action = "long", include = TRUE)

# Combine all imputations into one dataframe excluding the original data
# Which is the dataframe used henceforth (not sure if this is the correct way to do it?)
completedData <- complete(tempData, action = "long", include = FALSE)


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

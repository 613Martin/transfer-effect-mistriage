## Effects of transfer on mistriage study main R code file 
## 
## This file was created using RStudio 1.1.463
MainCodeRun <- function() {
  
    ## INITIALIZING
    ## Load required packages and source functions
    ## FuncPack() first needs to be sourced to run
    source("./functions/FuncPack.R")
    FuncPack()
    ## Set random seed
    set.seed(-41892)
    ## Create Results enviroment
    Results <- new.env()  
    ## Import data
    raw.data <- ImportStudyData("simulated-swetrau-data.csv")
    ## Create study sample from selected variables
    selected.data <- VariableSelection(raw.data)
    ## Select cases with age > 15 or age = NA
    selected.data <- InclusionSelection(selected.data)
    ## Data Cleaning
    selected.data <- DataCleaning(selected.data)
  
    ## DATA SETS AND SAMPLES
    ## Mark entries as High Volume or Low Volume
    selected.data.vol.mark <- HighVolumeCheck(selected.data)
    ## Create High Volume Sample and Low Volume Sample
    High.Volume.Sample <- selected.data.vol.mark[selected.data.vol.mark$High_Volume_Centre == "Yes", ]
    Low.Volume.Sample <- selected.data.vol.mark[selected.data.vol.mark$High_Volume_Centre == "No", ]
    rm(selected.data.vol.mark)
    ## Mark entries as Metropolitan or Non-Metropolitan
    selected.data.metro.mark <- Metrocheck(selected.data)
    ## Create Metropolitan Sample and Non-Metropolitan Sample
    Metropolitan.Sample <- selected.data.metro.mark[selected.data.metro.mark$metropolitan == "Yes", ]
    Non.Metropolitan.Sample <- selected.data.metro.mark[selected.data.metro.mark$metropolitan == "No", ]
    rm(selected.data.metro.mark)
    ## Mark entries as valid (>170 events) individual centres
    selected.data.ind.mark <- IndividualCentreCheck(selected.data)
    ## Create Multi Centre Sample (All data)
    Multi.Centre.Sample <- selected.data.ind.mark
    ## Create Single Centre Samples (valid individual centres)
    centre.ids <- unique(selected.data.ind.mark$Sjukhuskod) # Identify unique IDs
    centre.ids <- setNames(centre.ids, nm = paste0("single.centre.", centre.ids)) # Name IDs
    Single.Centre.Samples <- lapply(centre.ids, SelectSingleCentre, df = selected.data.ind.mark)
    Single.Centre.Samples <- Single.Centre.Samples[-which(sapply(Single.Centre.Samples, is.null))]
    rm(selected.data.ind.mark) 

    ## CREATE DATA SETS LIST  
    ## Add samples to list
    data.sets <- list(high.volume.vs.low.volume = list(high.volume = High.Volume.Sample,
                                                       low.volume = Low.Volume.Sample),
                      metropolitan.vs.non.metropolitan = list(metropolitan = Metropolitan.Sample,
                                                              non.metropolitan = Non.Metropolitan.Sample),
                      multi.centre.vs.single.centre = c(list(multi.centre = Multi.Centre.Sample),
                                                        Single.Centre.Samples))
    
    ## RESTRICTED CUBIC SPLINES AND MISSING DATA
    ## Create restricted cubic splines
    data.sets <- lapply(data.sets, function(sample) lapply(sample, RCSplineConvert))
    ## Extract missing data information from each sample
    NA.info.sample <- lapply(data.sets, function(sample) lapply(sample, NACounterDataSet))
    ## Extraxt missing data information from each variable in each sample
    NA.info.variable <- lapply(data.sets, function(sample) lapply(sample, NACounterVariable))                 
    ## Save information to Results enviroment
    Results$data.sets.before.imputations <- data.sets
    Results$NA.info.sample <- NA.info.sample
    Results$NA.info.variable <- NA.info.variable
    rm(NA.info.sample, NA.info.variable)                                              
    ## Impute missing data
    data.sets <- lapply(data.sets, MICEImplement)
    Results$data.sets.after.imputations <- data.sets                    
    
    ## TABLE ONE CREATION                  
    ## Create sample characteristics tables, and save to disk
    TableOneCreator(data.sets)

    ## DEVELOPMENT AND VALIDATION
    ## Create Development and validation sample, for each sample
    data.sets <- lapply(data.sets, function(sample) lapply(sample, DevValCreator))
    Results$data.sets.after.imputations.dev.val <- data.sets
    
    ## CLINICAL PREDICTION MODEL
    ## Model development
                        
    ## To set all splines to NULL you can also do
    df <- data.sets$high.volume.vs.low.volume$high.volume$Development
    df[, grep("^[a-z_]*_spline_[0-9]*$", colnames(df))] <- NULL
    
    ## Recreate RCS
    df <- RCSplineConvert(df)

    ## Create model function
    log.reg.model <- function(model.data) {
        glm(res_survival ~ ed_gcs_sum + 
                ed_sbp_value + 
                ed_rr_value + 
                ed_sbp_value_spline_1 +
                ed_sbp_value_spline_2 +
                ed_rr_value_spline_1,
            data = model.data, 
            family = "binomial")
    }
    
    ## Create development model
    development.model <- coef(log.reg.model(df))
    
    ## Estimate linear shrinkage factor
    get.prediction.slope <- function(original.data, indices) {  
        model.data <- original.data[indices,]  
        model.fit <- log.reg.model(model.data)
        prediction <- predict(model.fit, newdata = original.data)
        calibration.model <- glm(original.data$res_survival ~ prediction, family = "binomial")
        slope <- coef(calibration.model)["prediction"]
        return(slope)  
    }
    linear.shrinkage.factor <- mean(boot(  
      data = df, 
      statistic = get.prediction.slope, 
      R = 1000
    )$t)
    
    ## Apply bootstrap results to shrink model coefficients
    shrunk.development.model <- development.model * linear.shrinkage.factor

    ## Predict 30-day mortality in development sample
    # Create dummy model (as predict function only accepts glm. and not "pure" coefficients)
    dummy.model <- glm(res_survival ~ ed_gcs_sum + 
                   ed_sbp_value + 
                   ed_rr_value + 
                   ed_sbp_value_spline_1 +
                   ed_sbp_value_spline_2 +
                   ed_rr_value_spline_1,
                   data = df, 
                   family = "binomial")
    #Apply shrunk coefficients to dummy.model
    dummy.model$coefficients <- shrunk.development.model
  
    #Caldulate product and sum of coefficiants in all entries
    sum.coef <- predict(dummy.model, newdata=df)
  
    # Calculates probability of event in each entry
    prob <- exp(sum.coef)/(1+exp(sum.coef))
    
    ## Create grid from probabilities and ISS dichotomized
    grid <- data.frame(cbind(prob, df$ISS_over_15))
    names(grid) <- c("probs", "ISS_over_15")
    ## Create list of probabilities
    data.sets$high.volume.vs.low.volume$high.volume$probs <- as.list(unique(prob))
    ## Search for, and select the optimal cutoff
    optimal.cutoff <- FindCutOff(prob.list = data.sets$high.volume.vs.low.volume$high.volume$probs, grid = grid)
                                    
    ## Model Validation, mistriage rate in high volume validation
    mistriage.rate.high.vol.val <-  CalculateMistriage(data = data.sets$high.volume.vs.low.volume$high.volume$Validation, model = shrunk.development.model, cutoff = Optimal.cutoff)

    ## Model comparison, obtain mistriage rate in low volume validation
    ## Remove and recreate RCS
    data.sets$high.volume.vs.low.volume$low.volume$Validation [, grep("^[a-z_]*_spline_[0-9]*$", colnames(data.sets$high.volume.vs.low.volume$low.volume$Validation))] <- NULL
    data.sets$high.volume.vs.low.volume$low.volume$Validation <- RCSplineConvert(data.sets$high.volume.vs.low.volume$low.volume$Validation)
    ## Obtain mistriage rate
    mistriage.rate.low.vol.val <-  CalculateMistriage(data = data.sets$high.volume.vs.low.volume$low.volume$Validation, model = shrunk.development.model, cutoff = Optimal.cutoff)
  
    ## COMPILE RESULTS
    model.comparison.results <- list()
    model.comparison.results$high.vol.model.in$high.vol <- mistriage.rate.high.vol.val
    model.comparison.results$high.vol.model.in$low.vol <- mistriage.rate.low.vol.val
      
}

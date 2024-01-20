getwd()

# libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)
library(gridExtra)
library(rio)
library(broom.mixed)
library(lme4)
library(lmerTest)

# loading data 
sym <- readRDS("dat_symbolic_multiverse.rds")
op <- readRDS("operational_for_multi.rds")


# symbolic models no interactions
results_s <- list()
predictors <- list(c("covid", "wave"), c("unemployment", "wave"), c("immigration", "wave"), 
                   c("homicide", "wave"), c("covid", "wave","o", "c", "e", "a", "n"), 
                   c("unemployment", "wave","o", "c", "e", "a", "n"), 
                   c("immigration", "wave","o", "c", "e", "a", "n"), 
                   c("homicide", "wave","o", "c", "e", "a", "n"), 
                   c("covid", "wave","o", "c"), c("unemployment", "wave","o", "c"), 
                   c("immigration", "wave","o", "c"), c("homicide", "wave","o", "c"), 
                   c("covid", "wave","o"), c("unemployment", "wave","o"), 
                   c("immigration", "wave","o"), c("homicide", "wave","o"), 
                   c("covid", "wave","c"), c("unemployment", "wave","c"), 
                   c("immigration", "wave","c"), c("homicide", "wave","c"), 
                   c("covid", "immigration", "homicide", "wave", "unemployment", "o", 
                     "c", "e", "a", "n"))


for(i in 1:length(predictors)){
  name <- paste0(paste(predictors[[i]], collapse = "_"), "-", "symbolic")
  
  mod <-  lmer(ideology ~ . -Respondent_ID + (wave|Respondent_ID), 
               data = sym[,c(predictors[[i]], "ideology", "Respondent_ID")], 
               control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  results_s[[name]] <- mod
}

# now fitting models for operaational items 
DVs <- c("income", "immigrant_culture", "eu", 
          "mothers_work", "fathers_work", "immigration_ability",
          "marriage", "moms_working", "unions", "gender_kids")


op_results <- list()
for(i in 1:length(DVs)){
  for(j in 1:length(predictors)){
    
    name <- paste0(paste(predictors[[j]], collapse = "_"), "-", DVs[[i]])
    
    form <- as.formula(paste(DVs[i], "~", paste(predictors[[j]], collapse = " + "), "+ (wave|Respondent_ID)"))
    
    mod <-  lmer(form, 
                 data = op[,c(predictors[[j]], DVs[i], "Respondent_ID")], 
                 control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
    
    op_results[[name]] <- mod
  }
}


names(op_results)

########### now for interaction models ###################
predictors_i <- list(c("o", "c", "e", "a", "n", "wave", "covid" ,"wave:o", "covid:o"), 
                     c("o", "c", "e", "a", "n", "wave", "immigration", "wave:o", "immigration:o"), 
                     c("o", "c", "e", "a", "n", "wave", "homicide" ,"wave:o", "homicide:o"), 
                     c("o", "c", "e", "a", "n", "wave", "unemployment" ,"wave:o", "unemployment:o"), 
                     c("o", "c", "e", "a", "n", "wave", "covid" ,"wave:c", "covid:c"), 
                     c("o", "c", "e", "a", "n", "wave", "immigration" ,"wave:c", "immigration:c"), 
                     c("o", "c", "e", "a", "n", "wave", "homicide" ,"wave:c", "homicide:c"), 
                     c("o", "c", "e", "a", "n", "wave", "unemployment", "wave:c", "unemployment:c"), 
                     c("o", "wave", "covid", "wave:o", "covid:o"), c("o", "wave", "immigration", "wave:o", "immigration:o"), 
                     c("o", "wave", "homicide" ,"wave:o", "homicide:o"), c("o", "wave", "unemployment","wave:o", "unemployment:o"), 
                     c("c", "wave", "covid" ,"wave:c", "covid:c"),c("c", "wave", "immigration" ,"wave:c", "immigration:c"), 
                     c("c", "wave", "homicide" ,"wave:c", "homicide:c"), c("c", "wave", "unemployment","wave:c", "unemployment:c"), 
                     c("o", "c", "e", "a", "n", "wave", "covid","wave:o", "covid:o", "wave:c", "covid:c"), 
                     c("o", "c", "e", "a", "n", "wave", "immigration","wave:o", "immigration:o", "wave:c", "immigration:c"), 
                     c("o", "c", "e", "a", "n", "wave", "homicide" ,"wave:o", "homicide:o", "wave:c", "homicide:c"), 
                     c("o", "c", "e", "a", "n", "wave", "unemployment" ,"wave:o", "unemployment:o", "wave:c", "unemployment:c"))


#### symbolic interaction models 
results_s_interactions <- list()
for(i in 1:length(predictors_i)){
  name <- paste0(paste(predictors_i[[i]], collapse = "_"), "-", "symbolic")
  
  form <- as.formula(paste("ideology", "~", paste(predictors_i[[i]], collapse = " + "), "+ (wave|Respondent_ID)"))
  
  mod <-  lmer(form, 
               data = sym, 
               control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  
  results_s_interactions[[name]] <- mod
  
}

###### operational interaction models 
op_results_int <- list()
for(i in 1:length(DVs)){
  for(j in 1:length(predictors_i)){
    
    name <- paste0(paste(predictors_i[[j]], collapse = "_"), "-", DVs[[i]])
    
    form <- as.formula(paste(DVs[i], "~", paste(predictors_i[[j]], collapse = " + "), "+ (wave|Respondent_ID)"))
    
    mod <-  lmer(form, 
                 data = op, 
                 control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
    
    op_results_int[[name]] <- mod
  }
}

######## saving results for tidying script #########
saveRDS(op_results_int, "op_results_int.rds")
saveRDS(results_s_interactions, "results_s_int.rds")
saveRDS(op_results, "op_results.rds")
saveRDS(results_s, "results_s.rds")

rm(list = ls())

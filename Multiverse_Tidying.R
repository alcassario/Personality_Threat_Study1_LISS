getwd()

# libraries
library(tidyverse)
library(broom.mixed)

##### reading in rds #######
op_results_int <- readRDS("op_results_int.rds")
results_s_int <- readRDS("results_s_int.rds")
op_results <- readRDS("op_results.rds")
results_s <- readRDS("results_s.rds")

##### saving into tidied dataframes
t_op_results_int <- list()
for(i in 1:length(op_results_int)){
  t_op_results_int[[i]] <- tidy(op_results_int[[i]], effects = "fixed", conf.int = TRUE)
}

t_results_s_int <- list()
for(i in 1:length(results_s_int)){
  t_results_s_int[[i]] <- tidy(results_s_int[[i]], effects = "fixed", conf.int = TRUE)
}

t_op_results <- list()
for(i in 1:length(op_results)){
  t_op_results[[i]] <- tidy(op_results[[i]], effects = "fixed", conf.int = TRUE)
}

t_results_s <- list()
for(i in 1:length(results_s)){
  t_results_s[[i]] <- tidy(results_s[[i]], effects = "fixed", conf.int = TRUE)
}

##### adding name of model for the tidied dataframe ####
for(i in 1:length(op_results)){
  t_op_results[[i]]$spec <- names(op_results[i])
}

for(i in 1:length(op_results_int)){
  t_op_results_int[[i]]$spec <- names(op_results_int[i])
}

for(i in 1:length(results_s)){
  t_results_s[[i]]$spec <- names(results_s[i])
}

for(i in 1:length(results_s_int)){
  t_results_s_int[[i]]$spec <- names(results_s_int[i])
}

# specifying model number 
for(i in 1:length(op_results)){
  t_op_results[[i]]$Mod_Number <- i 
}

for(i in 1:length(results_s)){ 
  t_results_s[[i]]$Mod_Number <- 210 + i
}

for(i in 1:length(op_results_int)){
  t_op_results_int[[i]]$Mod_Number <- i
}

for(i in 1:length(results_s_int)){
  t_results_s_int[[i]]$Mod_Number <- 200 + i 
}



# for models with interaction terms, conceptually with our theory 
# threat is predictor, personality is moderator 
for(i in 1:length(t_results_s_int)){
  t_results_s_int[[i]]$Threat <- str_split_i(t_results_s_int[[i]]$term, ":", 2)
}

for(i in 1:length(t_op_results_int)){
  t_op_results_int[[i]]$Threat <- str_split_i(t_op_results_int[[i]]$term, ":", 2)
}

# personality is moderator 
for(i in 1:length(t_results_s_int)){
  t_results_s_int[[i]]$Moderator <- str_split_i(t_results_s_int[[i]]$term, ":", 1)
}

for(i in 1:length(t_op_results_int)){
  t_op_results_int[[i]]$Moderator <- str_split_i(t_op_results_int[[i]]$term, ":", 1)
}

# if not an interaction term, return na for moderator 
for(i in 1:length(t_results_s_int)){
  t_results_s_int[[i]]$Moderator <- ifelse(is.na(t_results_s_int[[i]]$Threat) == TRUE, NA, t_results_s_int[[i]]$Moderator) 
}

View(t_results_s_int[[1]])

for(i in 1:length(t_op_results_int)){
  t_op_results_int[[i]]$Moderator <- ifelse(is.na(t_op_results_int[[i]]$Threat) == TRUE, NA, t_op_results_int[[i]]$Moderator) 
}

###### Pulling dependent variable from specification character string ####
for(i in 1:length(t_results_s_int)){
  t_results_s_int[[i]]$Outcome <- str_split_i(t_results_s_int[[i]]$spec, "-", 2)
}

for(i in 1:length(t_op_results_int)){
  t_op_results_int[[i]]$Outcome <- str_split_i(t_op_results_int[[i]]$spec, "-", 2)
}

for(i in 1:length(t_results_s)){
  t_results_s[[i]]$Outcome <- str_split_i(t_results_s[[i]]$spec, "-", 2)
}

for(i in 1:length(t_op_results)){
  t_op_results[[i]]$Outcome <- str_split_i(t_op_results[[i]]$spec, "-", 2)
}

## for interaction dataframes, code whether coefficient is an interaction term ##
for(i in 1:length(t_results_s_int)){
  t_results_s_int[[i]]$grouping <- ifelse(is.na(t_results_s_int[[i]]$Moderator) == FALSE, "Interaction Estimate", "Not")
}


for(i in 1:length(t_op_results_int)){
  t_op_results_int[[i]]$grouping <- ifelse(is.na(t_op_results_int[[i]]$Moderator) == FALSE, "Interaction Estimate", "Not")
}

## for main effects, code term for main effect aesthetic ####
for(i in 1:length(t_results_s)){
  t_results_s[[i]]$grouping <- "Main Effect"
}


for(i in 1:length(t_op_results)){
  t_op_results[[i]]$grouping <- "Main Effect"
}


## code for statistical signifance, p < .05 ## 
for(i in 1:length(t_results_s_int)){
  t_results_s_int[[i]]$Significance <- ifelse(t_results_s_int[[i]]$p.value < .05, 1, 0)
}


for(i in 1:length(t_op_results_int)){
  t_op_results_int[[i]]$Significance <- ifelse(t_op_results_int[[i]]$p.value < .05, 1, 0)
}

for(i in 1:length(t_results_s)){
  t_results_s[[i]]$Significance <- ifelse(t_results_s[[i]]$p.value < .05, 1, 0)
}

for(i in 1:length(t_op_results)){
  t_op_results[[i]]$Significance <- ifelse(t_op_results[[i]]$p.value < .05, 1, 0)
}


## rbinding main effect models into one tidy df ### 
mains <- data.frame()
for(i in 1:length(t_results_s)){
  mains <- rbind(mains, t_results_s[[i]])
}

for(i in 1:length(t_op_results)){
  mains <- rbind(mains, t_op_results[[i]])
}

# remove intercept terms, we don't want to plot these # 
mains <- subset(mains, mains$term != "(Intercept)")
mains <- mains %>% dplyr::arrange(Mod_Number)

# save main effects df for multiverse plot script 
saveRDS(mains, "mains_for_multiplot.rds")

## now for interaction terms ##
interactions <- data.frame()
for(i in 1:length(t_results_s_int)){
  interactions <- rbind(interactions, t_results_s_int[[i]])
}

for(i in 1:length(t_op_results_int)){
  interactions <- rbind(interactions, t_op_results_int[[i]])
}

interactions <- subset(interactions, interactions$grouping == "Interaction Estimate")

# save for multiverse plot code # 
saveRDS(interactions, "interactions_for_multiplot.rds")

max(interactions$Mod_Number) # 220 
max(mains$Mod_Number) # 231 

rm(list = ls())


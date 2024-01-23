getwd()

library(dplyr)
library(broom.mixed)
library(ggplot2)
library(tidyverse)

### loading lists of results
load("OperationalResultsNoInteractions_2_wave.RData")
load("OperationalResults_2_wave.RData")
load("results_symbolic_2_wave.RData") 

### broom.mixed tidying
summary(results_1$fathers_work)

### tidy no interactions
results_1_tidy <- list()
for(i in 1:length(results_1)){
   results_1_tidy[[i]] <- tidy(results_1[[i]], effects = "fixed", conf.int = TRUE)
}

symbolic <- tidy(results_symbolic[[1]], effects = "fixed", conf.int = TRUE)
results_1_tidy[[11]] <- symbolic
names(results_1_tidy) <- c(names(results_1), "symbolic") 

### tidy with interactions 
results_tidy <- list()
for(i in 1:length(results)){
  results_tidy[[i]] <- tidy(results[[i]], effects = "fixed", conf.int = TRUE)
}

symbolic_int <- tidy(results_symbolic[[2]], effects = "fixed", conf.int = TRUE) 
results_tidy[[11]] <- symbolic_int
names(results_tidy) <- c(names(results), "symbolic")

#### making ggplots ####

#### renaming variables ####
for(i in 1:length(results_1_tidy)){
  results_1_tidy[[i]] <- results_1_tidy[[i]] %>% mutate(term = case_when(term == "(Intercept)"~ "Intercept", 
                                                                         term == "wave" ~ "Wave", 
                                                                         term == "homicide" ~ "Homicide", 
                                                                         term == "immigration" ~ "Immigration", 
                                                                         term == "covid" ~ "COVID", 
                                                                         term == "o" ~ "Openness", 
                                                                         term == "c" ~ "Conscientiousness", 
                                                                         term == "e" ~ "Extraversion", 
                                                                         term == "a" ~ "Agreeableness", 
                                                                         term == "n" ~ "Neuroticism", 
                                                                         term == "unemployment" ~ "Unemployment"))
}



for(i in 1:length(results_tidy)){
  results_tidy[[i]] <- results_tidy[[i]] %>% mutate(term = case_when(term == "(Intercept)"~ "Intercept", 
                                                                       term == "wave" ~ "Wave", 
                                                                       term == "homicide" ~ "Homicide", 
                                                                       term == "immigration" ~ "Immigration", 
                                                                       term == "covid" ~ "COVID", 
                                                                       term == "o" ~ "Openness", 
                                                                       term == "c" ~ "Conscientiousness", 
                                                                       term == "e" ~ "Extraversion", 
                                                                       term == "a" ~ "Agreeableness", 
                                                                       term == "n" ~ "Neuroticism", 
                                                                       term == "unemployment" ~ "Unemployment", 
                                                                       term == "wave:o" ~ "Wave:Open", 
                                                                       term == "wave:c" ~ "Wave:Conscientious", 
                                                                       term == "homicide:o" ~ "Homicide:Open", 
                                                                       term == "homicide:c" ~ "Homicide:Conscientious", 
                                                                       term == "immigration:o" ~ "Immigration:Open", 
                                                                       term == "immigration:c" ~ "Immigration:Conscientious", 
                                                                       term == "covid:o" ~ "COVID:Open", 
                                                                       term == "covid:c" ~ "COVID:Conscientious", 
                                                                       term == "unemployment:o" ~ "Unemployment:Open", 
                                                                       term == "unemployment:c" ~ "Unemployment:Conscientious"))
}

##### selecting the terms for plotting from each set of models ### 
for(i in 1:length(results_1_tidy)){
  results_1_tidy[[i]] <- results_1_tidy[[i]] %>% filter(term == "Immigration" | term == "Homicide" | term == "COVID" | 
                               term == "Unemployment")
}



for(i in 1:length(results_tidy)) { 
  results_tidy[[i]] <- results_tidy[[i]] %>% filter(term == "Homicide:Open" | term == "Homicide:Conscientious" | 
                                                    term == "Immigration:Open"| term == "Immigration:Conscientious"| 
                                                    term == "COVID:Open"| term == "COVID:Conscientious"| 
                                                    term == "Unemployment:Open" | term == "Unemployment:Conscientious")
}

##### Names of models ###### 
names(results_tidy) <- c("Income Inequality", "Immigrant Culture", "EU", 
                         "Mother's Work", "Father's Work", 
                         "Immigration Ability", "Marriage", 
                         "Moms of Young Work", 
                         "Union Support", "Gender Childrearing", "Symbolic Ideology")

names(results_1_tidy) <- c("Income Inequality", "Immigrant Culture", "EU", 
                           "Mother's Work", "Father's Work", 
                           "Immigration Ability", "Marriage", 
                           "Moms of Young Work", 
                           "Union Support", "Gender Childrearing", "Symbolic Ideology")

##### combining df's for plotting #####
for(i in 1:length(results_tidy)){
  results_tidy[[i]]$Model <-  names(results_tidy)[i]
}

plot_dat_interactions <- rbind(results_tidy[[1]], results_tidy[[2]], results_tidy[[3]], 
                               results_tidy[[4]], results_tidy[[5]], results_tidy[[6]], 
                               results_tidy[[7]], results_tidy[[8]], results_tidy[[9]], 
                               results_tidy[[10]], results_tidy[[11]])

for(i in 1:length(results_1_tidy)){
  results_1_tidy[[i]]$Model <-  names(results_1_tidy)[i]
}

plot_dat_no_int <- rbind(results_1_tidy[[1]], results_1_tidy[[2]], results_1_tidy[[3]], 
                               results_1_tidy[[4]], results_1_tidy[[5]], results_1_tidy[[6]], 
                               results_1_tidy[[7]], results_1_tidy[[8]], results_1_tidy[[9]], 
                               results_1_tidy[[10]], results_1_tidy[[11]])

##### adding things we want to map aesthetics for #######
sig <- function(x) {
  ifelse(x < .05, "Significant", "Non-significant")
}

plot_dat_no_int$Significance <- sig(plot_dat_no_int$p.value)

lib_con <- function(x) {
  ifelse(x < 0, "Liberal", 
         ifelse(x > 0, "Conservative", NA))
}


plot_dat_no_int$Direction <- lib_con(plot_dat_no_int$estimate)

### decided to map to theories rather direction and significance for interaction 
Theory <- function(x, y, z) { 
   ifelse(str_detect(x, "Open") == TRUE & y > 0 & z < .05, "Threat Constraint", 
          ifelse(str_detect(x, "Open") == TRUE & y < 0 & z < .05 , "Neg. Bias", 
                 ifelse(str_detect(x, "Conscientious") == TRUE & y > 0 & z < .05, "Neg. Bias", "Neither")))
} 

plot_dat_interactions$Theory <- Theory(plot_dat_interactions$term, plot_dat_interactions$estimate, plot_dat_interactions$p.value)

#### relevel model facet here so that symbolic ideology is last (Want it easy to pick out)
plot_dat_no_int <- plot_dat_no_int %>% mutate(Model = factor(Model)) %>%
                      mutate(Model=fct_relevel(Model, c("Income Inequality", "Immigrant Culture", "EU", "Mother's Work",      
                      "Father's Work", "Immigration Ability", "Marriage", "Moms of Young Work", 
                      "Union Support" ,"Gender Childrearing", "Symbolic Ideology")))

plot_dat_interactions <- plot_dat_interactions %>% mutate(Model = factor(Model)) %>%
  mutate(Model=fct_relevel(Model, c("Income Inequality", "Immigrant Culture", "EU", "Mother's Work",      
                                    "Father's Work", "Immigration Ability", "Marriage", "Moms of Young Work", 
                                    "Union Support" ,"Gender Childrearing", "Symbolic Ideology")))

plot_dat_interactions <- plot_dat_interactions %>% mutate(Theory = factor(Theory)) %>%
  mutate(Theory=fct_relevel(Theory, c("Neg. Bias", "Threat Constraint", "Neither")))
#### plotting models no interactions ####### 
plot1 <- ggplot(plot_dat_no_int, aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange(aes(color = Direction, shape = Significance), position = position_dodge(width = .2)) +
  facet_wrap(~Model) + geom_vline(xintercept = 0, lty = 3) + 
   scale_colour_manual(values = c("#ca0020", "#0571b0")) + 
  theme_classic()

plot1

#### Interaction ########################
plot2 <- ggplot(plot_dat_interactions, aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange(aes(color = Theory), position = position_dodge(width = .2)) +
  scale_x_continuous(limits = c(-.5, .9)) +
  facet_wrap(~Model) + geom_vline(xintercept = 0, lty = 3) + 
  scale_colour_manual(values = c("#E69F00", "#009E73", "#000000")) + 
  theme_classic() 
plot2



### N terms consistent with each framework personality interactions  ### 
open <- subset(plot_dat_interactions, plot_dat_interactions$term %>% stringr::str_detect("Open") == TRUE)

### n sig and consistent with neg bias (pos and sig o by threat)
sum(open$estimate > 0 & open$p.value < .05) # 5 threat constraint out of 44
sum(open$estimate < 0 & open$p.value < .05) # 2 neg bias out of 44


### same proc for cons
cons <- subset(plot_dat_interactions, plot_dat_interactions$term %>% stringr::str_detect("Conscientious") == TRUE)

sum(cons$estimate > 0 & cons$p.value < .05) # 4 neg bias out of 44

### neg bias, neg and sig o and threat, pos and sig c and threat
range(plot_dat_no_int$estimate)
sum(open$estimate < 0 & open$p.value < .05) # 2
sum(cons$estimate > 0 & open$p.value < .05) # 1
# 3/88 consistent with neg bias 



### now for direction of main effects 
sum(plot_dat_no_int$estimate > 0 & plot_dat_no_int$p.value < .05) # 21 sig and cons 
sum(plot_dat_no_int$estimate < 0 & plot_dat_no_int$p.value < .05) # 9 sig liberal 
sum(plot_dat_no_int$p.value > .05) # 14 non sig 

### mean lib and con shifts 
mean(plot_dat_no_int$estimate[plot_dat_no_int$estimate > 0 & plot_dat_no_int$p.value < .05]) # .03 cons
mean(plot_dat_no_int$estimate[plot_dat_no_int$estimate < 0 & plot_dat_no_int$p.value < .05]) # -.02 lib 
mean(plot_dat_no_int$estimate) # mean shift .007

#### arrange results 
plot_dat_no_int <- plot_dat_no_int %>% arrange(estimate)

rm(list = ls())

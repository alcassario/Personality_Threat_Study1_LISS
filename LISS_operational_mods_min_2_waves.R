getwd()

# loading libraries
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(weights)
library(lubridate)
library(stringr)

# loading cleaned data
load("LISS_long_Cleaned.RData") 
variable.names(dat)

### loading threat data ###
homicide <- read.csv("WB_homicide_cleaned.csv")
unemployment <- read.csv("WB_unemployment_cleaned.csv")
immigration <- read.csv("UN_immigration.csv")

homicide$Year <- recode(homicide$Year, X2007 = "2007", X2008 = "2008", X2009 = "2009",
                                                     X2010 = "2010", X2011 = "2011", X2012 = "2012", 
                                                     X2013 = "2013", X2014 = "2014", X2015 = "2015", 
                                                     X2016 = "2016", X2017 = "2017", X2018 = "2018",
                                                     X2019 = "2019", X2020 = "2020", X2021 = "2021", 
                                                     X2022 = "2022")

## for later waves all data collected in same year, for earlier waves some split 
dat$Threat_Year <- ifelse(dat$wave == 1 & str_detect(dat$wave1_start, "-07") == TRUE, 2007, 
                          ifelse(dat$wave == 1 & str_detect(dat$wave1_start,"-08") == TRUE, 2008,
                                 ifelse(dat$wave == 2 & str_detect(dat$wave2_start, "-08") == TRUE, 2008, 
                                        ifelse(dat$wave == 2 & str_detect(dat$wave2_start, "-09") == TRUE, 2009,
                                               ifelse(dat$wave == 3 & str_detect(dat$wave3_start, "2009") == TRUE, 2009,
                                                      ifelse(dat$wave == 3 & str_detect(dat$wave3_start, "2010") == TRUE, 2010, 
                                                             ifelse(dat$wave == 4 & str_detect(dat$wave4_start, "2010") == TRUE, 2010, 
                                                                    ifelse(dat$wave == 4 & str_detect(dat$wave4_start, "2011") == TRUE, 2011, 
                                                                           ifelse(dat$wave == 5 & str_detect(dat$wave5_start, "2011") == TRUE, 2011, 
                                                                                  ifelse(dat$wave == 5 & str_detect(dat$wave5_start, "2012") == TRUE, 2012, 
                                                                                         ifelse(dat$wave == 6 & str_detect(dat$wave6_start, "2012") == TRUE, 2012,
                                                                                                ifelse(dat$wave == 6 & str_detect(dat$wave6_start, "2013") == TRUE, 2013, 
                                                                                                       ifelse(dat$wave == 7 & str_detect(dat$wave7_start, "2013") == TRUE, 2013, 
                                                                                                              ifelse(dat$wave == 7 & str_detect(dat$wave7_start, "2014") == TRUE, 2014, 
                                                                                                                     ifelse(dat$wave == 8 & str_detect(dat$wave8_start, "2015") == TRUE, 2015, 
                                                                                                                            ifelse(dat$wave == 8 & str_detect(dat$wave8_start, "2016") == TRUE, 2016, 
                                                                                                                                   ifelse(dat$wave == 9 & str_detect(dat$wave9_start, "2016") == TRUE, 2016, 
                                                                                                                                          ifelse(dat$wave == 9 & str_detect(dat$wave9_start, "2017") == TRUE, 2017, NA))))))))))))))))))


#### for later years, there is a one to one mapping between wave and year 
dat$Threat_Year <- ifelse(dat$wave == 10, 2018, 
                          ifelse(dat$wave == 11, 2019, 
                                 ifelse(dat$wave == 12, 2020, 
                                        ifelse(dat$wave == 13, 2021, 
                                               ifelse(dat$wave == 14, 2022, dat$Threat_Year)))))

sum(is.na(dat$Threat_Year))
sort(unique(dat$Threat_Year)) 
which(is.na(dat$Threat_Year)) # these are wave obs that are missing for an ind 


# rescaling 0-1 before modeling 
dat$income <- nalevs(dat$income_1)
dat$immigrant_culture <- nalevs(dat$imm_c_1)
dat$eu <- nalevs(dat$eu_1)
dat$mothers_work <- nalevs(dat$mothers_work)
dat$fathers_work <- nalevs(dat$fathers_work)
dat$immigration_ability <- nalevs(dat$immigration_ability)
dat$marriage <- nalevs(dat$marriage)
dat$moms_working <- nalevs(dat$moms_working)
dat$unions <- nalevs(dat$unions)
dat$gender_kids <- nalevs(dat$gender_kids)
dat$o <- nalevs(dat$o)
dat$c <- nalevs(dat$c)
dat$e <- nalevs(dat$e)
dat$a <- nalevs(dat$a)
dat$n <- nalevs(dat$n)


# getting rid of nan's
issues <- c("mothers_work", "fathers_work", "immigration_ability", 
            "marriage", "moms_working", "gender_kids", 
            "income", "immigrant_culture", "eu")

for(i in 1:length(issues)){ 
  dat <- subset(dat, is.nan(dat[,issues[i]]) == FALSE) 
  
}

for(i in 1:length(issues)){ 
  dat <- subset(dat, is.na(dat[,issues[i]]) == FALSE) 
}


n_wave <- dat %>% dplyr::group_by(Respondent_ID) %>% dplyr::summarize(wave = n())
sort(unique(dat$wave))

length(unique(dat$Respondent_ID))

# 14,662 completed at least one wave 
mean(n_wave$wave) # mean is 4.99 waves 
sd(n_wave$wave) # sd is 4.16 waves
median(n_wave$wave) # median is 3 

# adding variable for how many waves each respondent completed 
dat <- dat %>% group_by(Respondent_ID) %>% mutate(n_waves = n()) %>% ungroup()



# keeping those who have at least two waves # 
dat <- subset(dat, dat$n_waves >= 2)

# how many at least two waves?
length(unique(dat$Respondent_ID)) # 11,189 completed at least one wave 

# covid
sort(unique(dmy(dat$wave12_cv_start))) # for op ideo, we have some pre and some post 
dat$covid <- ifelse(dat$wave > 12, 1, 0)
dat$covid <- ifelse(dat$wave == 12 & dmy(dat$wave12_cv_start) > dmy("27-02-2020"), 1, dat$covid)

##### add specific threat_year data from homicide, immigration, and unemployment data ####
##### left join left participant data, join with threat data, by Threat_Year = Year   #### 
dat$Threat_Year <- as.numeric(dat$Threat_Year)
homicide$Year <- as.numeric(homicide$Year)
dat <- left_join(dat, homicide, by = join_by(Threat_Year == Year))
dat$homicide <- dat$Rate
dat <- left_join(dat, immigration, by = join_by(Threat_Year == Year))
dat$immigration <- dat$Net.Migration.Rate..per.1.000.population.
dat <- left_join(dat, unemployment, by = join_by(Threat_Year == Year))
dat$unemployment <- dat$Rate.y

# recoding threat vars 0-1 for coef interpretation 
dat$immigration <- nalevs(dat$immigration)
dat$homicide <- nalevs(dat$homicide)
dat$unemployment <- nalevs(dat$unemployment)


# vector of dv's 
variable.names(dat)
vars <- c("income", "immigrant_culture", "eu", 
          "mothers_work", "fathers_work", "immigration_ability",
          "marriage", "moms_working", "unions", "gender_kids")

# if we want time 0 for growth curve so intercept is first time point 
dat$wave <- nalevs(dat$wave.x)


# mean center (immigration, homicide, covid, unemployment, o, c, e, a, n)
dat$immigration <- dat$immigration - mean(dat$immigration, na.rm = TRUE)
dat$homicide <- dat$homicide - mean(dat$homicide, na.rm = TRUE)
dat$unemployment <- dat$unemployment - mean(dat$unemployment, na.rm = TRUE)
dat$covid <- dat$covid - mean(dat$covid)
dat$o <- dat$o - mean(dat$o, na.rm = TRUE)
dat$c <- dat$c - mean(dat$c, na.rm = TRUE)
dat$e <- dat$e - mean(dat$e, na.rm = TRUE)
dat$a <- dat$a - mean(dat$a, na.rm = TRUE)
dat$n <- dat$n - mean(dat$n, na.rm = TRUE)

# writing file into multiverse data
saveRDS(dat, "operational_for_multi.rds")

######## running in lme4 ####
options(scipen = 999)
results <- list()

for(i in 1:length(vars)){
  results[[i]] <- lmer(unlist(dat[,vars[i]]) ~ wave + homicide + 
                         immigration + covid + unemployment + o + c + e + a + n + 
                         wave:o + wave:c + homicide:o + homicide:c + 
                         immigration:o + immigration:c + covid:o + covid:c + 
                         wave:o + wave:c + unemployment:o + unemployment:c + (wave | id), data = dat, 
                       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
}

names(results) <- vars



##### Saving results list as .RData #######
save(results, file = "OperationalResults_2_wave.RData")

#### no interactions ######
options(scipen = 999)
results_1 <- list()

for(i in 1:length(vars)){
  results_1[[i]] <- lmer(unlist(dat[,vars[i]]) ~ wave + homicide + 
                           immigration + unemployment + covid + o + c + e + a + n + 
                           (wave | id), data = dat, 
                         control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
}

names(results_1) <- vars

# pull threat coefficients
threats <- list()
p <- list()
dv <- list()
se <- list()
View(summary(results_1[[1]])$coefficients) 

for(i in 1:length(results_1)){
  coefs <- summary(results_1[[i]])$coefficients[,1]
  threats[[i]] <- coefs[c(3:6)]
  p.values <- summary(results_1[[i]])$coefficients[,5]
  p[[i]] <- p.values[c(3:6)]
  s <- summary(results_1[[i]])$coefficients[,2]
  se[[i]] <- s[c(3:6)]
  dvs <- rep(names(results_1)[i], 4)
  dv[[i]] <- dvs
}

sort(unlist(threats)) # -.07, .08


directions <- as.data.frame(cbind(unlist(threats), unlist(p), unlist(se), unlist(dv))) 
names(directions) <- c("coef", "p.value", "se", "dv")

# turn numeric 
directions$coef <- as.numeric(directions$coef)
directions$p.value <- as.numeric(directions$p.value)

# directionality 
sum(directions$coef < 0 & directions$p.value < .05) # 9 lib sig 
sum(directions$coef > 0 & directions$p.value < .05) # 19 con sig 
sum(directions$p.value > .05) # 12 non sig 


# mean shifts for operational and symbolic coefs 
mean(directions$coef[directions$coef > 0 & directions$p.value < .05], .015) # .03 con 
mean(directions$coef[directions$coef < 0 & directions$p.value < .05], -.055, -.013) # .02 lib

# which dvs and which threats have largest effects 
directions <- directions %>% arrange(desc(coef))
View(directions) # largest lib is imm; largest cons is imm 

save(results_1, file = "OperationalResultsNoInteractions_2_wave.RData")
rm(list = ls())

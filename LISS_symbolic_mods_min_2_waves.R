getwd()

# loading libraries
library(lavaan)
library(dplyr)
library(lme4)
library(lmerTest)
library(brms)
library(scales)
library(broom.mixed)
library(lubridate)
library(weights)


# loading data
dat <- read.csv("LISS_merged_scored.csv")
variable.names(dat) 
dat <- dat[,-1]

# how many completed each wave?
n_wave <- dat %>% dplyr::group_by(Respondent_ID) %>% dplyr::summarize(wave = n())
# 15,320 completed at least one wave 
mean(n_wave$wave) # mean is 4.91 waves 
sd(n_wave$wave) # sd is 4.40 waves
median(n_wave$wave) # median is 3 

# adding variable for how many waves each respondent completed 
dat <- dat %>% group_by(Respondent_ID) %>% mutate(n_waves = n()) %>% ungroup()

# subsetting such that we only include Ss with at least two waves 
dat <- subset(dat, dat$n_waves >= 2)

# mean waves
mean(dat$n_waves) # 9.34
sd(dat$n_waves) # 4.39

# N for min two waves
length(unique(dat$Respondent_ID)) #10,876


# don't have threat data for wave 15 yet, removing those observations 
dat <- subset(dat, dat$wave < 15)

# COVID 
dat$covid <- ifelse(dat$wave > 12, 1, 0) # 13 or later is during covid 

# coding threat year 
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





# All post wave 9 have 1:1 wave/year mapping
dat$Threat_Year <- ifelse(dat$wave == 10, 2018, 
                          ifelse(dat$wave == 11, 2019, 
                                 ifelse(dat$wave == 12, 2020, 
                                        ifelse(dat$wave == 13, 2021, 
                                               ifelse(dat$wave == 14, 2022, dat$Threat_Year)))))

# load threat data
homicide <- read.csv("WB_homicide_cleaned.csv")
unemployment <- read.csv("WB_unemployment_cleaned.csv")
immigration <- read.csv("UN_immigration.csv")

homicide$Year <- recode(homicide$Year, X2007 = "2007", X2008 = "2008", X2009 = "2009",
                        X2010 = "2010", X2011 = "2011", X2012 = "2012", 
                        X2013 = "2013", X2014 = "2014", X2015 = "2015", 
                        X2016 = "2016", X2017 = "2017", X2018 = "2018",
                        X2019 = "2019", X2020 = "2020", X2021 = "2021", 
                        X2022 = "2022")

# add threats by completion date 
dat$Threat_Year <- as.numeric(dat$Threat_Year)
homicide$Year <- as.numeric(homicide$Year)
dat <- left_join(dat, homicide, by = join_by(Threat_Year == Year))
dat$homicide <- dat$Rate
dat <- left_join(dat, immigration, by = join_by(Threat_Year == Year))
dat$immigration <- dat$Net.Migration.Rate..per.1.000.population.
dat <- left_join(dat, unemployment, by = join_by(Threat_Year == Year))
dat$unemployment <- dat$Rate.y
dat$wave <- dat$wave.x


# turn all vars of interest 0-1 
dat$ideology <- nalevs(dat$ideology)
dat$o <- nalevs(dat$o)
dat$c <- nalevs(dat$c)
dat$e <- nalevs(dat$e)
dat$a <- nalevs(dat$a)
dat$n <- nalevs(dat$n)
dat$homicide <- nalevs(dat$homicide)
dat$unemployment <- nalevs(dat$unemployment)
dat$covid <- nalevs(dat$covid)
dat$immigration <- nalevs(dat$immigration)
dat$wave <- nalevs(dat$wave)

# mean centering except for wave and dvs 
dat$o <- dat$o - mean(dat$o, na.rm = TRUE)
dat$c <- dat$c - mean(dat$c, na.rm = TRUE)
dat$e <- dat$e - mean(dat$e, na.rm = TRUE)
dat$a <- dat$a - mean(dat$a, na.rm = TRUE)
dat$n <- dat$n - mean(dat$n, na.rm = TRUE)
dat$homicide <- dat$homicide - mean(dat$homicide, na.rm = TRUE)
dat$unemployment <- dat$unemployment - mean(dat$homicide, na.rm = TRUE)
dat$covid <- dat$covid - mean(dat$covid, na.rm = TRUE)
dat$immigration <- dat$immigration - mean(dat$immigration, na.rm = TRUE)


# do people respond to threat with rightward shifts? not really, but maybe for covid 
options(scipen = 999)
dat$ideology <- ifelse(dat$ideology > 10, NA, dat$ideology)

# writing into dataframe for multiverse analysis
saveRDS(dat, "dat_symbolic_multiverse.rds")

# models 
m1 <- lmer(ideology ~ immigration + homicide + unemployment + covid + wave + o + c + e + a + n +
             (wave|Respondent_ID) , data = dat)
summary(m1) 
# looks like there is a leftward response to immigration, unemployment, rightward for covid 
m1 <- update(m1, control = lmerControl(optimizer = "bobyqa")) 
summary(m1) # -.05, immigration largest coef 

# fitting mlms with interaction, does this vary by personality?  
m2 <- lmer(ideology ~ immigration + homicide + unemployment + covid + wave + o + c + e + a + n +
             immigration:o + immigration:c + unemployment:o + 
             unemployment:c + homicide:o + homicide:c + covid:o + covid:c + 
             (wave|Respondent_ID), data = dat)
summary(m2) # not really, some suggestive evidence for unemployment, o, less lib than others
# homicide:c more conservative,
m2 <- update(m2, control = lmerControl(optimizer = "bobyqa")) 
summary(m2) # maybe cons sig diff than others for homicide 



# fitting mlms with interactions, now including n 
m3 <- lmer(ideology ~ immigration + homicide + unemployment  + o + c + e + a + n +
             immigration:o + immigration:c + immigration:n + unemployment:o + 
             unemployment:c + unemployment:n + homicide:o + homicide:c + homicide:n + 
             covid:n + covid:o + covid:c + 
             (wave|Respondent_ID), data = dat)
m3 <- update(m3, control = lmerControl(optimizer = "bobyqa")) 
summary(m3) # maybe c by homicide 

# how many completed at least 2 waves?
length(unique(dat$Respondent_ID)) #10,876

# saving results 
results_symbolic <- list(m1, m2, m3)
save(results_symbolic, file = "results_symbolic_2_wave.RData")

rm(list = ls())


library(simr)
library(lme4)
library(lmerTest)
library(dplyr)

### load data 
dat <- readRDS("operational_for_multi.rds")

### selecting only vars we use 
dat <- dat %>% select(c("income", "immigrant_culture", "eu", 
                        "mothers_work", "fathers_work", "immigration_ability",
                        "marriage", "moms_working", "unions", "gender_kids", "wave", "homicide", 
                        "covid", "immigration", "unemployment", "id", "o", "c", "e", "a", "n"))

### simr can't handle na
dat <- na.omit(dat)

### model no interactions
mod_int <- lmer(income ~ wave + homicide + 
              immigration + covid + unemployment + o + c + e + a + n + 
              wave:o + wave:c + homicide:o + homicide:c + 
              immigration:o + immigration:c + covid:o + covid:c + 
              wave:o + wave:c + unemployment:o + unemployment:c + (wave | id), data = dat, 
            control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(mod_int)

### model main effects 
mod <- lmer(income ~ wave + homicide + 
                  immigration + covid + unemployment + o + c + e + a + n + 
                (wave | id), data = dat, 
                control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(mod)

### simulation interaction effects 
fixef(mod_int)["unemployment:o"] <- .2
pow_test <- powerSim(mod_int, test = fixed("unemployment:o", "t"), nsim = 100)
pow_test # about 100 percent power for moderately sized threat effect 

### simulation main effect 
fixef(mod)["unemployment"] <- .2
pow_test <- powerSim(mod, test = fixed("unemployment", "t"), nsim = 100)
pow_test # about 100 percent power for moderately sized threat effect 

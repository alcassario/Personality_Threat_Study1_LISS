getwd()

# libraries
library(dplyr)
library(ggplot2)
library(psych)
library(performance)
library(tidyverse)

# data
load("LISS_long_Cleaned.Rdata")

# taking one wave to account for data structure/ not confuse package 
dat <- subset(dat, dat$wave == 1)

# reliabilities

# mom's working
psych::alpha(dat[,13:15]) # .8

# father's working
psych::alpha(dat[,16:19]) # .65

# imm ability 
psych::alpha(dat[,20:27]) # .70


# marriage
psych::alpha(dat[,28:34]) # .71

# mom's young work
psych::alpha(dat[,35:38]) # .8

# unions
item_intercor(dat[,43:44]) # .40

# gender kids
psych::alpha(dat[,39:42]) # .67

########## personality ############

# personality data
personality <- readRDS("personality.RDS")

# subsetting w1 bc of data structure 
personality <- personality %>% select(contains("cp08"))
variable.names(personality)

# removing items that we reverse scored 
personality <- personality %>% select(-c("cp08a029", "cp08a039", "cp08a049", "cp08a027", 
                                         "cp08a037", "cp08a047", "cp08a057", "cp08a021", 
                                         "cp08a031", "cp08a041", "cp08a051", "cp08a025", 
                                         "cp08a035", "cp08a045", "cp08a055", "cp08a065", 
                                         "cp08a028", "cp08a038"))

# alphas 
o <- personality %>% select(contains(c("024", "029_r", "034", "039_r", "044", 
                                       "049_r", "054", "059", "064", "069"))) %>% psych::alpha()
o # .76


c <- personality %>% select(contains(c("027_r", "022", "032", "037_r", "042", 
                                       "047_r", "052", "057_r", "062", "067"))) %>% psych::alpha()
c # .77 

a <- personality %>% select(contains(c("021_r", "031_r", "041_r", "051_r", "026", 
                                       "036", "046", "056", "066", "061"))) %>% psych::alpha()
a # .8 

e <- personality %>% select(contains(c("025_r", "035_r", "045_r", "055_r", "065_r", 
                                       "020", "030", "040", "050", "060"))) %>% psych::alpha()
e # .86 

n <- personality %>% select(contains(c("028", "038_r", "033", "043", "048", 
                                       "053", "058", "063", "068", "023"))) %>% psych::alpha()
n # .88 

# start fresh 
rm(list = ls()) 

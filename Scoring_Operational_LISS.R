
# loading packages
library(haven)
library(ggplot2)
library(psych)
library(dplyr)

# loading data 
dat <- readRDS(file = "LISS_for_scoring.rds")
variable.names(dat)
# variables to score 
variable.names(dat) 

# selecting only vars we need
dat <- dat %>% select(Respondent_ID, gender, age_t1, o, c, e, a, n, wave, income_1, 
                      imm_c_1, eu_1, m_w_1_1, m_w_2_1, m_w_3_1, f_w_1_1, f_w_2_1, 
                      f_w_3_1, f_w_4_1, imm_abb_1_1, imm_abb_2_1, imm_abb_3_1, 
                      imm_abb_4_1, imm_abb_5_1, imm_abb_6_1, imm_abb_7_1, imm_abb_8_1,
                      marr_1_1, marr_2_1, marr_3_1, marr_4_1, marr_5_1, marr_6_1,
                      marr_7_1, m_y_1_1, m_y_2_1, m_y_3_1, m_y_4_1, gen_1_1, gen_2_1, gen_3_1,
                      gen_4_1, union_1_1, union_2_1, id, wave12_cv_start, wave1_start, wave2_start, 
                      wave3_start, wave4_start, wave5_start, wave6_start, wave7_start, wave8_start, 
                      wave9_start)


# scoring variables #
# go through and redo these because missing forgotten mothers of young var with change. # 

# creating function to return NA for missings 
return_na <- function(x){ifelse(x == -9 | x == 99, NA, x)}

# scoring income
dat$income_1 <- return_na(dat$income_1)

# function to reverse code so more conservative is higher
recode_cons <- function(x){ifelse(x == 1, 5, 
                                  ifelse(x == 2, 4, 
                                         ifelse(x == 3, 3, 
                                                ifelse(x == 4, 2, 
                                                       ifelse(x == 5, 1, x)))))}
# making income so conservative position higher
dat$income_1 <- recode_cons(dat$income_1)
variable.names(dat)

# immigrant culture
dat$imm_c_1 <- return_na(dat$imm_c_1)
# already cons higher

# european union 
dat$eu_1 <- return_na(dat$eu_1)
# already cons higher

# mothers and work 
for(i in 13:15){
  dat[,i] <- return_na(dat[,i])
}
dat$m_w_1_1 <- recode_cons(dat$m_w_1_1)
dat$mothers_work <- rowMeans(dat[,13:15], na.rm = TRUE)

# fathers and work
for(i in 16:19){
  dat[,i] <- return_na(dat[,i])
}


items <- c("f_w_1_1", "f_w_3_1", "f_w_4_1")
for(i in 1:3){
  dat[,items[i]] <- recode_cons(dat[,items[i]])
}

dat$fathers_work <- rowMeans(dat[,16:19], na.rm = TRUE)


# recode : 1,2,3,4,6,7, 
for(i in 20:27){
  dat[,i] <- return_na(dat[,i])
}

variable.names(dat)
items <- c("imm_abb_1_1", "imm_abb_2_1", "imm_abb_3_1", "imm_abb_4_1", 
           "imm_abb_6_1", "imm_abb_7_1")

for(i in 1:length(items)){
  dat[,items[i]] <- recode_cons(dat[,items[i]])
}
dat$immigration_ability <- rowMeans(dat[,20:27], na.rm = TRUE)


# marriage 
for(i in 28:34){
  dat[,i] <- return_na(dat[,i])
}

cor(dat$marr_1_1, dat$marr_3_1, use = "complete")

items <- c("marr_3_1", "marr_4_1", "marr_5_1", "marr_6_1", "marr_7_1")
for(i in 1:length(items)){
  dat[,items[i]] <- recode_cons(dat[,items[i]])
}
dat$marriage <- rowMeans(dat[,28:34], na.rm = TRUE)


# womens work with young kids 
variable.names(dat) 
View(dat[,35:38])
for(i in 35:38){
  dat[,i] <- return_na(dat[,i])
}

# already higher more cons 
dat$moms_working <- rowMeans(dat[,34:38])


# unions
for(i in 43:44){
  dat[,i] <- return_na(dat[,i])
}

for(i in 43:44){
  dat[,i] <- ifelse(dat[,i] == 6, NA, dat[,i])
}
dat$unions <- rowMeans(dat[,43:44], na.rm = TRUE)


# gender and child rearing 
for(i in 39:42){
  dat[,i] <- return_na(dat[,i])
}

# already all cons higher vals 
dat$gender_kids <- rowMeans(dat[,39:42], na.rm = TRUE)
variable.names(dat)

# lots of missingness in later waves but code seems okay 
save(dat, file = "LISS_long_Cleaned.Rdata")
rm(list = ls())

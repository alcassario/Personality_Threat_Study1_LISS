getwd()

# load packages
library(reshape2)
library(dplyr)
library(haven)
library(purrr)

# vector of files names
LISS_files <- c("pols_08.dta", "pols_09.dta", "pols_10.dta", "pols_11.dta", "pols_12.dta",
                "pols_13.dta", "pols_14.dta", "pols_16.dta", "pols_17.dta", "pols_18.dta", 
                "pols_19.dta", "pols_20.dta", "pols_21.dta", "pols_22.dta","pols_23.dta","personality_08.dta",
                "personality_09.dta", "personality_10.dta", "personality_11.dta", "personality_12.dta",
                "personality_13.dta", "personality_14.dta", "personality_15.dta", "personality_17.dta", 
                "personality_18.dta", "personality_19.dta", "personality_20.dta", "personality_21.dta",
                "personality_22.dta", "personality_23.dta", 
                "background_08.dta")
LISS_list <- list()


# looping through vector of file names and saving in list
for(i in 1:length(LISS_files)){
  LISS_list[[i]] <- read_dta(LISS_files[i])
}

length(unique(LISS_list[[1]]$nomem_encr)) # unique subject identifier 

reduced <- LISS_list %>% reduce(full_join, by = "nomem_encr")


# easier to select and rename from data frame 
variable.names(reduced)
vars <- reduced %>% select(cp08a020:cp08a069, cp09b020:cp09b069,
                           cp10c020:cp10c069, cp11d020:cp11d069,
                           cp12e020:cp12e069, cp13f020:cp13f069,
                           cp14g020:cp14g069, cp15h020:cp15h069, 
                           cp17i020:cp17i069, cp18j020:cp18j069, 
                           cp19k020:cp19k069, cp20l020:cp20l069, 
                           cp21m020:cp21m069, cp22n020:cp22n069, 
                           cp23o020:cp23o069, 
                           wave = wave, Respondent_ID = nomem_encr,
                           Left_Right_1 = cv08a101, Left_Right_2 = cv09b101, 
                           Left_Right_3 = cv10c101, Left_Right_4 = cv11d101, 
                           Left_Right_5 = cv12e101, Left_Right_6 = cv13f101, 
                           Left_Right_7 = cv14g101, Left_Right_8 = cv16h101, 
                           Left_Right_9 = cv17i101, Left_Right_10 = cv18j101, 
                           Left_Right_11 = cv19k101, Left_Right_12 = cv20l101, 
                           Left_Right_13 = cv21m101, Left_Right_14 = cv22n101,
                           Left_Right_15 = cv23o101,
                           gender = geslacht, age_t1 = leeftijd,  
                           wave1_start = cv08a161, wave2_start = cv09b161, 
                           wave3_start = cv10c161, wave4_start = cv11d161, wave5_start = cv12e161, 
                           wave6_start = cv13f161, wave7_start = cv14g161, wave8_start = DatumB, 
                           wave9_start = cv17i161)
# remove extra objects
rm(list = c("reduced", "LISS_list"))

# scoring personality measure 
reverse <- function(x){
  ifelse(x == 1, 5, 
         ifelse(x == 2, 4, 
                ifelse(x == 3, 3, 
                       ifelse(x == 4, 2, 
                              ifelse(x == 5, 1, x)))))
}

# apply to rescore 
vars <- data.frame(vars)

rs <- vars %>% select(contains(c("029", "039", "049", "027", "037", "047", "057", "021", "031", "041", 
                              "051", "025", "035", "045", "055", "065", "028", "038"))) 

reversed <- apply(rs, 2, reverse)
variable.names(reversed)
colnames(reversed) <- c(paste0(variable.names(reversed), "_r"))

rs_vars <- cbind(vars, reversed)

# row means of big five 
rs_vars$o <- rs_vars %>% select(contains(c("024", "029_r", "034", "039_r", "044", 
                                           "049_r", "054", "059", "064", "069"))) %>%
                          rowMeans(na.rm = TRUE)


rs_vars$c <- rs_vars %>% select(contains(c("027_r", "022", "032", "037_r", "042", 
                                           "047_r", "052", "057_r", "062", "067"))) %>%
  rowMeans(na.rm = TRUE)


rs_vars$a <- rs_vars %>% select(contains(c("021_r", "031_r", "041_r", "051_r", "026", 
                                           "036", "046", "056", "066", "061"))) %>%
  rowMeans(na.rm = TRUE)


rs_vars$e <- rs_vars %>% select(contains(c("025_r", "035_r", "045_r", "055_r", "065_r", 
                                           "020", "030", "040", "050", "060"))) %>%
  rowMeans(na.rm = TRUE)


rs_vars$n <- rs_vars %>% select(contains(c("028", "038_r", "033", "043", "048", 
                                           "053", "058", "063", "068", "023"))) %>%
  rowMeans(na.rm = TRUE)


# reshaping to long 
variable.names(rs_vars)
rs_vars <- rs_vars %>% select(Left_Right_1:Left_Right_15, o, c, e, a, n, Respondent_ID, 
                              gender, age_t1, wave1_start:wave9_start)

long_liss <- tidyr::gather(rs_vars, wave, ideology, Left_Right_1:Left_Right_15)
# warning is from diff variable types, but everything comes over fine 


long_liss <- subset(long_liss, long_liss$ideology >= 0 & long_liss$ideology < 11)


# numeric wave 
long_liss$wave <- ifelse(long_liss$wave == "Left_Right_1", 1, 
                         ifelse(long_liss$wave == "Left_Right_2", 2, 
                                ifelse(long_liss$wave == "Left_Right_3", 3, 
                                       ifelse(long_liss$wave == "Left_Right_4", 4,
                                              ifelse(long_liss$wave == "Left_Right_5", 5, 
                                                     ifelse(long_liss$wave == "Left_Right_6", 6, 
                                                            ifelse(long_liss$wave == "Left_Right_7", 7, 
                                                                   ifelse(long_liss$wave == "Left_Right_8", 8, 
                                                                          ifelse(long_liss$wave == "Left_Right_9", 9,
                                                                                 ifelse(long_liss$wave == "Left_Right_10", 10, 
                                                                                        ifelse(long_liss$wave == "Left_Right_11", 11, 
                                                                                               ifelse(long_liss$wave == "Left_Right_12", 12,  
                                                                                                      ifelse(long_liss$wave == "Left_Right_13", 13, 
                                                                                                             ifelse(long_liss$wave == "Left_Right_14", 14, 
                                                                                                                    ifelse(long_liss$wave == "Left_Right_15", 15, long_liss$wave
                                                                                                                           )))))))))))))))

long_liss$wave <- as.numeric(long_liss$wave)

# writing into csv for use in modeling code 
write.csv(long_liss, "LISS_merged_scored.csv")
# this file can be used for demos 

rm(list = ls())

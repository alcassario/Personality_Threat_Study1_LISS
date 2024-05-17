getwd()

# load packages
library(reshape2)
library(dplyr)
library(haven)
library(purrr)

# vector of files names
LISS_files <- c("pols_08.dta", "pols_09.dta", "pols_10.dta", "pols_11.dta", "pols_12.dta",
                "pols_13.dta", "pols_14.dta", "pols_16.dta", "pols_17.dta", "pols_18.dta", 
                "pols_19.dta", "pols_20.dta", "pols_21.dta", "pols_22.dta","personality_08.dta",
                "personality_09.dta", "personality_10.dta", "personality_11.dta", "personality_12.dta",
                "personality_13.dta", "personality_14.dta", "personality_15.dta", "personality_17.dta", 
                "personality_18.dta", "personality_19.dta", "personality_20.dta", "personality_21.dta",
                "personality_22.dta", "personality_23.dta", "pols_23.dta" ,
                "background_08.dta")
LISS_list <- list()


# looping through vector of file names and saving in list
for(i in 1:length(LISS_files)){
  LISS_list[[i]] <- read_dta(LISS_files[i])
}

# reducing to dataframe
reduced <- LISS_list %>% reduce(full_join, by = "nomem_encr")

# no threat data for ideology from wae 15 yet, so we keep personality 
# for these individuals but we can't look at ideology dv yet 


# treating personality as stable for now 
vars <- reduced %>% select(cp08a020:cp08a069, cp09b020:cp09b069,
                           cp10c020:cp10c069, cp11d020:cp11d069,
                           cp12e020:cp12e069, cp13f020:cp13f069,
                           cp14g020:cp14g069, cp15h020:cp15h069, 
                           cp17i020:cp17i069, cp18j020:cp18j069, 
                           cp19k020:cp19k069, cp20l020:cp20l069, 
                           cp21m020:cp21m069, cp22n020:cp22n069, 
                           cp23o020:cp23o069,
                           Respondent_ID = nomem_encr,
                           income_1 = cv08a103, income_2 = cv09b103, income_3 = cv10c103, 
                           income_4 = cv11d103, income_5 = cv12e103, income_6 = cv13f103,
                           income_7 = cv14g103, income_8 = cv16h103, income_9 = cv17i103, 
                           income_10 = cv18j103, income_11 = cv19k103, income_12 = cv20l103, 
                           income_13 = cv21m103, income_14 = cv22n103,
                           imm_c_1 = cv08a104, imm_c_2 = cv09b104, imm_c_3 = cv10c104,
                           imm_c_4 = cv11d104, imm_c_5 = cv12e104, imm_c_6 = cv13f104, 
                           imm_c_7 = cv14g104, imm_c_8 = cv16h104, imm_c_9 = cv17i104, 
                           imm_c_10 = cv18j104, imm_c_11 = cv19k104, imm_c_12 = cv20l104, 
                           imm_c_13 = cv21m104, imm_c_14 = cv22n104, 
                           eu_1 = cv08a105, eu_2 = cv09b105, eu_3 = cv10c105, 
                           eu_4 = cv11d105, eu_5 = cv12e105, eu_6 = cv13f105, 
                           eu_7 = cv14g104, eu_8 = cv16h105, eu_9 = cv17i105, 
                           eu_10 = cv18j105, eu_11 = cv19k105, eu_12 = cv20l105, 
                           eu_13 = cv21m105, eu_14 = cv22n105, 
                           m_w_1_1 = cv08a109, m_w_1_2 = cv09b109, m_w_1_3 = cv10c109,
                           m_w_1_4 = cv11d109, m_w_1_5 = cv12e109, m_w_1_6 = cv13f109, 
                           m_w_1_7 = cv14g109, m_w_1_8 = cv16h109, m_w_1_9 = cv17i109,
                           m_w_1_10 = cv18j109, m_w_1_11 = cv19k109, m_w_1_12 = cv20l109,
                           m_w_1_13 = cv21m109, m_w_1_14 = cv22n109, 
                           m_w_2_1 = cv08a110, m_w_2_2 = cv09b110, m_w_2_3 = cv10c110,
                           m_w_2_4 = cv11d110, m_w_2_5 = cv12e110, m_w_2_6 = cv13f110, 
                           m_w_2_7 = cv14g110, m_w_2_8 = cv16h110, m_w_2_9 = cv17i110,
                           m_w_2_10 = cv18j110, m_w_2_11 = cv19k110, m_w_2_12 = cv20l110,
                           m_w_2_13 = cv21m110, m_w_2_14 = cv22n110,
                           m_w_3_1 = cv08a111, m_w_3_2 = cv09b111, m_w_3_3 = cv10c111,
                           m_w_3_4 = cv11d111, m_w_3_5 = cv12e111, m_w_3_6 = cv13f111, 
                           m_w_3_7 = cv14g111, m_w_3_8 = cv16h111, m_w_3_9 = cv17i111,
                           m_w_3_10 = cv18j111, m_w_3_11 = cv19k111, m_w_3_12 = cv20l111,
                           m_w_3_13 = cv21m111, m_w_3_14 = cv22n111,
                           f_w_1_1 = cv08a112, f_w_1_2 = cv09b112, f_w_1_3 = cv10c112,
                           f_w_1_4 = cv11d112, f_w_1_5 = cv12e112, f_w_1_6 = cv13f112, 
                           f_w_1_7 = cv14g112, f_w_1_8 = cv16h112, f_w_1_9 = cv17i112, 
                           f_w_1_10 = cv18j112, f_w_1_11 = cv19k112, f_w_1_12 = cv20l112,
                           f_w_1_13 = cv21m112, f_w_1_14 = cv22n112,
                           f_w_2_1 = cv08a113, f_w_2_2 = cv09b113, f_w_2_3 = cv10c113,
                           f_w_2_4 = cv11d113, f_w_2_5 = cv12e113, f_w_2_6 = cv13f113, 
                           f_w_2_7 = cv14g113, f_w_2_8 = cv16h113, f_w_2_9 = cv17i113, 
                           f_w_2_10 = cv18j113, f_w_2_11 = cv19k113, f_w_2_12 = cv20l113,
                           f_w_2_13 = cv21m113, f_w_2_14 = cv22n113, 
                           f_w_3_1 = cv08a114, f_w_3_2 = cv09b114, f_w_3_3 = cv10c114,
                           f_w_3_4 = cv11d114, f_w_3_5 = cv12e114, f_w_3_6 = cv13f114, 
                           f_w_3_7 = cv14g114, f_w_3_8 = cv16h114, f_w_3_9 = cv17i114, 
                           f_w_3_10 = cv18j114, f_w_3_11 = cv19k114, f_w_3_12 = cv20l114,
                           f_w_3_13 = cv21m114, f_w_3_14 = cv22n114,
                           f_w_4_1 = cv08a115, f_w_4_2 = cv09b115, f_w_4_3 = cv10c115,
                           f_w_4_4 = cv11d115, f_w_4_5 = cv12e115, f_w_4_6 = cv13f115, 
                           f_w_4_7 = cv14g115, f_w_4_8 = cv16h115, f_w_4_9 = cv17i115, 
                           f_w_4_10 = cv18j115, f_w_4_11 = cv19k115, f_w_4_12 = cv20l115,
                           f_w_4_13 = cv21m115, f_w_4_14 = cv22n115,
                           imm_abb_1_1 = cv08a116, imm_abb_1_2 = cv09b116, imm_abb_1_3 = cv10c116,
                           imm_abb_1_4 = cv11d116, imm_abb_1_5 = cv12e116, imm_abb_1_6 = cv13f116, 
                           imm_abb_1_7 = cv14g116, imm_abb_1_8 = cv16h116, imm_abb_1_9 = cv17i116,
                           imm_abb_1_10 = cv18j116, imm_abb_1_11 = cv19k116, imm_abb_1_12 = cv20l116,
                           imm_abb_1_13 = cv21m116, imm_abb_1_14 = cv22n116,
                           imm_abb_2_1 = cv08a117, imm_abb_2_2 = cv09b117, imm_abb_2_3 = cv10c117,
                           imm_abb_2_4 = cv11d117, imm_abb_2_5 = cv12e117, imm_abb_2_6 = cv13f117, 
                           imm_abb_2_7 = cv14g117, imm_abb_2_8 = cv16h117, imm_abb_2_9 = cv17i117,
                           imm_abb_2_10 = cv18j117, imm_abb_2_11 = cv19k117, imm_abb_2_12 = cv20l117,
                           imm_abb_2_13 = cv21m117, imm_abb_2_14 = cv22n117, 
                           imm_abb_3_1 = cv08a118, imm_abb_3_2 = cv09b118, imm_abb_3_3 = cv10c118,
                           imm_abb_3_4 = cv11d118, imm_abb_3_5 = cv12e118, imm_abb_3_6 = cv13f118, 
                           imm_abb_3_7 = cv14g118, imm_abb_3_8 = cv16h118, imm_abb_3_9 = cv17i118,
                           imm_abb_3_10 = cv18j118, imm_abb_3_11 = cv19k118, imm_abb_3_12 = cv20l118,
                           imm_abb_3_13 = cv21m118, imm_abb_3_14 = cv22n118,
                           imm_abb_4_1 = cv08a119, imm_abb_4_2 = cv09b119, imm_abb_4_3 = cv10c119,
                           imm_abb_4_4 = cv11d119, imm_abb_4_5 = cv12e119, imm_abb_4_6 = cv13f119, 
                           imm_abb_4_7 = cv14g119, imm_abb_4_8 = cv16h119, imm_abb_4_9 = cv17i119,
                           imm_abb_4_10 = cv18j119, imm_abb_4_11 = cv19k119, imm_abb_4_12 = cv20l119,
                           imm_abb_4_13 = cv21m120, imm_abb_4_14 = cv22n119,
                           imm_abb_5_1 = cv08a120, imm_abb_5_2 = cv09b120, imm_abb_5_3 = cv10c120,
                           imm_abb_5_4 = cv11d120, imm_abb_5_5 = cv12e120, imm_abb_5_6 = cv13f120, 
                           imm_abb_5_7 = cv14g120, imm_abb_5_8 = cv16h120, imm_abb_5_9 = cv17i120,
                           imm_abb_5_10 = cv18j120, imm_abb_5_11 = cv19k120, imm_abb_5_12 = cv20l120,
                           imm_abb_5_13 = cv21m120, imm_abb_5_14 = cv22n120, 
                           imm_abb_6_1 = cv08a121, imm_abb_6_2 = cv09b121, imm_abb_6_3 = cv10c121,
                           imm_abb_6_4 = cv11d121, imm_abb_6_5 = cv12e121, imm_abb_6_6 = cv13f121, 
                           imm_abb_6_7 = cv14g121, imm_abb_6_8 = cv16h121, imm_abb_6_9 = cv17i121,
                           imm_abb_6_10 = cv18j121, imm_abb_6_11 = cv19k121, imm_abb_6_12 = cv20l121,
                           imm_abb_6_13 = cv21m121, imm_abb_6_14 = cv22n121, 
                           imm_abb_7_1 = cv08a122, imm_abb_7_2 = cv09b122, imm_abb_7_3 = cv10c122,
                           imm_abb_7_4 = cv11d122, imm_abb_7_5 = cv12e122, imm_abb_7_6 = cv13f122, 
                           imm_abb_7_7 = cv14g122, imm_abb_7_8 = cv16h122, imm_abb_7_9 = cv17i122,
                           imm_abb_7_10 = cv18j122, imm_abb_7_11 = cv19k122, imm_abb_7_12 = cv20l122,
                           imm_abb_7_13 = cv21m122, imm_abb_7_14 = cv22n122, 
                           imm_abb_8_1 = cv08a123, imm_abb_8_2 = cv09b123, imm_abb_8_3 = cv10c123,
                           imm_abb_8_4 = cv11d123, imm_abb_8_5 = cv12e123, imm_abb_8_6 = cv13f123, 
                           imm_abb_8_7 = cv14g123, imm_abb_8_8 = cv16h123, imm_abb_8_9 = cv17i123,
                           imm_abb_8_10 = cv18j123, imm_abb_8_11 = cv19k123, imm_abb_8_12 = cv20l123,
                           imm_abb_8_13 = cv21m123, imm_abb_8_14 = cv22n123,
                           marr_1_1 = cv08a124, marr_1_2 = cv09b124, marr_1_3 = cv10c124, 
                           marr_1_4 = cv11d124, marr_1_5 = cv12e124, marr_1_6 = cv13f124, 
                           marr_1_7 = cv14g124, marr_1_8 = cv16h124, marr_1_9 = cv17i124, 
                           marr_1_10 = cv18j124, marr_1_11 = cv19k124, marr_1_12 = cv20l124, 
                           marr_1_13 = cv21m124, marr_1_14 = cv22n124,
                           marr_2_1 = cv08a125, marr_2_2 = cv09b125, marr_2_3 = cv10c125, 
                           marr_2_4 = cv11d125, marr_2_5 = cv12e125, marr_2_6 = cv13f125, 
                           marr_2_7 = cv14g125, marr_2_8 = cv16h125, marr_2_9 = cv17i125, 
                           marr_2_10 = cv18j125, marr_2_11 = cv19k125, marr_2_12 = cv20l125, 
                           marr_2_13 = cv21m125, marr_2_14 = cv22n125,
                           marr_3_1 = cv08a126, marr_3_2 = cv09b126, marr_3_3 = cv10c126, 
                           marr_3_4 = cv11d126, marr_3_5 = cv12e126, marr_3_6 = cv13f126, 
                           marr_3_7 = cv14g126, marr_3_8 = cv16h126, marr_3_9 = cv17i126, 
                           marr_3_10 = cv18j126, marr_3_11 = cv19k126, marr_3_12 = cv20l126, 
                           marr_3_13 = cv21m126, marr_3_14 = cv22n126,
                           marr_4_1 = cv08a127, marr_4_2 = cv09b127, marr_4_3 = cv10c127, 
                           marr_4_4 = cv11d127, marr_4_5 = cv12e127, marr_4_6 = cv13f127, 
                           marr_4_7 = cv14g127, marr_4_8 = cv16h127, marr_4_9 = cv17i127, 
                           marr_4_10 = cv18j127, marr_4_11 = cv19k127, marr_4_12 = cv20l127, 
                           marr_4_13 = cv21m127, marr_4_14 = cv22n127,
                           marr_5_1 = cv08a128, marr_5_2 = cv09b128, marr_5_3 = cv10c128, 
                           marr_5_4 = cv11d128, marr_5_5 = cv12e128, marr_5_6 = cv13f128, 
                           marr_5_7 = cv14g128, marr_5_8 = cv16h128, marr_5_9 = cv17i128, 
                           marr_5_10 = cv18j128, marr_5_11 = cv19k128, marr_5_12 = cv20l128, 
                           marr_5_13 = cv21m128, marr_5_14 = cv22n128,
                           marr_6_1 = cv08a129, marr_6_2 = cv09b129, marr_6_3 = cv10c129, 
                           marr_6_4 = cv11d129, marr_6_5 = cv12e129, marr_6_6 = cv13f129, 
                           marr_6_7 = cv14g129, marr_6_8 = cv16h129, marr_6_9 = cv17i129, 
                           marr_6_10 = cv18j129, marr_6_11 = cv19k129, marr_6_12 = cv20l129, 
                           marr_6_13 = cv21m129, marr_6_14 = cv22n129,
                           marr_7_1 = cv08a130, marr_7_2 = cv09b130, marr_7_3 = cv10c130, 
                           marr_7_4 = cv11d130, marr_7_5 = cv12e130, marr_7_6 = cv13f130, 
                           marr_7_7 = cv14g130, marr_7_8 = cv16h130, marr_7_9 = cv17i130, 
                           marr_7_10 = cv18j130, marr_7_11 = cv19k130, marr_7_12 = cv20l130, 
                           marr_7_13 = cv21m130, marr_7_14 = cv22n130,
                           m_y_1_1 = cv08a143, m_y_1_2 = cv09b143, m_y_1_3 = cv10c143, 
                           m_y_1_4 = cv11d143, m_y_1_5 = cv12e143, m_y_1_6 = cv13f143, 
                           m_y_1_7 = cv14g143, m_y_1_8 = cv16h143, m_y_1_9 = cv17i143, 
                           m_y_1_10 = cv18j143, m_y_1_11 = cv19k143, m_y_1_12 = cv20l143, 
                           m_y_1_13 = cv21m143, m_y_1_14 = cv22n143,
                           m_y_2_1 = cv08a144, m_y_2_2 = cv09b144, m_y_2_3 = cv10c144, 
                           m_y_2_4 = cv11d144, m_y_2_5 = cv12e144, m_y_2_6 = cv13f144, 
                           m_y_2_7 = cv14g144, m_y_2_8 = cv16h144, m_y_2_9 = cv17i144, 
                           m_y_2_10 = cv18j144, m_y_2_11 = cv19k144, m_y_2_12 = cv20l144, 
                           m_y_2_13 = cv21m144, m_y_2_14 = cv22n144,
                           m_y_3_1 = cv08a145, m_y_3_2 = cv09b145, m_y_3_3 = cv10c145, 
                           m_y_3_4 = cv11d145, m_y_3_5 = cv12e145, m_y_3_6 = cv13f145, 
                           m_y_3_7 = cv14g145, m_y_3_8 = cv16h145, m_y_3_9 = cv17i145, 
                           m_y_3_10 = cv18j145, m_y_3_11 = cv19k145, m_y_3_12 = cv20l145, 
                           m_y_3_13 = cv21m145, m_y_3_14 = cv22n145,
                           m_y_4_1 = cv08a146, m_y_4_2 = cv09b146, m_y_4_3 = cv10c146, 
                           m_y_4_4 = cv11d146, m_y_4_5 = cv12e146, m_y_4_6 = cv13f146, 
                           m_y_4_7 = cv14g146, m_y_4_8 = cv16h146, m_y_4_9 = cv17i146, 
                           m_y_4_10 = cv18j146, m_y_4_11 = cv19k146, m_y_4_12 = cv20l146, 
                           m_y_4_13 = cv21m146, m_y_4_14 = cv22n146,
                           union_1_1 = cv08a147, union_1_2 = cv09b147, union_1_3 = cv10c147, 
                           union_1_5 = cv12e147, union_1_6 = cv13f147,
                           union_1_7 = cv14g147, union_1_8 = cv16h147, union_1_9 = cv17i147,
                           union_1_10 = cv18j147, union_1_11 = cv19k147, union_1_12 = cv20l147,
                           union_1_13 = cv21m147, union_1_14 = cv22n147,
                           union_2_1 = cv08a148, union_2_2 = cv09b148, union_2_3 = cv10c148, 
                           union_2_5 = cv12e148, union_2_6 = cv13f148,
                           union_2_7 = cv14g148, union_2_8 = cv16h148, union_2_9 = cv17i148,
                           union_2_10 = cv18j148, union_2_11 = cv19k148, union_2_12 = cv20l148,
                           union_2_13 = cv21m148, union_2_14 = cv22n148,
                           gen_1_1 = cv08a151, gen_1_2 = cv09b151, gen_1_3 = cv10c151, 
                           gen_1_4 = cv11d151, gen_1_5 = cv12e151, gen_1_6 = cv13f151, 
                           gen_1_7 = cv14g151, gen_1_8 = cv16h151, 
                           gen_1_9 = cv17i151, gen_1_10 = cv18j151, gen_1_11 = cv19k151, 
                           gen_1_12 = cv20l151, gen_1_13 = cv21m151, gen_1_14 = cv22n151, 
                           gen_2_1 = cv08a152, gen_2_2 = cv09b152, gen_2_3 = cv10c152, 
                           gen_2_4 = cv11d152, gen_2_5 = cv12e152, gen_2_6 = cv13f152, 
                           gen_2_7 = cv14g152, gen_2_8 = cv16h152, 
                           gen_2_9 = cv17i152, gen_2_10 = cv18j152, gen_2_11 = cv19k152, 
                           gen_2_12 = cv20l152, gen_2_13 = cv21m152, gen_2_14 = cv22n152,
                           gen_3_1 = cv08a153, gen_3_2 = cv09b153, gen_3_3 = cv10c153, 
                           gen_3_4 = cv11d153, gen_3_5 = cv12e153, gen_3_6 = cv13f153, 
                           gen_3_7 = cv14g153, gen_3_8 = cv16h153, 
                           gen_3_9 = cv17i153, gen_3_10 = cv18j153, gen_3_11 = cv19k153, 
                           gen_3_12 = cv20l153, gen_3_13 = cv21m153, gen_3_14 = cv22n153,
                           gen_4_1 = cv08a154, gen_4_2 = cv09b154, gen_4_3 = cv10c154, 
                           gen_4_4 = cv11d154, gen_4_5 = cv12e154, gen_4_6 = cv13f154, 
                           gen_4_7 = cv14g154, gen_4_8 = cv16h154, 
                           gen_4_9 = cv17i154, gen_4_10 = cv18j154, gen_4_11 = cv19k154, 
                           gen_4_12 = cv20l154, gen_4_13 = cv21m154, gen_4_14 = cv22n154,
                           gender = geslacht, age_t1 = leeftijd, 
                           wave12_cv_start = cv20l297, wave1_start = cv08a161, wave2_start = cv09b161, 
                           wave3_start = cv10c161, wave4_start = cv11d161, wave5_start = cv12e161, 
                           wave6_start = cv13f161, wave7_start = cv14g161, wave8_start = DatumB, 
                           wave9_start = cv17i161) 
                          # after wave 8 all vars of interest are completed in same year 


# remove extra objects
rm(list = c("reduced", "LISS_list", "i", "LISS_files"))

# scoring personality measure 
# function 
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

colnames(reversed) <- c(paste0(variable.names(reversed), "_r"))

rs_vars <- cbind(vars, reversed)

# writing dataset for personality reliabilities 
saveRDS(rs_vars, file = "personality.RDS")

# row means of big five, personality averaged across waves 
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


# turning to long with reshape function 
library(stats)
library(labelled)
variable.names(rs_vars)

no_labs <- remove_labels(rs_vars)
no_labs$union_1_4 <- NA
no_labs$union_2_4 <- NA
options(max.print = 3000)

dat <- reshape(no_labs, varying = list(income = c(752:765), imm_c = c(766:779), 
                                       eu = c(780:793), m_w_1 = c(794:807), 
                                       m_w_2 = c(808:821), m_w_3 = c(822:835), 
                                       f_w_1  = c(836:849), f_w_2 = c(850:863), 
                                       f_w_3 = c(864:877), f_w_4 = c(878:891), 
                                       imm_abb_1 = c(892:905), imm_abb_2 = c(906:919), 
                                       imm_abb_3 = c(920:933), imm_abb_4 = c(934:947), 
                                       imm_abb_5 = c(948:961), imm_abb_6 = c(962:975), 
                                       imm_abb_7 = c(976:989), imm_abb_8 = c(990:1003), 
                                       marr_1 = c(1004:1017), marr_2 = c(1018:1031), 
                                       marr_3 = c(1032:1045), marr_4 = c(1046:1059), 
                                       marr_5 = c(1060:1073), marr_6 = c(1074:1087), 
                                       marr_7 = c(1088:1101), m_y_1 = c(1102:1115), 
                                       m_y_2 = c(1116:1129), m_y_3 = c(1130:1143), 
                                       m_y_4 = c(1144:1157),   
                                       gender_1 = c(1184:1197), 
                                       gender_2 = c(1198:1211), gender_3 = c(1212:1225), 
                                       gender_4 = c(1226:1239), union_1 = c(1158:1170,1527), 
                                       union_2 = c(1171:1183, 1528)), 
               direction = "long", 
               times = 1:14, 
               timevar = "wave")

length(unique(dat$Respondent_ID))
length(unique(dat$id)) # this tracks 

variable.names(dat)

# adding wave specific personality for big 5 for review: 


# create list of personality years to loop through 
p_years <- c("cp08", "cp09", "cp10", "cp11", "cp12", "cp13", "cp14", "cp15", "cp17", "cp18", 
             "cp19", "cp20", "cp21", "cp22", "cp23")

agree <- c("024", "029_r", "034", "039_r", "044", 
           "049_r", "054", "059", "064", "069")

neur <- c("028", "038_r", "033", "043", "048", 
          "053", "058", "063", "068", "023")

cons <- c("027_r", "022", "032", "037_r", "042", 
          "047_r", "052", "057_r", "062", "067")

open <- c("024", "029_r", "034", "039_r", "044", 
          "049_r", "054", "059", "064", "069")

extra <- c("025_r", "035_r", "045_r", "055_r", "065_r", 
           "020", "030", "040", "050", "060")

# score traits 
library(dplyr)
for(i in 1:length(p_years)){ 
  temp <- dat %>% select(contains(p_years[i]) & contains(open)) %>% rowMeans(na.rm = TRUE)
  temp <- as.vector(temp)
  dat <- cbind(dat, temp)
  names(dat)[names(dat) == "temp"] <- paste0(p_years[i], "open")
}

for(i in 1:length(p_years)){ 
  temp <- dat %>% select(contains(p_years[i]) & contains(cons)) %>% rowMeans(na.rm = TRUE)
  temp <- as.vector(temp)
  dat <- cbind(dat, temp)
  names(dat)[names(dat) == "temp"] <- paste0(p_years[i], "cons")
}

for(i in 1:length(p_years)){ 
  temp <- dat %>% select(contains(p_years[i]) & contains(neur)) %>% rowMeans(na.rm = TRUE)
  temp <- as.vector(temp)
  dat <- cbind(dat, temp)
  names(dat)[names(dat) == "temp"] <- paste0(p_years[i], "neur")
}

for(i in 1:length(p_years)){ 
  temp <- dat %>% select(contains(p_years[i]) & contains(agree)) %>% rowMeans(na.rm = TRUE)
  temp <- as.vector(temp)
  dat <- cbind(dat, temp)
  names(dat)[names(dat) == "temp"] <- paste0(p_years[i], "agree")
}

for(i in 1:length(p_years)){ 
  temp <- dat %>% select(contains(p_years[i]) & contains(cons)) %>% rowMeans(na.rm = TRUE)
  temp <- as.vector(temp)
  dat <- cbind(dat, temp)
  names(dat)[names(dat) == "temp"] <- paste0(p_years[i], "cons")
}

for(i in 1:length(p_years)){ 
  temp <- dat %>% select(contains(p_years[i]) & contains(extra)) %>% rowMeans(na.rm = TRUE)
  temp <- as.vector(temp)
  dat <- cbind(dat, temp)
  names(dat)[names(dat) == "temp"] <- paste0(p_years[i], "extra")
}


# writing into .rdata for scoring 
saveRDS(dat, file = "LISS_for_scoring_non_aggregated_personality.rds")
rm(list = ls())
gc()

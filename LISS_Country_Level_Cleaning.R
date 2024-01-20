getwd()

# libraries
library(dplyr)
library(haven)
library(tidyverse)
library(readxl)

# reading unemployment
unemployment <- read.csv("Unemployment_WB.csv", skip = 3)
names(unemployment) <- unemployment[1,]
unemployment <- unemployment[-1,]

# selecting netherlands data for years of interest
unemployment <- subset(unemployment, unemployment$`Country Name` == "Netherlands")
unemployment <- unemployment %>% select(c("2007": "2022"))
unemployment <- unemployment %>% pivot_longer(c("2007":"2022"), names_to = "Year", values_to = "Rate")
unemployment$wave <- 1:16

write.csv(unemployment, "WB_unemployment_cleaned.csv")

# reading intentional homicide 
homicide <- read.csv("WB_homicide_raw.csv", skip = 3)


# selecting Netherlands for years of interest 
homicide <- subset(homicide, homicide$Country.Name == "Netherlands")
homicide <- homicide %>% select(c("X2007":"X2022"))
homicide <- homicide %>% pivot_longer(c(1:16),names_to = "Year", values_to = "Rate")
homicide$wave <- 1:16

write.csv(homicide, "WB_homicide_cleaned.csv")


# reading in excel file for immigration
immigration <- read_excel("UN_net_migration_1000.xlsx")
immigration <- immigration[-c(1:11),]
names(immigration) <- as.character(immigration[1,]) 
immigration <- immigration[-1,]

# selecting Netherlands immigration rate
immigration <- subset(immigration, immigration$`Region, subregion, country or area *` == "Netherlands")
immigration <- immigration %>% select(c("Year", "Net Migration Rate (per 1,000 population)"))
immigration <- subset(immigration, immigration$Year >= 2007)
immigration$wave <- 1:15

write.csv(immigration, "UN_immigration.csv")

rm(list = ls())


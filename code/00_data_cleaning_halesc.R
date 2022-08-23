# Data Exploration
# Group 2
# 2022-08-16

# Libraries
library(ipumsr)
library(vtable)
library(tidyverse)
library(lubridate)
# Read the IPUMS Data
cps_ddi_file = "./data_raw/cps_00002.xml"
cps_data_file = "./data_raw/cps_00002.dat"
# Contains metadata, nice to have as separate object
cps_ddi = read_ipums_ddi(cps_ddi_file) 
cps_data = read_ipums_micro(cps_ddi_file, data_file = cps_data_file)
# Read in the Industry Codes
industries = read_csv("./data_raw/indnames.csv")
# Make colNames uppercase
names(industries) = toupper(names(industries))
# Merge in Industry names
merged_data = merge(cps_data, industries, by = "IND", all=FALSE)
# Group EMPSTAT into binary groups: employed or not as ISEMP
merged_data = merged_data %>% 
  mutate(ISEMP = case_when(EMPSTAT %in% c(10, 12) ~ TRUE,
                           EMPSTAT %in% c(20, 21, 22, 30, 31, 32, 
                                          33, 34, 35, 36) ~ FALSE)) %>% 
  drop_na(ISEMP)
# This dataset will specifically keep industry and demographic data on workers. COVID
# variable to be added later.
merged_data = merged_data %>% select(YEAR, MONTH, INDNAME, ISEMP, AGE, SEX, EDUC)
# Sanity Check.
vtable(merged_data)
# Make a YEAR/MONTH variable
merged_data = merged_data %>% mutate(YEARMONTH = ym(YEAR * 100 + MONTH))
# Sanity Check
vtable(merged_data)
# Focus on the impact of this change on adults. Removing anyone below the age of 18.
merged_data = merged_data %>% filter(AGE >= 18)
# Group EDUC into groups: no_diploma, al_high_school, al_associates, al_bachelors,
# al_masters, al_doctorate. al stands for at least.
merged_data = merged_data %>% 
  mutate(EDUC = case_when(EDUC %in% c(002, 010, 011, 012, 013, 014, 020, 021, 022, 
                                      030, 031, 032, 040, 050, 060, 070, 071, 
                                      072) ~ "no_diploma",
                          EDUC %in% c(073, 080, 081, 090) ~ "al_high_school",
                          EDUC %in% c(091, 092, 100, 110, 124) ~ "al_associates",
                          EDUC %in% c(111, 120, 121, 122) ~ "al_bachelors",
                          EDUC %in% c(123) ~ "al_masters",
                          EDUC %in% c(125) ~ "al_doctorate")) %>% 
  drop_na(EDUC)
# Now reassign sex.
merged_data = merged_data %>% mutate(SEX = case_when(SEX == 1 ~ "male",
                                                     SEX == 2 ~ "female")) %>% 
  drop_na(SEX)
# Sanity Check
vtable(merged_data)
# Bin age into common brackets
merged_data = merged_data %>% 
  mutate(AGE = case_when(AGE >= 18 & AGE < 25 ~ "18-24",
                         AGE >= 25 & AGE < 35 ~ "25-34",
                         AGE >= 35 & AGE < 45 ~ "35-44",
                         AGE >= 45 & AGE < 55 ~ "45-54",
                         AGE >= 55 & AGE < 65 ~ "55-64",
                         AGE >= 65 ~ "65+")) %>% 
  drop_na(AGE)
# Factor the categoricals so far.
merged_data = merged_data %>% 
  mutate(YEAR = factor(YEAR)) %>% 
  mutate(MONTH = factor(MONTH)) %>% 
  mutate(INDNAME = factor(INDNAME)) %>% 
  mutate(AGE = factor(AGE)) %>% 
  mutate(SEX = factor(SEX)) %>% 
  mutate(EDUC = factor(EDUC))
# Sanity Check
vtable(merged_data)
# The main research questions involve the effect of COVID breaking it into a before
# and after 03/2020 Difference in Difference question. Therefore a variable relating
# to these dates will be created. https://www.cdc.gov/mmwr/volumes/69/wr/mm6935a2.htm
# The above website was used to decide that dropping data from March, April, and May 
# of 2020 makes sense as that was the time for the majority of the stay-at-home orders.
merged_data = merged_data %>% 
  mutate(AFTERCOVID = case_when(YEARMONTH <= ymd("2020-02-01") ~ FALSE,
                                YEARMONTH >= ymd("2020-06-01") ~ TRUE)) %>% 
  drop_na(AFTERCOVID)
vtable(merged_data)
# Get counts relative to YEARMONTH and INDNAME for questions 1 and 2.
merged_data = merged_data %>% 
  group_by(YEARMONTH, INDNAME) %>% 
  mutate(EMPCOUNT = sum(ISEMP))
# This data can now be used with a binary dependent variable ISEMP to help
# answer question 3.
write_csv(merged_data, "./data_processed/cps_industry_and_demographic_data.csv")
# Make another quick access dataset that is just EMP counts at the YEARMONTH & INDNAME level.
converted_data = merged_data %>% group_by(YEARMONTH, INDNAME) %>% summarize(sum(ISEMP))
vtable(converted_data)
# This data can be used to answer question 2
write_csv(converted_data, "./data_processed/emp_count_by_industry.csv")
# Make a dataset specific to retail
retail_data = converted_data %>% filter(INDNAME == "Retail Trade")
write_csv(retail_data, "./data_processed/emp_count_by_retail_trade.csv")

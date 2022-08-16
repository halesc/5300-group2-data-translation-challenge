# Data Exploration
# Group 2
# 2022-08-16

# Libraries
library(ipumsr)
library(tidyverse)

# Read the IPUMS Data
cps_ddi_file = "./data_raw/cps_00002.xml"
cps_data_file = "./data_raw/cps_00002.dat"
# Contains metadata, nice to have as separate object
cps_ddi = read_ipums_ddi(cps_ddi_file) 
cps_data = read_ipums_micro(cps_ddi_file, data_file = cps_data_file)
# Read in the Industry Codes
industries = read_csv("./data_raw/indnames.csv")
# Make colNames uppercase
names(industries) <- toupper(names(industries))
# Merge in Industry names
cps_data = merge(cps_data, industries, by = "IND", all=FALSE)

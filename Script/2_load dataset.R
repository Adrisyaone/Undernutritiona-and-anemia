#----------------- Install, load libraries and datasets -------------------

# Important documents
# to join datasets: https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Analyzing_DHS_Data.htm
# to access dataset: https://www.dhsprogram.com/data/dataset/Nepal_Standard-DHS_2022.cfm?flag=0
# to access reports:
                #  2022: https://dhsprogram.com/pubs/pdf/FR379/FR379.pdf



# Clear environment
rm(list=ls())


# load packages and functions
source("Script/1_Functions.R")

# load datasets
# load PR data from 2022
PRdata2022 <- read_dta("Datasets/Raw data/NP_2022_DHS_10282024_1914_183234/NPPR82DT/NPPR82FL.DTA")


# load KR data from 2022
KRdata2022 <- read_dta("Datasets/Raw data/NP_2022_DHS_10282024_1914_183234/NPKR82DT/NPKR82FL.DTA")



# load IR data from  2022
IRdata2022 <- read_dta("Datasets/Raw data/NP_2022_DHS_10282024_1914_183234/NPIR82DT/NPIR82FL.DTA")


# load br data from 2022
BRdata2022 <- read_dta("Datasets/Raw data/NP_2022_DHS_10282024_1914_183234/NPBR82DT/NPBR82FL.DTA")



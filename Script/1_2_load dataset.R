#----------------- Install, load libraries and datasets -------------------

# Important documents
# to join datasets: https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Analyzing_DHS_Data.htm
# to access dataset: https://www.dhsprogram.com/data/dataset/Nepal_Standard-DHS_2022.cfm?flag=0
# to access reports:
                #  2016: https://www.dhsprogram.com/pubs/pdf/fr336/fr336.pdf
                #  2011: https://dhsprogram.com/pubs/pdf/FR257/FR257[13April2012].pdf
                #  2022: https://dhsprogram.com/pubs/pdf/FR379/FR379.pdf



# Clear environment
rm(list=ls())


# load packages and functions
source("Script/1_1_Functions.R")

# load datasets
# load PR data from 2011, 2016 and 2022
PRdata2022 <- read_dta("Datasets/Raw data/NP_2022_DHS_10282024_1914_183234/NPPR82DT/NPPR82FL.DTA")
PRdata2016 <- read_dta("Datasets/Raw data/NP_2016_DHS_10282024_1517_183234/NPPR7HDT/NPPR7HFL.DTA")
PRdata2011 <- read_dta("Datasets/Raw data/NP_2011_DHS_10282024_1518_183234/NPPR61DT/NPPR61FL.DTA")

# load KR data from 2011, 2016 and 2022
KRdata2022 <- read_dta("Datasets/Raw data/NP_2022_DHS_10282024_1914_183234/NPKR82DT/NPKR82FL.DTA")
KRdata2016 <- read_dta("Datasets/Raw data/NP_2016_DHS_10282024_1517_183234/NPKR7HDT/NPKR7HFL.DTA")
KRdata2011 <- read_dta("Datasets/Raw data/NP_2011_DHS_10282024_1518_183234/NPKR61DT/NPKR61FL.DTA")


# load KR data from 2011, 2016 and 2022
IRdata2022 <- read_dta("Datasets/Raw data/NP_2022_DHS_10282024_1914_183234/NPIR82DT/NPIR82FL.DTA")
IRdata2016 <- read_dta("Datasets/Raw data/NP_2016_DHS_10282024_1517_183234/NPIR7HDT/NPIR7HFL.DTA")
IRdata2011 <- read_dta("Datasets/Raw data/NP_2011_DHS_10282024_1518_183234/NPIR61DT/NPIR61FL.DTA")

# load br data from 2011, 2016 and 2022
BRdata2022 <- read_dta("Datasets/Raw data/NP_2022_DHS_10282024_1914_183234/NPBR82DT/NPBR82FL.DTA")
BRdata2016 <- read_dta("Datasets/Raw data/NP_2016_DHS_10282024_1517_183234/NPBR7HDT/NPBR7HFL.DTA")
BRdata2011 <- read_dta("Datasets/Raw data/NP_2011_DHS_10282024_1518_183234/NPBR61DT/NPBR61FL.DTA")
# remove unnecessary objects from environment
rm(list=c("new_packages"))

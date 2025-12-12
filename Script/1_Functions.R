#----------------- Install, load libraries and datasets -------------------

# Important documents
# to join datasets: https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Analyzing_DHS_Data.htm
# to access dataset: https://www.dhsprogram.com/data/dataset/Nepal_Standard-DHS_2022.cfm?flag=0
# to access reports:
                #  2022: https://dhsprogram.com/pubs/pdf/FR379/FR379.pdf



# Clear environment
rm(list=ls())

# Step-1: Install and load library

Packages <- c("sjmisc","sf","ggspatial", "expss","naniar","gtsummary","foreign","survey",'labelled',"readxl", "tidyverse", "haven","rockchalk", "forcats", "data.table", "srvyr", "marginaleffects")

new_packages <- Packages[!(Packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)




# load libraries
lapply(Packages, require, character.only=T)


# remove unnecessary objects from environment
rm(list=c("new_packages", "Packages"))





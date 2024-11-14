rm(list=ls())

# load functions and packages
source("Script/1_1_Functions.R")

# use of survyr package

# load processed dataset
dt<-readRDS("Datasets/Processed data/Cleaned_datasetV1.RDS")







# generate survey design

svydt <- dt |> 
  as_survey_design(weights = Sampling_wt, ids = ids)

# 
# svymean(design = svydt, x = ~Ecological_region, na.rm = T)
# 
# svyciprop(~I(Ecological_region=="mountain"), method = "li", design = svydt)
# 
# 
# 
# 
# svytable(formula = ~Ecological_region+Year+stunting, svydt)
# 
# k<-prop.table(svytable(formula = ~Ecological_region+stunting+Year, svydt),1)
# data.frame(k)
# 
# svyciprop(~I(Ecological_region=="mountain"), method = "mean", design = svydt)
# estimates <- svyby(~Ecological_region,by = "stunting", design = svydt, FUN = svymean)
# 
# 
# data.frame(estimates)
# 
# 
# library("survey")
# design <- svydesign(
#   data =dt ,
#   ids = ~ids,
#   weights = ~Sampling_wt,
#   nest = TRUE
# )
# 
# dsub <- subset(design,Year=="2011")
# 
# k<-svyciprop(~I(stunting=="1"), method = "li", design = dsub)
# 
# 
# # Compare race distribution by sex
# tbl_svysummary(data = design, by ="Ecological_region" ,include = "stunting", percent = "column") |> 
#   add_ci()
# 
# library(surveytable)

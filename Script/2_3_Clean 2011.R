rm(list=ls())

# load Script 1
source("Script/1 Functions, load datasets and packages.R")

# PR data processing
PRdata<-PRdata2011

# Clean Outcome variables
# //Severely stunted
PRdata <- PRdata %>%
  mutate(nt_ch_sev_stunt =
           case_when(
             hv103==1 &  hc70< -300  ~ 1 ,
             hv103==1 &  hc70>= -300 & hc70<9996 ~ 0 ,
             hc70>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_sev_stunt = c(99))) %>%
  set_value_labels(nt_ch_sev_stunt = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_sev_stunt = "Severely stunted child under 5 years")



# //Stunted
PRdata <- PRdata %>%
  mutate(nt_ch_stunt =
           case_when(
             hv103==1 &  hc70< -200  ~ 1 ,
             hv103==1 &  hc70>= -200 & hc70<9996 ~ 0 ,
             hc70>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_stunt = c(99))) %>%
  set_value_labels(nt_ch_stunt = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_stunt = "Stunted child under 5 years")



# //Mean haz
PRdata <- PRdata %>%
  mutate(haz = case_when(hv103 ==1 & hc70<996 ~ hc70/100)) 
PRdata$nt_ch_mean_haz <- matrixStats::weightedMean(PRdata$haz, PRdata$wt, idxs = NULL, na.rm = TRUE) 

# //Severely wasted 
PRdata <- PRdata %>%
  mutate(nt_ch_sev_wast =
           case_when(
             hv103==1 &  hc72< -300  ~ 1 ,
             hv103==1 &  hc72>= -300 & hc72<9996 ~ 0 ,
             hc72>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_sev_wast = c(99))) %>%
  set_value_labels(nt_ch_sev_wast = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_sev_wast = "Severely wasted child under 5 years")



# //Wasted
PRdata <- PRdata %>%
  mutate(nt_ch_wast =
           case_when(
             hv103==1 &  hc72< -200  ~ 1 ,
             hv103==1 &  hc72>= -200 & hc72<9996~ 0 ,
             hc72>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_wast = c(99))) %>%
  set_value_labels(nt_ch_wast = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_wast = "Wasted child under 5 years")



# //Overweight for height
PRdata <- PRdata %>%
  mutate(nt_ch_ovwt_ht =
           case_when(
             hv103==1 &  hc72> 200 & hc72<9996 ~ 1 ,
             hv103==1 &  hc72<= 200 & hc72<9996 ~ 0 ,
             hc72>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_ovwt_ht = c(99))) %>%
  set_value_labels(nt_ch_ovwt_ht = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_ovwt_ht = "Overweight for height child under 5 years")



# //Mean whz
PRdata <- PRdata %>%
  mutate(whz = case_when(hv103 ==1 & hc72<996 ~ hc72/100)) 
PRdata$nt_ch_mean_whz <- matrixStats::weightedMean(PRdata$whz, PRdata$wt, idxs = NULL, na.rm = TRUE) 



# //Severely underweight
PRdata <- PRdata %>%
  mutate(nt_ch_sev_underwt =
           case_when(
             hv103==1 &  hc71< -300  ~ 1 ,
             hv103==1 &  hc71>= -300 & hc71<9996 ~ 0 ,
             hc71>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_sev_underwt = c(99))) %>%
  set_value_labels(nt_ch_sev_underwt = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_sev_underwt = "Severely underweight child under 5 years")



# //Underweight
PRdata <- PRdata %>%
  mutate(nt_ch_underwt =
           case_when(
             hv103==1 &  hc71< -200  ~ 1 ,
             hv103==1 &  hc71>= -200  & hc71<9996 ~ 0 ,
             hc71>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_underwt = c(99))) %>%
  set_value_labels(nt_ch_underwt = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_underwt = "Underweight child under 5 years")
# 
# //Overweight for age
PRdata <- PRdata %>%
  mutate(nt_ch_ovwt_age =
           case_when(
             hv103==1 &  hc71> 200 & hc71<9996 ~ 1 ,
             hv103==1 &  hc71<= 200  & hc71<9996 ~ 0 ,
             hc71>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_ovwt_age = c(99))) %>%
  set_value_labels(nt_ch_ovwt_age = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_ovwt_age = "Overweight for age child under 5 years")

# //Mean waz
PRdata <- PRdata %>%
  mutate(waz = case_when(hv103 ==1 & hc71<996 ~ hc71/100)) 
PRdata$nt_ch_mean_waz <- matrixStats::weightedMean(PRdata$waz, PRdata$wt, idxs = NULL, na.rm = TRUE) 


# //Anemia indicators

# //Any anemia
PRdata <- PRdata %>%
  mutate(nt_ch_any_anem =
           case_when(
             hv103==1 & hc1>5 & hc1<60 & hc56<110 ~ 1 ,
             hv103==1 & hc1>5 & hc1<60 & hc56>=110 ~ 0)) %>%
  set_value_labels(nt_ch_any_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_any_anem = "Any anemia - child 6-59 months")

# //Mild anemia
PRdata <- PRdata %>%
  mutate(nt_ch_mild_anem =
           case_when(
             hv103==1 & hc1>5 & hc1<60 & hc56>99 & hc56<110 ~ 1 ,
             hv103==1 & hc1>5 & hc1<60 & hc56<=99 | hc56>=110 ~ 0)) %>%
  set_value_labels(nt_ch_mild_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_mild_anem = "Mild anemia - child 6-59 months")

# //Moderate anemia
PRdata <- PRdata %>%
  mutate(nt_ch_mod_anem =
           case_when(
             hv103==1 & hc1>5 & hc1<60 & hc56>69 & hc56<100 ~ 1 ,
             hv103==1 & hc1>5 & hc1<60 & hc56<=69 | hc56>=100 ~ 0)) %>%
  set_value_labels(nt_ch_mod_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_mod_anem = "Moderate anemia - child 6-59 months")

# //Severe anemia
PRdata <- PRdata %>%
  mutate(nt_ch_sev_anem =
           case_when(
             hv103==1 & hc1>5 & hc1<60 & hc56<70 ~ 1 ,
             hv103==1 & hc1>5 & hc1<60 & hc56>=70 ~ 0)) %>%
  set_value_labels(nt_ch_sev_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_sev_anem = "Severe anemia - child 6-59 months")


# Demographic variables
PRdata <- PRdata %>%
  mutate(agemonths = case_when(hc1<6~ 1, hc1%in%c(6,7,8)~ 2, hc1%in%c(9,10,11)~ 3, hc1>=12&hc1<=17~ 4, 
                               hc1>=18&hc1<=23~ 5, hc1>=24&hc1<=35~ 6, hc1>=36&hc1<=47~ 7, hc1>=48&hc1<=59~ 8),
         shdist=shdistrict,
         education=factor(hc61, levels=c(0:3), labels=c("No education", "Primary", "Secondary", "Above")),
         hv024==NA) %>%
  set_value_labels(agemonths = c("<6"=1, "6-8"=2, "9-11"=3, "12-17"=4, "18-23"=5, "24-35"=6, "36-47"=7, "48-59"=8 )) %>%
  set_variable_labels(agemonths = "Age of child months categories") |>
  filter(hc1>5 & hc1<60) |>
  filter(!is.na(nt_ch_any_anem) |!is.na(nt_ch_stunt) ) |>
  filter(!is.na(nt_ch_stunt)) |>
  filter(!is.na(nt_ch_any_anem)) |>
  filter(!is.na(nt_ch_wast))

# PRdata <- forcats::as_factor(PRdata, only_labelled = TRUE)

PRdata11<-PRdata |> 
  mutate(Year=2011) |> 
  select(Year,hv021, hv022, hv005, agemonths, hc27, shecoreg, shdist,hv025, hv024,education, hv270, nt_ch_ovwt_ht, nt_ch_mean_haz, nt_ch_mean_whz, nt_ch_underwt, nt_ch_sev_underwt,nt_ch_stunt,nt_ch_sev_stunt,nt_ch_sev_wast,nt_ch_wast,nt_ch_ovwt_age,nt_ch_any_anem, nt_ch_mild_anem, nt_ch_mod_anem, nt_ch_sev_anem)


saveRDS(PRdata11, "Datasets/Processed data/PRdata2011.RDS")

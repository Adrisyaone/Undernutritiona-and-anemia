rm(list=ls())

# load packages, datasets and functions
source("Script/1_Functions.R")
source("Script/2_load dataset.R")

# PR data processing
PRdata<-PRdata2022

# # Clean Outcome variables from PR data
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



# // Demographic variables
PRdata <- PRdata %>%
  mutate(agemonths = case_when(hc1<6~ 1, hc1%in%c(6,7,8)~ 2, hc1%in%c(9,10,11)~ 3, hc1>=12&hc1<=17~ 4, 
                               hc1>=18&hc1<=23~ 5, hc1>=24&hc1<=35~ 6, hc1>=36&hc1<=47~ 7, hc1>=48&hc1<=59~ 8),
        education=ifelse(hc61==1 |hc61==2|hc61==3 |hc61==4 | hc61==5, 2,ifelse(hc61==0, 1, NA)),
        education=factor(education, levels=c(1,2), labels=c("No education", "With Education")), # not working well
         score=hfs1+hfs2+hfs3+hfs4+hfs5+hfs6+hfs7+hfs8,
         hfs=case_when((hfs1==1 | hfs2==1 | hfs3==1)& hfs4==0 & hfs5==0 & hfs6==0 & hfs7==0 & hfs8==0 ~ 2,
                       (hfs4==1 | hfs5==1 | hfs6==1) & hfs7==0 & hfs8==0 ~ 3,
                       hfs7==1 | hfs8==1 ~ 4,
                       hfs1==0 & hfs2==0 & hfs3==0 & hfs4==0 & hfs5==0 & hfs6==0 & hfs7==0 & hfs8==0 ~1),
         hfs=factor(hfs, levels=c(1:4), labels=c("No", "Mild", "Moderate", "Severe"))
                       ) %>% 
  set_value_labels(agemonths = c("<6"=1, "6-8"=2, "9-11"=3, "12-17"=4, "18-23"=5, "24-35"=6, "36-47"=7, "48-59"=8 )) %>%
  set_variable_labels(agemonths = "Age of child months categories") # |>
  # filter(hc1>5 & hc1<60) |>
  # filter(!is.na(nt_ch_any_anem) |!is.na(nt_ch_stunt) ) |>
  # filter(!is.na(nt_ch_stunt)) |>
  # filter(!is.na(nt_ch_any_anem)) |>
  # filter(!is.na(nt_ch_wast))

#################IR data procesisng#######################
IRdata <- IRdata2022 %>%
  mutate(nt_wm_sev_anem =
           case_when(
             v042==1 & v457==1 ~ 1 ,
             v042==1 &  v455==0 ~ 0)) %>%
  set_value_labels(nt_wm_sev_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_sev_anem = "Severe anemia - women")

# *** Anthropometry indicators ***

# * age of most recent child
# age of child. If b19_01 is not available in the data use v008 - b3_01
if ("TRUE" %in% (!("b19_01" %in% names(IRdata))))
  IRdata [[paste("b19_01")]] <- NA
if ("TRUE" %in% all(is.na(IRdata $b19_01)))
{ b19_included <- 0} else { b19_included <- 1}

if (b19_included==1) {
  IRdata <- IRdata %>%
    mutate(age = b19_01)
} else {
  IRdata <- IRdata %>%
    mutate(age = v008 - b3_01)
}

# //Height less than 145cm
IRdata <- IRdata %>%
  mutate(nt_wm_ht =
           case_when(
             v438<1450 & v438>=1300 & v438<=2200 ~ 1 ,
             v438>=1450 & v438>=1300 & v438<=2200  ~ 0)) %>%
  set_value_labels(nt_wm_ht = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_ht = "Height under 145cm - women")

# //Mean BMI
IRdata <- IRdata %>%
  mutate(bmi = case_when(v445>=1200 & v445<=6000 & v213==0 & (v208==0 | age>=2) ~ v445/100)) 
IRdata$nt_wm_bmi_mean <- matrixStats::weightedMean(IRdata$bmi, IRdata$wt, idxs = NULL, na.rm = TRUE) 


# //Normal weight
IRdata <- IRdata %>%
  mutate(nt_wm_norm =
           case_when(
             v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
             v445< 1850 | v445> 2499  ~ 0, 
             v445>=1850 & v445<=2499  ~ 1 )) %>%
  replace_with_na(replace = list(nt_wm_norm = c(99))) %>%
  set_value_labels(nt_wm_norm = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_norm = "Normal BMI - women")

# //Thin
IRdata <- IRdata %>%
  mutate(nt_wm_thin =
           case_when(
             v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
             v445>= 1850   ~ 0, 
             v445>=1200 & v445<1850  ~ 1 )) %>%
  replace_with_na(replace = list(nt_wm_thin = c(99))) %>%
  set_value_labels(nt_wm_thin = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_thin = "Thin BMI - women")

# //Mildly thin
IRdata <- IRdata %>%
  mutate(nt_wm_mthin =
           case_when(
             v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
             v445<1700 | v445>= 1850   ~ 0, 
             v445>=1700 & v445<1850  ~ 1 )) %>%
  replace_with_na(replace = list(nt_wm_mthin = c(99))) %>%
  set_value_labels(nt_wm_mthin = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_mthin = "Mildly thin BMI - women")

# //Moderately and severely thin
IRdata <- IRdata %>%
  mutate(nt_wm_modsevthin =
           case_when(
             v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
             v445>= 1700   ~ 0, 
             v445>=1200 & v445<1700  ~ 1 )) %>%
  replace_with_na(replace = list(nt_wm_modsevthin = c(99))) %>%
  set_value_labels(nt_wm_modsevthin = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_modsevthin = "Moderately and severely thin BMI - women")

# //Overweight or obese
IRdata <- IRdata %>%
  mutate(nt_wm_ovobese =
           case_when(
             v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
             v445< 2500   ~ 0, 
             v445>=2500 & v445<=6000  ~ 1 )) %>%
  replace_with_na(replace = list(nt_wm_ovobese = c(99))) %>%
  set_value_labels(nt_wm_ovobese = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_ovobese = "Overweight or obese BMI - women")

# //Overweight
IRdata <- IRdata %>%
  mutate(nt_wm_ovwt =
           case_when(
             v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
             v445< 2500  | v445>=3000 ~ 0, 
             v445>=2500 & v445<3000  ~ 1 )) %>%
  replace_with_na(replace = list(nt_wm_ovwt = c(99))) %>%
  set_value_labels(nt_wm_ovwt = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_ovwt = "Overweight BMI - women")

# //Obese
IRdata <- IRdata %>%
  mutate(nt_wm_obese =
           case_when(
             v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
             v445< 3000   ~ 0, 
             v445>=3000 & v445<=6000  ~ 1 )) %>%
  replace_with_na(replace = list(nt_wm_obese = c(99))) %>%
  set_value_labels(nt_wm_obese = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_obese = "Obese BMI - women")

IRdata <- IRdata %>%
  mutate(nt_wm_any_anem =
           case_when(
             v042==1 & v457<4 ~ 1 ,
             v042==1 &  v455==0 ~ 0)) %>%
  set_value_labels(nt_wm_any_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_any_anem = "Any anemia - women")
# //Mild anemia
IRdata <- IRdata %>%
  mutate(nt_wm_mild_anem =
           case_when(
             v042==1 & v457==3 ~ 1 ,
             v042==1 &  v455==0 ~ 0)) %>%
  set_value_labels(nt_wm_mild_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_mild_anem = "Mild anemia - women")

# //Moderate anemia
IRdata <- IRdata %>%
  mutate(nt_wm_mod_anem =
           case_when(
             v042==1 & v457==2 ~ 1 ,
             v042==1 &  v455==0 ~ 0)) %>%
  set_value_labels(nt_wm_mod_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_mod_anem = "Moderate anemia - women")

# //Severe anemia
IRdata <- IRdata %>%
  mutate(nt_wm_sev_anem =
           case_when(
             v042==1 & v457==1 ~ 1 ,
             v042==1 &  v455==0 ~ 0)) %>%
  set_value_labels(nt_wm_sev_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_sev_anem = "Severe anemia - women")

##########################################################


# Pull data from IR to PR
# // select required data from IR 
IRdata2022_sub<-IRdata |> 
  select(caseid,v190,v218,v012,v130,v013,v131,v171b, v157, v481, v701,v106,v743a, v743b, v743d , d106, d107, d108, nt_wm_obese, nt_wm_ovwt, nt_wm_ovobese, nt_wm_modsevthin, nt_wm_mthin, nt_wm_thin, nt_wm_norm, nt_wm_any_anem, nt_wm_mod_anem, nt_wm_sev_anem)


# // Generate ID to merge PR data with IR data 
PRdata<-PRdata |> 
  mutate(
    clusterID=ifelse(nchar(hv001)==1, paste0("       ", hv001), 
                        ifelse(nchar(hv001)==2, paste0("      ", hv001), 
                               ifelse(nchar(hv001)==3, paste0("     ", hv001),
                                      ifelse(nchar(hv001)==4, paste0("    ", hv001),
                                             ifelse(nchar(hv001)>5, paste0("NV", hv001), NA))))),
    household_id=ifelse(nchar(hv002)==1, paste0("   ", hv002), 
                        ifelse(nchar(hv002)==2, paste0("  ", hv002), 
                               ifelse(nchar(hv002)==3, paste0(" ", hv002), NA))),
    line_id=ifelse(nchar(hc60)==1, paste0("  ", hc60), 
                   ifelse(nchar(hc60)==2, paste0(" ", hc60), NA)),
    id=paste0(clusterID,household_id),
    mother_id=paste0(id, line_id)) |> 
  left_join(IRdata2022_sub, by=c("mother_id"="caseid"))


# // Generate ID in PRdata to join with the KR data
PRdata<-PRdata |> 
  mutate(idx=ifelse(nchar(hvidx)==1, paste0("  ", hvidx), ifelse(nchar(hvidx)==2, paste0(" ", hvidx), NA)),
         caseid=paste0(hhid, idx))



# // Process KR data to generate MDD to merge with PR data
dt<-KRdata2022
dt<-dt |> 
  mutate(clusterID=ifelse(nchar(v001)==1, paste0("       ", v001), 
                          ifelse(nchar(v001)==2, paste0("      ", v001), 
                                 ifelse(nchar(v001)==3, paste0("     ", v001),
                                        ifelse(nchar(v001)==4, paste0("    ", v001),
                                               ifelse(nchar(v001)>5, paste0("NV", v001), NA))))),
         household_id=ifelse(nchar(v002)==1, paste0("   ", v002), 
                             ifelse(nchar(v002)==2, paste0("  ", v002), 
                                    ifelse(nchar(v002)==3, paste0(" ", v002), NA))),
         line_id=ifelse(nchar(b16)==1, paste0("  ", b16), 
                        ifelse(nchar(b16)==2, paste0(" ", b16), NA)),
         id=paste0(clusterID,household_id),
         child_id=paste0(id, line_id))

dt<-data.table::setDT(dt)
dt <- dt |> filter(b19 < 24 & b9 == 0)
dt <- dt %>%
  filter(caseid != lag(caseid) | is.na(lag(caseid)))
dt <- dt %>%
  filter(row_number() == 1 | caseid != lag(caseid) | is.na(lag(caseid)))
dt <- dt |> filter(b19 >= 6)
dt <- dt |> mutate(breastmilk = ifelse(m4 == 95,1,0))
dt[is.na(breastmilk), breastmilk := 0]
dt <- dt |> mutate(grains = ifelse(v414e == 1 | v414f == 1,1,0))
dt[is.na(grains), grains := 0]
dt <- dt |> mutate(legumes = ifelse(v414o == 1 | v414c == 1,1,0))
dt[is.na(legumes), legumes := 0]
dt <- dt |> mutate(dairy = ifelse( v411 == 1 | v411a == 1 | v414v == 1 | v414p == 1 | v413a == 1 , 1,0))
dt[is.na(dairy), dairy := 0]
#v411 == 1 | v411a == 1 |
dt <- dt |> mutate(flesh = ifelse(v414h == 1 | v414m == 1 | v414n == 1 | v414b == 1 , 1,0))
dt[is.na(flesh), flesh := 0]
dt <- dt |> mutate(egg = ifelse(v414g == 1,1,0))
dt[is.na(egg), egg := 0]
dt <- dt |> mutate(fruits = ifelse(v414i == 1 | v414j == 1 | v414k == 1 | v414wa == 1, 1 , 0))
dt[is.na(fruits), fruits := 0]
dt <- dt |> mutate(other = ifelse(v414a == 1 | v414l == 1, 1 , 0))
dt[is.na(other), other := 0]
dt <- dt |> mutate(minimumDV = ifelse((breastmilk + grains + legumes + dairy + flesh + egg + fruits + other) >= 5, "Yes","No"))

# //Select variables from KR data to merge with PR data
dt<-dt |> 
  data.frame() |> 
  select(child_id, breastmilk,grains,legumes,dairy,flesh, egg,fruits,other, minimumDV)


# // merge KR data and PR data
PRdata<-PRdata |> 
  left_join(dt, by=c("caseid"="child_id"))




# // Processing BR data to pull some characteritics of children during their birth

brdt22<-BRdata2022 |> 
  mutate(
    clusterID=ifelse(nchar(v001)==1, paste0("       ", v001), 
                     ifelse(nchar(v001)==2, paste0("      ", v001), 
                            ifelse(nchar(v001)==3, paste0("     ", v001),
                                   ifelse(nchar(v001)==4, paste0("    ", v001),
                                          ifelse(nchar(v001)>5, paste0("NV", v001), NA))))),
    household_id=ifelse(nchar(v002)==1, paste0("   ", v002), 
                        ifelse(nchar(v002)==2, paste0("  ", v002), 
                               ifelse(nchar(v002)==3, paste0(" ", v002), NA))),
    line_id=ifelse(nchar(b16)==1, paste0("  ", b16), 
                   ifelse(nchar(b16)==2, paste0(" ", b16), NA)),
    id=paste0(clusterID,household_id),
    child_id=paste0(id, line_id),
    age_child=b19,
    health_program=ifelse(s1115fa==1|s1115fb==1|s1115fc==1|s1115fd==1|s1115fe==1|s1115ff==1|s1115fg==1|s1115fh==1,1, 0),
    health_program=factor(health_program, levels=c(0,1), labels=c("No", "Yes")),
    
  ) |> 
  select(child_id, age_child, m15, m17, m18, health_program)

PRdata<-PRdata |> 
  left_join(brdt22, by=c("caseid"="child_id"))





# // Select only required variables to pull to main dataset
PRdata22<-PRdata |>
  mutate(Year=2022) |> 
  select(caseid,mother_id, Year, hv021, hv022, hv005, agemonths, hc27, hv025,shecoreg,shdist,education, hv024, hv270,hc68, nt_ch_ovwt_ht, waz, haz, whz,nt_ch_underwt,nt_ch_sev_underwt,nt_ch_stunt,nt_ch_sev_stunt,nt_ch_sev_wast,nt_ch_wast,nt_ch_ovwt_age,nt_ch_any_anem, nt_ch_mild_anem, nt_ch_mod_anem, nt_ch_sev_anem,hfs, v190,v218,v012,v013,v130,v131,v171b, v157, v481, v701,v106,hc1,v743a, v743b, v743d,breastmilk, grains,legumes,dairy,flesh, egg,fruits,other, minimumDV, m15, m17, m18, health_program, nt_wm_obese, nt_wm_ovwt, nt_wm_ovobese, nt_wm_modsevthin, nt_wm_mthin, nt_wm_thin, nt_wm_norm, nt_wm_any_anem, nt_wm_mod_anem, nt_wm_sev_anem)




saveRDS(PRdata22, "Datasets/Processed data/PRdata2022.RDS")





# Next level of cleaning 
# load datasets
PRdata22<-readRDS("Datasets/Processed data/PRdata2022.RDS")

# strata
PRdata22$strata <-PRdata22$hv022


# v701 (husband's education)--> converted education 0/1 for harmonoization.
PRdata22$husband_edu<-ifelse(PRdata22$v701==0, 0, 1)

# v106 (mother's education)--> converted education 0/1 for harmonoization.
PRdata22$edu<-ifelse(PRdata22$v106==0, 0, 1)






# rename variable names
PRdata<-PRdata22 |> 
  rename("Child_id"="caseid",
         "PSU"="hv021",
         "Sampling_wt"="hv005",
         "Sex"="hc27",
         "Place_of_residence"="hv025",
         "Ecological_region"="shecoreg",
         "District"="shdist",
         "Mother_education"="education",
         "Wealth_quintile"="hv270",
         "household_food_security"="hfs",
         "Other_foods"="other"
  )


# provide id to row as row_id
PRdata<-rownames_to_column(PRdata, "row_id")
PRdata$ids<-paste0(PRdata$PSU, "_", PRdata$Year)


# select only required variables for later use and order in sequential order
PRdata<-PRdata |> 
  select(row_id,Child_id, mother_id, Year, ids, strata, PSU, Sampling_wt, hv024,District,Ecological_region,Place_of_residence,  Wealth_quintile, agemonths, Sex, Mother_education,hc68, edu, husband_edu, breastmilk:minimumDV, household_food_security, haz, whz, waz, whz,  nt_ch_ovwt_ht:nt_ch_sev_anem, v190:v743d, m17,m18, m15, v012, health_program, nt_wm_obese, nt_wm_ovwt, nt_wm_ovobese, nt_wm_modsevthin, nt_wm_mthin, nt_wm_thin, nt_wm_norm, nt_wm_any_anem)


# set sampling wt to correct length and filter the data of 
PRdata <- PRdata %>%
  mutate(combined_indicator=paste0(nt_ch_stunt, nt_ch_wast, nt_ch_any_anem, nt_ch_underwt),
         Sampling_wt=Sampling_wt/1000000,
         
         Province=factor(hv024, levels=c(1:7), labels=c("Koshi", "Madhesh", "Bagmati", "Gandaki", "Lumbini", "Karnali", "Sudurpashchim")),
         
         # mother education
         motheredu = ifelse(hc68==0, 0, ifelse(hc68==1|hc68==2, 1, ifelse(hc68==3|hc68==4|hc68==5, 2,  NA))),
         motheredu = factor(motheredu, levels = c(0:2), labels=c("No education", "basic ", "Secondary and Higher")),
         
         # father education
         fatheredu = ifelse(v701==0, 0, ifelse(v701==1, 1, ifelse(v701==2|v701==3, 2, NA))),
         fatheredu<-factor(fatheredu, levels = c(0:2), labels=c("No education", "basic ", "Secondary and Higher")),
         
         # anemia
         Anemia01=ifelse(nt_ch_any_anem==1, 1,0),
         Anemia2=ifelse(nt_ch_any_anem==1, "A","NNA"),
         Anemia = factor(Anemia01, levels = c(0,1), labels=c("No", "Yes")),
         
         # undernutrition
         stunting=ifelse(nt_ch_stunt==1, 1,0),
         wasting=ifelse(nt_ch_wast==1, 1,0),
         underwt=ifelse(nt_ch_underwt==1, 1,0),
         stunting2=ifelse(nt_ch_stunt==1, "S","NS"),
         wasting2=ifelse(nt_ch_wast==1, "W","NW"),
         underwt2=ifelse(nt_ch_underwt==1, "UW","NUW"),
         
   
         
         under_nut_score=stunting+wasting+underwt,
         under_nut_multinom=factor(under_nut_score, levels=c(0:3), labels=c("None", "One condition", "Two conditions", "Three conditions")),
         undernut_number_ane_comb=paste0(under_nut_score, Anemia),
         undernut_number_ane_comb_multi=factor(undernut_number_ane_comb, levels=c("00","01","10","11","20","21","30","31" ), labels=c("None", "Anemia only", "One condition of undernutrtion", "One cond+anemia", "2 condition+no anemia", "2 condition+anemia", "3 condition+no anemia", "3 condition+anemia")),
         under_nut_binary=ifelse(under_nut_score>0, 1, 0),
         under_nut_binary2=ifelse(under_nut_score>0, "UN", "NN"),
         
         
         Combined1=paste(stunting2, wasting2, underwt2, Anemia2, sep = "--"),
         Combined1_1=as.factor(Combined1),
         Combined2= paste(under_nut_binary2,Anemia2, sep = "--"),
         Combined2_1=factor(Combined2, levels=c("NN--NNA","NN--A","UN--NNA", "UN--A"), labels=c("Normal", "Anemia only", "Undernutrition only", "Coexistence")),
         
         # stunting and anemia
         stunt_ane=paste0(stunting2,Anemia2 ),
         stunt_ane2=factor(stunt_ane, levels=c("NSNNA","NSA", "SNNA", "SA"), labels=c("Normal", "Anemia only","Stunting only", "both")),
         
         # WASTING AND  ANEMIA  
         wast_ane=paste0(wasting2,Anemia2 ),
         wast_ane2=factor(wast_ane, levels=c("NWNNA","NWA", "WNNA", "WA"), labels=c("Normal", "Anemia only","Wasting only", "both")),
         
         # underwt and anemia
         underwt_ane=paste0(underwt2,Anemia2 ),
         underwt_ane2=factor(underwt_ane, levels=c("NUWNNA","NUWA", "UWNNA", "UWA"), labels=c("Normal", "Anemia only","Underwt only", "both")),
         
         all_under_nutrition=paste0(stunting, wasting, underwt),
         
         all_under_nutrition=factor(all_under_nutrition, levels=c("000", "100", "010", "001", "110", "101", "011","111"), labels=c("None", "Stunting only", "Wasting only", "Underweight only", "stunting+wasting only", "Stunting and underweight only", "Wasting and underweight", "All")),
         
         
         # food insecurity
         hfs=ifelse(household_food_security=="No", 1, 0),
         hfs=factor(hfs, levels=c(0,1), labels=c("No", "Yes")),
         
         # Media exposure
         media_exp=ifelse(v157==0 , 0, 1),
         
         # birthweight
         birth_wt=ifelse(m18==2 |m18==1|m18==3, "Avergage or large", ifelse(m18==4, "Small", ifelse(m18==5, "Very small", NA))),
        
         # parity
          parity=ifelse(v218==0, "Nullipara", ifelse(v218==1, "Primipara", ifelse(v218>1,"Multipara", NA))),
         parity=factor(parity, levels=c("Nullipara","Primipara", "Multipara"),labels=c("Nullipara","Primipara", "Multipara")),
        
         # age
          age=ifelse(hc1<24, 1, 2),
         age2=ifelse(hc1<13, 1, ifelse(hc1>12 & hc1<36, 2, ifelse(hc1>35 & hc1<60, 3, NA))),
         age2=factor(age2, levels=c(1:3), labels=c("6-12 months", "1 year to 3 years", "4 to 5 years")),
         age=factor(age, levels=c(1,2), labels=c("6-23", "24-59")),
         
         # mothers nutritional status
         maternal_nut=ifelse(nt_wm_norm==1, 0, ifelse(nt_wm_ovobese==1, 1, ifelse(nt_wm_mthin==1 |nt_wm_thin==1 |nt_wm_modsevthin==1, 2, NA) )),
         maternal_nut=factor(maternal_nut, levels=c(0:2), labels=c("Normal", "Overwt obese", "thin"))
         
  ) |> 
  filter(hc1>5 & hc1<60) |>
  filter(!is.na(nt_ch_stunt)) |>
  filter(!is.na(nt_ch_any_anem) |!is.na(nt_ch_stunt) ) |>
  filter(!is.na(nt_ch_any_anem)) |>
  filter(!is.na(nt_ch_wast))



# clean some variables
# decision making 
PRdata <- PRdata %>% mutate(health =ifelse(v743a %in% c("respondent alone", "respondent and husband/partner", "respondent and other person"), 1, 0))
PRdata <- PRdata%>% mutate(pruchase = ifelse(v743b %in% c("respondent alone", "respondent and husband/partner", "respondent and other person"), 1, 0))
PRdata <- PRdata %>% mutate(visit = ifelse(v743d %in% c("respondent alone", "respondent and husband/partner", "respondent and other person"), 1, 0))

PRdata$x <- (PRdata$health + PRdata$pruchase + PRdata$visit)

PRdata <- PRdata %>% 
  mutate(housedecision = as.factor(ifelse(x == 0 ,"No participation", "Participation")))

#--------- Outcome variable
PRdata$OUTCOME<- ifelse(PRdata$combined_indicator=="000", 0, 
                        ifelse(PRdata$combined_indicator=="111", 3,
                               ifelse(PRdata$combined_indicator=="100"|PRdata$combined_indicator=="100"|PRdata$combined_indicator=="100", 1, ifelse(PRdata$combined_indicator=="110"|PRdata$combined_indicator=="101"|PRdata$combined_indicator=="011", 2, NA))))

PRdata$OUTCOME2<-factor(PRdata$OUTCOME, levels = c(0:3), labels = c("No", "1 condition", "2 conditions", "All"))


# save final cleaned dataset
saveRDS(PRdata, "Datasets/Processed data/Cleaned_datasetV1.RDS")


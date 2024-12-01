rm(list=ls())

# load Script 1
source("Script/1 Functions, load datasets and packages.R")


# PR data processing
PRdata<-PRdata2016

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
         education=factor(hc61, levels=c(0:3), labels=c("No education", "Primary", "Secondary", "Above")),
         
         
         hds_cat=case_when((sh145a==0 |sh145a==1) & (sh145b==0 & sh145c==0 & sh145d==0 & sh145e==0 & sh145f==0 & sh145g==0 & sh145h==0 & sh145i==0 )~1,
                           (sh145a==2 |sh145a==3 | sh145b==1 |sh145b==2 |sh145b==3| sh145c==1 |sh145d==1) & sh145e==0 & sh145f==0 & sh145g==0 & sh145h==0 & sh145i==0 ~2,
                           (sh145c==2 |sh145c==3 |sh145d==2 |sh145d==3 |sh145e==1 |sh145e==2 |sh145f==1 |sh145f==2) & sh145g==0 & sh145h==0 & sh145i==0 ~3,
         (sh145e==3 |sh145f==3 |sh145g==1 |sh145g==2 |sh145g==3 |sh145h==1 |sh145h==2 |sh145h==3 |sh145i==1 |sh145i==2 |sh145i==3)~4),
         
         hfs=factor(hds_cat, levels=c(1:4), labels=c("No", "Mild", "Moderate", "Severe"), ordered = T)) %>%
  set_value_labels(agemonths = c("<6"=1, "6-8"=2, "9-11"=3, "12-17"=4, "18-23"=5, "24-35"=6, "36-47"=7, "48-59"=8 )) %>%
  set_variable_labels(agemonths = "Age of child months categories") # |>
  # filter(hc1>5 & hc1<60) |>
  # filter(!is.na(nt_ch_any_anem) |!is.na(nt_ch_stunt) ) |>
  # filter(!is.na(nt_ch_stunt)) |>
  # filter(!is.na(nt_ch_any_anem)) |>
  # filter(!is.na(nt_ch_wast))


# Pull data from IR to PR
# // select required data from IR 
IRdata2016_sub<-IRdata2016 |> 
  select(caseid,v190,v218,v012,v130,v013,v131,v171b, v157, v481, v701,v106,v743a, v743b, v743d , d106, d107, d108)


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
  left_join(IRdata2016_sub, by=c("mother_id"="caseid"))



# // Generate ID in PRdata to join with the KR data
PRdata<-PRdata |> 
  mutate(idx=ifelse(nchar(hvidx)==1, paste0("  ", hvidx), ifelse(nchar(hvidx)==2, paste0(" ", hvidx), NA)),
         caseid=paste0(hhid, idx))



# // Process KR data to generate MDD to merge with PR data
dt<-KRdata2016

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


dt<-data.frame(dt)
dt <- dt |> filter(b19 < 60 & b9 == 0)
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
dt <- dt |> mutate(flesh = ifelse(v414h == 1 | v414m == 1 | v414n == 1 | v414b == 1 , 1,0))
dt[is.na(flesh), flesh := 0]
dt <- dt |> mutate(egg = ifelse(v414g == 1,1,0))
dt[is.na(egg), egg := 0]
dt <- dt |> mutate(fruits = ifelse(v414i == 1 | v414j == 1 | v414k == 1 | v414w == 1, 1 , 0))
dt[is.na(fruits), fruits := 0]
dt <- dt |> mutate(other = ifelse(v414a == 1 | v414l == 1, 1 , 0))
dt[is.na(other), other := 0]
dt <- dt |> mutate(minimumDV = ifelse((breastmilk + grains + legumes + dairy + flesh + egg + fruits + other) >= 5, "Yes","No"))


# //Select variables from KR data to merge with PR data
dt<-dt |> 
  select(child_id, breastmilk,grains,legumes,dairy,flesh, egg,fruits,other, minimumDV)

# // merge KR data and PR data
PRdata<-PRdata |> 
  left_join(dt, by=c("caseid"="child_id"))





# // Select only required variables to pull to main dataset
PRdata16<-PRdata |>
  mutate(Year=2016) |> 
  select(Year, hv021, hv022, hv005, agemonths, hc27, hv025,shecoreg,shdist, education, hv024, hv270, nt_ch_ovwt_ht, nt_ch_mean_haz, nt_ch_mean_whz,nt_ch_underwt,nt_ch_sev_underwt,nt_ch_stunt,nt_ch_sev_stunt,nt_ch_sev_wast,nt_ch_wast,nt_ch_ovwt_age,nt_ch_any_anem, nt_ch_mild_anem, nt_ch_mod_anem, nt_ch_sev_anem, ,hfs, v190,v218,v012,v013,v130,v131, v157, v481, v701,v106, hc1,  grains,legumes,dairy,flesh, egg,fruits,other, minimumDV)




# save final data of NDHS 2016 to processed folder
saveRDS(PRdata16, "Datasets/Processed data/PRdata2016.RDS")

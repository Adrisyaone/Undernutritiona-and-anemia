
# load libraries
source("Script/1_1_Functions.R")

# load datasets
PRdata22<-readRDS("Datasets/Processed data/PRdata2022.RDS")
PRdata16<-readRDS("Datasets/Processed data/PRdata2016.RDS")
PRdata11<-readRDS("Datasets/Processed data/PRdata2011.RDS")

#hv022 (strata)
PRdata22$strata<-paste(PRdata22$hv022,PRdata22$Year, sep = "_")
PRdata16$strata<-paste(PRdata16$hv022, PRdata16$Year, sep = "_")
PRdata11$strata<-paste(PRdata11$hv022, PRdata11$Year, sep = "_")


# hv024 (province)
PRdata22$Province<-as.numeric(PRdata22$hv024)
PRdata16$Province<-as.numeric(PRdata16$hv024)


# v701 (husband's education)
PRdata11$husband_edu<-ifelse(PRdata11$v701==0, 0, 1)
PRdata16$husband_edu<-ifelse(PRdata16$v701==0, 0, 1)
PRdata22$husband_edu<-ifelse(PRdata22$v701==0, 0, 1)


# v106 (mother's educaton)
PRdata11$edu<-ifelse(PRdata11$v106==0, 0, 1)
PRdata16$edu<-ifelse(PRdata16$v106==0, 0, 1)
PRdata22$edu<-ifelse(PRdata22$v106==0, 0, 1)



# merge datasets
PRdata<-bind_rows(PRdata22, PRdata16, PRdata11)


# rename variable names
PRdata<-PRdata |> 
  rename("Child_id"="caseid",
         "PSU"="hv021",
         "Sampling_wt"="hv005",
         "Sex"="hc27",
         "Place_of_residence"="hv025",
         "Ecological_region"="shecoreg",
         "District"="shdist",
         "Mother_education"="education",
         "Wealth_quintile"="hv270",
         "Mother_education2"="hc68",
         "household_food_security"="hfs",
         "Other_foods"="other"
         )
# order all variables in a sequential order

# provide id to row as row_id
PRdata<-rownames_to_column(PRdata, "row_id")
PRdata$ids<-paste0(PRdata$PSU, "_", PRdata$Year)


PRdata<-PRdata |> 
  select(row_id,Child_id, mother_id, Year, ids, strata, PSU, Sampling_wt, Province,District,Ecological_region,Place_of_residence,  Wealth_quintile, agemonths, Sex, Mother_education,Mother_education2, edu, husband_edu, breastmilk:minimumDV, household_food_security, nt_ch_ovwt_ht:nt_ch_sev_anem, v190:v743d )

# set sampling wt to correct length
PRdata <- PRdata %>%
  mutate(combined_indicator=paste0(nt_ch_stunt, nt_ch_wast, nt_ch_any_anem),
         Sampling_wt=Sampling_wt/1000000,
         Province=factor(Province, levels=c(1:7), labels=c("Koshi", "Madhesh", "Bagmati", "Gandaki", "Lumbini", "Karnali", "Sudurpashchim")),
         stunting=ifelse(nt_ch_stunt==1, 1,0),
         wasting=ifelse(nt_ch_wast==1, 1,0),
         Anemia=ifelse(nt_ch_any_anem==1, 1,0),
         stunting2=ifelse(nt_ch_stunt==1, "S","NS"),
         wasting2=ifelse(nt_ch_wast==1, "W","NW"),
         Anemia2=ifelse(nt_ch_any_anem==1, "A","NNA"),
         Combined=paste(stunting2, wasting2, Anemia2),
         cooccurence=ifelse(combined_indicator=="111",1, 0)) |> 
  filter(hc1>5 & hc1<60) |>
  filter(!is.na(nt_ch_stunt)) |>
  filter(!is.na(nt_ch_any_anem) |!is.na(nt_ch_stunt) ) |>
  filter(!is.na(nt_ch_any_anem)) |>
  filter(!is.na(nt_ch_wast))
  

PRdata<-forcats::as_factor(PRdata)

#---------
PRdata$OUTCOME<- ifelse(PRdata$combined_indicator=="000", 0, 
                        ifelse(PRdata$combined_indicator=="111", 3,
                               ifelse(PRdata$combined_indicator=="100"|PRdata$combined_indicator=="100"|PRdata$combined_indicator=="100", 1, ifelse(PRdata$combined_indicator=="110"|PRdata$combined_indicator=="101"|PRdata$combined_indicator=="011", 2, NA))))

PRdata$OUTCOME2<-factor(PRdata$OUTCOME, levels = c(0:3), labels = c("No", "1 condition", "2 conditions", "All"))


# save final cleaned dataset
saveRDS(PRdata, "Datasets/Processed data/Cleaned_datasetV1.RDS")


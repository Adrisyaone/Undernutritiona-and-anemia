#load libraries
library(haven)
library(sjmisc)
library(labelled)
library(survey)
library(tidyselect)
library(gtsummary)
library(tidyverse)
library(sf)
library(ggspatial)
library(expss)
library(naniar)

# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

# load PR data
PRdata <- read_dta("Datasets/Raw data/NDHS22/NPPR81DT/NPPR81FL.DTA")

# set sampling wt to correct length
PRdata <- PRdata %>%
  mutate(sampling_wt = hv005/1000000)



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

# *** Anemia indicators ***

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
                               hc1>=18&hc1<=23~ 5, hc1>=24&hc1<=35~ 6, hc1>=36&hc1<=47~ 7, hc1>=48&hc1<=59~ 8)) %>%
  set_value_labels(agemonths = c("<6"=1, "6-8"=2, "9-11"=3, "12-17"=4, "18-23"=5, "24-35"=6, "36-47"=7, "48-59"=8 )) %>%
  set_variable_labels(agemonths = "Age of child months categories") |> 
  filter(hc1>5 & hc1<60) |> 
  filter(!is.na(nt_ch_any_anem) |!is.na(nt_ch_stunt) ) |> 
  filter(!is.na(nt_ch_stunt)) |> 
  filter(!is.na(nt_ch_any_anem)) |> 
  filter(!is.na(nt_ch_wast))
  


# labelling data
PRdata <- forcats::as_factor(PRdata, only_labelled = TRUE)
# Extract variable names and their labels
# write.csv(t(data.frame(var_label(PRdata))), "children_label.csv", na = "")


PRdata<-PRdata |> 
  mutate(var=paste0(nt_ch_stunt, nt_ch_wast, nt_ch_any_anem))



# Descriptive statistics
svy_dt<-svydesign(data = PRdata, ids = ~hv021,strata = ~hv022, weights = ~sampling_wt)


svy_dt %>%
  tbl_svysummary(include =c("nt_ch_sev_stunt",'nt_ch_stunt', 'nt_ch_sev_wast','nt_ch_wast', 'nt_ch_ovwt_ht', 'nt_ch_sev_underwt', 'nt_ch_underwt', 'nt_ch_ovwt_age', 'nt_ch_any_anem', 'nt_ch_mild_anem', 'nt_ch_mod_anem', 'nt_ch_sev_anem', "var")) |> 
  add_ci()


library("ggVennDiagram")
dt<-data.frame(PRdata$hv002, PRdata$hhid, PRdata$hv001)
dt$stunt<-ifelse(PRdata$nt_ch_any_anem=="Yes", 1, 0)
dt$waste<-ifelse(PRdata$nt_ch_wast=="Yes", 1, 0)
dt$ane<-ifelse(PRdata$nt_ch_stunt=="Yes", 1, 0)
dt$universe<-ifelse(PRdata$var=="NoNoNo", 1,0)
ggVennDiagram(lapply(dt[,4:7], function(x) which(x == 1)) )


combinations <- c(A=0.3, B=0.3, C=1.1, "A&B"=0.1, "A&C"=0.2, "B&C"=0.1 ,"A&B&C"=0.1,"D"=0.2,"C&D"=0.1)
vd <- venneuler(combinations)
plot(vd)


# ----------------------Shape file of Nepal--------------------------------------------------
# shape file of Nepal
nepal<-sf::st_read("Datasets/hermes_NPL_new_wgs(1)/hermes_NPL_new_wgs_2.shp")

# cluster GPS points
cluster<-sf::st_read("Datasets/GPS_NDHS2022/NPGE81FL/NPGE81FL.shp")

# # clip between shape file of Nepal and cluster and save them
# clipped_data<-st_intersection(nepal,cluster)
# st_write(clipped_data3, "Datasets/GPS_NDHS2022/districts.shp")
# clipped_data2<-clipped_data[, c(1:24)]
# clipped_data3<-clipped_data2[, 1:8]

# read cluster shape file
cluster<-st_read("Datasets/GPS_NDHS2022/districts.shp")

PRdata<-PRdata %>%
  left_join(cluster, by=c("hv021"="DHSCLUST"))


# Load INLA and spdep package
library(INLA)
library(spdep)

# Create spatial adjacency matrix
regions_shapefile <- st_read("Datasets/hermes_NPL_new_wgs(1)/hermes_NPL_new_wgs_2.shp")
dist_list<-c("Achham", "Arghakhanchi", "Baglung", "Baitadi", "Bajhang", 
  "Bajura", "Banke", "Bara", "Bardiya", "Bhaktapur", "Bhojpur", 
  "Chitawan", "Dadeldhura", "Dailekh", "Dang", "Darchula", 
  "Dhading", "Dhankuta", "Dhanusha", "Dolakha", "Dolpa", "Doti", 
  "Gorkha", "Gulmi", "Humla", "Ilam", "Jajarkot", "Jhapa", 
  "Jumla", "Kabhrepalanchok", "Kailali", "Kalikot", "Kanchanpur", 
  "Kapilbastu", "Kaski", "Kathmandu", "Khotang", "Lalitpur", 
  "Lamjung", "Mahottari", "Makawanpur", "Manang", "Morang", 
  "Mugu", "Mustang", "Myagdi", "Nawalpur", "Nuwakot", "Okhaldhunga", 
  "Palpa", "Panchthar", "Parasi", "Parbat", "Parsa", "Pyuthan", 
  "Ramechhap", "Rasuwa", "Rautahat", "Rolpa", "Rukum East", 
  "Rukum West", "Rupandehi", "Salyan", "Sankhuwasabha", "Saptari", 
  "Sarlahi", "Sindhuli", "Sindhupalchok", "Siraha", "Solukhumbu", 
  "Sunsari", "Surkhet", "Syangja", "Tanahu", "Taplejung", "Terhathum", 
  "Udayapur")
regions_shapefile$DISTRICT2<-regions_shapefile$DISTRICT
regions_shapefile$DISTRICT<-factor(regions_shapefile$DISTRICT, levels=dist_list, labels = dist_list)
regions_shapefile$DISTRICT<-as.numeric(regions_shapefile$DISTRICT)
PRdata$DISTRICT<-as.numeric(factor(PRdata$DISTRICT, levels=dist_list, labels = dist_list))

# construct neighbor list from shape files containing polygons 
nb <- spdep::poly2nb(regions_shapefile,row.names = regions_shapefile$DISTRICT)

# Construct adjacent matrix
adj_matrix <- spdep::nb2mat(nb, style="B", zero.policy=TRUE)

# mat<-getAmat(regions_shapefile, regions_shapefile$DISTRICT)

# Keep names of column and rows same
colnames(adj_matrix)<-rownames(adj_matrix)


# define outcome variable
PRdata$outcome<-ifelse(PRdata$nt_ch_wast=="No", 0, 1)

PRdata3<-PRdata |> mutate(ifelse(is.na(nt_ch_stunt), "No", as.character(nt_ch_stunt))) %>% filter(!is.na(nt_ch_stunt))
PRdata2<-PRdata %>% filter(!is.na(PRdata$nt_ch_wast))

# weighted variable

# selected data
selected_data<-PRdata2[, c("sampling_wt", "outcome","hc27","shecoreg","hv270","hv021","hv022","hv005", "DISTRICT","DHSID","geometry")]
saveRDS(selected_data, "Datasets/Selected_data.RDS")

dt<-readRDS("Datasets/Selected_data.RDS")

# INLA model
# Define the INLA model formula
formula <- outcome ~1+f(DISTRICT, model="besag", graph=adj_matrix,scale.model  = T)


# Fit the model with INLA, including weights
result <- inla(formula, 
               family="binomial", weights = sampling_wt, 
               control.predictor = list(compute = TRUE),
               control.compute = list(return.marginals.predictor = TRUE, waic=T),
               data=dt)
               


# Examine results
summary(result)

# Fixed and random effects
fixed_effect <-result$summary.fixed
random_effect<-result$summary.random$DISTRICT




# Estimate prevalence for each districts
prevalence <-(exp(fixed_effect[, "mean"]+random_effect$mean))/(1+exp(fixed_effect[, "mean"]+random_effect$mean))

result_dt<-data.frame(DISTRICT=random_effect$ID, Prevalence=prevalence)

# join prevalence data with the shape file to plot
regions_shapefile<-regions_shapefile |> 
  left_join(result_dt, by= "DISTRICT")



# Plot shape file of Nepal with districts and prevalence of stunting
# ggplot() +
#   geom_sf(data = regions_shapefile, aes(fill=Prevalence.mean))+
#   xlab("Longitude") + ylab("Latitude")+
#   annotation_scale(location = "bl", width_hint = 0.3) +
#   annotation_north_arrow(location = "tr", which_north = "true", 
#                          pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
#                          style = north_arrow_fancy_orienteering) +
#   coord_sf(xlim = c(79.9, 88.2), ylim = c(26, 30.5))+
#   theme_bw()








# USing summer package
library(SUMMER)

# Construct adjacent matrix
adj_matrix <- spdep::nb2mat(nb, style="B", zero.policy=TRUE)

mat<-getAmat(regions_shapefile, regions_shapefile$DISTRICT)


data("BRFSS")



d2<-dt[, 1:10]

svy_dt<-svydesign(data = d2, ids = ~hv021, strata = ~hv022, weights = ~hv005)

summer.dhs<-smoothArea(formula =outcome~1, domain = ~DISTRICT, transform = "logit", adj.mat = mat, level=0.95, design=svy_dt )




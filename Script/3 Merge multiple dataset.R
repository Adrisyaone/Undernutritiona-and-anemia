
# load libraries
lapply(Packages, require, character.only=T)

# load datasets
PRdata22<-readRDS("Datasets/Processed data/PRdata2022.RDS")
PRdata16<-readRDS("Datasets/Processed data/PRdata2016.RDS")
PRdata11<-readRDS("Datasets/Processed data/PRdata2011.RDS")


# merge datasets
PRdata<-bind_rows(PRdata22, PRdata16, PRdata11)

# set sampling wt to correct length
PRdata <- PRdata %>%
  mutate(sampling_wt = hv005/1000000) |> 
  mutate(combined_indicator=paste0(nt_ch_stunt, nt_ch_wast, nt_ch_any_anem),
         agemonths<-as.factor(agemonths),
         stunting=ifelse(nt_ch_stunt==1, 1,0),
         wasting=ifelse(nt_ch_wast==1, 1,0),
         Anemia=ifelse(nt_ch_any_anem==1, 1,0),
         cooccurence=ifelse(combined_indicator=="111",1, 0))



# labelling data
PRdata <- forcats::as_factor(PRdata, only_labelled = TRUE)




# Descriptive statistics
svy_dt<-svydesign(data = PRdata, ids = ~hv021, weights = ~sampling_wt)


svy_dt %>%
  tbl_svysummary(include =c('nt_ch_stunt','nt_ch_wast',  'nt_ch_any_anem', "combined_indicator"), by="Year", 
                 digits = list(all_categorical()~c(0,1))) |> 
  add_ci(style_fun = list(all_categorical() ~ purrr::partial(style_percent, digits = 1))) |> 
  bold_labels() |> 
  as_hux_xlsx("Results/table2 prevalence of stunting, wasting and anemia.xlsx")


svy_dt %>%
  tbl_svysummary(include =c("agemonths", "hc27", "hv025", "shecoreg", "education", "hv024", "hv270" ), by="Year", 
                 digits = list(all_categorical()~c(0,1))) |> 
  bold_labels() |> 
  as_hux_xlsx("Results/table1 characteristics.xlsx")





# Concentration index for stunting
dt1<-concentration_curve(dt = PRdata[PRdata$Year== 2022,], inq_var ="hv270", outcome = "stunting",weight = "sampling_wt", out = 1)
dt2<-concentration_curve(dt = PRdata[PRdata$Year== 2016,], inq_var ="hv270", outcome = "stunting",weight = "sampling_wt", out=1)
dt3<-concentration_curve(dt = PRdata[PRdata$Year== 2011,], inq_var ="hv270", outcome = "stunting",weight = "sampling_wt", out=1)


dt1.1<-concentration_curve(dt = PRdata[PRdata$Year== 2022,], inq_var ="hv270", outcome = "stunting",weight = "sampling_wt", out = 2)
dt2.1<-concentration_curve(dt = PRdata[PRdata$Year== 2016,], inq_var ="hv270", outcome = "stunting",weight = "sampling_wt", out=2)
dt3.1<-concentration_curve(dt = PRdata[PRdata$Year== 2011,], inq_var ="hv270", outcome = "stunting",weight = "sampling_wt", out=2)


ch1<-conc_chart(dt1, dt2, dt3, chart_of = "Stunting")
ch1


# concentration index for wasting
dt1<-concentration_curve(dt = PRdata[PRdata$Year== 2022,], inq_var ="hv270", outcome = "wasting",weight = "sampling_wt", out=1)
dt2<-concentration_curve(dt = PRdata[PRdata$Year== 2016,], inq_var ="hv270", outcome = "wasting",weight = "sampling_wt", out=1)
dt3<-concentration_curve(dt = PRdata[PRdata$Year== 2011,], inq_var ="hv270", outcome = "wasting",weight = "sampling_wt", out=1)

dt1.1<-concentration_curve(dt = PRdata[PRdata$Year== 2022,], inq_var ="hv270", outcome = "wasting",weight = "sampling_wt", out = 2)
dt2.1<-concentration_curve(dt = PRdata[PRdata$Year== 2016,], inq_var ="hv270", outcome = "wasting",weight = "sampling_wt", out=2)
dt3.1<-concentration_curve(dt = PRdata[PRdata$Year== 2011,], inq_var ="hv270", outcome = "wasting",weight = "sampling_wt", out=2)


ch2<-conc_chart(dt1, dt2, dt3, chart_of = "Wasting")
ch2
# concentration index for anemia
dt1<-concentration_curve(dt = PRdata[PRdata$Year== 2022,], inq_var ="hv270", outcome = "Anemia",weight = "sampling_wt", out=1)
dt2<-concentration_curve(dt = PRdata[PRdata$Year== 2016,], inq_var ="hv270", outcome = "Anemia",weight = "sampling_wt",out=1)
dt3<-concentration_curve(dt = PRdata[PRdata$Year== 2011,], inq_var ="hv270", outcome = "Anemia",weight = "sampling_wt", out=1)

dt1.1<-concentration_curve(dt = PRdata[PRdata$Year== 2022,], inq_var ="hv270", outcome = "Anemia",weight = "sampling_wt", out = 2)
dt2.1<-concentration_curve(dt = PRdata[PRdata$Year== 2016,], inq_var ="hv270", outcome = "Anemia",weight = "sampling_wt", out=2)
dt3.1<-concentration_curve(dt = PRdata[PRdata$Year== 2011,], inq_var ="hv270", outcome = "Anemia",weight = "sampling_wt", out=2)


ch3<-conc_chart(dt1, dt2, dt3, chart_of = "Anemia")
ch3


# concentration index for anemia
dt1<-concentration_curve(dt = PRdata[PRdata$Year== 2022,], inq_var ="hv270", outcome = "cooccurence",weight = "sampling_wt", out=1)
dt2<-concentration_curve(dt = PRdata[PRdata$Year== 2016,], inq_var ="hv270", outcome = "cooccurence",weight = "sampling_wt", out=1)
dt3<-concentration_curve(dt = PRdata[PRdata$Year== 2011,], inq_var ="hv270", outcome = "cooccurence",weight = "sampling_wt", out=1)

dt1.1<-concentration_curve(dt = PRdata[PRdata$Year== 2022,], inq_var ="hv270", outcome = "cooccurence",weight = "sampling_wt", out = 2)
dt2.1<-concentration_curve(dt = PRdata[PRdata$Year== 2016,], inq_var ="hv270", outcome = "cooccurence",weight = "sampling_wt", out=2)
dt3.1<-concentration_curve(dt = PRdata[PRdata$Year== 2011,], inq_var ="hv270", outcome = "cooccurence",weight = "sampling_wt", out=2)


ch4<-conc_chart(dt1, dt2, dt3, chart_of = "Co-occurence")
ch4

library("patchwork")
library(cowplot)
library(gridExtra)

p123<-ch1+ch2+ch3+ch4+plot_layout(ncol=2, guides = "collect",axis_titles =  "collect_x", design = "AB\nCD",tag_level = "keep") & theme(legend.position = 'bottom')
p123


plot_grid( ch1,ch2,ch3,ch4,
           labels = c("A", "B", "C", "D"),
           hjust = -1,
           nrow = 2)+xlab("Cummulative share of women ranked by wealth")+ 
  ylab("Cummulative share of Nutritonla status")


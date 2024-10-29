#----------------- Install, load libraries and datasets -------------------

# Clear environment
rm(list=ls())

# Step-1: Install and load library

Packages <- c("sjmisc","sf","ggspatial", "expss","naniar","gtsummary","foreign","survey",'labelled',"readxl", "tidyverse", "haven","rockchalk", "forcats", "INLA")

new_packages <- Packages[!(Packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)


# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)



# load libraries
lapply(Packages, require, character.only=T)




# load PR data from 2011, 2016 and 2022
PRdata2022 <- read_dta("Datasets/Raw data/NP_2022_DHS_10282024_1914_183234/NPPR82DT/NPPR82FL.DTA")
PRdata2016 <- read_dta("Datasets/Raw data/NP_2016_DHS_10282024_1517_183234/NPPR7HDT/NPPR7HFL.DTA")
PRdata2011 <- read_dta("Datasets/Raw data/NP_2011_DHS_10282024_1518_183234/NPPR61DT/NPPR61FL.DTA")

# load KR data from 2011, 2016 and 2022
KRdata2022 <- read_dta("Datasets/Raw data/NP_2022_DHS_10282024_1914_183234/NPKR82DT/NPKR82FL.DTA")
KRdata2016 <- read_dta("Datasets/Raw data/NP_2016_DHS_10282024_1517_183234/NPKR7HDT/NPKR7HFL.DTA")
KRdata2011 <- read_dta("Datasets/Raw data/NP_2011_DHS_10282024_1518_183234/NPKR61DT/NPKR61FL.DTA")



# remove unnecessary objects from environment
rm(list=c("new_packages"))




# concentration index

hci<-function(hci_obj){
  
  myOrder <- order(hci_obj$fractional_rank)
  xCoord <- hci_obj$fractional_rank[myOrder]
  y <- hci_obj$outcome[myOrder]
  cumdist <- cumsum(y) / sum(y)
  dt<-data.frame(myOrder, xCoord,cumdist)
}


concentration_curve<-function(dt,inq_var, outcome, weight, type=c("CI"), method=c("linreg_delta"), out=c(1,2)){
  if(!require(rineq)) install.packages("rineq")
  dt<-data.frame(dt)
  dt$inq_var=as.numeric(dt[, inq_var])
  dt$outcome=dt[, outcome]
  dt$wt=dt[,weight]
  dt<-data.frame(dt)

  k<-rineq::ci(ineqvar =dt$inq_var ,outcome = dt$outcome, weights = dt$wt,type = "CI",method = "linreg_delta",robust_se = T)

dt<-hci(k)

ifelse(out==1, return(dt), return(summary(k)))
}


conc_chart <- function(dt1, dt2, dt3, chart_of){
  chart1<-ggplot()+
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),col="black", lwd=1.3)+
    geom_line(data=dt1,aes(x=xCoord, y = cumdist,col="2022"), lwd=1.1)+
    geom_line(data=dt2,aes(x=xCoord, y = cumdist,col="2016"), lwd=1.1)+
    geom_line(data=dt3,aes(x=xCoord, y = cumdist,col="2011"), lwd=1.1)+
    scale_y_continuous(
      paste0("Cummulative share of ", chart_of, "\n"), expand = c(0, 0),
      sec.axis = sec_axis(~ . *1)
    )+
    scale_x_continuous(
      "\nCummulative share of children ranked by wealth", expand = c(0, 0),
      sec.axis = sec_axis(~ . *1)
    )+
    scale_color_manual("Year",values = c("orange", "red","navy"))+
    
    labs(title = "A")+
    theme_bw()+
    theme(axis.title = element_text(size = 10, face="bold"),
          legend.title = element_text(size=10, face="bold"),
          axis.text.y.right = element_blank(),
          axis.text.x.top = element_blank())
  
  return(chart1)
}



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

# Step-1: Install and load library

Packages <- c("sjmisc","sf","ggspatial", "expss","naniar","gtsummary","foreign","survey",'labelled',"readxl", "tidyverse", "haven","rockchalk", "forcats", "INLA", "data.table", "srvyr", "marginaleffects")

new_packages <- Packages[!(Packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)


# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)



# load libraries
lapply(Packages, require, character.only=T)


# remove unnecessary objects from environment
rm(list=c("new_packages", "Packages"))






# Functions
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


conc_chart <- function(dt1, dt2, dt3, chart_of, tag){
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
    
    labs(title = tag)+
    theme_bw()+
    theme(axis.title = element_text(size = 10, face="bold"),
          legend.title = element_text(size=10, face="bold"),
          axis.text.y.right = element_blank(),
          axis.text.x.top = element_blank())
  
  return(chart1)
}



# function to determine CI
#some common functions
one_way_svy_table<-function(dataset, cat, num){
  
  svy_hf<-svydesign(data = dataset, ids = ~hv021,,weights = ~sampling_wt)
  df_result <- 
    tibble(variable = cat) %>% #choose variables here
    # get the levels of each variable in a new column
    # adding them as a list to allow for different variable classes
    rowwise() %>%
    mutate(
      # level to be used to construct call
      level = unique(svy_hf$variables[[variable]]) %>% as.list() %>% list(),
      # character version to be merged into table
      label = unique(svy_hf$variables[[variable]]) %>% as.character() %>% as.list() %>% list()
    ) %>%
    unnest(c(level, label)) %>%
    mutate(
      label = unlist(label)
    )
  
  # construct call to svyciprop
  df_result$svyciprop <-
    map2(
      df_result$variable, df_result$label,
      function(variable, level) rlang::inject(survey::svyciprop(~I(!!rlang::sym(variable) == !!level), svy_hf))
    )
  
  
  
  # round/format the 95% CI
  df_result <-
    df_result %>%
    rowwise() %>%
    mutate(
      ci = 
        svyciprop %>%
        attr("ci") %>%
        style_number(scale = 100, digits = 1) %>%
        paste0(collapse = ", ")
    ) %>% 
    ungroup() %>%
    # keep variables needed in tbl
    select(variable, label, ci)
  
  
  
  # construct gtsummary table with CI
  
  tab1<-svy_hf %>%
    tbl_svysummary(include =c(cat, num),
                   
                   type = list(cat~"categorical",
                               num~"continuous"), 
                   digits = list(all_categorical()~1,
                                 all_continuous()~c(1,1)),
                   statistic = list(all_categorical()~"{n} ({p})",
                                    all_continuous()~"{mean}±{sd}; {p50} ({p25}, {p75})")) %>%
    # merge in CI
    modify_table_body(
      ~.x %>%
        left_join(
          df_result, 
          by = c("variable", "label")
        )
    ) %>%
    # add a header
    modify_header(ci = "**95% CI**")
  
  return(tab1)
}
ci <- function(variable, by, data, ...) {
  svyby(as.formula( paste0( "~" , variable)) , by = as.formula( paste0( "~" , by)), data, svyciprop, vartype="ci") %>%
    tibble::as_tibble() %>%
    dplyr::mutate_at(vars(ci_l, ci_u), ~style_number(., scale = 100, digits = 1) %>% paste0("%")) %>%
    dplyr::mutate(ci = stringr::str_glue("({ci_l}, {ci_u})")) %>%
    dplyr::select(all_of(c(by, "ci"))) %>%
    tidyr::pivot_wider(names_from = all_of(by), values_from = ci) %>%
    set_names(paste0("add_stat_", seq_len(ncol(.))))
}








svy_two_way_table<-function(dataset, selected_vars, by, percent=c("column", "row")){
  
  
  svy_hf<-svydesign(data = dataset, ids = ~hv021,,weights = ~sampling_wt)
  tab<-svy_hf%>%
    tbl_svysummary(by =by,include=c(selected_vars), percent = percent,
                   digits = list(all_categorical()~1,
                                 all_continuous()~c(1,1)),
                   statistic = list(all_categorical()~"{n} ({p})",
                                    all_continuous()~"{mean}±{sd})")) %>%
    add_stat(everything() ~ ci) %>%
    # merge in CI
    modify_table_body(
      dplyr::relocate, add_stat_1, .after = stat_1
    )%>%
    # add a header
    modify_header(starts_with("add_stat_") ~ "**95% CI**")
  
  return(tab)
}



est.fun1 <- function(data, variable, by) {
  data <- as.data.frame(data)
  categories <- levels(data[, all_of(variable)])
  result <- c()
  
  for (i in 1:length(categories)) {
    o = categories[i]
    data$v <- data |> dplyr::select(all_of(variable)) |> deframe()
    data$v <- as.numeric(data$v == categories[i])
    full_data <- data |> as_survey_design(weight = Sampling_wt)
    model <- svyglm(as.formula(paste("v ~ ", by)), design = full_data, family = binomial(link = "logit"))
    # Risk Difference
    rd.obj <- avg_comparisons(model)
    
    # Formatting risk difference and ci
    est <- style_number(round(rd.obj$estimate*100, 3), digits = 1)
    l.ci <- style_number(round(rd.obj$conf.low*100, 3), digits = 1)
    u.ci <- style_number(round(rd.obj$conf.high*100, 3), digits=1)
    
    # Returning estimate with CI together
    result1 <- str_glue("{est}PP ({l.ci}, {u.ci})")
    names(result1) <- categories[i]
    result <- c(result, result1)
    rm(result1)
  }
  return(result)
}



# This is the table function to properly format the table with the required statistics
tab.fun1 <- function(dataset, multinomlist=NA, by){
  # Creating survey design data inside the function 
  svydt <- dataset |> as_survey_design(weight = Sampling_wt)
  
  # Creating the risk difference estimates with CI
  a <- est.fun1(data = dataset , variable = multinomlist[1], by = by)
  
  a<-data.frame(a) # Converting to data.frame 
  a<-rownames_to_column(a, "class") # Assigning rownames (categories) to variable "class"
  
  # Creating gtsummary table for the first variable
  tab1 <- svydt |> 
    tbl_svysummary(by = by,
                   include = multinomlist[1],
                   type = list(all_categorical() ~ 'categorical'),
                   digits = list(all_categorical() ~ c(1, 0, 1)),
                   statistic = list(all_categorical() ~ c("{p}"))) |> 
    modify_table_body(~.x |>
                        left_join(a, by=c("label" = "class"))) |> 
    modify_header(a ~ "**Difference (CI)**",) |> 
    add_ci(statistic = list(all_categorical() ~ "{conf.low}, {conf.high}", 
                            all_continuous() ~ "{conf.low}, {conf.high}"),
           pattern =  "{stat} ({ci})",
           style_fun = everything() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
           method = multinomlist[1] ~ "svyprop.logit") # Checking the svyprop.logit method to compare with the default
  
  # This will only run if more than one variable supplied to the argument multinomlist. This will create gtsummary tables like above and stack for multiple variables
  if (length(multinomlist) > 1) {
    for(i in 2:length(multinomlist)){
      a <- est.fun1(data = dataset, variable = multinomlist[i], by = by)
      a <- data.frame(a)
      a <- rownames_to_column(a, "class")
      tab2 <- svydt |> 
        tbl_svysummary(by = by,
                       include = multinomlist[i],
                       type = list(all_categorical() ~ 'categorical'),
                       digits = list(all_categorical() ~ c(1, 0, 1)),
                       statistic = list(all_categorical() ~ c("{p}"))) |> 
        modify_table_body(~.x |>
                            left_join(a, by=c("label" = "class"))) |> 
        modify_header(a ~ "**Difference (CI)",) |> 
        add_ci(statistic = list(all_categorical() ~ "{conf.low}, {conf.high}", 
                                all_continuous() ~ "{conf.low}, {conf.high}"),
               pattern =  "{stat} ({ci})",
               style_fun = everything() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
               method = multinomlist[i] ~ "svyprop.logit") # Checking the svyprop.logit method to compare with the default
      
      tab1 <- tbl_stack(list(tab1, tab2))
    }
  }
  return(tab1)
}


# Small area estimation
sae<-function(var, nepal, grade, name, col=c("yellow", "red")){
  bin.res1 <- smoothArea(as.formula(paste0(var,"~ 1")), 
                         domain = ~dist_code,
                         design = svydt,
                         adj.mat = mat, 
                         transform = "logit",
                         pc.u = 1,
                         pc.alpha = 0.01,
                         pc.u.phi = 0.5,
                         pc.alpha.phi = 2/3)
  
  a<-data.frame(bin.res1$bym2.model.est)
  a$domain<-as.numeric(a$domain)
  a$median<-a$median*100
  nepal2<-nepal |> 
    left_join(a,by = c("dist_code"="domain"))
  
  k<-ggplot() +
    geom_sf(data = nepal2, aes(fill=median))+
    geom_sf_text(data = nepal2, aes(label=OBJECTID), size=2)+
    scale_fill_gradient(low = col[1], high = col[2])+
    annotation_scale(location = "bl", width_hint = 0.3) +
    labs(title = paste0(grade, ". Small Area Estimation of ", name))+
    xlab("Longitude") + ylab("Latitude")+labs(fill="Prevalence")+
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(79.9, 88.2), ylim = c(26, 30.5))+
    theme_bw()+
    theme(title = element_text(size=11))
  
  return(k)
  
}





sae_table<-function(var){
  bin.res1 <- smoothArea(as.formula(paste0(var,"~ 1")), 
                         domain = ~dist_code,
                         design = svydt,
                         adj.mat = mat, 
                         transform = "logit",
                         pc.u = 0.1,
                         pc.alpha = 0.01,
                         pc.u.phi = 0.5,
                         pc.alpha.phi = 2/3)
  
  a<-data.frame(bin.res1$bym2.model.est)
  return(a)
  
}

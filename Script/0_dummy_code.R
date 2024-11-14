#///////////////////////////////////////////////////////////////////////////////
### Example code to illustrate potential analysis approach to HERD COSTAR 
### survey data, plus hopefully helpful additional advice!
#///////////////////////////////////////////////////////////////////////////////

# Load required libraries (install these if you don't have them)

library(tidyverse)
library(visdat)
library(srvyr)
library(survey)
library(crosstable)
library(marginaleffects)
library(rlang)

#///////////////////////////////////////////////////////////////////////////////
# Background details ----

# If you don't already use comments extensively then I would recommend doing so.
# You can't have too many comments or explanations! This is really helpful
# for others reading the code and for yourself if you come back at a later date.

# Also, if you aren't aware already: in RStudio you can give your scripts 
# headings/sub-headings that you can instantly navigate to. Just use four "-" 
# symbols like below in a comment:

# Example main heading ----
# Example level 2 sub-heading ----
# Example level 3 sub-heading ----

# If you use one hash "#" symbol the heading will be a main heading, two hashes
# will make it a level 2 sub-heading, three hashes a level 3 sub-heading, and 
# so on. So you can organise content clearly.

# How to use the headings? Click on this line. Now if  you look at the bottom 
# left of the script window you should see the words 
# "Example level 3 sub-heading". If you click on this you will see all the other 
# headings in this script, and you can instantly go to each one by clicking. 
# So it makes navigation a bit easier.

# In the below code I will often (but not always) use the "pipe" operator: |>
# This can be read like the word "then". It's really useful to chain functions 
# together more easily. e.g. instead of round(mean(d$y), 1), where I am first 
# taking a mean then rounding to 1 decimal place using nested 
# parentheses/brackets, which get really hard to read quickly, I can write:
# d$y |> mean() |> round(digits = 1)
# Which is like saying "take d$y, then take its mean, then round that
# result to 1 decimal place".

# I will also try and remember to include all arguments in function calls,
# but for functions I am used to and that have few arguments I may miss the
# argument name. e.g. the "round" function above has two arguments: x and digits
# x = the value to round and digits = the rounding level. So you could write:
# round(x = d$y, digits = 3) or equally round(d$y, 3) as long as the arguments
# are in the correct order you don't need to name them. Naming them is probably
# best though at least for more complicated functions as it keeps things clear!

#///////////////////////////////////////////////////////////////////////////////
# 1. Load "1.COSTAR_human_main_dataset.RDS" data and make initial checks ----

# Remove any previous data

rm(list = ls())

# I use the project functionality of RStudio, so I create a new project for
# each project. If you haven't done this before from the main menu go:
# File > New Project > New Directory > New Project > select a directory then 
# click "Create Project"
# Then in that folder you'll get a .RProj file. Now whenever you need to work
# on the project you can open that file and it'll use the directory/folder 
# that it's in as the working directory. I think when you just open RStudio
# as well it will be using the last project you were working on so be aware.
# You can change projects from the upper right area where you'll see the project
# name.

# Otherwise set your working directory (I typically have a folder called
# "Data, analysis & results" for each project, and then in that sub-folders for 
# "Data", "Analysis" - i.e. scripts like this, "Results" etc)

# For example:
# setwd("C:/Users/fbsjhi/OneDrive - University of Leeds/Research/Projects/COSTAR/Nepal survey/")


# Load human main data (as I'm already in a project I just need to tell it the
# folder within the project folder where the data are)

d <- readRDS("Data/1.COSTAR_human_main_dataset.RDS")

#///////////////////////////////////////////////////////////////////////////////
## 1.1. Do some quick basic exploration and checks ----

# For a proper analysis I would do extensive exploration and checking of the
# data, but for now I'll just illustrate a few quick checks as the focus
# here initially is on how to produce the tables.

# Check data dimensions

dim(d) # 816 rows and 528 columns

# Have an initial quick look at missingness in the design variables and 
# socio-demographics. 

d |> select(uuid_gen:q121) |> vis_miss() # Here I am telling R to use the data
# "d", then select all variables from uuid_gen to q121, then to apply the
# vis_miss function, which produces the graph of missing values you will see
# when you run it. The black lines show where observations are missing across
# different variables. Clearly there are a few individuals (the black rows) 
# with mostly/totally missing data. Why is this? 

# Create a row_id variable then get the row ids for the individuals with 
# missing data

d$row_id <- 1:dim(d)[1]
d |> filter(is.na(q101)) |> select(row_id) # filter() subsets the data based
# on q101 being missing

# Look at the municipality and ward variables

table(d$q101, useNA = "always") # Municipality
table(d$q102, useNA = "always") # Ward

#///////////////////////////////////////////////////////////////////////////////
## 1.2. Create additional required variables ----

# I need to check I understand the sampling design correctly, but for now
# based on the weights spreadsheet I'll assume that you sampled wards as 
# clusters.

# However, there doesn't appear to be a cluster variable, so I will create a 
# municipality_ward id via the paste function to be a cluster ID by joining the
# municipality labels with the corresponding cluster number per observation

d$cls <- paste(d$q101, d$q102, sep = "_")

# Same for the strata, I can't immediately see an obvious variable for that.
# However, for now I'll ignore it because as you will see you can just update
# the survey design object(s) and all the subsequent results will be updated
# once run.

#///////////////////////////////////////////////////////////////////////////////
## 1.3. Finalise main dataset ----

# Note: it's probably easiest to remove all cases with fully missing data before
# doing the analyses, as we won't be dealing explicitly with any missing values.
# However, we will certainly need to show the frequency of all missing values 
# for each variable/analysis in the tables.
# This would include non-consenters and any who consented and then had some
# missing data for whatever reason. So below I'll remove cases based on not 
# missing the q101 var (!is.na() returns TRUE if the value isn't missing),
# and this will be the final dataset used from here on.

d <- d[!is.na(d$q101), ]


#///////////////////////////////////////////////////////////////////////////////
# 2. Create survey design object ----

# First, we must create the survey design object. This contains the information
# about which variables code for strata, clusters and weights. As discussed you
# may or may not include all of these, but the analyses certainly need to 
# account for the clustering whatever, so we'll just include that for now.

full_d <- d |> as_survey_design(ids = cls) # I'll call this survey design "full"
# because it contains the full dataset. We may also create subsets

# We then use the analysis functions in the srvyr package, telling each function
# which survey design information to use (full), which will give us analyses 
# that correctly account for the survey design.



#///////////////////////////////////////////////////////////////////////////////
# 3. Descriptive statistics tables ----

# Note: unless you include the weights then the descriptive statistics will be
# identical whether you account for the survey design or not, as only the 
# weights affect point estimates, and the descriptives will just be point
# estimates as they are describing the basic characteristics of the sample
# to help readers understand who the results can be representative of.
# I'll skip these for now as they are less complicated, but I will try and 
# provide some code another time if necessary. My preferred package to use
# to calculate descriptives is "tableone", as it creates summary descriptives
# tables really easily all in one go.

#///////////////////////////////////////////////////////////////////////////////
# 4. Example inferential results table using the full sample ----

# Below I will illustrate how to calculate the count frequencies, percentages,
# 95% CIs, and the "risk differences" (or percentage point differences) and 
# their 95% CIs for categorical variables, where this applies to the full
# sample, i.e. every person in the survey could respond to this question.
# Then after all the separate code chunks I'll combine them into a function
# so that you can more easily re-run each of those steps all in one go on 
# different variables.

# Later we'll see how to create tables for subsets of the sample, e.g. only 
# those individuals who say they know what an AB is.

# I think the best approach to this is to calculate each value for the final
# result you want in a raw form, e.g. no rounding of decimal places, no 
# combining of values (e.g. joining of point estimates and confidence 
# intervals), no scaling from proportions to percentages etc.
# Then in a separate final step format those results and combine as necessary,
# e.g. scale to %, round, format etc.
# That way if you decide to present the results differently, e.g. change the
# number of decimal places, how your format the values etc, you can just change
# that code at the end, and you don't have to go through and change the 
# formatting when you calculate each result.


# As a simple example let's look at the variable q205x1: heard about ABs,
# which will be part of "Table 2: Heard about Antimicrobials".
# I haven't seen the final plan yet for the structure of the tables, but
# hopefully the below code will show you how to get the necessary results,
# and you can adapt as needed (obviously do ask for any help). For now I'll
# just look at the "Yes" responses, but obviously the below can also be used
# to easily look at "No"/"Don't know".

#///////////////////////////////////////////////////////////////////////////////
## 4.1. Overall results: ab aware example ----

# The below assumes you have loaded the data and that there are no missing 
# values on the relevant variables.
# That code is all above, but I've repeated here for ease of running if 
# needed:

d <- readRDS("Data/1.COSTAR_human_main_dataset.RDS")
d$cls <- paste(d$q101, d$q102, sep = "_")
d <- d[!is.na(d$q101), ]

# Then we set the variable of interest and the outcome of interest:

v <- "q205x1" # Variable
o <- "Yes" # Outcome of interest for the variable

# Add variable to dataset called "v", and make it binary based on the outcome
# of interest being present/absent for automatic reference when running GLM

d$v <- d |> select(all_of(v)) |> deframe()
d$v <- as.numeric(d$v == o)

# Set survey design (for full sample here)

full_d <- d |> as_survey_design(ids = cls)

# First, let's get the raw frequency overall and then by sex. 
# We don't need this to be adjusted
# for the survey as it won't matter unless we weight the results, and even if
# we do I don't think the count needs to be weighted as people don't really 
# make inferences about the count. They are only interested really in the
# percentage as a measure of the likely percentage in the population.

# Note: it's really important to use a clear and consistent naming structure
# for your created objects like extracted results, so that you can easily
# work out which result etc you have, because you will have to create a lot.

# I prefer using the often recommended "camel-case" naming, where you separate
# words with underscores (_), and I don't use capitals at all. Then I prefer
# longer names whenever it helps with ease of understanding as I think that's 
# more important than being able to write it a bit quicker.
# However, as I will use the below code in a general function I will keep the
# names general rather than specific to any variable.

# Get count of yeses (initially is a string value so change to numeric with
# that last function).

# Overall yeses

o_y <-
  full_d |>
  summarise(
    n = sum(v)
  ) |> as.numeric()

# Overall sample size

o_N <-
  full_d |>
  summarise(
    n = n()
  ) |> 
  as.numeric()

# Take a look:

o_y
o_N

# Next let's get the survey adjusted proportion and 95% CI. First we specify
# the survey design object, then use the tidyverse package "summarise" function,
# which works with the srvyr package survey estimation functions that we then
# use to get the proportion. We use survey_mean but specify "proportion = T"
# so it uses appropriate calculations to calculate the 95% CIs for a binary
# outcome, and we specify "prop_method = lo" for logit method. Have a look at
# the help for the survey_mean function if you want to understand this a little
# more.

o_p95 <-
  full_d |>
  summarise(
    p = survey_mean(
      v, 
      proportion = T,  
      vartype = "ci", 
      prop_method = "lo")
  )

# Take a look:

o_p95

# Compare to the raw proportion (not adjusted for the survey) as a sanity check:

sum(d$q205x1 == "Yes") / length(d$q205x1)

# Looks good.

#///////////////////////////////////////////////////////////////////////////////
## 4.2. By sex results: ab aware example ----

# Now get n and sample size by sex

# Female yeses

f_y <-
  full_d |>
  group_by(q104) |> 
  summarise(
    n = sum(v)
  ) |> 
  filter(q104 == "Female") |>
  select(n) |> 
  as.numeric()

# Male yeses

m_y <-
  full_d |>
  group_by(q104) |> 
  summarise(
    n = sum(v)
  ) |> 
  filter(q104 == "Male") |>
  select(n) |> 
  as.numeric()

# Female sample size

f_N <-
  full_d |>
  group_by(q104) |> 
  summarise(
    n = n()
  ) |> 
  filter(q104 == "Female") |>
  select(n) |> 
  as.numeric()

# Male sample size

m_N <-
  full_d |>
  group_by(q104) |> 
  summarise(
    n = n()
  ) |> 
  filter(q104 == "Male") |>
  select(n) |> 
  as.numeric()

# Take a look:

f_y
f_N
m_y
m_N

# Now get proportion and 95% CI results for each sex. The only difference from
# the code for getting the overall result is adding a "group_by" function,
# which tells the survey_mean command to appropriately subset the data by sex
# and calculate the results for "q205x1 == "Yes"" for each level of sex.

sex_p95 <-
  full_d |>
  group_by(q104) |> 
  summarise(
    p = survey_mean(
      v, 
      proportion = T,  
      vartype = "ci", 
      prop_method = "lo")
  )

# Have a look:

sex_p95

# Compare to the raw proportions as a sanity check:

sum(d$q205x1[d$q104 == "Female"] == "Yes") / 
  length(d$q205x1[d$q104 == "Female"])

sum(d$q205x1[d$q104 == "Male"] == "Yes") / 
  length(d$q205x1[d$q104 == "Male"])

# Looks good.

#///////////////////////////////////////////////////////////////////////////////
## 4.3. Difference between sex results: ab aware example ----

# Now finally let's get the difference in percentages between females and males
# and the 95% CI for this "risk difference" (i.e. difference in percentages).
# Obviously you can compare female - male or male - female, the only difference
# will be one will be the negative of the other. Personally I always compare
# female vs male because the norm is male vs female which seems a little sexist!

# We will calculate the risk difference and 95% CI based on the average marginal
# difference using a survey appropriate logistic regression. You can read about
# the approach here: https://vincentarelbundock.github.io/marginaleffects/
# And more specifically here: https://vincentarelbundock.github.io/marginaleffects/articles/comparisons.html

# The below code creates the logistic regression of ab_aware = yes ~ sex and 
# then the avg_comparisons function calculates the RD and 95% CI:

rd <- avg_comparisons(svyglm(v ~ q104, design = full_d))

#///////////////////////////////////////////////////////////////////////////////
## 4.4. Combine results: ab aware example ----

# Extract all results and put in new dataset
# o_y = overall yeses, o_N = overall sample size, o_p = overall proportion of
# yeses, o_p_l/o_p_h = lower/upper 95% CI for overall proportion
# Then the same values but for females/males with a f_ or m_ prefix instead 
# of o_, then finally rd_p = risk difference on proportion scale, 
# rd_p_l/rd_p_u = lower/upper 95% CIs of RD.

res <- 
  tibble(
    o_y, o_N, o_p = o_p95$p, o_p_l = o_p95$p_low, o_p_u = o_p95$p_upp,
    f_y, f_N, f_p = sex_p95$p[sex_p95$q104 == "Female"], 
    f_p_l = sex_p95$p_low[sex_p95$q104 == "Female"],
    f_p_u = sex_p95$p_upp[sex_p95$q104 == "Female"],
    m_y, m_N, m_p = sex_p95$p[sex_p95$q104 == "Male"], 
    m_p_l = sex_p95$p_low[sex_p95$q104 == "Male"],
    m_p_u = sex_p95$p_upp[sex_p95$q104 == "Male"],
    rd_p = rd$estimate, rd_p_l = rd$conf.low, rd_p_u = rd$conf.high
  )

# Take a look:

res

#///////////////////////////////////////////////////////////////////////////////
## 4.5. Combine results creation code into a function for easier use ----

# Below I have combined the above code into a function so you just need to tell
# it the variable name and the outcome of interest (e.g. the ab aware variable
# and "Yes") and it will create all the results and return a dataset containing
# them.

# To create the function just run the first line of the code ("res_tab") and
# R should then create the function and take your cursor to the end of the 
# function:

res_tab <- 
  function(v, o){
    d$v <- d |> select(all_of(v)) |> deframe()
    d$v <- as.numeric(d$v == o)
    
    full_d <- d |> as_survey_design(ids = cls)
    
    o_y <-
      full_d |>
      summarise(
        n = sum(v)
      ) |> as.numeric()
    
    o_N <-
      full_d |>
      summarise(
        n = n()
      ) |> 
      as.numeric()
    
    o_p95 <-
      full_d |>
      summarise(
        p = survey_mean(
          v, 
          proportion = T,  
          vartype = "ci", 
          prop_method = "lo")
      )
    
    f_y <-
      full_d |>
      group_by(q104) |> 
      summarise(
        n = sum(v)
      ) |> 
      filter(q104 == "Female") |>
      select(n) |> 
      as.numeric()
    
    m_y <-
      full_d |>
      group_by(q104) |> 
      summarise(
        n = sum(v)
      ) |> 
      filter(q104 == "Male") |>
      select(n) |> 
      as.numeric()
    
    f_N <-
      full_d |>
      group_by(q104) |> 
      summarise(
        n = n()
      ) |> 
      filter(q104 == "Female") |>
      select(n) |> 
      as.numeric()
    
    m_N <-
      full_d |>
      group_by(q104) |> 
      summarise(
        n = n()
      ) |> 
      filter(q104 == "Male") |>
      select(n) |> 
      as.numeric()
    
    sex_p95 <-
      full_d |>
      group_by(q104) |> 
      summarise(
        p = survey_mean(
          v, 
          proportion = T,  
          vartype = "ci", 
          prop_method = "lo")
      )
    
    rd <- avg_comparisons(svyglm(v ~ q104, design = full_d))
    
    res <- 
      tibble(
        o_y, o_N, o_p = o_p95$p, o_p_l = o_p95$p_low, o_p_u = o_p95$p_upp,
        f_y, f_N, f_p = sex_p95$p[sex_p95$q104 == "Female"], 
        f_p_l = sex_p95$p_low[sex_p95$q104 == "Female"],
        f_p_u = sex_p95$p_upp[sex_p95$q104 == "Female"],
        m_y, m_N, m_p = sex_p95$p[sex_p95$q104 == "Male"], 
        m_p_l = sex_p95$p_low[sex_p95$q104 == "Male"],
        m_p_u = sex_p95$p_upp[sex_p95$q104 == "Male"],
        rd_p = rd$estimate, rd_p_l = rd$conf.low, rd_p_u = rd$conf.high
      )
    
    return(res)
  }

# Now we have the function in the memory we can use it.

# Let's try it on the ab awareness variable and then the anti-protozoal

ab_res <- res_tab(v = "q205x1", o = "Yes")
proto_res <- res_tab(v = "q206x1", o = "Yes")

# Take a look:

ab_res
proto_res 

# Note: some of the yes counts are very low for proto_res, if you get any 0s 
# the CIs won't be able to be calculated and we'll have to have a think about 
# alternative options.

# So to be clear to use this function on additional variables you just need to
# run the function code and then you can use it. So you can have it at the start
# of an analysis script and then just repeatedly call the res_tab function.

#///////////////////////////////////////////////////////////////////////////////
## 4.6. Put results into a table ----

# Let's look at one quick-ish way to put the results into a crude table to 
# export for adding into Word. We'll use the ab_res results so make sure that
# you've created that results dataset using the function call above.

# Now there are lots of ways we might combine, format and present these results,
# and it depends what you decide for the tables, so this is just one possible
# example. To save space I would present frequencies and percentages on 
# different rows like this, e.g.
# Overall         Female          Male
# 400/800          250/350        150/450
# 50% (40, 60)     30% (20, 40)  70% (60, 80)

# Then you could either have the female-male difference as a separate column,
# or maybe another row in the female column. Another column is maybe easiest
# to read, but obviously takes more space.

# Previously I've always just created results for tables as Excel csv files
# and copied into word and formatted more there, but there is a really good
# looking package called "gt" to create fully formatted tables that avoids
# then messing around in Word and changing things constantly each time.
# So I think it would be worth using that, but right now as I haven't used it
# before and I'll just show you how to create a simple Excel csv with the above
# results structure. However, I'd be keen to later show you code to make a 
# better formatted table with full title, labels, footnotes etc in R using
# that package.

# So for now let's just create an R datafame with three columns like above, 
# using the paste() function to join the values we created together as needed.

# Set rounding level so we can change it for all results as needed:

dp <- 1

# First the overall results:

ab_aware_o_res <- c(
  "Overall",
  paste(ab_res$o_y, "/", ab_res$o_N, sep = ""),
  paste(round(ab_res$o_p * 100, dp), "%", 
        " (", round(ab_res$o_p_l * 100, dp), ", ",
        round(ab_res$o_p_u * 100, dp), ")",
        sep = "")
)

# Then by sex:

ab_aware_f_res <- c(
  "Female",
  paste(ab_res$f_y, "/", ab_res$f_N, sep = ""),
  paste(
    round(
      ab_res$f_p * 100, dp), 
    "%", " (", 
    round(
      ab_res$f_p_l * 100, dp),
    ", ",
    round(
      ab_res$f_p_u * 100, dp),
    ")", sep = "")
)

ab_aware_m_res <- c(
  "Male",
  paste(ab_res$m_y, "/", ab_res$m_N, sep = ""),
  paste(
    round(
      ab_res$m_p * 100, dp), 
    "%", " (", 
    round(
      ab_res$m_p_l * 100, dp),
    ", ",
    round(
      ab_res$m_p_u * 100, dp),
    ")", sep = "")
)

# Then the difference results (as the other result variables created have three
# entries [name, frequency and percentage] we need to add two missing values
# first):

ab_aware_f_m_res <-
  c("", "",
    paste(
      round(ab_res$rd_p * 100, dp),
      " (", round(ab_res$rd_p_l * 100, dp),
      ", ", round(ab_res$rd_p_u * 100, dp),
      ")",
      sep = ""
    ))

# Then put them all together in a dataframe and export as a csv

ab_aware_res <- 
  data.frame(ab_aware_o_res, ab_aware_f_res, ab_aware_m_res, ab_aware_f_m_res)
write.csv(ab_aware_res, "Results/Tables/ab_aware_res.csv")

#///////////////////////////////////////////////////////////////////////////////
## 5. Analysing results for subsets ----

# Note that when you calculate results for subsets, say individuals who report
# having heard of ABs, you need to properly account for this in the survey
# design object and not just delete the unwanted cases. This is because the
# errors depend on the full sample design. In practice it means using the 
# subset() function. I haven't had time to illustrate this fully, but assume
# you want to calculate results like above, i.e. proportions for a categorical
# variable, overall and by sex, but only for individuals who report knowing
# what an AB is. You would create a new design object like follows:

ab_aware_d <- subset(full_d, q205x1 == "Yes")

# Then use ab_aware_d instead of full_d in all calculations.
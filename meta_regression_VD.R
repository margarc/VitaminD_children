library(car)
library(gdata)
library(meta)
library(metafor)
require(gdata)
library(ggplot2)
library(QuantPsyc)
library(rmeta)
library(xlsx)
library(reshape2)


data_pr <- read.csv("data_pr_metareg.csv", as.is = TRUE)
data_pr
View(data_pr)

#--- dummy code for: design (cohort, other), country groups (group 1, group 2 and group 3) and setting (PICU, NICU)
# convert these variables from character to factor

#------------------------------- for study_group ----------------------------------------#

data_pr$design_group <- as.factor(data_pr$design_group)
levels(data_pr$design_group)
# "cohort" "other" 

# contr. treatment(number of groups, base = number representing the baseline group)

# set cohort design as baseline i.e = 0
contrasts(data_pr$design_group)<-contr.treatment(2, base = 1)
data_pr$design_group
# check that contrasts were entered correctly
attr(data_pr$design_group,"contrasts")

#        2
# cohort 0
# other  1

# give them a more meaningful name (cohort = coded as 0)
cohort_vs_other <- c(0,1)
# bind the newly created dummy variables together
contrasts(data_pr$design_group) <- cbind(cohort_vs_other)
attr(data_pr$design_group, "contrasts")
#               cohort_vs_other
# cohort               0
# other                1


#---------for countrygrp_cat variable [groups 1 and 2 grouped together versus group 3]-----------#

data_pr$countrygrp_cat <- as.factor(data_pr$countrygrp_cat)
levels(data_pr$countrygrp_cat)
# "grp1_or_2" "grp3"
# contr. treatment(number of groups, base = number representing the baseline group)
# grp1_or_2 as baseline i.e = 0
contrasts(data_pr$countrygrp_cat)<-contr.treatment(2, base = 1)
data_pr$countrygrp_cat
attr(data_pr$countrygrp_cat,"contrasts")
#             2
# grp1_or_2   0
# grp3        1

grp1_or_2_vs_grp3 <- c(0,1)
contrasts(data_pr$countrygrp_cat) <- cbind(grp1_or_2_vs_grp3)
attr(data_pr$countrygrp_cat,"contrasts")

#                    grp1_or_2_vs_grp3
# grp1_or_2                 0
# grp3                      1

#---------------- for setting PICU vs NICU----------------------------#
data_pr$setting <- as.factor(data_pr$setting)
levels(data_pr$setting)
# "NICU" "PICU"

# contr. treatment(number of groups, base = number representing the baseline group)
# set NICU as baseline i.e = 0
contrasts(data_pr$setting)<-contr.treatment(2, base = 1)
data_pr$setting

#         2
#    NICU 0
#    PICU 1
# Levels: NICU PICU

NICU_vs_PICU <- c(0,1)
contrasts(data_pr$setting) <- cbind(NICU_vs_PICU)
attr(data_pr$setting,"contrasts")
#           NICU_vs_PICU
# NICU            0
# PICU            1

#---------------------------- Meta-regressions for prevalence 
# First step: proportion meta-analysis (with all 48 studies-overall)
metapr <- metaprop(vddch, totch, studlab=(study), data=data_pr, sm="PLOGIT", comb.fixed= FALSE)
metapr

#------- UNIVARIATE meta-regressions to obtain bubble plots for each covariate 

#----- Year of publication
metareg_yr <- metareg(metapr, "~ year", method.tau = 'REML', hakn = TRUE)
summary(metareg_yr)
bubble(metareg_yr, studlab=FALSE, col="darkblue", bg="transparent",
       xlab="Year of study publication")

#----- Quality score 
metareg_qs <- metareg(metapr, "~ QS", method.tau = 'REML', hakn = TRUE)
summary(metareg_qs)
bubble(metareg_qs, studlab=FALSE, col="darkblue", bg="transparent",
       xlab="Quality score")


#----- Total children 
metareg_totch <- metareg(metapr, "~ totch", method.tau = 'REML', hakn = TRUE)
summary(metareg_totch)
bubble(metareg_totch, studlab=FALSE, col="darkblue", bg="transparent",
       xlab="Total children")


#----- Country group category
metareg_ctry <- metareg(metapr, "~ countrygrp_cat", method.tau = 'REML', hakn = TRUE)
summary(metareg_ctry)
bubble(metareg_ctry, studlab=FALSE, col="darkblue", bg="transparent",
       xlab="Country groups")


#----- Design group
metareg_desgr <- metareg(metapr, "~ design_group", method.tau = 'REML', hakn = TRUE)
summary(metareg_desgr)
bubble(metareg_desgr, studlab=FALSE, col="darkblue", bg="transparent",
       xlab="Study design")

#----- Setting
metareg_setting <- metareg(metapr, "~ setting", method.tau = 'REML', hakn = TRUE)
summary(metareg_setting)
bubble(metareg_setting, studlab=FALSE, col="darkblue", bg="transparent",
       xlab="Setting")

#------------------ FULL META-REGRESSION WITH ALL SIX COVARIATES-----------------#
metareg_all <- metareg(metapr,
                       "~ year + totch + countrygrp_cat + setting + design_group + QS",
                       method.tau = 'REML', hakn = TRUE)
metareg_all

print(metareg_all)



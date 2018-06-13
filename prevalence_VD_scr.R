library(meta)
library(metafor)
library(rmeta)
library(gdata)
library(rmeta)
library(readxl)
library(ggplot2)
library(xlsx)
require(xlsx)

# Load the required dataset
read_excel("data_prevalence.xlsx", sheet= "all")
prev_all <- read_excel("data_prevalence.xlsx", sheet= "all")
prev_all
View(prev_all)

# check
table(prev_all$design)
#case_control=24, cohort=20, cross_sectional=4 
table(prev_all$country_group)
#group1 group2 group3 
#18     18     12 

# Prevalence of vitamin D deficiency in a meta-analysis of all studies together 
meta_all <- metaprop(vddch, totch, studlab=(study), 
                     data=prev_all, sm="PLOGIT")
meta_all

# Forest plot (random effects model)
forest.meta(meta_all, studlab = TRUE,
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", bysort=TRUE,
            col.square="grey", comb.fixed=FALSE,digits.I2= 1, col.inside="white", col.diamond="lightgrey", fontsize= 10)

# Funnel plot 
funnel.meta(meta_all, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

# Eggers Test for publication bias
metabias(meta_all, method.bias = "linreg", 
         plotit = TRUE, correct = FALSE,
         k.min=2)

# Prevalence of vitamin D deficiency in critically ill children of: 

# Cohort studies
meta_cohort <- metaprop(vddch, totch, studlab=(study), subset=(prev_all$design=="cohort"), 
                        data=prev_all, sm="PLOGIT")

funnel.meta(meta_cohort, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

# case-control studies
meta_casec <- metaprop(vddch, totch, studlab=(study), subset=(prev_all$design=="case_control"), 
                       data=prev_all, sm="PLOGIT")

funnel.meta(meta_casec, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

# cross-sectional studies
meta_cross <- metaprop(vddch, totch, studlab=(study), subset=(prev_all$design=="cross_sectional"), 
                       data=prev_all, sm="PLOGIT")

funnel.meta(meta_cross, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

# subgroup, prevalence by study design (all together in a plot)

meta_design <- metaprop(vddch, totch, studlab=(study), data=prev_all, byvar=design, bylab = "", 
                        print.byvar=FALSE, print.subgroup.labels=TRUE,
                        sm="PLOGIT", comb.fixed= FALSE)
print(meta_design)
forest.meta(meta_design, studlab = TRUE,
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", col.diamond="lightskyblue", fontsize= 10)
# bysort = a logical indicating whether groups should be ordered alphabetically.
# in this case it will be case-control/cohort/cross-sectional

funnel.meta(meta_design, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

# subgroup, prevalence by country group as follows: 
# group1 = USA, Chile, Australia, Canada, Ireland, Japan, Spain
# group2 = South Africa, China, Egypt, Iran, Turkey, Saudi Arabia
# group3 = Bangladesh, Thailand, India
meta_countr <- metaprop(vddch, totch, studlab=(study), data=prev_all, byvar=country_group, bylab = "", 
                        print.byvar=FALSE, print.subgroup.labels=TRUE,
                        sm="PLOGIT", comb.fixed= FALSE)
print(meta_countr)
?forest.meta
forest.meta(meta_countr, studlab = TRUE,
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", col.diamond="lightgrey", fontsize = 6,
            plotwidth=unit(5, "cm"), squaresize=0.4, digits.I2= 1, colgap=unit(0.8, "mm"))



# studies with threshold <50 nmol/L 
# Load another sheet from the excel file 
read_excel("data_prevalence.xlsx", sheet= "threshold")
prev_thresh <- read_excel("data_prevalence.xlsx", sheet= "threshold")
prev_thresh
View(prev_thresh)

meta_thresh <- metaprop(vddch, totch, studlab=(study), data=prev_thresh, sm="PLOGIT")
meta_thresh
forest.meta(meta_thresh, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", col.diamond="darkslategray3", fontsize= 11)

funnel.meta(meta_thresh, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

# Eggers Test
metabias(meta_thresh, method.bias = "linreg", 
         plotit = TRUE, correct = FALSE,
         k.min=2)

# prevalence, studies with small "sample size" (<100)
read_excel("data_prevalence.xlsx", sheet= "small_sample")
prev_smallsample <- read_excel("data_prevalence.xlsx", sheet= "small_sample")
View(prev_smallsample)

meta_smallsample <- metaprop(vddch, totch, studlab=(study), data=prev_smallsample, sm="PLOGIT")
print(meta_smallsample)
forest.meta(meta_smallsample, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", col.diamond="darkslategray3", fontsize= 10)

funnel.meta(meta_smallsample, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

# prevalence, studies with "large" sample size (>100)
read_excel("data_prevalence.xlsx", sheet= "large_sample")
prev_largesample <- read_excel("data_prevalence.xlsx", sheet= "large_sample")
View(prev_largesample)

meta_largesample <- metaprop(vddch, totch, studlab=(study), data=prev_largesample, sm="PLOGIT")
print(meta_largesample)

forest.meta(meta_largesample, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", col.diamond="darkslategray3", fontsize= 10)

funnel.meta(meta_largesample, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

# prevalence, studies from turkey and india
# load the sheet with turkey and india data 
read_excel("data_prevalence.xlsx", sheet= "ind_turk")
data_indturk <- read_excel("data_prevalence.xlsx", sheet= "ind_turk")
View(data_indturk)

# check numbers
table(data_indturk$country)
#India Turkey 
#10      7 

# INDIA
meta_india <- metaprop(vddch, totch, studlab=(study), subset=(data_indturk$country =="India"),
                       data=data_indturk,  sm="PLOGIT")
print(meta_india)

forest.meta(meta_india, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", col.diamond="darkslategray3", fontsize= 10)

funnel.meta(meta_india, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

# TURKEY
meta_turk <- metaprop(vddch, totch, studlab=(study), subset=(data_indturk$country =="Turkey"),
                      data=data_indturk,  sm="PLOGIT")

print(meta_turk)
forest.meta(meta_turk, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", col.diamond="darkslategray3", fontsize= 10)

funnel.meta(meta_turk, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

# prevalence in those studies of children have respiratory outcomes
read_excel("data_prevalence.xlsx", sheet= "respir")
data_resp <- read_excel("data_prevalence.xlsx", sheet= "respir")
View(data_resp)

meta_resp <- metaprop(respdeficient, totresp, studlab=(study), data=data_resp, sm="PLOGIT")
print(meta_resp)

forest.meta(meta_resp, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", col.diamond="darkslategray3", fontsize= 10)

funnel.meta(meta_resp, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

# Neonates versus all other age groups
read_excel("data_prevalence.xlsx", sheet= "neons_others")
data_ages <- read_excel("data_prevalence.xlsx", sheet= "neons_others")
View(data_ages)
# check
table(data_ages$age)
# neonates    other 
# 6       42 

meta_age <- metaprop(vddch, totch, studlab=(study), data=data_ages, byvar=age, bysort=TRUE, bylab = "", 
                     print.byvar=FALSE, print.subgroup.labels=TRUE,
                     sm="PLOGIT", comb.fixed= FALSE)

meta_age
forest.meta(meta_age, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", digits.I2= 1, col.diamond="darkslategray3", fontsize= 10)

funnel.meta(meta_age, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")
 

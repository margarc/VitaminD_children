library(meta)
library(metafor)
library(rmeta)
library(gdata)
library(rmeta)
library(readxl)
library(ggplot2)
library(xlsx)
require(xlsx)

read_excel("dataprev_sepsis.xlsx", sheet= "sepsis_prevall")
data_sepsis <- read_excel("dataprev_sepsis.xlsx", sheet= "sepsis_prevall")
View(data_sepsis)

# Check data
table(data_sepsis$design)
# case_control= 7, cohort= 9 
table(data_sepsis$setting)
#NICU= 3, PICU= 13 
table(data_sepsis$country_group)
# group1= 6, group2= 4, group3= 6

# group1 = USA, Chile, Australia, Canada, Ireland, Japan, Spain
# group2 = South Africa, China, Egypt, Iran, Turkey, Saudi Arabia
# group3 = Bangladesh, Thailand, India

# Prevalence of Vitamib D deficiency in all children with sepsis 
meta_sepsis <- metaprop(vddseps, totseps, studlab=(study), data=data_sepsis, sm="PLOGIT")
print(meta_sepsis)

forest.meta(meta_sepsis, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", bysort=TRUE,
            col.square="grey", digits.I2= 1, col.inside="white", col.diamond="darkslategray3",col.diamond.lines="black",
            fontsize= 10)

funnel.meta(meta_sepsis, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

?metabias
# Eggers Test
metabias(meta_sepsis, method.bias = "linreg", 
         plotit = TRUE, correct = FALSE,
         k.min=2)

# studies with threshold <50 nmol/L 

read_excel("dataprev_sepsis.xlsx", sheet= "threshold_s")
data_threshseps <- read_excel("dataprev_sepsis.xlsx", sheet= "threshold_s")
View(data_threshseps)
meta_thresh <- metaprop(vddseps, totseps, studlab=(study), data=data_threshseps, sm="PLOGIT")
print(meta_thresh)

forest.meta(meta_thresh, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", bysort=TRUE,
            col.square="grey", digits.I2= 1, col.inside="white", col.diamond="darkslategray3", fontsize= 11)

funnel.meta(meta_thresh, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

# Eggers Test
metabias(meta_thresh, method.bias = "linreg", 
         plotit = TRUE, correct = FALSE,
         k.min=2)

# Subgroup with all the studies (sepsis) by country group 
meta_country <- metaprop(vddseps, totseps, studlab=(study), data=data_sepsis, byvar=country_group, bylab = "", 
                         print.byvar=FALSE, print.subgroup.labels=TRUE,
                         sm="PLOGIT", comb.fixed= FALSE)

print(meta_country)
forest.meta(meta_country, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", 
            bysort=TRUE, col.square="grey", digits.I2= 1, col.inside="white", 
            col.diamond="darkslategray3", fontsize= 11)


# Subgroup analysis with all the studies (of children with sepsis) by study design (cohort vs case-control)
meta_design <- metaprop(vddseps, totseps, studlab=(study), data=data_sepsis, byvar=design, bylab = "", 
                        print.byvar=FALSE, print.subgroup.labels=TRUE,
                        sm="PLOGIT", comb.fixed= FALSE)

print(meta_design)
forest.meta(meta_design, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", 
            bysort=TRUE, col.square="grey", digits.I2= 1, col.inside="white", 
            col.diamond="lightgray", fontsize= 10)


# Ages neonates versus all other age groups 
read_excel("dataprev_sepsis.xlsx", sheet= "ages_all")
data_ages <- read_excel("dataprev_sepsis.xlsx", sheet= "ages_all")
View(data_ages)

meta_ages <- metaprop(vddseps, totseps, studlab=(study), data=data_ages, 
                      byvar=age_def, bylab = "", 
                      print.byvar=FALSE, print.subgroup.labels=TRUE,
                      sm="PLOGIT", comb.fixed= FALSE)
print(meta_ages)

forest.meta(meta_ages, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", 
            bysort=TRUE, col.square="grey", digits.I2= 1, col.inside="white", 
            col.diamond="darkslategray3", fontsize= 11)

# only the studies with other age range than neonates 
meta_otherage <- metaprop(vddseps, totseps, studlab=(study), 
                          subset=(data_ages$age_def=="other"), 
                          data=data_ages, sm="PLOGIT")
meta_otherage

forest.meta(meta_otherage, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", 
            bysort=TRUE, col.square="grey", digits.I2= 1, col.inside="white", 
            col.diamond="darkslategray3", fontsize= 11)

funnel.meta(meta_otherage, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

metabias(meta_otherage, method.bias = "linreg", 
         plotit = TRUE, correct = FALSE,
         k.min=2)

# sample sizes (large vs small)
read_excel("dataprev_sepsis.xlsx", sheet= "sample_size")
data_samplesiz <- read_excel("dataprev_sepsis.xlsx", sheet= "sample_size")
View(data_samplesiz)

# Small sample size (< 40 children)
meta_small <- metaprop(vddseps, totseps, studlab=(study), 
                       subset=(data_samplesiz$sample_size=="small"), 
                       data=data_samplesiz, sm="PLOGIT")
meta_small
forest.meta(meta_small, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", 
            bysort=TRUE, col.square="grey", digits.I2= 1, col.inside="white", 
            col.diamond="darkslategray3", fontsize= 11)

funnel.meta(meta_small, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

metabias(meta_small, method.bias = "linreg", 
         plotit = TRUE, correct = FALSE,
         k.min=2)

# Larger sample size (> 40 children)
meta_large <- metaprop(vddseps, totseps, studlab=(study), 
                       subset=(data_samplesiz$sample_size=="large"), 
                       data=data_samplesiz, sm="PLOGIT")
meta_large
forest.meta(meta_large, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", 
            bysort=TRUE, col.square="grey", digits.I2= 1, col.inside="white", 
            col.diamond="darkslategray3", fontsize= 11)

funnel.meta(meta_large, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")


metabias(meta_large, method.bias = "linreg", 
         plotit = TRUE, correct = FALSE,
         k.min=2)

# INDIA and TURKEY
read_excel("dataprev_sepsis.xlsx", sheet= "ind_turkey")
data_indturk <- read_excel("dataprev_sepsis.xlsx", sheet= "ind_turkey")
View(data_indturk)
# (both on the forest plot)
meta_indturk <- metaprop(vddseps, totseps, studlab=(study), data=data_indturk, 
                         byvar=country, bylab = "", 
                         print.byvar=FALSE, print.subgroup.labels=TRUE,
                         sm="PLOGIT", comb.fixed= FALSE)

meta_indturk
forest.meta(meta_indturk, studlab = TRUE, comb.fixed=FALSE, 
            leftlabs = c("Study", "VD deficient", "Total"),  col.study="black", 
            bysort=TRUE, col.square="grey", digits.I2= 1, col.inside="white", 
            col.diamond="darkslategray3", fontsize= 11)

funnel.meta(meta_indturk, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")


















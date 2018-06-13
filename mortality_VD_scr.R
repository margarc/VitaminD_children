library(meta)
library(metafor)
library(rmeta)
library(gdata)
library(rmeta)
library(readxl)
library(ggplot2)
library(xlsx)
require(xlsx)

#--------------------------------- MORTALITY---------------------------------#

# Mortality risk vitamin D deficient versus vitamin D not deficient critically ill children
read_excel("data_mortality.xlsx", sheet= "mortality_all")
data_mortal <- read_excel("data_mortality.xlsx", sheet= "mortality_all")
View(data_mortal)

# checks
table(data_mortal$country_group)
# group1= 6, group2= 4, group3= 8  
table(data_mortal$design)
# case_control= 4, cohort= 14 
table(data_mortal$setting)
# NICU= 1, PICU= 17

?metafor
?metabin

metabin(deaddef, alldef, deadnotdef, allnotdef, sm= "OR", method="I", 
        data=data_mortal, studlab=study)

meta_mortall <- metabin(deaddef, alldef, deadnotdef, allnotdef, sm= "OR", method="I", 
                        data=data_mortal, studlab=study)

forest.meta(meta_mortall, studlab = TRUE, comb.random=FALSE,
            col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", 
            col.diamond="darkslategray3", digits.I2= 1,fontsize= 11)

funnel.meta(meta_mortall, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

# Eggers Test
metabias(meta_mortall, method.bias = "linreg", 
         plotit = TRUE, correct = FALSE,
         k.min=2)


#----------------------- Subgroup by country group:

metamort_country <- metabin(deaddef, alldef, deadnotdef, allnotdef, sm= "OR", method="I",
                            byvar=country_group, bysort=TRUE, bylab = "", 
                            print.byvar=FALSE, print.subgroup.labels=TRUE,
                            data=data_mortal, studlab=study)

print(metamort_country)
# Fixed effects forest plot:
forest.meta(metamort_country, studlab = TRUE, comb.random=FALSE,
            col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", 
            col.diamond="darkslategray3", digits.I2= 1,fontsize= 11)

# Random effects forest plot:
forest.meta(metamort_country, studlab = TRUE, comb.fixed=FALSE,
            col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", 
            col.diamond="darkslategray3", digits.I2= 1,fontsize= 11)


#---------------------- Subgroup by study design:
metamort_design <- metabin(deaddef, alldef, deadnotdef, allnotdef, sm= "OR", method="I",
                           byvar=design, bysort=TRUE, bylab = "", 
                           print.byvar=FALSE, print.subgroup.labels=TRUE,
                           data=data_mortal, studlab=study)
metamort_design 
# Random effects forest plot:
forest.meta(metamort_design, studlab = TRUE, comb.fixed=FALSE,
            col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", 
            col.diamond="darkslategray3", digits.I2= 1,fontsize= 11)

#----------------------Subgroup only cohort studies 
meta_cohort <- metabin(deaddef, alldef, deadnotdef, allnotdef, sm= "OR", method="I",
                       bysort=TRUE, bylab = "",
                       subset=(data_mortal$design=="cohort"), 
                       data=data_mortal, studlab=study)
print(meta_cohort)

forest.meta(meta_cohort, studlab = TRUE, comb.fixed=FALSE,
            col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", 
            col.diamond="darkslategray3", digits.I2= 1,fontsize= 11)

funnel.meta(meta_cohort, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")

# Eggers Test
metabias(meta_cohort, method.bias = "linreg", 
         plotit = TRUE, correct = FALSE,
         k.min=2)



#------------------ Subgroup: studies that reported Vitamin D levels with a threshold <50 nmol/L
read_excel("data_mortality.xlsx", sheet= "mortality_threshold")
data_thresh <- read_excel("data_mortality.xlsx", sheet= "mortality_threshold")
View(data_thresh)

meta_thresh <- metabin(deaddef, alldef, deadnotdef, allnotdef, sm= "OR", method="I", 
                       data=data_thresh, studlab=study)

print(meta_thresh)
# Fixed effects forest plot
forest.meta(meta_thresh, studlab = TRUE, comb.random=FALSE,
            col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", 
            col.diamond="darkslategray3", digits.I2= 1,fontsize= 11)

# Random effects forest plot
forest.meta(meta_thresh, studlab = TRUE, comb.fixed=FALSE,
            col.study="black", bysort=TRUE,
            col.square="grey", col.inside="white", 
            col.diamond="darkslategray3", digits.I2= 1,fontsize= 11)

funnel.meta(meta_thresh, pch=1, xlim = NULL,
            col.random = "deepskyblue",
            main = "Funnel Plot with pseudo 95% Confidence Intervals",
            cex.main = 1,   font.main = 3, col.main= "darkblue")




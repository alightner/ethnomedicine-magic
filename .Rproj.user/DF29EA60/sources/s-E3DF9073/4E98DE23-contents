rm(list=ls())
set.seed(2021)
source('package-list.R')
source('functions.R')
load('eHRAF-data/var_dict2')
source('eHRAF-variables.R')

df <- read_csv('eHRAF-data/coded-data.csv')
dm <- read_csv('eHRAF-data/IRR-domains.csv')
culture_df <- read_csv('eHRAF-data/culture-data.csv')
d <- df[sapply(df, is.numeric)]
colnames(d) <- var_dict2[colnames(d)]
source('eHRAF-models.R')

# Descriptive stats -------------------------------------------------------

perc_religious_leader <- round(mean(df$religious_leader)*100)
perc_supernatural <- round(mean(df$supernatural)*100)
perc_divination <- round(mean(df$divination)*100)

source('eHRAF-IRR-check.R')
source('eHRAF-cultural-variation2.R')
source('eHRAF-SCCS.R')
source('FIELD-analysis.R')

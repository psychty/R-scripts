
# GP high co-occuring prevalence clusters

# Following Julian Flowers excellent work...
# http://rpubs.com/jflowers/458789

# Motivation
# Increasing importance of MM - costs, inequality, increasing prevalence
# Limited datasets
# Prevalence estimates at practice level from QOF at practice level and over time
# Use unsupervised machine learning to look at association between variables = prevalence, deprivation, sociodemography?



# Approach
# Identify and extract relevant variables from Fingertips via fingertipsR
# Preprocess - missing data, convert to wide format, scale
# Hierarchical clustering and kmeans
# Statistical tests to confirm cluster numbers
# Calculate summary statistics for each cluster
# Assign practices to clusters
# Map
# Other

library(tidyverse, warn.conflicts = FALSE)
library(fingertipsR)
library(stringr) ## text manipulation
library(Hmisc) ## data description
library(ggraph) ## plot as network charts
library(igraph, warn.conflicts = FALSE) ## network tools
library(janitor)
# install.packages('corrplot')
library(corrplot)

indicator_areatypes(AreaTypeID = 7)

GP_indicators <- unique(indicator_areatypes(AreaTypeID = 7))
GP_indicators <- indicator_metadata(GP_indicators$IndicatorID)

# The above is the 57 indicators used by J Flowers plus additional indicators chosen by me.
GP_selected_ind <- c(200, 212, 219, 224, 241, 247,253,258,262,273,276,280,285,294,336,339,340, 639, 640,641,642,643,848,849,90443,90452,90453,90581,90646,91261,91262,91269,91280, 91459, 91872, 92588,92589,92590, 92643,92658,92659,92661,92663,92783,92847,92848,93204,93205,93206,93207,93208,93209,93210,93211,93437,93444,383, 93443, 355, 356, 91461, 92660)

# Not all indicators have metadata
GP_indicators_meta <- indicator_metadata(GP_selected_ind)

raw_data <- fingertips_data(GP_selected_ind, AreaTypeID = 7) %>% 
  group_by(IndicatorID) %>% 
  filter(ParentName %in% c("NHS Coastal West Sussex CCG", "NHS Crawley CCG", "NHS Horsham And Mid Sussex CCG")) %>% 
  filter(AreaType == "GP",
         TimeperiodSortable == max(TimeperiodSortable)) %>% 
  select(IndicatorID, IndicatorName, ParentName, AreaCode, AreaName, Age, Sex, Timeperiod, Value, LowerCI95.0limit, UpperCI95.0limit) %>%
#  mutate(index = paste(Timeperiod, Sex,  Age, IndicatorName, sep = "-")) %>%
  mutate(index = IndicatorID) %>% 
  mutate(Area = paste(AreaCode, AreaName, sep = "-")) %>% 
  ungroup() %>% 
  select(Area, index, Value) %>% 
  spread(index, Value) 

data_missing <- raw_data %>% 
  summarise_if(is.numeric, funs(sum(is.na(.)))) %>% 
  gather() %>% 
  rename(Indicator = key,
         Records_missing = value)

# To run a correlation matrix we need to have non-missing data and so we need to impute any values that are missing
raw_data_scaled <- raw_data %>% 
  mutate_if(is.numeric, funs(impute(., mean))) %>% 
  mutate_if(is.numeric, funs(scale(.))) %>% 
  select(-Area)

# #devtools::install_github("drsimonj/corrr")
# library(corrr)
# 
# cor_1 <- raw_data_scaled %>% 
#   select(-Area) %>% 
#   correlate() %>% 
#   shave()


cor_2 <- cor(raw_data_scaled)
corrplot(cor_2, 
         method = "ellipse",
         type = "upper",
         sig.level = 0.05,
         insig = "blank",
         tl.col = "#000000",
         tl.srt = 0)


# Factor analysis

# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Clustering/Partitioning_Around_Medoids_(PAM)

library(cluster)
?pam()

# partition around mediods   model
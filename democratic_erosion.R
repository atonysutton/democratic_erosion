setwd('C:/Tony/git_workspace/democratic_erosion')

#load libraries
library(tidyverse)

#load and shape data ----

##regime type data from V-Dem - one row per country year 
vdem <- read_csv('./data/V-Dem-CY-Full+Others-v11.1.csv')

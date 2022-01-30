setwd('C:/Tony/git_workspace/democratic_erosion')
#setwd('C:/Tony/Political Science MA/_thesis/quantitative work - backup copy/democratic_erosion')

#load libraries
library(tidyverse)

#load and shape data ----

##regime type data from V-Dem - one row per country year 
#vdem <- read_csv('./data/V-Dem-CY-Full+Others-v11.1.csv')

#vdem <- vdem %>% select(country_name, country_text_id, year, COWcode,
#                        v2x_polyarchy, v2x_libdem, v2x_partipdem, v2x_delibdem, v2x_egaldem,
#                        v2xnp_client, v2elvotbuy, v2dlencmps, v2psprlnks, v2x_elecreg,
#                        v2smonex, v2smmefra, v2smgovdom, v2smpardom, v2smfordom, 
#                        v2cacamps, v2smpolsoc, v2smpolhate,
#                        e_migdpgro, e_migdppc,
#                        e_mipopula, e_miurbani,
#                        e_total_fuel_income_pc, e_total_resources_income_pc)

#write_csv(vdem, file = './data/vdem_trimmed.csv')

vdem <- read_csv('./data/vdem_trimmed.csv')

vdem <- vdem %>% filter(!is.na(v2x_polyarchy))

skimr::skim(vdem)

#set arbitrary thresholds
dem_threshold = 0.5

#identify consolidated democracies ----

##label democratization and autocratization, which define spells
 ###create empty variable
vdem$democratize <- as.logical(FALSE)

for (i in seq_along(vdem$year)){
  #skip along if no entry for the country in the prior year
  if (length(vdem$v2x_polyarchy[vdem$country_name == vdem[i,]$country_name & vdem$year == (vdem[i,]$year - 1)]) == 0) next
  
  #identify the polyarchy score for the same country in the preceding year
  pre1_polyarchy <- vdem %>%
    filter(country_name == vdem[i,]$country_name & year == (vdem[i,]$year - 1)) %>%
    pull(v2x_polyarchy)
  
  #call it democratization if a country is a democracy this year and was not in prior year
  vdem[i,]$democratize <- 
    case_when(pre1_polyarchy > dem_threshold ~ FALSE,
              vdem[i,]$v2x_polyarchy >= dem_threshold ~ TRUE,
              TRUE ~ FALSE
              )
}

summary(vdem$democratize)

####still need to address instance of a country appearing for the first year in the df but as a democracy ####


##how? 
 ##minimum polyarchy threshold? perhaps look for discontinuities in polyarchy peak among all that ever eroded.
 ##robust across multiple vdem high level indexes?
 ##longevity threshold, perhaps empirically derived from survival distribution?
 ##some combination?
 ##or come at it backwards, as requiring two-step erosion? would then be identifying a phenomenon, not a dependent variable.



##of consolidated pool, label whether later eroded

#test whether theorized factors predict later erosion ----

##clientelism

##information control

##polarization

##search for most telling time lag
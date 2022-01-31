setwd('C:/Tony/git_workspace/democratic_erosion')
#setwd('C:/Tony/Political Science MA/_thesis/quantitative work - backup copy/democratic_erosion')

#load libraries
library(tidyverse)

#load and shape data ----

##regime type data from V-Dem - one row per country year 
#vdem <- read_csv('./data/V-Dem-CY-Full+Others-v11.1.csv')

##trim to a manageable file size by selecting only relevent variables
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
 ###create empty variables
vdem$democratize <- as.logical(NA)
vdem$autocratize <- as.logical(NA)
vdem$first_appear <- as.logical(FALSE)
 
 ###loop over every country-year
for (i in seq_along(vdem$year)){
  #if no entry for the country in the prior year, label first year and skip to next country-year
  if (length(vdem$v2x_polyarchy[vdem$country_name == vdem$country_name[i] & vdem$year == (vdem$year[i] - 1)]) == 0) vdem$first_appear[i] <- TRUE
  if (vdem$first_appear[i] == TRUE) next
  
  #otherwise, identify the polyarchy score for the same country in the preceding year
  pre1_polyarchy <- vdem %>%
    filter(country_name == vdem$country_name[i] & year == (vdem$year[i] - 1)) %>%
    pull(v2x_polyarchy)
  
  #call it democratization if a country is a democracy this year and was not in prior year
  vdem$democratize[i] <- 
    case_when(pre1_polyarchy > dem_threshold ~ FALSE,
              vdem$v2x_polyarchy[i] >= dem_threshold ~ TRUE,
              TRUE ~ FALSE
              )
  
  #call it autocratization if a country is an autocracy this year and was not in prior year
  vdem$autocratize[i] <- 
    case_when(pre1_polyarchy < dem_threshold ~ FALSE,
              vdem$v2x_polyarchy[i] < dem_threshold ~ TRUE,
              TRUE ~ FALSE
    )
}

 ###for countries first appearing in data, determine whether democratized 
vdem$democratize <- if_else(vdem$first_appear == TRUE,
                            if_else(vdem$v2x_polyarchy >= dem_threshold,
                                    TRUE,
                                    FALSE),
                            vdem$democratize)
summary(vdem$democratize)

 ###for countries first appearing in data, determine whether autocratized 
vdem$autocratize <- if_else(vdem$first_appear == TRUE,
                            if_else(vdem$v2x_polyarchy < dem_threshold,
                                    TRUE,
                                    FALSE),
                            vdem$autocratize)
summary(vdem$autocratize)

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
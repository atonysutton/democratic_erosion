setwd('C:/Tony/git_workspace/democratic_erosion')
#setwd('C:/Tony/Political Science MA/_thesis/quantitative work - backup copy/democratic_erosion')

#set arbitrary thresholds----
dem_threshold = 0.5 #on vdem's 1-point scales
survival_threshold = 0.5 #as a probability
erosion_threshold = 1 #as multiple of standard deviations
lag_range = 10 #test time lags of dependent variables from 1 to this many years

#load libraries
library(tidyverse)
library(scales)

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

##write_csv(vdem, file = './data/vdem_trimmed.csv')

vdem <- read_csv('./data/vdem_trimmed.csv')

vdem <- vdem %>% filter(!is.na(v2x_polyarchy))

##rescale variables to zero-to-one, 
 ##and ensure 1 is the direction of more of that variable (not always more democratic)
vdem$v2cacamps <- rescale(vdem$v2cacamps, to = c(0,1))
vdem$v2smpolsoc <- 1 - rescale(vdem$v2smpolsoc)
vdem$v2smpolhate <- 1 - rescale(vdem$v2smpolhate)
vdem$v2smonex <- rescale(vdem$v2smonex, to = c(0,1)) #already runs low to high
vdem$v2smmefra <- 1 - rescale(vdem$v2smmefra, to = c(0,1))
vdem$v2smgovdom <- 1 - rescale(vdem$v2smgovdom, to = c(0,1))
vdem$v2smpardom <- 1 - rescale(vdem$v2smpardom, to = c(0,1))
vdem$v2smfordom <- 1 - rescale(vdem$v2smfordom, to = c(0,1))
vdem$v2psprlnks <- 1 - rescale(vdem$v2psprlnks, to = c(0,1))
vdem$v2dlencmps <- 1 - rescale(vdem$v2dlencmps, to = c(0,1))

##recalculate natural resources income as fraction of gdp
vdem$e_total_resources_percent <- 
  case_when(is.na(vdem$e_migdppc) ~ as.numeric(NA),
            vdem$e_migdppc <= 0 ~ as.numeric(NA),
            TRUE ~ vdem$e_total_resources_income_pc / vdem$e_migdppc )

##multiply variables to interact in linear regressions
vdem$clientXresources <- vdem$v2xnp_client * vdem$e_total_resources_percent #clientelism * natural resources
vdem$smmefraXsmpardom <- vdem$v2smmefra * vdem$v2smpardom #online fractionalization * party disinfo
vdem$smonexXsmmefra <- vdem$v2smonex * vdem$v2smmefra #online consumption * fractionalization
vdem$smonexXsmfordom <- vdem$v2smonex * vdem$v2smfordom #online consuption * foreign disinfo
vdem$smmefraXsmfordom <- vdem$v2smmefra * vdem$v2smfordom #online fractionalization * foreign disinfo
vdem$clientXcacamps <- vdem$v2cacamps * vdem$v2xnp_client #clientelism * political polarization
vdem$clientXsmpolsoc <- vdem$v2smpolsoc * vdem$v2xnp_client #clientelism * social polarization
vdem$smmefraXsmpolsoc <- vdem$v2smmefra * vdem$v2smpolsoc #online fractionalization * social polarization


  
##examine data
skimr::skim(vdem)

#situate country-years within democratic spells ----

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
    case_when(pre1_polyarchy >= dem_threshold ~ FALSE,
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

##identify beginning of each democratic spell
vdem$dem_spell_start <- as.numeric(NA)
 #loop over every country-year
for (i in seq_along(vdem$year)){
 #ignore autocracies
 if (vdem$v2x_polyarchy[i] < dem_threshold) next
 #recognize democratization years as their own spell starts
 if (vdem$democratize[i] == TRUE) vdem$dem_spell_start[i] <- vdem$year[i]
 if (vdem$democratize[i] == TRUE) next

 vdem$dem_spell_start[i] <- vdem %>%
   filter(country_name == vdem$country_name[i] &
            vdem$year < vdem$year[i] & 
            vdem$democratize == TRUE) %>%
   summarize(dem_spell_start = max(year)) %>%
   pull(dem_spell_start)
}

summary(vdem$dem_spell_start)

##name democratic spells
vdem$dem_spell_name <- if_else(is.na(vdem$dem_spell_start),
                               as.character(NA),
                               paste(vdem$country_name, as.character(vdem$dem_spell_start)))

 ###check that every democratic country year is labeled with a spell start
sum(!is.na(vdem$dem_spell_start)) == sum(vdem$v2x_polyarchy >= dem_threshold)

##identify prior peak polyarchy within same democratic spell
vdem$dem_spell_peak = as.numeric(NA)
 ###loop over every country year
for (i in seq_along(vdem$year)){
  ###ignore autocracies
  if(is.na(vdem$dem_spell_start[i])) next
  ###examine polyarchy scores from each year during democratic spell, record highest
  vdem$dem_spell_peak[i] <- vdem %>% 
    filter(country_name == vdem$country_name[i] &
             year >= vdem$dem_spell_start[i] &
             year <= vdem$year[i]) %>% 
    summarize(peak_polyarchy = max(v2x_polyarchy), na.rm = TRUE) %>%
    pull(peak_polyarchy)
}
summary(vdem$dem_spell_peak)

##record running tally of length of democracy spell
vdem$dem_spell_running <- vdem$year - vdem$dem_spell_start

##identify eventual outcome of each democracy spell
vdem$dem_spell_outcome <- as.character(NA)

for (i in seq_along(vdem$year)){
vdem$dem_spell_outcome[i] <- 
  if_else(vdem$v2x_polyarchy[i] >= dem_threshold,
          if_else(sum(vdem$autocratize[vdem$country_name == vdem$country_name[i] & vdem$year >= vdem$year[i]]) > 0,
                  'autocracy',
                  'democracy'),
          as.character(NA))
}
vdem$dem_spell_outcome <- as.factor(vdem$dem_spell_outcome)
summary(vdem$dem_spell_outcome)

##note length of spells at autocratization or last year of data
vdem <- vdem %>%
  group_by(dem_spell_name) %>%
  mutate(dem_spell_length = max(dem_spell_running)) %>%
  ungroup()
summary(vdem$dem_spell_length)

#identify consolidated democracies ----

##how to identify consolidation? 
##minimum polyarchy threshold? perhaps look for discontinuities in polyarchy peak among all that ever eroded.
###polyarchy score of 0.76 predicts better than 80% chance of surviving
###polyarchy score of 0.58 predicts better than 50% chance of surviving
##longevity threshold, perhaps empirically derived from survival distribution?
###20 years predicts at least 80% chance of surviving
###0 years predicts better than 50% chance of surviving
##robust across multiple vdem high level indexes?
###libdem: 80% threshold is 0.61; 50% threshold is 0.46
###partipdem: 80% threshold is 0.50; 50% threshold is 0.34
###delibdem: 80% threshold is 0.59; 50% threshold is 0.51
###egaldem: 80% threshold is 0.56; 50% threshold is 0.40
##some combination?
##or come at it backwards, as requiring two-step erosion? would then be identifying a phenomenon, not a dependent variable.


##chart outcomes by polyarchy height
vdem %>% filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(polyarchy_cohort = round(v2x_polyarchy, digits = 2)) %>%
  mutate(outcome_rate = sum(dem_spell_outcome == 'democracy') / n()) %>%
  ggplot(aes(x = polyarchy_cohort, y = outcome_rate))+
  geom_point()+
  geom_smooth()

##calculate minimum polyarchy to predict survival above threshold
height_threshold <- vdem %>% 
  filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(polyarchy_cohort = round(v2x_polyarchy, digits = 2)) %>%
  summarize(outcome_rate = sum(dem_spell_outcome == 'democracy') / n()) %>%
  filter(outcome_rate < survival_threshold) %>%
  summarize(last_cohort = max(polyarchy_cohort)) %>%
  pull(last_cohort) + 0.01
  
##chart outcome by democracy spell length
vdem %>% filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(dem_spell_running) %>%
  mutate(outcome_rate = sum(dem_spell_outcome == 'democracy') / n()) %>%
  ggplot(aes(x = dem_spell_running, y = outcome_rate))+
  geom_line()+
  coord_cartesian(xlim = c(0,70))

##confirm that just over half of all democracy spells lasted
vdem %>% filter(dem_spell_running == 0) %>%
  summarize(count = n(), 
            stayed_democracies = sum(dem_spell_outcome == 'democracy'),
            survival_rate = stayed_democracies/count)

##calculate minimum spell length to predict survival above threshold
length_threshold <- vdem %>% 
  filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(dem_spell_running) %>%
  summarize(outcome_rate = sum(dem_spell_outcome == 'democracy') / n()) %>%
  filter(outcome_rate < survival_threshold) %>%
  summarize(last_cohort = max(dem_spell_running)) %>%
  pull(last_cohort) + 1

##compare vdem high level indexes
 ##the four other varieties of democracy tend to score lower than polyarchy
vdem %>% filter(v2x_polyarchy >= dem_threshold) %>% 
  summarize(polyarchy = mean(v2x_polyarchy),
            liberal = mean(v2x_libdem, na.rm = TRUE),
            participatory = mean(v2x_partipdem, na.rm = TRUE),
            deliberative = mean(v2x_delibdem, na.rm = TRUE),
            egalitarian = mean(v2x_egaldem, na.rm = TRUE))

vdem %>% filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(libdem_cohort = round(v2x_libdem, digits = 2)) %>%
  mutate(outcome_rate = sum(dem_spell_outcome == 'democracy') / n()) %>%
  ggplot(aes(x = libdem_cohort, y = outcome_rate))+
  geom_point()+
  geom_smooth()+
  coord_cartesian(ylim = c(0,1))+
  scale_y_continuous(minor_breaks = seq(from = 0, to = 1, by = 0.1),
                     breaks = c(0,1))+
  geom_hline(yintercept = 0.5)+
  geom_hline(yintercept = 0.8)

vdem %>% filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(partipdem_cohort = round(v2x_partipdem, digits = 2)) %>%
  mutate(outcome_rate = sum(dem_spell_outcome == 'democracy') / n()) %>%
  ggplot(aes(x = partipdem_cohort, y = outcome_rate))+
  geom_point()+
  geom_smooth()+
  coord_cartesian(ylim = c(0,1))+
  scale_y_continuous(minor_breaks = seq(from = 0, to = 1, by = 0.1),
                     breaks = c(0,0.5,1))+
  geom_hline(yintercept = 0.5)+
  geom_hline(yintercept = 0.8)

vdem %>% filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(delibdem_cohort = round(v2x_delibdem, digits = 2)) %>%
  mutate(outcome_rate = sum(dem_spell_outcome == 'democracy') / n()) %>%
  ggplot(aes(x = delibdem_cohort, y = outcome_rate))+
  geom_point()+
  geom_smooth()+
  coord_cartesian(ylim = c(0,1))+
  scale_y_continuous(minor_breaks = seq(from = 0, to = 1, by = 0.1),
                     breaks = c(0,0.5,1))+
  geom_hline(yintercept = 0.5)+
  geom_hline(yintercept = 0.8)

vdem %>% filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(egaldem_cohort = round(v2x_egaldem, digits = 2)) %>%
  mutate(outcome_rate = sum(dem_spell_outcome == 'democracy') / n()) %>%
  ggplot(aes(x = egaldem_cohort, y = outcome_rate))+
  geom_point()+
  geom_smooth()+
  coord_cartesian(ylim = c(0,1))+
  scale_y_continuous(minor_breaks = seq(from = 0, to = 1, by = 0.1),
                     breaks = c(0,0.5,1))+
  geom_hline(yintercept = 0.5)+
  geom_hline(yintercept = 0.8)

##calculate survival rate for democracies scoring well on all indexes
vdem %>% filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(v2x_libdem >= dem_threshold &
             v2x_partipdem >= dem_threshold & 
             v2x_delibdem >= dem_threshold & 
             v2x_egaldem >= dem_threshold) %>%
  summarize(count = n(), 
            stayed_democracies = sum(dem_spell_outcome == 'democracy'),
            success_rate = stayed_democracies/count)

###list all democratic spells that ever crossed all thresholds in same year
broad_dem_spell_list <- vdem %>% 
  filter(v2x_polyarchy >= dem_threshold,
         v2x_libdem >= dem_threshold &
           v2x_partipdem >= dem_threshold & 
           v2x_delibdem >= dem_threshold & 
           v2x_egaldem >= dem_threshold) %>%
  distinct(dem_spell_name) %>%
  pull(dem_spell_name)

###calculate survival rate predicted by whether broad democracy
vdem %>% filter(v2x_polyarchy >= dem_threshold) %>%
  distinct(dem_spell_name, .keep_all = TRUE)  %>%
  group_by(broad_dem = dem_spell_name %in% broad_dem_spell_list) %>%
  summarize(stayed_democratic = sum(dem_spell_outcome == 'democracy'),
            count = n(),
            survival_rate = stayed_democratic / count)

###repeat process using individual thresholds predicting 80% survival
 ###results are not very different - similar split of cases
high_broad_dem_spell_list <- vdem %>% 
  filter(v2x_polyarchy >= 0.76,
         v2x_libdem >= 0.61 &
           v2x_partipdem >= 0.50 & 
           v2x_delibdem >= 0.59 & 
           v2x_egaldem >= 0.56) %>%
  distinct(dem_spell_name) %>%
  pull(dem_spell_name)

vdem %>% filter(v2x_polyarchy >= dem_threshold) %>%
  distinct(dem_spell_name, .keep_all = TRUE)  %>%
  group_by(high_broad_dem = dem_spell_name %in% high_broad_dem_spell_list) %>%
  summarize(stayed_democratic = sum(dem_spell_outcome == 'democracy'),
            count = n(),
            survival_rate = stayed_democratic / count)

##label each country-year as consolidated or not
###...operationalizing consolidation by democracy spell length
vdem$consolidated_long <- case_when(vdem$v2x_polyarchy < dem_threshold ~ as.logical(NA),
                                    vdem$dem_spell_running >= length_threshold ~ TRUE,
                                    TRUE ~ FALSE)
summary(vdem$consolidated_long)

###...operationalizing consolidation by polyarchy height
vdem$consolidated_high <- case_when(vdem$v2x_polyarchy < dem_threshold ~ as.logical(NA),
                                    vdem$v2x_polyarchy >= height_threshold ~ TRUE,
                                    TRUE ~ FALSE)
summary(vdem$consolidated_high)

###...operationalizing consolidation by varietal breadth
vdem$consolidated_broad <- case_when(vdem$v2x_polyarchy < dem_threshold ~ as.logical(NA),
                                     (vdem$v2x_libdem >= dem_threshold &
                                        vdem$v2x_partipdem >= dem_threshold & 
                                        vdem$v2x_delibdem >= dem_threshold & 
                                        vdem$v2x_egaldem >= dem_threshold) ~ TRUE,
                                     TRUE ~ FALSE)
summary(vdem$consolidated_broad)

##examine overlap among consolidation measures
vdem %>% select(consolidated_long, consolidated_high, consolidated_broad) %>%
  na.omit() %>% 
  cor()

##create a variable for combined consolidation
vdem$consolidated_lhb <- case_when(vdem$v2x_polyarchy < dem_threshold ~ as.logical(NA),
                                   (vdem$consolidated_long == TRUE &
                                      vdem$consolidated_high == TRUE &
                                      vdem$consolidated_broad == TRUE) ~ TRUE,
                                   TRUE ~ FALSE)
summary(vdem$consolidated_lhb)
vdem %>% filter(consolidated_lhb == TRUE) %>% 
  distinct(dem_spell_name) %>% 
  summarize(count = n())

##label erosion as any democratic country-year  
 ##with polyarchy score at least X times 1 standard deviations below spell peak
 ##where X is an arbitrarily defined multiple,
 ##and so long as the spell ever peaked high enough to erode without autocratizing 
vdem$erode <- case_when(vdem$v2x_polyarchy < dem_threshold ~ as.logical(NA),
                        vdem$dem_spell_peak < (dem_threshold + (erosion_threshold * sd(vdem$v2x_polyarchy[vdem$v2x_polyarchy >= dem_threshold]))) ~ as.logical(NA),
                        vdem$dem_spell_peak - vdem$v2x_polyarchy >= (erosion_threshold * sd(vdem$v2x_polyarchy[vdem$v2x_polyarchy >= dem_threshold])) ~ TRUE,
                        TRUE ~ FALSE)
summary(vdem$erode)
vdem %>% filter(erode == TRUE) %>%
  group_by(dem_spell_outcome) %>% 
  summarize(count = n())

###then label all country-years within spell, saying whether spell included erosion
vdem <- vdem %>% group_by(dem_spell_name) %>% mutate(dem_spell_erosion = sum(erode) > 0) %>% ungroup()
summary(vdem$dem_spell_erosion)

##review available cases against which to build predictive models 
###examine cases where consolidated regime autocratized
 ###note that some (eg Denmark 1902) ended in foreign occupation
vdem %>% filter(consolidated_lhb == TRUE, dem_spell_outcome == 'autocracy') %>% 
  distinct(dem_spell_name)
vdem %>% filter(consolidated_long == TRUE, dem_spell_outcome == 'autocracy') %>% 
  distinct(dem_spell_name)
vdem %>% filter(consolidated_high == TRUE, dem_spell_outcome == 'autocracy') %>% 
  distinct(dem_spell_name)
vdem %>% filter(consolidated_broad == TRUE, dem_spell_outcome == 'autocracy') %>% 
  distinct(dem_spell_name)
  
###examine cases where consolidated regime eroded
vdem %>% filter(consolidated_lhb == TRUE, erode == TRUE) %>% 
  distinct(dem_spell_name)
vdem %>% filter(consolidated_long == TRUE, erode == TRUE) %>% 
  distinct(dem_spell_name)
vdem %>% filter(consolidated_high == TRUE, erode == TRUE) %>% 
  distinct(dem_spell_name)
vdem %>% filter(consolidated_broad == TRUE, erode == TRUE) %>% 
  distinct(dem_spell_name)


#predict eventual autocratization or erosion ----

##produce a few illustrative statistics

###highest polyarchy peak that later autocratized
vdem %>% 
  filter(dem_spell_outcome == 'autocracy') %>% 
  group_by(dem_spell_name) %>%
  summarize(farthest_faller = max(dem_spell_peak), last_year = max(year))
  arrange(desc(farthest_faller))

###distribution of lowest polyarchy scores after a country reached a high threshold
####identify mean and sd for all democratic country-years
dems_polyarchy_mean <- vdem %>% 
  filter(v2x_polyarchy >= dem_threshold) %>%
  summarize(dems_polyarchy_mean = mean(v2x_polyarchy)) %>%
  pull(dems_polyarchy_mean)
dems_polyarchy_sd <- vdem %>% 
  filter(v2x_polyarchy >= dem_threshold) %>%
  summarize(dems_polyarchy_sd = sd(v2x_polyarchy)) %>%
  pull(dems_polyarchy_sd)

####identify all democratic spells that ever exceeded 1 sd above democratic mean
high_dems_reference_set <-
vdem %>%
  filter(v2x_polyarchy >= dems_polyarchy_mean + dems_polyarchy_sd) %>%
  group_by(country_name) %>%
  summarize(year_above_highthreshold = min(year))

####log the lowest polyarchy score in a spell after it exceeded 1 sd above mean
high_dems_reference_set$low_after_high <- as.numeric(NA)
for (i in seq_along(high_dems_reference_set$country_name)){
  high_dems_reference_set$low_after_high[i] <-
    vdem %>% filter(country_name == high_dems_reference_set$country_name[i] &
                    year >= high_dems_reference_set$year_above_highthreshold[i]) %>%
    summarize(low_after_high = min(v2x_polyarchy, na.rm = TRUE)) %>%
    pull(low_after_high)
}

####identify lowest sinking democracies that previously exceeded 1 sd above mean
high_dems_reference_set %>% arrange(low_after_high)
count(high_dems_reference_set, autocratized_after_high_threshold = low_after_high < dem_threshold)
count(high_dems_reference_set, fell_below_avg_dem_after_high_threshold = low_after_high < dems_polyarchy_mean)

####chart distribution of lowest polyarchy scores after exceeding 1 sd above mean
ggplot(data = high_dems_reference_set, aes(x = low_after_high))+
  geom_histogram(binwidth = 0.05)+
  geom_vline(xintercept = dems_polyarchy_mean)+
  geom_vline(xintercept = dems_polyarchy_mean + dems_polyarchy_sd, linetype = 'dashed')


##examine independent variables

###clientelism
summary(vdem$v2xnp_client) #clientelism index, rolls up psprlnks and dlencmps plus additional variables
summary(vdem$v2psprlnks) #how parties link to constituents
summary(vdem$v2dlencmps) #particularistic social spending

vdem %>% select(v2xnp_client, v2psprlnks, v2dlencmps) %>%
  na.omit() %>% 
  cor()

###information control
summary(vdem$v2smonex) # online media consumption
summary(vdem$v2smmefra) # online media fractionalization
summary(vdem$v2smgovdom) # government disseminates false info
summary(vdem$v2smpardom) # party disseminates false info
summary(vdem$v2smfordom) # foreign governments inject false info

vdem %>% select(v2smonex, v2smmefra, v2smgovdom, v2smpardom, v2smfordom) %>%
  na.omit() %>% 
  cor()

###polarization
summary(vdem$v2cacamps) # political polarization extends into society
summary(vdem$v2smpolsoc) # societal polarization
summary(vdem$v2smpolhate) # parties use hate speech

vdem %>% select(v2cacamps, v2smpolsoc, v2smpolhate) %>%
  na.omit() %>% 
  cor()

##model nomenclature ----
#m1 = all democratic cases, predict autocratization
#m2 = consolidated cases, predict autocratization
#m3 = all democratic cases, predict erosion
#m4 = consolidated cases, predict erosion
vdem_dem = vdem %>% filter(v2x_polyarchy >= dem_threshold)
vdem_con = vdem %>% filter(consolidated_lhb == TRUE)

##model clientelism ----
cm1 <- lm(dem_spell_outcome == 'autocracy' ~ v2xnp_client, data = vdem_dem)
summary(cm1)

cm2 <- lm(dem_spell_outcome == 'autocracy' ~ v2xnp_client, data = vdem_con)
summary(cm2)

cm3 <- lm(dem_spell_erosion == TRUE ~ v2xnp_client, data = vdem_dem)
summary(cm3)

cm4 <- lm(dem_spell_erosion == TRUE ~ v2xnp_client, data = vdem_con)
summary(cm4)


##model media----
mm1_full <- lm(dem_spell_outcome == 'autocracy' ~ v2smonex + v2smmefra + v2smgovdom + v2smpardom + v2smfordom, data = vdem_dem)
summary(mm1_full)

mm2_full <- lm(dem_spell_outcome == 'autocracy' ~ v2smonex + v2smmefra + v2smgovdom + v2smpardom + v2smfordom, data = vdem_con)
summary(mm2_full)

mm3_full <- lm(dem_spell_erosion == TRUE ~ v2smonex + v2smmefra + v2smgovdom + v2smpardom + v2smfordom, data = vdem_dem)
summary(mm3_full)

mm4_full <- lm(dem_spell_erosion == TRUE ~ v2smonex + v2smmefra + v2smgovdom + v2smpardom + v2smfordom, data = vdem_con)
summary(mm4_full)


##model polarization----
pm1_full <- lm(dem_spell_outcome == 'autocracy' ~ v2cacamps + v2smpolsoc + v2smpolhate, data = vdem_dem)
summary(pm1_full)

pm2_full <- lm(dem_spell_outcome == 'autocracy' ~ v2cacamps + v2smpolsoc + v2smpolhate, data = vdem_con)
summary(pm2_full)

pm3_full <- lm(dem_spell_erosion == TRUE ~ v2cacamps + v2smpolsoc + v2smpolhate, data = vdem_dem)
summary(pm3_full)

pm4_full <- lm(dem_spell_erosion == TRUE ~ v2cacamps + v2smpolsoc + v2smpolhate, data = vdem_con)
summary(pm4_full)

##search for most telling time lag
###write custom function to find optimal time lag for regression on any set of variables
test_lags <- function(df = vdem_con, vars) {
  model_results <- data.frame(time_lag = c(1:lag_range),
                              regression_coef = as.numeric(NA),
                              p_value = as.numeric(NA))
  
  for (i in 1:lag_range){
    wdf <- df %>%
      group_by(country_name) %>%
      arrange(year) %>%
      mutate(v2x_polyarchy_lagged = lead(v2x_polyarchy, n = i)) %>%
      ungroup()
    wdf$polyarchy_change <- wdf$v2x_polyarchy_lagged - wdf$v2x_polyarchy
    wdf$year_factor <- as.factor(wdf$year)
    
    wm <- lm(polyarchy_change ~ ., data = wdf[,c('polyarchy_change', vars, 'year_factor')])
    model_results$regression_coef[i] <- summary(wm)$coefficients[2,'Estimate'] #coefficient
    model_results$p_value[i] <- summary(wm)$coefficients[2, 4] #p-value
  }
  print(model_results)
  print(model_results[model_results %>%
                        filter(p_value < 0.05) %>%
                        summarize(strongest_prediction = time_lag[which.max(abs(regression_coef))]) %>%
                        pull(strongest_prediction),])
}

test_lags(vars = 'v2xnp_client')
test_lags(vars = c('v2xnp_client', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('v2xnp_client', 'v2x_polyarchy', 'e_migdppc', 'e_total_resources_percent'))
test_lags(vars = c('v2smonex', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('v2smmefra', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('v2smgovdom', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('v2smpardom', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('v2smfordom', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('v2cacamps', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('v2smpolsoc', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('v2smpolhate', 'v2x_polyarchy', 'e_migdppc'))

test_lags(vars = c('clientXresources', 'v2xnp_client', 'e_total_resources_percent', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('smmefraXsmpardom', 'v2smmefra', 'v2smpardom', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('smonexXsmmefra', 'v2smonex', 'v2smmefra', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('smonexXsmfordom', 'v2smonex', 'v2smfordom', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('smmefraXsmfordom', 'v2smmefra', 'v2smfordom', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('clientXcacamps', 'v2xnp_client', 'v2cacamps', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('clientXsmpolsoc', 'v2xnp_client', 'v2smpolsoc', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('smmefraXsmpolsoc', 'v2smmefra', 'v2smpolsoc', 'v2x_polyarchy', 'e_migdppc'))

#chart interacted variables
 ##works as hard coded. next step is to code generically for any two illustrated variables
illustrate_1 = 'v2smmefra'
illustrate_2 = 'v2smpolsoc'
lag_years = 10
point_scale = seq(from = 0, to = 1, by = 0.05)

#test code, genericized
mimir <- data.frame(independent_variable = rep(point_scale, times = length(point_scale)),
                    interacted_variable = rep(point_scale, each = length(point_scale)),
                    v2x_polyarchy = median(vdem_con$v2x_polyarchy, na.rm = TRUE),
                    e_migdppc = median(vdem_con$e_migdppc, na.rm = TRUE))

wdf <- vdem_con %>%
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(v2x_polyarchy_lagged = lead(v2x_polyarchy, n = lag_years)) %>%
  ungroup()
wdf$polyarchy_change <- wdf$v2x_polyarchy_lagged - wdf$v2x_polyarchy
wm <- lm(polyarchy_change ~  + v2x_polyarchy + e_migdppc, data = wdf)

mimir <- mimir %>% mutate(expected_polyarchy = predict(object = wm, newdata = mimir))
mimir %>% filter(v2smpolsoc %in% c(0.25, 0.75)) %>%
  ggplot(aes(x = v2smmefra, y = expected_polyarchy, color = as.factor(v2smpolsoc)))+
  geom_line()


#working code, hard coded
mimir <- data.frame(v2smmefra = rep(point_scale, times = length(point_scale)),
                    v2smpolsoc = rep(point_scale, each = length(point_scale)),
                    v2x_polyarchy = median(vdem_con$v2x_polyarchy, na.rm = TRUE),
                    e_migdppc = median(vdem_con$e_migdppc, na.rm = TRUE))
wdf <- vdem_con %>%
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(v2x_polyarchy_lagged = lead(v2x_polyarchy, n = lag_years)) %>%
  ungroup()
wdf$polyarchy_change <- wdf$v2x_polyarchy_lagged - wdf$v2x_polyarchy
wm <- lm(polyarchy_change ~ v2smmefra * v2smpolsoc + v2x_polyarchy + e_migdppc, data = wdf)

mimir <- mimir %>% mutate(expected_polyarchy = predict(object = wm, newdata = mimir))
mimir %>% filter(v2smpolsoc %in% c(0.25, 0.75)) %>%
ggplot(aes(x = v2smmefra, y = expected_polyarchy, color = as.factor(v2smpolsoc)))+
  geom_line()

#catch cursor










#notes----
##establish directionality of independent variables
##control for country- or year-fixed effects? seems especially important for online factors
##search for interactions:
 ##among media variables
 ##between media and polarization
 ##between clientelism and natural resources

setwd('C:/Tony/git_workspace/democratic_erosion')
#setwd('C:/Tony/Political Science MA/_thesis/quantitative work - backup copy/democratic_erosion')

#set arbitrary thresholds----
dem_threshold = 0.5 #on vdem's 1-point scales
survival_threshold = 0.5 #as a probability
erosion_threshold = 1 #as multiple of standard deviations
treatment_threshold = 0.9 #percentile that distinguishes high clientelism/disinfo/polarization from low
                          #must hold high threshold else few democratic spells experience onset of "treatment"
                          # because they start the spell already above the threshold
lag_range = 10 #test time lags of dependent variables from 1 to this many years

dem_color = 'dodgerblue'
interlock_color = 'midnightblue'
client_color = 'sienna'
media_color = 'forestgreen'
disinfo_color = 'olivedrab3'
polar_color = 'tomato1'

#load libraries
library(tidyverse)
library(scales)
library(knitr)  #for kable function to format regression results
library(stargazer)  #for displaying multiple model results
library(MatchIt)

#load and shape data ----

##regime type data from V-Dem - one row per country year 
#vdem <- read_csv('./data/V-Dem-CY-Full+Others-v11.1.csv')

##trim to a manageable file size by selecting only relevant variables
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

##count GDP per capita in thousands of dollars, changing from single dollars
vdem$e_migdppc <- vdem$e_migdppc / 1000 

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
  geom_point(size = 4, color = dem_color)+
  geom_smooth(method = 'loess', size = 2.5, color = dem_color)+
  coord_cartesian(ylim = c(0,1))+
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  geom_hline(yintercept = 0.8, linetype = 'dashed')+
  theme_minimal()+
  labs(title = 'Democracy Survival Rates',
       subtitle = '  by polyarchy height at year of observation',
       y = 'Survival Rate',
       x = 'V-Dem Polyarchy Score')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16)) 
ggsave(filename = "./visuals/democracy_survival_height.jpg",
       width = 10,
       height = 6,
       units = 'in')


##calculate minimum polyarchy to predict survival above threshold
height_threshold <- vdem %>% 
  filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(polyarchy_cohort = round(v2x_polyarchy, digits = 2)) %>%
  summarize(outcome_rate = sum(dem_spell_outcome == 'democracy') / n()) %>%
  filter(outcome_rate < survival_threshold) %>%
  summarize(last_cohort = max(polyarchy_cohort)) %>%
  pull(last_cohort) + 0.01
height_threshold
  
##chart outcome by democracy spell length
vdem %>% 
  filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(dem_spell_running) %>%
  mutate(outcome_rate = sum(dem_spell_outcome == 'democracy') / n()) %>%
  ggplot(aes(x = dem_spell_running, y = outcome_rate))+
  geom_point(size = 1.5, color = dem_color)+
  geom_smooth(method = 'loess', size = 2.5, color = dem_color)+
  coord_cartesian(ylim = c(0,1), xlim = c(0,110))+
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  geom_hline(yintercept = 0.8, linetype = 'dashed')+
  theme_minimal()+
  labs(title = 'Democracy Survival Rates',
       subtitle = '  by length of spell at year of observation',
       y = 'Survival Rate',
       x = 'Consecutive Years as Democracy (V-Dem Polyarchy)')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16)) 
ggsave(filename = "./visuals/democracy_survival_length.jpg",
       width = 10,
       height = 6,
       units = 'in')


##calculate fraction of all democracy spells that lasted
vdem %>% filter(dem_spell_running == 0) %>%
  summarize(count = n(), 
            stayed_democracies = sum(dem_spell_outcome == 'democracy'),
            survival_rate = stayed_democracies/count)

##calculate minimum spell length to predict survival above threshold
length_threshold <- vdem %>% 
  filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(dem_spell_running) %>%
  summarize(outcome_rate = sum(dem_spell_outcome == 'democracy') / n()) %>%
  filter(outcome_rate >= survival_threshold) %>%
  summarize(last_cohort = min(dem_spell_running)) %>%
  pull(last_cohort)
length_threshold

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
  geom_point(size = 4, color = dem_color)+
  geom_smooth(method = 'loess', size = 2.5, color = dem_color)+
  coord_cartesian(ylim = c(0,1))+
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  geom_hline(yintercept = 0.8, linetype = 'dashed')+
  theme_minimal()+
  labs(title = 'Democracy Survival Rates',
       subtitle = '  by liberal democracy at year of observation',
       y = 'Survival Rate',
       x = 'V-Dem Liberal Democracy Index')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16)) 
ggsave(filename = "./visuals/democracy_survival_libdem.jpg",
       width = 10,
       height = 6,
       units = 'in')

vdem %>% filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(partipdem_cohort = round(v2x_partipdem, digits = 2)) %>%
  mutate(outcome_rate = sum(dem_spell_outcome == 'democracy') / n()) %>%
  ggplot(aes(x = partipdem_cohort, y = outcome_rate))+
  geom_point(size = 4, color = dem_color)+
  geom_smooth(method = 'loess', size = 2.5, color = dem_color)+
  coord_cartesian(ylim = c(0,1))+
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  geom_hline(yintercept = 0.8, linetype = 'dashed')+
  theme_minimal()+
  labs(title = 'Democracy Survival Rates',
       subtitle = '  by participatory democracy at year of observation',
       y = 'Survival Rate',
       x = 'V-Dem Participatory Democracy Index')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16)) 
ggsave(filename = "./visuals/democracy_survival_partipdem.jpg",
       width = 10,
       height = 6,
       units = 'in')

vdem %>% filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(delibdem_cohort = round(v2x_delibdem, digits = 2)) %>%
  mutate(outcome_rate = sum(dem_spell_outcome == 'democracy') / n()) %>%
  ggplot(aes(x = delibdem_cohort, y = outcome_rate))+
  geom_point(size = 4, color = dem_color)+
  geom_smooth(method = 'loess', size = 2.5, color = dem_color)+
  coord_cartesian(ylim = c(0,1))+
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  geom_hline(yintercept = 0.8, linetype = 'dashed')+
  theme_minimal()+
  labs(title = 'Democracy Survival Rates',
       subtitle = '  by deliberative democracy at year of observation',
       y = 'Survival Rate',
       x = 'V-Dem Deliberative Democracy Index')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16)) 
ggsave(filename = "./visuals/democracy_survival_delibdem.jpg",
       width = 10,
       height = 6,
       units = 'in')

vdem %>% filter(v2x_polyarchy >= dem_threshold) %>%
  group_by(egaldem_cohort = round(v2x_egaldem, digits = 2)) %>%
  mutate(outcome_rate = sum(dem_spell_outcome == 'democracy') / n()) %>%
  ggplot(aes(x = egaldem_cohort, y = outcome_rate))+
  geom_point(size = 4, color = dem_color)+
  geom_smooth(method = 'loess', size = 2.5, color = dem_color)+
  coord_cartesian(ylim = c(0,1))+
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  geom_hline(yintercept = 0.8, linetype = 'dashed')+
  theme_minimal()+
  labs(title = 'Democracy Survival Rates',
       subtitle = '  by egalitarian democracy at year of observation',
       y = 'Survival Rate',
       x = 'V-Dem Egalitarian Democracy Index')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16)) 
ggsave(filename = "./visuals/democracy_survival_egaldem.jpg",
       width = 10,
       height = 6,
       units = 'in')

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
vdem <- vdem %>% 
  group_by(dem_spell_name) %>%
  mutate(dem_spell_erosion = case_when(max(vdem$v2x_polyarchy, na.rm = TRUE) < dem_threshold ~ as.logical(NA),
                                       max(vdem$dem_spell_peak, na.rm = TRUE) < (erosion_threshold * sd(vdem$v2x_polyarchy[vdem$v2x_polyarchy >= dem_threshold])) ~ as.logical(NA),
                                       sum(erode, na.rm = TRUE) > 0 ~ TRUE,
                                       TRUE ~ FALSE)) %>%
  ungroup()
summary(vdem$dem_spell_erosion)

vdem$dem_spell_outcome <- case_when(vdem$dem_spell_outcome == 'autocracy' ~ 'autocracy',
                                    vdem$dem_spell_erosion == TRUE ~ 'erosion',
                                    TRUE ~ as.character(vdem$dem_spell_outcome))
vdem$dem_spell_outcome <- as.factor(vdem$dem_spell_outcome)

##review available cases against which to build predictive models 
###count and itemize consolidated democracies, by outcome
vdem %>% 
  filter(consolidated_lhb == TRUE) %>% 
  group_by(dem_spell_outcome) %>%
  summarize(n_distinct(dem_spell_name))
  

interlocked_list <-
vdem %>% 
  filter(consolidated_lhb == TRUE) %>% 
  group_by('Democratic Spell' = dem_spell_name) %>%
  summarize(Outcome = case_when(sum(dem_spell_outcome == 'autocracy') > 0 ~ 'autocratized',
                                sum(dem_spell_erosion == TRUE) > 0 ~ 'eroded',
                                TRUE ~ 'remained democratic')) %>%
  arrange('Democratic Spell') %>%
  print(n = 100)

interlocked_list
write.table(interlocked_list, file = "./visuals/interlocked_list.txt", sep = ",", quote = FALSE, row.names = F)

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
  
###examine cases where consolidated regime eroded but didn't autocratize
vdem %>% filter(consolidated_lhb == TRUE, dem_spell_outcome == 'erosion') %>% 
  distinct(dem_spell_name)
vdem %>% filter(consolidated_long == TRUE, dem_spell_outcome == 'erosion') %>% 
  distinct(dem_spell_name)
vdem %>% filter(consolidated_high == TRUE, dem_spell_outcome == 'erosion') %>% 
  distinct(dem_spell_name)
vdem %>% filter(consolidated_broad == TRUE, dem_spell_outcome == 'erosion') %>% 
  distinct(dem_spell_name)

##spin off data frames for all democratic country-years
 ##and for all country-years within democratic spells that ever consolidated
vdem_dem = vdem %>% filter(v2x_polyarchy >= dem_threshold)
vdem <- vdem %>% 
  group_by(dem_spell_name) %>%
  mutate(ever_consolidated = if_else(sum(consolidated_lhb == TRUE, na.rm = TRUE) > 0,
                                     TRUE,
                                     FALSE)) %>%
  ungroup() 
vdem_con <- vdem %>%
  filter(ever_consolidated == TRUE)

#summary statistics ----

##year range
summary(vdem$year) #full data set
vdem %>% filter(v2x_polyarchy >= dem_threshold) %>% summarize(min(year)) #first year with democracy
vdem %>% filter(consolidated_lhb == TRUE) %>% summarize(min(year)) #first year with democracy
vdem %>% filter(!is.na(v2xnp_client)) %>% summarize(min(year), max(year), n_distinct(country_name))
vdem %>% filter(!is.na(v2cacamps)) %>% summarize(min(year), max(year), n_distinct(country_name))
vdem %>% filter(!is.na(smonexXsmmefra)) %>% summarize(min(year), max(year), n_distinct(country_name))
vdem %>% filter(!is.na(v2smpardom)) %>% summarize(min(year), max(year), n_distinct(country_name))

##number of regimes
n_distinct(vdem$country_name)
vdem %>% filter(!is.na(dem_spell_name)) %>% summarize(n_distinct(country_name))
vdem %>% filter(!is.na(dem_spell_name)) %>% summarize(n_distinct(dem_spell_name))

##distribution of variables
ggplot(data = vdem, aes(x = v2x_polyarchy))+
  geom_histogram(bins = 20, fill = 'dodgerblue', color = 'white')+
  theme_minimal()+
  labs(title = 'Distribution of Democracy',
       subtitle = '  all countries, 1789-2020',
       y = '',
       x = 'V-Dem Polyarchy Score')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank()) 
ggsave(filename = "./visuals/democracy_histogram_all.jpg",
       width = 10,
       height = 6,
       units = 'in')

vdem %>% filter(v2x_polyarchy >= dem_threshold) %>%
ggplot(aes(x = v2x_polyarchy))+
  geom_histogram(bins = 20, fill = 'dodgerblue', color = 'white')+
  theme_minimal()+
  labs(title = 'Distribution of Democracy',
       subtitle = '  across democratic country-years',
       y = '',
       x = 'V-Dem Polyarchy Score')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank()) 
ggsave(filename = "./visuals/democracy_histogram_dems.jpg",
       width = 10,
       height = 6,
       units = 'in')

ggplot(data = (vdem %>% filter(consolidated_lhb == TRUE)), aes(x = v2x_polyarchy))+
  geom_histogram(bins = 20, fill = 'dodgerblue', color = 'white')+
  theme_minimal()+
  labs(title = 'Distribution of Democracy',
       subtitle = '  interlocked democracies, 1900-2020',
       y = '',
       x = 'V-Dem Polyarchy Score')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank()) 
ggsave(filename = "./visuals/democracy_histogram_interlocked.jpg",
       width = 10,
       height = 6,
       units = 'in')


ggplot(data = vdem, aes(x = year, y = v2x_polyarchy))+
  geom_point(aes(color = consolidated_lhb == TRUE, alpha = consolidated_lhb == TRUE))+
  scale_color_manual(values = c(dem_color, interlock_color), na.value = 'gray50')+ #unconsolidated, consolidated, nondem
  scale_alpha_manual(values = c(0.1, 0.3), na.value = 0.06)+
  theme_minimal()+
  labs(title = 'Democracy over Time',
       #subtitle = '  noting interlocked democracies',
       x = '',
       y = 'V-Dem Polyarchy Score')+
  annotate('text', label = 'Democracies', color = dem_color, size = 7, x = 1825, y = 0.625)+
  annotate('text', label = 'Interlocked\nDemocracies', color = interlock_color, size = 7, x = 1860, y = 0.825)+
  theme(legend.position = 'none',
        title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(margin = margin(r = 8))) 
ggsave(filename = "./visuals/democracy_interlocked_over_time.jpg",
       width = 10,
       height = 6,
       units = 'in')


vdem %>% 
  ggplot(aes(x = v2xnp_client))+
  geom_histogram(bins = 20, fill = client_color, color = 'white')+
  theme_minimal()+
  labs(title = 'Distribution of Clientelism',
       subtitle = '  across all country-years',
       y = '',
       x = 'V-Dem Clientelism Index')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank()) 
ggsave(filename = "./visuals/client_histogram.jpg",
       width = 10,
       height = 6,
       units = 'in')

vdem %>% 
  ggplot(aes(x = v2cacamps))+
  geom_histogram(bins = 20, fill = polar_color, color = 'white')+
  theme_minimal()+
  labs(title = 'Distribution of Polarization',
       subtitle = '  across all country-years',
       y = '',
       x = 'V-Dem Political Polarization Index')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank()) 
ggsave(filename = "./visuals/polar_histogram.jpg",
       width = 10,
       height = 6,
       units = 'in')

vdem %>% 
  ggplot(aes(x = smonexXsmmefra))+
  geom_histogram(bins = 20, fill = media_color, color = 'white')+
  theme_minimal()+
  labs(title = 'Distribution of Media Use and Fractionalization',
       subtitle = '  across all country-years',
       y = '',
       x = 'V-Dem Online Media Use X Fractionalization')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank()) 
ggsave(filename = "./visuals/media_histogram.jpg",
       width = 10,
       height = 6,
       units = 'in')

vdem %>% 
  filter(!is.na(smonexXsmmefra)) %>%
  group_by(year_factor = as.factor(year)) %>%
  summarize(avg_smonexXsmmefra = mean(smonexXsmmefra)) %>%
  ggplot(aes(x = year_factor, y = avg_smonexXsmmefra))+
  geom_point(size = 5, color = media_color)+
  theme_minimal()+
  scale_x_discrete(breaks = c(2000,2005,2010,2015,2020))+
  labs(title = 'Rise in Media Use and Fractionalization',
       subtitle = '  yearly averages across all countries',
       y = 'V-Dem Media Use  X Fractionalization',
       x = '')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16)) 
ggsave(filename = "./visuals/media_chronology.jpg",
       width = 10,
       height = 6,
       units = 'in')

vdem %>% 
  ggplot(aes(x = v2smpardom))+
  geom_histogram(bins = 20, fill = disinfo_color, color = 'white')+
  theme_minimal()+
  labs(title = 'Distribution of Party Disinformation',
       subtitle = '  across all country-years',
       y = '',
       x = 'V-Dem Index of Party Disinformation')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank()) 
ggsave(filename = "./visuals/par_disinfo_histogram.jpg",
       width = 10,
       height = 6,
       units = 'in')

vdem %>% 
  filter(!is.na(v2smpardom)) %>%
  group_by(year_factor = as.factor(year)) %>%
  summarize(avg_v2smpardom = mean(v2smpardom)) %>%
  ggplot(aes(x = year_factor, y = avg_v2smpardom))+
  geom_point(size = 5, color = disinfo_color)+
  theme_minimal()+
  scale_x_discrete(breaks = c(2000,2005,2010,2015,2020))+
  labs(title = 'Rise in Party Disinformation',
       subtitle = '  yearly averages across all countries',
       y = 'V-Dem Index of Party Disinformation',
       x = '')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16)) 
ggsave(filename = "./visuals/par_disinfo_chronology.jpg",
       width = 10,
       height = 6,
       units = 'in')

vdem %>%
  filter(ever_consolidated == TRUE) %>% 
  group_by(dem_spell_name) %>% 
  summarize(age = max(dem_spell_length), outcome = (dem_spell_outcome)) %>% 
  ggplot(aes(x = age))+
  geom_histogram(aes(fill = outcome), binwidth = 20, color = 'white')

##demonstrate need for higher threshold than simply 0.5
vdem %>% filter(country_name == 'Albania', between(year, 2003, 2020)) %>%
  select(country_name, year, v2x_polyarchy) %>% 
  print(n = 20)

vdem %>% filter(country_name == 'Kosovo', between(year, 2001, 2020)) %>%
  select(country_name, year, v2x_polyarchy) %>% 
  print(n = 20)
  

#predict eventual autocratization or erosion ----

##produce a few illustrative statistics

###highest polyarchy peak that later autocratized
vdem %>% 
  filter(dem_spell_outcome == 'autocracy') %>% 
  group_by(dem_spell_name) %>%
  summarize(farthest_faller = max(dem_spell_peak), last_year = max(year)) %>%
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
#m5 = consolidated cases, predict autocratization using matching for controls
#m6 = consolidated cases, predict erosion using matching for controls
#m7 = consolidated cases, predict [autocratization or erosion]
#m8 = consolidated cases, predict [autocratization or erosion] using matching

##model clientelism ----
cm1 <- lm(dem_spell_outcome == 'autocracy' ~ v2xnp_client + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_dem)
summary(cm1)

cm2 <- lm(dem_spell_outcome == 'autocracy' ~ v2xnp_client + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_con)
summary(cm2)

cm3 <- lm(dem_spell_erosion == TRUE ~ v2xnp_client + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_dem)
summary(cm3)

cm4 <- lm(dem_spell_erosion == TRUE ~ v2xnp_client + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_con)
summary(cm4)

match_client <- matchit(v2xnp_client >= mean(vdem_con$v2xnp_client, na.rm = TRUE) ~ 
                          v2x_polyarchy + e_migdppc + as.factor(year), 
                        data = vdem_con %>% filter(!is.na(v2x_polyarchy), !is.na(e_migdppc)),
                        method = 'nearest', distance = 'glm')
summary(match_client, un = FALSE)
plot(match_client, type = "jitter", interactive = FALSE)
vdem_client <- match.data(match_client)
cm5 <- lm(dem_spell_outcome == 'autocracy' ~ v2xnp_client + v2x_polyarchy + e_migdppc + as.factor(year), 
          data = vdem_client, 
          weights = weights)
summary(cm5)

cm6 <- lm(dem_spell_erosion == TRUE ~ v2xnp_client + v2x_polyarchy + e_migdppc + as.factor(year), 
          data = vdem_client, 
          weights = weights)
summary(cm6)

cm7 <- lm(dem_spell_outcome != 'democracy' ~ v2xnp_client + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_con)
summary(cm7)

cm8 <- lm(dem_spell_outcome != 'democracy' ~ v2xnp_client + v2x_polyarchy + e_migdppc + as.factor(year), 
          data = vdem_client, 
          weights = weights)
summary(cm8)


##model media----
mm1_full <- lm(dem_spell_outcome == 'autocracy' ~ v2smonex + v2smmefra + v2smgovdom + v2smpardom + v2smfordom + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_dem)
summary(mm1_full)

mm2_full <- lm(dem_spell_outcome == 'autocracy' ~ v2smonex + v2smmefra + v2smgovdom + v2smpardom + v2smfordom + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_con)
summary(mm2_full)

mm3_full <- lm(dem_spell_erosion == TRUE ~ v2smonex + v2smmefra + v2smgovdom + v2smpardom + v2smfordom + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_dem)
summary(mm3_full)

mm4_full <- lm(dem_spell_erosion == TRUE ~ v2smonex + v2smmefra + v2smgovdom + v2smpardom + v2smfordom + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_con)
summary(mm4_full)

mm7_full <- lm(dem_spell_outcome != 'democracy' ~ v2smonex + v2smmefra + v2smgovdom + v2smpardom + v2smfordom + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_con)
summary(mm7_full)

match_consume <- matchit(v2smonex >= mean(vdem_con$v2smonex, na.rm = TRUE) ~ 
                           v2x_polyarchy + e_migdppc + as.factor(year), 
                         data = vdem_con %>% filter(!is.na(v2smonex), !is.na(v2x_polyarchy), !is.na(e_migdppc)),
                         method = 'nearest', distance = 'glm')
summary(match_consume, un = FALSE)
plot(match_consume, type = "jitter", interactive = FALSE)

vdem_consume <- match.data(match_consume)
mm5_consume <- lm(dem_spell_outcome == 'autocracy' ~ v2smonex + v2x_polyarchy + e_migdppc + as.factor(year), 
                  data = vdem_consume, 
                  weights = weights)
summary(mm5_consume)

mm8_consume <- lm(dem_spell_outcome != 'democracy' ~ v2smonex + v2x_polyarchy + e_migdppc + as.factor(year), 
                  data = vdem_consume, 
                  weights = weights)
summary(mm8_consume)


match_fraction <- matchit(v2smmefra >= mean(vdem_con$v2smmefra, na.rm = TRUE) ~ 
                            v2x_polyarchy + e_migdppc + as.factor(year), 
                          data = vdem_con %>% filter(!is.na(v2smmefra), !is.na(v2x_polyarchy), !is.na(e_migdppc)),
                          method = 'nearest', distance = 'glm')
summary(match_fraction, un = FALSE)
plot(match_fraction, type = "jitter", interactive = FALSE)

vdem_fraction <- match.data(match_fraction)
mm5_fraction <- lm(dem_spell_outcome == 'autocracy' ~ v2smmefra + v2x_polyarchy + e_migdppc + as.factor(year), 
                   data = vdem_fraction, 
                   weights = weights)
summary(mm5_fraction)

mm8_fraction <- lm(dem_spell_outcome != 'democracy' ~ v2smmefra + v2x_polyarchy + e_migdppc + as.factor(year), 
                   data = vdem_fraction, 
                   weights = weights)
summary(mm8_fraction)


match_gov_disinfo <- matchit(v2smgovdom >= mean(vdem_con$v2smgovdom, na.rm = TRUE) ~ 
                               v2x_polyarchy + e_migdppc + as.factor(year), 
                             data = vdem_con %>% filter(!is.na(v2smgovdom), !is.na(v2x_polyarchy), !is.na(e_migdppc)),
                             method = 'nearest', distance = 'glm')
summary(match_gov_disinfo, un = FALSE)
plot(match_gov_disinfo, type = "jitter", interactive = FALSE)

vdem_gov_disinfo <- match.data(match_gov_disinfo)
mm5_gov <- lm(dem_spell_outcome == 'autocracy' ~ v2smgovdom + v2x_polyarchy + e_migdppc + as.factor(year), 
              data = vdem_gov_disinfo, 
              weights = weights)
summary(mm5_gov)

mm8_gov <- lm(dem_spell_outcome != 'democracy' ~ v2smgovdom + v2x_polyarchy + e_migdppc + as.factor(year), 
              data = vdem_gov_disinfo, 
              weights = weights)
summary(mm8_gov)


match_par_disinfo <- matchit(v2smpardom >= mean(vdem_con$v2smpardom, na.rm = TRUE) ~ 
                               v2x_polyarchy + e_migdppc + as.factor(year), 
                             data = vdem_con %>% filter(!is.na(v2smpardom), !is.na(v2x_polyarchy), !is.na(e_migdppc)),
                             method = 'nearest', distance = 'glm')
summary(match_par_disinfo, un = FALSE)
plot(match_par_disinfo, type = "jitter", interactive = FALSE)

vdem_par_disinfo <- match.data(match_par_disinfo)
mm5_par <- lm(dem_spell_outcome == 'autocracy' ~ v2smpardom + v2x_polyarchy + e_migdppc + as.factor(year), 
              data = vdem_gov_disinfo, 
              weights = weights)
summary(mm5_par)

mm8_par <- lm(dem_spell_outcome != 'democracy' ~ v2smpardom + v2x_polyarchy + e_migdppc + as.factor(year), 
              data = vdem_gov_disinfo, 
              weights = weights)
summary(mm8_par)


match_for_disinfo <- matchit(v2smfordom >= mean(vdem_con$v2smfordom, na.rm = TRUE) ~ 
                               v2x_polyarchy + e_migdppc + as.factor(year), 
                             data = vdem_con %>% filter(!is.na(v2smfordom), !is.na(v2x_polyarchy), !is.na(e_migdppc)),
                             method = 'nearest', distance = 'glm')
summary(match_for_disinfo, un = FALSE)
plot(match_for_disinfo, type = "jitter", interactive = FALSE)

vdem_for_disinfo <- match.data(match_for_disinfo)
mm5_for <- lm(dem_spell_outcome == 'autocracy' ~ v2smfordom + v2x_polyarchy + e_migdppc + as.factor(year), 
              data = vdem_for_disinfo, 
              weights = weights)
summary(mm5_for)

mm8_for <- lm(dem_spell_outcome != 'democracy' ~ v2smfordom + v2x_polyarchy + e_migdppc + as.factor(year), 
              data = vdem_for_disinfo, 
              weights = weights)
summary(mm8_for)


##model polarization----
pm1_full <- lm(dem_spell_outcome == 'autocracy' ~ v2cacamps + v2smpolsoc + v2smpolhate + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_dem)
summary(pm1_full)

pm2_full <- lm(dem_spell_outcome == 'autocracy' ~ v2cacamps + v2smpolsoc + v2smpolhate + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_con)
summary(pm2_full)

pm3_full <- lm(dem_spell_erosion == TRUE ~ v2cacamps + v2smpolsoc + v2smpolhate + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_dem)
summary(pm3_full)

pm4_full <- lm(dem_spell_erosion == TRUE ~ v2cacamps + v2smpolsoc + v2smpolhate + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_con)
summary(pm4_full)

pm7_full <- lm(dem_spell_outcome != 'democracy' ~ v2cacamps + v2smpolsoc + v2smpolhate + v2x_polyarchy + e_migdppc + as.factor(year), data = vdem_con)
summary(pm7_full)

##search for most telling time lag----
###write custom function to find optimal time lag for regression on any set of variables
test_lags <- function(df = vdem, vars) {
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
    wdf <- wdf %>% filter(consolidated_lhb == TRUE)
    
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

 ###models with interacted variables
test_lags(vars = c('smmefraXsmpardom', 'v2smmefra', 'v2smpardom', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('smonexXsmmefra', 'v2smonex', 'v2smmefra', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('smonexXsmfordom', 'v2smonex', 'v2smfordom', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('smmefraXsmfordom', 'v2smmefra', 'v2smfordom', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('clientXsmpolsoc', 'v2xnp_client', 'v2smpolsoc', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('smmefraXsmpolsoc', 'v2smmefra', 'v2smpolsoc', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('clientXresources', 'v2xnp_client', 'e_total_resources_percent', 'v2x_polyarchy', 'e_migdppc'))
test_lags(vars = c('clientXcacamps', 'v2xnp_client', 'v2cacamps', 'v2x_polyarchy', 'e_migdppc'))


##run models at best time lag of 10 years, and export results ----
vdem <- vdem %>% 
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(v2x_polyarchy_lagged = lead(v2x_polyarchy, n = 10)) %>%
  ungroup()
vdem$polyarchy_change <- vdem$v2x_polyarchy_lagged - vdem$v2x_polyarchy

cm_print <- lm(polyarchy_change ~ v2xnp_client + v2x_polyarchy + e_migdppc + as.factor(year), 
               data = (vdem %>% filter(consolidated_lhb == TRUE)))
summary(cm_print)

pm_print <- lm(polyarchy_change ~ v2cacamps + v2x_polyarchy + e_migdppc + as.factor(year), 
               data = (vdem %>% filter(consolidated_lhb == TRUE)))
summary(pm_print)

mm_print <- lm(polyarchy_change ~ (v2smonex * v2smmefra) + v2x_polyarchy + e_migdppc + as.factor(year), 
               data = (vdem %>% filter(consolidated_lhb == TRUE)))
summary(mm_print)

dm_print <- lm(polyarchy_change ~ v2smpardom + v2x_polyarchy + e_migdppc + as.factor(year), 
               data = (vdem %>% filter(consolidated_lhb == TRUE)))
summary(dm_print)

fullm_print <- lm(polyarchy_change ~ v2xnp_client + v2cacamps + (v2smonex * v2smmefra) + v2smpardom + v2x_polyarchy + e_migdppc + as.factor(year), 
                  data = (vdem %>% filter(consolidated_lhb == TRUE)))
summary(fullm_print)

threem_print <- lm(polyarchy_change ~ v2xnp_client + v2cacamps + v2smpardom + v2x_polyarchy + e_migdppc + as.factor(year), 
                         data = (vdem %>% filter(consolidated_lhb == TRUE)))
summary(threem_print)

###save out model results
stargazer(cm_print, pm_print, mm_print, dm_print, fullm_print, 
          title = 'Solvents of Democracy',
          keep = c('polyarchy_change', 'v2xnp_client', 'v2cacamps', 'v2smonex', 'v2smmefra', 'v2smpardom',
                   'v2x_polyarchy', 'e_migdppc'),
          dep.var.labels = 'Change in Polyarchy Score After 10 Years',
          model.names = TRUE,
          covariate.labels = c('Clientelism', 'Polarization', 'Online Media Consumption', 'Online Media Fractionalization', 'Party Disinformation',
                               'Level of Democracy', 'GDP Per Capita', 'Online Media Consumption X Fractionalization'),
          nobs = TRUE,
          type = 'html',
          out = './models/democracy_erosion_model_results.doc')

stargazer(cm_print, pm_print, dm_print, threem_print, 
          title = 'Solvents of Democracy',
          keep = c('polyarchy_change', 'v2xnp_client', 'v2cacamps', 'v2smpardom',
                   'v2x_polyarchy', 'e_migdppc'),
          dep.var.labels = 'Change in Polyarchy Score After 10 Years',
          model.names = TRUE,
          covariate.labels = c('Clientelism', 'Polarization', 'Party Disinformation',
                               'Level of Democracy', 'GDP Per Capita'),
          nobs = TRUE,
          type = 'html',
          out = './models/corrosive_factors_model_results.doc')


##chart interacted variables----
lag_years = 10
point_scale = seq(from = 0, to = 1, by = 0.05)
df = vdem_con

##party disinformation by media fractionalization
mimir <- data.frame(v2smpardom = rep(point_scale, times = length(point_scale)),
                    v2smmefra = rep(point_scale, each = length(point_scale)),
                    v2x_polyarchy = median(vdem_con$v2x_polyarchy[df$v2x_polyarchy >= 0.5], na.rm = TRUE),
                    e_migdppc = median(vdem_con$e_migdppc[df$v2x_polyarchy >= 0.5], na.rm = TRUE))
wdf <- vdem %>%
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(v2x_polyarchy_lagged = lead(v2x_polyarchy, n = lag_years)) %>%
  ungroup()%>%
  filter(consolidated_lhb == TRUE)
wdf$polyarchy_change <- wdf$v2x_polyarchy_lagged - wdf$v2x_polyarchy
wm <- lm(polyarchy_change ~ v2smpardom * v2smmefra + v2x_polyarchy + e_migdppc, data = wdf)

mimir <- mimir %>% mutate(expected_polyarchy = predict(object = wm, newdata = mimir))
mimir %>% filter(v2smmefra %in% c(0.25, 0.75)) %>%
ggplot(aes(x = v2smpardom, y = expected_polyarchy, color = as.factor(v2smmefra)))+
  geom_line(size = 2.5)+
  scale_color_manual(values = c(disinfo_color, polar_color),
                     labels = c('low', 'high'))+
  theme_minimal()+
  labs(title = 'Party Disinformation X Fractionalization',
       subtitle = '  affect democracy 10 years later',
       y = 'Predicted Polyarchy Change',
       x = 'Party Disinformation',
       color = 'Fractionalization')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 8)),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))
ggsave(filename = "./visuals/model_pardisinfo_fract_inter.jpg",
       width = 10,
       height = 6,
       units = 'in')

##online consumption by media fractionalization
mimir <- data.frame(v2smonex = rep(point_scale, times = length(point_scale)),
                    v2smmefra = rep(point_scale, each = length(point_scale)),
                    v2x_polyarchy = median(vdem_con$v2x_polyarchy[df$v2x_polyarchy >= 0.5], na.rm = TRUE),
                    e_migdppc = median(vdem_con$e_migdppc[df$v2x_polyarchy >= 0.5], na.rm = TRUE),
                    year = as.factor(2010))
wdf <- vdem %>%
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(v2x_polyarchy_lagged = lead(v2x_polyarchy, n = lag_years)) %>%
  ungroup() %>%
  filter(consolidated_lhb == TRUE)
wdf$polyarchy_change <- wdf$v2x_polyarchy_lagged - wdf$v2x_polyarchy
wm <- lm(polyarchy_change ~ v2smonex * v2smmefra + v2x_polyarchy + e_migdppc + as.factor(year), data = wdf)

mimir <- mimir %>% mutate(expected_polyarchy = predict(object = wm, newdata = mimir))
mimir %>% filter(v2smmefra %in% c(0.25, 0.75)) %>%
  ggplot(aes(x = v2smonex, y = expected_polyarchy, color = as.factor(v2smmefra)))+
  geom_line(size = 2.5)+
  scale_color_manual(guide = 'none', 
                     values = c(media_color, disinfo_color))+
  theme_minimal()+
  labs(title = 'Online Use X Fractionalization',
       subtitle = '  affect democracy 10 years later',
       y = 'Predicted Polyarchy Change',
       x = 'Online Media Consumption')+
  annotate('text', label = 'at low fractionalization', color = media_color, size = 7, x = 0.25, y = -0.04)+
  annotate('text', label = 'at high fractionalization', color = disinfo_color, size = 7, x = 0.66, y = -0.125)+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 8)),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        legend.position = 'none') 
ggsave(filename = "./visuals/model_online_fract_inter.jpg",
       width = 10,
       height = 6,
       units = 'in')

##online consumption by foreign disinformation
mimir <- data.frame(v2smonex = rep(point_scale, times = length(point_scale)),
                    v2smfordom = rep(point_scale, each = length(point_scale)),
                    v2x_polyarchy = median(vdem_con$v2x_polyarchy[df$v2x_polyarchy >= 0.5], na.rm = TRUE),
                    e_migdppc = median(vdem_con$e_migdppc[df$v2x_polyarchy >= 0.5], na.rm = TRUE))
wdf <- vdem %>%
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(v2x_polyarchy_lagged = lead(v2x_polyarchy, n = lag_years)) %>%
  ungroup()%>%
  filter(consolidated_lhb == TRUE)
wdf$polyarchy_change <- wdf$v2x_polyarchy_lagged - wdf$v2x_polyarchy
wm <- lm(polyarchy_change ~ v2smonex * v2smfordom + v2x_polyarchy + e_migdppc, data = wdf)

mimir <- mimir %>% mutate(expected_polyarchy = predict(object = wm, newdata = mimir))
mimir %>% filter(v2smfordom %in% c(0.25, 0.75)) %>%
  ggplot(aes(x = v2smonex, y = expected_polyarchy, color = as.factor(v2smfordom)))+
  geom_line(size = 2.5)+
  scale_color_manual(values = c(media_color, disinfo_color),
                     labels = c('low', 'high'))+
  theme_minimal()+
  labs(title = 'Online Consumption X Foreign Disinformation',
       subtitle = '  affect democracy 10 years later',
       y = 'Predicted Polyarchy Change',
       x = 'Online Consumption',
       color = 'Foreign\n Disinfo')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 8)),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))
ggsave(filename = "./visuals/model_online_fordisinfo_inter.jpg",
       width = 10,
       height = 6,
       units = 'in')

##foreign disinformation by media fractionalization
mimir <- data.frame(v2smfordom = rep(point_scale, times = length(point_scale)),
                    v2smmefra = rep(point_scale, each = length(point_scale)),
                    v2x_polyarchy = median(vdem_con$v2x_polyarchy[df$v2x_polyarchy >= 0.5], na.rm = TRUE),
                    e_migdppc = median(vdem_con$e_migdppc[df$v2x_polyarchy >= 0.5], na.rm = TRUE))
wdf <- vdem %>%
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(v2x_polyarchy_lagged = lead(v2x_polyarchy, n = lag_years)) %>%
  ungroup()%>%
  filter(consolidated_lhb == TRUE)
wdf$polyarchy_change <- wdf$v2x_polyarchy_lagged - wdf$v2x_polyarchy
wm <- lm(polyarchy_change ~ v2smfordom * v2smmefra + v2x_polyarchy + e_migdppc, data = wdf)

mimir <- mimir %>% mutate(expected_polyarchy = predict(object = wm, newdata = mimir))
mimir %>% filter(v2smmefra %in% c(0.25, 0.75)) %>%
  ggplot(aes(x = v2smfordom, y = expected_polyarchy, color = as.factor(v2smmefra)))+
  geom_line(size = 2.5)+
  scale_color_manual(values = c(disinfo_color, polar_color),
                     labels = c('low', 'high'))+
  theme_minimal()+
  labs(title = 'Foreign Disinformation X Fractionalization',
       subtitle = '  affect democracy 10 years later',
       y = 'Predicted Polyarchy Change',
       x = 'Foreign Disinformation',
       color = 'Fractionalization')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 8)),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))
ggsave(filename = "./visuals/model_fordisinfo_fract_inter.jpg",
       width = 10,
       height = 6,
       units = 'in')

##clientelism by polarized society
mimir <- data.frame(v2xnp_client = rep(point_scale, times = length(point_scale)),
                    v2smpolsoc = rep(point_scale, each = length(point_scale)),
                    v2x_polyarchy = median(vdem_con$v2x_polyarchy[df$v2x_polyarchy >= 0.5], na.rm = TRUE),
                    e_migdppc = median(vdem_con$e_migdppc[df$v2x_polyarchy >= 0.5], na.rm = TRUE))
wdf <- vdem %>%
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(v2x_polyarchy_lagged = lead(v2x_polyarchy, n = lag_years)) %>%
  ungroup()%>%
  filter(consolidated_lhb == TRUE)
wdf$polyarchy_change <- wdf$v2x_polyarchy_lagged - wdf$v2x_polyarchy
wm <- lm(polyarchy_change ~ v2xnp_client * v2smpolsoc + v2x_polyarchy + e_migdppc, data = wdf)

mimir <- mimir %>% mutate(expected_polyarchy = predict(object = wm, newdata = mimir))
mimir %>% filter(v2smpolsoc %in% c(0.25, 0.75)) %>%
  ggplot(aes(x = v2xnp_client, y = expected_polyarchy, color = as.factor(v2smpolsoc)))+
  geom_line(size = 2.5)+
  scale_color_manual(values = c(client_color, polar_color),
                     labels = c('low', 'high'))+
  theme_minimal()+
  labs(title = 'Clientelism X Polarization',
       subtitle = '  affect democracy 10 years later',
       y = 'Predicted Polyarchy Change',
       x = 'Clientelism',
       color = 'Polarization')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 8)),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))
ggsave(filename = "./visuals/model_client_pol_inter.jpg",
       width = 10,
       height = 6,
       units = 'in')

##media fractionalization by polarized society
mimir <- data.frame(v2smmefra = rep(point_scale, times = length(point_scale)),
                    v2smpolsoc = rep(point_scale, each = length(point_scale)),
                    v2x_polyarchy = median(vdem_con$v2x_polyarchy[df$v2x_polyarchy >= 0.5], na.rm = TRUE),
                    e_migdppc = median(vdem_con$e_migdppc[df$v2x_polyarchy >= 0.5], na.rm = TRUE))
wdf <- vdem %>%
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(v2x_polyarchy_lagged = lead(v2x_polyarchy, n = lag_years)) %>%
  ungroup()%>%
  filter(consolidated_lhb == TRUE)
wdf$polyarchy_change <- wdf$v2x_polyarchy_lagged - wdf$v2x_polyarchy
wm <- lm(polyarchy_change ~ v2smmefra * v2smpolsoc + v2x_polyarchy + e_migdppc, data = wdf)

mimir <- mimir %>% mutate(expected_polyarchy = predict(object = wm, newdata = mimir))
mimir %>% filter(v2smpolsoc %in% c(0.25, 0.75)) %>%
  ggplot(aes(x = v2smmefra, y = expected_polyarchy, color = as.factor(v2smpolsoc)))+
  geom_line(size = 2.5)+
  scale_color_manual(values = c(media_color, polar_color),
                     labels = c('low', 'high'))+
  theme_minimal()+
  labs(title = 'Fractionalization X Polarization',
       subtitle = '  affect democracy 10 years later',
       y = 'Predicted Polyarchy Change',
       x = 'Fractionalization',
       color = 'Polarization')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 8)),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))
ggsave(filename = "./visuals/model_fract_pol_inter.jpg",
       width = 10,
       height = 6,
       units = 'in')

##polarized society by media fractionalization
mimir <- data.frame(v2cacamps = rep(point_scale, times = length(point_scale)),
                     v2smmefra = rep(point_scale, each = length(point_scale)),
                    v2x_polyarchy = median(vdem_con$v2x_polyarchy[df$v2x_polyarchy >= 0.5], na.rm = TRUE),
                    e_migdppc = median(vdem_con$e_migdppc[df$v2x_polyarchy >= 0.5], na.rm = TRUE))
wdf <- vdem %>%
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(v2x_polyarchy_lagged = lead(v2x_polyarchy, n = lag_years)) %>%
  ungroup()%>%
  filter(consolidated_lhb == TRUE)
wdf$polyarchy_change <- wdf$v2x_polyarchy_lagged - wdf$v2x_polyarchy
wm <- lm(polyarchy_change ~ (v2cacamps * v2smmefra) + v2x_polyarchy + e_migdppc, data = wdf)

mimir <- mimir %>% mutate(expected_polyarchy = predict(object = wm, newdata = mimir))
mimir %>% filter(v2smmefra %in% c(0.25, 0.75)) %>%
  ggplot(aes(x = v2cacamps, y = expected_polyarchy, color = as.factor(v2smmefra)))+
  geom_line(size = 2.5)+
  scale_color_manual(values = c(media_color, polar_color),
                     labels = c('low', 'high'))+
  theme_minimal()+
  labs(title = 'Polarization X Fractionalization',
       subtitle = '  affect democracy 10 years later',
       y = 'Predicted Polyarchy Change',
       x = 'Polarization',
       color = 'Fractionalization')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.title.y = element_text(margin = margin(r = 8)),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))
ggsave(filename = "./visuals/model_pol_fract_inter.jpg",
       width = 10,
       height = 6,
       units = 'in')


##difference in difference charts----
###observe polyarchy relative to treatments

####dif in dif - clientelism
 #set level that counts as being "treated" with clientelism
clientelism_threshold <- as.numeric(quantile(vdem_con$v2xnp_client, probs = treatment_threshold, na.rm = TRUE))

 #label years relative to onset of clientelism treatment 
 #only count as treatment if happens while inside a democratic spell,
 # and if first year of spell did not see high clientelism
 #but year count extends ten years prior and after the treatment, regardless of democratic status in those years
vdem$year_rel_client <- as.numeric(NA)
vdem$client_control_poly <- as.numeric(NA)
for (i in seq_along(vdem$year)){
  #skip country-years that are not in democratic spells or lack score
  if (is.na(vdem$dem_spell_name[i])) next
  if (is.na(vdem$v2xnp_client[i])) next
  #skip country-years that are in democratic spells that began with high score
  if (vdem$v2xnp_client[!is.na(vdem$dem_spell_name) &
                        vdem$dem_spell_name == vdem$dem_spell_name[i] &
                        !is.na(vdem$dem_spell_running) &
                        vdem$dem_spell_running == 0] > clientelism_threshold) next
  
  #find earliest year within each dem spell that crosses treatment threshold. 
  client_year_zero <- vdem %>% filter(dem_spell_name == vdem$dem_spell_name[i]) %>%
    filter(v2xnp_client >= clientelism_threshold) %>%
    summarize(client_year_zero = min(year, na.rm = TRUE)) %>%
    pull(client_year_zero)
  
  #renumber infinite values as nulls
  #value is Inf if spell never crossed threshold
  client_year_zero <- if_else((client_year_zero == Inf | client_year_zero == -Inf), 
                              as.numeric(NA),
                              client_year_zero)
  
  if (is.na(client_year_zero)) next
  
  #label year of onset as relative year zero
  vdem$year_rel_client[i] <- if_else(vdem$year[i] == client_year_zero,
                                     0,
                                     as.numeric(NA))
}
for (i in seq_along(vdem$year)){
  #locate treatment onset within 10 years of each country-year
  client_year_zero <- vdem %>% 
    filter(country_name == vdem$country_name[i] &
             year >= (vdem$year[i] - 10) &
             year <= (vdem$year[i] + 10) &
             year_rel_client == 0) %>%
    summarize(client_year_zero = min(year)) %>%
    pull(client_year_zero)

  #renumber infinite values as nulls
  #value is Inf if spell never crossed threshold
  client_year_zero <- if_else((client_year_zero == Inf | client_year_zero == -Inf), 
                              as.numeric(NA),
                              client_year_zero)
    
  if (is.na(client_year_zero)) next
  
  #label all preceding and following years in spell, relative to that zero  
  vdem$year_rel_client[i] = vdem$year[i] - client_year_zero
  
  #log control value of clientelism among all consolidated democracies in same absolute year
  vdem$client_control_poly[i] <- if_else(is.na(vdem$year_rel_client[i]),
                                         as.numeric(NA),
                                         mean(vdem$v2x_polyarchy[vdem$v2x_polyarchy >= dem_threshold & 
                                                                   vdem$year == vdem$year[i] &
                                                                   vdem$country_name != vdem$country_name[i]],
                                              na.rm = TRUE))
}
summary(vdem$year_rel_client)
summary(vdem$client_control_poly)

vdem %>%
  filter(!is.na(year_rel_client)) %>%
  group_by(year_rel_client) %>%
  summarize(polyarchy_client = mean(v2x_polyarchy, na.rm = TRUE),
            polyarchy_control = mean(client_control_poly, na.rm = TRUE)) %>%
  ggplot(aes(x = year_rel_client))+
  geom_point(aes(y = polyarchy_client), color = client_color, size = 4)+
  geom_smooth(data = . %>% filter(year_rel_client < 0),
              method = 'loess',
    aes(y = polyarchy_client), color = client_color, size = 2.5, fill = client_color)+
  geom_smooth(data = . %>% filter(year_rel_client > 0),
              method = 'loess',
              aes(y = polyarchy_client), color = client_color, size = 2.5, fill = client_color)+
  geom_point(aes(y = polyarchy_control), color = dem_color, size = 4)+
  geom_smooth(data = . %>% filter(year_rel_client < 0),
              method = 'loess',
              aes(y = polyarchy_control), color = dem_color, size = 2.5, fill = dem_color)+
  geom_smooth(data = . %>% filter(year_rel_client > 0),
              method = 'loess',
              aes(y = polyarchy_control), color = dem_color, size = 2.5, fill = dem_color)+
  coord_cartesian(xlim = c(-10,10), ylim = c(0,1))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  annotate('text', label = 'Democracies with\n Clientelism', x = 5, y = 0.375, color = client_color, size = 8)+
  annotate('text', label = 'Other Democracies', x = 5, y = 0.875, color = dem_color, size = 8)+
  theme_minimal()+
  labs(title = 'Onset of Clientelism',
       subtitle = '  effect on democracy',
       y = 'V-Dem Polyarchy Score',
       x = 'Years, Relative to Onset')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16)) 
ggsave(filename = "./visuals/client_dif.jpg",
       width = 10,
       height = 6,
       units = 'in')


####dif in dif - polarization
#set level that counts as being "treated" with polarization
polarization_threshold <- as.numeric(quantile(vdem_con$v2cacamps, probs = treatment_threshold, na.rm = TRUE))

#label years relative to onset of polarization treatment 
#only count as treatment if happens while inside a democratic spell,
# and if first year of spell did not see high polarization
#but year count extends ten years prior and after the treatment, regardless of democratic status in those years
vdem$year_rel_polar <- as.numeric(NA)
vdem$polar_control_poly <- as.numeric(NA)
for (i in seq_along(vdem$year)){
  #skip country-years that are not in democratic spells or lack score
  if (is.na(vdem$dem_spell_name[i])) next
  if (is.na(vdem$v2cacamps[i])) next
  
  #find earliest year within each dem spell that crosses treatment threshold. 
  polar_year_zero <- vdem %>% filter(dem_spell_name == vdem$dem_spell_name[i]) %>%
    filter(v2cacamps >= polarization_threshold) %>%
    summarize(polar_year_zero = min(year, na.rm = TRUE)) %>%
    pull(polar_year_zero)
  
  #skip country-years that are in democratic spells that began with high score
  if (vdem$year[!is.na(vdem$dem_spell_name) &
                vdem$dem_spell_name == vdem$dem_spell_name[i] &
                !is.na(vdem$dem_spell_running) &
                vdem$dem_spell_running == 0] == polar_year_zero) next
  
  #renumber infinite values as nulls
  #value is Inf if spell never crossed threshold
  polar_year_zero <- if_else((polar_year_zero == Inf | polar_year_zero == -Inf), 
                             as.numeric(NA),
                             polar_year_zero)
  
  if (is.na(polar_year_zero)) next
  
  #label year of onset as relative year zero
  vdem$year_rel_polar[i] <- if_else(vdem$year[i] == polar_year_zero,
                                    0,
                                    as.numeric(NA))
}
for (i in seq_along(vdem$year)){
  #locate treatment onset within 10 years of each country-year
  polar_year_zero <- vdem %>% 
    filter(country_name == vdem$country_name[i] &
             year >= (vdem$year[i] - 10) &
             year <= (vdem$year[i] + 10) &
             year_rel_polar == 0) %>%
    summarize(polar_year_zero = min(year)) %>%
    pull(polar_year_zero)
  
  #renumber infinite values as nulls
  #value is Inf if spell never crossed threshold
  polar_year_zero <- if_else((polar_year_zero == Inf | polar_year_zero == -Inf), 
                             as.numeric(NA),
                             polar_year_zero)
  
  if (is.na(polar_year_zero)) next
  
  #label all preceding and following years in spell, relative to that zero  
  vdem$year_rel_polar[i] = vdem$year[i] - polar_year_zero
  
  #log control value of polarization among all consolidated democracies in same absolute year
  vdem$polar_control_poly[i] <- if_else(is.na(vdem$year_rel_polar[i]),
                                        as.numeric(NA),
                                        mean(vdem$v2x_polyarchy[vdem$v2x_polyarchy >= dem_threshold & 
                                                                  vdem$year == vdem$year[i] &
                                                                  vdem$country_name != vdem$country_name[i]],
                                             na.rm = TRUE))
}
summary(vdem$year_rel_polar)
summary(vdem$polar_control_poly)


vdem %>%
  filter(!is.na(year_rel_polar)) %>%
  group_by(year_rel_polar) %>%
  summarize(polyarchy_polar = mean(v2x_polyarchy, na.rm = TRUE),
            polyarchy_control = mean(polar_control_poly, na.rm = TRUE)) %>%
  ggplot(aes(x = year_rel_polar))+
  geom_point(aes(y = polyarchy_polar), color = polar_color, size = 4)+
  geom_smooth(data = . %>% filter(year_rel_polar < 0),
              method = 'loess',
              aes(y = polyarchy_polar), color = polar_color, size = 2.5, fill = polar_color)+
  geom_smooth(data = . %>% filter(year_rel_polar > 0),
              method = 'loess',
              aes(y = polyarchy_polar), color = polar_color, size = 2.5, fill = polar_color)+
  geom_point(aes(y = polyarchy_control), color = dem_color, size = 4)+
  geom_smooth(data = . %>% filter(year_rel_polar < 0),
              method = 'loess',
              aes(y = polyarchy_control), color = dem_color, size = 2.5, fill = dem_color)+
  geom_smooth(data = . %>% filter(year_rel_polar > 0),
              method = 'loess',
              aes(y = polyarchy_control), color = dem_color, size = 2.5, fill = dem_color)+
  coord_cartesian(xlim = c(-10,10), ylim = c(0,1))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  annotate('text', label = 'Democracies with\n Polarization', x = 5, y = 0.375, color = polar_color, size = 8)+
  annotate('text', label = 'Other Democracies', x = 5, y = 0.875, color = dem_color, size = 8)+
  theme_minimal()+
  labs(title = 'Onset of Polarization',
       subtitle = '  effect on democracy',
       y = 'V-Dem Polyarchy Score',
       x = 'Years, Relative to Onset')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16)) 
ggsave(filename = "./visuals/polar_dif.jpg",
       width = 10,
       height = 6,
       units = 'in')

####dif in dif - party disinfo
#set level that counts as being "treated" with par_disinfo
par_disinfo_threshold <- as.numeric(quantile(vdem_con$v2smpardom, probs = treatment_threshold, na.rm = TRUE))

#label years relative to onset of party disinfo treatment 
#only count as treatment if happens while inside a democratic spell,
# and if first year of spell did not see high par_disinfo
#but year count extends ten years prior and after the treatment, regardless of democratic status in those years
vdem$year_rel_par_disinfo <- as.numeric(NA)
vdem$par_disinfo_control_poly <- as.numeric(NA)
for (i in seq_along(vdem$year)){
  #skip country-years that are not in democratic spells or lack score
  if (is.na(vdem$dem_spell_name[i])) next
  if (is.na(vdem$v2smpardom[i])) next
  
  #find earliest year within each dem spell that crosses treatment threshold. 
  par_disinfo_year_zero <- vdem %>% filter(dem_spell_name == vdem$dem_spell_name[i]) %>%
    filter(v2smpardom >= par_disinfo_threshold) %>%
    summarize(par_disinfo_year_zero = min(year, na.rm = TRUE)) %>%
    pull(par_disinfo_year_zero)
  
  #skip country-years that are in democratic spells that began with high score
  if (vdem$year[!is.na(vdem$dem_spell_name) &
                vdem$dem_spell_name == vdem$dem_spell_name[i] &
                !is.na(vdem$dem_spell_running) &
                vdem$dem_spell_running == 0] == par_disinfo_year_zero) next
  
  #renumber infinite values as nulls
  #value is Inf if spell never crossed threshold
  par_disinfo_year_zero <- if_else((par_disinfo_year_zero == Inf | par_disinfo_year_zero == -Inf), 
                                   as.numeric(NA),
                                   par_disinfo_year_zero)
  
  if (is.na(par_disinfo_year_zero)) next
  
  #label year of onset as relative year zero
  vdem$year_rel_par_disinfo[i] <- if_else(vdem$year[i] == par_disinfo_year_zero,
                                          0,
                                          as.numeric(NA))
}
for (i in seq_along(vdem$year)){
  #locate treatment onset within 10 years of each country-year
  par_disinfo_year_zero <- vdem %>% 
    filter(country_name == vdem$country_name[i] &
             year >= (vdem$year[i] - 10) &
             year <= (vdem$year[i] + 10) &
             year_rel_par_disinfo == 0) %>%
    summarize(par_disinfo_year_zero = min(year)) %>%
    pull(par_disinfo_year_zero)
  
  #renumber infinite values as nulls
  #value is Inf if spell never crossed threshold
  par_disinfo_year_zero <- if_else((par_disinfo_year_zero == Inf | par_disinfo_year_zero == -Inf), 
                                   as.numeric(NA),
                                   par_disinfo_year_zero)
  
  if (is.na(par_disinfo_year_zero)) next
  
  #label all preceding and following years in spell, relative to that zero  
  vdem$year_rel_par_disinfo[i] = vdem$year[i] - par_disinfo_year_zero
  
  #log control value of par_disinfo among all consolidated democracies in same absolute year
  vdem$par_disinfo_control_poly[i] <- if_else(is.na(vdem$year_rel_par_disinfo[i]),
                                              as.numeric(NA),
                                              mean(vdem$v2x_polyarchy[vdem$v2x_polyarchy >= dem_threshold & 
                                                                        vdem$year == vdem$year[i] &
                                                                        vdem$country_name != vdem$country_name[i]],
                                                   na.rm = TRUE))
}
summary(vdem$year_rel_par_disinfo)
summary(vdem$par_disinfo_control_poly)

vdem %>%
  filter(!is.na(year_rel_par_disinfo)) %>%
  group_by(year_rel_par_disinfo) %>%
  summarize(polyarchy_par_disinfo = mean(v2x_polyarchy, na.rm = TRUE),
            polyarchy_control = mean(par_disinfo_control_poly, na.rm = TRUE)) %>%
  ggplot(aes(x = year_rel_par_disinfo))+
  geom_point(aes(y = polyarchy_par_disinfo), color = disinfo_color, size = 4)+
  geom_smooth(data = . %>% filter(year_rel_par_disinfo < 0),
              method = 'loess',
              aes(y = polyarchy_par_disinfo), color = disinfo_color, size = 2.5, fill = disinfo_color)+
  geom_smooth(data = . %>% filter(year_rel_par_disinfo > 0),
              method = 'loess',
              aes(y = polyarchy_par_disinfo), color = disinfo_color, size = 2.5, fill = disinfo_color)+
  geom_point(aes(y = polyarchy_control), color = dem_color, size = 4)+
  geom_smooth(data = . %>% filter(year_rel_par_disinfo < 0),
              method = 'loess',
              aes(y = polyarchy_control), color = dem_color, size = 2.5, fill = dem_color)+
  geom_smooth(data = . %>% filter(year_rel_par_disinfo > 0),
              method = 'loess',
              aes(y = polyarchy_control), color = dem_color, size = 2.5, fill = dem_color)+
  coord_cartesian(xlim = c(-10,10), ylim = c(0,1))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  annotate('text', label = 'Democracies with\n Disinformation', x = 5, y = 0.375, color = disinfo_color, size = 8)+
  annotate('text', label = 'Other Democracies', x = 5, y = 0.875, color = dem_color, size = 8)+
  theme_minimal()+
  labs(title = 'Onset of Disinformation from Parties',
       subtitle = '  effect on democracy',
       y = 'V-Dem Polyarchy Score',
       x = 'Years, Relative to Onset')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16)) 
ggsave(filename = "./visuals/par_disinfo_dif.jpg",
       width = 10,
       height = 6,
       units = 'in')

####dif in dif - online consumption X fractionalization
#set level that counts as being "treated" with frac_inter
frac_inter_threshold <- as.numeric(quantile(vdem_con$smonexXsmmefra, probs = treatment_threshold, na.rm = TRUE))

#label years relative to onset of frac_inter treatment 
#only count as treatment if happens while inside a democratic spell,
# and if first year of spell did not see high frac_inter
#but year count extends ten years prior and after the treatment, regardless of democratic status in those years
vdem$year_rel_frac_inter <- as.numeric(NA)
vdem$frac_inter_control_poly <- as.numeric(NA)
for (i in seq_along(vdem$year)){
  #skip country-years that are not in democratic spells or lack score
  if (is.na(vdem$dem_spell_name[i])) next
  if (is.na(vdem$smonexXsmmefra[i])) next
  
  #find earliest year within each dem spell that crosses treatment threshold. 
  frac_inter_year_zero <- vdem %>% filter(dem_spell_name == vdem$dem_spell_name[i]) %>%
    filter(smonexXsmmefra >= frac_inter_threshold) %>%
    summarize(frac_inter_year_zero = min(year, na.rm = TRUE)) %>%
    pull(frac_inter_year_zero)
  
  #skip country-years that are in democratic spells that began with high score
  if (vdem$year[!is.na(vdem$dem_spell_name) &
                vdem$dem_spell_name == vdem$dem_spell_name[i] &
                !is.na(vdem$dem_spell_running) &
                vdem$dem_spell_running == 0] == frac_inter_year_zero) next
  
  #renumber infinite values as nulls
  #value is Inf if spell never crossed threshold
  frac_inter_year_zero <- if_else((frac_inter_year_zero == Inf | frac_inter_year_zero == -Inf), 
                                  as.numeric(NA),
                                  frac_inter_year_zero)
  
  if (is.na(frac_inter_year_zero)) next
  
  #label year of onset as relative year zero
  vdem$year_rel_frac_inter[i] <- if_else(vdem$year[i] == frac_inter_year_zero,
                                         0,
                                         as.numeric(NA))
}
for (i in seq_along(vdem$year)){
  #locate treatment onset within 10 years of each country-year
  frac_inter_year_zero <- vdem %>% 
    filter(country_name == vdem$country_name[i] &
             year >= (vdem$year[i] - 10) &
             year <= (vdem$year[i] + 10) &
             year_rel_frac_inter == 0) %>%
    summarize(frac_inter_year_zero = min(year)) %>%
    pull(frac_inter_year_zero)
  
  #renumber infinite values as nulls
  #value is Inf if spell never crossed threshold
  frac_inter_year_zero <- if_else((frac_inter_year_zero == Inf | frac_inter_year_zero == -Inf), 
                                  as.numeric(NA),
                                  frac_inter_year_zero)
  
  if (is.na(frac_inter_year_zero)) next
  
  #label all preceding and following years in spell, relative to that zero  
  vdem$year_rel_frac_inter[i] = vdem$year[i] - frac_inter_year_zero
  
  #log control value of frac_inter among all consolidated democracies in same absolute year
  vdem$frac_inter_control_poly[i] <- if_else(is.na(vdem$year_rel_frac_inter[i]),
                                             as.numeric(NA),
                                             mean(vdem$v2x_polyarchy[vdem$v2x_polyarchy >= dem_threshold & 
                                                                       vdem$year == vdem$year[i] &
                                                                       vdem$country_name != vdem$country_name[i]],
                                                  na.rm = TRUE))
}
summary(vdem$year_rel_frac_inter)
summary(vdem$frac_inter_control_poly)

vdem %>%
  filter(!is.na(year_rel_frac_inter)) %>%
  group_by(year_rel_frac_inter) %>%
  summarize(polyarchy_frac_inter = mean(v2x_polyarchy, na.rm = TRUE),
            polyarchy_control = mean(frac_inter_control_poly, na.rm = TRUE)) %>%
  ggplot(aes(x = year_rel_frac_inter))+
  geom_point(aes(y = polyarchy_frac_inter), color = media_color, size = 4)+
  geom_smooth(data = . %>% filter(year_rel_frac_inter < 0),
              method = 'loess',
              aes(y = polyarchy_frac_inter), color = media_color, size = 2.5, fill = media_color)+
  geom_smooth(data = . %>% filter(year_rel_frac_inter > 0),
              method = 'loess',
              aes(y = polyarchy_frac_inter), color = media_color, size = 2.5, fill = media_color)+
  geom_point(aes(y = polyarchy_control), color = dem_color, size = 4)+
  geom_smooth(data = . %>% filter(year_rel_frac_inter < 0),
              method = 'loess',
              aes(y = polyarchy_control), color = dem_color, size = 2.5, fill = dem_color)+
  geom_smooth(data = . %>% filter(year_rel_frac_inter > 0),
              method = 'loess',
              aes(y = polyarchy_control), color = dem_color, size = 2.5, fill = dem_color)+
  coord_cartesian(xlim = c(-10,10), ylim = c(0,1))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  annotate('text', label = 'Democracies with\n Online\n Fractionalism', x = 5, y = 0.375, color = media_color, size = 8)+
  annotate('text', label = 'Other Democracies', x = 5, y = 0.875, color = dem_color, size = 8)+
  theme_minimal()+
  labs(title = 'Onset of Online Consumpton and Fractionalism',
       subtitle = '  effect on democracy',
       y = 'V-Dem Polyarchy Score',
       x = 'Years, Relative to Onset')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16)) 
ggsave(filename = "./visuals/media_dif.jpg",
       width = 10,
       height = 6,
       units = 'in')

#charts for case studies ----

vdem %>% filter(year == 1995,
                country_name %in% c('Venezuela', 'Brazil', 'Uruguay', 'Hungary', 'Poland', 'Estonia')) %>%
  select(country_name, e_migdppc)

##generic code for plot
plot_case <- function(country_case){
  max_year <- max(vdem$year, na.rm = TRUE)
  case_labels_df <- data.frame(variable = c('Democracy', 'Clientelism', 'Polarization', 'Disinformation'),
                               last_value = c(vdem$v2x_polyarchy[vdem$year == max_year &
                                                                   vdem$country_name == country_case],
                                              vdem$v2xnp_client[vdem$year == max_year&
                                                                  vdem$country_name == country_case],
                                              vdem$v2cacamps[vdem$year == max_year&
                                                               vdem$country_name == country_case],
                                              vdem$v2smonex[vdem$year == max_year&
                                                              vdem$country_name == country_case] *
                                                vdem$v2smpardom[vdem$year == max_year&
                                                                  vdem$country_name == country_case]),
                               color = c(dem_color, client_color, polar_color, disinfo_color))
  
  ggplot(data = (vdem %>% filter(country_name == country_case &
                                   year >= 1981)),
         aes(x = year))+
    geom_line(aes(y = v2x_polyarchy), color = dem_color, size = 2.5)+
    geom_line(aes(y = v2xnp_client), color = client_color, size = 2.5, linetype = 'dotted')+
    geom_line(aes(y = v2cacamps), color = polar_color, size = 2.5, linetype = 'twodash')+
    geom_line(aes(y = (v2smonex * v2smpardom)), color = disinfo_color, size = 2.5, linetype = 'dashed')+
    coord_cartesian(xlim = c(1980, 2020), ylim = c(0,1))+
    scale_y_continuous(sec.axis = dup_axis(
      breaks = case_labels_df$last_value,
      labels = case_labels_df$variable))+
    theme_minimal()+
    labs(title = paste0(country_case, "'s History"),
         subtitle  = " of Democracy and its Solvents")+
    theme(title = element_text(size = 20, face = 'bold'),
          axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 16),
          axis.text.y.right = element_text(size = 16, color = c(dem_color, client_color, polar_color, disinfo_color))) 
}

##apply to desired cases
plot_case(country_case = 'Venezuela')
ggsave(filename = "./visuals/case_venezuela.jpg",
       width = 10,
       height = 6,
       units = 'in')

plot_case(country_case = 'Brazil')
ggsave(filename = "./visuals/case_brazil.jpg",
       width = 10,
       height = 6,
       units = 'in')

plot_case(country_case = 'Uruguay')
ggsave(filename = "./visuals/case_uruguay.jpg",
       width = 10,
       height = 6,
       units = 'in')

plot_case(country_case = 'Hungary')
ggsave(filename = "./visuals/case_hungary.jpg",
       width = 10,
       height = 6,
       units = 'in')

plot_case(country_case = 'Poland')
ggsave(filename = "./visuals/case_poland.jpg",
       width = 10,
       height = 6,
       units = 'in')

plot_case(country_case = 'Estonia')
ggsave(filename = "./visuals/case_estonia.jpg",
       width = 10,
       height = 6,
       units = 'in')



#notes----
##sharpen interactive charting. now uses vdem_con. Switch to using full vdem but --after recording lagged polyarchy--
 ##filter to only consolidated country-years

##standardize x axis coordinates across charts for survival based on breadth 

##run matching analysis for polarization models

##compare polyarchy scores in relative time before and after each treatment variable reached some critical threshold
 ###treatments are clientelism, consumption X fractionalization, party disinfo, and societal polarization
 ###control is polyarchy in all consolidated democracies still below that treatment threshold in same absolute year

##for all consolidated spells, summarize treatment variables in first year of spell. 
 ###some doomed democracies might have been born flawed rather than suffering onset of treatment (eg Venezuela 1959)
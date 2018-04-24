## 01b_create_working_df.R ----
##
## This creates a dataframe with state/age/year/race death counts combined
## with the population, standard population, division, and state information.
## 
## This one dataframe should be able to generate all the other rates we are
## interested in --- national, division, or state age-standradized and
## age-specific rates as well as population weights and age-adjusted population
## weights for different types of regressions if we want.

## Imports ----
library(tidyverse)
library(narcan)   

## DELETE THIS ----
Sys.setenv(R_CONFIG_ACTIVE = "dev")

## Define parameters ---- 
cfig        <- config::get()
priv_folder <- cfig$private_data
data_folder <- cfig$working_data
year_0      <- cfig$start_year
year_n      <- cfig$end_year

## Load data ----
state_opioid <- readRDS(sprintf("%s/state_opioid_deaths.RDS", priv_folder))

state_pops   <- readRDS(sprintf("%s/pop_est_collapsed_long.RDS", 
                                data_folder)) %>% 
    filter(year %in% year_0:year_n) %>% 
    rename(st_fips = fipst)

## Dataframe of state abbreviation, name, fips, and division mapping ----
st_info <- tibble(abbrev   = state.abb, 
                  division = as.character(state.division), 
                  st_lat   = state.center$y, 
                  st_lon   = state.center$x) %>% 
    ## We have to add DC because it's not a state
    add_row(abbrev = "DC", division = "South Atlantic", 
            st_lat = 38.9072, st_lon = 77.0369) %>% 
    left_join(narcan::st_fips_map) %>% 
    rename(st_fips = fips)

## Remap/filter race_ethnicity column of opioid data ---- 
## Remap race to one that matches population files and subset to just: 
##      total, nonhispanic black, nonhispanic white
state_opioid <- state_opioid %>% 
    mutate(race = case_when(
        race_ethnicity == "all_races_all_hisp" ~ "total", 
        race_ethnicity == "other_all_hisp" ~ "other", 
        race_ethnicity == "white_all_hisp" ~ "white", 
        race_ethnicity == "black_all_hisp" ~ "black", 
        race_ethnicity == "white_nonhispanic" ~ "nhw", 
        race_ethnicity == "black_nonhispanic" ~ "nhb")) %>%
    filter(race %in% c("nhb", "nhw", "total")) %>% 
    select(-race_ethnicity) 

## Munge population data ----
## We want to end up with population estimates for every state from 1999-2016
## by 5-year age group for total, all white, all black, non-hispanic white, 
## and non-hispanic black. We do each population in turn by subsetting, then
## bind at the end.
state_pops <- state_pops %>% 
    group_by(year, st_fips, age)

total_pop <- state_pops %>% 
    summarize(pop = sum(pop_est)) %>% 
    mutate(race = "total") %>% 
    ungroup()

all_white <- state_pops %>% 
    filter(race == "white") %>% 
    group_by(race, add = TRUE) %>% 
    summarize(pop = sum(pop_est)) %>% 
    ungroup()

all_black <- state_pops %>% 
    filter(race == "black") %>% 
    group_by(race, add = TRUE) %>% 
    summarize(pop = sum(pop_est)) %>% 
    ungroup()

nhw_only <- state_pops %>% 
    filter(race == "white", hispanic == 0) %>% 
    summarize(pop = sum(pop_est)) %>% 
    mutate(race = "nhw") %>% 
    ungroup()

nhb_only <-  state_pops %>% 
    filter(race == "black", hispanic == 0) %>% 
    summarize(pop = sum(pop_est)) %>% 
    mutate(race = "nhb") %>% 
    ungroup()

state_pop_est <- bind_rows(total_pop, all_white, all_black, nhw_only, nhb_only)

## Merge in standard populations and state/division info
state_pop_est <- state_pop_est %>% 
    left_join(st_info) %>% 
    narcan::add_std_pop(.)

## Clean up
rm(state_pops, total_pop, all_white, all_black, nhw_only, nhb_only)

## Merge pop and death data ----
## Recategorize age because the join will have NAs, merge the state pops, 
## then replace NA counts with 0, and finally add standard population.
## NOTE: We drop 99 opioid cases in the entire data because they do not
## include age.
##  `state_opioid %>% filter(is.na(age)) %>% summarize(sum(opioid_death))`
## 
## The `vars(one_of())` is (I think) the most robust way to select only
## the flagged columns. Else, we have to specify by column name, which may 
## change as analysis carries on.
all_data <- state_pop_est  %>% 
    filter(race %in% c("total", "nhw", "nhb")) %>% 
    left_join(state_opioid, by = c("year", "st_fips", "age", "race")) %>% 
    mutate(age_cat = categorize_age_5(age)) %>% 
    mutate_at(
        vars(one_of(
            state_opioid %>% 
                select(-year, -st_fips, -age, 
                       -age_cat, -race) %>% 
                names())), 
        funs(ifelse(is.na(.), 0, .))) %>% 
    ungroup()

## Make age-standardized population weight
all_data <- all_data %>% 
    mutate(ps_weight = unit_w * pop / 1000) 

saveRDS(all_data, sprintf("%s/state_working_data.RDS", priv_folder))

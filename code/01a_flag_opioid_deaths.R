## 01a_flag_opioid_deaths.R ----
## 
## Import the processed files, flag the deaths related to opioids. The 
## flag_and_summarize() function takes each year, flags the deaths using 
## narcan, and then subsets the race/ethnicity groups we are focused on. 
## Finally, it combines them at the end. The result is a dataframe with age, 
## state, race/ethnicity, and the number of deaths for that subset and
## a variety of outcomes.

## Imports ----
library(tidyverse)
library(narcan)     ## devtools::install_github("mkiang/narcan")

# Sys.setenv(R_CONFIG_ACTIVE = "dev")

## Pull in YAML config ----
cfig <- config::get()

## Define parameters ---- 
year_0      <- cfig$start_year
year_n      <- cfig$end_year
paral_proc  <- cfig$proc_in_parallel
priv_data   <- cfig$private_data
data_folder <- cfig$working_data
n_cores     <- cfig$num_cores

## Load parallel package if necessary ----
if (paral_proc) library(foreach)

## Helper function ----
flag_and_summarize <- function(priv_data, year) {
    ## Flag opioid deaths
    flagged_df <- readRDS(sprintf("%s/proc_%s.RDS", priv_data, year)) %>% 
        flag_opioid_deaths(year = year) %>% 
        flag_opioid_types(year = year) %>% 
        mutate(st_fips  = substr(countyrs, 1, 2)) %>% 
        select(-ucod, -f_records_all, -countyrs, -month, -countyoc) 
    
    ## All race/ethnicity 
    all_races <- flagged_df %>% 
        select(-race, -race_cat, -hspanicr, -hsp_cat) %>% 
        summarize_binary_columns(st_fips, age, age_cat) %>% 
        mutate(race_ethnicity = "all_races_all_hisp") %>% 
        select(year, st_fips, age, age_cat, race_ethnicity, everything())
    
    ## Black and white, all hispanic origin
    white_black_all <- flagged_df %>% 
        select(-race, -hspanicr, -hsp_cat) %>% 
        filter(race_cat %in% c("white", "black")) %>% 
        summarize_binary_columns(st_fips, age, age_cat, race_cat) %>% 
        mutate(race_ethnicity = ifelse(race_cat == "white", 
                                       "white_all_hisp", 
                                       "black_all_hisp")) %>% 
        select(-race_cat) %>% 
        select(year, st_fips, age, age_cat, race_ethnicity, everything()) 
    
    ## All others (regardless of hispanic origin)
    other_all <- flagged_df %>% 
        filter(!(race_cat %in% c("white", "black"))) %>% 
        select(-race, -race_cat, -hspanicr, -hsp_cat) %>% 
        summarize_binary_columns(st_fips, age, age_cat) %>% 
        mutate(race_ethnicity = "other_all_hisp") %>% 
        select(year, st_fips, age, age_cat, race_ethnicity, everything()) 
    
    ## nonhispanic white and nonhispanic black
    white_black_nh <- flagged_df %>% 
        select(-race, -race_cat, -hspanicr) %>% 
        filter(hsp_cat %in% c("nonhispanic_white", "nonhispanic_black")) %>% 
        summarize_binary_columns(st_fips, age, age_cat, hsp_cat) %>% 
        mutate(race_ethnicity = ifelse(hsp_cat == "nonhispanic_white", 
                                       "white_nonhispanic", 
                                       "black_nonhispanic")) %>% 
        select(-hsp_cat) %>% 
        select(year, st_fips, age, age_cat, race_ethnicity, everything())
    
    ## Combine dataframes
    state_opioid_deaths <- 
        rbind(all_races, other_all, white_black_all, white_black_nh) %>% 
        arrange(race_ethnicity, year, st_fips, age) %>% 
        ungroup()
    
    return(state_opioid_deaths)
}


## Run the function ----
if (paral_proc) {
    doParallel::registerDoParallel(cores = n_cores)
    
    state_opioid_data <- foreach(year = year_0:year_n, 
                                  .combine=rbind, .inorder = FALSE) %dopar% 
        (flag_and_summarize(priv_data = priv_data, year = year))
} else {
    state_opioid_data <- NULL
    for (year in year_0:year_n) {
        temp_df <- flag_and_summarize(priv_data = priv_data, year = year)
        state_opioid_data <- rbind(state_opioid_data, temp_df)
        
        rm(temp_df); gc()
    }
}

## Save raw flagged opioid data ----
saveRDS(state_opioid_data, 
        file = sprintf('%s/state_opioid_deaths.RDS', priv_data))

## 00a_process_raw_data_to_working_data.R ----
## 
## Just takes in the restricted MCOD files, subsets to the columns we want, 
## cleans them up using the `narcan` package, and saves the intermediate file
## as working data.
## 
## Note, we keep the (unaggregated) data in the private data folder so it 
## does not accidentally get uploaded to an online repo.

## Imports ----
library(tidyverse)
library(narcan) 

# Sys.setenv(R_CONFIG_ACTIVE = "dev")

## Pull in YAML config ----
cfig <- config::get()

## Define parameters ---- 
year_0     <- cfig$start_year       # First year of restricted data
year_n     <- cfig$end_year         # Last year of restricted data
paral_proc <- cfig$proc_in_parallel # Parallel process?
priv_data  <- cfig$private_data     # Where to keep private data
rmcod_path <- cfig$mcod_files       # Path to restricted files
force_proc <- cfig$force_process    # Overwrite existing files
n_cores    <- cfig$num_cores        # Number of cores to use in parallel

## Load parallel package if necessary ----
if (paral_proc) library(foreach)

## Helper function for parallel processing ----
convert_raw_data <- function(priv_data, rmcod_path, year) {
    ## Don't run if file already exist and `force` is not TRUE
    if (file.exists(sprintf("%s/proc_%s.RDS", priv_data, year)) &&
        !force_proc) {
        next
    } else {
        ## Load the data
        temp_df <- readRDS(sprintf("%s/mcod%s_clean.RDS", rmcod_path, year))
        
        ## Subset to columns we want
        temp_df <- temp_df %>% 
            select(one_of(c("race", "hspanicr", "ager27", "restatus", 
                           "ucod", "monthdth")), 
                   starts_with("record_"), 
                   one_of(c("countyrs", "countyoc", "fipsctyo", "fipsctyr")))
        
        ## Fix 1999-2002 FIPS/geo columns
        ## NOTE: 1999-2002, countyrs and countyoc were weird NCHS-specific
        ##       codes and in 2003+, they switched to FIPS but kept the 
        ##       variable names.
        if (year <= 2002) {
            temp_df <- temp_df %>% 
                select(-countyoc, -countyrs) %>% 
                rename(countyoc = fipsctyo, countyrs = fipsctyr)
        }
        
        ## Munging -- add year, subset to residents, better contributory 
        ## cause column, better age categories, temporally consistent race
        ## variable, and better categorized variables.
        temp_df <- temp_df %>% 
            mutate(year = year) %>% 
            subset_residents() %>% 
            unite_records() %>% 
            convert_ager27() %>% 
            remap_race() %>% 
            mutate(race_cat = categorize_race(race), 
                   hsp_cat  = categorize_hspanicr(hspanicr), 
                   age_cat  = categorize_age_5(age)) %>% 
            select(year, month = monthdth, race, race_cat, hspanicr, 
                   hsp_cat, age, age_cat, countyoc, countyrs, 
                   ucod, f_records_all)
        
        ## Save as RDS (readr::write_csv() has parsing errors)
        saveRDS(temp_df, sprintf("%s/proc_%s.RDS", priv_data, year))
        
        ## Clean up --- usually unnecessary
        rm(temp_df, processed_df); gc()
        
        ## Return **something** for %dopar%
        return(year)
    }
}


## Perform actual processing ----
narcan::mkdir_p(priv_data)

if (paral_proc) {
    doParallel::registerDoParallel(cores = n_cores)
    
    foreach(year = year_0:year_n, .packages = c("tidyverse", "narcan"), 
            .combine = c, .inorder = FALSE) %dopar% 
        (convert_raw_data(priv_data, rmcod_path, year))
} else {
    for (year in year_0:year_n) {
        convert_raw_data(priv_data, rmcod_path, year)
    }
}

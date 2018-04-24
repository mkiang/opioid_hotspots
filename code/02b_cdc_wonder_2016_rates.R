## 02b_cdc_wonder_2016_rates.R ----
## 
## We don't yet have the 2016 restricted access files so for now, we'll just 
## use the CDC WONDER file for 2016. This results in a lot of models ending
## in 2015 due to data suppression, but most of the white rates will have an
## extra year and it should make it easier to transition the code once we get
## approved for the 2016 file.

## Imports ----
library(tidyverse)

## Pull in YAML config ----
cfig <- config::get()
data_folder <- cfig$working_data

## Helper ----
read_cdc <- function(f_path) {
    read_delim(f_path,  delim = "\t", escape_double = FALSE, 
               trim_ws = TRUE, na = c("Suppressed", "Unreliable", "", "."))
}

## Define a dict of the CDC file paths to the type of opioid ----
opioid_dict <- list(synth_opioid = "cdc_wonder_pull_synth.txt", 
                    opioid = "cdc_wonder_pull_all_opioids.txt", 
                    natural_opioid = "cdc_wonder_pull_natural.txt", 
                    heroin = "cdc_wonder_pull_heroin.txt",
                    synth_opioid_total = "cdc_wonder_pull_total_synth.txt", 
                    opioid_total = "cdc_wonder_pull_total_all_opioids.txt", 
                    natural_opioid_total = "cdc_wonder_pull_total_natural.txt", 
                    heroin_total = "cdc_wonder_pull_total_heroin.txt"
                    )

## Loop through each file and munge it to match our files ----
## NOTE: You get parsing errors but this is just because the CDC files have 
## extra notes at the end of the file.
holder <- NULL
for (x in names(opioid_dict)) {
    ## Read in raw file
    temp_df <- read_cdc(sprintf("./%s/cdc_wonder/%s",
                                data_folder, opioid_dict[[x]]))
    
    ## Subset to 2016 and remove missing states
    temp_df <- temp_df %>%
        filter(!is.na(State), 
               Year == 2016)
    
    ## Is this by race or total population?
    if (grepl("_total", x, fixed = TRUE)) {
        ## If total population, assign race as total and make opioid type
        o_type <- gsub("_total", "", x)
        
        temp_df <- temp_df %>%
            mutate(race = "total", 
                   opioid_type = o_type) 
    } else {
        ## Make matching race, opioid_type
        temp_df <- temp_df %>% 
            mutate(race = 
                       case_when(
                           Race == "Black or African American" ~ "nhb", 
                           Race == "White" ~ "nhw", 
                           TRUE ~ Race
                       ), 
                   opioid_type = x)
    }
    
    ## Rename and subset
    temp_df <- temp_df %>% 
        select(st_fips = `State Code`, 
               name = State, 
               race, 
               year = Year, 
               opioid_type, 
               rate = `Age Adjusted Rate`, 
               sd   = `Age Adjusted Rate Standard Error`, 
               pop  = Population, 
               count = Deaths)
    
    ## Bind
    holder <- rbind(holder, temp_df)
}

## Need to add two columns to match with restricted data
## Also add state name and abbrev to make joining easier later
holder <- holder %>% 
    ungroup() %>% 
    mutate(ps_weight = NA, var = sd^2) %>% 
    left_join(
        tibble(name = c(state.name, "District of Columbia"), 
               abbrev = c(state.abb, "DC"))
    )

## Save
saveRDS(holder, sprintf("./%s/cdc_wonder_2016.RDS", data_folder))

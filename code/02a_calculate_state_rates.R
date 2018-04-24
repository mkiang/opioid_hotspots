## 02a_calculate_state_rates.R ----
## 
## Calculate age-standardized rates for each year/race/drug/state, 
## using the state-level working dataframe. In addition, this creates a 
## dataframe that contains the total number of deaths for all observations
## with greater than 10 deaths. We can then use this dataframe to suppress
## the private data before public release.

## Imports ----
library(tidyverse)
library(narcan)

# Sys.setenv(R_CONFIG_ACTIVE = "dev")

## Define parameters ---- 
cfig <- config::get()
data_folder <- cfig$private_data
save_folder <- cfig$working_data
year_0 <- cfig$start_year
year_n <- cfig$end_year
z_rate <- cfig$zero_rate

## Load data ----
all_data <- readRDS(sprintf("%s/state_working_data.RDS", data_folder)) %>% 
    ungroup()
    
## Convert counts to age-specific rates ----
age_spec_rates <- all_data %>% 
    calc_asrate_var(opioid, opioid_death) %>% 
    calc_asrate_var(opium, opium_present) %>%
    calc_asrate_var(heroin, heroin_present) %>%
    calc_asrate_var(natural_opioid, other_natural_present) %>%
    calc_asrate_var(methadone, methadone_present) %>%
    calc_asrate_var(synth_opioid, other_synth_present) %>%
    calc_asrate_var(other_opioid, other_op_present) %>%
    calc_asrate_var(unspecified_opioid, unspecified_op_present) %>%
    mutate_at(vars(ends_with("_var")), 
              funs(ifelse(is.nan(.), 0, .))) %>% 
    ungroup()

## Get total deaths for every state/race/year/opioid_type ----
total_deaths <- age_spec_rates %>% 
    group_by(year, race, st_fips, abbrev) %>% 
    summarize_at(vars(ends_with("_death"), ends_with("_present"), pop), 
                 sum, na.rm = TRUE) %>% 
    mutate_all(function(x) ifelse(x < 10, NA, x)) %>% 
    ungroup()

saveRDS(total_deaths, sprintf("%s/total_deaths_suppressed.RDS", 
                              save_folder))

## Calculate age-standardized rates ---- 
std_death_rates <- age_spec_rates %>% 
    calc_stdrate_var(opioid_rate, opioid_var, 
                     race, year, st_fips, abbrev, name) %>% 
    left_join(age_spec_rates %>% 
                  calc_stdrate_var(natural_opioid_rate, natural_opioid_var, 
                                   race, year, st_fips, abbrev, name)) %>% 
    left_join(age_spec_rates %>% 
                  calc_stdrate_var(heroin_rate, heroin_var, 
                                   race, year, st_fips, abbrev, name)) %>% 
    left_join(age_spec_rates %>% 
                  calc_stdrate_var(synth_opioid_rate, synth_opioid_var, 
                                   race, year, st_fips, abbrev, name)) %>% 
    ungroup()

## Fix NaNs ----
## Note that for some fips/race/year combinations, there are literally zero
## popuations. This results in a NaN rate, but should be a zero rate.
std_death_rates <- std_death_rates %>% 
    mutate_at(vars(ends_with("_rate")), 
              funs(ifelse(is.nan(.) | . == 0, z_rate, .)))

saveRDS(std_death_rates, 
        file = sprintf('%s/state_age_std_rates.RDS', 
                       data_folder))

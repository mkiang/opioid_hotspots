## 02c_reshape_for_joinpoint.R ----
## 
## Takes the rates we calculated and reshapes them into a format the 
## joinpoint regression program will accept.

## Imports ----
library(tidyverse)

## Define parameters ---- 
cfig <- config::get()
priv_folder <- cfig$private_data
data_folder <- cfig$working_data

## Get rates ----
std_rates_wide <- readRDS(
    sprintf("./%s/state_age_std_rates.RDS", priv_folder)
    )
cdc_wonder     <- readRDS(
    sprintf("./%s/cdc_wonder_2016.RDS", data_folder)
    ) %>% 
    filter(race != "total")

## Get counts ----
death_counts <- readRDS(
    sprintf('./data/total_deaths_suppressed.RDS', data_folder)
    )

## Convert to long ----
std_rates_long <- left_join(
    std_rates_wide %>% 
        select(st_fips, abbrev, name, race, year, ends_with("_rate")) %>% 
        gather(opioid_type, rate, ends_with("rate")) %>% 
        mutate(opioid_type = gsub("_rate", "", fixed = TRUE, opioid_type)), 
    std_rates_wide %>% 
        select(st_fips, abbrev, name, race, year, ends_with("_var")) %>% 
        gather(opioid_type, var, ends_with("var")) %>% 
        mutate(opioid_type = gsub("_var", "", fixed = TRUE, opioid_type))
) %>% 
    ungroup()

std_rates_long <- std_rates_long %>% 
    left_join(death_counts %>% 
                  select(st_fips, race, year, opioid = opioid_death, 
                         natural_opioid = other_natural_present, 
                         heroin = heroin_present,
                         synth_opioid = other_synth_present, 
                         abbrev, pop) %>% 
                  gather(opioid_type, count, opioid:synth_opioid)
    )

## Remove rates where pop is small and deaths < 10.
std_rates_long <- std_rates_long %>% 
    mutate(rate = case_when(
        pop < 100000 & is.na(count) ~ NA_real_, 
        TRUE ~ rate)
    )

## Arrange and filter for JP
## We remove NA rates and also replace 0 observation rates with the specified
## zero rate from the yaml. We replace missing variance values with the 
## minimum variance observed for that race/state/opioid type.
opioids_by_type <- std_rates_long %>% 
    ungroup() %>% 
    filter(!is.na(rate)) %>% 
    mutate(var  = ifelse(var == 0, NA, var), 
           rate = ifelse(rate == config::get()$zero_rate, 
                         .01, rate)) %>% 
    group_by(st_fips, opioid_type, race) %>% 
    mutate(var = ifelse(is.na(var), min(var, na.rm = TRUE), var), 
           sd  = sqrt(var)) %>% 
    ungroup() %>% 
    arrange(race, opioid_type, st_fips, year) %>% 
    bind_rows(cdc_wonder)

## Remove state/race/opioid types where we have fewer than 10 years of obs
opioids_by_type <- opioids_by_type %>% 
    group_by(race, opioid_type, abbrev) %>% 
    filter(n() > 10, is.finite(var))

## Save --- combining restricted with CDC wonder
narcan::mkdir_p("./joinpoint_analyses")
options(scipen = 20)    ## Or else it writes one row using scientific notation
write.csv(opioids_by_type, "./joinpoint_analyses/opioids_by_type_2016.csv", 
          row.names = FALSE)

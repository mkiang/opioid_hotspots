## Imports ----
library(tidyverse)
library(geofacet)

## Config file ----
cfig      <-  config::get()
PVAL      <-cfig$sig_p_value
priv_data <- cfig$private_data
data_dir  <- cfig$working_data

## Helper functions  ----
### To clean up joinpoint output
import_results_data <- function(filename) {
    df <- readr::read_delim(filename, delim = ";", 
                            escape_double =  FALSE, 
                            trim_ws = TRUE)
    names(df) <- tolower(names(df))
    names(df) <- gsub(" |#|%|", "", names(df))
    names(df) <- gsub(",|-|\\.", "_", names(df))
    
    return(df)
}

### Calculate the mode of a vector 
###     We need this to figure out the level of significance for each
###     line segment when we do the row duplication.
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

## Get the death counts so we can make a public version ----
death_counts <- readRDS(
    sprintf('./data/total_deaths_suppressed.RDS', data_dir)
    ) %>% 
    select(st_fips, race, year, opioid = opioid_death, 
           natural_opioid = other_natural_present, 
           heroin = heroin_present,
           synth_opioid = other_synth_present, 
           abbrev, pop) %>% 
    gather(opioid_type, count, opioid:synth_opioid)


## Import the results dataframe ---- 
## which has the fitted line and the raw data
jp_results <- 
    import_results_data(paste0("./joinpoint_analyses/jp_output_files_2016/", 
                               "opioids_by_type_2016.data.txt")) %>% 
    select(year, abbrev, opioid_type, race, rate, rate_se = standarderror,
           pred_rate = model, everything()) %>% 
    mutate(apc = as.numeric(gsub("\\^", "", apc)))

## Import some columns from the model estimates dataframe ----
## which has the slope and intercepts of every segment
jp_estimates <- 
    import_results_data(
        paste0("./joinpoint_analyses/jp_output_files_2016/", 
               "opioids_by_type_2016.modelestimates.txt")
        ) %>% 
    select(year = joinpoint, abbrev, opioid_type, race, 
           int_est = interceptestimate, int_se = interceptstderror, 
           int_pval = interceptp_value, slope_est = slopeestimate, 
           slope_se = slopestderror, slope_pval = slopep_value, 
           slopechange_est = slopechgestimate, 
           slopechange_se = slopechgstderror, 
           slopechange_pval = slopechgp_value, 
           df, sse, mse, year_lb = joinpoint95lcl, 
           year_ub = joinpoint95ucl, model, segment) %>% 
    mutate(j_year = year, 
           year = ifelse(is.na(year), 1999, year))

## Import the AAPC dataframe ---- 
## which has average APC over the entire period
aapc_df <- import_results_data(
    paste0("./joinpoint_analyses/jp_output_files_2016/", 
           "opioids_by_type_2016.aapc.txt")
    )

aapc_df <- aapc_df %>% 
    rename(sig_aapc  = `statisticallysignificant(0=no1=yes)`, 
           aapc_pval = p_value, 
           aapc_ub   = aapcc_i_high, 
           aapc_lb   = aapcc_i_low) %>% 
    mutate(aapc_sig = ifelse(aapc_pval < PVAL, aapc, NA),
           aapc_cat = factor(sig_aapc, 
                             levels = 0:1, 
                             labels = c("Non-significant", "Significant"), 
                             ordered = TRUE)) %>% 
    filter(opioid_type %in% c("opioid", "heroin", 
                              "natural_opioid", "synth_opioid")) %>% 
    select(-joinpointmodel, -aapcindex, -startobs, -endobs, -teststatistic)

## Join them together ----
## and then use last obs carried forward
jp_results <- jp_results %>% 
    left_join(jp_estimates) %>% 
    left_join(aapc_df) %>% 
    mutate_at(vars(starts_with("int_"), starts_with("slope_"), model, segment), 
              zoo::na.locf)

jp_results <- jp_results %>% 
    filter(opioid_type %in% c("opioid", "natural_opioid", 
                              "heroin", "synth_opioid")) %>% 
    mutate(opioid_cat = factor(opioid_type, 
                               levels = c("opioid", "natural_opioid", 
                                          "heroin", "synth_opioid"), 
                               labels = c("All opioids", "Natural", 
                                          "Heroin", "Synthetic"), 
                               ordered = TRUE), 
           race_cat = factor(race, 
                             levels = c("nhw", "nhb", "total"), 
                             labels = c("Non-Hispanic White", 
                                        "Non-Hispanic Black", 
                                        "Total"), 
                             ordered = TRUE))

## Indicators for significant slope and changepoints ----
jp_results <- jp_results %>% 
    mutate(slope_sig    = (slope_pval < PVAL) + 0, 
           slopechg_sig = (slopechange_pval < PVAL) + 0) %>% 
    mutate(slopechg_sig = factor(slopechg_sig, 
                                 levels = 0:1, 
                                 labels = c(
                                     sprintf("NS (P>%0.2f)", PVAL), 
                                     sprintf("Sig. (P<%0.2f)", PVAL)
                                     ), 
                                 ordered = TRUE), 
           slope_sig = factor(slope_sig, 
                              levels = 0:1, 
                              labels = c(
                                  sprintf("NS (P>%0.2f)", PVAL), 
                                  sprintf("Sig. (P<%0.2f)", PVAL)
                              ), 
                              ordered = TRUE)
    )

## Create a line segment grouping for every regression line ----
jp_results <- jp_results %>% 
    group_by(opioid_type, abbrev, race, int_est, 
             int_se, slope_est, slope_pval) %>% 
    mutate(line_seg = case_when(year == min(year) ~ 1, TRUE ~ 0)) %>% 
    group_by(opioid_type, race, abbrev) %>% 
    mutate(line_seg = cumsum(line_seg)) %>% 
    ungroup()

## Double observations for sig-non-sig transition ----
## We need to duplicate rows where there's a transition between 
## significant and non-significant segments or else the lines get weird.
## 
## So we take the dataframe, we find all models with more than one line 
## segment, take the first observation from the line segments (after the 
## first one), duplicate that row and assign it the line segment before it.
## 
## Then we need to make sure that the duplicate row switches significance
## to match the rest of the line segment when necessary.
## 
## Add a `dupe_row` column so it is easier to filter out the rows when we 
## don't need them.
jp_results <- bind_rows(
    jp_results %>% 
        mutate(dupe_row = 0), 
    jp_results %>% 
        filter(line_seg > 1) %>% 
        group_by(abbrev, opioid_type, race, line_seg) %>% 
        filter(year == min(year)) %>% 
        ungroup() %>%  
        mutate(line_seg = line_seg - 1, 
               dupe_row = 1)
    ) %>% 
    arrange(abbrev, race, opioid_type, year, line_seg) %>% 
    group_by(abbrev, race, opioid_type, line_seg) %>% 
    mutate(slope_sig = getmode(slope_sig), 
           slope_pval = getmode(slope_pval)) %>% 
    ungroup()

## Save it ----
saveRDS(jp_results, 
        sprintf('./%s/joinpoint_results_dupe_rows_2016.RDS', priv_data))

## Make a version we can share by removing rates with < 10 deaths
## and also the SSE and MSE of the models that used those rates.
jp_results <- jp_results %>% 
    left_join(death_counts) %>% 
    mutate(
        count   = ifelse(year == 2016, 0, count), 
        rate    = ifelse(is.na(count), NA, rate), 
        rate_se = ifelse(is.na(count), NA, rate_se)
        )

jp_results <- jp_results %>% 
    group_by(race, opioid_type, abbrev) %>% 
    mutate(
        sse = ifelse(any(is.na(count)), NA, sse), 
        mse = ifelse(any(is.na(count)), NA, mse)
    ) %>% 
  ungroup() 

saveRDS(jp_results, 
        sprintf('./%s/joinpoint_results_dupe_rows_2016.RDS', data_dir))

## Read in the AAPC results from the joinpoint and make a public version 
## of just those

## Imports
library(tidyverse)

## Pull yaml ----
cfig        <-  config::get()
data_folder <- cfig$working_data

## Helpers
import_results_data <- function(filename) {
    df <- readr::read_delim(filename, delim = ";", 
                            escape_double =  FALSE, 
                            trim_ws = TRUE, col_types = cols())
    names(df) <- tolower(names(df))
    names(df) <- gsub(" |#|%|", "", names(df))
    names(df) <- gsub(",|-|\\.", "_", names(df))
    
    return(df)
}

## Import AAPC data
aapc_df <- import_results_data(
    paste0("./joinpoint_analyses/", 
           "jp_output_files_2016/", 
           "opioids_by_type_2016.aapc.txt"))

aapc_df <- aapc_df %>% 
    rename(sig_aapc = `statisticallysignificant(0=no1=yes)`) %>% 
    mutate(race_cat = factor(race, 
                             levels = c("total", "nhw", "nhb"), 
                             labels = c("Total", "Non-Hispanic White", 
                                        "Non-Hispanic Black"), 
                             ordered = TRUE), 
           opioid_cat = factor(opioid_type, 
                               levels = 
                                   c("opioid", "natural_opioid", 
                                     "heroin", "synth_opioid"), 
                               labels = c("All opioids", "Natural", 
                                          "Heroin", "Synthetic"), 
                               ordered = TRUE)) %>% 
    filter(opioid_type %in% c("opioid", "heroin", 
                              "natural_opioid", "synth_opioid")) %>% 
    select(opioid_cat, race_cat, abbrev, startobs, 
           endobs, aapc, aapcc_i_low, aapcc_i_high, p_value) %>% 
    arrange(opioid_cat, race_cat, abbrev)

saveRDS(aapc_df, sprintf('%s/aapc_results.RDS', data_folder))

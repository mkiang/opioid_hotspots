## Imports ----
library(tidyverse)
source('./code/mk_nytimes.R')

## Pull in YAML config ----
cfig <- config::get()
data_folder <- cfig$working_data
plot_folder <- cfig$plot_dir

## Make a file dictionary of opioid_type to file path
file_dict <- list(
    synth_opioid      = "cdc_wonder_pull_natl_synth.txt", 
    opioid            = "cdc_wonder_pull_natl_all_opioids.txt", 
    natural_opioid    = "cdc_wonder_pull_natl_natural.txt", 
    heroin            = "cdc_wonder_pull_natl_heroin.txt"
)

## Loop and import
holder <- NULL
for (x in names(file_dict)) {
    print(x)
    temp_df <- read_delim(sprintf("./%s/cdc_wonder/%s", 
                                  data_folder, file_dict[[x]]),  
                          delim = "\t", 
                          escape_double = FALSE, 
                          trim_ws = TRUE, 
                          na = c("Suppressed", "Unreliable", "", ".")) %>% 
        filter(!is.na(Race), 
               `Hispanic Origin` == "Not Hispanic or Latino") %>% 
        select(one_of("Year", "Race", "Deaths", "Age Adjusted Rate", 
                      "Age Adjusted Rate Standard Error"))
    
    temp_df <- temp_df %>% 
        mutate(
            opioid_type = x
        ) %>% 
        rename(year     = Year, 
               race = Race, 
               deaths   = Deaths, 
               std_rate = `Age Adjusted Rate`, 
               rate_se  = `Age Adjusted Rate Standard Error`)
    
    holder <- rbind(holder, temp_df)
}

## Better labels for the plot
holder <- holder %>% 
    mutate(
        opioid_cat = factor(opioid_type, 
                            levels = c("opioid", "natural_opioid", 
                                       "heroin", "synth_opioid"), 
                            labels = c("All opioids", "Natural", 
                                       "Heroin", "Synthetic"), 
                            ordered = TRUE), 
        race = case_when(
            race == "Black or African American" ~ "nhb", 
            race == "White" ~ "nhw", 
            TRUE ~ NA_character_
        ), 
        race_cat = factor(race, 
                          levels = c("nhw", "nhb"), 
                          labels = c("Non-Hispanic White", 
                                     "Non-Hispanic Black"), 
                          ordered = TRUE)
    )

## Plot it
p1 <- ggplot() + 
    geom_line(data = holder %>% 
                  filter(opioid_type != "opioid"), 
              aes(x = year, y = std_rate, linetype = race_cat, 
                  group = race_cat, color = opioid_cat), 
              alpha = 1, size = .8) + 
    scale_color_brewer(palette = "Set1") + 
    scale_linetype_manual(values = c("solid", "dashed")) + 
    scale_x_continuous(NULL, expand = c(0, 0), 
                       breaks = seq(2000, 2015, 5), 
                       labels = c("'00", "'05", "'10", "'15")) + 
    scale_y_continuous("Age-adjusted mortality rate (per 100,000)", 
                       expand = c(0, .5)) + 
    facet_wrap(~ opioid_cat) + 
    mk_nytimes(panel.border = element_rect(linetype = "solid", 
                                           fill = NA, 
                                           color = "grey75"), 
               legend.position = "none")

## Save
saveRDS(p1, "./plots/grobs/presentation_fig5.RDS")

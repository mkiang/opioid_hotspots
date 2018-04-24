## Imports ----
library(tidyverse)
source('./code/mk_nytimes.R')

## Pull in YAML config ----
cfig <- config::get()
data_folder <- cfig$working_data
plot_folder <- cfig$plot_dir

## File dictionary ----
file_dict <- list(
    synth_opioid      = "cdc_wonder_pull_total_synth.txt", 
    opioid            = "cdc_wonder_pull_total_all_opioids.txt", 
    natural_opioid    = "cdc_wonder_pull_total_natural.txt", 
    heroin            = "cdc_wonder_pull_total_heroin.txt",
    synth_opioid_us   = "cdc_wonder_pull_total_natl_synth.txt", 
    opioid_us         = "cdc_wonder_pull_total_natl_all_opioids.txt", 
    natural_opioid_us = "cdc_wonder_pull_total_natl_natural.txt", 
    heroin_us         = "cdc_wonder_pull_total_natl_heroin.txt"
)

## Loop and import ----
holder <- NULL
for (x in names(file_dict)) {
    print(x)
    temp_df <- read_delim(sprintf("./%s/cdc_wonder/%s", 
                                  data_folder, file_dict[[x]]),  
                          delim = "\t", 
                          escape_double = FALSE, 
                          trim_ws = TRUE, 
                          na = c("Suppressed", "Unreliable", "", ".")) %>% 
        select(one_of("State", "Year", "Deaths", "Age Adjusted Rate", 
                      "Age Adjusted Rate Standard Error"))
    
    if (grepl("_us", x)) {
        temp_df <- temp_df %>% 
            mutate(
                State = "US"
            )
        x <- gsub("_us", "", x)
    }
    
    temp_df <- temp_df %>% 
        filter(!is.na(State)) %>% 
        mutate(
            opioid_type = x
        ) %>% 
        rename(state    = State, 
               year     = Year, 
               deaths   = Deaths, 
               std_rate = `Age Adjusted Rate`, 
               rate_se  = `Age Adjusted Rate Standard Error`)
    
    holder <- rbind(holder, temp_df)
}

## Better categories 
holder <- holder %>% 
    mutate(
        opioid_cat = factor(opioid_type, 
                            levels = c("opioid", "natural_opioid", 
                                       "heroin", "synth_opioid"), 
                            labels = c("All opioids", "Natural", 
                                       "Heroin", "Synthetic"), 
                            ordered = TRUE
        )
    )

## Plot
p1 <- ggplot() + 
    geom_line(data = holder %>% 
                  filter(opioid_type != "opioid", 
                         state != "US"), 
              aes(x = year, y = std_rate, 
                  group = state, color = opioid_cat), 
              alpha = .5) + 
    geom_line(data = holder %>% 
                  filter(opioid_type != "opioid", 
                         state == "US"), 
              aes(x = year, y = std_rate), 
              color = "black", alpha = .5, 
              size = 1.2) + 
    scale_color_brewer(palette = "Set1") + 
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
saveRDS(p1, sprintf('./%s/grobs/presentation_fig6.RDS', plot_folder))

ggsave(sprintf('./%s/fig_state_variation.pdf', plot_folder), 
       p1, width = 6, height = 3.5, scale = 1.5, device = cairo_pdf)

ggsave(sprintf('./%s/fig_state_variation.png', plot_folder), 
       p1, width = 6, height = 3.5, scale = 1.5, dpi = 300)

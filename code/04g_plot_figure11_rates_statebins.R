
## Imports 
library(tidyverse)
library(viridis)
library(statebins)
source("./code/mk_nytimes.R")

## Pull yaml ----
cfig        <- config::get()
plot_folder <- cfig$plot_dir
data_folder <- cfig$working_data

## Data ----
est_df <- readRDS(sprintf("%s/joinpoint_results_dupe_rows_2016.RDS", 
                          data_folder)) %>% 
    filter(dupe_row == 0)

final_rates <- est_df %>% 
    group_by(abbrev, race, opioid_type) %>% 
    filter(year == max(year)) %>% 
    mutate(rate_cat = cut(rate, 
                          breaks = c(0, 2.5, 5, 7.5, 10, 15, 20, 25, Inf), 
                          include.lowest = TRUE, 
                          ordered_result = TRUE)
    ) %>% 
    ungroup()

rate_range <- range(final_rates$rate, na.rm = TRUE)
n_cats <- n_distinct(final_rates$rate_cat)
levels(final_rates$rate_cat)[n_cats] <- 
    sprintf("(25,%i]", round(rate_range[2]))

p1 <- ggplot(final_rates, 
             aes(state = abbrev, fill = rate_cat)) +
    scale_fill_viridis(option = "magma", 
                       name = "Mortality (per 100,000)", direction = -1, 
                       discrete = TRUE, na.value = "grey50") + 
    geom_statebins() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    facet_grid(opioid_cat ~ race_cat) +
    mk_nytimes(legend.position = "bottom", 
               panel.grid.major = element_blank(), 
               axis.text = element_blank(), 
               panel.border = element_rect(linetype = "solid", 
                                           fill = NA, 
                                           color = "grey75")) + 
    guides(fill = 
               guide_legend(
                   title.position = "left", 
                   keywidth = 2.5, 
                   keyheight = .5, 
                   label.position = "bottom", 
                   nrow = 1, 
                   label.hjust = .5
               )
    ) 

p2 <- ggplot(final_rates, 
             aes(fill = rate_cat, x = opioid_cat)) +
    scale_fill_viridis(option = "magma", 
                       name = "Mortality rate (per 100,000)", 
                       direction = -1, discrete = TRUE, 
                       na.value = "grey50") + 
    scale_x_discrete(NULL) + 
    scale_y_continuous("Number of states (#)", expand = c(0, .5)) + 
    geom_histogram(stat = "count", position = "stack", color = "white") +
    facet_grid(~ race_cat) +
    mk_nytimes(legend.position = "bottom", 
               panel.grid.major = element_blank(), 
               panel.border = element_rect(linetype = "solid", 
                                           fill = NA, 
                                           color = "grey75")) + 
    guides(fill = 
               guide_legend(
                   title.position = "left", 
                   keywidth = 2.5, 
                   keyheight = .5, 
                   label.position = "bottom", 
                   nrow = 1, 
                   label.hjust = .5
               ))

ggsave(sprintf('%s/fig_current_rates.pdf', plot_folder), 
       p1, width = 6, height = 8, scale = 1.25, device = cairo_pdf)
ggsave(sprintf('%s/fig_current_rates.png', plot_folder), 
       p1, width = 6, height = 8, scale = 1.25, dpi = 300)

ggsave(sprintf('%s/fig_current_rates_barchart.pdf', plot_folder), 
       p2, width = 7, height = 4, scale = 1.25, device = cairo_pdf)
ggsave(sprintf('%s/fig_current_rates_barchart.png', plot_folder), 
       p2, width = 7, height = 4, scale = 1.25, dpi = 300)

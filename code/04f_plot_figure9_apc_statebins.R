
## Imports 
library(tidyverse)
library(viridis)
library(statebins)
source("./code/mk_nytimes.R")

## Pull yaml ----
cfig        <- config::get()
PVAL        <- cfig$sig_p_value
MIN_RATE    <- cfig$min_rate
plot_folder <- cfig$plot_dir
data_folder <- cfig$working_data

## Data ----
est_df <- readRDS(sprintf("%s/joinpoint_results_dupe_rows_2016.RDS", 
                          data_folder)) %>% 
    filter(dupe_row == 0)

## Better APC categories ----
est_df <- est_df %>% 
    mutate(apc_sig = ifelse(slope_pval < PVAL & rate > MIN_RATE, 
                            apc, NA), 
           apc_cat = cut(apc_sig, 
                         breaks = c(-Inf, 0, 5, 10, 15, 20, 
                                    30, 50, 75, 100, Inf), 
                         ordered_result = TRUE))

est_df$apc_cat <- addNA(est_df$apc_cat)

apc_range <- range(est_df$apc_sig, na.rm = TRUE)
n_cats <- n_distinct(est_df$apc_cat)
levels(est_df$apc_cat)[1] <- sprintf("(%i,0]", round(apc_range[1]))
levels(est_df$apc_cat)[n_cats - 1] <- sprintf("(100,%i]", round(apc_range[2]))
levels(est_df$apc_cat)[n_cats] <- "NS"

## Plot APCs of current segment
apc_cat_df <- est_df %>% 
    group_by(opioid_type, race_cat, abbrev, race, opioid_cat) %>% 
    filter(year == max(year)) %>% 
    select(apc_cat, apc_sig) %>% 
    ungroup()

p1 <- ggplot(apc_cat_df, 
             aes(state = abbrev, fill = apc_cat)) +
    scale_fill_viridis(
        name = "APC (%)", direction = -1, 
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

p2 <- ggplot(apc_cat_df %>% filter(!is.na(apc_cat)), 
             aes(fill = apc_cat, x = opioid_cat)) +
    scale_fill_viridis(
        name = "APC (%)", direction = -1, discrete = TRUE, 
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

ggsave(sprintf('%s/fig_current_trajectory.pdf', plot_folder), 
       p1, width = 6, height = 8, scale = 1.25, device = cairo_pdf)
ggsave(sprintf('%s/fig_current_trajectory.png', plot_folder), 
       p1, width = 6, height = 8, scale = 1.25, dpi = 300)

ggsave(sprintf('%s/fig_current_trajectory_barchart.pdf', plot_folder), 
       p2, width = 7, height = 4, scale = 1.25, device = cairo_pdf)
ggsave(sprintf('%s/fig_current_trajectory_barchart.png', plot_folder), 
       p2, width = 7, height = 4, scale = 1.25, dpi = 300)

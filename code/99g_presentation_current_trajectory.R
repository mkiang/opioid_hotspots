library(tidyverse)
library(viridis)
library(statebins)
source("./code/mk_nytimes.R")

PVAL <- config::get()$sig_p_value
MIN_RATE <- config::get()$min_rate

## Data ----
est_df <- readRDS("./data/joinpoint_results_dupe_rows_2016.RDS") %>% 
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

p1 <- ggplot(apc_cat_df %>% 
                 filter(opioid_type != "opioid"), 
             aes(state = abbrev, fill = apc_cat)) +
    scale_fill_viridis(
        name = "APC (%)", direction = -1, 
        discrete = TRUE, na.value = "grey50") + 
    geom_statebins() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    facet_grid(race_cat ~ opioid_cat) +
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

saveRDS(p1, './plots/grobs/current_trajectory_map.RDS')

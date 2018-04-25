## Imports ----
library(tidyverse)
library(geofacet)
library(cowplot)
source('./code/mk_nytimes.R')

## Place to save
narcan::mkdir_p('./plots/state_plots_jp')

## Pull yaml ----
cfig        <- config::get()
PVAL        <- cfig$sig_p_value
MIN_RATE    <- cfig$min_rate
plot_folder <- cfig$plot_dir
data_folder <- cfig$working_data
rate_cuts <- cfig$rate_cuts
apc_cuts  <- cfig$apc_cuts

## Load data ----
jp_results <- readRDS(sprintf('%s/joinpoint_results_dupe_rows_2016.RDS', 
                              data_folder))
               
## Make state-specific plots ----
for (s in unique(jp_results$abbrev)) {
    temp_df <- jp_results %>% 
        filter(abbrev == s, 
               race != "total")
    
    p1 <- ggplot(data = temp_df) + 
        geom_errorbar(aes(x = year, ymax = rate + 1.96 * rate_se, 
                          ymin = rate - 1.96 * rate_se, color = race_cat), 
                      width = 0, alpha = .2) + 
        geom_point(aes(x = year, y = rate, color = race_cat), 
                   alpha = .2, shape = 5) + 
        geom_line(aes(x = year, y = pred_rate, color = race_cat, 
                      group = interaction(race_cat, line_seg), 
                      linetype = as.factor(slope_sig))) + 
        geom_point(aes(x = j_year, y = pred_rate, color = race_cat, 
                       shape = as.factor(slopechg_sig))) + 
        facet_grid(~ opioid_cat, scales = "free_y") + 
        scale_linetype_manual(values = c("dashed", "solid"), guide = "none") + 
        scale_shape_manual(values = c(0, 19), guide = "none") + 
        scale_x_continuous(NULL, 
                           breaks = seq(2000, 2015, 5), 
                           labels = c("'00", "'05", "'10", "'15"), 
                           expand = c(0, .25)) + 
        scale_y_continuous(sprintf("%s Mortality Rate (per 100,000)", s), 
                           expand = c(0, .5)) + 
        scale_color_brewer(palette = "Set1", guide = "none", direction = -1) + 
        mk_nytimes(panel.border = element_rect(linetype = "solid", 
                                               fill = NA, 
                                               color = "grey75"))
    
    p2 <- ggplot(data = temp_df, 
                 aes(x = j_year, xmin = year_lb, 
                     xmax = year_ub, y = race_cat, 
                     color = race_cat)) + 
      geom_errorbarh(height = .5, alpha = .9, position = "nudge") + 
      geom_point(color = "white", size = 2.5) + 
      geom_point(size = 1.5) + 
      facet_grid(~ opioid_cat, scales = "free_y") + 
      scale_linetype_manual(values = c("dashed", "solid"), guide = "none") + 
      scale_shape_manual(values = c(0, 19), guide = "none") + 
      scale_x_continuous(NULL, 
                         breaks = seq(2000, 2015, 5), 
                         labels = c("'00", "'05", "'10", "'15"), 
                         expand = c(0, .25), 
                         limits = c(1999, 2016)) + 
      scale_y_discrete(NULL, drop = FALSE, expand = c(0, 1.5)) + 
      scale_color_brewer(palette = "Set1", guide = "none", direction = -1) + 
      mk_nytimes(panel.border = element_rect(linetype = "solid", 
                                             fill = NA, 
                                             color = "grey75"), 
                 strip.text = element_blank(), 
                 strip.background = element_blank(), 
                 axis.text.y = element_blank(), 
                 legend.position = "none") + 
      theme(panel.grid.major.y = element_blank(), 
            plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
      labs(subtitle = "Joinpoint (95% CI)")
    
    p <- plot_grid(p1, p2, nrow = 2, 
                   axis = "tblr", align = "v", 
                   rel_heights = c(3.5, 1))
    
    ggsave(sprintf("%s/state_plots_jp/%s_facet_type_2016.pdf", plot_folder, s), 
           p, width = 8, height = 4.5, scale = 1.25, device = cairo_pdf)
}

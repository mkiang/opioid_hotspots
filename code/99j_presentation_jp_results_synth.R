## Plot the joinpoint model results

## Imports ----
library(tidyverse)
library(geofacet)
source('./code/mk_nytimes.R')

## Pull yaml ----
cfig        <- config::get()
PVAL        <- cfig$sig_p_value
plot_folder <- cfig$plot_dir
data_folder <- cfig$working_data

## Load data ----
jp_results <- readRDS(sprintf('%s/joinpoint_results_dupe_rows_2016.RDS', 
                              data_folder))

temp_df <- jp_results %>% 
    filter(opioid_type == "synth_opioid", 
           race != "total")

p <- ggplot(data = temp_df) + 
    geom_errorbar(aes(x = year, ymax = rate + 1.96 * rate_se, 
                      ymin = rate - 1.96 * rate_se, color = race), 
                  width = 0, alpha = .2) + 
    geom_point(aes(x = year, y = rate, color = race), 
               alpha = .2, shape = 5) + 
    geom_line(aes(x = year, y = pred_rate, color = race, 
                  group = interaction(race, line_seg), 
                  linetype = as.factor(slope_sig))) + 
    geom_point(
        data = temp_df %>% 
            filter(!is.na(j_year)), 
        aes(x = j_year, y = pred_rate, color = race, 
            shape = as.factor(slopechg_sig))) + 
    facet_geo(~ abbrev) + 
    coord_cartesian(ylim = c(0, 30)) + 
    scale_linetype_manual("Slope", 
                          values = c("dashed", "solid"), 
                          labels = c(sprintf("NS (P>%0.2f)", PVAL), 
                                     sprintf("Sig (P<%0.2f)", PVAL)), 
                          drop = FALSE) + 
    scale_shape_manual("Joinpoint", values = c(0, 19, NA), 
                       labels = c(sprintf("NS (P>%0.2f)", PVAL), 
                                  sprintf("Sig (P<%0.2f)", PVAL)), 
                       drop = FALSE) + 
    scale_x_continuous(NULL, 
                       breaks = seq(2000, 2015, 5), 
                       labels = c("'00", "'05", "'10", "'15"), 
                       expand = c(0, .25)) + 
    scale_y_continuous(sprintf("%s mortality rate (per 100,000)", 
                               "Synthetic opioid"), 
                       expand = c(0, .5)) + 
    scale_color_brewer("Race/Ethnicity", palette = "Set1", direction = -1, 
                       labels = c("Non-Hispanic Black", 
                                  "Non-Hispanic White")) + 
    mk_nytimes(panel.border = element_rect(linetype = "solid", 
                                           fill = NA, 
                                           color = "grey75"),
               legend.position = c(1, 0), 
               legend.justification = c(1, 0))

saveRDS(p, './plots/grobs/jp_results_synth.RDS')

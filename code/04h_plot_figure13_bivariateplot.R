
## Imports 
library(tidyverse)
library(viridis)
library(gridExtra)
library(statebins)
source("./code/mk_nytimes.R")

## Pull yaml ----
cfig        <- config::get()
PVAL        <- cfig$sig_p_value
MIN_RATE    <- cfig$min_rate
plot_folder <- cfig$plot_dir
data_folder <- cfig$working_data
rate_cuts <- cfig$rate_cuts
apc_cuts  <- cfig$apc_cuts

## Data ----
est_df <- readRDS(sprintf("./data/joinpoint_results_dupe_rows_2016.RDS", 
                          data_folder)) %>% 
    filter(dupe_row == 0)

## Let's make a bivariate color palette:
##  X axis will be mortality rate with three categories: low, medium, high
##  Y axis will be APC with three categories but all positive (ignore negs)

c_legend <- 
    expand.grid(list(mort_lev = c("low_mort", "med_mort", "high_mort"), 
                     apc_lev  = c("low_apc", "med_apc", "high_apc")), 
                stringsAsFactors = TRUE) %>% 
    as_tibble() %>% 
    mutate(
        color_hex = c("#e8e6f2", "#b5d3e7", "#4fadd0", 
                      "#e5b4d9", "#b8b3d8", "#3983bb", 
                      "#d34fa6", "#b03598", "#2a1a8a"),
        color_hex_a = c("#e8e8e8", "#e4acac", "#c85a5a", 
                        "#b0d5df", "#ad9ea5", "#985356", 
                        "#64acbe", "#627f8c", "#574249")
    )

## Make new categorical versions of mortality and APC based on the config.yml
hotspots <- est_df %>% 
    group_by(abbrev, race, opioid_type) %>% 
    filter(year == max(year), 
           slope_pval < PVAL) %>% 
    transmute(mort_lev = 
                  case_when(
                      between(rate, 0, rate_cuts[1]) ~ "low_mort", 
                      between(rate, rate_cuts[1], rate_cuts[2]) ~ "med_mort", 
                      rate > rate_cuts[2] ~ "high_mort", 
                      TRUE ~ NA_character_
                  ), 
              apc_lev = 
                  case_when(
                      between(apc, 0, apc_cuts[1]) ~ "low_apc", 
                      between(apc, apc_cuts[1], apc_cuts[2]) ~ "med_apc", 
                      apc > apc_cuts[2] ~ "high_apc", 
                      TRUE ~ NA_character_
                  )
    ) 

## Add the color scale according to categories
hotspots <- hotspots%>% 
    left_join(c_legend) %>% 
    right_join(est_df %>% 
                   group_by(abbrev, race, opioid_type) %>% 
                   filter(year == max(year))
    ) 

## Convert them to factors so they retain order
hotspots <- hotspots %>% 
    mutate(
        color_hex_na = ifelse(is.na(color_hex), "grey97", color_hex),  
        mort_lev = factor(mort_lev, 
                          levels = c("low_mort", "med_mort", "high_mort"), 
                          ordered = TRUE), 
        apc_lev = factor(apc_lev,
                         levels = c("low_apc", "med_apc", "high_apc"), 
                         ordered = TRUE)
    )

## Add a race_opioid factor
hotspots <- hotspots %>% 
    mutate(
        race_opioid = paste(race, opioid_type, sep = "_")
    ) %>% 
    mutate(
        race_opioid = factor(race_opioid, 
                             levels = c("nhw_opioid", "nhw_natural_opioid", 
                                        "nhw_heroin", "nhw_synth_opioid", 
                                        "nhb_opioid", "nhb_natural_opioid", 
                                        "nhb_heroin", "nhb_synth_opioid"), 
                             labels = c("NHW, All", "NHW, Natural", 
                                        "NHW, Heroin", "NHW, Synthetic", 
                                        "NHB, All", "NHB, Natural", 
                                        "NHB, Heroin", "NHB, Synthetic"), 
                             ordered = TRUE)
    ) %>% 
    ungroup()

saveRDS(hotspots, './data/hotspots_df.RDS')

p1 <- ggplot(hotspots, 
             aes(state = abbrev, fill = color_hex_na)) +
    scale_fill_identity(NULL, na.value = "black") + 
    geom_statebins(border_col = "grey90", border_size = .5) +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    facet_grid(opioid_cat ~ race_cat) +
    mk_nytimes(legend.position = "none", 
               panel.grid.major = element_blank(), 
               axis.text = element_blank(), 
               panel.border = element_rect(linetype = "solid", 
                                           fill = NA, 
                                           color = "grey75"))

p_legend <- 
    ggplot(c_legend, 
           aes(x = mort_lev, y = apc_lev, fill = color_hex)) + 
    geom_tile(color = "white") + 
    scale_fill_identity(na.value = "grey50") + 
    scale_x_discrete("Mortality rate\n(per 100,000)", expand = c(0.05, 0), 
                     labels = c(
                         sprintf("<%i", rate_cuts[1]), 
                         sprintf("%i-%i", rate_cuts[1], rate_cuts[2]), 
                         sprintf(">%i", rate_cuts[2]))
    ) + 
    scale_y_discrete("APC (%)", expand = c(0.05, 0), 
                     labels = c(
                         sprintf("0-%i", apc_cuts[1]), 
                         sprintf("%i-%i", apc_cuts[1], apc_cuts[2]),
                         sprintf(">%i", apc_cuts[2]))
    ) + 
    mk_nytimes(panel.grid.major = element_blank(), 
               axis.text = element_text(size = 11), 
               axis.line = element_line(
                   arrow = arrow(
                       length = unit(.15, "inches"), 
                       type = "open", angle = 20)
               ), 
               plot.subtitle = element_text(size = 13, hjust=0.5)
    ) + 
    labs(subtitle = "Legend") + 
    coord_equal()

ggsave(sprintf('%s/fig_hotspots.pdf', plot_folder), 
       p1, width = 6, height = 8, scale = 1.25, device = cairo_pdf)

ggsave(sprintf('%s/fig_hotspots_legend.pdf', plot_folder), 
       p_legend, width = 1, height = 1, scale = 2.5, device = cairo_pdf)

ggsave(sprintf('%s/fig_hotspots_combined.pdf', plot_folder), 
       arrangeGrob(p_legend, p1, 
                   layout_matrix = cbind(c(rep(NA, 6), 1, 1), rep(2, 8)), 
                   widths = c(2, 7)), 
       width = 8, height = 8, scale = 1.25, 
       device = cairo_pdf)

ggsave(sprintf('%s/fig_hotspots.png', plot_folder), 
       p1, width = 6, height = 8, scale = 1.25, dpi = 300)

ggsave(sprintf('%s/fig_hotspots_legend.png', plot_folder), 
       p_legend, width = 1, height = 1, scale = 2.5, dpi = 300)

ggsave(sprintf('%s/fig_hotspots_combined.png', plot_folder), 
       arrangeGrob(p_legend, p1, 
                   layout_matrix = cbind(c(rep(NA, 6), 1, 1), rep(2, 8)), 
                   widths = c(2, 7)), 
       width = 8, height = 8, scale = 1.25, dpi = 300)

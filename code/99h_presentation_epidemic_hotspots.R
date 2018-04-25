## Plotting hotspots
library(tidyverse)
library(viridis)
library(statebins)
library(gridExtra)
source("./code/mk_nytimes.R")

## Config
cfig <-  config::get()
PVAL <-cfig$sig_p_value
rate_cuts <- cfig$rate_cuts
apc_cuts  <- cfig$apc_cuts

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

hotspots <- readRDS('./data/hotspots_df.RDS')

p1 <- ggplot(hotspots %>% 
                 filter(opioid_type != "opioid"), 
             aes(state = abbrev, fill = color_hex_na)) +
    scale_fill_identity(NULL, na.value = "black") + 
    geom_statebins(border_col = "grey90", border_size = .5) +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    facet_grid(race_cat ~ opioid_cat) +
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

p3_black <- ggplot(hotspots, 
                   aes(group = abbrev, color = "black", 
                       shape = race_opioid))  + 
    # geom_vline(xintercept = c(rate_cuts[1], rate_cuts[2]), 
    #            linetype = "dotted", alpha = .8) + 
    # geom_hline(yintercept = c(apc_cuts[1], apc_cuts[2]), 
    #            linetype = "dotted", alpha = .8) +
    geom_point(aes(x = rate, y = apc), size = 2) +
    scale_shape_manual(NULL, 
                       values = c(1, 0, 2, 5, 16, 15, 17, 18)) +
    scale_color_identity(NULL) + 
    scale_x_continuous("Mortality rate (per 100,000)", 
                       expand = c(0, .5), 
                       breaks = c(0, 10, 20, 30, 40, 50), 
                       limits = c(0, 50)) +
    scale_y_continuous("Annual Percent Change (%)",
                       expand = c(0, 0), limits = c(0, 350), 
                       breaks = c(0, apc_cuts[1], apc_cuts[2], 
                                  100, 200, 300, 350)) +
    mk_nytimes(legend.position = "bottom", 
               panel.grid.major = element_blank()) +
    guides(shape = guide_legend(nrow = 2))

p3_black_vert <- ggplot(hotspots, 
                        aes(group = abbrev, color = "black", 
                            shape = race_opioid))  + 
    geom_vline(xintercept = c(rate_cuts[1], rate_cuts[2]),
               linetype = "dotted", alpha = .8) +
    # geom_hline(yintercept = c(apc_cuts[1], apc_cuts[2]), 
    #            linetype = "dotted", alpha = .8) +
    geom_point(aes(x = rate, y = apc), size = 2) +
    scale_shape_manual(NULL, 
                       values = c(1, 0, 2, 5, 16, 15, 17, 18)) +
    scale_color_identity(NULL) + 
    scale_x_continuous("Mortality rate (per 100,000)", 
                       expand = c(0, .5), 
                       breaks = c(0, 10, 20, 30, 40, 50), 
                       limits = c(0, 50)) +
    scale_y_continuous("Annual Percent Change (%)",
                       expand = c(0, 0), limits = c(0, 350), 
                       breaks = c(0, apc_cuts[1], apc_cuts[2], 
                                  100, 200, 300, 350)) +
    mk_nytimes(legend.position = "bottom", 
               panel.grid.major = element_blank()) +
    guides(shape = guide_legend(nrow = 2))

p3_black_vert_horiz <- ggplot(hotspots, 
                              aes(group = abbrev, color = "black", 
                                  shape = race_opioid))  + 
    geom_vline(xintercept = c(rate_cuts[1], rate_cuts[2]),
               linetype = "dotted", alpha = .8) +
    geom_hline(yintercept = c(apc_cuts[1], apc_cuts[2]),
               linetype = "dotted", alpha = .8) +
    geom_point(aes(x = rate, y = apc), size = 2) +
    scale_shape_manual(NULL, 
                       values = c(1, 0, 2, 5, 16, 15, 17, 18)) +
    scale_color_identity(NULL) + 
    scale_x_continuous("Mortality rate (per 100,000)", 
                       expand = c(0, .5), 
                       breaks = c(0, 10, 20, 30, 40, 50), 
                       limits = c(0, 50)) +
    scale_y_continuous("Annual Percent Change (%)",
                       expand = c(0, 0), limits = c(0, 350), 
                       breaks = c(0, apc_cuts[1], apc_cuts[2], 
                                  100, 200, 300, 350)) +
    mk_nytimes(legend.position = "bottom", 
               panel.grid.major = element_blank()) +
    guides(shape = guide_legend(nrow = 2))

p3 <- ggplot(hotspots, 
             aes(group = abbrev, color = color_hex, 
                 shape = race_opioid))  + 
    geom_vline(xintercept = c(rate_cuts[1], rate_cuts[2]), 
               linetype = "dotted", alpha = .8) + 
    geom_hline(yintercept = c(apc_cuts[1], apc_cuts[2]), 
               linetype = "dotted", alpha = .8) +
    geom_point(aes(x = rate, y = apc), size = 2) +
    scale_shape_manual(NULL, 
                       values = c(1, 0, 2, 5, 16, 15, 17, 18)) +
    scale_color_identity(NULL) + 
    scale_x_continuous("Mortality rate (per 100,000)", 
                       expand = c(0, .5), 
                       breaks = c(0, 10, 20, 30, 40, 50), 
                       limits = c(0, 50)) +
    scale_y_continuous("Annual Percent Change (%)",
                       expand = c(0, 0), limits = c(0, 350), 
                       breaks = c(0, apc_cuts[1], apc_cuts[2], 
                                  100, 200, 300, 350)) +
    mk_nytimes(legend.position = "bottom", 
               panel.grid.major = element_blank()) +
    guides(shape = guide_legend(nrow = 2))

saveRDS(p1, './plots/grobs/hotspots_map.RDS')
saveRDS(p_legend, './plots/grobs/hotspots_legend.RDS')
saveRDS(p3_black, './plots/grobs/hotspots_scatter1.RDS')
saveRDS(p3_black_vert, './plots/grobs/hotspots_scatter2.RDS')
saveRDS(p3_black_vert_horiz, './plots/grobs/hotspots_scatter3.RDS')
saveRDS(p3, './plots/grobs/hotspots_scatter_final.RDS')

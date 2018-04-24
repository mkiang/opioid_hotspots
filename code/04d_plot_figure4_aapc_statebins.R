## Plot statebins of the AAPC by race/ethnicity and opioid type
library(tidyverse)
library(statebins)
library(viridis)
source('./code/mk_nytimes.R')

## Pull yaml ----
cfig        <-  config::get()
PVAL        <-cfig$sig_p_value
plot_folder <- cfig$plot_dir
data_folder <- cfig$working_data

## Import AAPC data
aapc_df <- readRDS(sprintf("%s/public_aapc_results.RDS", working_data))

aapc_df <- aapc_df %>% 
    mutate(aapc_sig = ifelse(p_value < PVAL, aapc, NA)) 

## Plot it
brks <- c(min(aapc_df$aapc_sig, na.rm = TRUE), 
          10, 20, 30, 40, 
          max(aapc_df$aapc_sig, na.rm = TRUE))

lbls <- sprintf("%0.1f", 
                c(min(aapc_df$aapc_sig, na.rm = TRUE), 
                  10, 20, 30, 40, 
                  max(aapc_df$aapc_sig, na.rm = TRUE)))

p <- ggplot(aapc_df, aes(fill = aapc_sig, state = abbrev)) + 
    geom_statebins() + 
    scale_fill_viridis("AAPC (%)", direction = -1, option = "plasma", 
                       breaks = brks, 
                       labels = lbls) + 
    facet_grid(opioid_cat ~ race_cat) + 
    coord_equal() + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    mk_nytimes(legend.position = "bottom", 
               panel.border = element_rect(linetype = "solid", 
                                           fill = NA, 
                                           color = "grey75"),
               panel.grid.major = element_blank(), 
               axis.text = element_blank(), 
               legend.key.width = unit(2, "cm"), 
               legend.key.height = unit(.25, "cm"))

ggsave(sprintf('./%s/fig_aapc_of_states.pdf', plot_folder),
       p, width = 6, height = 8, scale = 1.35, device = cairo_pdf)
ggsave(sprintf('./%s/fig_aapc_of_states.png', plot_folder), 
       p, width = 6, height = 8, scale = 1.35, dpi = 300)

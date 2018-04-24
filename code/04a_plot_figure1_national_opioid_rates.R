## Create Figure 1 in the paper

## Imports ----
library(tidyverse)
library(ggrepel)
source('./code/mk_nytimes.R')

## Pull in YAML config ----
cfig <- config::get()
data_folder <- cfig$working_data
plot_folder <- cfig$plot_dir

## Load national data
## NOTE: These data are from the public-use data (because it includes 2016), 
##      and can be found on a branch of our `opioid_trends` project. See:
##      paste0("https://github.com/mkiang/opioid_trends/blob/", 
##             "with-2016-data/data/age_standardized_rates_long.csv")
age_std <- read_csv('./data/national_age_standardized_rates_long.csv')

## Make a color palette
c_pal <- c("black", RColorBrewer::brewer.pal(3, "Set1"))

df1 <- age_std %>% 
  filter(race == "total", 
         opioid_type %in% c("opioid", "synth", "heroin", "natural")) %>% 
  mutate(opioid_cat = 
           factor(opioid_type, 
                  levels = c("opioid", "natural", "heroin", "synth"), 
                  labels = c("All opioids", "Natural", 
                             "Heroin", "Synthetic"), 
                  ordered = TRUE), 
         std_rate = 
           case_when(year <= 1998 &
                       opioid_type %in% c("synth", "natural") ~ 
                       NA_real_, 
                     TRUE ~ std_rate), 
         lab = 
           sprintf("%s (%0.1f)", 
                   as.character(opioid_cat), 
                   round(std_rate, 1))
  )

f1 <- ggplot(df1, aes(x = year, y = std_rate, 
                      color = opioid_cat, group = opioid_cat)) + 
  geom_line(data = df1 %>% filter(year <= 1998), size = 1) + 
  geom_line(data = df1 %>% filter(year > 1998), size = 1) + 
  geom_point(size = 2.5, color = "white") + 
  geom_point() + 
  scale_color_manual(name = NULL, values = c_pal) + 
  scale_x_continuous(NULL, 
                     expand = c(0, .5)) + 
  scale_y_continuous("Age-adjusted mortality rate (per 100,000)", 
                     expand = c(0, .08), 
                     limits = c(0, NA)) + 
  mk_nytimes()

ggsave(sprintf('%s/fig_overall_opioid_mortality.pdf', plot_folder), 
       f1, width = 6, height = 3.5, scale = 1.5, device = cairo_pdf)

ggsave(sprintf('%s/fig_overall_opioid_mortality.png', plot_folder), 
       f1, width = 6, height = 3.5, scale = 1.5, dpi = 300)

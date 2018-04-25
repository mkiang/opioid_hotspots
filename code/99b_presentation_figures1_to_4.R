library(tidyverse)
library(ggrepel)
source('./code/mk_nytimes.R')

narcan::mkdir_p("./plots/grobs")

## Load national data
## NOTE: These data are from the public-use data (because it includes 2016), 
##      and can be found on a branch of our `opioid_trends` project. See:
##      paste0("https://github.com/mkiang/opioid_trends/blob/", 
##             "with-2016-data/data/age_standardized_rates_long.csv")
age_std <- read_csv('./data/national_age_standardized_rates_long.csv')

## Make a color palette
c_pal <- RColorBrewer::brewer.pal(3, "Set1")

## First plot is just the national opioid mortality rate, total population
df1 <- age_std %>% 
  filter(race == "total", 
         opioid_type == "opioid") %>% 
  mutate(lab = sprintf("All opioids: %.2f", std_rate), 
         lab2 = "Over 42,000\ndeaths")

f1 <- ggplot(df1, aes(x = year, y = std_rate)) + 
  geom_line(data = df1 %>% filter(year <= 1998), size = 1) + 
  geom_line(data = df1 %>% filter(year > 1998), size = 1) + 
  geom_point(size = 2.5, color = "white") + 
  geom_point() +
  scale_x_continuous(NULL, expand = c(0, .75)) + 
  scale_y_continuous("Age-adjusted mortality rate (per 100,000)", 
                     expand = c(0, .1), 
                     limits = c(0, NA)) + 
  geom_text_repel(data = df1 %>% filter(year %in% c(2016)), 
                  aes(label = lab), 
                  nudge_x = -5, alpha = .75) + 
  geom_text_repel(data = df1 %>% filter(year %in% c(1979)), 
                  aes(label = lab), 
                  nudge_y = 1, alpha = .75) + 
  mk_nytimes()

f1a <- ggplot(df1, aes(x = year, y = std_rate)) + 
    geom_line(data = df1 %>% filter(year <= 1998), size = 1) + 
    geom_line(data = df1 %>% filter(year > 1998), size = 1) + 
    geom_point(size = 2.5, color = "white") + 
    geom_point() +
    scale_x_continuous(NULL, expand = c(0, .75)) + 
    scale_y_continuous("Age-adjusted mortality rate (per 100,000)", 
                       expand = c(0, .1), 
                       limits = c(0, NA)) + 
    geom_text_repel(data = df1 %>% filter(year %in% c(2016)), 
                    aes(label = lab2), 
                    nudge_x = -5, alpha = .75) + 
    mk_nytimes()

## Add natural to the plot
df2 <- age_std %>% 
  filter(race == "total", 
         opioid_type == "natural", 
         year > 1998) %>% 
  mutate(lab = sprintf("Natural: %.2f", std_rate))

f2 <- ggplot(df1, aes(x = year, y = std_rate)) + 
  geom_line(data = df1 %>% filter(year <= 1998), alpha = .4) + 
  geom_line(data = df1 %>% filter(year > 1998), alpha = .4) + 
  geom_point(alpha = .4) +
  scale_x_continuous(NULL, expand = c(0, .75)) + 
  scale_y_continuous("Age-adjusted mortality rate (per 100,000)", 
                     expand = c(0, .05), 
                     limits = c(0, NA)) + 
  
  ## Adding prescription line
  geom_line(data = df2, aes(x = year, y = std_rate), 
            color = c_pal[1], size = 1) + 
  geom_text_repel(data = df2 %>% filter(year == 2016), 
                  aes(x = year, y = std_rate, label = lab), 
                  color = c_pal[1], nudge_x = -2.5, alpha = .75) + 
  geom_point(data = df2, aes(x = year, y = std_rate), 
             size = 2.5, color = "white") + 
  geom_point(data = df2, aes(x = year, y = std_rate), 
             color = c_pal[1]) + 
  
  mk_nytimes()

df4 <- age_std %>% 
  filter(race == "total", 
         opioid_type == "heroin") %>% 
  mutate(lab = sprintf("Heroin: %.2f", std_rate))

f4 <- ggplot(df1, aes(x = year, y = std_rate)) + 
  geom_line(data = df1 %>% filter(year <= 1998), alpha = .4) + 
  geom_line(data = df1 %>% filter(year > 1998), alpha = .4) + 
  geom_point(alpha = .4) +
  scale_x_continuous(NULL, expand = c(0, .85)) + 
  scale_y_continuous("Age-adjusted mortality rate (per 100,000)", 
                     expand = c(0, .05), 
                     limits = c(0, NA)) + 
  
  ## Adding prescription line
  geom_line(data = df2, aes(x = year, y = std_rate), 
            color = c_pal[1], alpha = .5) + 
  geom_point(data = df2, aes(x = year, y = std_rate), 
             color = c_pal[1], alpha = .5) + 
  
  ## Adding heroin
  geom_line(data = df4 %>% filter(year > 1998), 
            aes(x = year, y = std_rate), 
            color = c_pal[2], alpha = 1, size = 1) + 
  geom_line(data = df4 %>% filter(year < 1999), 
            aes(x = year, y = std_rate, size = 1), 
            color = c_pal[2], alpha = 1, size = 1) + 
  geom_point(data = df4, aes(x = year, y = std_rate), 
             color = "white", alpha = 1, size = 2.5) + 
  geom_point(data = df4, aes(x = year, y = std_rate), 
             color = c_pal[2], alpha = 1) + 
  geom_text_repel(data = df4 %>% filter(year == 2016), 
                  aes(label = lab), nudge_x = -2.5, 
                  color = c_pal[2], alpha = .75) + 
  
  mk_nytimes()


df5 <- age_std %>% 
  filter(race == "total", 
         opioid_type == "synth", 
         year > 1998) %>% 
  mutate(lab = sprintf("Synthetic: %.2f", std_rate))

f5 <- ggplot(df1, aes(x = year, y = std_rate)) + 
  geom_line(data = df1 %>% filter(year <= 1998), alpha = .4) + 
  geom_line(data = df1 %>% filter(year > 1998), alpha = .4) + 
  geom_point(alpha = .4) +
  scale_x_continuous(NULL, expand = c(0, .75)) + 
  scale_y_continuous("Age-adjusted mortality rate (per 100,000)", 
                     expand = c(0, .05), 
                     limits = c(0, NA)) + 
  
  ## Adding prescription line
  geom_line(data = df2, aes(x = year, y = std_rate), 
            color = c_pal[1], alpha = .5) + 
  geom_point(data = df2, aes(x = year, y = std_rate), 
             color = c_pal[1], alpha = .5) + 
  
  ## Adding heroin
  geom_line(data = df4 %>% filter(year > 1998), 
            aes(x = year, y = std_rate, alpha = .4), 
            color = c_pal[2], alpha = .5) + 
  geom_line(data = df4 %>% filter(year < 1999), 
            aes(x = year, y = std_rate, alpha = .4), 
            color = c_pal[2], alpha = .5) + 
  geom_point(data = df4, aes(x = year, y = std_rate), 
             color = c_pal[2], alpha = .5) + 
  
  ## Adding synth
  geom_line(data = df5, aes(x = year, y = std_rate), 
            color = c_pal[3], alpha = 1, size = 1) + 
  geom_point(data = df5, aes(x = year, y = std_rate), 
             color = "white", size = 2.5, alpha = 1) + 
  geom_point(data = df5, aes(x = year, y = std_rate), 
             color = c_pal[3], alpha = 1) + 
  geom_text_repel(data = df5 %>% filter(year == 2016), 
                  aes(x = year, y = std_rate, label = lab), 
                  color = c_pal[3]) + 
  
  mk_nytimes()

saveRDS(f1,  "./plots/grobs/presentation_fig1.RDS")
saveRDS(f1a, "./plots/grobs/presentation_fig1a.RDS")
saveRDS(f2,  "./plots/grobs/presentation_fig2.RDS")
saveRDS(f4,  "./plots/grobs/presentation_fig3.RDS")
saveRDS(f5,  "./plots/grobs/presentation_fig4.RDS")


f6 <- ggplot(df1, aes(x = year, y = std_rate)) + 
    geom_line(data = df1 %>% filter(year <= 1998), alpha = .4) + 
    geom_line(data = df1 %>% filter(year > 1998), alpha = .4) + 
    geom_point(alpha = .4) +
    scale_x_continuous(NULL, expand = c(0, .75)) + 
    scale_y_continuous("Age-adjusted mortality rate (per 100,000)", 
                       expand = c(0, .05), 
                       limits = c(0, NA)) + 
    
    ## Adding prescription line
    geom_line(data = df2, aes(x = year, y = std_rate), 
              color = c_pal[1], size = 1) + 
    geom_text_repel(data = df2 %>% filter(year == 2016), 
                    aes(x = year, y = std_rate, label = lab), 
                    color = c_pal[1], nudge_x = -6.5, alpha = .75) + 
    geom_point(data = df2, aes(x = year, y = std_rate), 
               size = 2.5, color = "white") + 
    geom_point(data = df2, aes(x = year, y = std_rate), 
               color = c_pal[1]) + 
    
    ## Adding heroin
    geom_line(data = df4 %>% filter(year > 1998), 
              aes(x = year, y = std_rate), 
              color = c_pal[2], alpha = 1, size = 1) + 
    geom_line(data = df4 %>% filter(year < 1999), 
              aes(x = year, y = std_rate, size = 1), 
              color = c_pal[2], alpha = 1, size = 1) + 
    geom_point(data = df4, aes(x = year, y = std_rate), 
               color = "white", alpha = 1, size = 2.5) + 
    geom_point(data = df4, aes(x = year, y = std_rate), 
               color = c_pal[2], alpha = 1) + 
    geom_text_repel(data = df4 %>% filter(year == 2016), 
                    aes(label = lab), nudge_x = -5.5, 
                    color = c_pal[2], alpha = .75) + 
    
    ## Adding synth
    geom_line(data = df5, aes(x = year, y = std_rate), 
              color = c_pal[3], alpha = 1, size = 1) + 
    geom_point(data = df5, aes(x = year, y = std_rate), 
               color = "white", size = 2.5, alpha = 1) + 
    geom_point(data = df5, aes(x = year, y = std_rate), 
               color = c_pal[3], alpha = 1) + 
    geom_text_repel(data = df5 %>% filter(year == 2016), 
                    aes(x = year, y = std_rate, label = lab), 
                    color = c_pal[3]) + 
    
    mk_nytimes()
saveRDS(f6,  "./plots/grobs/presentation_fig2_to_4.RDS")

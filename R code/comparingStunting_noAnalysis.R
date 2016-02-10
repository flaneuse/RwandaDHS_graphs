# File to make a preliminary graphic based on a secondary analysis of Rwandan DHS data.
# Data are taken from the DHS's 2010 Report () and 2015 Report


# Load functions ----------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)


# Import data -------------------------------------------------------------

df = data.frame(stunting2010 = c(.507, .499, .439, .423, .235),
                stunting2014 = c(.392, .449, .348, .405, .227),
                region = c('North', 'West', 'East', 'South', 'Kigali')) %>% 
  mutate(chg = stunting2014 - stunting2010)

df$region = factor(df$region, 
                   levels = rev(c('North', 'West', 'East', 'South', 'Kigali')))

# plot the change ---------------------------------------------------------

ggplot(df, aes(x = stunting2010, xend = stunting2014 * 1.018,
               y = region, yend = region, size = 4)) +
  geom_segment(colour = 'grey', size = 1.5,
               arrow = arrow(length = unit(0.05, "npc"))) + 
  geom_point(colour = 'red', shape = 19) +
  geom_point(aes(x = stunting2014), colour = 'blue', shape = 16) +
  theme_bw() +
  xlab('stunting rates in Rwanda') +
  geom_text(aes(x = 0.375),
            label = '2014', colour = 'blue', hjust = 1,
            data = df %>% filter(region == 'North')) + 
    geom_text(aes(x = 0.52),
    label = '2010', colour = 'red', hjust = 0,
            data = df %>% filter(region == 'North')) + 
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.55))

# File to make a preliminary graphic based on a secondary analysis of Rwandan DHS data.
# Data are taken from the DHS's 2010 Report () and 2015 Report


# Load functions ----------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(llamar)
library(choroplethr)
library(choroplethrAdmin1)


# Import data -------------------------------------------------------------

df = data.frame(stunting2010 = c(.507, .499, .439, .423, .235),
                stunting2014 = c(.392, .449, .348, .405, .227),
                region = c('North', 'West', 'East', 'South', 'Kigali'),
                choroName = c('northern province', 'western province',
                              'eastern province', 'southern province',
                              'kigali province')
                ) %>% 
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
  ggtitle('Stunting rates dropped considerably but unequally across Rwanda between 2010 and 2014') +
  geom_text(aes(x = 0.36),
            label = '2014', colour = 'blue', hjust = 0.5,
            data = df %>% filter(region == 'North')) + 
    geom_text(aes(x = 0.54),
    label = '2010', colour = 'red', hjust = 0.5,
            data = df %>% filter(region == 'North')) + 
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.55)) +
  coord_flip()


# choropleth --------------------------------------------------------------
df2010 = choroplethrAdmin1::get_admin1_regions(country.name = "rwanda")

df2014 = choroplethrAdmin1::get_admin1_regions(country.name = "rwanda")

df2010 = full_join(df2010, df, by = c("region" = "choroName")) %>% 
  select(country, region, value = stunting2010)

df2014 = full_join(df2014, df, by = c("region" = "choroName")) %>% 
  select(country, region, value = stunting2014)

admin1_choropleth(df = df2014, country.name = 'rwanda', num_colors = 1) +
  scale_fill_gradientn(colours = brewer.pal(9, 'Blues'),
    limits = c(0.1, 0.55)) +
  ggtitle('2014 DHS')

admin1_choropleth(df = df2010, country.name = 'rwanda', num_colors = 1) +
  scale_fill_gradientn(colours = brewer.pal(9, 'Blues'),
                       limits = c(0.1, 0.55)) + 
  ggtitle('2010 DHS')

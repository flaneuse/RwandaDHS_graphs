# File to make a preliminary graphic based on a secondary analysis of Rwandan DHS data.
# Data are taken from the DHS's 2010 Report () and 2015 Report


# Load functions ----------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(llamar)
library(choroplethr)
library(choroplethrAdmin1)
library(llamar)
library(RColorBrewer)


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
blue = brewer.pal(11, 'RdYlBu')[11]
red = brewer.pal(11, 'RdYlBu')[2]

ggplot(df, aes(x = stunting2010, xend = stunting2014 * 1.018,
               y = region, yend = region, size = 4)) +
  geom_segment(colour = grey40K, size = 1.5) + 
  geom_segment(colour = grey40K, size = 1.5,
               data = df %>% filter(abs(chg) > 0.04),
               arrow = arrow(length = unit(0.03, "npc"))) + 
  geom_point(colour = red, shape = 19) +
  geom_point(aes(x = stunting2014), colour = blue, shape = 19) +
  llamar::theme_ygrid() +
  theme(axis.title.y = element_blank(),
        title = element_text(size = 13)) +
  ggtitle('Stunting rates dropped considerably but unequally across Rwanda between 2010 and 2014/2015') +
  geom_text(aes(x = 0.36), family = 'Segoe UI Semilight',
            size = 7,
            label = '2014/2015', colour = blue, hjust = 0.5,
            data = df %>% filter(region == 'North')) + 
    geom_text(aes(x = 0.54), family = 'Segoe UI Semilight',
    label = '2010', colour = red, hjust = 0.5,
    size = 7,
            data = df %>% filter(region == 'North')) + 
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.55)) +
  coord_flip()


ggsave("~/Documents/USAID/Rwanda/DHSgraphs/stunting.pdf",
       width = 6, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# choropleth --------------------------------------------------------------
df2010 = choroplethrAdmin1::get_admin1_regions(country.name = "rwanda")

df2014 = choroplethrAdmin1::get_admin1_regions(country.name = "rwanda")

df2010 = full_join(df2010, df, by = c("region" = "choroName")) %>% 
  select(country, region, value = stunting2010)

df2014 = full_join(df2014, df, by = c("region" = "choroName")) %>% 
  select(country, region, value = stunting2014)

admin1_choropleth(df = df2014, country.name = 'rwanda', num_colors = 1) +
  scale_fill_gradientn(colours = brewer.pal(9, 'Blues'),
                       limits = c(0.1, 0.55), 
                       breaks = c(0.1, 0.3, 0.5),
                       name = 'stunting',
                       labels = scales::percent) + 
  ggtitle('2014/2015') +
  theme_legend() +
  geom_text(aes(label = percent(value)),
            colour = 'white', family = 'Segoe UI Semilight', size = 6) + 
  theme(title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(ticks = FALSE))

ggsave("~/Documents/USAID/Rwanda/DHSgraphs/stunting2014.pdf",
       width = 3, height = 2.25,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

admin1_choropleth(df = df2010, country.name = 'rwanda', num_colors = 1) +
  scale_fill_gradientn(colours = brewer.pal(9, 'Blues'),
                       limits = c(0.1, 0.55), 
                       breaks = c(0.1, 0.3, 0.5),
                       name = 'stunting',
                       labels = scales::percent) +
  geom_text(aes(label = percent(value)),
            colour = 'white', family = 'Segoe UI Semilight', size = 6) + 
  
  ggtitle('2010') + 
  theme_legend() +
  theme(title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(ticks = FALSE))

ggsave("~/Documents/USAID/Rwanda/DHSgraphs/stunting2010.pdf",
       width = 3, height = 2.25,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

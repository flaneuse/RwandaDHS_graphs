# Overview ----------------------------------------------------------------
# This script imports high-level indicators from the 2014/2015 Rwanda DHS
# Key indicators preliminary report and compares them to other relevant stats,
# including previous DHS and WHO MDG indicators.

# In all of these figures, none of the data have been verified; they are all
# lifted directly from reports, since the most recent DHS data have not been 
# released.  As a result, the numbers are subject to any errors contained in
# the reports.

# Laura Hughes • lhughes@usaid.gov • GeoCenter, USAID • 1 March 2016



# Load packages and set things up -----------------------------------------

library(llamar)

loadPkgs()


# Import data -------------------------------------------------------------

url = 'https://docs.google.com/a/usaid.gov/spreadsheets/d/1TTn_fgkWZdEu1P4VALBlJN6AJ2zcBImcr-FBmgKcnRA/pub?output=csv'
# Note: sheet must be published to the web for connection to work.

DHSws = url %>% gs_url(visibility = 'private')


# Childhood mortality -----------------------------------------------------

childMort = DHSws %>% 
  gs_read(ws = 9) %>% 
  mutate(DHS_infantRatio = DHS_infant / DHS_under5,
         WHO_infantRatio = WHO_infant / WHO_under5,
         WHO_AfrRatio = WHO_Afr_infant / WHO_Afr_under5)


# Under 5 mortality -------------------------------------------------------

colourAfr = grey40K
colourDHS = '#045a8d'
colourWHO = '#74a9cf'
colourTarget = '#fdd0a2'
widthLine = 1.25
sizeDot = 4


ggplot(childMort) +
  # --- MDG target ---
  geom_hline(yintercept = childMort$WHO_target, 
             colour = colourTarget, size = 0.5) +
  annotate(geom = 'text', x = 2000, y = 58, hjust = 0,
           family = 'Segoe UI Light', colour = '#fd8d3c',
           size = 4.5, 
           label = 'MDG target (51 deaths per 1000 live births)') +
  
  # --- Comparison to Africa-wide stats ---
  geom_line(aes(x = WHO_Africa_year, y = WHO_Afr_under5, group = 3),
            colour = colourAfr, size = widthLine * 0.8) +
  geom_point(aes(x = WHO_Africa_year, y = WHO_Afr_under5, group = 3),
             data = childMort %>% filter(WHO_Africa_year == 2015),
             colour = colourAfr, size = sizeDot) +
  geom_text(aes(x = WHO_Africa_year, y = WHO_Afr_under5, group = 3, label = 'all Africa'),
            data = childMort %>% filter(WHO_Africa_year == 2015),
            colour = colourAfr, size = 5,
            family = 'Segoe UI Light', hjust = 0, 
            nudge_x = 0.5) +
  geom_text(aes(x = WHO_Africa_year, y = WHO_Afr_under5, group = 3, label = round(WHO_Afr_under5, 0)),
            data = childMort %>% filter(WHO_Africa_year == 2015),
            colour = colourAfr, size = 5,
            family = 'Segoe UI Light', hjust = 0.5, 
            nudge_y = 11) +
  geom_text(aes(x = WHO_Africa_year, y = WHO_Afr_under5, group = 3, label = round(WHO_Afr_under5, 0)),
            data = childMort %>% filter(WHO_Africa_year == 2000),
            colour = colourAfr, size = 5,
            family = 'Segoe UI Light', hjust = 0.5, 
            nudge_y = 11) +
  
  # --- DHS data ---
  geom_line(aes(x = DHS_year, y = DHS_under5, group = 1),
            colour = colourDHS, size = widthLine) +
  geom_point(aes(x = DHS_year, y = DHS_under5, group = 1),
             data = childMort %>% filter(DHS_year == 2015),
             colour = colourDHS, size = sizeDot) +
  geom_text(aes(x = DHS_year, y = DHS_under5, group = 3, label = 'Rwanda (DHS)'),
            data = childMort %>% filter(DHS_year == 2015),
            colour = colourDHS, size = 5,
            family = 'Segoe UI Light', hjust = 0,
            nudge_x = 0.5) +
  geom_text(aes(x = DHS_year, y = DHS_under5, group = 3, label = DHS_under5),
            data = childMort %>% filter(DHS_year == 2015),
            colour = colourDHS, size = 5,
            family = 'Segoe UI Light', hjust = 0.5,
            nudge_y = 10) +
  geom_text(aes(x = DHS_year, y = DHS_under5, group = 3, label = DHS_under5),
            data = childMort %>% filter(DHS_year == 2000),
            colour = colourDHS, size = 5,
            family = 'Segoe UI Light', hjust = 0.5,
            nudge_y = 10) +
  
  # --- WHO MDG indcators ---
  geom_line(aes(x = WHO_year, y = WHO_under5, group = 2),
            colour = colourWHO, size = widthLine) +
  geom_point(aes(x = WHO_year, y = WHO_under5, group = 2),
             data = childMort %>% filter(WHO_year == 2015), 
             colour = colourWHO, size = sizeDot) +
  geom_text(aes(x = WHO_year, y = WHO_under5, group = 3, label = 'Rwanda (WHO)'),
            data = childMort %>% filter(WHO_year == 2015),
            colour = colourWHO, size = 5,
            family = 'Segoe UI Light', hjust = 0,
            nudge_x = 0.5) +
  
  
  # --- Titles ---
  ggtitle('Rwanda was one of the few countries to meet the Millennium Development goal on under 5 child mortality') +
  # alt: Rwanda has made substantial gains in reducing under 5 child mortality in the past decade
  annotate(geom = 'text', x = 2000, y = 225, hjust = 0,
           family = 'Segoe UI Light', colour = grey60K,
           size = 5, 
           label = 'probability of dying by age 5 (per 1000 live births)') +
  
  # --- Themes ---
  theme_ygrid() +
  theme(axis.title = element_blank(),
        title = element_text(size = 13, hjust = 0)) +
  scale_x_continuous(limits = c(2000, 2017), breaks = seq(2000, 2017, by = 5)) +
  scale_y_continuous(limits = c(0, 225))



ggsave("~/Documents/USAID/Rwanda/DHSgraphs/under5.pdf",
       width = 9, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# Comparison: under 5 mortality and infant mort ---------------------------
colourInf = '#fd8d3c'
colourInfLight = '#fdd0a2'
colourDHSlight = '#74a9cf'

ggplot(childMort) +


  
  # --- DHS data ---
  geom_line(aes(x = DHS_year, y = DHS_infant, group = 1),
            colour = colourInf, size = widthLine) +
  geom_point(aes(x = DHS_year, y = DHS_infant, group = 1),
             data = childMort %>% filter(DHS_year == 2015),
             colour = colourInf, size = sizeDot) +
  geom_text(aes(x = DHS_year, y = DHS_infant, group = 3, label = 'infant mortality'),
            data = childMort %>% filter(DHS_year == 2015),
            colour = colourInf, size = 5,
            family = 'Segoe UI Light', hjust = 0,
            nudge_x = 0.5) +
  geom_text(aes(x = DHS_year, y = DHS_infant, group = 3, label = DHS_infant),
            data = childMort %>% filter(DHS_year == 2015),
            colour = colourInf, size = 5,
            family = 'Segoe UI Light', hjust = 0.5,
            nudge_y = 10) +
  geom_text(aes(x = DHS_year, y = DHS_infant, group = 3, label = DHS_infant),
            data = childMort %>% filter(DHS_year == 2000),
            colour = colourInf, size = 5,
            family = 'Segoe UI Light', hjust = 0.5,
            nudge_y = 10) +

  # --- DHS data ---
  geom_line(aes(x = DHS_year, y = DHS_under5, group = 1),
            colour = colourDHS, size = widthLine) +
  geom_point(aes(x = DHS_year, y = DHS_under5, group = 1),
             data = childMort %>% filter(DHS_year == 2015),
             colour = colourDHS, size = sizeDot) +
  geom_text(aes(x = DHS_year, y = DHS_under5, group = 3, label = 'under 5 mortality'),
            data = childMort %>% filter(DHS_year == 2015),
            colour = colourDHS, size = 5,
            family = 'Segoe UI Light', hjust = 0,
            nudge_x = 0.5) +
  geom_text(aes(x = DHS_year, y = DHS_under5, group = 3, label = DHS_under5),
            data = childMort %>% filter(DHS_year == 2015),
            colour = colourDHS, size = 5,
            family = 'Segoe UI Light', hjust = 0.5,
            nudge_y = 10) +
  geom_text(aes(x = DHS_year, y = DHS_under5, group = 3, label = DHS_under5),
            data = childMort %>% filter(DHS_year == 2000),
            colour = colourDHS, size = 5,
            family = 'Segoe UI Light', hjust = 0.5,
            nudge_y = 10) +
  
  # --- Change lines --
  
  geom_segment(aes(x = 2000, xend = 2000,
                   y = childMort$DHS_under5[1], yend = childMort$DHS_under5[5]),
               colour = colourDHSlight, size = 0.2) +
  
  geom_segment(aes(x = 2000, xend = 2000,
                   y = childMort$DHS_infant[1], yend = childMort$DHS_infant[5]),
               colour = colourInfLight, size = 0.2) +
  
  geom_point(aes(x = 2000, xend = 2000,
                 y = childMort$DHS_under5[5]),
             colour = colourDHSlight, size = 2) +
  
  geom_point(aes(x = 2000, xend = 2000,
                 y = childMort$DHS_under5[1]),
             colour = colourDHSlight, size = 2) +
  
  geom_point(aes(x = 2000, xend = 2000,
                 y = childMort$DHS_infant[5]),
             colour = colourInfLight, size = 2) +
  
  geom_point(aes(x = 2000, xend = 2000,
                 y = childMort$DHS_infant[1]),
             colour = colourInfLight, size = 2) +
  
  # --- Titles ---
  ggtitle('Under 5 child mortality has declined more than infant mortality') +
  # alt: Rwanda has made substantial gains in reducing under 5 child mortality in the past decade
  annotate(geom = 'text', x = 2000, y = 225, hjust = 0,
           family = 'Segoe UI Light', colour = grey60K,
           size = 5, 
           label = 'probability of dying (per 1000 live births)') +
  
  # --- Themes ---
  theme_ygrid() +
  theme(axis.title = element_blank()) +
  scale_x_continuous(limits = c(2000, 2017), breaks = seq(2000, 2017, by = 5)) +
  scale_y_continuous(limits = c(0, 225), expand = c(0, 10))


ggsave("~/Documents/USAID/Rwanda/DHSgraphs/infantMort.pdf",
       width = 9, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# Infant proportion -------------------------------------------------------
ggplot(childMort) +
  # # --- Comparison to Africa-wide stats ---
  # geom_line(aes(x = WHO_Africa_year, y = WHO_AfrRatio, group = 3),
  #           colour = colourAfr, size = widthLine * 0.8) +
  # geom_point(aes(x = WHO_Africa_year, y = WHO_AfrRatio, group = 3),
  #            data = childMort %>% filter(WHO_Africa_year == 2015),
  #            colour = colourAfr, size = sizeDot) +
  # geom_text(aes(x = WHO_Africa_year, y = WHO_AfrRatio, group = 3, label = 'all Africa'),
  #           data = childMort %>% filter(WHO_Africa_year == 2015),
  #           colour = colourAfr, size = 5,
  #           family = 'Segoe UI Light', hjust = 0, 
  #           nudge_x = 0.5) +
# geom_text(aes(x = WHO_Africa_year, y = WHO_AfrRatio, group = 3, label = percent(WHO_AfrRatio, 0)),
#           data = childMort %>% filter(WHO_Africa_year == 2015),
#           colour = colourAfr, size = 5,
#           family = 'Segoe UI Light', hjust = 0.5, 
#           nudge_y = 0.02) +

# --- DHS data ---
geom_line(aes(x = DHS_year, y = DHS_infantRatio, group = 1),
          colour = colourDHS, size = widthLine) +
  geom_point(aes(x = DHS_year, y = DHS_infantRatio, group = 1),
             data = childMort %>% filter(DHS_year == 2015),
             colour = colourDHS, size = sizeDot) +
  geom_text(aes(x = DHS_year, y = DHS_infantRatio, group = 3, label = '(DHS)'),
            data = childMort %>% filter(DHS_year == 2015),
            colour = colourDHS, size = 5,
            family = 'Segoe UI Light', hjust = 0,
            nudge_x = 0.5) +
  geom_text(aes(x = DHS_year, y = DHS_infantRatio, group = 3, label = percent(DHS_infantRatio,0)),
            data = childMort %>% filter(DHS_year == 2015),
            colour = colourDHS, size = 5,
            family = 'Segoe UI Light', hjust = 0.5,
            nudge_y = 0.04) +
  
  # --- WHO MDG indcators ---
  geom_line(aes(x = WHO_year, y = WHO_infantRatio, group = 2),
            colour = colourWHO, size = widthLine) +
  geom_point(aes(x = WHO_year, y = WHO_infantRatio, group = 2),
             data = childMort %>% filter(WHO_year == 2015), 
             colour = colourWHO, size = sizeDot) +
  geom_text(aes(x = WHO_year, y = WHO_infantRatio, group = 3, label = '(WHO)'),
            data = childMort %>% filter(WHO_year == 2015),
            colour = colourWHO, size = 5,
            family = 'Segoe UI Light', hjust = 0,
            nudge_x = 0.5) +
  geom_text(aes(x = WHO_year, y = WHO_infantRatio, group = 3, label = percent(WHO_infantRatio,0)),
            data = childMort %>% filter(WHO_year == 2015),
            colour = colourWHO, size = 5,
            family = 'Segoe UI Light', hjust = 0.5,
            nudge_y = 0.04) +
  
  
  # --- Titles ---
  ggtitle('The highest probability of childhood death in Rwanda occurs in the first year of life') +
  annotate(geom = 'text', x = 2000, y = 1, hjust = 0,
           family = 'Segoe UI Light', colour = grey60K,
           size = 5,
           label = 'proportion of under 5 deaths occurring before first birthday') +
  
  # --- Themes ---
  theme_ygrid() +
  theme(axis.title = element_blank(),
        title = element_text(size = 13, hjust = 0)) +
  scale_x_continuous(limits = c(2000, 2017), breaks = seq(1990, 2017, by = 5)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)



# Anemia (children) -------------------------------------------------------

childAnemia = DHSws %>% 
  gs_read(ws = 4) %>% 
  dplyr::select(cohort, comparison, 
                anyAnemia2010, anyAnemia2014, choroName) %>% 
  mutate(anyAnemia2014 = anyAnemia2014/100,
         anyAnemia2010 = anyAnemia2010/100,
         chg = anyAnemia2014 - anyAnemia2010)


blue = brewer.pal(11, 'RdYlBu')[11]
red = brewer.pal(11, 'RdYlBu')[2]

childAnemia$cohort =
  factor(childAnemia$cohort,
         levels = rev(c('North', 'South', 'Kigali', 'West', 'East',
                        'female', 'male',
                        'urban', 'rural',
                        'highest wealth', 'fourth wealth', 'third wealth', 'second wealth', 'lowest wealth',
                        '48-59', '36-47', '24-35', '12-23',  '6-11')))

childAnemia$comparison = 
  factor(childAnemia$comparison,
         levels = c('age', 'wealth', 'region'),
         labels = c('Anemia is higher in young children',
                    'Anemia incidence is largely wealth-independent',
                    'South and North provinces had higher child anemia in 2014/2015, while the other provinces had slightly lower'))

ggplot(childAnemia %>% filter(!is.na(comparison)),
       aes(y = anyAnemia2010, yend = anyAnemia2014 * 1.018,
                        x = cohort, xend = cohort, size = 4)) +
  geom_segment(colour = grey40K, size = 1.5) +
  geom_segment(colour = grey40K, size = 1.5,
               data = childAnemia %>% filter(!is.na(comparison),
                                             abs(chg) > 0.03),
               arrow = arrow(length = unit(0.03, "npc"))) +
  geom_point(colour = red, shape = 19) +
  geom_point(aes(y = anyAnemia2014), colour = blue, shape = 19) +
  llamar::theme_ygrid() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12),
        title = element_text(size = 13)) +
  ggtitle('Child anemia incidence decreased slightly between 2010 and 2014/2015') +
  geom_text(aes(x = 0.36), family = 'Segoe UI Semilight',
            label = '2014/2015', colour = blue, hjust = 0.5,
            data = childAnemia %>% filter(cohort == 'North')) + 
  geom_text(aes(x = 0.54), family = 'Segoe UI Semilight',
            label = '2010', colour = red, hjust = 0.5,
            data = childAnemia %>% filter(cohort == 'North')) + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7)) +
  facet_wrap(~comparison, scales = 'free_x')


ggsave("~/Documents/USAID/Rwanda/DHSgraphs/kidsAnemia.pdf",
       width = 9, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# choropleth (kids anemia) -----------------------------------------------------
df2010 = choroplethrAdmin1::get_admin1_regions(country.name = "rwanda")
df2014 = choroplethrAdmin1::get_admin1_regions(country.name = "rwanda")


df2010 = full_join(df2010, childAnemia %>% filter(comparison == 'region'), 
                   by = c("region" = "choroName")) %>% 
  dplyr::select(country, region, value = anyAnemia2010)

df2014 = full_join(df2014, childAnemia %>% filter(comparison == 'region'), by = c("region" = "choroName")) %>% 
  dplyr::select(country, region, value = anyAnemia2014)

admin1_choropleth(df = df2014, country.name = 'rwanda', num_colors = 1) +
  scale_fill_gradientn(colours = brewer.pal(9, 'Blues'),
                       limits = c(0.1, 0.450), 
                       breaks = c(0.15, 0.3, 0.45),
                       name = 'anemia in children',
                       labels = scales::percent) + 
  ggtitle('2014/2015') +
  theme_legend() +
  theme(title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(ticks = FALSE))

ggsave("~/Documents/USAID/Rwanda/DHSgraphs/kids2014.pdf",
       width = 5, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

admin1_choropleth(df = df2010, country.name = 'rwanda', num_colors = 1) +
  scale_fill_gradientn(colours = brewer.pal(9, 'Blues'),
                       limits = c(0.1, 0.450), 
                       breaks = c(0.15, 0.3, 0.45),
                       name = 'anemia in children',
                       labels = scales::percent) + 
  # geom_text(aes(label = percent(value))) + 
  ggtitle('2010') + 
  theme_legend() +
  theme(title = element_text(hjust = 0.5, vjust = -10)) +
  guides(fill = guide_colorbar(ticks = FALSE))

ggsave("~/Documents/USAID/Rwanda/DHSgraphs/kids2010.pdf",
       width = 5, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# Anemia (women) -------------------------------------------------------

womenAnemia = DHSws %>% 
  gs_read(ws = 5) %>% 
  dplyr::select(cohort, comparison, 
                womenAnemia2010, womenAnemia2014, choroName) %>% 
  mutate(womenAnemia2014 = womenAnemia2014/100,
         womenAnemia2010 = womenAnemia2010/100,
         chg = womenAnemia2014 - womenAnemia2010)



womenAnemia$cohort =
  factor(womenAnemia$cohort,
         levels = rev(c('North', 'West', 'South', 'Kigali','East',
                        'urban', 'rural',
                        'highest quintile', 'fourth quintile', 'middle quintile', 'second quintile', 'lowest quintile')))


womenAnemia$comparison = 
  factor(womenAnemia$comparison, 
         levels = c('wealth', 'rural', 'region'), 
         labels = c('Anemia increased across wealth but especially for the poorest',
                    'Higher levels of anemia was measured in rural women',
                    'Though most provinces had more anemia in 2014/2015, Kigali and the East had lower'))

ggplot(womenAnemia, aes(y = womenAnemia2010, yend = womenAnemia2014*0.98,
                        x = cohort, xend = cohort, size = 4)) +
  geom_segment(colour = grey40K, size = 1.5) + 
  geom_segment(colour = grey40K, size = 1.5,
               data = womenAnemia %>% filter(abs(chg) > 0.02),
               arrow = arrow(length = unit(0.03, "npc"))) + 
  geom_point(colour = red, shape = 19) +
  geom_point(aes(y = womenAnemia2014), colour = blue, shape = 19) +
  llamar::theme_ygrid() +
  theme(axis.title = element_blank(),
        strip.text = element_text(family = 'Segoe UI Light', colour = grey70K, size = 11),
        title = element_text(size = 13)) +
  ggtitle('Anemia prevalenence in women 15-49 years old increased between 2010 and 2014/2015') +
  geom_text(aes(y= 0.27), family = 'Segoe UI Semilight',
            label = '2014/2015', colour = blue, hjust = 0,
            size = 7,
            data = womenAnemia %>% filter(cohort == 'lowest quintile')) + 
  geom_text(aes(y = 0.17), family = 'Segoe UI Semilight',
            label = '2010', colour = red, hjust = 0.5,
            size = 7, 
            data = womenAnemia %>% filter(cohort == 'lowest quintile')) + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.3)) +
  facet_wrap(~comparison, scales = 'free_x')


ggsave("~/Documents/USAID/Rwanda/DHSgraphs/womenAnemia.pdf",
       width = 9, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# choropleth (women's anemia) -----------------------------------------------------
df2010 = choroplethrAdmin1::get_admin1_regions(country.name = "rwanda")
df2014 = choroplethrAdmin1::get_admin1_regions(country.name = "rwanda")


df2010 = full_join(df2010, womenAnemia %>% filter(comparison == 'Though most provinces had more anemia in 2014/2015, Kigali and the East had lower'), 
                   by = c("region" = "choroName")) %>% 
  dplyr::select(country, region, value = womenAnemia2010)

df2014 = full_join(df2014, womenAnemia %>% filter(comparison == 'Though most provinces had more anemia in 2014/2015, Kigali and the East had lower'), by = c("region" = "choroName")) %>% 
  dplyr::select(country, region, value = womenAnemia2014)

admin1_choropleth(df = df2014, country.name = 'rwanda', num_colors = 1) +
  scale_fill_gradientn(colours = brewer.pal(9, 'Blues'),
                       limits = c(0.1, 0.45), 
                       breaks = c(0.15, 0.3, 0.45),
                       name = 'anemia in women',
                       labels = scales::percent) + 
  ggtitle('2014/2015') +
  theme_legend() +
  theme(title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(ticks = FALSE))

ggsave("~/Documents/USAID/Rwanda/DHSgraphs/women2014.pdf",
       width = 5, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

admin1_choropleth(df = df2010, country.name = 'rwanda', num_colors = 1) +
  scale_fill_gradientn(colours = brewer.pal(9, 'Blues'),
                       limits = c(0.1, 0.45), 
                       breaks = c(0.15, 0.3, 0.45),
                       name = 'anemia in women',
                       labels = scales::percent) + 
  # geom_text(aes(label = percent(value))) + 
  ggtitle('2010') + 
  theme_legend() +
  theme(title = element_text(hjust = 0.5, vjust = -10)) +
  guides(fill = guide_colorbar(ticks = FALSE))


ggsave("~/Documents/USAID/Rwanda/DHSgraphs/women2010.pdf",
       width = 5, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# breastfeeding -----------------------------------------------------------

colour2015 = brewer.pal(9, 'Blues')[7]
colour2010 = '#fd8d3c'

breast = DHSws %>% 
  gs_read(ws = 2) %>% 
  dplyr::select(age, year, contains('excl'), contains('Compl')) %>% 
  mutate(`exclusive + compl` = exclusive + complementary)

breast$age = factor(breast$age,
                    levels = c('0-1', '2-3', '4-5', '6-8', '9-11', '12-17', '18-23'))

breast = breast %>% 
  filter(!is.na(age)) %>% 
  gather(feeding, pct, -age, -year) %>% 
  mutate(pct = pct/100)

breast$feeding = factor(breast$feeding,
                        levels = c('exclusive', 'complementary', 'exclusive + compl'),
                        labels = c('Exclusive breastfeeding is high in babies < 6 mo. old',
                                   'Breastfeeding with complementary foods is high in babies > 9 mo. old', 
                                   'Healthy practices are lowest in babies 6-8 mo. old'))


ggplot(breast, aes(x = age, y = pct, colour = factor(year),
                   fill = factor(year), group = year)) + 
  geom_area(alpha = 0.2, position = 'dodge') +
  geom_line() +
  scale_fill_manual(values = c('2015' = colour2015, '2010' = colour2010)) + 
  scale_colour_manual(values = c('2015' = colour2015, '2010' = colour2010)) + 
  facet_wrap(~feeding) +
  geom_text(aes(label = year),
            family = 'Segoe UI Semilight',
            size = 5,
            hjust = 0, 
            nudge_x = 0.2,
            data = breast %>% filter(age == '4-5',
                                     str_detect(feeding, 'xclusive'))) + 
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  ggtitle('Breastfeeding rates are virtually unchanged, though high, between 2010 and 2015') +
  xlab('age (months)') +
  theme_ygrid() +
  theme(title = element_text(hjust = 0),
        axis.title.x = element_text(family = 'Segoe UI Semilight', size = 14, colour = grey60K, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10.5), 
        strip.text = element_text(family = 'Segoe UI Light', colour = grey70K, size = 11))


ggsave("~/Documents/USAID/Rwanda/DHSgraphs/breastfeeding.pdf",
       width = 9.5, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)




# minimal diet ------------------------------------------------------------
mad = DHSws %>% 
  gs_read(ws = 3) %>% 
  dplyr::select(age, frac2014) %>% 
  filter(age != 'total')

mad$age = factor(mad$age, levels = 
                   c('6-8', '9-11', '12-17', '18-23'))

ggplot(mad, aes(x = age, y = frac2014)) +
  geom_bar(fill = 'dodgerblue', stat = 'identity') +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.3)) +
  theme_ygrid() +
  geom_text(aes(label = percent(frac2014, 0)), colour = 'white', 
            size = 8, family = 'Segoe UI', nudge_y = -0.015) +
  ggtitle('Percent of children with a minimum acceptable diet is low but increases with age') +
  xlab('age (months)') + 
  theme(title = element_text(hjust = 0),
        axis.title.x = element_text(family = 'Segoe UI Semilight', size = 14, colour = grey60K, hjust = 1),
        axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        strip.text = element_text(family = 'Segoe UI Light', colour = grey70K, size = 11))


ggsave("~/Documents/USAID/Rwanda/DHSgraphs/MAD.pdf",
       width = 9, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

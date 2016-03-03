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

loadPkgs()

# Data-ish ----------------------------------------------------------------

# Table 3 DHS, 2014; Tbale 5.1, 2010
fertility = data_frame(`2014` = c(41, 143, 185, 185, 107, 52, 1, 
                                  46, 190, 220, 187, 139, 67, 13),
                       `2010` = c(40, 143, 180, 137, 113, 58, 16,
                                  41, 198, 235, 211, 153, 92, 21),
                       age = rep(c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49'),2),
                       rural = c(rep('urban', 7), rep('rural', 7))) %>% 
  mutate(diff = `2014` - `2010`)

fertility = fertility %>% 
  gather(year, fertility, -age, -rural, -diff)


# fertility rate ----------------------------------------------------------
blue = brewer.pal(11, 'RdYlBu')[11]
red = brewer.pal(11, 'RdYlBu')[2]

ggplot(fertility, aes(y = fertility, x = age, group = year, colour = factor(year))) +
  geom_point() +
  geom_line() + 
  facet_wrap(~ rural) +
  scale_colour_manual(values = c('2010' = red, '2014' = blue)) +
  xlab('age of woman') +
  annotate(label = 'fertility rate (per 1000 women)',
           geom = 'text',
           size = 5,
           x = 1, y = 250, hjust = 0, family = 'Segoe UI Light', colour = grey75K) + 
  geom_text(aes(label = year),
            size = 5, family = 'Segoe UI Light',
            data = fertility %>% filter(rural == 'rural', 
                                        age =='40-44')) +
  geom_text(aes(label = fertility),
            nudge_x = 0.1, 
            nudge_y = 10,
            size = 5, family = 'Segoe UI Light',
            data = fertility %>% filter(rural == 'urban', 
                                        age =='30-34')) +
  geom_text(aes(label = 'Urban women aged 30-34 have higher fertility rates in 2014/2015'),
            size = 5, family = 'Segoe UI Light',
            data = fertility %>% filter(rural == 'urban',
                                        year == 2014,
                                        age =='30-34')) +
  ggtitle('While fertility for rural women has decreased, it has increased for urban women') +
  theme_ygrid() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14))

ggsave("~/Documents/USAID/Rwanda/DHSgraphs/fertility.pdf",
       width = 9, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

overallFert = data_frame(
  urban = c(4.9, 4.7, 3.4, 3.6),
  rural = c(6.3, 5.7, 4.8, 4.3),
  total = c(6.1, 5.5, 4.6, 4.2),
  year = c(2005, 2008, 2010, 2015)
) %>% 
  gather(grp, fert, -year)

ggplot(overallFert, aes(x = year, y = fert, 
                        group = grp,
                        colour = grp, 
                        label = fert)) +
  geom_line() +
  geom_point(size = 15, colour = 'white') +
  geom_text(family = 'Segoe UI Semilight', size = 7) +
  geom_text(aes(label = grp), 
                family = 'Segoe UI Semilight', size = 7, 
            nudge_x = 1, 
            data = overallFert %>% filter(year == 2015)
            ) +
  scale_y_continuous(limits = c(0, 6.5)) +
  scale_x_continuous(breaks = c(2005, 2008, 2010, 2015),
                     labels = c('2005', '2007/2008', '2010', '2014/2015'))+
  theme_labelsOnly() +
  ggtitle('Total fertility rate is plateauing') +
  theme(axis.text.y = element_blank(),
        axis.text = element_text(size = 18))


ggsave("~/Documents/USAID/Rwanda/DHSgraphs/tfr.pdf",
       width = 5, height = 8,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# Fertility desires -------------------------------------------------------
# Table 5, 2014; Table 6.1, 2010

fertPref2014 = data_frame(
  `0` = c(89, 3.7, 0, 0.8, 0.7, 0.6, 5.2, 0),
  `1` = c(17.9, 78, 0.4, 0.3, 2.7, 0.2, 0.4, 0.1),
  `2` = c(10.3, 64.1, 0.3, 1.3, 22.9, 0.5, 0.6, 0.1),
  `3` = c(6.2, 37.1, 0.1, 2.3, 52.4, 1.4, 0.5, 0.1),
  `4` = c(4.6, 17, 0.1, 2, 73.3, 1.9, 0.9, 0.2), 
  `5` = c(1.8, 10.7, 0, 0.8, 82.5, 3.1, 0.4, 0.6),
  `6+` = c(1.2, 4.8, 0.1, 1.1, 88.7, 3.1, 0.9, 0.2),
  desire = c('wants within 2 years', 'wants > 2 years',
             'wants another, not sure when', 'undecided',
             'no more', 'sterilized', 'infecund', 'missing')
) 

fertPref2014 %>% 
  mutate_each(funs(sum), vars = c(`0`, `1`, `2`, `3`, `4`, `5`, `6+`))

fertPref2014 = fertPref2014 %>% 
  gather(numKids, pct, -desire)

fertPref2010  = data_frame(
  `0` = c(88.5, 5.1, 0.5, 0, 1.4, 0, 4.1, 0.5),
  `1` = c(15, 79.5, 0.8, 0.8, 3.4, 0.1, 0.4, 0.1),
  `2` = c(8.1, 61.7, 1, 1.6, 26.9, 0.1, 0.5, 0.1),
  `3` = c(4.6, 35, 0.4, 2.6, 56.2,1, 0.3, 0),
  `4` = c(2.5, 15.9, 0.5, 2.3, 76, 1.6, 1, 0.3), 
  `5` = c(1.1, 8.3, 0, 1.4, 86.1, 2.3, 0.8, 0),
  `6+` = c(0.5, 3, 0.3, 1.5, 91.7, 0.9, 1.7, 0.4),
  desire = c('wants within 2 years', 'wants > 2 years',
             'wants another, not sure when', 'undecided',
             'no more', 'sterilized', 'infecund', 'missing')
) 

fertPref2010 %>% 
  mutate_each(funs(sum), vars = c(`0`, `1`, `2`, `3`, `4`, `5`, `6+`))

fertPref2010 = fertPref2010 %>% 
  gather(numKids, pct, -desire)

fertPref = full_join(fertPref2010, fertPref2014,
                     by = c('desire', 'numKids')) %>% 
  rename(`2010` = `pct.x`,
         `2014` = `pct.y`) %>% 
  mutate(diff = `2014` - `2010`) %>% 
  gather(year, pct, -desire, -numKids, -diff)

fertPref$desire = factor(fertPref$desire,
                         levels = c('wants within 2 years', 'wants > 2 years'),
                         labels = c('wants another baby within 2 years', 'wants another baby > 2 years'))

# ggplot(fertPref %>% filter(!(desire %in% 
#                                c('infecund', 'wants another, not sure when',
#                                  'missing', 'undecided'))),
#        aes(x = numKids, y = pct,
#            group = year, colour = year)) +
#   geom_point(size = 3) +
#   geom_text(aes(label = year,
#                 x = 1, y = pct * 3),
#             data = fertPref %>% filter(desire == 'no more',
#                                        numKids == '2'),
#             family = 'Segoe UI Light') +
#   theme_xygrid() +
#   facet_wrap(~desire, scales = 'free_y') +
#   xlab('number of currently living children') +
#   ylab('percent') +
#   ggtitle('Change in preference for more children')

ggplot(fertPref %>% filter(year == 2010,
                           (!is.na(desire))),
       aes(x = numKids, y = diff)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue') +
  theme_ygrid() +
  facet_wrap(~desire) +
  theme(axis.title.y = element_blank(),
        axis.title = element_text(size = 13)) +
  ggtitle('Women in 2014 report stronger preferences for having more children') +
  xlab('number of currently living children') +
  geom_text(aes(x = 0, y = 3.2,label = 'change (percent 2014 - percent 2010)'),
            data = fertPref %>% filter(desire == 'wants another baby within 2 years'),
            hjust = 0,
            colour = grey50K, family = 'Segoe UI Light' 
  )


ggsave("~/Documents/USAID/Rwanda/DHSgraphs/wantsBaby.pdf",
       width = 9, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# Contraception -----------------------------------------------------------
# Any modern method, married women
# Table 6, 2014
contraception= data.frame(
  `2014` = c(32.8, 44.3, 50.9, 51.1, 51, 46.6, 29.5)/100,
  `2010` = c(30.6, 42.1, 49.8, 50.2, 51.8, 42.1, 21.4)/100,
  age = c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49')
) %>% 
  gather(year, pct, -age) %>% 
  mutate(year = ifelse(year == 'X2010', '2010',
                       ifelse(year == 'X2014', '2014/2015', NA)))

contraception2 = data.frame(
  `2014` = c(32.8, 44.3, 50.9, 51.1, 51, 46.6, 29.5)/100,
  `2010` = c(30.6, 42.1, 49.8, 50.2, 51.8, 42.1, 21.4)/100,
  age = c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49')
)

contraception2$age = factor(contraception2$age, 
                            rev(c('45-49', '15-19', '20-24', '40-44', '25-29', '35-39', '30-34')
                            ))

ggplot(contraception, 
       aes(x = age, y = pct, colour = year, group = year)) +
  geom_point(size = 5) +
  geom_text(aes(label = year),
            hjust = 0,
            nudge_x = 0.1,
            family = 'Segoe UI Light',
            size = 6,
            data = contraception %>% filter(age == '45-49')) +
  theme_ygrid() +
  theme(axis.title.y = element_blank()) +
  xlab('years old') +
  scale_y_continuous(limits = c(0, 0.55), 
                     breaks = seq(0, 0.5, by = 0.1),
                     labels = scales::percent) +
  scale_colour_manual(values = c('2010' = red, '2014/2015' = blue)) +
  ggtitle('Percent of married women using modern contraception is highest in late 20s - late 30s')

ggsave("~/Documents/USAID/Rwanda/DHSgraphs/contraception_age.pdf",
       width = 9, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

ggplot(contraception2, aes(x = 2010, xend = 2014, 
                           colour = age,
                           y = `X2010`, yend = `X2014`)) +
  geom_segment() +
  scale_x_continuous(breaks = c(2010, 2014)) +
  geom_point(size = 4, shape = 16) +
  geom_point(aes(x = 2014, y = `X2014`, colour = age), size = 4) +
  geom_text(aes(label = age, x = 2014, y = `X2014`, colour = age),
            nudge_x = 0.3, family = 'Segoe UI Light',
            size = 6.5,
            check_overlap = TRUE) +
  geom_text(aes(label = age, x = 2014, y = `X2014`, colour = age),
            size = 6.5,
            nudge_x = 0.3, nudge_y = 0.03, family = 'Segoe UI Light',
            data = contraception2 %>% filter(age == '30-34'),
            check_overlap = TRUE) +
  geom_text(aes(label = age, x = 2014, y = `X2014`, colour = age),
            size = 6.5,
            nudge_x = 0.3, nudge_y = 0.015, family = 'Segoe UI Light',
            data = contraception2 %>% filter(age == '35-39')) +
  geom_text(aes(label = percent(`X2010`, 0), colour = age),
            size = 5, nudge_y = 0.02,
            family = 'Segoe UI Light',
            data = contraception2 %>% filter(age == '45-49')) +
  geom_text(aes(label = percent(`X2014`, 0), x = 2014, y = `X2014`, colour = age),
            size = 5, nudge_y = 0.015,
            family = 'Segoe UI Light',
            data = contraception2 %>% filter(age == '45-49')) +
  geom_text(aes(label = 'Married women aged 45-49 increased their use of contraception', x = 2014, y = `X2014`, colour = age),
            size = 5, nudge_y = -0.07,
            family = 'Segoe UI Light',
            data = contraception2 %>% filter(age == '45-49')) +
  theme_ygrid() +
  scale_y_continuous(labels = scales::percent) +
  theme(title = element_text(size = 18),
        axis.title.y = element_blank()) +
  ggtitle('Percent of married women using modern contraception remained similar between 2010 and 2014/2015')


ggsave("~/Documents/USAID/Rwanda/DHSgraphs/contraception_time.pdf",
       width = 9, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)
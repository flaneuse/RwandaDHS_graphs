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

ggplot(fertility, aes(y = fertility, x = age, group = year, colour = year)) +
  geom_point() +
  geom_line() + 
  facet_wrap(~ rural) +
  xlab('age of woman') +
  ylab('fertility rate (per 1000 women)') + 
  theme_basic()

overallFert = data_frame(
  urban = c(3.4, 3.6),
  rural = c(4.8, 4.3),
  year = c(2010, 2014)
)

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

ggplot(fertPref %>% filter(!(desire %in% 
                               c('infecund', 'wants another, not sure when',
                                 'missing', 'undecided'))),
       aes(x = numKids, y = pct,
           group = year, colour = year)) +
  geom_point(size = 3) +
  geom_text(aes(label = year,
                x = 1, y = pct * 3),
            data = fertPref %>% filter(desire == 'no more',
                                       numKids == '2'),
            family = 'Segoe UI Light') +
  theme_xygrid() +
  facet_wrap(~desire, scales = 'free_y') +
  xlab('number of currently living children') +
  ylab('percent') +
  ggtitle('Change in preference for more children')

ggplot(fertPref %>% filter(year == 2010,
  !(desire %in% 
                               c('infecund', 'wants another, not sure when',
                                 'missing', 'undecided'))),
       aes(x = numKids, y = diff)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue') +
  theme_xygrid() +
  facet_wrap(~desire, scales = 'free_y') +
  theme(axis.title.y = element_blank(),
        title = element_text(size = 11)) +
  ggtitle('Women in 2014 report stronger preference for having more children') +
  xlab('number of currently living children') +
  geom_text(aes(x = 0, y = 2.2,label = 'change (percent 2014 - percent 2010)'),
            data = fertPref %>% filter(desire == 'no more'),
           hjust = 0,
           colour = grey50K, family = 'Segoe UI Light' 
           )


# Contraception -----------------------------------------------------------
# Any modern method, married women
# Table 6, 2014
contraception= data.frame(
  `2014` = c(32.8, 44.3, 50.9, 51.1, 51, 46.6, 29.5),
  `2010` = c(30.6, 42.1, 49.8, 50.2, 51.8, 42.1, 21.4),
  age = c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49')
) %>% 
  gather(year, pct, -age)

contraception2 = data.frame(
  `2014` = c(32.8, 44.3, 50.9, 51.1, 51, 46.6, 29.5),
  `2010` = c(30.6, 42.1, 49.8, 50.2, 51.8, 42.1, 21.4),
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
            data = contraception %>% filter(age == '45-49')) +
  theme_xygrid() +
  ggtitle('Percent of women using modern contraception')

ggplot(contraception2, aes(x = 2010, xend = 2014, 
                           colour = age,
                           y = `X2010`, yend = `X2014`)) +
  geom_segment() +
  scale_x_continuous(breaks = c(2010, 2014)) +
  geom_point(size = 4) +
  geom_point(aes(x = 2014, y = `X2014`, colour = age), size = 4) +
  geom_text(aes(label = age, x = 2014, y = `X2014`, colour = age),
            nudge_x = 0.3, family = 'Segoe UI Light',
            check_overlap = TRUE) +
  geom_text(aes(label = age, x = 2014, y = `X2014`, colour = age),
            nudge_x = 0.3, nudge_y = 3, family = 'Segoe UI Light',
            data = contraception2 %>% filter(age == '30-34'),
            check_overlap = TRUE) +
  geom_text(aes(label = age, x = 2014, y = `X2014`, colour = age),
            nudge_x = 0.3, nudge_y = 1.5, family = 'Segoe UI Light',
            data = contraception2 %>% filter(age == '35-39')) +
  theme_ygrid() +
  theme(title = element_text(size = 10),
        axis.title.y = element_blank()) +
  ggtitle('Percent of sexually active women using modern contraception increased from 2010-2014')

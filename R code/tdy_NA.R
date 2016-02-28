library(llamar)

loadPkgs()

tdy <- read.csv("~/GitHub/Rwanda/Datain/TDY.schedule.csv") %>% 
  mutate(occupied = ifelse(Office == "Mission Closed", 0,
                           ifelse(Office == "", 0, 1)),
         Time2 = lead(Time),
         time = mdy_hm(paste0(Month, " ", Time)),
         Time = as.POSIXct(strptime(Time, '%H:%M')),
         time2 = mdy_hm(paste0(Month, " ", Time2)))

# Correct
# M&E mtg
# # avail seats on shuttle


# total time / day --------------------------------------------------------

tdy %>% 
  group_by(Month) %>% 
  summarise(totTime = sum(occupied) / 2) %>% 
  mutate(pctBusy = ifelse(Month == '2016/02/26', totTime / 4, 
                          totTime / 8))

shuttleSeats = 
  data.frame(Day = c('Wednesday', 'Tuesday', 'Monday',
                     'Wednesday', 'Tuesday', 'Monday'), 
             Time = c('7:45', '8:10', '7:45',
                      '17:30', '17:30', '17:30'), 
             numSeats = c(-1, 3, 0,
                          NA, 0, -4))

ggplot(tdy, aes(x = time, xend = time2,
                y = occupied, yend = occupied,
                colour = type)) +
  geom_segment(size = 3) + 
  facet_wrap(~Month, ncol = 1, scales = 'free_x') + 
  theme_xgrid()

ggplot(tdy, aes(x = time,
                y = occupied, group = Month,
                colour = type)) +
  geom_point(size = 3) + geom_line()+
  facet_wrap(~Month, ncol = 1, scales = 'free_x') +
  theme_xgrid() + coord_cartesian(ylim = c(0.9, 1.1))

tdy$Day = factor(tdy$Day, 
                 rev(c('Monday', 'Tuesday', 'Wednesday', 
                       'Thursday', 'Friday')))

tdy$type = factor(tdy$type, 
                  c('USAID', 'none', 'Portfolio Review',
                    'closed', 'Needs Assessment', 'IP', 'Government'
                    ))

ggplot(tdy, aes(y = Day, x = Time, fill = type)) +
  geom_tile(colour = 'white') +
  theme_labelsOnly() +
  theme(legend.position = 'bottom',
        legend.text = element_text(family = 'Segoe UI Light',
                                   colour = grey60K)) +
  scale_fill_manual(name = "",
    values = c('Government' = '#FE9F37',
                               'IP' = '#72BF5A',
                               'none' = grey15K,
                               'closed' = grey40K,
                               'Needs Assessment' = '#A6CEE3',
                               'Portfolio Review' = '#62A3CB',
                               'USAID' = '#1F78B4')) +
  scale_x_datetime(limits = as.POSIXct(c('2016-02-28 08:00:00', '2016-02-28 18:00:00'))) +
  geom_text(aes(label = 'Mission closed'),
            family = 'Segoe UI Semilight',
            size = 4.5,
            colour = grey75K,
            data = tdy %>% filter(Day == 'Friday', 
                                  Time == '2016-02-28 15:30:00'))

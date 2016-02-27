library(llamar)

loadPkgs()

tdy <- read.csv("~/GitHub/Rwanda/Datain/TDY.schedule.csv") %>% 
  mutate(occupied = ifelse(Office == "Mission Closed", 0,
                           ifelse(Office == "", 0, 1)),
         Time2 = lead(Time),
  time = mdy_hm(paste0(Month, " ", Time)),
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

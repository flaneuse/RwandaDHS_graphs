library(llamar)

loadPkgs()

tdy <- read.delim("~/GitHub/Rwanda/Datain/TDY.schedule.txt") %>% 
  mutate(occupied = ifelse(Office == "", 0, 1))

# Correct
# M&E mtg
# # avail seats on shuttle

shuttleSeats = 
  data.frame(Day = c('Wednesday', 'Tuesday', 'Monday',
                     'Wednesday', 'Tuesday', 'Monday'), 
             Time = c('7:45', '8:10', '7:45',
                      '17:30', '17:30', '17:30'), 
             numSeats = c(-1, 3, 0,
                          NA, 0, -4))

ggplot(tdy, aes(x = Time, y = occupied)) +
  geom_point()

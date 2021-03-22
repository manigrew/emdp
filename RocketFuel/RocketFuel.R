library(dplyr)
library(ggplot2)

read.csv("RocketFuel.csv")



ctrl <- filter(dat, test == 0) # 23524
str(ctrl)

ctrl_no <- filter(dat, test == 1) # 564527
str(ctrl_no)

conv_ctrl <- filter(dat, converted == 1, test == 0) #420
dim(conv_ctrl)

conv_ctrl_no <- filter(dat, converted == 1, test == 1) #14423
str(conv_ctrl_no)

(420*100/23524) *564527 
14423 *100/564527

by_tot_impr <- group_by(dat, tot_impr)
summ <- summarise(by_tot_impr, mconverted = mean(converted))
by_tot_impr
str(summ)
max(summ$tot_impr)

summ %>% 
  mutate(x_bins = cut(tot_impr, breaks = 20))

bin_summ <- cut(summ$tot_impr, seq.int(1, 2100, 10))



library(dlookr)
binni 
esquisse::esquisser(dat)

ggplot(summ) +
  aes(x = tot_impr, y = mconverted) +
  geom_point(size = 2L, colour = "#0c4c8a") +
  geom_smooth(method = 'loess') +
  theme_minimal()


ctrl <- filter(dat, test == 1)
expd <- filter(dat, test == 0)

by_mode_day_ctrl <- group_by(ctrl, mode_impr_day)
summ_ctrl <- summarise(by_mode_day_ctrl, mconverted = mean(converted))

by_mode_day_expd <- group_by(expd, mode_impr_day)
summ_expd <- summarise(by_mode_day_expd, mconverted = mean(converted))


ggplot() +
  geom_point(data = summ_ctrl, aes(x = mode_impr_day, y = mconverted), size = 2L, colour = "#0c4c8a") +
  geom_point(data = summ_expd, aes(x = mode_impr_day, y = mconverted), size = 2L, colour = "#8a4c0c") +
  #geom_smooth(method = 'loess') +
  geom_jitter() +
  theme_minimal()

ggplot() +
  geom_bar(data = summ_ctrl, aes(x = mode_impr_day, y = mconverted), stat = "identity", fill = "red") +
  geom_bar(data = summ_expd, aes(x = mode_impr_day, y = mconverted), stat = "identity", fill = "blue") 
  

dat1 <- filter(dat, mode_impr_hour > 7)
ctrl <- filter(dat1, test == 1)
expd <- filter(dat1, test == 0)

by_mode_hour_ctrl <- group_by(ctrl, mode_impr_hour)
summ_ctrl <- summarise(by_mode_hour_ctrl, mconverted = mean(converted))

by_mode_hour_expd <- group_by(expd, mode_impr_hour)
summ_expd <- summarise(by_mode_hour_expd, mconverted = mean(converted))


ggplot() +
  geom_point(data = summ_ctrl, aes(x = mode_impr_hour, y = mconverted), size = 2L, colour = "red") +
  geom_point(data = summ_expd, aes(x = mode_impr_hour, y = mconverted), size = 2L, colour = "blue") +
  #geom_smooth(method = 'loess') +
  geom_jitter() +
  theme_bw()


ggplot() +
  geom_bar(data = summ_ctrl, aes(x = mode_impr_hour, y = mconverted), stat = "identity",  fill = "red") +
  geom_bar(data = summ_expd, aes(x = mode_impr_hour, y = mconverted), stat = "identity", fill = "blue") +
  scale_colour_manual("", 
                      breaks = c("TempMedia", "TempMax", "TempMin"),
                      values = c("TempMedia"="green", "TempMax"="red", 
                                 "TempMin"="blue")) 
esquisse::esquisser(summ2)


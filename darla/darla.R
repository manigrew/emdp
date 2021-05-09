library(readxl)
library(ggplot2)
library(dplyr)
library(esquisse)
dat <- read_excel("darla.xlsx")
str(dat)
summary(dat)

dat$lcecSiteId <- as.factor(dat$lcecSiteId)
dat$processorId <- as.factor(dat$processorId)
dat$eventTypeEnum <- as.factor(dat$eventTypeEnum)

dat <- filter(dat, processorId != -1)
dat$processorId <- as.factor(as.character(dat$processorId))
summary(dat$processorId)

dat <- dat[,c("id", "lcecSiteId", "processorId", "eventTypeEnum", "scheduledFor", "executedAt", 
             "milliseconds", "DATE", "queue_type" )]

str(dat)

by_site <- group_by(dat, lcecSiteId)
summ <- summarise(by_site, ct = n()) 


summary(summ$ct)
summ %>%
  filter()

summ %>%
  filter(ct > 2000) %>%
  arrange(ct) %>%
  ggplot() +
  geom_col(aes(x = reorder(lcecSiteId, -ct), y = ct)) +
  theme(axis.text.x = element_text(angle = 90)) 
?geom_bar


by_site <- group_by(dat, lcecSiteId)
summ <- summarise(by_site, ct = n()) 

by_event_type <- group_by(dat, eventTypeEnum, queue_type)
summ <- summarise(by_event_type, ct = n())
summ %>%
  #filter(ct > 2000) %>%
  arrange(ct) %>%
  ggplot() +
  geom_col(aes(x = reorder(eventTypeEnum, -ct), y = log10(ct), fill = queue_type)) +
  #facet_wrap(~queue_type) +
  theme(axis.text.x = element_text(angle = 90, size = 11, vjust = 0.7)) 

dat$delay_min <- (as.numeric(dat$executedAt - dat$scheduledFor)) / 60

dat$delay2 <- NULL
summary(dat)
esquisser(dat)
x <- filter(dat, id == 116476086)

dat$DATE <- format(dat$scheduledFor, format = '%Y/%m/%d')
head(dat$DATE)

by_date <- group_by(dat, DATE)
summ <- summarise(by_date, mdelay = mean(delay_min), sdelay = sum(delay_min))

summ %>%
  #filter(ct > 2000) %>%
  arrange(mdelay) %>%
  ggplot(aes(x = DATE)) +
  geom_col(aes(y = mdelay), fill = "blue") +
  #facet_wrap(~queue_type) +
  theme(axis.text.x = element_text(angle = 90, size = 11, vjust = 0.7)) 

summ %>%
  #filter(ct > 2000) %>%
  arrange(DATE) %>%
  ggplot(aes(x = DATE)) +
  geom_col(aes(y = sdelay), fill = "red") +
  #facet_wrap(~queue_type) +
  theme(axis.text.x = element_text(angle = 90, size = 11, vjust = 0.7)) 

by_date <- group_by(dat, DATE, queue_type)
summ <- summarise(by_date, mdelay = mean(delay_min), sdelay = sum(delay_min))

### only reports
summ %>%
  filter(queue_type == "secondary-darla") %>%
  arrange(mdelay) %>%
  ggplot(aes(x = DATE)) +
  geom_col(aes(y = mdelay), fill = "blue") +
  #facet_wrap(~queue_type) +
  theme(axis.text.x = element_text(angle = 90, size = 11, vjust = 0.7)) 

summ %>%
  #filter(ct > 2000) %>%
  arrange(DATE) %>%
  ggplot(aes(x = DATE)) +
  geom_col(aes(y = sdelay), fill = "red") +
  #facet_wrap(~queue_type) +
  theme(axis.text.x = element_text(angle = 90, size = 11, vjust = 0.7)) 


library(rmarkdown)
library(readxl)
library(dplyr)

setwd("C:/Users/manish.grewal/git-emdp/emdp/darla")

dat <- read_excel("darla.xlsx")

dat$lcecSiteId <- as.factor(dat$lcecSiteId)
dat$eventTypeEnum <- as.factor(dat$eventTypeEnum)
dat$queue_type <- as.factor(dat$queue_type)
dat$secs <- dat$milliseconds / 1000

dat <- filter(dat, processorId != -1)
dat$processorId <- as.factor(as.character(dat$processorId))

dat <- dat[,c("id", "lcecSiteId", "processorId", "eventTypeEnum", 
              "scheduledFor", "executedAt", "secs", "queue_type" )]

dat$delay_min <- (as.numeric(dat$executedAt - dat$scheduledFor)) / 60
dat$sched_date <- format(dat$scheduledFor, format = '%Y/%m/%d')

render("darla.RMD")



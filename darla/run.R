library(readxl)
library(rmarkdown)
library(dplyr)

st <- proc.time()

setwd("C:/Users/manish.grewal/git-emdp/emdp/darla")

dat1 <- read.csv("eventmetrics1.csv")
dat2 <- read.csv("eventmetrics2.csv")

inactive_sites <- read_excel("RFC-2373-inactive-site-list.xlsx")

dat <- rbind(dat1, dat2)
dat1 <- dat2 <- NULL

# drop unused cols
dat <- dat[, c("id", "lcecSiteId", "processorId", "campaignId", "eventTypeEnum", 
               "scheduledFor", "executedAt", "milliseconds" )]


# Fix date cols
dat$executedAt <- gsub("^(.{10} [0-9][0-9]:[0-9][0-9])$", "\\1:00", dat$executedAt)
dat$executedAt <- strptime(dat$executedAt, format = "%d-%m-%Y %H:%M:%S", tz = "America/Los_Angeles")

dat$scheduledFor <- gsub("^(.{10} [0-9][0-9]:[0-9][0-9])$", "\\1:00", dat$scheduledFor)
dat$scheduledFor <- strptime(dat$scheduledFor, format = "%d-%m-%Y %H:%M:%S", tz = "America/Los_Angeles")


## add index col
dat <- dat %>%
  mutate(index1 = row.names(dat), .before = 1)

#######################
# add queue_type col
#######################
events_critical <- c("welcomeEmail", "reminderEmail", "driveAssignment", "eptest1", "drive assignment retry", "reviewerReminderEmail", "reminder email series", "campaignDailyMailDelivery", "thankYouEmail", "campaignHourlyMailDelivery", "correctSubmissionEmail")
events_secondary <- c("reportDeliver", "driveAssignmentValidate", "eptest2", "notifyNearStart", "notifyNearDue", "notifyNearClose", "notifyClosed", "validateAnnouncementRecipients", "validateReminderRecipients", "bulkload poll", "reportNow", "driveAssignmentValidateReport", "validateReviewerReminderRecipients", "validateCorrectSubmissionRecipients", "reminderEmailRetry", "welcomeEmailRetry", "reviewerReminderEmailRetry", "correctSubmissionEmailRetry", "validateAnnouncementRecipientsUseAssignmentRules", "validateReminderRecipientsUseAssignmentRules", "VariableWelcomeEmailRetry", "VariableReminderEmailRetry", "VariableReviewerReminderEmailRetry", "VariableCorrectSubmissionReminderEmailRetry", "FailedEmailDeliveryReport", "CumulativeEmailRetry", "supervisorEmail")
events_tertiary <- c("lyris poll", "lyris gc", "monitor", "dailyAdminEmail", "unitTest", "dailyLogRotation", "computeCampaignProgress", "computeLicenseReports", "eptest3", "ContractModulesExceeded")

dat$queue_type <- case_when(
  dat$eventTypeEnum %in% events_critical ~ "critical",
  dat$eventTypeEnum %in% events_secondary ~ "secondary",
  dat$eventTypeEnum %in% events_tertiary ~ "tertiary")

# Queue changed for this job from secondary to tertiary on 18th April
dat$queue_type[dat$eventTypeEnum == "driveAssignmentValidateReport" & dat$scheduledFor >= "2021-04-18 12:00:00"] <- "tertiary"

#######################

dat$lcecSiteId <- as.factor(dat$lcecSiteId)
#dat$eventTypeEnum <- as.factor(dat$eventTypeEnum)
dat$queue_type <- as.factor(dat$queue_type)
dat$secs <- dat$milliseconds / 1000

dat <- filter(dat, processorId != -1)
dat$processorId <- as.factor(as.character(dat$processorId))

dat$delay_min <- (as.numeric(dat$executedAt - dat$scheduledFor)) / 60
dat$sched_date <- format(dat$scheduledFor, format = '%Y/%m/%d')
dat$sched_time <- format(dat$scheduledFor, format = '%H:%M:%S')
#dat$sched_daydate <- format(dat$scheduledFor, format = '%Y/%m/%d %a')
dat$sched_day <- format(dat$scheduledFor, format = '%a')
dat$sched_day <- factor(dat$sched_day, ordered = TRUE,
                        levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

dat$active <- ifelse(dat$lcecSiteId %in% inactive_sites$Site, "inactive", "active")

idx_rel1 <- which(dat$scheduledFor == "2021/04/18 12:00:00")[1]
idx_rel2 <- which(dat$scheduledFor == "2021/05/06 09:00:00")[1]

dat$period <- c(rep("Before", idx_rel1),
                rep("Release1", idx_rel2 - idx_rel1),
                rep("Release2", nrow(dat) - idx_rel2))


proc.time() - st

st <- proc.time()
render("darla.RMD")
print("Rendering markdown: ")
proc.time() - st
    
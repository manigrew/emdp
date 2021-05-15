library(readxl)
library(rmarkdown)
library(dplyr)

setwd("C:/Users/manish.grewal/git-emdp/emdp/darla")

st <- proc.time()
dat1 <- read.csv("eventmetrics1.csv")
dat2 <- read.csv("eventmetrics2.csv")

st <- proc.time()

print("Reading data: ")
proc.time() - st

inactive_sites <- read_excel("RFC-2373-inactive-site-list.xlsx")

dat <- rbind(dat1, dat2)

dat1 <- dat2 <- NULL

# drop unused cols
dat <- dat[, c("id", "lcecSiteId", "processorId", "campaignId", "eventTypeEnum", 
              "scheduledFor", "executedAt", "milliseconds" )]

# Fix date cols
st <- proc.time()
dat$executedAt <- gsub("^(.{10} [0-9][0-9]:[0-9][0-9])$", "\\1:00", dat$executedAt)
dat$executedAt <- strptime(dat$executedAt, format = "%d-%m-%Y %H:%M:%S", tz = "America/Los_Angeles")

dat$scheduledFor <- gsub("^(.{10} [0-9][0-9]:[0-9][0-9])$", "\\1:00", dat$scheduledFor)
dat$scheduledFor <- strptime(dat$scheduledFor, format = "%d-%m-%Y %H:%M:%S", tz = "America/Los_Angeles")


print("Fixing dates: ")
proc.time() - st


#######################
# add queue_type col
#######################
events_critical <- c("welcomeEmail", "reminderEmail", "driveAssignment", "eptest1", "drive assignment retry", "reviewerReminderEmail", "reminder email series", "campaignDailyMailDelivery", "thankYouEmail", "campaignHourlyMailDelivery", "correctSubmissionEmail")
events_secondary <- c("reportDeliver", "driveAssignmentValidate", "eptest2", "notifyNearStart", "notifyNearDue", "notifyNearClose", "notifyClosed", "validateAnnouncementRecipients", "validateReminderRecipients", "bulkload poll", "reportNow", "driveAssignmentValidateReport", "validateReviewerReminderRecipients", "validateCorrectSubmissionRecipients", "reminderEmailRetry", "welcomeEmailRetry", "reviewerReminderEmailRetry", "correctSubmissionEmailRetry", "validateAnnouncementRecipientsUseAssignmentRules", "validateReminderRecipientsUseAssignmentRules", "VariableWelcomeEmailRetry", "VariableReminderEmailRetry", "VariableReviewerReminderEmailRetry", "VariableCorrectSubmissionReminderEmailRetry", "FailedEmailDeliveryReport", "CumulativeEmailRetry", "supervisorEmail")
events_tertiary <- c("lyris poll", "lyris gc", "monitor", "dailyAdminEmail", "unitTest", "dailyLogRotation", "computeCampaignProgress", "computeLicenseReports", "eptest3", "ContractModulesExceeded")

dat1 <- dat %>%
  filter(eventTypeEnum %in% events_critical) %>%
  mutate(queue_type = "critical")

dat2 <- dat %>%
  filter(eventTypeEnum %in% events_secondary) %>%
  mutate(queue_type = "secondary")

# Queue changed for this job from secondary to tertiary on 18th April
dat2$queue_type[dat2$eventTypeEnum == "driveAssignmentValidateReport" & dat2$scheduledFor >= "2021-04-18 12:00:00"] <- "tertiary"

dat3 <- dat %>%
  filter(eventTypeEnum %in% events_tertiary) %>%
  mutate(queue_type = "tertiary")

dat <- rbind(dat1, dat2, dat3)

dat1 <- dat2 <- dat3 <- NULL


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
                        levels = c("Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed"))

dat$active <- ifelse(dat$lcecSiteId %in% inactive_sites$Site, "inactive", "active")


st <- proc.time()
render("darla.RMD")
print("Rendering markdown: ")
proc.time() - st


# dat %>%
#   filter(queue_type == "tertiary" & scheduledFor > executedAt) %>%


library(shiny)
library(dplyr)
library(ggplot2)




events_critical <- c( "welcomeEmail", "reminderEmail", "driveAssignment", "eptest1", "drive assignment retry", "reviewerReminderEmail", "reminder email series", "campaignDailyMailDelivery", "thankYouEmail", "campaignHourlyMailDelivery", "correctSubmissionEmail")
events_secondary <- c("reportDeliver", "driveAssignmentValidate", "eptest2", "notifyNearStart", "notifyNearDue", "notifyNearClose", "notifyClosed", "validateAnnouncementRecipients", "validateReminderRecipients", "bulkload poll", "reportNow", "driveAssignmentValidateReport", "validateReviewerReminderRecipients", "validateCorrectSubmissionRecipients", "reminderEmailRetry", "welcomeEmailRetry", "reviewerReminderEmailRetry", "correctSubmissionEmailRetry", "validateAnnouncementRecipientsUseAssignmentRules", "validateReminderRecipientsUseAssignmentRules", "VariableWelcomeEmailRetry", "VariableReminderEmailRetry", "VariableReviewerReminderEmailRetry", "VariableCorrectSubmissionReminderEmailRetry", "FailedEmailDeliveryReport", "CumulativeEmailRetry", "supervisorEmail")
events_tertiary <- c("lyris poll", "lyris gc", "monitor", "dailyAdminEmail", "unitTest", "dailyLogRotation", "computeCampaignProgress", "computeLicenseReports", "eptest3", "ContractModulesExceeded")

setwd("C:/Users/manish.grewal/git-emdp/emdp/darla")


#inactive_sites <- read_excel("RFC-2373-inactive-site-list.xlsx")

# dat1 <- read.csv("eventmetrics1.csv")
# dat2 <- read.csv("eventmetrics2.csv")
# dat3 <- read.csv("eventmetrics3.csv")
# dat4 <- read.csv("eventmetrics4.csv")
# dat4 <- dat4[, 1:12]
# 
# dat <- rbind(dat1, dat2, dat3, dat4)
# dat1 <- dat2 <- dat3 <- dat4 <- NULL
# saveRDS(dat, file = "eventmetrics.RDS")

st <- proc.time()
dat <- readRDS("eventmetrics.RDS")
proc.time() - st

# drop unused cols
dat <- dat[, c("id", "lcecSiteId", "processorId", "campaignId", "eventTypeEnum", "scheduledFor", "executedAt", "milliseconds")]

# Fix date cols
dat$executedAt <- gsub("^(.{10} [0-9][0-9]:[0-9][0-9])$", "\\1:00", dat$executedAt)
dat$executedAt <- strptime(dat$executedAt, format = "%d-%m-%Y %H:%M:%S", tz = "America/Los_Angeles")

dat$scheduledFor <- gsub("^(.{10} [0-9][0-9]:[0-9][0-9])$", "\\1:00", dat$scheduledFor)
dat$scheduledFor <- strptime(dat$scheduledFor, format = "%d-%m-%Y %H:%M:%S", tz = "America/Los_Angeles")


# ## add index col
# dat <- dat %>% 
#     mutate(index1 = row.names(dat), .before = 1)

#######################
# add queue_type col
#######################

dat$queue_type <- case_when(
    dat$eventTypeEnum %in% events_critical ~ "critical",
    dat$eventTypeEnum %in% events_secondary ~ "secondary",
    dat$eventTypeEnum %in% events_tertiary ~ "tertiary"
)

# Queue changed for this job from secondary to tertiary on 18th April
dat$queue_type[dat$eventTypeEnum == "driveAssignmentValidateReport" &
                   dat$scheduledFor >= "2021-04-18 12:00:00"] <- "tertiary"

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
dat$sched_day <- factor(
    dat$sched_day,
    ordered = TRUE,
    levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
)

dat$sched_time_30 <- paste0(
    substr(dat$sched_time, 1, 3),
    ifelse(as.integer(substr(dat$sched_time, 4, 5)) >= 30, "30", "00"))

#dat$active <- ifelse(dat$lcecSiteId %in% inactive_sites$Site, "inactive", "active")

# idx_rel1 <- which(dat$scheduledFor == "2021/04/18 12:00:00")[1]
# idx_rel2 <- which(dat$scheduledFor == "2021/05/06 09:00:00")[1]
# 
# dat$period <- c(
#     rep("Before", idx_rel1),
#     rep("Release1", idx_rel2 - idx_rel1),
#     rep("Release2", nrow(dat) - idx_rel2)
# )


dat %>% 
    filter(sched_date != "2021/04/08")


proc.time() - st

by_date <- dat %>%
    group_by(sched_date, sched_day)

plot_by_date <- function(x, y, desc) {
    
    what <- gsub("_", " ", y)
    title <- paste("By date:", what, "(", desc, ")")
    
    print(title)
    
    #idx_rel1 <- which(x$sched_date == "2021/04/18")
    #idx_rel2 <- which(x$sched_date == "2021/05/06")
    
    p <- x %>%
        ggplot(aes(x = sched_date)) +
        geom_col(aes_string(y = y, fill = "sched_day")) +
        theme(axis.text.x = element_text(angle = 90, size = 11, vjust = 0.7)) +
        #scale_fill_manual(values = cbp1) +
        xlab("Date") +
        ylab(what) +
        ggtitle(title) +
        #geom_vline(aes(xintercept = c(idx_rel1), color="Rel1"), size=1, linetype="dashed") +
        #geom_vline(aes(xintercept = c(idx_rel2), color="Rel2"), size=1, linetype="dashed") +
        #scale_color_manual(name = "Release", values = c(Rel1 = "blue", Rel2 = "black")) +
        scale_x_discrete(breaks = x$sched_date[seq(1, nrow(x), 2)])
    
    print(p)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Darla Jobs Analysis"),
    
    # Sidebar with a slider input for number of bins
    selectInput(
        "queue_type",
        "Select Queue",
        choices = c("critical", "secondary", "tertiary")
    ),
    selectInput(
        "stat",
        "Select statistic",
        choices = c("average_delay_minutes", "total_delay_minutes", "average_execution_minutes", "total_execution_minutes", "max_delay_minutes", "count")
    ),
    
    # Show a plot of the generated distribution
    plotOutput("date_plot")
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    output$date_plot <- renderPlot({
        summ <- by_date %>%
            filter(queue_type == input$queue_type) %>%
            summarise(
                average_delay_minutes = mean(delay_min),
                total_delay_minutes = sum(delay_min),
                average_execution_minutes = mean(secs / 60),
                total_execution_minutes = sum(secs / 60),
                max_delay_minutes = max(delay_min),
                count = n()
            )
        plot_by_date(summ, input$stat, "Stat")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

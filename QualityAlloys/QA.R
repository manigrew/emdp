library(readxl)
library(ggplot2)
library(dplyr)

setwd("C:/Users/manish.grewal/emdp/R/QualityAlloys")

## Load the dataset
daily_visits <- read_excel("CU46-XLS-ENG.xls", sheet = "Daily Visits", skip = 4)

# Read the main worksheets from xls workbook  

weekly_visits <- read_excel("CU46-XLS-ENG.xls", sheet = "Weekly Visits", skip = 4)
financials <- read_excel("CU46-XLS-ENG.xls", sheet = "Financials", skip = 4)
lbs_sold <- read_excel("CU46-XLS-ENG.xls", sheet = "Lbs. Sold", skip = 4)

# and remove non alphabet characters from column headers (names)

names(weekly_visits) <- gsub("[^a-zA-Z]+", "", names(weekly_visits))
names(financials) <- gsub("[^a-zA-Z]+", "", names(financials))
names(lbs_sold) <- gsub("[^a-zA-Z]+", "", names(lbs_sold))

# gather columns into weekly_data
weekly_data <- data.frame(
  Week = weekly_visits$Week, 
  UniqueVisits = weekly_visits$UniqueVisits, 
  Revenue = financials$Revenue, 
  Profit = financials$Profit, 
  LbsSold = financials$LbsSold)

# gather columns into weekly_data
weekly_data <- data.frame(
  Week = weekly_visits$Week, 
  Visits = weekly_visits$Visits,
  UniqueVisits = weekly_visits$UniqueVisits, 
  Revenue = financials$Revenue, 
  Profit = financials$Profit, 
  LbsSold = financials$LbsSold)

# convert Week to ordered factored 
weekly_data$Week <- factor(weekly_data$Week, levels = weekly_data$Week)

idx_init <- which(levels(weekly_data$Week) == "Aug 24 - Aug 30")
idx_pre  <- which(levels(weekly_data$Week) == "Jan 18 - Jan 24")
idx_prom <- which(levels(weekly_data$Week) == "May 17 - May 23")
idx_post <- nrow(weekly_data)
weekly_data$Period <- c(rep("Initial Period", idx_init),
                        rep("Pre-promotion",  idx_pre - idx_init),
                        rep("promotion",      idx_prom - idx_pre),
                        rep("Post-promotion", idx_post - idx_prom))


p <- ggplot(weekly_data, aes(x = Week)) +
  scale_x_discrete(breaks = weekly_visits$Week[seq(1, nrow(weekly_visits), 2)]) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### a. unique visits over time
p +   geom_bar(aes(y = UniqueVisits), stat = "identity", fill = "blue", color = "black")

### b. revenue over time,
p +   geom_bar(aes(y = Revenue), stat = "identity", fill = "blue", color = "black")

### c. profit over time
p +   geom_bar(aes(y = Profit), stat = "identity", fill = "blue", color = "black")

### d. pounds sold over time.
p +   geom_bar(aes(y = LbsSold), stat = "identity", fill = "blue", color = "black")

## 2. Summary by period
by_period <- group_by(weekly_data, Period)

summ <- by_period %>% 
  select(!Week) %>% 
  summarise(across(everything(), list(mean, median, sd, min, max)))

summ_init <- as.data.frame(
  matrix(as.numeric(summ[1, 2:26]), nrow = 5), 
  row.names = c('mean', 'median', 'std. dev.', 'minimum', 'maximum'))
names(summ_init) <- names(by_period)[2:6]

summ_pre  <- as.data.frame(
  matrix(as.numeric(summ[3, 2:26]), nrow = 5),
  row.names = c('mean', 'median', 'std. dev.', 'minimum', 'maximum'))
names(summ_pre) <- names(by_period)[2:6]

summ_prom <- as.data.frame(
  matrix(as.numeric(summ[4, 2:26]), nrow = 5),
  row.names = c('mean', 'median', 'std. dev.', 'minimum', 'maximum'))
names(summ_prom) <- names(by_period)[2:6]

summ_post <- as.data.frame(
  matrix(as.numeric(summ[2, 2:26]), nrow = 5),
  row.names = c('mean', 'median', 'std. dev.', 'minimum', 'maximum'))
names(summ_post) <- names(by_period)[2:6]

View(summ)


## 3. Column charts of means over th four periods

by_period %>%

  
??factor

### 8f

#ggplot() +
#geom_histogram(data = lbs_sold, aes(x = scale)) +
# geom_histogram(data = rn, aes(x = rn))

p1 <- ggplot(data = data.frame(x = c(-5, 5)), aes(x)) +
  stat_function(fun = dnorm, n = 1001, args = list(mean = 0, sd = 1))  +
  scale_y_continuous(breaks = NULL)
p1

set.seed(123)
rn2 <- rnorm(290, mean = mean(lbs_sold$LbsSold), sd = sd(lbs_sold$LbsSold))
scale2 <- scale(rn2)

lbs_sold$rn2 <- rn2
lbs_sold$scale2 <- scale2
#ggplot(lbs_sold) +
# geom_histogram(aes(zscore), color = "#0033cc", fill = "#cc000007") +
# geom_histogram(aes(scale2), color = "#cc0033", fill = "#0000cc07")



ggplot(daily_visits, aes(Visits)) +
  geom_histogram(binwidth = 5)

summary(daily_visits)



uv_by_period <- select(weekly_data, Period, UniqueVisits) %>%
  group_by(Period)

summarise(uv_by_period, mean(UniqueVisits), median(UniqueVisits), sd(UniqueVisits), min(UniqueVisits), max(UniqueVisits))

summ <- uv_by_period %>% 
  select_if(is.numeric) %>% 
  summarise(across(everything(), list(mean, median, sd, min, max) 
                   #, .names = c("{col}.{fn}")
  ))

summ_init <- as.data.frame(
  matrix(as.numeric(summ[1, 2:26]), nrow = 5), 
  row.names = c('mean', 'median', 'std. dev.', 'minimum', 'maximum'))
names(summ_init) <- names(by_period)[2:6]

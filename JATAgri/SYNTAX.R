library(readxl)

library(ggplot2)

##############################
#Question 1: Anand, the cofounder of JAT, claims that disease 6 (leaf curl) information was accessed at least 60 times every month on average since October 2017 due to this disease outbreak.
#Test this claim at a significance level of 0.05 using an appropriate hypothesis test.
############################

##IMPORT THE DATA
setwd("C:/Users/manish.grewal/emdp/R/JATAgri")
data <- read_excel("IMB733-XLS-ENG.xlsx", sheet = "Data Sheet")
bdata <- read_excel("IMB733-XLS-ENG.xlsx", sheet = "Belagavi_weather")
ddata <- read_excel("IMB733-XLS-ENG.xlsx", sheet = "Dharwad_weather")

#data <- read.csv("clipboard", sep = "\t", header = TRUE)
data <- data[-124,]

View(data)
#########################
##To test this claim, select the D6 disease access data from October 2017 onwards
data1 <- data[95:123,10]
View(data1)

###calculate the mean and standard deviation from data1
summary(data1)
sd(data1)

### one-sample t-test

t.test(data1, mu = 60, alternative = "greater", conf.level = 0.95)
t.test(data1, mu = 60, alternative = "greater", conf.level = 0.98)

## INTERPRETATION
In this case, we reject the null hypothesis and infer that the claim made by Anand that the access of disease 6 in his app is at least 60 times a month on average due to the disease outbreak is correct.

##############################
Question 2: Among the app users for disease information, at least 15% of the users access disease information related to disease 6.
Use an appropriate hypothesis test to check this claim at a = 0.05.

D6_usage <- sum(data$D6)
D6_usage

tot_usage <- sum (data$D1, data$D2,data$D3, data$D4,data$D5,data$D6,
                  data$D7,data$D8,data$D9, data$D10,data$D11)
tot_usage
p_usage <- D6_usage/tot_usage

sums <- lapply(data[,5:15], sum )
tot_usage <- sum(unlist(sums))
prop <- sums[["D6"]] / tot_usage



test <- prop.test(x = 4295, n = 26830, p = .15, alternative = "greater", conf.level = 0.95)
test
#############################
#Question 3: JAT believes that over the years, the average number of app users have increased significantly. Is there statistical evidence to support that the average number of users in year 2017-2018 is more than average number of users in year 2015-2016 at at=0.05? 
#  Support your answer with all necessary tests.
##################################
## Spliting the data into two(2015-16, 2017-2018)
data2 <- data[1:73,]
data3 <- data[74:123,]
## Assigning a dummy column in each and clubbing these two data

data2$dummy <- "range_1"
data3$dummy <- "range_2"
newdata <- rbind(data2, data3)
names(newdata)

## Assuming equql varience 
t.test (newdata$"No of users"~newdata$dummy, mu = 0, alternative = "two.sided", conf.level = 0.95, paired = FALSE, var.eq = TRUE)
t.test (newdata$"No of users"~newdata$dummy, mu = 0, alternative = "greater", conf.level = 0.95, paired = FALSE, var.eq = TRUE)
t.test (newdata$"No of users"~newdata$dummy, mu = 0, alternative = "two.sided", conf.level = 0.95, paired = FALSE, var.eq = FALSE)
#paired
#paired false means independent sample t test
#paired true means dependent sample t test

#2-t rule of thumb
#running a test, where df > 20, alpha 5 perent
#t value greater than 2, it  will be significant


## Assuming unequql varience 
t.test (newdata$No.of.users~newdata$dummy, mu = 0, alternative = "two.sided", conf.level = 0.95, paired = FALSE, var.eq = FALSE)

##The critical value for a=0.05 with 121 degrees of freedom is 1.65. Since the calculated statistic value is greater than critical value, we reject the null hypothesis and accept the claim that there is statistically significant difference between the users base of the years 2017-2018 and 2015-2016.

##Question 4  #######################
Farmers use apps to access information throughout the month. Using the data, check whether
app usage is same or different across the four weeks of a month. Anand claims that app usage picked up after January 2016; so, test this hypothesis using data from January-2016 - May 2018.

##creating data-subset (2016-2018)
data5 <- data [26:123,]

## Test of Normality (the data is not normal)
#H0 - data deviates from normality
#H1 - data devaites from normality
shapiro.test(data5$Usage)

qqnorm(data5$Usage)
ggplot(data5, aes(sample = Usage)) +
  stat_qq() +
  stat_qq_line()

## converting the data using log transformation and checking the normality

data5$logusage <- log(data5$Usage)

ggplot(data5, aes(sample = logusage)) +
  stat_qq() +
  stat_qq_line()


shapiro.test(data5$logusage)
## using log transformed variables
anova <- aov (data5$logusage~data5$Week)
model.tables(anova, type = "means")
summary(anova)

One-way ANOVA output shows that p-value = 0.326 > 0.05 and F-stat =1.16, 
which is less than F critical. Hence, we fail to reject the null hypothesis; 
and hence we conclude that there is no statistically significant difference in 
the app usage across various weeks.

## using orginal variable

anova1 <- aov (data5$Usage~data5$Week)
model.tables(anova1, type = "means")


##Question-5
##Anand claims that the number of users has increased over a period of two years. He wants to understand if his app usage (number of times his app is accessed in a month by various users) has increased with the increased number of users. Prove this claim statistically. Also suggest a suitable statistical test to prove that the correlation between users and usage is non-zero.

cor <- cor.test(data5$"No of users", data5$Usage)
cor
##This shows a very high correlation between the number of users enrolled and the usage of the app; hence with the increase in the number of users, the usage also increased.
#Based on the above test, we can prove that users and usage have a correlation is higher and can never be zero.

##################################
##Question 6: A new version of the app was released in August 2016. Anand wants to know which month in the given time frame after the launch of new version, the mean usage pattern would start to show a statistically significant shift.

##draw the line chart and see the spike


data$rdate <- data$"Month-Year"
plot(data$Usage~data$"Month-Year", type ="l", col = "red")
#creating a subset(from October 2016 onwards)

every_nth = function(n) {return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})}

ggplot(data, aes(rdate, Usage)) +
  geom_line(group = 1) #+
#  scale_x_discrete(breaks = every_nth(n = 1))

data6 <- data[1:61,]
data7 <- data[62:123,]

data6$dummy <- "before"
data7$dummy <- "after"
newdata1 <- rbind(data6, data7)

t.test (newdata1$Usage~newdata1$dummy, mu = 0, alternative = "two.sided", conf.level = 0.95, paired = FALSE, var.eq = TRUE)
##The calculated t-statistic is more than the critical value of t; and thus, we reject the null hypothesis and conclude that the app usage is statistically significantly different before and after October 2016. So, after the new release in August 2016, the usage of JAT's app increased, starting from October 2016.


#Question 7: If a disease is likely to spread in particular weather condition (data given in the disease index sheet), then the access of that disease should be more in the months having suitable weather conditions. Help the analyst in coming up with a statistical test to support the claim for two districts for which the sample of weather and disease access data is provided in the data sheet. Mention the diseases for which you can support this claim. Test this claim both for temperature and relative humidity at 95% confidence.
#bdata <- read.csv ("clipboard", sep = "\t", header = TRUE)
#names(bdata)

bdata1 <- subset(bdata,Temperature <= 24 & Humidity >= 80)
bdata2 <- subset(bdata,!(Temperature <= 24 & Humidity >= 80))

bdata1$weather <- "favourable"
bdata2$weather <- "unfavourable"
newbdata <- rbind (bdata1, bdata2)

##t-test 1 (RUNNING T TEST USING DATA USING THE DISEASE INDEX OF D1)
#NOTE: PLEASE USE THE INDEX OF DIFFERENT DISESES TO MAKE THE GROUP (FAVOURABLE VS. UNFAVOURABLE )

test1 <- t.test (newbdata$D1~newbdata$weather, mu = 0, alternative = "two.sided", conf.level = 0.95, paired = FALSE, var.eq = TRUE)
test2 <- t.test (newbdata$D2~newbdata$weather, mu = 0, alternative = "two.sided", conf.level = 0.95, paired = FALSE, var.eq = TRUE)
test3 <- t.test (newbdata$D3~newbdata$weather, mu = 0, alternative = "two.sided", conf.level = 0.95, paired = FALSE, var.eq = TRUE)
test4 <- t.test (newbdata$D4~newbdata$weather, mu = 0, alternative = "two.sided", conf.level = 0.95, paired = FALSE, var.eq = TRUE)
test5 <- t.test (newbdata$D5~newbdata$weather, mu = 0, alternative = "two.sided", conf.level = 0.95, paired = FALSE, var.eq = TRUE)
test6 <- t.test (newbdata$D7~newbdata$weather, mu = 0, alternative = "two.sided", conf.level = 0.95, paired = FALSE, var.eq = TRUE)

test1
test2 #(## NOT CORRECT)
test3 #(## NOT CORRECT)
test4 #(## NOT CORRECT)
test5 #(## NOT CORRECT)
test6 #(## NOT CORRECT)
############################################################
SIMILAR TO THE PREVIOUS ONE RUN ANALYSIS FOR DHARWAD DISTRICT
############################################################



conds <- data.frame(Hmin = c(80, 83, 0, 85, 77, 80),
                    Hmax = c(100, 100, 100, 100, 85, 100),
                    Tmin = c(20, 21.5, 22, 22, 22, 25),
                    Tmax = c(24, 24.5, 24, 26, 24.5, 100),
                    row.names = paste0("D", c(1:5, 7)))

myttest <- function(bdata, temp_humid) {
  Hmin <- temp_humid$Hmin
  Hmax <- temp_humid$Hmax
  Tmin <- temp_humid$Tmin
  Tmax <- temp_humid$Tmax
  
  bdata1 <- subset(bdata, Temp >= Tmin & Temp <= Tmax & Humid >= Hmin & Humid <= Hmax)
  bdata2 <- subset(bdata, !(Temp >= Tmin & Temp <= Tmax & Humid >= Hmin & Humid <= Hmax))
  
  bdata1$weather <- "favourable"
  bdata2$weather <- "unfavourable"
  newbdata <- rbind(bdata1, bdata2)
  
  test1 <- t.test (newbdata$D ~ newbdata$weather, mu = 0, alternative = "two.sided", conf.level = 0.95, paired = FALSE, var.eq = TRUE)
  
  df <- test1$parameter[[1]]
  critical <- abs(qt(0.05, 9))
  
  c(nrow(bdata1), nrow(bdata2), test1$estimate[[1]], 
    test1$estimate[[2]], 
    sd(bdata1$D), sd(bdata2$D),
    test1$statistic[[1]], df, critical)
}


res <- data.frame(matrix(NA, nrow = 9, ncol = 6))
row.names(res) <- c('nrow1', 'nrow2', 'mean1', 'mean2', 'sd1', 'sd2', 't-value', 'df', 'critical')
names(res) <- paste0("D", c(1:5, 7))

mydata <- data.frame(Temp = bdata$Temperature, Humid = bdata$Humidity)

for (name in names(res)) {
  print(name)
  mydata$D <- unlist(bdata[name])
  res[, name] <- myttest(mydata, conds[name,])
}

mydata <- data.frame(Temp = ddata$Temperature, Humid = ddata$`Relative Humidity`)

for (name in names(res)) {
  print(name)
  mydata$D <- unlist(ddata[name])
  res[, name] <- myttest(mydata, conds[name,])
}


qt(0.05, 3) 
qf(0.05, 3, 3)

library(tidyr)
library(dplyr)
gather(bdata)

bdata1 <- newbdata[,!(names(bdata) %in% c("D6", "D8", "D9", "D10", "D11" 
                                          #, "Temperature", "Humidity"
                                          ))]

bdata2 <- bdata[, names(bdata) %in% c("Months", "Temperature", "Humidity")] %>%
  pivot_longer(-Months, names_to = "Weather")

colors <- c("Temperature" = "Red", "Humidity" = "Green")

bdata1 %>%
  #select(-c(D8, D9, Temperature, Humidity))
  pivot_longer(-Months, names_to = "Disease") %>%
  ggplot( aes(x = Months)) +
    geom_bar(aes(y = value, fill = Disease), position = "dodge", group = 1, stat = "identity", size = 1.2) +
    geom_line(data = bdata2, aes(Months, y = value, group = Weather, color = Weather)) +
  labs(y = "App Usage\nTemperature(C)\nRel. Humidity(%)")

+ 
    scale_color_manual(values = colors)
    


vignette("pivot")

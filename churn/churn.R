library(ggplot2)
library(dplyr)

setwd("C:/Users/manish.grewal/emdp/R/churn")
churn <- read.csv("churn.csv")

churn$Phone[is.na(churn$Phone)]



### Q1
any( is.na( churn ) )

sum(is.na(churn))
summary(churn$CustServ.Calls)
sd(churn$CustServ.Calls)
(1 - 1.563) / 1.3155

names(churn)
for (nam in colnames(churn)) {
  print(paste(nam, summary(churn[,nam])))
}

esquisse::esquisser(churn)


library(ggplot2)

### Q2
ggplot(churn) +
  aes(x = "", y = CustServ.Calls) +
  geom_boxplot(fill = "#0c4c8a") +
  scale_y_continuous(n.breaks = 10) +
  theme_minimal()

ggplot(churn, aes(CustServ.Calls, fill = Churn.)) +
  geom_bar(position = "fill") 

### Q3
churn$CustServ.Calls_zscore <- scale(churn$CustServ.Calls)
filter(churn, CustServ.Calls_zscore < -3 | CustServ.Calls_zscore > 3)



summary(churn$CustServ.Calls_zscore)

### Q4

churn$Day.Mins_zscore <- scale(churn$Day.Mins)

### Q5
skew <- 3 * (mean(churn$Day.Mins) - median(churn$Day.Mins)) / sd(churn$Day.Mins)
skew_lib <- skewness(churn$Day.Mins)

skew_z <- 3 * (mean(churn$Day.Mins_zscore) - median(churn$Day.Mins_zscore)) / sd(churn$Day.Mins_zscore)
skew_z_lib <- skewness(churn$Day.Mins_zscore)

nearly perfectly symmetric

### Q6

ggplot(churn, aes(sample = Day.Mins)) +
  stat_qq(col = "blue") +
  stat_qq_line(col = "red", size = 0.75)

### Q7

ggplot(churn, aes(sample = Intl.Mins)) +
  stat_qq() +
  stat_qq_line(col = "red")

ggplot(churn) +
  aes(x = "", y = Intl.Mins) +
  geom_boxplot(fill = "#0c4c8a") +
  scale_y_continuous(n.breaks = 10) +
  theme_minimal()

ggplot(churn, aes(Intl.Mins)) +
  geom_histogram(bins = 50)  


churn <- mutate(churn, nzim = ifelse(Intl.Mins > 0, 1, 0))

ggplot(churn, aes(sample = nzim)) +
  stat_qq() +
  stat_qq_line(col = "red")

summary(churn$nzim)

ggplot(churn, aes(nzim)) +
  geom_histogram(bins = 10)  



summary(churn$Intl.Mins)
count(churn, churn$Intl.Mins)

### q8
churn$Night.Mins_zscore <- scale(churn$Night.Mins)

ggplot(churn, aes(Night.Mins_zscore)) +
  geom_boxplot() +
  geom_histogram()





#################################################

ggplot(churn, aes(Night.Mins, Day.Mins, color = Churn.)) +
  geom_point(size = 2) +
  geom_jitter() +
  geom_smooth(method = "loess") 


ggplot(churn, aes(Day.Mins, CustServ.Calls, color = Churn.)) +
  geom_point(size = 1) +
  geom_jitter()
geom_h  

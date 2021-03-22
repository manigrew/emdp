
library(readxl)

##############################
#Question 1: Anand, the cofounder of JAT, claims that disease 6 (leaf curl) information was accessed at least 60 times every month on average since October 2017 due to this disease outbreak.
#Test this claim at a significance level of 0.05 using an appropriate hypothesis test.
############################

##IMPORT THE DATA
setwd("C:/Users/manish.grewal/emdp/R/Agri")
data <- read_excel("IMB733-XLS-ENG.xlsx", sheet = "Data Sheet")

data <- data[-124,]
#data <- read.csv ("clipboard", sep = "\t", header = TRUE)
names (data)
library(dplyr)
data1 <- mutate (data, disease = D1+ D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10 + D11,
                 variety = V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10)

group <- group_by(data1, Week)

sum <- summarize(group, disease = sum(disease, na.rm = TRUE), variety = sum(variety, na.rm = TRUE))

sum1 <- data.frame (week = 1:5, disease = sum$disease, variety = sum$variety)

chisq.test(sum1[1:4,])




library(readxl)
library(ggplot2)
### MISSION HOSPITALS: CASE ANALYSIS

## IMPORT DATA INTO R
data <- read.csv ("clipboard", sep = "\t", header = TRUE)
setwd("C:/Users/manish.grewal/emdp/R/missionhospital")

data <- read_excel("IMB529-XLS-ENG.xlsx", sheet = "MH-Modified Data")

names(data) <- gsub(" ", ".", names(data))
######################################################################
#Q1: Develop a simple linear regression model to check if there is an association between total cost and body weight? For the model developed, interpret the regression coefficient value for body weight.

## Ans: Develop a simple linear regression model with total cost as the response variable (dependent variable) and body weight as the explanatory variable (independent variable).

reg1 <- lm (TOTAL.COST.TO.HOSPITAL~ BODY.WEIGHT, data  = data)
summary(reg1)

ggplot(data, aes(BODY.WEIGHT, TOTAL.COST.TO.HOSPITAL)) +
  geom_point() + 
  geom_smooth()

anova(reg1)
#Result: We observe that the p-value of the variable "Body Weight" is close to 0  
#and thus significant at 95% confidence level and has a coefficient of 1846.703. 
# The model suggests a positive association between total cost and body weight. 
#However, we should accept the model only after checking the important assumptions
#of normality and homoscedasticity.
#############################################################
# CHECKING THE ASSUMPTION (NORMALITY)
plot (data$BODY.WEIGHT, data$TOTAL.COST.TO.HOSPITAL)
abline (reg1)
plot(reg1)
str(reg1)

hist(residuals(reg1))
  boxplot(residuals(reg1))
shapiro.test(residuals(reg1))
################
# transforming the dependent and independent variable into log form

data$lmTOTAL.COST.TO.HOSPITAL <- log (data$TOTAL.COST.TO.HOSPITAL)
data$lnBODY.WEIGHT <- log(data$BODY.WEIGHT)
names(data)

reg2 <- lm (lmTOTAL.COST.TO.HOSPITAL~ lnBODY.WEIGHT, data  = data)
shapiro.test(residuals(reg2))
plot(reg2)
#################################

rdata <- data[c(-2,-7, -36),]
reg3 <- lm (lmTOTAL.COST.TO.HOSPITAL~ lnBODY.WEIGHT, data  = rdata)
shapiro.test(residuals(reg3))
plot(reg3)
summary (reg3)

Or
reg4 <- lm (TOTAL.COST.TO.HOSPITAL~ BODY.WEIGHT, data  = rdata)
summary(reg4)
####################################
#Q3: At the time of admission, a patient's body weight is 50 kg. At 95% confidence level, what will be the maximum cost of treatment for this patient?

TC <- 122111.2 + (1930.8 *50)
ME = 1.96*291.8
MIN = TC-ME
MAX <- TC+ME
MAX
MIN
##########################################################
#Q5. Build a simple linear regression model between total cost and gender. There is a belief that the cost of treatment for males is INR 20,000 more than females. Validate this statement.

names(rdata)
levels (rdata$GENDER)
library(car)
rdata$GENDER <- recode(rdata$GENDER, "'F'=0; 'M'=1", as.factor=TRUE)

reg5 <- lm (TOTAL.COST.TO.HOSPITAL~GENDER, data = rdata)
summary(reg5)
###############################################################################
#Q6: Develop a model and validate if married people spend INR 50,000 more than unmarried people in treatment cost.
names(rdata)
levels (rdata$GENDER)
rdata$MARITAL.STATUS <- recode(rdata$MARITAL.STATUS, "'UNMARRIED'=0; 'MARRIED'=1", as.factor=TRUE)

reg6 <- lm (TOTAL.COST.TO.HOSPITAL~MARITAL.STATUS, data = rdata)
summary(reg6)

#######################################################################
##### Q7: PREPARE A FINAL DATA WITH FOR PERFORMING FULL MODEL
names (rdata)
attach(rdata)
newdata <- data.frame(AGE,GENDER,MARITAL.STATUS,ACHD,CAD.DVD,CAD.SVD,CAD.TVD,CAD.VSD,OS.ASD,other..heart,other..respiratory,other.general,other.nervous,
                      other.tertalogy,PM.VSD,RHD,Diabetes1, Diabetes2,hypertension1,hypertension2, hypertension3,other,AMBULANCE,TRANSFERRED,ALERT,BODY.WEIGHT,BODY.HEIGHT,HR.PULSE,
                      TOTAL.COST.TO.HOSPITAL,TOTAL.LENGTH.OF.STAY,LENGTH.OF.STAY...ICU,
                      LENGTH.OF.STAY..WARD,IMPLANT.USED..Y.N.,COST.OF.IMPLANT)

names(newdata)[33] <- "IMPLANT.USED"
levels(newdata$IMPLANT.USED)
library(car)
newdata$IMPLANT.USED <- recode(newdata$IMPLANT.USED, "'N'=0; 'Y'=1", as.factor=TRUE)
###################################################################
#NEW MODEL WITH ALL VARIABLES
reg7 <- lm (TOTAL.COST.TO.HOSPITAL~., data = newdata)
summary(reg7)
step <- step (reg7, direction = "backward")
summary (step)
##############################################
#truncated model

newdata1 <- data.frame (TOTAL.COST.TO.HOSPITAL,PM.VSD,other.general,other.tertalogy,BODY.HEIGHT,Diabetes2,TOTAL.LENGTH.OF.STAY,COST.OF.IMPLANT,LENGTH.OF.STAY...ICU)
reg8 <- lm (TOTAL.COST.TO.HOSPITAL~., data = newdata1)


summary (reg8)

For estimating standardized beta values
install.packages ("QuantPsyc")
library(QuantPsyc)
lm.beta(reg8)
#####################################################

             
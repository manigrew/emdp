## BASICS OF REGRESSION
income <- c (100, 120, 140, 160, 180,200)
consumption <- c(80,100, 120,140,160,170)

sum(income*income) / 6
#keynes: when income increases, consumption increases but not as much as increase in income

saving <- c(21,21, 22, 25, 28, 30)
gender <- c (0, 0, 0, 1, 1, 1)
gender  <- as.factor (gender)
reg <- lm (consumption ~ income)
summary(reg)
## DRAWING REGRESSION LINE
plot(consumption ~ income)
abline (reg)
## GETTING THE SUMMARY RESULTS 
summary(reg)
anova(reg)

anova <- aov (consumption ~gender)
model.tables(anova, type = "means")


reg3 <- lm(saving ~ income)
summary(reg3)

mvalue <- 1/(1-.9254)

library(car)
vif(reg3)

reg4 <- lm(consumption ~ gender)
summary(reg4)

plot(consumption ~ gender)
abline(reg4)
############################################################
#CASE ANALYSIS: JAYALAKSHMI AGRO TECH
############################################################
## IMPORT DATA INTO R & Perform a regression with income_per_acre as the dependent variable, and temp_mgmt as the independent variable
library(readxl)
data <- read.csv("clipboard", sep = "\t", header = TRUE)

setwd("C:/Users/manish.grewal/emdp/R/JATSeri")

data <- read_excel("IMB737-XLS-ENG.xlsx", sheet = "Regression Data")
data <- data[-25]
names (data)

#MODEL 1
mod1 <- lm (income_per_acre~temp_mgmt, data  = data)
summary(mod1)
anova(mod1)

#Result interpretation
The income_per_acre variable is regressed on the temp_mgmt dummy variable. The coefficient of temp_mgmt is positive and significant suggesting that the income per acre is higher by Rs. 35057 with the implementation of temperature management in the rearing house.
##############################################################

#MODEL 2
mod2 <- lm (income_per_acre~humidity_mgmt, data  = data)
summary(mod2)
anova(mod2)


mod2 <- lm (income_per_acre~humidity_mgmt + temp_mgmt, data  = data)
summary(mod2)
anova(mod2)

#Result interpretation
#The income_per_acre variable is now regressed on the humidity_mgmt dummy variable. The coefficient of humidity_mgmt is positive and significant suggesting that the income per acre increases with the implementation of humidity management in the rearing house
###############################################

cor(data$humidity_mgmt, data$temp_mgmt)

#MODEL 3 (with income_per acre as dependent and all others are independent)
mod3 <- lm (income_per_acre~., data  = data)
summary(mod3)
anova(mod3)
# Need a package (car) for VIF (is a measure of multicollinearity) check
install.packages("car")
library(car)
# check vif
vif (mod3)
#Result interpretation
#The results of this regression might be erroneous because of the incidence of multicollinearity. Multicollinearity tends to inflate the standard error of coefficient estimates. It can also destabilize the regression estimation. Variance Inflation Factor (VIF) is one of the metrics which can be used to check the presence of multicollinearity. 
#VIF values greater than 4 might indicate significant multicollinearity.
#In this instance, the variables temp_mgmt and humidity_mgmt have VIF values greater than 4. 
######################################################
#MODEL 4: A new model was estimated after removing the humidity_mgmt variable.
data1 <- data[-19]
names(data1)
mod4 <- lm (income_per_acre~., data  = data1)
summary(mod4)
vif (mod4)
anova(mod4)

#Results
#A new model was estimated after removing the humidity_mgmt variable. The model summary, ANOVA and coefficient estimates are presented. In this model, all the VIF values are under 4.

#######################################
#MODEL 5
#Stepwise regression approach can be used to develop a parsimonious model. At each iteration, independent variables are either entered or removed from the model 
# AIC: Aakaike information critiera
step <- step (mod4, direction = "backward")
summary (step)
# RESULTS
# Idetified the relevant variables: borewell_recharge, training_on_sericulture,bio_fertilizers,crop_insured,temp_mgmt,affected_by_pest,rearing_cost,loan_amount,own_vermi_compost, chawki_bivol,mechanization, mulberry_diseases

# MODEL 6
# attach data1 and create a subset data based on stepwise regression results
attach(data1)
data2 <- data.frame (income_per_acre,borewell_recharge, training_on_sericulture,bio_fertilizers,crop_insured,temp_mgmt,affected_by_pest,rearing_cost,loan_amount,own_vermi_compost, chawki_bivol,mechanization, mulberry_diseases)
detach(data1)

# PEFROM A REGRESSION USING data2 variables

mod6 <- lm (income_per_acre ~., data = data2)
summary(mod6)
# compare the same with mod4 to check the model parsimony
summary(mod4)

########################################
For estimating standardized beta values
library(QuantPsyc)
sort(abs(lm.beta(mod6)))
#######################################


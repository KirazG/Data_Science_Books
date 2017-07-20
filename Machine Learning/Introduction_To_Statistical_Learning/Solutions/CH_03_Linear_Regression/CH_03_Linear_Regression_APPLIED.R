#################################################################################
##### BOOK: INTRODUCTION TO STATISTICAL LEARNING                            #####
##### CHAPTER: 03 - LINEAR REGRESSION                                       #####
##### APPLIED EXERCISE CODE                                                 #####
#################################################################################

##### Cleanup environment & Set working directory

rm(list = ls(all.names = TRUE))
require(ISLR)
require(MASS)
require(corrplot)
require(car)

#################################################################################
##### EXERCISE 08:                                                          #####
#################################################################################
### This question involves the use of simple linear regression on the Auto data set.
### (a) Use the lm() function to perform a simple linear regression with
### mpg as the response and horsepower as the predictor. Use the summary() function
### to print the results. Comment on the output.

dfAuto = Auto
str(Auto)

lmSLR = lm(mpg ~ horsepower, data = dfAuto)
summary(lmSLR)

### i. Is there a relationship between the predictor and the response?
# YES - There is a strong relationship
# The overall F-statistic for model is 599 and p-value is 2.2 X 10e-16 which is ~0.
# The coefficient of horsepower is also significnat. Hence there is strong relationship
# between the predictor and the response

### ii. How strong is the relationship between the predictor and the response?
# RSE for the model is 4.906 while the mean of fitted.values (or response) is 23.446
# That gives % error as = 4.906*100/23.446 = 20.93%
# Also note that Adjusted R-Square = 0.6049 OR horsepower is able to explain 60.49%
# of variation in milage (mpg)

### iii. Is the relationship between the predictor and the response positive or negative?
# Negative: as the coefficient of horsepower is negative - which is intuitive too !

### iv. What is the predicted mpg associated with a horsepower of 98? What are the associated 
### 95% confidence and prediction intervals?
# Y = 39.9359 - 0.1578 * horsepower

# Confidence Interval
predict(lmSLR, data.frame(horsepower = 98), interval = "confidence")

# Prediciton Interval
predict(lmSLR, data.frame(horsepower = 98), interval = "prediction")

### (b) Plot the response and the predictor. Use the abline() function to display the least squares regression line.

plot(x = dfAuto$horsepower, y = dfAuto$mpg)
plot(x = dfAuto$horsepower, y = dfAuto$mpg, col = "blue", ylab = "MPG", xlab = "Horsepower")
abline(reg = lmSLR, col = "red")

### (c) Use the plot() function to produce diagnostic plots of the least squares regression fit. 
### Comment on any problems you see with the fit.

par(mfrow = c(2,2))
plot(lmSLR)
par(mfrow = c(1,1))

# OBSERVATION:
# Residual Vs Fitted plot displays a V shaped patterns & indicates non-linearity 
# in the data. Also plot of Studentized Residual Vs. Leverage indicates presence of # few High Leverage data point and few outliers.


#################################################################################
##### EXERCISE 09:                                                          #####
#################################################################################

### This question involves the use of multiple linear regression on the Auto data set.
### (a) Produce a scatterplot matrix which includes all of the variables in the data set.

# Scatterplot 1-excluding name column
str(dfAuto)
plot(dfAuto[,1:(ncol(dfAuto)-1)], col = "red")

# Scatterplot 2-excluding name column
pairs(dfAuto[,1:(ncol(dfAuto)-1)], col = "blue")

### (b) Compute the matrix of correlations between the variables using the function cor(). 
### You will need to exclude the name variable, cor() which is qualitative.

# Correlation calculation
CORR = cor(dfAuto[,1:(ncol(dfAuto)-1)])
CORR

# Correlogram
corrplot(corr = CORR)

### Use the lm() function to perform a multiple linear regression with mpg as the response 
### and all other variables except name as the predictors. Use the summary() function to print ### the results. Comment on the output. For instance:

lmMLR = lm(formula = mpg ~ .-name, data = dfAuto)
summary(lmMLR)

## i. Is there a relationship between the predictors and the response?
# YES - the overall model is significant based on F-Statistic and p-Value.

## ii. Which predictors appear to have a statistically significant relationship to the response?
# displacement, weight, year, origin

## What does the coefficient for the year variable suggest?
# Coefficient of year variable has positive sign and a value of 0.7508 which indicates that -
# when value of year increases by 1 - keeping all other variable constant -  the AVERAGE milage value # increases by 0.7508 (OR AVERAGE effect of year on milage increases by 0.7508)
# Intuitively, the cars become more fuel-efficient year-on-year.

### (d) Use the plot() function to produce diagnostic plots of the linear regression fit.
# Comment on any problems you see with the fit. Do the residual plots suggest any unusually 
# large outliers? Does the leverage plot identify any observations with unusually high 
# leverage?

par(mfrow = c(2,2))
plot(lmMLR)
par(mfrow = c(2,2))

### (e) Use the * and : symbols to fit linear regression models with interaction effects. Do any interactions appear to be statistically significant?

names(dfAuto)

# "mpg" "cylinders" "displacement" "horsepower"   "weight"       "acceleration" "year" "origin"       "name" 
# Picking top-3 pairs of co-related variabls and intorudcing interaction tems
lmInteraction = lm(formula = mpg ~ cylinders*displacement + horsepower*displacement + displacement*weight, data = dfAuto)
summary(lmInteraction)

# OBSERVATIONS:
# For above set of variables - and using p-values the interaction between displacement & cylinder and displacement & weight is NOT significant statistically. However, interaction between displacement & horsepower IS statistically significant.

### (f) Try a few different transformations of the variables, such as log(X), √X, X2. Comment on your findings.

# Trying different tranformations and plotting

plot(sort(dfAuto$cylinders))
# "cylinders" is a discrete variable - tranformation wont help much !

plot(sort(dfAuto$displacement))
# "cylinders" is a semi-discrete variable - not performing any tranformation!

# Checking on horsepower
plot(sort(dfAuto$horsepower))
par(mfrow = c(2,2))
plot(sort(dfAuto$horsepower), main = "horsepower (ASIS)")
plot(log(sort(dfAuto$horsepower)), main = "log(horsepower)")
plot(sqrt(sort(dfAuto$horsepower)), main = "sqrt(horsepower)")
plot(sort(dfAuto$horsepower)^2, main = "horsepower ^ 2")
par(mfrow = c(1,1))
# log trnasformation seem to straighten the line 

# Checking on weight
plot(sort(dfAuto$weight))
par(mfrow = c(2,2))
plot(sort(dfAuto$weight), main = "Weight (ASIS)")
plot(log(sort(dfAuto$weight)), main = "log(Weight)")
plot(sqrt(sort(dfAuto$weight)), main = "sqrt(Weight)")
plot(sort(dfAuto$weight)^2, main = "Weight^2")
par(mfrow = c(1,1))
# Trnaformation does seem to help (visually) compared to variable in ASIS form

# Checking on acceleration
plot(sort(dfAuto$acceleration))
par(mfrow = c(2,2))
plot(sort(dfAuto$acceleration), main = "acceleration (ASIS)")
plot(log(sort(dfAuto$acceleration)), main = "log(acceleration)")
plot(sqrt(sort(dfAuto$acceleration)), main = "sqrt(acceleration)")
plot(sort(dfAuto$acceleration)^2, main = "acceleration^2")
par(mfrow = c(1,1))
# Trnaformation does seem to help (visually) compared to variable in ASIS form


#################################################################################
##### EXERCISE 10:                                                          #####
#################################################################################

### This question should be answered using the Carseats data set.
### (a) Fit a multiple regression model to predict Sales using Price, Urban, and US.

dfCarSeats = Carseats
str(dfCarSeats)
summary(dfCarSeats)

lm10MLR1 = lm(formula = Sales ~ Price+Urban+US, data = dfCarSeats)
summary(lm10MLR1)

### (b) Provide an interpretation of each coefficient in the model. Be careful—some of the variables in the model are qualitative!

## INTERPRETATION:
# 1:    Coefficeint for price is -0.0545 and p-value is significantly < 0. This mean for 1 unit
#       increase in Price - keeping Urban and US constant, the AVERGAE estimated value of Sales #       unit goes down by 54.459 units 
# 2:    Coefficeint for US = Yes is 1.2005 and p-value is significantly < 0 (OR statistically 
#       significant). This mean if the store is located in US then for a given price point and
#       Urban value the average estimate of Sales Unit is higher/increased by 1200.5 units
# 3:    Coefficeint for Urban = Yes is -0.02192 and p-value is 0.936 (OR statistically 
#       NOT significant). This mean Urban variable does not have effect on Sales.

### (c) Write out the model in equation form, being careful to handle the qualitative variables properly.
#               Sales = 12.043469 - 0.054459*Price + 1.200573*(US=Yes) 

### (d) For which of the predictors can you reject the null hypothesis H0 : βj = 0?
#   Price & US

### (e) On the basis of your response to the previous question, fit a smaller model that only
### uses the predictors for which there is evidence of association with the outcome.
lm10MLR2 = update(object = lm10MLR1, formula. = Sales ~ Price+US )
summary(lm10MLR1)
summary(lm10MLR2)

### (f) How well do the models in (a) and (e) fit the data?
# Bothe model have same R-squared, however, Adjusted R-squared is marginally better in 
# 2nd model due to reduced complexity. 
# %ERROR = RSE/MEAN(fitted.values) = ~33% >> HIGH ERROR !
# The value of R2 is not impressive as two the predictors are able to explain only 23.54% 
# (OR < 25%) variation in the Sales data !

### (g) Using the model from (e), obtain 95% confidence intervals for the coefficient(s).
confint(object = lm10MLR2, level = 0.95)

### (h) Is there evidence of outliers or high leverage observations in the model from (e)?

# Drawing default 4 plots
par(mfrow = c(2,2))
plot(lm10MLR2)
par(mfrow = c(1,1))

# Drawing all 6 pltos
par(mfrow = c(3,2))
plot(lm10MLR2, which = 1:6)
par(mfrow = c(1,1))

# Outlier Test: (ToTheBestOfMyKnowledg - Indicates not outliers in data)
outlierTest(model = lm10MLR2)

# Calculating influence measures and drawing influecne plot !
influence.measures(model = lm10MLR2)
influencePlot(model = lm10MLR2)

# OBSERVATION:
# Outliers: Not present. All residuals from Residual Vs. Leverage plot are bound between -3 & 3. 
# High Leverage: Some points seem to have high leverage
# Influenctial Points: Going by Cooks.D < 0.5 - No point seem to have high Influence on regression line


#################################################################################
##### EXERCISE 11:                                                          #####
#################################################################################

### In this problem we will investigate the t-statistic for the null hypothesis 
### H0 : β = 0 in simple linear regression without an intercept. To begin, we generate
### a predictor x and a response y as follows.

set.seed (1)
x=rnorm (100)
y=2*x+rnorm (100)

### (a) Perform a simple linear regression of y onto x, without an intercept.
### Report the coefficient estimate ˆβ, the standard error of this coefficient estimate,
### and the t-statistic and p-value associated with the null hypothesis H0 : β = 0. 
### Comment on these results. 
### (You can perform regression without an intercept using the command lm(y~x+0))

# Performing regression without Intercept
lm11SLR1 = lm(y~x + 0)
summary(lm11SLR1)

# β of x = 1.9939 | Std. Error: 0.1065 | t-Stat = 18.73 | p-Value < 2e-16
# Coefficient is +VE hence Y incease with inceaase in X
# p-value is very close to zero and hence co-efficient is statically significant. Null hypothesis H0 can be rejected.

### (b) Now perform a simple linear regression of x onto y without an intercept, and report the coefficient estimate, its standard error, and the corresponding t-statistic and p-values associated with the null hypothesis H0 : β = 0. Comment on these results.

# Performing regression without Intercept
lm11SLR2 = lm(x~y + 0)
summary(lm11SLR1)

# β of y = 1.9939 | Std. Error: 0.1065 | t-Stat = 18.73 | p-Value < 2e-16
# Coefficient is +VE hence Y incease with inceaase in X
# p-value is very close to zero and hence co-efficient is statically significant. Null hypothesis H0 can be rejected.

### (c) What is the relationship between the results obtained in (a) and (b)?
## Both regression lines obtained in (a) & (b) are the same !

### (d) SKIPPED

### (e) SKIPPED

### (f) In R, show that when regression is performed with an intercept, the t-statistic for H0 : β1 = 0 is the same for the regression of y onto x as it is for the regression of x onto y.

lm11SLR3 = lm(x~y)
lm11SLR4 = lm(y~x)
summary(lm11SLR3)
summary(lm11SLR4)

# Based on Summary, it can be observed that t-Statistic is same for both regressions !


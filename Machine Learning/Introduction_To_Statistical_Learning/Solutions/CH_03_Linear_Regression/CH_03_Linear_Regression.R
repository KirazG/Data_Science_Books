#################################################################################
##### BOOK: INTRODUCTION TO STATISTICAL LEARNING                            #####
##### CHAPTER: 03 - LINEAR REGRESSION                                       #####
#################################################################################

##### Cleanup environment & Set working directory

rm(list = ls(all.names = TRUE))
require(ISLR)
require(MASS)
require(corrplot)
require(car)


#################################################################################
##### SECTION 3.6.x: BOSTON HOUSING DATA SET                                #####
#################################################################################

##### Simple Linear Regression using Boston Housing Dataset

### Load and Copy Boston Housing data
data("Boston")
dfBoston = Boston

### Take a look at the data
fix(dfBoston)
str(dfBoston)
summary(dfBoston)

### Look at variable names in the set
names(dfBoston)

### Look at variable names in the set
lm.fit =lm(medv ~ .,data = dfBoston)
summary (lm.fit)

### To access individual elements of Sumamry

objSum <- summary(lm.fit)

# Model's F-statistics
objSum$fstatistic

# Model's RSE
objSum$sigma

# R-square & Adjusted-R-square
objSum$r.squared
objSum$adj.r.squared

### Updating linear model using update()

# Exclude age variable
lm.fit1 = update(object = lm.fit, formula. = medv ~ .-age)
summary(lm.fit1)

# Exlcude indus as it's not significant
lm.fit2 = update(object = lm.fit1, formula. = medv ~ .-indus)
summary(lm.fit2)

### INTERACTION TERMS:
# The syntax lstat:black tells R to include an interaction term between lstat and black. # The syntax lstat*age simultaneously includes lstat, age, and the interaction term lstat×age as predictors; it is a shorthand for - lstat+age+lstat:age.

### Trying polynomial predictors/terms
### Using ANOVA for comparing linear model fits with different parameters

# Predictor lstat with power 1
lm1 = lm(formula = medv ~ lstat, data = dfBoston)
summary(lm1)

# Predictor lstat with power 2
# I() is required in the forula to preserve the meaning of ^ as an arithmatic operator
lm2 = lm(formula = medv ~ lstat + I(lstat^2), data = dfBoston)
summary(lm2)

# Predictor lstat with power 3
lm3 = lm(formula = medv ~ lstat + I(lstat^3), data = dfBoston)
summary(lm3)

# Using ANOVA - Compare lm1 & lm2
anova(lm1, lm2)
anova(lm2, lm3)
anova(lm1, lm3)
anova(lm1, lm2, lm3)

## Based on ANOVA - it's clear that the non-linearity in the data can be best modelled using a quadratic term. Using a cubic term is not helping as the model with cubic term is inferior to the model with quadratic temrm.




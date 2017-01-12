#################################################################################
##### BOOK: INTRODUCTION TO STATISTICAL LEARNING                            #####
##### CHAPTER: 02 - STATISTICAL LEARNING                                    #####
#################################################################################

##### Cleanup environment & Set working directory

rm(list = ls(all.names = TRUE))
require(ISLR)
require(MASS)
require(corrplot)
setwd("D:/GitHub_Repositories/Data_Science_Books/Machine Learning/Introduction_To_Statistical_Learning/Solutions/CH_02_Statistical_Learning")

################################################################################
##### EXERCISE 8 : COLLEGE DATA SET                                        #####
################################################################################

##### Read data 

### Use the read.csv() function to read the data into R. Call the loaded data college. 
# Make sure that you have the directory set to the correct location for the data.
data("College")
dfCollege = College

# Look at the data using the fix() function. You should notice that the first column is just the name of each university.
# We don’t really want R to treat this as data. However, it may be handy to have these names for later.
fix(dfCollege)

### Eliminate row names
rownames(dfCollege) = NULL
fix(dfCollege)

### C-i :: Use the summary() function to produce a numerical summary of the variables in the data set.
summary(dfCollege)

### C-ii :: Use the pairs() function to produce a scatterplot matrix of the first ten columns or variables of the data.
pairs(dfCollege[1:10])
# plot also does the same job!
plot(dfCollege[1:10])

### C-iii :: Use the plot() function to produce side-by-side boxplots of Outstate versus Private. 
plot(dfCollege$Outstate ~ dfCollege$Private,
     xlab = "Private University", 
     ylab ="Out Of State tuition ($)", 
     main = "Outstate Tuition Fees")

### C-iV :: Create a new qualitative variable, called Elite, by binning
# the Top10perc variable. We are going to divide universities
# into two groups based on whether or not the proportion
# of students coming from the top 10% of their high school
# classes exceeds 50%.Use the summary() function to see how many elite universities
# there are. Now use the plot() function to produce
# side-by-side boxplots of Outstate versus Elite.

dfCollege$Elite = ifelse(dfCollege$Top10perc > 50, "Yes", "No")
dfCollege$Elite = as.factor(dfCollege$Elite)

summary(dfCollege$Elite)

plot(dfCollege$Outstate ~ dfCollege$Elite,
     xlab = "Elite University", 
     ylab ="Out Of State tuition ($)", 
     main = "Outstate Tuition Fees")

### C-v :: Use the hist() function to produce some histograms with
# differing numbers of bins for a few of the quantitative variables.

par(mfrow = c(2,2))
hist(dfCollege$Books, col = 2, xlab = "Books", ylab = "Count", breaks = 50, main = "Histogram: Books")
hist(dfCollege$PhD, col = 3, xlab = "PhD", ylab = "Count", breaks = 25, main = "Histogram: PhD")
hist(dfCollege$Grad.Rate, col = 4, xlab = "Grad Rate", ylab = "Count", breaks = 25, main = "Histogram: Grad.Rate")
hist(dfCollege$perc.alumni, col = 6, xlab = "% Alumni", ylab = "Count", breaks = 25, main = "Histogram: %Alumni")
par(mfrow = c(1,1))


################################################################################
##### EXERCISE 9 : AUTO DATA SET                                           #####
################################################################################

##### Read data 
data(Auto)
dfAuto = Auto

### Make sure that the missing values have been removed from the data.
sum(is.na(dfAuto))

### a :: Which of the predictors are quantitative, and which are qualitative?
str(dfAuto)

### b :: What is the range of each quantitative predictor? You can answer this using the range() function.
summary(dfAuto[ ,-ncol(dfAuto)])
apply(dfAuto[ ,-ncol(dfAuto)], 2, range)

### c :: What is the mean and standard deviation of each quantitative predictor?
apply(dfAuto[ ,-ncol(dfAuto)], 2, mean)
apply(dfAuto[ ,-ncol(dfAuto)], 2, sd)

### d :: Now remove the 10th through 85th observations. 
# What is the range, mean, and standard deviation of each predictor in the subset of the data that remains?
dfAuto1 = dfAuto[-c(10:85), ]

apply(dfAuto1[ ,-ncol(dfAuto)], 2, range)
apply(dfAuto1[ ,-ncol(dfAuto)], 2, mean)
apply(dfAuto1[ ,-ncol(dfAuto)], 2, sd)

### e :: Using the full data set, investigate the predictors graphically, using scatterplots or other tools of your choice. 
# Create some plots highlighting the relationships among the predictors. 
# Comment on your findings.
pairs(x = dfAuto[ ,-ncol(dfAuto)])

# More mileage per gallon on a 4 cyl vehicle than the others. 
# Weight, displacement and horsepower seem to have an inverse effect with mpg. 
# There is an overall increase in mpg over the years - 2x in one decade. 
# Japanese cars have higher mpg than US or European cars.

### f :: Suppose that we wish to predict gas mileage (mpg) on the basis of the other variables. 
# Do your plots suggest that any of the other variables might be useful in predicting mpg?

## Weight, displacement and horsepower can be used for prediction


################################################################################
##### EXERCISE 10 : BOSTON HOUSING DATA                                    #####
################################################################################


##### Read data 
data("Boston")
dfBoston = Boston

### a :: How many rows are in this data set? How many columns? What do the rows and columns represent?
dim(dfBoston)
dfBoston$chas = as.factor(dfBoston$chas)

summary(dfBoston)
str(dfBoston)

### b :: Make some pairwise scatterplots of the predictors (columns) in this data set. Describe your findings.
pairs(dfBoston)

### c :: Are any of the predictors associated with per capita crime rate? 
# If so, explain the relationship.

corrplot(corr = cor(dfBoston[,-c(4)]), 
         method = "circle", 
         type = "lower",title = "Boston Data Correlogram")

# Majority of the predictors have a week-correlation with the Cime Rate (crim)
# The correlation is in the range -0.6 to 0.4

### d :: Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher ratios? 
# Comment on the range of each predictor.

## Crime Rate:
# Per capita crime rate has highly right skewed distribution.
# In 70% suburbs - Per Cap. Crime Rate <= 2
# In 79% suburbs - Per Cap. Crime Rate <= 5
# In 90% suburbs - Per Cap. Crime Rate <= 10
hist(x = dfBoston$crim, col = 4, breaks = 50, main = "Crime Rate In Boston Suburbs")

## Tax Rates:
# Average full value property tax rate/$10000 is $408
# In 72% suburbs - full value property tax rate/$10000 is < $500
# In 26% suburbs - full value property tax rate/$10000 is as high as $666
hist(x = dfBoston$tax, col = 4, breaks = 50, main = "Tax Rate In Boston Suburbs")

## Pupil-teacher ratios:
# Pupil-teacher ratios is in the range 12.60-22.00 with Average = 18.46
# For 30% of suburbs the PT ratio is between 20-21 !
hist(x = dfBoston$ptratio, col = 4, breaks = 10, main = "Pupil-Teacher Ration In Boston Suburbs")

### e :: How many of the suburbs in this data set bound the Charles river?
table(dfBoston$chas)

### f :: What is the median pupil-teacher ratio among the towns in this data set?
median(dfBoston$ptratio)

### g :: Which suburb of Boston has lowest median value of owneroccupied homes?
# What are the values of the other predictors for that suburb, 
# and how do those values compare to the overall ranges for those predictors?

# Which suburb of Boston has lowest median value of owneroccupied homes?
print(paste0("Suburb #", which.min(dfBoston$medv), " has lowest median value of owner-occupied homes !"))

# What are the values of the other predictors for that suburb, 
View(dfBoston[which.min(dfBoston$medv), ])

### g :: In this data set, how many of the suburbs average more than seven rooms per dwelling? 
# More than eight rooms per dwelling? Comment on the suburbs that average more than eight rooms per dwelling.

print(paste0("# Suburbs with >7 Avg. Rooms/Dwelling := ", sum(dfBoston$rm > 7)))
print(paste0("# Suburbs with >8 Avg. Rooms/Dwelling := ", sum(dfBoston$rm > 8)))

# Only 2.5% suburbs have >8 Avg. Rooms/Dwelling !



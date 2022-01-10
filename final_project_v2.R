# clear plots
if(!is.null(dev.list())) dev.off()

# clear console
cat("\014")

# clean workspace
rm(list=ls())

setwd("C:/Users/Checo/Desktop/Statistical Methods/Projects/Project2")
getwd()

## Import dataset
read.data = read.csv("superint_salary_all.csv",header = T)
head(read.data)
colnames(read.data)
## Cleaning data
# Remove some redundant variables 
library(dplyr)
library(ggplot2)
dataset = select(read.data, -c(State, Region, County, ï..District, Superintendent, Base.Pay, FTE, ))
dataset

# Change numeric predictors to factors 
str(dataset)
dataset$Charter.Status = as.factor(dataset$Charter.Status)
levels(dataset$Charter.Status) = c(2,1)   # 1 = Traditional   2 = Charter
dataset$Size = as.factor(dataset$Size)
levels(dataset$Size) = c("L", "M", "S")
head(dataset)
#### Data Analysis

table(dataset$Charter.Status)
num_traditional = 836
num_charter = 114

# Pie charts
pie.STATUS = pie(table(dataset$Charter.Status), labels = c("Charter= 12%", "Traditional= 88%"))

#Box Plot
boxplot(Enrollment ~Size, data = dataset, ylim= range(0:100000))

#Histograms
hist.SIZE = plot(as.factor(dataset$Size), main = paste("Histogram of ",names(dataset)[6]) )


# Plot Graph
if(!is.null(dev.list())) dev.off()
par(mfrow=c(2,2))
plot(dataset$Enrollment , dataset$FTE.Pay,
     xlab = "Enrollment",
     ylab = "Salary SUperintendent" ) 
abline(lm(dataset$FTE.Pay ~ dataset$Enrollment ))


plot(dataset$Avg.Teacher.Salary, dataset$FTE.Pay,
     xlab = "Avg Teacher's Pay",
     ylab = "Salary Superintendent" ) 
abline(lm(dataset$FTE.Pay ~ dataset$Avg.Teacher.Salary ))


# Simple Linear Regression

if(!is.null(dev.list())) dev.off()
fit.regression = lm( FTE.Pay  ~ Enrollment + Avg.Teacher.Salary     ,data = dataset)
fit.regression
summary(fit.regression)
par(mfrow=c(2,2))
plot(fit.regression)
summary(fit.regression)$r.sq
summary(fit.regression)$sigma

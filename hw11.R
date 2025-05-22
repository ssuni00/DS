## Data Science 01분반 Team 4 HW 11 ##

# Load Data
getwd()
setwd('/Users/ssuni/Desktop/HGU/25-1/DS/hw11')

# 필요한 pkg 불러오기
library(dplyr)
library(rpart)
library(rpart.plot)
library(ROCR)
install.packages("car")
library("car")

## Question 1
SeoulBike<- read.csv("SeoulBikeData.csv")
str(SeoulBike)

vif(SeoulBike)

## Question 2
names(SeoulBike) <- c("Date", "Rented", "Hour", "Temp", "Humidity", "Wind")

## Question 3

## Question 4
# 삭제할 val: 

## Question 5

## Question 6

## Question 7

## Question 8

## Question 9

## Question 10

## Question 11
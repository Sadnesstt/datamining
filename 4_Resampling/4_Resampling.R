pacman::p_load(ggplot2, tidyverse, dplyr, plotly, processx)
rm(list=ls(all=TRUE))
setwd('C:/Users/dnskd/Desktop/20Spring/datamining/week5')

## 5.3 Lab : Cross-Validation and the Bootstrap
## 5.3.1 The Validation Set Approach
library(ISLR)
set.seed(1)
train = sample(392, 196) # index만 뽑았다는 점, sample의 short cut을 사용했다는 점
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
set.seed(2)
train = sample(392, 196)
lm.fit = lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

## 5.3.2 Leave-One-Out Cross-Validation
glm.fit = glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)
lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)
if(!require(boot)) install.packages('boot'); library(boot)
glm.fit = glm(mpg ~ horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta
cv.error = rep(0,5)
for(i in 1:5){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

## 5.3.3 k-Fold Cross-Validation
set.seed(17)
cv.error.10 = rep(0, 10)
for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10

## 5.3.4 The Bootstrap
alpha.fn <- function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100)
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))
boot(Portfolio, alpha.fn, R = 1000)
boot.fn = function(data, index){
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}
boot.fn(Auto, 1:392)
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot.fn(Auto, sample(392, 392, replace = T))
boot(Auto, boot.fn, 1000)
summary(lm(mpg ~ horsepower, data = Auto))$coef

boot.fn = function(data, index){
  coefficients((lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index)))
}
set.seed(1)
boot(Auto, statistic = boot.fn, R = 1000)
summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef


## 5.2
# g.
boot_is <- function(n){
  1-(1-(1/n))^n
}
n <- 1:100000
df <- data.frame(n = n, prob = sapply(n, boot_is))
df %>% ggplot(aes(n, prob)) + geom_path() + geom_point() + ylim(c(0.4, 1))
ggsave('ex2.png')

# h.
store = rep(NA, 10000)
for (i in 1:10000){
  store[i] = sum(sample(1:100, rep = TRUE)==4)>0
}
mean(store)

## 5.5
# a.
library(ISLR)
set.seed(1)
glm.fit <- glm(default ~ income + balance, data = Default, family = "binomial")
sum.coef <- summary(glm.fit)$coef
write.csv(sum.coef, "fitting.csv")

# b. 
train <- sample(nrow(Default), nrow(Default)*0.5, replace = FALSE)
glm.fit <- glm(default ~ income + balance, data = Default[train,], family = "binomial")
post <- predict(glm.fit, Default[-train, ], type = "response")
pred <- ifelse(post > 0.5, "Yes", "No")
mean(pred != Default$default[-train])

# c.
sampling <- function(i){
  set.seed(i)
  train <- sample(nrow(Default), nrow(Default)*0.5, replace = FALSE)
  glm.fit <- glm(default ~ income + balance, data = Default[train,], family = "binomial")
  post <- predict(glm.fit, Default[-train, ], type = "response")
  pred <- ifelse(post > 0.5, "Yes", "No")
  return(mean(pred != Default$default[-train]))
}
sampling(2); sampling(3); sampling(4)

# d.
set.seed(1)
train <- sample(nrow(Default), nrow(Default)*0.5, replace = FALSE)
glm.fit <- glm(default ~ income + balance + student, data = Default[train,], family = "binomial")
post <- predict(glm.fit, Default[-train, ], type = "response")
pred <- ifelse(post > 0.5, "Yes", "No")
mean(pred != Default$default[-train])
sum.coef2 <- summary(glm.fit)$coef
write.csv(sum.coef2, 'sum.csv')

## 7.
# a.
glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")
summary_7 <- summary(glm.fit)$coef
write.csv(summary_7, "summary_7.csv")

# b.
glm.fit <- glm(Direction ~ Lag1 + Lag2 , data = Weekly[-1, ], family = "binomial")
summary_7_b <- summary(glm.fit)$coef
write.csv(summary_7_b, "summary_7_b.csv")

# c.
ifelse(predict(glm.fit, Weekly[1, ], type = "response")>0.5, "Up", "Down") == Weekly[1, "Direction"]

# d.
result <- c()
for(i in 1:nrow(Weekly)){
  glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ], family = "binomial")
  result[i] <- sum(Weekly[i, "Direction"] != ifelse(predict(glm.fit, Weekly[i, ], type = "response")>0.5, "Up", "Down"))
}
sum(result)/nrow(Weekly)


## 9.
# a. 
library(MASS)
names(Boston)
mean(Boston$medv)

# b.
sd(Boston$medv)/sqrt(nrow(Boston))

# c.
library(boot)
set.seed(1)
boot.fn <- function(data, index){
  return(mean(data[index, "medv"]))
}
result <- boot(Boston, boot.fn, 1000)
result

# d.
c(22.53281 - 2*0.4106622, 22.53281 + 2*0.4106622)
t.test(Boston$medv)

# e.
median(Boston$medv)

# f.
set.seed(1)
boot.fn <- function(data, index){
  return(median(data[index, "medv"]))
}
boot(Boston, boot.fn, 1000)

# g.
quantile(Boston$medv, 0.1)

# h.
set.seed(1)
boot.fn <- function(data, index){
  return(quantile(data[index, "medv"], 0.1))
}
boot(Boston, boot.fn, 1000)

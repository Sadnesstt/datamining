pacman::p_load(ggplot2, tidyverse, dplyr, plotly, processx)
rm(list=ls(all=TRUE))
setwd('C:/Users/dnskd/Desktop/20Spring/datamining/week4')

#---- 4.6 Lab : Logistic Regression, LDA, QDA, and KNN ----#
## 4.6.1 The Stock Market Data
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

## 4.6.2 Logistic Regression
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = Smarket)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
glm.probs = predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Direction)
glm.pred = rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction)
(507+145)/1250
mean(glm.pred == Direction)
train = (Year < 2005)
Smarket.2005 = Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 = Direction[!train]
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
mean(glm.pred == Direction.2005)
table(glm.pred, Direction.2005)
mean(glm.pred != Direction.2005)
glm.fits = glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs>.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
106/(106+76)
predict(glm.fits, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")

## Linear Discriminant Analysis
library(MASS)
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20, 1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)

## Quadratic Discriminant Analysis
qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

## 4.6.5 K-Nearest Neighbors
library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
(83+43)/252
knn.pred = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

## 4.6.6 An Application to Caravan Insurance Data
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
standardized.X = scale(Caravan[, -86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred, test.Y)
9/(68+9)
knn.pred = knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5/26
knn.pred = knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4/15
glm.fits = glm(Purchase ~., data = Caravan, family=binomial, subset = -test)
glm.probs = predict(glm.fits, Caravan[test, ], type = "response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs > .5] = "Yes"
table(glm.pred, test.Y)
glm.pred = rep("No", 1000)
glm.pred[glm.probs>0.25] = "Yes"
table(glm.pred, test.Y)
11/(22+11)

#--- Exercise 4, 10, 11 ---#
## Exercise 10

## 10-a
names(Weekly)
summary(Weekly)
png('weekly.png')
plot(Weekly)
dev.off()
sum.week <- summary(Weekly)
write.csv(sum.week, 'weekly.csv')
attach(Weekly)
png('boxplot.png')
par(mfrow=c(2,4))
boxplot(Direction, Lag1, main = "Lag1", xlab = "Direction",  names=c("Down","Up"))
boxplot(Direction, Lag2, main = "Lag2", xlab = "Direction",  names=c("Down","Up"))
boxplot(Direction, Lag3, main = "Lag3", xlab = "Direction",  names=c("Down","Up"))
boxplot(Direction, Lag4, main = "Lag4", xlab = "Direction",  names=c("Down","Up"))
boxplot(Direction, Lag5, main = "Lag5", xlab = "Direction",  names=c("Down","Up"))
boxplot(Direction, Volume, main = "Volume", xlab = "Direction",  names=c("Down","Up"))
boxplot(Direction, Today, main = "Today", xlab = "Direction",  names=c("Down","Up"))
dev.off()

## 10-b
glm.fit.weekly <- glm(Direction ~ Lag1 + Lag2+ Lag3 + Lag4+ Lag5+ Volume, data = Weekly, family = binomial)
summary(glm.fit.weekly)
sum.glm <- summary(glm.fit.weekly)$coef
write.csv(sum.glm, 'glm.csv', row.names = FALSE)

## 10-c
glm.probs = predict(glm.fit.weekly, type = "response")
glm.pred = rep("Down", nrow(Weekly))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction)
557/(430+557)
mean(glm.pred == Direction)

## 10-d
training = (Year<2009)
train_data = Weekly[training,]
test_data = Weekly[!training,]
glm.train.fit <- glm(Direction~Lag2, data = train_data, family = binomial)
glm.probs <- predict(glm.train.fit, test_data, type = "response")
glm.pred <- rep("Down", nrow(test_data))
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, test_data$Direction)
mean(glm.pred == test_data$Direction)

## 10-e
lda.fit = lda(Direction ~ Lag2, data = train_data)
lda.fit
lda.pred = predict(lda.fit, test_data)
lda.class = lda.pred$class
table(lda.class, test_data$Direction)
mean(lda.class == test_data$Direction)

## 10-f
qda.fit = qda(Direction ~ Lag2, data = train_data)
qda.fit
qda.class = predict(qda.fit, test_data)$class
table(qda.class, test_data$Direction)
mean(qda.class == test_data$Direction)

## 10-g
train <- (Year < 2009)
train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
train.Direction = Direction[train]
test.Direction = Direction[!train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, test.Direction)
mean(knn.pred == test.Direction)

## 10-i
set.seed(1886)
result <- c()
for(i in 1:30){
  knn.pred <- knn(train.X, test.X, train.Direction, k = i)
  result[i] <- mean(knn.pred == test.Direction)
}
df <- data.frame(K = 1:30, accuracy = result)
best <- which(df$accuracy == max(df$accuracy))
library(ggrepel)
df %>% ggplot(aes(K, accuracy)) + geom_line() + geom_point() + theme_bw() + labs(title = "Accuracy by K from 1 to 30")+
  geom_text_repel(aes(label = paste0("(", K, ", ", round(accuracy, 3), ")")) , data = df[best, ]) + geom_point(data=df[best,], size = 3, col = "red")
ggsave('best.png')

## 11-a
attach(Auto)
Auto$mpg01 <- as.factor(ifelse(mpg > median(mpg), 1, 0))
table(Auto$mpg01)

## 11-b
summary(Auto)
png('scattter.png')
plot(Auto[,-c(9,10)])
dev.off()
png('boxplotagina.png')
par(mfrow=c(2,4))
boxplot(cylinders~mpg01,main="cylinders~mpg01")
boxplot(displacement~mpg01,main="displacement~mpg01")
boxplot(horsepower~mpg01,main="horsepower~mpg01")
boxplot(weight~mpg01,main="weight~mpg01")
boxplot(acceleration~mpg01,main="acceleration~mpg01")
boxplot(year~mpg01,main="year~mpg01")
boxplot(origin~mpg01,main="origin~mpg01")
dev.off()

## 11-(c)
set.seed(1886)
train <- sample(1:nrow(Auto), round(nrow(Auto)*0.8,0), replace = FALSE)
training <- Auto[train, ]
test <- Auto[-train,]
table(training$mpg01)
table(test$mpg01)

## 11-(d)
lda.fit = lda(mpg01 ~ displacement+horsepower+weight+cylinders, data = training)
lda.fit
lda.pred = predict(lda.fit, test)
lda.class = lda.pred$class
table(lda.class, test$mpg01)
mean(lda.class != test$mpg01)

## 11-(e)
qda.fit = qda(mpg01 ~ displacement+horsepower+weight+cylinders, data = training)
qda.pred = predict(qda.fit, test)
qda.class = qda.pred$class
mean(qda.class != test$mpg01)

## 11-(f)
glm.train.fit <- glm(mpg01 ~ displacement+horsepower+weight+cylinders, data = training, family = binomial)
glm.probs <- predict(glm.train.fit, test, type = "response")
glm.pred <- rep("0", nrow(test))
glm.pred[glm.probs > 0.5] <- "1"
mean(glm.pred != test$mpg01)

## 11-(g)
standardized.X = scale(Auto[, -c(9,10)])
train.X = standardized.X[train,c(2,3,4,5)]
test.X = standardized.X[-train,c(2,3,4,5)]
train.mpg01 = mpg01[train]
test.mpg01 = mpg01[-train]

set.seed(1886)
result <- c()
for(i in 1:15){
  knn.pred <- knn(train.X, test.X, train.mpg01, k = i)
  result[i] <- mean(knn.pred != test.mpg01)
}
df <- data.frame(K = 1:15, error = result)
best <- which(df$error == min(df$error))
df %>% ggplot(aes(K, error)) + geom_line() + geom_point() + theme_bw() + labs(title = "Test error by K from 1 to 15") +
  geom_text_repel(aes(label = paste0("(", K, ", ", round(error, 3), ")")) , data = df[best, ]) + geom_point(data=df[best,], size = 3, col = "red")
ggsave('best2.png')

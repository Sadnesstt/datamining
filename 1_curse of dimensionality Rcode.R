setwd('C:/Users/dnskd/Desktop/20Spring/datamining/week2/hw')
pacman::p_load(ggplot2, tidyverse, dplyr, plotly, processx)
rm(list=ls(all=TRUE))
setwd('C:/Users/dnskd/Desktop/20Spring/datamining/week2/hw')

#############################################
#############      HW4       ################
#############################################

# lab3 in ISL
##----------------Basic Commands----------------##
# concatenate : c()
x <- c(1, 3, 2, 5)
x

# save =
x = c(1, 6, 2)
x
y = c(1, 4, 3)

# sum
length(x)
length(y)
x + y

# ls() : allows us to look at a list of the objects, such as data and functions, that we saved so far.
# rm() : to delete any that we don't want
ls()
rm(x, y)
ls()
# remove all objects at once
rm(list=ls())

# matrix
x = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
x
x = matrix(c(1, 2, 3, 4), 2, 2)
# As default R creates matrices by successivley filling in columns.
# byrow = TRUE can be used to populate the matrix in order of the rows
matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE)

x^2
sqrt(x)
x = rnorm(50)
y = x + rnorm(50, mean = 50, sd = .1)
cor(x, y)

set.seed(1303)
rnorm(50)
set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

##------------- Graphics---------------##
# plot()
x = rnorm(100)
y = rnorm(100)
plot(x,y)
plot(x, y, xlab = "this is the x-axis", ylab = "this is the y-axis", main = "Plot of X vs Y")

pdf('figure.pdf')
plot(x, y, col = 'green')
dev.off()
# dev.off() indicates to R that we are done creating the plot. 

x = seq(1, 10)
x
x = 1:10
x
x = seq(-pi, pi, length.out = 50)
x


# contour() : represents three-dimensional data; topographical map
# three arguments : A vector of the x values, A vector of the y values, A matrix whose elements correspond to the z vlaue
y = x
f = outer(x, y, function(x, y) cos(y)/(1+x^2))
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)
fa = (f-t(f))/2
contour(x, y, fa, nlevels = 15)

# image() function works the same way as contour(), except that it produces a color-coded plot whose colors depend on the z value.
image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)

##----------indexing data----------------##
A = matrix(1:16, 4, 4)
A

A[2,3]

A[c(1, 3), c(2, 4)]
A[1:3, 2:4]
A[1:2, ]
A[, 1:2]
A[1, ]
A[-c(1,3), ]
A[-c(1, 3), -c(1, 3, 4)]
dim(A)

##---------------Loading Data-------------##
# import : read.table()
# export : write.table()
if(!require(ISLR)) install.packages('ISLR'); library(ISLR)
fix(Auto)
dim(Auto)

# na.omit() : simply remove na rows
Auto = na.omit(Auto)
dim(Auto)
names(Auto)

##---------------Additional Graphical and Numerical Summarise-------------##
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)

cylinders = as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col ='red')
plot(cylinders, mpg, col = 'red', varwidth=T)
plot(cylinders, mpg, col = 'red', varwidth=T, horizontal = T)
plot(cylinders, mpg, col = 'red', varwidth=T, xlab = "cylinders", ylab = "MPG")

hist(mpg)
hist(mpg, col=2)
hist(mpg, col=2, breaks = 15)

pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration , Auto)
plot(horsepower, mpg)
identify(horsepower, mpg, name)
summary(Auto)
summary(mpg)

##----------save-------------##
savehistory()
loadhistory()
q()


## ------------- HW2 ------------- ##
## myrand 함수 만들기 ##
myrand <- function(n, p){
  xx <- matrix(rnorm(n*p), nrow = n, ncol = p)
  S <- rowSums(xx^2)
  u <- runif(n)
  
  result <- (xx*(u^(1/p)))/sqrt(S)
  return(result)
}


## plot 그려보기 ##
# 2차원
library(ggplot2)
sample1 <- data.frame(myrand(1000, 2))
x <- seq(-1, 1, length=500)
y <- sqrt(1-x^2)

ggplot(sample1) + geom_point(aes(X1, X2)) + geom_path(aes(x=c(x, rev(x)), y=c(y, -y))) + theme_bw() + 
  labs(title = "Generating Uniformly Random Points", subtitle = "N = 1000, p = 2", x = expression(X[1]), y = expression(X[2])) + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = c(-1, 0, 1), limits = c(-1.2, 1.2)) + scale_y_continuous(breaks = c(-1, 0, 1), limits = c(-1.2, 1.2))
ggsave('2dim.png')

# 3차원
library(scatterplot3d)
sample2 <- data.frame(myrand(1000, 3))
png('3dim.png')
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
s3d <- scatterplot3d(sample2[,1:3], xlab = expression(X[1]), ylab = expression(X[2]), zlab = expression(X[3]),
              main = "Generating Uniformly Random Points", sub = "N = 1000, p = 3", pch = "",grid = FALSE, box = FALSE)
addgrids3d(sample2[, 1:3], grid = c("xy", "xz", "yz"))
s3d$points3d(sample2[, 1:3], pch = 16)
dev.off()


## ------------- HW3 ------------- ##

d <- function(p, n){
  (1-(1/2)^(1/n))^(1/p)
}


simulation <- function(n , p ,  n.sim = 10000){
  
  sim.res <- c()
  
  for(i in 1:n.sim){
    x <- myrand(n = n, p = p)
    dist1 <- sqrt(rowSums(x^2))
    min1 <- min(dist1)
    sim.res[i] <- min1
  }
  return(median(sim.res))
  
}


p <- c(5, 10, 15, 20, 30)
for(N in c(100, 500, 1000)) print(data.frame(p = p, dpn = d(p = p, n = N), sim = sapply(p, function(p) simulation(n = N, p = p, n.sim = 10000))))

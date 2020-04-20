pacman::p_load(ggplot2, tidyverse, dplyr, plotly, processx)
rm(list=ls(all=TRUE))


# HW3 - exercise 8
library(ISLR)
fix(Auto)
attach(Auto)
lm.fit <- lm(mpg ~ horsepower)
summary(lm.fit)

predict(lm.fit, data.frame(horsepower=c(98)), interval = "confidence")
predict(lm.fit, data.frame(horsepower=c(98)), interval = "prediction")

library(ggpmisc)
my.formula <- y ~ x
ggplot(aes(x = horsepower, y = mpg), data = Auto) +
  geom_smooth(method = "lm", se=FALSE, color="red",size = 1.5, formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, label.x = "right", label.y = "top") +         
  geom_point()+ labs(title = "Scatter with regression line between mpg and horsepower") + theme(plot.title = element_text(hjust = 0.5, size = 15)) + theme_bw()
ggsave("reg.png")

png("diag.png")
par(mfrow = c(2,2))
plot(lm.fit)
dev.off()

# HW3 - exercise 9
png("all.png")
pairs(Auto)
dev.off()

cor(Auto[,1:8])

lm.fit2 <- lm(mpg ~.-name, data = Auto)
summary(lm.fit2)

png("diag2.png")
par(mfrow = c(2,2))
plot(lm.fit2)
dev.off()

lm.fit3 <- lm(mpg ~.^2, data = Auto[,1:8])
sum_lm <- summary(lm.fit3)
sum_lm <- as.data.frame(sum_lm$coefficients)
sum_lm$name <- rownames(sum_lm)
write_csv(sum_lm, "summary.csv")

step(lm.fit2)
lm.fit4 <- lm(mpg ~ log(cylinders) + log(displacement) + log(horsepower) + log(weight) + log(year) + log(origin), data = Auto)
lm.fit5 <- lm(formula = mpg ~ cylinders + displacement + horsepower + weight + 
                year + origin, data = Auto)
lm.fit6 <- lm(mpg ~ sqrt(cylinders) + sqrt(displacement) + sqrt(horsepower) + sqrt(weight) + sqrt(year) + sqrt(origin),data = Auto)
lm.fit7 <- lm(mpg ~ I(cylinders^2) + I(displacement^2) + I(horsepower^2)+I(weight^2) + I(year^2) + I(origin^2), data = Auto)
data.frame(AIC = c(extractAIC(lm.fit4)[2], extractAIC(lm.fit5)[2],extractAIC(lm.fit6)[2],extractAIC(lm.fit7)[2]))

library(gridExtra)
ggplot(aes(mpg, cylinders), data = Auto) + geom_point()

#HW3 - exercise 13
set.seed(1)
X <- rnorm(100, 0, 1)
mean(X); sd(X)

eps <- rnorm(100, 0, sqrt(0.25))
mean(eps); sd(eps)

y <- -1 + 0.5*X + eps
mean(y); sd(y)

df <- data.frame(x = X, y = y)
ggplot(aes(x=x, y = y), data = df) + geom_point() + theme_bw() +
  labs(title = "Scatterplot of x and y") + theme(plot.title = element_text(hjust = 0.5))
ggsave('scatter.png')                                                                           

fit <- lm(y ~ X)
summary(fit)

my.formula = y ~ x
ggplot(aes(x = x, y = y), data = df) +
  geom_smooth(aes(col="regression"), method = "lm", se=FALSE, size = 1.5, formula = my.formula, show.legend = FALSE) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, label.x = "left", label.y = "top") +         
  geom_point()+ labs(title = "Scatter with regression line between x and y") + theme(plot.title = element_text(hjust = 0.5, size = 15)) + theme_bw()+
geom_abline(aes(col = "population", slope = 0.5, intercept = -1), size = 1.5) + scale_color_manual("model", values = c(1, 2))
ggsave('comp.png')

fit2 <- lm(y ~ x + I(x^2), data = df)
anova(fit, fit2)


#h
set.seed(1)
X <- rnorm(100, 0, 1)
eps <- rnorm(100, 0, sqrt(0.1))
mean(X); sd(X); mean(eps); sd(eps)
y <- -1 + 0.5 * X  + eps
df <- data.frame(x = X, y = y)

ggplot(aes(x=x, y = y), data = df) + geom_point() + theme_bw() +
  labs(title = "Scatterplot of x and y \nwith smaller error variance") + theme(plot.title = element_text(hjust = 0.5))
ggsave('last.png')

fit_1 <- lm(y ~ x, data =df)
summary(fit_1)

my.formula = y ~ x
ggplot(aes(x = x, y = y), data = df) +
  geom_smooth(aes(col="regression"), method = "lm", se=FALSE, size = 1.5, formula = my.formula, show.legend = FALSE) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, label.x = "left", label.y = "top") +         
  geom_point()+ labs(title = "Scatter with regression \n between x and y") + theme(plot.title = element_text(hjust = 0.5, size = 15)) + theme_bw()+
  geom_abline(aes(col = "population", slope = 0.5, intercept = -1), size = 1.5) + scale_color_manual("model", values = c(1, 2))
ggsave('comp2.png')

#i
set.seed(1)
X <- rnorm(100, 0, 1)
eps <- rnorm(100, 0, sqrt(0.5))
mean(X); sd(X); mean(eps); sd(eps)
y <- -1 + 0.5 * X  + eps
df <- data.frame(x = X, y = y)

ggplot(aes(x=x, y = y), data = df) + geom_point() + theme_bw() +
  labs(title = "Scatterplot of x and y \nwith bigger error variance") + theme(plot.title = element_text(hjust = 0.5))
ggsave('scatter_big.png')

fit_1 <- lm(y ~ x, data =df)
summary(fit_1)

my.formula = y ~ x
ggplot(aes(x = x, y = y), data = df) +
  geom_smooth(aes(col="regression"), method = "lm", se=FALSE, size = 1.5, formula = my.formula, show.legend = FALSE) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, label.x = "left", label.y = "top") +         
  geom_point()+ labs(title = "Scatter with regression \n between x and y") + theme(plot.title = element_text(hjust = 0.5, size = 15)) + theme_bw()+
  geom_abline(aes(col = "population", slope = 0.5, intercept = -1), size = 1.5) + scale_color_manual("model", values = c(1, 2))
ggsave('comp3.png')


#j
my_function <- function(var){
  set.seed(1)
  x <- rnorm(100, 0, 1)
  eps <- rnorm(100, 0, sqrt(var))
  y <- -1 + 0.5*x +eps
  fit <- lm(y ~ x)
  result = confint(fit)
  return(result)
}
var <- c(0.25, 0.1, 0.5)
sapply(var, function(var) my_function(var))



# HW3 - exercise 14
#a.
set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100)/10
y <- 2 + 2*x1+0.3*x2+rnorm(100)

#b.
cor(x1, x2)
df <- data.frame(yy = y, x1 = x1, x2= x2)
ggplot(aes(x=x1, y=x2), data=df) + geom_point()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Scatterplot of X1 and X2")+
  theme_bw()
ggsave('plot1.png')

#c.
fit <- lm(yy ~ x1 + x2, data =df)
summary(fit)

#d.
lm_fit <- lm(yy ~ x1, data = df)
summary(lm_fit)

#e.
lm_fit2 <- lm(yy ~ x2, data = df)
summary(lm_fit2)

#g.
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)
df <- data.frame(y = y, x1 = x1, x2 = x2)

#g-c
fit <- lm(y ~ x1 + x2, data = df)
summary(fit)
png("fit1.png")
par(mfrow = c(2,2))
plot(fit)
dev.off()

#g-d
lm_fit <- lm(y ~ x1, data = df)
summary(lm_fit)
png("fit2.png")
par(mfrow = c(2,2))
plot(lm_fit)
dev.off()

#g-e
lm_fit2 <- lm(y ~ x2, data = df)
summary(lm_fit2)
png("fit3.png")
par(mfrow = c(2,2))
plot(lm_fit2)
dev.off()
options(digits=3)

########################################################
## A classic example: Anscombe's quartet!

anscombe <- read.csv("D:/stat 315/anscombe.csv")

summary(lm(y1~x1, data=anscombe))
summary(lm(y2~x2, data=anscombe))
summary(lm(y3~x3, data=anscombe))
summary(lm(y4~x4, data=anscombe))

# but are the 4 datasets really all the same?
# set-up space for 4 plots on a page:
par(mfrow=c(2,2))
plot(y1~x1, data= anscombe)
plot(y2~x2, data= anscombe)
plot(y3~x3, data= anscombe)
plot(y4~x4, data= anscombe)


########################################################
## A classic example from the first class: Age vs. Money

x1 <- c(82, 45, 71, 22, 29, 9, 12, 18, 24)
x2 <- c(22, 44, 31, 122, 20, 0, 2, 10, 35)
y <- c(71, 54, 43, 45, 21, 11, 30, 45, 10)




lm(y~x1+x2)
lm(y~I(log(x1))+x2)

plot(y~(x1))
plot(y~log(x1))
plot(log(y)~x1)
plot(log(y)~log(x1))



########################################################
## Example for serial correlation: prices over time


price_data <- read.csv("D:/stat 315/price_data.csv")

n <- dim(price_data)[1]

plot(y~location, data=price_data)

lmod <- lm(y~location, data=price_data)
summary(lmod)


res <- lmod$residuals
plot(res~time,data= price_data)


## shift by one timepoint:
cbind(res[-1],res[-n])

## correlation at a lag of one timepoint
cor1 <- cor(res[-1],res[-n])
cor1
sum(res[-1]*res[-n])/sqrt(sum(res[-1]^2)*sum(res[-n]^2))

## scatterplot
plot(cbind(res[-1],res[-n]))

DW <- sum((res[-1] - res[-n])^2)/sum(res^2)
DW

#approx equal to:
2-2*cor1

library(lmtest)
dwtest(lmod)
.


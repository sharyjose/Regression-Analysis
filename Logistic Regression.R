####  "simple logistic regression"  ####

x <- c(82, 45, 71, 22, 29, 9, 12, 18, 24)
y <- c(1, 1, 1, 0, 0, 0, 0, 1, 0)
mod1<-glm(y~x, family="binomial")
summary(mod1)

####  plot  ####

Z <- cbind(1,1:100)%*%coef(mod1)
fitted_line <- 1/(1+exp(-Z))

plot(y~x, pch=20, cex=3, xlim=c(0,100), ylim=c(0,1))
lines(1:100, fitted_line, lwd=3)

####  "multiple logistic regression"  ####

x1 <- c(82, 45, 71, 22, 29, 9, 12, 18, 24)
x2 <- c(0, 0, 1, 1, 0, 0, 1, 1, 0)
y <- c(1, 1, 1, 0, 0, 0, 0, 1, 0)
mod2<-glm(y~x1+x2, family="binomial")
summary(mod2)


####  "multiple logistic regression with interaction effect"  ####

x1 <- c(82, 45, 71, 22, 29, 9, 12, 18, 24)
x2 <- c(0, 0, 1, 1, 0, 0, 1, 1, 0)
y <- c(1, 1, 1, 0, 0, 0, 0, 1, 0)
mod3 <- glm(y~x1*x2, family="binomial")
summary(mod3)


########################################################
###  Try the 3 models with a bit more data:
########################################################

x1 <- c(82, 45, 71, 22, 29, 9, 12, 18, 24, 51, 37, 66, 83, 28, 74, 30, 44, 77)
x2 <- c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1)
y <- c(1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1)


mod1<-glm(y~x1, family="binomial")
summary(mod1)

####  plot  ####
Z <- cbind(1,1:100)%*%coef(mod1)
fitted_line <- 1/(1+exp(-Z))
plot(y~x1, pch=20, cex=3, xlim=c(0,100), ylim=c(0,1))
lines(1:100, fitted_line, lwd=3)

mod2<-glm(y~x1+x2, family="binomial")
summary(mod2)


mod3 <- glm(y~x1*x2, family="binomial")
summary(mod3)


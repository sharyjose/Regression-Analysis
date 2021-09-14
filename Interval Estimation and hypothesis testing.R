
# This question concerns a galaxy data set on Hubble’s law from...

distance <- c(22,68,108,137,255,315,390,405,685,700,1100)
velocity <- c(1.21,3.86,5.15,7.56,15.0,19.3,21.6,23.2,39.4,41.8,61.2)

mydata <- data.frame(distance, velocity)
rownames(mydata) <- c("Virgo", "Pegasus", "Perseus", "Coma Berenices",
					"Ursa Major No.1", "Leo", "Corona Borealis",
					"Gemini", "Bootes", "Ursa Major No.2", "Hydra")
					
print(mydata)

x <- distance
y <- velocity

n <- length(y)
dim(mydata)
reg <- lm(y~x)
summary(reg)
reg$coef
b0 <- reg$coef[1]
b1 <- reg$coef[2]
c(b0,b1)

xbar <- (1/n)*sum(x)
ybar <- (1/n)*sum(y)
sxy <- (1/(n-1))*sum((x-xbar)*(y-ybar))
rxy <- sxy/(sx*sy)
beta1hat <- rxy*sy/sx
beta0hat <- ybar-beta1hat*xbar
residuals <- y - beta0hat - beta1hat*x
s <- sqrt( (1/(n-2))*sum(residuals^2))

myx <- 300
muhat_x <- beta0hat+beta1hat*myx
muhat_x
lowerPI <- muhat_x - qt(0.975,n-2) * s * sqrt(1+1/n + ((myx-xbar)^2)/((n-1)*sx^2))
upperPI <-  muhat_x + qt(0.975,n-2) * s * sqrt(1+1/n + ((myx-xbar)^2)/((n-1)*sx^2))
lowerPI
upperPI

##
n<- 11
xbar <- (1/n)*sum(x)
ybar <- (1/n)*sum(y)
sx <- sqrt( sum((x-xbar)^2)/(n-1) )
sy <- sqrt( sum((-ybar)^2)/(n-1) )
sxy <- (1/(n-1))*sum((x-xbar)*(y-ybar))
rxy <- sxy/(sx*sy)
beta1hat <- rxy*sy/sx
beta0hat <- ybar-beta1hat*xbar
residuals <- y - beta0hat - beta1hat*x
s <- sqrt( (1/(n-2))*sum(residuals^2))

plot(y~x, xlim=c(0,100), ylim=c(0,100), pch=20, cex=3)
abline(beta0hat, beta1hat)


myx <- 300
muhat_x <- b0+b1*myx
muhat_x
lowerCI <- muhat_x - qt(0.975,n-2) * s * sqrt(1/n + ((myx-xbar)^2)/((n-1)*sx^2))
#se<-s * sqrt(1/n + ((myx-xbar)^2)/((n-1)*sx^2))
lowerCI <- muhat_x - qt(0.975,n-2) * s * sqrt(1/n + ((myx-xbar)^2)/((n-1)*sx^2))
upperCI <-  muhat_x + qt(0.975,n-2) * s * sqrt(1/n + ((myx-xbar)^2)/((n-1)*sx^2))
lowerCI
upperCI

# (a) Obtain summary statistics:

xbar <- (1/n)*sum(x)
sx <- sqrt( sum((x-xbar)^2)/(n-1) )
ybar <- (1/n)*sum(y)
sy <- sqrt( sum((y-ybar)^2)/(n-1) )
sxy <- (1/(n-1))*sum((x-xbar)*(y-ybar))
rxy <- sxy/(sx*sy)

xbar
sx
ybar
sy
sxy
rxy


# (b) Obtain the slope and intercept of the least squares line:

b1_hat <- rxy*sy/sx
b1_hat

b0_hat <- ybar-b1_hat*xbar
b0_hat

# (c) Compute the residuals and the residual standard deviation

yhat <- b0_hat + b1_hat*x
y
residuals <- y - b0_hat - b1_hat*x
residuals
s <- sqrt( (1/(n-2))*sum(residuals^2) )
s

# Should be equal to zero:
sum(residuals)
mean(residuals)

# (d) Obtain the standard error for b1_hat

SE_b1 <- s/(sqrt(n-1)*sx)
SE_b1

# (e) Obtain a 95% C.I. for beta1

b1_CI95 <- c(b1_hat - qt(0.975,n-2)*(SE_b1) , 
			 b1_hat + qt(0.975,n-2)*(SE_b1))
b1_CI95

# (f) Plot the data and draw the lesat squares line...

plot(y~x)

plot(y~x, xlab="distance (millions-of-light-years)",
		 ylab="velocity (thousands-of-km/sec)",
		 pch = 20,
		 cex = 4)

abline(b0_hat, b1_hat, col="red")
# add labels:
text(x,y, rownames(mydata), adj=1)

# (g) Does the assumption of homoscedasticity seem reasonable?

plot(residuals~distance)
abline(0,0)

# or:

yhat <- b0_hat + b1_hat*x
plot(residuals~yhat)
abline(0,0)

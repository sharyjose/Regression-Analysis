
############################################################
# example "from introductory statistics..."
############################################################

# "n" is the number of samples:
n <- 9

# mu and sigma2 are population parameters:
# let's pretend that we know the values of these parameters:
mu <- 0
sigma2 <- 1

# sample, "y" from a normal distribution:
y <- rnorm(n=9, mean=mu, sd=sqrt(sigma2))

# sample statistics:
ybar <- (1/n)*sum(y)
s <- sqrt( ((y-ybar)^2)/(n-1) )

# Think about ybar as a random variable "YBAR"
# Then we can talk about E[YBAR] and Var[YBAR]
# Then se(ybar) is an estimate of sqrt(Var[YBAR])

# So what is the Var[YBAR] ?
# let's repeat the study (considered a "random phenonmenon")
# and consider the variance of the different "ybar" estimates we get: 


y <- rnorm(n=9, mean=mu, sd=sqrt(sigma2))
ybar <- (1/n)*sum(y)
print(ybar)

# Repeat the "random phenomenon" 1000 times:

ybar_vector <- NULL
for(i in 1:1000){
y <- rnorm(n=9, mean=mu, sd=sqrt(sigma2))
ybar <- (1/n)*sum(y)
print(ybar)
ybar_vector <- c(ybar_vector, ybar)
}

# The variance of the random variable YBAR
# can be estimated from our 1000 studies:
var(ybar_vector)

# How does this compare to what the theory says:
sigma2/n

############################################################
# Linear regression example
############################################################
# x and n are fixed values
x <- c(82, 45, 71, 22, 29, 9, 12, 18, 24)
n <- 9

# y is a realization of the random variable "Y", i.e. "observed data":
y <- c(71, 54, 43, 45, 21, 11, 30, 45, 10)

plot(y~x, xlim=c(0,100), ylim=c(0,100), pch=20, cex=3)

# beta0, beta1, and sigma2 are population parameters
# let's pretend that we know the values of these parameters:
beta0 <- 20
beta1 <- 0.5
sigma2 <- 100

############################################################
# Now we introduce the random variables:

# epsilon (unknown) is a random variable
epsilon <- rnorm(n, mean=0, sd=sqrt(sigma2))

# Y (unknown) is a random variable
Y <- beta0 + beta1*x + epsilon

# Sample statistics (also known as "estimators") 
# can be considered as random variables:

# sample means:
xbar <- (1/n)*sum(x)
ybar <- (1/n)*sum(Y)

# sample standard deviations:
sx <- sqrt( sum((x-xbar)^2)/(n-1) )
sy <- sqrt( sum((Y-ybar)^2)/(n-1) )

# sample covariance and sample correlation:
sxy <- (1/(n-1))*sum((x-xbar)*(Y-ybar))
rxy <- sxy/(sx*sy)

# best estimators for beta0 and beta1 parameters
beta1hat <- rxy*sy/sx
beta0hat <- ybar-beta1hat*xbar

residuals <- y - beta0hat - beta1hat*x
s <- sqrt( (1/(n-2))*sum(residuals^2))

# Let's plot another a realization of the random variable "Y"

points(x,Y,pch=20,cex=4, col=rgb(0.2,0.7,0.25,alpha=0.10))
abline(beta0hat, beta1hat, col=rgb(0,0,1,alpha=0.50), lwd=3)

# Repeat the random phenomenon 1000 times:
beta_matrix <- matrix(0,1000,2)
for(i in 1:1000){
epsilon <- rnorm(n, mean=0, sd=sqrt(sigma2))
Y <- beta0 + beta1*x + epsilon
xbar <- (1/n)*sum(x)
ybar <- (1/n)*sum(Y)
sx <- sqrt( sum((x-xbar)^2)/(n-1) )
sy <- sqrt( sum((Y-ybar)^2)/(n-1) )
sxy <- (1/(n-1))*sum((x-xbar)*(Y-ybar))
rxy <- sxy/(sx*sy)
beta1hat <- rxy*sy/sx
beta0hat <- ybar-beta1hat*xbar
points(x,Y, pch=20,cex=4, col=rgb(0.2,0.7,0.25,alpha=0.10))
abline(beta0hat, beta1hat, col=rgb(0,0,1,alpha=0.10), lwd=3)
beta_matrix[i,] <- c(beta0hat, beta1hat)
}


# plot the 95% confidence interval for a series of subpopulation means:
# this should look like a confidence interval for the regr
for(myx in c(0,10,20,30,40,50,60,70,80,90,100)){
muhat_x <- beta0+beta1*myx
muhat_x
lowerCI <- muhat_x - qt(0.975,n-2) * s * sqrt(1/n + ((myx-xbar)^2)/((n-1)*sx^2))
upperCI <-  muhat_x + qt(0.975,n-2) * s * sqrt(1/n + ((myx-xbar)^2)/((n-1)*sx^2))

points(myx, lowerCI, pch="-", cex=8, col="lightblue")
points(myx, upperCI, pch="-", cex=8, col="lightblue")
}

# plot the variance for our different values of x:
for(myx in x){
lines(c(myx,myx),c((beta0 + beta1*myx)-sqrt(sigma2),(beta0 + beta1*myx)+sqrt(sigma2)),col="red",lwd=4)
}




# The variance of the random variable B1
# can be estimated from our 1000 studies:
var(beta_matrix[,2])

# How does this compare with what the theory says: 
sigma2/((n-1)*sx^2)



###############
# Derivations for the "subpopulation mean"

# x and n are fixed values
x <- c(82, 45, 71, 22, 29, 9, 12, 18, 24)
n <- 9

# y is a realization of the random variable "Y", i.e. "observed data":
y <- c(71, 54, 43, 45, 21, 11, 30, 45, 10)
xbar <- (1/n)*sum(x)
ybar <- (1/n)*sum(y)
sxy <- (1/(n-1))*sum((x-xbar)*(y-ybar))
rxy <- sxy/(sx*sy)
beta1hat <- rxy*sy/sx
beta0hat <- ybar-beta1hat*xbar
residuals <- y - beta0hat - beta1hat*x
s <- sqrt( (1/(n-2))*sum(residuals^2))

plot(y~x, xlim=c(0,100), ylim=c(0,100), pch=20, cex=3)
abline(beta0hat, beta1hat)


myx <- 20
muhat_x <- b0+b1*myx
 muhat_x
lowerCI <- muhat_x - qt(0.975,n-2) * s * sqrt(1/n + ((myx-xbar)^2)/((n-1)*sx^2))
upperCI <-  muhat_x + qt(0.975,n-2) * s * sqrt(1/n + ((myx-xbar)^2)/((n-1)*sx^2))
lowerCI
upperCI

points(myx, lowerCI, pch="-", cex=8, col="blue")
points(myx, upperCI, pch="-", cex=8, col="blue")

#prediction interval
myx <- 20
muhat_x <- beta0hat+beta1hat*myx
muhat_x
lowerCI <- muhat_x - qt(0.975,n-2) * s * sqrt(1+1/n + ((myx-xbar)^2)/((n-1)*sx^2))
upperCI <-  muhat_x + qt(0.975,n-2) * s * sqrt(1+1/n + ((myx-xbar)^2)/((n-1)*sx^2))
lowerPI
upperPI



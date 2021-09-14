data("mtcars") 
head(mtcars) #first few entries of the data set mtcars
dim(mtcars) #number of rows and columns

plot(mpg~hp, data = mtcars, cex=3, pch=18, col="blue")

linear_reg<-function(y,X){
  
  # First, define our model dimensions:
  n <- dim(X)[1]
  k <- dim(X)[2]
  p <- k-1
  
  # Obtain basic summary statistics:
  ybar <- (1/n)*sum(y)
  sy <- sqrt( sum((y-ybar)^2)/(n-1) )
  
  # Next, use equation 3.11 to obtain coefficient estimates:
  betahat <- solve(t(X) %*% X) %*% t(X) %*% y
  
  # Obtain "fitted values" (3.39) and residuals (3.40):
  yhat <- X%*%betahat
  residuals <- yhat-y
  
  # Calculate all the different "SS"s (equations 3.41 - 3.44):
  SS_Total <- sum((y-ybar)^2)
  SS_Res <- sum(residuals^2)
  MS_Res <- SS_Res/(n-k)
  
  # Calculate the R-squared and adjusted R-squared:
  # see 3.45 and 3.46
  R2 <- 1 - SS_Res/SS_Total
  adjR2 <- 1 - (SS_Res/(n-k))/(SS_Total/(n-1))
  
  
  # Now, for each beta, we calculate SE and confidence interval.
  se_betahat<-rep(NA,k)
  tratio<-rep(NA,k)
  moe<-rep(NA,k)
  pvalue<-rep(NA,k)
  ci_lower_beta<-rep(NA,k)
  ci_upper_beta<-rep(NA,k)
  for(i in 1:k){
    se_betahat[i]	<-sqrt(MS_Res)*sqrt(solve(t(X)%*%X)[i,i])
    moe[i]	<-qt(0.975, n-k)*sqrt(MS_Res)*sqrt(solve(t(X)%*%X)[i,i])
    ci_lower_beta[i]	 <-betahat[i]-moe[i]
    ci_upper_beta[i]	 <-betahat[i]+moe[i]
    tratio[i]	<-betahat[i]/se_betahat[i]
    pvalue[i]	<-2*(1-pt(abs(tratio[i]), n-k))
  }
  
  # All these results together in a table:
  coeftable <- 
    round(data.frame(
      betahat, se_betahat, tratio, ci_lower_beta, ci_upper_beta, pvalue)
      ,3)
  
  # Bonus: ingredients for F-test (not in course notes), see:
  # http://analyticspro.org/2016/03/15/r-tutorial-how-to-interpret-f-statistic-in-regression-models/
  
  Fstatistic<- (R2/(k-1))/((1-R2)/(n-k))
  Ftest_pval<- pf(Fstatistic,p,n-k,lower.tail=FALSE)
  
  # Sums of squares decomposition table (see section 3.4)
  SStable <- 
    round(data.frame(
      SS_Total, 
      SS_Res, 
      MS_Res, 
      sqrt(MS_Res), 
      R2, 
      adjR2, 
      Fstatistic, 
      Ftest_pval)
      ,3)
  
  # return all the important information for our model:
  return(list(coeftable= coeftable, SStable= SStable))}

#summary(lm(mtcars$mpg~mtcars$hp))
#define outcome variables
carsy <- mtcars$mpg
carsx <- cbind(1, mtcars$hp)

#--- and use linear regression
carsmod <- linear_reg(carsy,carsx)
carsmod


model1<-lm(mtcars$mpg~mtcars$hp)
model1$fitted
vcov(model1)

abline(30.099,-0.068, col="darkblue", lwd=4)

yhat <- carsx%*%c(30.099, -0.068)
residual <- carsy-yhat
plot(residual~carsy)
abline(0,0)

#add a "quadratic term" to design matrix
carsX <- cbind(1, mtcars$hp, mtcars$hp^2)
carsX

carsmodQ <- linear_reg(carsy,carsX)
carsmodQ

#scale by a factor of 100 and add a "quadratic term"
carsX <- cbind(1,mtcars$hp/100, (mtcars$hp/100)^2)

carsmodQ <- linear_reg(carsy,carsX)
carsmodQ

plot(mpg~hp, data = mtcars, cex=3, pch=18, col="blue") #colinear
yhat <- carsX%*%c(40.409,-21.331,4.208)
points(mtcars$hp, yhat, col="orange", pch=20,cex=3)


residual <- carsy-yhat
plot(residual~carsy)
abline(0,0)

# add "am" -- Automatic (A) vs. Manual (M) covarite
carsy <- mtcars$mpg
carsX <- cbind(1,mtcars$hp/100, (mtcars$hp/100)^2, mtcars$am)
linear_reg(carsy,carsX)



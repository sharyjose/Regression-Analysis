# "linear_reg" is a function whit two inputs 
# "y" the "response variable"
# "X" the "design matrix"


linear_reg<-function(y,X){

# First, define our model dimensions:
n <- dim(X)[1]
k <- dim(X)[2]
p <- k-1

# Obtain basic summary statistics:
ybar <- (1/n)*sum(y)
sy <- sqrt( sum((y-ybar)^2)/(n-1) )

# Next, obtain coefficient estimates:
betahat <- solve(t(X) %*% X) %*% t(X) %*% y



# Obtain "fitted values"  and "residuals":
yhat <- X%*%betahat
residuals <- yhat-y

# Calculate all the different "SS"s:
SS_Total <- sum((y-ybar)^2)
SS_Res <- sum(residuals^2)
MS_Res <- SS_Res/(n-k)

# Calculate the R-squared and adjusted R-squared:
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

# Ingredients for F-test see:
# http://analyticspro.org/2016/03/15/r-tutorial-how-to-interpret-f-statistic-in-regression-models/

Fstatistic<- (R2/(k-1))/((1-R2)/(n-k))
Ftest_pval<- pf(Fstatistic,p,n-k,lower.tail=FALSE)

# Sums of squares decomposition table 
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


########################################################
########################################################
### Two examples from lecture:

########################################################
### EXAMPLE 1:
# x is age as a category (0="old", 1="young")
# y is cash on hand
x <- c(0, 0, 0, 1, 1, 1, 1, 1, 1)
y <- c(71, 54, 43, 45, 21, 11, 30, 45, 10)

# We must always add a column of 1s:
linear_reg(y, cbind(1,x))

# Notice that we obtain the same 
# p-value and t-ratio statistic 
# as with a standard t-test:
old <- y[x==0]
young <- y[x==1]
t.test(x= young, y= old, var.equal=TRUE)


# Now we consider "age" as a continuous variable, x1 (years)
# and also "income", as a continuous variable, x2 (K$)
x1 <- c(82, 45, 71, 22, 29, 9, 12, 18, 24)
x2 <- c(26, 49, 76, 37, 40,  0,  2, 10, 92)

linear_reg(y, cbind(1,x1, x2))


########################################################
### EXAMPLE 2:
### y is continuous variable
### country is categorical variable

y <- c(23,25,21,32,16,23,15,10,8,9,6,12,13,13,12,21,16,10)
country <- c(
"France", "France", "France", 
"France", "France", "France", 
"England", "England", "England",
"England", "England", "England",
"Thailand", "Thailand", "Thailand",
"Thailand", "Thailand", "Thailand")

gender <- c("M", "F", "M", "F", "M", "F", "F", "F", "M", "M", "M", "M", "M", "M","M", 
            "F", "F", "M")


# together in a data.frame:
mydata <- data.frame(y, country,gender)

# Typically one could use "ANOVA":
summary(aov(y~country, data=mydata))

# Alternatively, one could re-code country 
# with binary "dummy" variables...
x1 <- as.numeric(country=="France")
x2 <- as.numeric(country=="Thailand")
x3<- as.numeric(gender=="M")
myX<-cbind(1,x1,x2,x3)
myX
model1<-lm(y~myX)

vcov(model1)
# ...and use linear regression:
linear_reg(y, myX)

######france as reference cory
x1 <- as.numeric(country=="England")
x2 <- as.numeric(country=="Thailand")
myX<-cbind(1,x1,x2)
myX
model1<-lm(y~myX)

vcov(model1)
# ...and use linear regression:
linear_reg(y, myX)




# Cross-validation for Burnaby condominium data to compare
# 11-variable and 9-variable models (best from regsubsets() in leaps)
# variable list: ffarea beds baths sqfl view age mfee , +
# imetrotown,  age^2, ffarea:sqfl, ffarea:age, sqfl:age

b<-read.table("bcondo-161116.txt",header=T,skip=2)
b<-b[,2:10]
b$askprice<-b$askprice/10000
b$ffarea<-b$ffarea/100
b$mfee<-b$mfee/10
b$sqfl<-sqrt(b$floor)
# transforms with centered variables
b$ffac<-b$ffarea-1000; b$sqfc<-b$sqfl-3
b$agec<-b$age-15; b$bedc<-b$beds-2
b$agec2<-b$agec^2; b$ffsqf<-b$ffac*b$sqfc
b$ffage<-b$ffac*b$agec; b$sqfage<-b$sqfc*b$agec
n<-nrow(b)
b$imetr<-rep(0,n)
imetr<-(b$region=="metrotown")
b$imetr[imetr]<-1  # 1 if region is metrotown, 0 otherwise


fit_11<-lm(askprice~ffac+sqfc+agec+bedc+baths+view+mfee+ffsqf+ffage+sqfage+imetr, data=b)
fit_9<-lm(askprice~ffac+sqfc+agec+bedc+baths+ffsqf+ffage+sqfage+imetr, data=b)


summ11<-summary(fit_11); summ9<-summary(fit_9)
print(summ11); print(summ9)

# loop deleting one observation at a time 
cat("\nLeave-one-out Cross-validation\n")
cvss11<-0; cvss9<-0
y<-b$askprice; n<-nrow(b); cvres9<-rep(0,n)
for(i in 1:n)
{ cvfit11<-lsfit(Xmat11[-i,],y[-i])  # delete ith observation 
  cvfit9<-lsfit(Xmat9[-i,],y[-i])  # delete ith observation 
  bvec11<-cvfit11$coef; bvec9<-cvfit9$coef
  ypred11<-sum(bvec11*c(1,Xmat11[i,]))  # prediction of omitted y based on its x
  ypred9<-sum(bvec9*c(1,Xmat9[i,]))
  cvres9[i]<-y[i]-ypred9
  cat("i=",i, " y= ", y[i], "  ypred11=", ypred11, "  ypred9=", ypred9,
     "cvres9 ", cvres9[i],"\n")
  cvss11<-cvss11+(y[i]-ypred11)^2; cvss9<-cvss9+(cvres9[i])^2
}

cat("\nCross-validated SS for models with 11 and 9 explanatory variables\n")
cat(cvss11,cvss9,"\n")                 #  2443.791 2450.994
print(sqrt(c(cvss11,cvss9)/n))         #   6.228188 6.237360
# compare with residual SDs, cross-validated rmse usually a little larger
cat("residualSDs\n")
cat(summ11$sigma,summ9$sigma,"\n")     #  5.358771 5.440099

# Compute cross-validated root mean squared error of prediction.
# ls.out is a fitted regression model from lsfit or lm.
ls.cvrmse <- function(ls.out)
{ res.cv <- ls.out$residuals / (1.0 - ls.diag(ls.out)$hat)
  # above formula to be explained in class
  is.na.res <- is.na(res.cv)  # Identify NA's and remove them.
  res.cv <- res.cv[!is.na.res]
  cvrmse <- sqrt(sum(res.cv^2) / length(res.cv))
  return(cvrmse)
}


# compare with ls.cvrmse(); not necessary to loop through the regressions
cat("\nCross-validation root mean square error using ls.cvrmse function\n")
cvrmse11<-ls.cvrmse(fit_11); cvrmse9<-ls.cvrmse(fit_9)
cat(cvrmse11,cvrmse9,"\n") #  6.228188 6.23736

# Model with 11 explanatory variables is better based on leave-one-out
# Some of the outputs of ls.diag() are explained in Section 4.3
fit9.diag<-ls.diag(fit_9); names(fit9.diag)
# [1] "std.dev"      "hat"          "std.res"      "stud.res"     "cooks"       
# [6] "dfits"        "correlation"  "std.err"      "cov.scaled"   "cov.unscaled"


#============================================================
# training set/ holdout set RMS prediction error
set.seed(123) # set seed so that random subsets will be same on re-running code
n<-nrow(b) # 63
iperm<-sample(n,n)  # random permutation of 1:n
train<-b[iperm[1:37],]  # random subset of 37 for training set
holdout<-b[iperm[38:n],]  # remainder is holdout set
fit11train<-lm(askprice~ffac+sqfc+agec+bedc+baths+view+mfee+ffsqf+ffage+sqfage+imetr,
data<-train)
fit9train<-lm(askprice~ffac+sqfc+agec+bedc+baths+ffsqf+ffage+sqfage+imetr,
data=train)
pred11hold<-predict(fit11train,new=holdout)
pred9hold<-predict(fit9train,new=holdout)

rmse11hold<-sqrt(mean((holdout$askprice-pred11hold)^2))
rmse9hold<-sqrt(mean((holdout$askprice-pred9hold)^2))
cat("\nSize of holdout set is ", nrow(holdout),"\n") # 26
cat("Cross-validation root mean square error with holdout set\n")
print(c(rmse11hold,rmse9hold))
#  5.845358 6.159125
# These can vary a little, depending on the training/holdout split.
# But generally, these are larger than respective values of residualSD.


##############################################################
# Using cv.glm() to find the CVRMSE_leaveoneout/RMSE_holdout
# This function calculates the estimated K-fold cross-validation 
# prediction error for generalized linear models.


library(boot)

k = 10 # K - fold cross validation
mod_loo_9 <- cv.glm(b, fit9,K=k)
mod_loo_11 <- cv.glm(b, fit11,K=k)
names(mod_loo_9)
# [1] "call"  "K"     "delta" "seed" 
cat("RMSE\n")
cat(sqrt(mod_loo_11$delta[1]), sqrt(mod_loo_9$delta[1]), "\n")

cat("residualSDs\n")
cat(summ11$sigma,summ9$sigma,"\n")





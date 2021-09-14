# burnaby condo: find best subsets (size 1,2,...) of explanatory variables 
# variable list: ffarea beds baths sqfl view age mfee , +
# imetrotown,  age^2, ffarea:sqfl, ffarea:age, sqfl:age
b<-read.table('D:/stat 315/bcondo-161116.txt',header=T,skip=2)
b<-b[,2:10]
summary(b)
b$askprice<-b$askprice/10000
b$ffarea<-b$ffarea/100
b$mfee<-b$mfee/10
b$sqfl<-sqrt(b$floor)
# add transforms with centered variables
b$ffac<-b$ffarea-1000
b$sqfc<-b$sqfl-3
b$agec<-b$age-15
b$bedc<-b$beds-2
b$agec2<-b$agec^2
b$ffsqf<-b$ffac*b$sqfc
b$ffage<-b$ffac*b$agec
b$sqfage<-b$sqfc*b$agec
n<-nrow(b)
b$imetr<-rep(0,n)
imetr<-(b$region=="metrotown")
b$imetr[imetr]<-1  # 1 if region is metrotown, 0 otherwise

library(leaps)
  #  leaps() performs an exhaustive search for the best subsets of the
#     variables in x for predicting y in linear regression, using an
#     efficient branch-and-bound algorithm. 
# Original source: Furnival and Wilson (1974), Regression by
#     leaps and bounds, Technometrics, v 16, pp 499-511.
# regsubsets               package:leaps      
#    Model selection by exhaustive search, forward or backward
#    stepwise, or sequential replacement

options(digits=5)
fit<-lm(askprice~ffac+sqfc+agec+bedc+baths+view+mfee+agec2+ffsqf+ffage+sqfage+imetr, data=b)
summ<-summary(fit)
print(summ)

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.44e+03   7.51e+02    4.58  3.1e-05 ***
#ffac         3.42e+00   7.56e-01    4.53  3.7e-05 ***
#sqfc         1.06e+03   2.70e+02    3.91  0.00028 ***
#agec        -1.28e+02   3.18e+01   -4.01  0.00020 ***
#bedc         4.70e+00   2.45e+00    1.92  0.06024 .  
#baths        4.22e+00   2.37e+00    1.78  0.08097 .  
#view         2.41e+00   1.70e+00    1.42  0.16172    
#mfee        -1.26e-01   1.35e-01   -0.93  0.35445    
#agec2       -4.86e-03   6.73e-03   -0.72  0.47363    
#ffsqf        1.06e+00   2.72e-01    3.90  0.00029 ***
#ffage       -1.28e-01   3.21e-02   -3.99  0.00022 ***
#sqfage      -1.64e-01   4.59e-02   -3.56  0.00081 ***
#imetr        6.33e+00   2.21e+00    2.86  0.00615 ** 
#Residual standard error: 5.38 on 50 degrees of freedom
#Multiple R-squared:  0.954,     Adjusted R-squared:  0.943 



cat("\nexhaustive\n") # default method="exhaustive"
out.exh<-regsubsets(askprice~ffac+sqfc+agec+bedc+baths+view+mfee+agec2+ffsqf+ffage+sqfage+imetr, data=b,nbest=1,nvmax=12)
summ.exh<-summary(out.exh)
#names(summ.exh)
#[1] "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"
print(summ.exh$outmat)
cat("Cp = SS(Res;subset)/MS(Res:full) + 2*ncol(Xmat) - n: smaller is better\n")
print(summ.exh$cp)
cat("adjr: larger is better\n")
print(summ.exh$adjr)

cat("\nbackward\n")
out.back<-regsubsets(askprice~ffac+sqfc+agec+bedc+baths+view+mfee+agec2+ffsqf+ffage+sqfage+imetr, data=b,method="backward",nvmax=12)
summ.back<-summary(out.back)
print(summ.back$outmat)
cat("Cp and adjr\n")
print(summ.back$cp)
print(summ.back$adjr)

cat("\nforward\n")
out.forw<-regsubsets(askprice~ffac+sqfc+agec+bedc+baths+view+mfee+agec2+ffsqf+ffage+sqfage+imetr, data=b,method="forward",nvmax=12)
summ.forw<-summary(out.forw)
print(summ.forw$outmat)
cat("Cp and adjr\n")
print(summ.forw$cp)
print(summ.forw$adjr)

cat("\nseqrep\n")
out.sequ<-regsubsets(askprice~ffac+sqfc+agec+bedc+baths+view+mfee+agec2+ffsqf+ffage+sqfage+imetr, data=b,method="seqrep",nvmax=12)
summ.sequ<-summary(out.sequ)
print(summ.sequ$outmat)
cat("Cp and adjr\n")
print(summ.sequ$cp)
print(summ.sequ$adjr)
# The seqrep and backward options are not reliable when some columns of the
# X matrix are functions of others, such as with quadratic terms.
#
# The steps for finding important explanatory variables and multiple regression 
# are as follows.
# 1. Transform any heavily-skewed explanatory variable to be less-skewed.
# 2. Input the modified original list of explanatory variables into regsubsets
# 3. Use method="exhaustive" if number of explanatory variables is not too large
# 4. Otherwise use method="seqrep"
# 5. Look at residuals plots and decide if quadratic terms and further
# transforms are needed.


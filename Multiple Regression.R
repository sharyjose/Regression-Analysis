dat<-read.csv("D:/stat 315/burnaby_condos.csv")
# Look at the data:
head(dat)
tail(dat)

# Make some adjustements to make the numbers more manageable:
dat$askprice<-dat$askprice/1000
dat$ffarea<-dat$ffarea/100
dat$mfee<-dat$mfee/10


# The simplest linear regression model:
summary(lm(askprice~1, data=dat))
confint(lm(askprice~1, data=dat))

# Look at the regions
dat$region

model1<-lm(askprice~region, data=dat)
summary(model1)

# Include floor area
model2<-lm(askprice~region+ffarea, data=dat)
summary(model2)

# Check model assumtions:
qqnorm(model2$residuals)
plot(model2$residuals~ model2$fitted)

# Residuals do not look very good.  Let's try a log transform:
dat$logaskprice<-(log(dat$askprice))

# New model with y=log(price) and check model assumtions:
model3<-lm(logaskprice~region+ffarea, data=dat)
summary(model3)

# Residuals are not perfect but look much better 
qqnorm(model3$residuals)
plot(model3$residuals~ model3$fitted)

# Include number of beds:
model4<-lm(logaskprice ~region+ffarea+beds, data=dat)
summary(model4)

# Include number of baths and age:
model5<-lm(logaskprice ~region+ffarea+beds+baths+age, data=dat)
summary(model5)

# Without number of bedrooms:
model6<-lm(logaskprice ~region+ffarea+baths+age, data=dat)
summary(model6)


# Compare Adj-R2:
summary(model3)$adj.r.squared
summary(model4)$adj.r.squared
summary(model5)$adj.r.squared
summary(model6)$adj.r.squared


# Residuals look okay: 
qqnorm(model6$residuals)
plot(model6$residuals~ model6$fitted)



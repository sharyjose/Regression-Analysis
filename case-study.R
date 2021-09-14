##################################################################
### Suggested Price of used General Motors (GM) cars in 2005 ###
##################################################################

cars<-read.csv("carSalesPrice.csv")

head(cars)
summary(cars)


simple_model<-lm(Price ~ Mileage, data=cars)
summary(simple_model)

plot(Price ~ Mileage, col=rgb(0,0,1,alpha=0.5), pch=20, cex=2, data = cars)
abline(simple_model, col = "black", lwd = 3)

plot(Price ~ Mileage, col=as.factor(cars$Make), pch=20, cex=2, data = cars)
abline(simple_model, col = "black", lwd = 3)

plot(Price ~ Mileage, col=as.factor(cars$Type), pch=20, cex=2, data = cars)
abline(simple_model, col = "red3", lwd = 3)


library(ggplot2)
qplot(Mileage,Price, col= Make, data = cars)#
qplot(Mileage,Price, col= Type, data = cars)

# Create a model with Make and Mileage
model1 <- lm( Price ~ Make + Mileage, data = cars)
summary(model1)

# How do the residuals look ?not good 
hist(ls.diag(model1)$stud.res)
plot(model1$fitted,ls.diag(model1)$stud.res)
qplot(model1$fitted,ls.diag(model1)$stud.res, col=Type, data=cars)

# Create a model with Make and Mileage and Type
model2 <- lm( Price ~ Make + Mileage + Type, data = cars)
summary(model2)
qplot(model2$fitted,ls.diag(model2)$stud.res, col=Type, data=cars)


library(leaps)
# At this point we will use a model selection algorithm to find other good variables:
regs1 <- regsubsets(Price~., data= cars[,c("Price","Make","Type","Mileage","Cylinder","Liter","Doors","Cruise","Sound","Leather")], method="backward", nvmax=14)
ss1 <- summary(regs1)
ss1$which
which.min(ss1$cp)


regs2 <- regsubsets(Price~., data= cars[,c("Price","Make","Type","Mileage","Cylinder","Liter","Doors","Cruise","Sound","Leather")], method="backward", force.in=c(2:10), nvmax=14)
summary(regs2)
ss2 <- summary(regs2)
ss2$which
which.min(ss2$cp)

# Create a model with Mileage + Type + Make + Cylinder + Liter + Sound
model3 <- lm( Price ~ Mileage + Type + Make + Cylinder + Liter + Sound, data = cars)
summary(model3)
qplot(model3$fitted,ls.diag(model3)$stud.res, data=cars)

# Create a model with log(Price) and Mileage + Type + Make + Cylinder + Liter + Sound
model4 <- lm( log(Price) ~ Mileage + Type + Make + Cylinder + Liter + Sound, data = cars)
summary(model4)
qplot(model4$fitted,ls.diag(model4)$stud.res, data=cars)

# residuals vs. other variables:
qplot(model4$fitted,ls.diag(model4)$stud.res, data=cars, col=(Make))
qplot(Mileage,ls.diag(model4)$stud.res, data=cars, col=(Make))
qplot(Type,ls.diag(model4)$stud.res, data=cars, col=(Make))
qplot(Make,ls.diag(model4)$stud.res, data=cars, col=(Make))
qplot(Cylinder,ls.diag(model4)$stud.res, data=cars, col=(Make))
qplot(Liter,ls.diag(model4)$stud.res, data=cars, col=(Make))
qplot(Sound,ls.diag(model4)$stud.res, data=cars, col=(Make))


######################################################
# Any issues with highly-leveraged points? or highly-influential points 

plot((ls.diag(model4)$hat) ~model4$fitted)
plot(ls.diag(model4)$cooks~model4$fitted)

cars[(ls.diag(model4)$cooks)>0.01,]

######################################################
# Try some CV:

library(caret)

train( log(Price) ~  ., data=cars, method="lm", 
trControl=trainControl(method="cv",number=10))

train( log(Price) ~  Trim, data=cars, method="lm", 
trControl=trainControl(method="cv",number=10))


train( log(Price) ~ Mileage + Make + Type + Cylinder + Liter + Sound, data=cars, method="lm", 
trControl=trainControl(method="cv",number=10))

train( log(Price) ~ Mileage + Make + Type  + Liter + Sound, data=cars, method="lm", 
trControl=trainControl(method="cv",number=10))


######################################################
# Create a model with log(Price) and Mileage/1000 + Type + Make + Cylinder + Liter + Sound
model4 <- lm( log(Price) ~ I(Mileage/1000) + Type + Make + Cylinder + Liter + Sound, data = cars)
summary(model4)

model5<-lm( log(Price) ~ I(Mileage/1000) + Type + Make  + Liter + Sound, data = cars)
summary(model5)

library(car)
vif(model4)
vif(model5)#perfect



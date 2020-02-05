
#---------#Lab 1(Linear Regression : Drug Response Prediction Model)--------
#---------------------------------------------------------#

#Under Lab1 what all sobhan does is checking the normality of errors through hist of residuals
  #checking linearity by fitted.values & residuals graph.
setwd("C:\\Users\\V Sudheer Kumar\\Documents")
drug = read.csv("drug2.csv")
head(drug)
attach(drug)
names(drug)
hist(response)

boxplot.stats(response)$out

boxplot(response)

drug[!complete.cases(drug),]

library(ggplot2)

ggplot(data = drug, aes(dose,response))+geom_point()

model1 = lm(response~dose)
summary(model1)

abline(model1,col ="red")
plot(model1)
termplot(model1)
summary(model1)


err = residuals(model1)
hist(err)
plot(model1$fitted.values,err)

model2 = lm(response~dose+sex)
summary(model2)

err = residuals(model2)
hist(err)
plot(model2$fitted.values,err)


# Lab 2(Polynomial regression : ----
# Tooth decay Prediction Model  "dmf=Decayed, Missing or FilledTeeth")----
#--------------------------------------------------------------------------------------------------#
# Sobhan taught the effect of transformations on the linearity relationship of fitted.values
# and errors ## fitted.values and actual values.
# Transformations are applied on dependent variables and linearity is checked on models
  
# DMF describe the amount of dental caries in an individual. DMFT is to numerically 
# express the caries prevalence and are obtained by calculating the number of
# Decayed (D), Missing (M) and Filled (F) teeth.
# 
# The sum of the three figures forms the DMFT-value. For example: DMFT of 4-3-9=16 means
# that 4 teeth are decayed, 3 teeth are missing and 9 teeth have fillings. fluoride is accepted 
# as an effective method to prevent caries


dmf.data = read.csv("dmf.csv")
attach(dmf.data)

plot(dmf,flor)
dmf.data

regression.out<-lm(dmf~flor)
summary(regression.out)
plot(regression.out$fitted.values,regression.out$residuals)

#The graph and residual plot both strongly suggest a non-linear relationship between
# DMF teeth and fluoride.

# First create the fluoride squared variable:

flor2 <- flor^2
regression.out<-lm(dmf ~ flor + flor2)
summary(regression.out)
hist(regression.out$residuals)
plot(regression.out$fitted.values,regression.out$residuals)
plot(regression.out$fitted.values,dmf)

just.sqr.val=lm(dmf~flor2)
summary(just.sqr.val)
hist(just.sqr.val$residuals)
plot(just.sqr.val$fitted.values,just.sqr.val$residuals)
plot(just.sqr.val$fitted.values,dmf)

flor3 <- flor^3
regression.out<-lm(dmf ~ flor + flor2+flor3)
summary(regression.out)
hist(regression.out$residuals)
plot(regression.out$fitted.values,regression.out$residuals)
plot(regression.out$fitted.values,dmf)


sqrtflor <- sqrt(flor)
regression.out<-lm(dmf ~ flor + flor2+flor3+sqrtflor)
summary(regression.out)
hist(regression.out$residuals)
plot(regression.out$fitted.values,regression.out$residuals)
plot(regression.out$fitted.values,dmf)


regression.out<-lm(dmf ~log(flor+1)+ flor + flor2+flor3+sqrtflor)
summary(regression.out)
hist(regression.out$residuals)
plot(regression.out$fitted.values,regression.out$residuals)
plot(regression.out$fitted.values,dmf)  


library(car)
qqPlot(regression.out, main="QQ Plot") # qqPlot func requires library car


library(ISLR)


#Beware of range: In any regression model, all conclusions apply only over the range
# of the collected data. So here, with a reasonably good fitting model, we can
# expect good predictions over the range, say, from 0 to 3 units of 
# floride, but not above 2.5 (where the curve starts to rise again, which is not likely).

plot(dmf,flor)



#------------Lab 3(Moderation : Drug Response Prediction Model )-----------
#-----------------------------------------------------#
# Sobhan taught the effect of moderation variable the product of dose and sex.
# dose being a numeric variable and sex being a int variable. 
# Moderation gave good results and cured linearity problem, normality of errors was excellent.
str(drug)
  
model2 = lm(response~dose+sex)
summary(model2)

drug=read.csv("drug2.csv")


product = sex*dose

drug$product = drug$sex*drug$dose

set.seed(1)

indx = sample(nrow(drug),nrow(drug)*0.7)
train = drug[indx,]
test = drug[-indx,]


model4 = lm(response~dose+product+sex, data = train)

err4 = residuals(model4)

train.rmse = sqrt(mean(err4**2))

predict4= predict(model4, newdata = test)

diff = test$response - predict4

err5 = diff

test.rmse = sqrt(mean(err5**2))
train.rmse
test.rmse

names(model2)

err = residuals(model2)
hist(err)
plot(model2$fitted.values,err)
plot(model2$fitted.values,response)

qqPlot(model2, main = "QQ Plot")

#Interaction among dose and sex

product = sex * dose
model3 = lm(response~dose+product+sex)
summary(model3)

err = residuals(model3)
hist(err)
plot(model3$fitted.values,err)                    #$ this is excellent
plot(model3$fitted.values,response)               #$ this is excellent


qqPlot(model2)   # without interaction variable
qqPlot(model3)   # with interaction variable      #$

names(model2)

err2 = residuals(model2)

err3 = residuals(model3)


# RMSEs'
sqrt(mean(err2**2)) # without interaction variable
# [1] 34.89014438   
sqrt(mean(err3**2)) # with interaction variable
# [1] 2.312748851

#Model matrix

info = read.table("salary.txt", header=T)

str(info)

# model.matrix creates dummy variables for factor variables.  #$

x <- model.matrix(salary~age+pubs,data=info)
x
x <- model.matrix(salary~age+pubs+dept,data=info)
x
x <- model.matrix(salary~age+pubs*dept,data=info)
x


#--Lab 4(Model Validation : Drug Response Prediction Model ) very informative code----
#-----------------------------------------------------------#
  
  # In Lab 4  Sobhan checked the model got through moderation variable for underfitting
  # and overfitting by calculating rmse values for both training and testing.
  
  
  
drug<- read.csv("drug2.csv")
drug[1:3,]
product = drug$dose*drug$sex
drugdata = cbind(drug,product=product)
drugdata = data.frame(drugdata)


# fixing the seed value for the random selection guarantees the  same results in repeated runs

set.seed(1)

n=nrow(drugdata)
n1=floor(n*0.7)
n2=n-n1
train=sample(1:n,n1)

#regression on training set

m1=lm(response~.,data=drugdata[train,])
summary(m1)

names(m1)
plot(m1$fitted.values,obs)

plot(m1$fitted.values,drugdata$response[train])

plot(m1$fitted.values,m1$residuals)

pred=predict(m1,newdat=drugdata[-train,])
obs=drugdata$response[-train]
diff=obs-pred
Test.rmse=sqrt(sum(diff**2)/n2)
Test.rmse
testrmse=sqrt(mean(diff**2))
testrmse
Train.rmse = sqrt(sum(m1$residuals**2)/n1)
Train.rmse
trainrmse=sqrt(mean(m1$residuals**2))
trainrmse

names(pred)

# Lab 5(Lasso Models :  House Price Prediction)----
#------------------------------------------------#
  
  
houseprice<- read.csv("housepricedata.csv")
summary(houseprice)
houseprice[1:3,]
str(houseprice)
table(houseprice$condition)
table(houseprice$grade)
table(houseprice$waterfront)


#sqft_living15 : average sq.ft. of 15 nearest neighbors 				
#sqft_lot15     : average lot size of 15 nearest neighbors 
#condition     : condition of house				
#grade           : measure of quality of construction				
#waterfront    : waterfront property				
#view            : type of view	

houseprice[!complete.cases(houseprice),] # No missing values

boxplot.stats(houseprice$price)$out

boxplot(houseprice$price)

length(boxplot.stats(houseprice$price)$out) # 1146 outliers

houseprice$zipcode = factor( houseprice$zipcode)
houseprice$grade = factor( houseprice$grade)
houseprice$condition = factor( houseprice$condition)
houseprice$view = factor( houseprice$view)

#houseprice$zipcode = NULL                           #$ Sobhan din't drop zipcode

# model.matrix creates dummy variables for variables declared as factor variables.

str(houseprice)

housedata = model.matrix(price~.,data=houseprice)
str(housedata)
x = housedata[,-1]

dim(x)

# We normalize the covariates as they are of very different magnitudes.  #$
# Each normalized covariate has mean 0 and standard deviation 1

for (j in 1:27) {
  x[,j]=(x[,j]-mean(x[,j]))/sd(x[,j])                                    #$
}

## One could consider the standard regression model
reg <- lm(houseprice$price~x)
summary(reg)
p0=predict(reg)
?predict


## Or, one could consider LASSO
library(lars)
lasso <- lars(x=x, y=houseprice$price, trace=TRUE)
plot(lasso)
coef(lasso, s=c(.10,.175,0.50,1.00), mode="fraction")
## creates LASSO estimates as function of lambda
## gives you the estimates for four shrinkage coef
## Check that predictions in regression and lars (s=1) are the
## same
p1=predict(lasso,x,s=1,mode="fraction")
p1$fit
pdiff=p1$fit-p0
pdiff ## zero differences
## out of sample prediction; estimate model on 20,000 rows
MSElasso10=dim(10)
MSElasso175=dim(10)
MSElasso50=dim(10)
MSElasso100=dim(10)
set.seed(1) ## fixes seed to make random draws reproducible
for(i in 1:10){
  train <- sample(1:nrow(houseprice), 20000)
  lasso <- lars(x=x[train,], y=houseprice$price[train])
  MSElasso10[i]=mean((predict(lasso,x[-train,], s=.10, mode="fraction")$fit - houseprice$price[-train])^2)
  MSElasso175[i]=mean((predict(lasso,x[-train,], s=.175, mode="fraction")$fit - houseprice$price[-train])^2)
  MSElasso50[i]=mean((predict(lasso,x[-train,], s=.5, mode="fraction")$fit - houseprice$price[-train])^2)
  MSElasso100[i]=mean((predict(lasso,x[-train,], s=1.0, mode="fraction")$fit - houseprice$price[-train])^2)
}

mean(MSElasso10)
mean(MSElasso175)
mean(MSElasso50)
mean(MSElasso100)

boxplot(MSElasso10,MSElasso175,MSElasso50,MSElasso100, ylab="MSE", 
        sub="LASSO model", xlab="s=0.1 s=0.175 s=0.5 s=1.0(LS)")


library(lars)



# cross-validation using 10 folds
cv.lars(x=x,y=houseprice$price,K=10)

# lasso (least absolute shrinkage and selection operator) on all data.
# Continuous subset selection algorithm, can "shrink" the effect of unimportant predictors, 
# can  set effects to zero

lasso <- lars(x=x,y=houseprice$price,trace=TRUE)

plot(lasso)

# trace of lasso coefficients for varying penalty
coef(lasso,s=c(.42),mode="fraction")

g=coef(lasso,s=c(.42),mode="fraction")

g

names(which(g>0))

coef(lasso,s=c(0.175),mode="fraction") # as per lasso graph

abcd=cbind(houseprice$price,housedata)            #$ combining lasso variables and target variable

class(abcd)

abcd  = as.data.frame(abcd)

class(abcd)

# lazy in giving zipcode variables
new.with.lasso=lm(V1~sqft_living+bathrooms+view1+view4+sqft_living15+waterfront+view2+condition3+
                    condition5+grade9+grade11+sqft_lot15+grade12+grade13+grade10+grade7+view3,data = abcd)

summary(new.with.lasso)


#Lab 6  ( Compounding and Collinearity : Effect of Suns Screen Lotion)----
#---------------------------------------------------------------------------------------------#
# Here sobhan showed the effect in Multiple R square(goodness of fit) which can
# be shown only by adding the other variable.
  
  #Example : Cancer
  cancer = read.csv("skincancer.csv")

cancer[!complete.cases(cancer),]   # No missing values

boxplot(Skin.cancer.index)

boxplot.stats(Skin.cancer.index)$out  # 5 outliers

attach(cancer)
regression1.out <- lm(Skin.cancer.index~Usage.sun.screen.lotion)
regression1.out
summary(regression1.out)
regression2.out <- lm(Skin.cancer.index~Exp.to.sun+ Usage.sun.screen.lotion)
summary(regression2.out)

plot(regression1.out$fitted.values,regression1.out$residua0ls)
library(car)
durbinWatsonTest(regression1.out)                 #$ Autocorrelation test
plot(regression1.out$fitted.values,cancer$Skin.cancer.index)

plot(regression2.out$fitted.values,regression2.out$residua0ls)
plot(regression2.out$fitted.values,cancer$Skin.cancer.index)

vif(regression1.out)
vif(regression2.out)

regression3.out = lm(Skin.cancer.index~Exp.to.sun+ Usage.sun.screen.lotion)
summary(regression3.out)


cor(cancer)

#$
#-------------------------------Linear regression commands 112 lines--------------------
#--- library usages car MASS ISLR --------------------------------------------    
library(car) # for durbinWatsonTest(), qqPlot() , mtcars dataset, recoding variables in logReg
library(MASS) # to calculate stdres
library(ISLR) # for data default, data iris


# Transformations

mtcars
str(mtcars)
summary(mtcars)
table(mtcars$cyl)
table(mtcars$gear)
table(mtcars$vs)
table(mtcars$am)
table(mtcars$carb)

boxplot(mtcars$mpg)  # No outliers

mtcars[!complete.cases(mtcars),] # No missing values

fit=lm(mpg~hp+wt+disp,data = mtcars)
plot(fit)                                    # Shows 3 outliers in model fit

head(mtcars)
fit

names(fit)

summary(fit)


fit$coefficients

fit$residual


library(ggplot2)
stdres(fit)


# Checking Normality of errors----
qqPlot(fit,main = "QQ Plot")


summary(fit$residuals)

#Post modelling checks----
# for Linearity check
plot(fit$fitted.values,fit$residuals)

termplot(fit)

abline(fit,col="red")

#Anything above 2.5 on +ve side and -ve side is considered an Outlier----
abline(h=-2.5)
abline(h=2.5)

plot(fit$fitted.values,mtcars$mpg)


# for Autocorrelation of errors check----
durbinWatsonTest(fit)

# for Outliers check----
outlierTest(fit)

?outlier.test

# for leverages----
leveragePlots(fit)
cd=cooks.distance(fit)

head(cd)
sort(cd, decreasing = T)
### Cooks distance for high leverage points----
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2))

cutoff <- 4/((nrow(mtcars)-2 -1)) 
cutoff
plot(fit, which=5, cook.levels=cutoff)

# Multicollinearity post modelling----
vif(fit)

#LEAPS----

library(leaps)
FuelEff = read.csv("FuelEfficiency.csv")
head(FuelEff)

boxplot(FuelEff$MPG)
#GPM = Gallons per 100 miles
#WT  = Weight of the car in 1000lbs
#DIS = Cubic displacement in cubic inches
#NC  = No.of cylinders
#HP  = Horse Power
#ACC = Acceleration in seconds from 0 to 60kmph
#ET  = Engine type(V-type and straight)
X=FuelEff[,2:7]
y=FuelEff[,1]
out=summary(regsubsets(X,y,nbest = 4, nvmax=ncol(X)))
tab=cbind(out$which,out$rsq,out$adjr2,out$cp)
tab


model3$residuals
normresid=scale(model1$residuals)

plot(model3$fitted.values,model3$residuals)

# leaps
# best subset regression in R
library(leaps)
X=FuelEff[,2:7]
y=FuelEff[,1]
out=summary(regsubsets(X,y,nbest = 2, nvmax=ncol(X))) # 
# nbest is the no.of best models for considered variables
tab=cbind(out$which,out$rsq,out$adjr2,out$cp)
tab

# (Intercept) WT DIS NC HP ACC ET R-Sq R-Sq(adj) Cp
# 1 1 1 0 0 0 0 0 0.8579697 0.8540244 37.674750
# 1 1 0 0 0 1 0 0 0.7880098 0.7821212 72.979632
# 2 1 1 1 0 0 0 0 0.8926952 0.8865635 22.150747
# 2 1 1 0 0 0 0 1 0.8751262 0.8679906 31.016828
# 3 1 0 0 1 1 0 1 0.9145736 0.9070360 13.109930
# 3 1 1 1 1 0 0 0 0.9028083 0.8942326 19.047230
# 4 1 0 0 1 1 1 1 0.9313442 0.9230223 6.646728
# 4 1 1 0 1 1 0 1 0.9204005 0.9107520 12.169443
# 5 1 1 1 1 1 0 1 0.9337702 0.9234218 7.422476
# 5 1 0 1 1 1 1 1 0.9325494 0.9220103 8.038535
# 6 1 1 1 1 1 1 1 0.9385706 0.9266810 7.000000




# Lasso should be used when no.of variables are too high compared to no.of observations
# say 30 to 40 variables for below 300 observations.
# (or) 200 variables for say below 1000 observations
# Lasso should be used when overfitting occurs in above scenarios for important variable 
# selection.

#-----------------------SimpleLR 27 lines--------------------------------

data()
data(airquality)
names(airquality)
head(airquality)
summary(airquality)
str(airquality)


attach(airquality)
plot(Ozone~Solar.R)
#plot(Ozone~Solar.R,data=airquality)

#Trying the Mean of Ozone
#mean(airquality$Ozone)
#airquality$Ozone

#calculate mean ozone concentration (na´s removed)
mean.Ozone=mean(airquality$Ozone,na.rm=T)

abline(h=mean.Ozone)

#use lm to fit a regression line through these data:

model1=lm(Ozone~Solar.R,data=airquality)

model1

abline(model1,col="red")                  #$ plotting model line using abline()
plot(model1)

termplot(model1)
summary(model1)



#very slight difference for plot(model1) and other command line linear regression plots

leveragePlots(model1)
cd = cooks.distance(model1) # by using sort(cd, decreasing = T) we can see the top leverage points

cutoff <- 4/((nrow(airquality)-length(model1$coefficients)-2))

plot(model1, which=5, cook.levels=cutoff)

#Need to bring back----

dataset = read.csv("needtobringback.csv")
attach(dataset)
ul=lm(c~b+a)

summary(lm(c~b+a))
d = b*b
summary(lm(c~b+d))
dim(dataset)
summary(lm(c~b+d+a))

#Case Study(Case study on Retail data set)



save.image("Linear regression Sobhan practice.RData")

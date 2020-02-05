Libraries
-----------
  
# 1.car
# 2.VGAM
# 3.distrom
# 4.textir

library(car)
library(VGAM)               # Multinomial logistic regression    vglm() func
library(distrom) #distributed multinomial regression via Poisson distribution and gamlr package
library(textir)              # Multinomial logistic Regression

# Lab 1( Bank Loan Prediction Model)----
----------------------------------------------
  setwd("C:\\Users\\V Sudheer Kumar\\Documents")                                               
loan = read.csv("bank.csv")
head(loan)
m1=glm(repaid~age+salary,family=binomial,data=loan)
summary(m1)

set.seed(1)
idx = sample(nrow(loan), 0.7*nrow(loan))
train = loan[idx,]
test = loan[-idx,]

m1 = glm(repaid~age+salary+salary*salary,family=binomial,data=train)
summary(m1)

ypred = predict(m1, newdata = test, type = "response")

hist(ypred)

ypredc=ifelse(ypred>.5,1,0)

table(ypredc, test$repaid)

library(ROCR)

pred = prediction(ypredc, test$repaid)

perf = performance(pred, "tpr","fpr")

accuracy = (212+549)/(212+35+90+549)
accuracy

precision = 549/(549+35)
precision
recall = 549/(549+90)
recall

f1.score = 2*precision*recall/(precision+recall)

f1.score



# Exercise(Analysing the impact of race and level of crime on death penality)----
---------------------------------------------------------------------------

getwd()  
  
dpen = read.csv("DeathPenalty.csv")  

head(dpen)
str(dpen)
table(dpen$Agg)
table(dpen$VRace)

m1=glm(Death~VRace+Agg,family=binomial,data=dpen)
summary(m1)

# Below steps are not needed as these columns have continous integers.

# dpen$Vr=as.factor(dpen$VRace)
# dpen$Aggg=as.factor(dpen$Agg)
# 
# head(dpen)
# 
# dpen=dpen[,c(2,3,1)]
# 
# m2=glm(Death~VRace+Agg,family = binomial,data = dpen)
# summary(m2)


# Lab 2(Polynomial Modeling :  Heart Stroke Prediction)----
#-------------------------------------------------------#

health = read.csv("health.csv")
attach(health)
head(health)

plot(weight,BP,col=Suffered.Heart.storke+1)         #$ good simple colored scatterplot
BPS = BP*BP
weights = weight*weight
BPweight = BP*weight

m1=glm(Suffered.Heart.storke~BP+weight+weights+BPS+BPweight,family=binomial)
summary(m1)


# =======================================Lab 3=====================================----
#   
#   Example 1: ICU Data ----
# 
# SIZE:  200 observations, 21 variables
# 
# SOURCE: Hosmer, D.W., Lemeshow, S. and Sturdivant, R.X. (2013) 
# 
# LIST OF VARIABLES:
#   
#   Name	                                  	            Codes/Values	                           Abbreviations
#-------------------------------------------------------------------------------------------------------------------------------------#
#   Identification Code	                                    ID Number	                                         ID
# 
# Vital Status	                                            0 = Lived, 1=Died	                                 STA
# 
# Age		                                                    Years		                                           AGE
# 
# Gender		                                                0 = Male,1=Female	                                 GENDER
# 
# Race	                                                    1 = White,2=Black,3=Other                          RACE
# 
# Service at ICU Admission                                  0 = Medical,1=Surgical	                           SER
# 
# Cancer Part of PresentProblem                             0 = No,1=Yes		                                   CAN
# 
# History of Chronic Renal Failure                          0 = No,  1=Yes                                     CRN
# 
# Infection Probable at ICU Admission	                      0 = No,1=Yes            		                       INF
# 
# CPR Prior to ICU Admission	                              0 = No,1=Yes	        	                           CPR	
# 
# Systolic Blood Pressure at ICU Admission	                mm Hg		                                           SYS
# 
# Heart Rate at ICU Admission	                              Beats/min	                                         HRA	
# 
# Previous Admission to an ICU within 6 Months              0 = No,1=Yes		                                   PRE	
# 
# Type of Admission		                                      0 = Elective 1=Emerggency	                         TYP
# 
# 
# Long Bone, Multiple, Neck,	                              0 = No,1=Yes		                                   FRA
# Single Area, or Hip Fracture	
# 
# PO2 from Initial Blood Gases	                            0 = > 60, 1<60	                                   PO2
# 
# 
# PH from Initial Blood Gases	                              0 => 7.25, 1<7.25          	                       PH
# 
# 
# PCO2 from initial Blood Gases		                          0  < 45, 1>=45	                                   PCO
# 
# 
# Bicarbonate from Initial Blood Gases	                    0 = >18, 1=<18	                                   BIC
# 
# 
# Creatinine from Initial Blood Gases	                      0 = < 2.0, 1>2.0	                                 CRE
# 
# 
# Level of Consciousness at ICU Admission                   0 = No Coma or Stupor,1=Deep stupor,2=Coma	       LOC
# 


icu.dat = read.table("ICU.txt",header=T)
head(icu.dat)
summary(icu.dat)
head(icu.dat)

model = glm(sta~age+sex+ser+hra+pre+typ+can+sys, family=binomial,data=icu.dat)
summary(model)

model = glm(sta~age+pre+typ+can, family=binomial,data=icu.dat)
summary(model)

model = glm(sta~age+typ+can, family=binomial,data=icu.dat)
summary(model)

#Deviance test

names(model)
#lower.tail = FALSE gives probability of greater or equal
pchisq(model$null.deviance - model$deviance,  model$df.null - model$df.residual,  lower.tail = FALSE)


model$df.null
dim(icu.dat)
model$df.residual




set.seed(1)
n=nrow(icu.dat)
n
n1=floor(n*(0.7))
n1
n2=n-n1
n2

train=sample(1:n,n1)
model = glm(sta~age+typ+can, family=binomial,data=icu.dat[train,])
summary(model)
names(model)
model$null.deviance-model$deviance

pchisq(model$null.deviance - model$deviance,  model$df.null - model$df.residual,  lower.tail = FALSE)

ptrain <- predict(model,newdata=icu.dat[train,],type="response")

head(ptrain)
names(ptrain)
dim(ptrain)

gg1=floor(ptrain+0.5)
ttt=table(icu.dat$sta[train],gg1)
ttt
error=(ttt[1,2]+ttt[2,1])/n1
error

x= ptrain>0.5
length(x)
x

ptest <- predict(model,newdata=icu.dat[-train,],type="response")
y=ptest>0.5
y
summary(ptest)
gg1=floor(ptest+0.5)
ttt=table(icu.dat$sta[-train],gg1)
error=(ttt[1,2]+ttt[2,1])/n2
error


# Example 2: Delayed Airplanes ----


library(car) # needed to recode variables
set.seed(1)

# read and print the data
del <- read.csv("FlightDelays.csv",stringsAsFactors = FALSE)
del[1:3,]
str(del)
# define hours of departure since delay is not a linear function of time of the day 
del$sched=factor(floor(del$schedtime/100))
table(del$sched)

#Recoding delay variable
t = del$delay=="ontime"
del$delay[t]=0
del$delay[!t]=1
del$delay = as.numeric(del$delay)

del$dayweek = factor(del$dayweek)

table(del$dayweek)

# Keep only these coloumns : carrier, dest, origin, weather, dayweek, delay, sched and delete 
# others
#Model matrix construction
Xdel = model.matrix(delay~factor(carrier)+factor(dest)+factor(origin)+factor(weather)+
                      factor(sched),data=del)
Xdel = Xdel[,-1]
dim(Xdel)

n=nrow(del)
n
n1=floor(n*(0.6))
n1
n2=n-n1
n2

train=sample(1:n,n1)
xtrain <- Xdel[train,]
xtest <- Xdel[-train,]
ytrain <- del$delay[train]
ytest <- del$delay[-train]
traindata = data.frame(delay=ytrain,xtrain)
View(traindata)
m1=glm(delay~.,family=binomial,data=traindata)
summary(m1)
names(m1)
pchisq(m1$null.deviance - m1$deviance,  m1$df.null - m1$df.residual,  lower.tail = FALSE)



# prediction: predicted default probabilities for cases in train set
ptrain <- predict(m1,newdata=data.frame(xtrain),type="response")
gg1=floor(ptrain+0.5)
ttt=table(ytrain,gg1)
error=(ttt[1,2]+ttt[2,1])/n1
error

# prediction: predicted default probabilities for cases in test set
ptest <- predict(m1,newdata=data.frame(xtest),type="response")
gg1=floor(ptest+0.5)
ttt=table(ytest,gg1)
ttt
error=(ttt[1,2]+ttt[2,1])/n2
error


# prediction: predicted default probabilities for cases in train set
ptrain <- predict(m1,newdata=data.frame(xtrain),type="response")
gg1=floor(ptrain+0.7)
ttt=table(ytrain,gg1)
error=(ttt[1,2]+ttt[2,1])/n1
error

# prediction: predicted default probabilities for cases in test set
ptest <- predict(m1,newdata=data.frame(xtest),type="response")
gg1=floor(ptest+0.7)
ttt=table(ytest,gg1)
ttt
error=(ttt[1,2]+ttt[2,1])/n2
error


ptest = predict(m1,newdata = del,type = "response")
ptest = predict(m1,newdata = del,type = "response")
gg1=floor(ptest+0.7)
ttt=table(ytest,gg1)
ttt
error=(ttt[1,2]+ttt[2,1])/n2
error






#=====================================Lab 4(ROCR graph)===================----
  
  
tp = c(1:95)
fp = c(1:95)


# for loop alert ----------------------------------------------------------

for ( i in 1:95){
  gg1 = floor(ptest+i/100)
  ttt=table(xtest,gg1)
  tp[i] = ttt[2,2]/(ttt[2,1]+ttt[2,2])
  fp[i] = ttt[1,2]/(ttt[1,1]+ttt[1,2])
}

plot(tp~fp, type="l")




#======================================Lab5(Lift)===============----
  
  
# order cases in test set according to their success prob and show actual outcome shown
# next to it

bb=cbind(ptest,ytest)
bb
bb1=bb[order(ptest,decreasing=TRUE),]
bb1


# calculating the lift :  cumulative 1's sorted by predicted values
axis=dim(n2)
ay=dim(n2)


for (i in 1:n2) {
  axis[i]=i
  ay[i]=sum(bb1[1:i,2])
}


plot(axis,ay,xlab="number of cases",ylab="number of successes",main="Lift: Cum successes sorted by pred val/success prob",type="l")


#=====================Lab 6(Lasso Logistic regression)=============----
library(glmnet)

cv.glmmod<-cv.glmnet(x=xtrain,y=ytrain,alpha=1, type.measure = "deviance")
plot(cv.glmmod)
f<-cv.glmmod$lambda.min
f

glmmod<-glmnet(x=xtrain,y=ytrain, alpha=1, family='binomial')
coef(glmmod,s=f)

predict(glmmod, newx = xtest, type = "response", s = c(f))

# ===================Lab 6================================----
  
#   Several choices are available to estimate multinomial logistic regression models
# in R. For example, one can use the command mlogit in the package mlogit, the
# command vglm in the package VGAM, or the mnlm function in the package textir.
# 
# The penalty estimation approach in mnlm allows for shrinkage, similar to the shrinkage of 
# LASSO estimates. This approach is especially useful for models with many parameters.




Example

library(VGAM)

covars = read.csv("forensicglass.csv")
head(covars)


for( i in 1: 9){
  covars[,i] = (covars[,i]- mean(covars[,i]))/sd(covars[,i])
}


n=nrow(covars)
nt=n-20
set.seed(1)
train <- sample(1:n,nt)

gg <- vglm(type ~ Na+Mg+Al,multinomial(refLevel=6),data=covars[train,])
p1=predict(gg,newdata=covars[-train,])


p1=exp(p1)
sum=(1+p1[,1]+p1[,2]+p1[,3]+p1[,4]+p1[,5])

probCon=round(p1[,1]/sum,2) 
probHead=round(p1[,2]/sum,2)  
probTabl=round(p1[,3]/sum,2)    
probVeh=round(p1[,4]/sum,2)   
probWinF=round(p1[,5]/sum,2)  
probWinNf=round(1/sum,2)        

ppp=data.frame(probCon,probHead,probTabl,probVeh,probWinF,probWinNf,covars$type[-train])
head(ppp)


# Sudheers exclusive----

x = read.csv("blooddonation.csv")

head(x)

attach(x)

set.seed(123)

idx = sample(nrow(x), 0.7*nrow(x))

train = x[idx,]

test = x[-idx,]

model1 = glm(whether.he.she.donated.blood.in.March.2007~., data = train, family = 'binomial')

summary(model1)

pre = predict(model1, newdata = test, type = 'response')

test$predicted = ifelse(pre>=0.5,1,0)

table(test$predicted, test$whether.he.she.donated.blood.in.March.2007)

library(ROCR)

pred = prediction(test$predicted, test$whether.he.she.donated.blood.in.March.2007)

perf = performance(pred, 'tpr','fpr')

plot(perf)



# Logistic Regression Charan flight delay ---------------------------------


setwd("D:\\Data Science - Course\\Logistic")

library(boot)
library(MASS)


flights <- read.csv("FlightDelays.csv", header=T, sep=",")
head(flights)
table(flights$weather)

table(flights$delay)

## class Imbalance 

prop.table(table(flights$delay ))*100




### 

flights$weather <- as.factor(flights$weather)
flights$delay <- as.factor(flights$delay)

table(flights$weather)

library(ggplot2)

ggplot(flights, aes( delay,fill= weather)) + geom_bar()

## convert the delay variable to binary ( 0 , 1)

flights$delay = ifelse(flights$delay =="ontime", 0, 1)


flights$delay = as.factor(flights$delay)

## diff b/w scheduled and departure time 

flights$diff =flights$schedtime - flights$deptime

names(flights)


file3 = flights[ , -c(6,7,12 )]


### ggplot(file2, aes(delay)) + geom_bar(aes(fill=weather),position = "stack")

ggplot(file3, aes(delay)) + geom_bar(aes(fill=weather),position = "stack")

ggplot(file3, aes(delay)) + geom_bar(aes(fill=weather),position = "dodge")

ggplot(file3, aes(delay)) + geom_bar(aes(fill=weather),position = "fill")

set.seed(1234)


sub = sample(nrow(file3), nrow(file3) * 0.7)

train = file3[sub,]

test = file3[-sub,]


### Generalised linear models : logistic regression

model1 = glm(delay ~ . -weather -dayweek -diff ,  data=train, family="binomial")

summary(model1)

### Predict probability on test data using model1

test$pred = predict(model1, newdata=test, type="response")


### convert the probability to a class variable ( > 0.5 is delayed , <=0.5 is "ontime" )

test$pred_class = ifelse(test$pred >= 0.4, 1,0)




## cross table(confusion matrix) of two discrete variables

table(test$delay, test$pred_class)

#     0   1
# 0 514   7
# 1  47  93

accuracy = (514+93)/(514+7+47+93)
accuracy
precision = 93/100
precision
recall = 93/(47+93)
recall
sensi = recall
sensi
speci = 514/521
speci


93/100
93/140

2*0.93*0.66/(0.93+0.66)

(517+80)/661

library(caret)

confusionMatrix(test$delay, test$pred_class)

79/(91)
79/(79+48)

2*0.86*0.62/(0.86+0.62)


library(e1071)



library(AUC)
library(ROCR)
library(ggplot2)
library(caret)

?confusionMatrix

model2 = glm(delay ~ . -schedtime -deptime - weather -dayweek -diff ,  data=train, family="binomial")

summary(model2)

pchisq(model2$null.deviance-model2$deviance,model2$df.null-model2$df.residual, lower.tail = FALSE)

test$pred1 = predict(model2, newdata=test, type="response")



hist(test$pred1)

test$pred_class1 = ifelse( test$pred1 >= 0.5, "yes", "no")



table(test$delay, test$pred_class1)

## ROC graph for model1 and model2 

names(test)


print(model2)
model_back <- step(model1,direction="backward",trace=T)


model_back <- step(model2,direction="backward",trace=T)


file3$pred <- predict(model1, newdata=file3, type="response")

head(file3)

file3$predclass <- ifelse(file3$pred > 0.65, 1, 0)

table( file3$predclass, file3$delay)

summary(model1)
head(file2)

table(file2$origin)

file2$depdelay <- file2$deptime - file2$schedtime

table(file2$dest)

file2$delay <- as.factor(file2$delay)



## roc graph 
library(ROCR)

pred <- prediction(test$pred, test$delay)
perf <- performance(pred,"tpr","fpr")
plot( perf)
auc = performance(pred,measure = "auc")
auc = auc@y.values[[1]]
auc # 0.8454875


pred2 <- prediction(test$pred1, test$delay)
perf2 <- performance(pred2,"tpr","fpr")
plot( perf)
plot(perf2, add = TRUE, colorize = TRUE)
auc = performance(pred2,measure = "auc")
auc = auc@y.values[[1]]
auc # 0.5811966


#auc(churn$predictions,churn$labels)

#auc(file3$pred, file3$delay)

auc = performance(pred,measure = "auc")
auc = auc@y.values[[1]]
auc

model2 <- rpart(delay ~ .   , data=file1) # Need to check about this file1 in google drive.

plot(model2)
text(model2)

fancyRpartPlot(model2) 
install.packages("lmtest")

### Optional tests

require(lmtest)
lrtest (model1, model2)


?cv.glm
summary(model1)

cvres <- cv.glm(file1, model1, K=7)

names(cvres)


####cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

cvres$delta


##beetle##
faraway(beetle)
data(beetle, package = "faraway")
View(beetle) #tiny - shit

##africa##
data(africa, package = "faraway")
View(africa) #aight not huge

##pima##
data(pima, package = "faraway")
View(pima)
dim(pima) #9 dimensions, 768 observations
colnames(pima)

#I think I use pima


##dvisits##
data(dvisits, package = "faraway")
View(dvisits)
dim(dvisits) #19 dimensions, 5190 observations

##ozone##
data(ozone, package = "faraway")
View(ozone) 
dim(ozone) #10 dimensions, 330 observations

##trees?##
data(trees, package = "faraway")
View(trees)
data(trees) ##girth height volume of trees##

##ethanol##
library(faraway)
data(ethanol, package = "lattice")
View(ethanol)

#Chapter 15#----
#15.1#----
library(ggplot2)
library(faraway)
data(ozone,package = "faraway")
ggplot(ozone,aes(x=temp,y=O3)) + geom_point(size=1) + geom_smooth() 
ggplot(ozone, aes(x=ibh,y=O3)) + geom_point(size=1) + geom_smooth() + theme(axis.text.x = element_text(angle = 90))
ggplot(ozone,aes(x=ibt, y=O3)) + geom_point(size=1) + geom_smooth()

olm <- lm(O3 ~ temp + ibh + ibt,ozone)
sumary(olm)

library(effects)
plot(Effect("temp", olm, partial.residuals=TRUE))
plot(Effect("ibh", olm, partial.residuals=TRUE))
plot(Effect("ibt", olm, partial.residuals=TRUE))

#15.2#----
library(mgcv)
ammgcv <- gam(O3 ~ s(temp) + s(ibh) + s(ibt), data=ozone)
summary(ammgcv)

plot(ammgcv,residuals=TRUE, select=1)
plot(ammgcv,residuals=TRUE, select=2)
plot(ammgcv,residuals=TRUE, select=3)

am1 <- gam(O3 ~ s(temp)+s(ibh), data=ozone)
am2 <- gam(O3 ~ temp + s(ibh), data=ozone)
anova(am2,am1,test="F")


#Chapter 15 exercise#----
#a)
#Plot the test results against each of the predictors#----
par(mfrow = c(1,2))
data.class(pima$test)
pima$test <- as.factor(pima$test)
plotpima <- as.factor(pima$test)

plot(test ~ pregnant, data=pima)
plot(pregnant ~ test, data = pima)
plot(jitter(test,0.1) ~ jitter(pregnant), pima, pch=".", xlab="pregnant", ylab = "test", cex.lab = 1.5)
plot(jitter(test,0.1) ~ jitter(glucose), pima, pch=".",xlab="glucose", ylab = "test",cex.lab = 1.5) #remove zeroes
plot(jitter(test,0.1) ~ jitter(diastolic), pima, pch=".",xlab="diastolic", ylab = "test",cex.lab = 1.5) #remove zeroes
plot(jitter(test,0.1) ~ jitter(triceps), pima, pch=".",xlab="triceps", ylab = "test",cex.lab = 1.5) #remove zeroes
plot(jitter(test,0.1) ~ jitter(insulin), pima, pch=".",xlab="insulin", ylab = "test",cex.lab = 1.5) #fine just means they're diabetic
plot(jitter(test,0.1) ~ jitter(bmi), pima, pch=".",xlab="bmi", ylab = "test",cex.lab = 1.5) #remove zeroes
plot(jitter(test,0.1) ~ jitter(diabetes), pima, pch=".",xlab="diabetes", ylab = "test",cex.lab = 1.5) 
plot(jitter(test,0.1) ~ jitter(age), pima, pch=".",xlab="age", ylab = "test",cex.lab = 1.5)

plot(pregnant ~ plotpima, data = pima, xlab="test",cex.lab = 1.5)
plot(glucose ~ plotpima, data = pima, xlab = "test",cex.lab = 1.5)
plot(diastolic ~ plotpima, data = pima,xlab = "test",cex.lab = 1.5)
plot(triceps ~ plotpima, data = pima,xlab = "test",cex.lab = 1.5)
plot(insulin ~ plotpima, data = pima,xlab = "test",cex.lab = 1.5)
plot(bmi ~ plotpima, data = pima,xlab = "test",cex.lab = 1.5)
plot(diabetes ~ plotpima, data = pima,xlab = "test",cex.lab = 1.5)
plot(age ~ plotpima, data = pima,xlab = "test",cex.lab = 1.5)

#replace the impossible values with missing value code#

pima$glucose[pima$glucose == 0] <- NA
pima$diastolic[pima$diastolic == 0] <- NA
pima$triceps[pima$triceps == 0] <- NA
pima$bmi[pima$bmi == 0] <- NA

#plot the data again and comment on the relationships#
plot(glucose ~ plotpima, data = pima, xlab = "test",cex.lab = 1.5)
plot(diastolic ~ plotpima, data = pima,xlab = "test",cex.lab = 1.5)
plot(triceps ~ plotpima, data = pima,xlab = "test",cex.lab = 1.5)
plot(bmi ~ plotpima, data = pima,xlab = "test",cex.lab = 1.5)

plot(jitter(test,0.1) ~ jitter(glucose), pima, pch=".",xlab="glucose", ylab = "test",cex.lab = 1.5) 
plot(jitter(test,0.1) ~ jitter(diastolic), pima, pch=".",xlab="diastolic", ylab = "test",cex.lab = 1.5)
plot(jitter(test,0.1) ~ jitter(triceps), pima, pch=".",xlab="triceps", ylab = "test",cex.lab = 1.5) 
plot(jitter(test,0.1) ~ jitter(bmi), pima, pch=".",xlab="bmi", ylab = "test",cex.lab = 1.5)

#b)
#Find the subset of the cases that are complete#----
pimaComplete <- pima[rowSums(is.na(pima)) == 0,]
table(is.na(pimaComplete))
dim(pimaComplete)

pimaNA <- pima[rowSums(is.na(pima)) > 0,]
table(is.na(pimaNA))
dim(pimaNA)
236 + 532
View(pimaComplete)

#From this subset, take a random sample of size 100, this is the test set#
#All remaining cases, including those with missing values are the training set#
library(dplyr)
testIndex <- sample(532,100)

pimaTest <- pimaComplete[c(testIndex),]
pimaTrain <- pima[-c(testIndex),]
pimaTrainr <- pimaComplete[-c(testIndex),]

dim(pimaTest) #this is the test set no matter what, except the nn
dim(pimaTrain) #this is the train set with na's
dim(pimaTrainr) #this is the train set without na's

table(is.na(pimaTest))
table(is.na(pimaTrain))
table(is.na(pimaTrainr))

#c) #works no problem
#Fit a GLM to the diabetes training set using all predictors#
library(faraway)
pimaGLM <- glm(test ~ ., data = pimaTrain) #works no problem
summary(pimaGLM)

probTest <- predict(pimaGLM, newdata = pimaTest,  type = "response")
length(probTest)

thresh <- seq(0.01,0.9,0.01)
Sensitivity <- numeric(length(thresh))
Specificity <- numeric(length(thresh))

for(j in seq(along=thresh)) {
  pp <- ifelse(probTest < thresh[j],0,1)
  xx <- xtabs( ~ test + pp, pimaTest)
  Specificity[j] <- xx[1,1]/(xx[1,1]+xx[1,2])
  Sensitivity[j] <- xx[2,2]/(xx[2,1] + xx[2,2])
}

matplot(thresh,cbind(Sensitivity,Specificity),type = "l",ylim = c(0,1), xlab = "Threshold",ylab = "Proportion",lty=1:2)
plot(1-Specificity,Sensitivity,type="l")
abline(0,1,lty=2)

pp <- ifelse(probTest < 0.43,0,1)
table(pp, pimaTest$test)
xx <- xtabs( ~ test + pp, pimaTest)
xx

predTest[probTest > .5] = 1

table(predTest, pimaTest$test)

#d)
#step AIC

#step AIC on complete model, apply to other model#
rpimaGLM <- step(glm(test ~ ., data=pimaTrainr)) #need to remove na's for step function
summary(rpimaGLM)

probTest <- predict(rpimaGLM, newdata = pimaTest,  type = "response")

thresh <- seq(0.01,0.9,0.01)
Sensitivity <- numeric(length(thresh))
Specificity <- numeric(length(thresh))

for(j in seq(along=thresh)) {
  pp <- ifelse(probTest < thresh[j],0,1)
  xx <- xtabs( ~ test + pp, pimaTest)
  Specificity[j] <- xx[1,1]/(xx[1,1]+xx[1,2])
  Sensitivity[j] <- xx[2,2]/(xx[2,1] + xx[2,2])
}

matplot(thresh,cbind(Sensitivity,Specificity),type = "l",ylim = c(0,1), xlab = "Threshold",ylab = "Proportion",lty=1:2)
plot(1-Specificity,Sensitivity,type="l")
abline(0,1,lty=2)

pp <- ifelse(probTest < 0.41,0,1)
table(pp, pimaTest$test)
xx <- xtabs( ~ test + pp, pimaTest)
xx

#e)
#Fit a GAM using all the predictors#
colnames(testTrain)
library(mgcv)
testGAM <- gam(test ~ s(pregnant) + s(glucose) + s(diastolic) + s(triceps) + s(insulin) + s(bmi) + s(diabetes) + s(age), data=pimaTrain)
summary(testGAM)
sumary(testGAM)

probTest <- predict(testGAM, newdata = pimaTest,  type = "response")

thresh <- seq(0.01,0.9,0.01)
Sensitivity <- numeric(length(thresh))
Specificity <- numeric(length(thresh))

for(j in seq(along=thresh)) {
  pp <- ifelse(probTest < thresh[j],0,1)
  xx <- xtabs( ~ test + pp, pimaTest)
  Specificity[j] <- xx[1,1]/(xx[1,1]+xx[1,2])
  Sensitivity[j] <- xx[2,2]/(xx[2,1] + xx[2,2])
}

matplot(thresh,cbind(Sensitivity,Specificity),type = "l",ylim = c(0,1), xlab = "Threshold",ylab = "Proportion",lty=1:2)
plot(1-Specificity,Sensitivity,type="l")
abline(0,1,lty=2)

pp <- ifelse(probTest < 0.37,0,1)
table(pp, pimaTest$test)
xx <- xtabs( ~ test + pp, pimaTest)
xx
#f)
#Fit a GAM using only predictors selected from the reduced GLM model
rtestGAM <- gam(test ~ s(pregnant) + s(glucose) + s(diabetes) + s(bmi) + s(age), data = pimaTrain)

probTest <- predict(rtestGAM, newdata = pimaTest,  type = "response")

thresh <- seq(0.01,0.9,0.01)
Sensitivity <- numeric(length(thresh))
Specificity <- numeric(length(thresh))

for(j in seq(along=thresh)) {
  pp <- ifelse(probTest < thresh[j],0,1)
  xx <- xtabs( ~ test + pp, pimaTest)
  Specificity[j] <- xx[1,1]/(xx[1,1]+xx[1,2])
  Sensitivity[j] <- xx[2,2]/(xx[2,1] + xx[2,2])
}

matplot(thresh,cbind(Sensitivity,Specificity),type = "l",ylim = c(0,1), xlab = "Threshold",ylab = "Proportion",lty=1:2)
plot(1-Specificity,Sensitivity,type="l")
abline(0,1,lty=2)

pp <- ifelse(probTest < 0.46,0,1)
table(pp, pimaTest$test)
xx <- xtabs( ~ test + pp, pimaTest)
xx

#Chapter16#----
#16.1#----
data(ozone, package="faraway")

library(rpart)
(tmod <- rpart(O3 ~ .,ozone))

plot(tmod)
text(tmod)
plot(tmod,compress=T,uniform=T,branch=0.4)
text(tmod)

plot(jitter(predict(tmod)),residuals(tmod),xlab="Fitted",ylab="Residuals")
abline(h=0)
qqnorm(residuals(tmod))
qqline(residuals(tmod))

(x0 <- apply(ozone[,-1],2,median))
predict(tmod,data.frame(t(x0)))

#16.2#----
set.seed(123)
tmode <- rpart(O3 ~ .,ozone,cp=0.001)
printcp(tmode)

plotcp(tmod)

library(rpart.plot)
rpart.plot(tmod, type=3)

tmodr <- prune.rpart(tmod, 0.0154)
1-sum(residuals(tmodr)^2)/sum((ozone$O3-mean(ozone$O3))^2)

set.seed(123)
tmod <- rpart(O3 ~ ., ozone[sample(330,165),])
(tmods <- prune.rpart(tmod,0.0154))

#16.3#----

library(randomForest)
fmod <- randomForest(O3 ~ ., ozone)
plot(fmod,main="")

cvr <- rfcv(ozone[,-1],ozone[,1],step=0.9)
cbind(nvars=cvr$n.var,MSE=cvr$error.cv)

fmod <- randomForest(O3 ~ ., ozone, mtry=9)
1-sum((fmod$predict-ozone$O3)^2)/sum((ozone$O3-mean(ozone$O3))^2)

tgrid <- 20:100
meds <- apply(ozone, 2, median) [-match(c("O3","temp"),names(ozone))]
tdf <- cbind(temp=tgrid, data.frame(t(meds)))
head(tdf)

medfor <- predict(fmod,tdf)
partialPlot(fmod, ozone, "temp", main="")
lines(tgrid,medfor,lty=5)

lmod<-lm(O3 ~ ., ozone)
lmpred <- predict(lmod, tdf)
lines(tgrid, lmpred, lty=2)

importance(fmod)

#16.4#----

data(kanga, package="faraway")
x0 <- c(1115, NA, 748,182,NA,NA,178,311,756,226,NA,NA,NA,48,1009,NA,204,593)

kanga<- kanga[,c(T,F,!is.na(x0))]
kanga[1:2,]
newko <- na.omit(kanga[,-c(4,10)])
dim(newko)

dim(na.omit(kanga))

ggplot(newko, aes(x=zygomatic.width, y=foramina.length,shape=species)) + 
  geom_point() + theme(legend.position = "top", legend.direction = "horizontal", legend.title = element_blank())

set.seed(123)
kt <- rpart(species ~ ., data=newko,cp=0.001)
printcp(kt)

(ktp <- prune(kt,cp=0.0211))

plot(ktp,compress=T,uniform=T,branch=0.4)
text(ktp)

(tt <- table(actual=newko$species, predicted=predict(ktp, type="class")))

1-sum(diag(tt))/sum(tt)

pck <- princomp(newko[,-1])
pcdf <- data.frame(species=newko$species,pck$scores)
kt <- rpart(species ~ ., pcdf, cp=0.001)
printcp(kt)

(kt <- prune.rpart(kt, 0.0421))

(tt<- table(newko$species, predict(ktp,type="class")))

1-sum(diag(tt))/sum(tt)

nx0 <- x0[!is.na(x0)]
nx0 <- nx0[-c(3,9)]
nx0 <- (nx0-pck$center)/pck$scale
ndf <- data.frame(nx0 %*% pck$loadings)
predict(ktp,ndf)

library(MASS)
ldamod <- lda(species ~ ., newko)
(tt <- table(newko$species,predict(ldamod)$class))

1-sum(diag(tt))/sum(tt)

#16.5#----

cvr <- rfcv(newko[,-1],newko[,1],step=0.9)
cbind(nvars=cvr$n.var,error.rate=cvr$error.cv)

fmod <- randomForest (species ~ ., newko, mtry=6)
(tt <- table(actual=newko$species, predicted=predict(fmod)))

1-sum(diag(tt))/sum(tt)

pck <- princomp(newko[,-1])
pcdf <- data.frame(species=newko$species,pck$scores)
fmod <- randomForest(species ~ ., pcdf, mtry=6)
tt <- table(actual=newko$species,predicted=predict(fmod))

1-sum(diag(tt))/sum(tt)






#Chapter 16 exercise#----

#a)
#Fit the default tree model with pimaTest as response
library(rpart)
library(rpart.plot)
(tmod <- rpart(test ~ ., pimaTrain))

plot(tmod)
text(tmod)
plot(fmod, main = "")
text(tmod)
par(mfrow=c(1,1))
rpart.plot(tmod, type = 3)

#b)
#What fraction of cases in the test set are correctly classified according to this model
probTest <- predict(tmod,newdata = pimaTest)

thresh <- seq(0.01,0.9,0.01)
Sensitivity <- numeric(length(thresh))
Specificity <- numeric(length(thresh))

for(j in seq(along=thresh)) {
  pp <- ifelse(probTest < thresh[j],0,1)
  xx <- xtabs( ~ test + pp, pimaTest)
  Specificity[j] <- xx[1,1]/(xx[1,1]+xx[1,2])
  Sensitivity[j] <- xx[2,2]/(xx[2,1] + xx[2,2])
}

matplot(thresh,cbind(Sensitivity,Specificity),type = "l",ylim = c(0,1), xlab = "Threshold",ylab = "Proportion",lty=1:2)
plot(1-Specificity,Sensitivity,type="l")
abline(0,1,lty=2)

pp <- ifelse(probTest < 0.59,0,1)
table(pp, pimaTest$test)
xx <- xtabs( ~ test + pp, pimaTest)
xx

#c)
#Use LOO cross-validation to select the optimal tree size

tmode <- rpart(test ~ ., pimaTrain, cp=0.001)
printcp(tmode)
plotcp(tmode)
rpart.plot(tmode,type=3)

tmodr <- prune.rpart(tmode,0.005)
rpart.plot(tmodr,type=3)

#Check performance
probTest <- predict(tmodr,newdata = pimaTest)

predTest <- rep(0, 100)   # assigning predicted prob of fire
predTest[probTest > .5] = 1

table(predTest, pimaTest$test)

#d)
#Fit the default random forest model to the training set. Use this to classify the cases in the test set
library(randomForest)
fmod <- randomForest(test ~ ., pimaTrainr)

probTest <- predict(fmod,newdata = pimaTest)

predTest <- rep(0, 100)   # assigning predicted prob of fire
predTest[probTest > .5] = 1

table(predTest, pimaTest$test)
rpart.plot(fmod,type=3)
#How many are correctly classified
#e)
#Use the subsample size selection method for the random forest 

library(randomForest)
cvr <- rfcv(pimaTrainr[,-1],pimaTrainr[,1],step=0.9)
cbind(nvars=cvr$n.var,error.rate=cvr$error.cv)
fmod <- randomForest(test ~ ., pimaTrainr,mtry=8)

#evaluate the performance on the test set

probTest <- predict(fmod,newdata = pimaTest)

predTest <- rep(0, 100)   # assigning predicted prob of fire
predTest[probTest > .5] = 1

table(predTest, pimaTest$test)

#Chapter17#----
#17.3#----
library(nnet)
data(ozone, package="faraway")

set.seed(123)
nnmdl <- nnet(O3 ~ temp + ibh + ibt, ozone, size=2, linout=T)

sx <- scale(ozone)

bestrss <- 10000
for(i in 1:100) {
  nnmdl <- nnet(O3 ~ temp + ibh + ibt, sx, size = 2, linout=T, trace=F)
  cat(i,nnmdl$value,"\n")
  if(nnmdl$value < bestrss) {
    bestnn2 <- nnmdl
    bestrss <- nnmdl$value
  }
}
bestnn$value

summary(bestnn)

1-88.03/sum((sx[,1]-mean(sx[,1]))^2)

ozmeans <- colMeans(ozone)
ozscales <- apply(ozone,2,sd)
xx <- expand.grid(temp=seq(-3,3,0.1),ibh=0, ibt=0)
plot(xx$temp*ozscales['temp']+ozmeans['temp'], predict(bestnn,new=xx)*ozscales['O3']
     +ozmeans['O3'], xlab="Temp", ylab="O3", type = "l")
xx <- expand.grid(temp=0,ibh=seq(-3,3,0.1), ibt=0)
plot(xx$ibh*ozscales['ibh']+ozmeans['ibh'], predict(bestnn,new=xx)*ozscales['O3']
     +ozmeans['O3'], xlab="IBH", ylab="O3", type = "l")
xx <- expand.grid(temp=0,ibh=0, ibt=seq(-3,3,0.1))
plot(xx$ibt*ozscales['ibt']+ozmeans['ibt'], predict(bestnn,new=xx)*ozscales['O3']
     +ozmeans['O3'], xlab="IBT", ylab="O3", type = "l")

bestrss <- 10000
for(i in 1:100) {
  nnmdl <- nnet(O3 ~ temp + ibh +ibt, sx, size=2, linout=T, decay=0.001, trace=F)
  cat(i,nnmdl$value,"\n")
  if(nnmdl$value < bestrss) {
    bestnn <- nnmdl
    bestrss <- nnmdl$value
  }
}
bestnn$value

xx <- expand.grid(temp=seq(-3,3,0.1), ibh=0, ibt=0)





#Chapter 17 exercise#----
#a)
#Fit a NN using the full data set, 2 hidden units, not rescaled#----

library(nnet)
library(MASS)
library(NeuralNetTools)
set.seed(123)
pimaTrain$test <- as.factor(pimaTrain$test)
nnmdl <- nnet(test ~ ., data = pimaTrain, size = 2, decay = 0.2) #trained on the normal data

summary(nnmdl)
plotnet(nnmdl)

probTest <- predict(nnmdl,newdata = pimaTest, type = "class")

thresh <- seq(0.01,0.9,0.01)
Sensitivity <- numeric(length(thresh))
Specificity <- numeric(length(thresh))

for(j in seq(along=thresh)) {
  pp <- ifelse(probTest < thresh[j],0,1)
  xx <- xtabs( ~ test + pp, pimaTest)
  Specificity[j] <- xx[1,1]/(xx[1,1]+xx[1,2])
  Sensitivity[j] <- xx[2,2]/(xx[2,1] + xx[2,2])
}

matplot(thresh,cbind(Sensitivity,Specificity),type = "l",ylim = c(0,1), xlab = "Threshold",ylab = "Proportion",lty=1:2)
plot(1-Specificity,Sensitivity,type="l")
abline(0,1,lty=2)

pp <- ifelse(probTest < 0.50,0,1)
table(pp, pimaTest$test)
xx <- xtabs( ~ test + pp, pimaTest)
xx

predTest <- rep(0, 100)   # assigning predicted prob of fire
predTest[probTest > .5] = 1

table(predTest, pimaTest$test)

table()
#compare to the data
#How many fitted values are calculated, how does this compare to the rows of the data set
#b)

pimaComplete$test <- as.factor(pimaComplete$test)
nnmdl <- nnet(test ~ ., data = pimaComplete, size = 2, decay=0.2)
probTest <- predict(nnmdl,newdata = pimaComplete, type = "class")
nnmdl$fitted.values
pp <- ifelse(probTest < 0.50,0,1)
table(pp, pimaComplete$test)
xx <- xtabs( ~ test + pp, pimaTest)
xx

plotnet(bestnn)
(23+13)/(164+13+23+332)
#Rescale the predictors to standard units and refit the model#----
pimaComplete$test <- as.numeric(pimaComplete$test)
pimann <- scale(pimaComplete[,c(1:9)])
dim(pimann)
View(pimann)
pimann$test <- ifelse(pimann$test == "2",1,0)
pimann$test <- as.factor(pimann$test)

colnames(pimaComplete)
View(pimascaledTrainr)
data.class(pimann$test)
pimann <- as.data.frame(pimann)
pimascaledTrainr$test <- as.factor(pimascaledTrainr$test)
data.class(pimascaledTrainr$test)

nnmdl <- nnet(test ~ ., data = pimann, size = 2, decay=0.2, trace = F)
nnmdl$fitted.values
dim(pimann)

bestrss <- 10000
for(i in 1:100) {
  nnmdl <- nnet(test ~ ., pimann, size = 8)
  cat(i,nnmdl$value,"\n")
  if(nnmdl$value < bestrss) {
    bestnn <- nnmdl
    bestrss <- nnmdl$value
  }
}
bestnn$value
bestnn$fitted.values
dim(pima)

pp <- ifelse(bestnn$fitted.values < 0.45,0,1)
length(bestnn$fitted.values)
table(pp, pimaComplete$test)
xx <- xtabs( ~ test + pp, pimaComplete)
xx

table(pimaTest$test, predict(nnmdl, pima, type = "class"))

probTest <- predict(bestnn,newdata = pima, type = "class")
colnames(pimaComplete)
predTest <- rep(0, 100)   # assigning predicted prob of fire
predTest[probTest > .5] = 1

table(predTest, pimaTest$test)
#Are the fitted values more informative now?
#c)
#Fit the model 100 times and save the best fitting model.

bestrss <- 10000
for(i in 1:100) {
  nnmdl <- nnet(test ~ ., pimascaledTrainr, size = 2)
  cat(i,nnmdl$value,"\n")
  if(nnmdl$value < bestrss) {
    bestnn <- nnmdl
    bestrss <- nnmdl$value
  }
}
768 - 523
bestnn$value

probTest <- predict(bestnn,newdata = pimaComplete, type = "class")
plotnet(bestnn)
pp <- ifelse(probTest < 0.90,0,1)
xx <- xtabs( ~ test + pp, pimaComplete)
xx
(65+50)/(65+50+305+112)
#What proportion of response values are classified correctly by the best model

probTest <- predict(bestnn,newdata = pimaTest)

predTest <- rep(0, 100)   # assigning predicted prob of fire
predTest[probTest > .5] = 1

table(predTest, pimaTest$test)

#d)
#Use test set from before

#e)
bestrss <- 10000
for(i in 1:100) {
  nnmdl <- nnet(test ~ ., sx, size = 8, linout = T, trace=F)
  cat(i,nnmdl$value,"\n")
  if(nnmdl$value < bestrss) {
    bestnn <- nnmdl
    bestrss <- nnmdl$value
  }
}

bestnn$value

probTest <- predict(bestnn,newdata = sxt)

predTest <- rep(0, 100)   # assigning predicted prob of fire
predTest[probTest > .5] = 1

table(predTest, pimaTest$test)
#data sets for the nn#----
View(str(pima))
pimascaled <- scale(pima)
dim(pimascaled)
table(is.na(pimascaled))

pimascaledComplete <- scale(pimaComplete)
dim(pimascaledComplete)
table(pimascaledComplete$test)
pimascaledComplete[,9] <- pimaComplete$test 
table(pimascaledComplete$test)
dim(pimascaledComplete)
table(pimaComplete$test)
View(pimascaledComplete)

dim(pimascaledComplete)
table(is.na(pimascaledComplete))

testIndex <- sample(532,100)

pimascaledTest <- pimascaledComplete[c(testIndex),]
pimascaledTrain <- pimascaled[-c(testIndex),]
pimascaledTrainr <- pimascaledComplete[-c(testIndex),]

dim(pimaComplete)
#othernn#----

library(neuralnet)
nn <- neuralnet(test ~ ., data = pimascaledTrainr, hidden=c(2,1))
nn$result.matrix
plot(nn)

nn.results <- compute(nn,pimascaledTest)
results <- data.frame(actual = pimaTest$test, prediction = nn.results$net.result)
results

roundedresults <- sapply(results,round,digits=0) #getting negative 1 from rounding predictions
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)




normalize <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}
maxmindf <- as.data.frame(lapply(pimaComplete, normalize))

dim(maxmindf)

testset <- maxmindf[1:100,]
trainset <- maxmindf[101:532,]

nn <- neuralnet(test ~ ., data = testset, hidden = c(2,1))
nn$result.matrix
plot(nn)

nn.results <- compute(nn,trainset)
results <- data.frame(actual = pimaTrainr$test, prediction = nn.results$net.result)
results

roundedresults <- sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)




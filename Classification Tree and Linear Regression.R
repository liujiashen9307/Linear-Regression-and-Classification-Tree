#####Call All Packages We Need###
library(rpart)        
library(rpart.plot)   
library(rattle)  
library(corrgram)
library(MASS)
library(stargazer)
###Question 1#####
####Prepare the Data Set###

Bank<-read.csv("Bank.csv",sep = ";")

####Summary the Data###

summary(Bank)

####Check the missing values###

colSums(is.na(Bank))#No Missing Values##

####Transform the Data###

Bank$y<-ifelse(Bank$y=="yes",1,0)
Bank$default<-ifelse(Bank$default=="yes",1,0)
Bank$housing<-ifelse(Bank$housing=="yes",1,0)
Bank$loan<-ifelse(Bank$loan=="yes",1,0)

for(i in (1:4521)){
  if(Bank$pdays[i]==-1){
    Bank$pdays[i]<-999
  }
}
####Question 2#############
####Descriptive Analysis####


####Balance and Success####
layout(  matrix(c(1,2), 1, 2, byrow = TRUE)  ) 
hist(Bank$balance,main = "Histogram of Balance",xlab = "Balance",col="orange")
plot(density(Bank$balance),main="Density of Balance",xlab = "Balance",col="orange",las=1)
polygon(density(Bank$balance), col = "orange")
par(mfrow=c(1,1))

##Duration Between Calls###
boxplot(pdays~poutcome, data=Bank, main="Boxplot of Duration From Last Call by Previous Outcome",
        xlab="Previous Outcome", ylab="Duration",col="blue") 

###ScatterPlot of Successful Contract###

Suc<-Bank[Bank$y==1,]

pairs(~balance+duration+previous,data = Bank,
      pch=20, cex=1.2, col=3,main="Correlation between Balance,Duration and Number of Contacts when contract is signed")

####Correlation Between Variables#####
corrgram(Bank, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Correlation Between Variables")

####Summary of in-numeric Variables###

InNumeric<-Bank[,c(2,3,4,5,7,8,9)]
summary(InNumeric)

###Question 3#####
###Classification Trees####

treemodelA<-y~age+job+marital+education+default+balance+housing+loan+contact+day+month+campaign+pdays+previous
treemodelB<-y~default+loan+campaign+pdays+previous+balance+housing+duration+job+marital

treeRsltA <- rpart(treemodelA, data=Bank, method="class", parms = list(split = "information"))
treeRsltB <- rpart(treemodelB, data=Bank, method="class", parms = list(split = "information"))

layout(matrix(c(1,2), 1, 2, byrow = TRUE)) 
rpart.plot(treeRsltA, box.col=c("yellow", "green")[treeRsltA$frame$yval], extra = 1,main="Model A")
rpart.plot(treeRsltB, box.col=c("yellow", "green")[treeRsltB$frame$yval], extra = 1,main="Model B")
par(mfrow=c(1,1))
# Plot the tree as a set of rules
asRules(treeRsltA)
asRules(treeRsltB)


####Regression Analysis#####

RegModel<-y ~ age + job + marital + education + default + balance + housing + 
  loan + contact + campaign + pdays+previous + poutcome

regRslt <- lm(RegModel, data = Bank)


rsltStep <- stepAIC(regRslt, direction="both")
rsltStep$anova 

###Logit Regression###

logRegRslt<-glm(RegModel, data=Bank, binomial(link="logit"))
LogrsltStep<-stepAIC(logRegRslt,direction = "both")
LogrsltStep$anova


###Part 4####

#Define Function###

accRates <- function(s){
  FPR  <- sum((ypred > s)*(yvalue==0))/sum(yvalue==0)
  TPR  <- sum((ypred > s)*(yvalue==1))/sum(yvalue==1)
  return(c(FPR = FPR, TPR = TPR))
}

smpl <- sample(1:nrow(Bank), floor(0.7*nrow(Bank)))
smpl <- sort(smpl)

# Selection of the training and test sets
BankTraining <- Bank[smpl,]
BankHandout <- Bank[-smpl,]
RegreModel<-y ~ age + marital + education + housing + loan + contact + campaign + poutcome
rsltLogitModel <- glm(RegreModel, data=BankHandout, binomial(link="logit"))
yvalue <- BankHandout$y
ypred  <- predict.glm(rsltLogitModel, BankHandout, type="response")
ypred<-as.numeric(ypred > 0.5) 
####Accuracy Rate Logit####
accRates(0.5)
ypred1  <- as.numeric(ypred > 0.5) 

# Confusion matrix
table(ypred1,yvalue)
table(Predicted = ypred1, Observed = yvalue)


####AR Linear Regression###

RegreModel2<-y ~ job + marital + housing + loan + contact + campaign + poutcome
RegModel<-lm(RegreModel2,BankHandout)
ypred  <- predict.lm(RegModel, BankHandout, type="response")
ypred<-as.numeric(ypred > 0.5) 
accRates(0.5)

ypred2<- as.numeric(ypred > 0.5) 

table(ypred2,yvalue)
table(Predicted = ypred2, Observed = yvalue)
###AR Tree Model A####

ypred<-predict(treeRsltA,BankHandout,type="prob")[,2]
ypred<-as.numeric(ypred > 0.5) 
accRates(0.5)

ypred3<-as.numeric(ypred > 0.5)
table(ypred3,yvalue)
table(Predicted = ypred3, Observed = yvalue)
###AR Tree Model B####

ypred<-predict(treeRsltB,BankHandout,type="prob")[,2]
ypred<-as.numeric(ypred > 0.5) 
accRates(0.5)
ypred4<- as.numeric(ypred > 0.5) 
table(ypred4,yvalue)
table(Predicted = ypred4, Observed = yvalue)

####ROC Curve######

# Estimate
treeRsltQA <- rpart(treemodelA, data=BankTraining, method="class")
treeRsltQB <- rpart(treemodelB,data = BankTraining,method = "class")

# Predict
yvalue <- BankHandout$y
ypred  <- predict(treeRsltQA,BankHandout,type = "prob")[,2]

# Classification performances
accRatesFunTreeA <- Vectorize(accRates)
accRatesValTreeA <- accRatesFunTreeA(seq(0,1,by=.005))

ypred <- predict(treeRsltB,BankHandout,type = "prob")[,2]

accRatesFunTreeB <- Vectorize(accRates)
accRatesValTreeB <- accRatesFunTreeB(seq(0,1,by=.005))
#--- Prediction: estimate logistic model on the training set and find 
#--- predictions for the test set

###--Regression Model###

RegModel<-y ~ job + marital + housing + loan + contact + campaign + poutcome

RsltQ<-lm(RegModel,data = BankTraining)

ypred<- predict(RsltQ,newdata=BankHandout,type=c("response"))

accRatesFunReg <- Vectorize(accRates)
accRatesValReg <- accRatesFunReg(seq(0,1,by=.005))

# Estimate (maximum number of iterations was increased from 
# default 25 to 100)

logitmodel<-y~age + marital + education + housing + loan + contact + campaign + poutcome
logitRsltQ <- glm(logitmodel,data=BankTraining, binomial(link="logit"), maxit = 100)

# Predict

ypred  <- predict(logitRsltQ, newdata=BankHandout, type=c("response"))

accRatesFunReg <- Vectorize(accRates)
accRatesReg <- accRatesFunReg(seq(0,1,by=.005))

# Classification performances
accRatesFunLogit <- Vectorize(accRates)
accRatesValLogit <- accRatesFunLogit(seq(0,1,by=.005))


plot(accRatesValTreeA[1,],accRatesValTreeA[2,],
     col="red",lwd=2,type="l",
     xlab="False positive rate",
     ylab="True positive rate",main = "ROC Curves of four models")
lines(accRatesValTreeB[1,],accRatesValTreeB[2,],
      col="yellow",lwd=2,type="l")
lines(accRatesValLogit[1,],accRatesValLogit[2,],
      col="green",lwd=2,type="l")
lines(accRatesValReg[1,],accRatesValReg[2,],
      col="blue",lwd=2,type="l")

lines(c(0,1),c(0,1))


#######Cross Validation####

####Precision Measurement Included#####

accRates <- function(s){
  FPR  <- sum((ypred > s)*(yvalue==0))/sum(yvalue==0)
  TPR  <- sum((ypred > s)*(yvalue==1))/sum(yvalue==1)
  PRE  <- sum((ypred > s)*(yvalue==1))/(sum((ypred > s)*(yvalue==1))+sum((ypred > s)*(yvalue==0)))
  return(c(FPR = FPR, TPR = TPR,PRE=PRE))
}

####Logit Model###

logitmodel<-y~age + marital + education + housing + loan + contact + campaign + poutcome
RegreModel2<-y ~ job + marital + housing + loan + contact + campaign + poutcome

# Set the number of folds and assign random fold to the observations in 

K <- 5 # Number of folds
Bank$folds <- sample(1:K, nrow(Bank), replace = TRUE)

# Define empty list to store results
tmpLogit <- list()

for (fold in 1:K) {
  
  dsTrain <- Bank[-which(Bank$folds == fold),]
  dsTest  <- Bank[which(Bank$folds == fold),]
  
  rsltLogit <- glm(logitmodel, data=dsTrain, binomial(link="logit"),
                   control = list(maxit = 100))
  
  # Predict outcomes test set
  ypred  <- predict(rsltLogit, dsTest, type = "response")
  yvalue <- dsTest$y
  

  tmpLogit[[fold]] <- as.data.frame(t(accRates(0.5)))
}

perfLogit <- do.call("rbind",tmpLogit)
V1<-colMeans(perfLogit)

#####Regression Model###

RegreModel<-y ~ job + marital + housing + loan + contact + campaign + poutcome

tmpReg <- list()

for (fold in 1:K) {
  
  dsTrain <- Bank[-which(Bank$folds == fold),]
  dsTest  <- Bank[which(Bank$folds == fold),]

  rsltReg <- lm(RegreModel, data=dsTrain)
  
  # Predict outcomes test set
  ypred  <- predict(rsltReg, dsTest, type = "response")
  yvalue <- dsTest$y
  
  
  tmpReg[[fold]] <- as.data.frame(t(accRates(0.5)))
}

perfReg <- do.call("rbind",tmpReg)
V2<-colMeans(perfReg)

####Tree Model B#######

tmpTreeB <- list()

for (fold in 1:K) {
  
  dsTrain <- Bank[-which(Bank$folds == fold),]
  dsTest  <- Bank[which(Bank$folds == fold),]
  treeRsltB <- rpart(treemodelB,dsTrain, method="class", parms = list(split = "information"))
  # outcomes test set
  ypred  <- predict(treeRsltB,dsTest,type = "prob")[,2]
  yvalue <- dsTest$y
  
  
  tmpTreeB [[fold]] <- as.data.frame(t(accRates(0.5)))
}

perftreeB <- do.call("rbind",tmpTreeB )
V3<-colMeans(perftreeB )

CV<-rbind(V1,V2,V3)

write.csv(CV,"1.csv")

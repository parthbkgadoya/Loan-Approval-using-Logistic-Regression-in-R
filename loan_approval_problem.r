
#load data
german <- read.csv("german_credit.csv")
german1 <- read.csv("german_credit.csv")
#look at data
str(german)
cor(german1)
#convert categorical variables to factors data type
-F=c(1,2,4,5,7,8,9,10,11,12,13,15,16,17,18,19,20,21)
for(i in F) 
  german[,i]=as.factor(german[,i])

#Exploratory data analysis
plot(german$Creditability,german$Account.Balance)
table(german$Age..years.>30,german$Creditability)

#cross-tabulation and test independence
library(gmodels)

with(german,CrossTable(Creditability, german$Instalment.per.cent, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T))
with(german,CrossTable(Creditability, german$Type.of.apartment, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T))

with(german,CrossTable(Creditability, german$Sex...Marital.Status, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T))

with(german,CrossTable(Creditability, german$No.of.dependents, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T))


#graph
brk <- seq(0, 80, 10)
hist(german$Duration.of.Credit..month., breaks=brk, xlab = "Credit Month", ylab = "Frequency", main = "Freqency of Credit Months ", cex=0.4,col='lightblue') 
grid(col = "lightgray", lty = "dotted")

table(german$Duration.of.Credit..month.)

hist(german$Age..years., xlab = "Age", ylab = "Frequency", main = "Age Distribution", cex=0.4,col='lightblue')
grid(col = "lightgray", lty = "dotted")

hist(german$Credit.Amount, xlab = "Credit Amount", ylab = "Frequency", main = "Credit Amount Distribution", cex=0.4,col='lightblue')
grid(col = "lightgray", lty = "dotted")

#multi scatter plot
library(lattice)
xyplot(german$Credit.Amount ~ german$Age..years. | german$Purpose, german, grid = TRUE, group = Creditability,auto.key = list(points = FALSE, rectangles = TRUE, space = "right"),main="Age vs credit amount for various purposes")
table(german$Purpose,german$Creditability)
xyplot(german$Credit.Amount ~ german$Age..years.|german$Sex...Marital.Status, german, grid = TRUE, group = german$Creditability,auto.key = list(points = FALSE, rectangles = TRUE, space = "right"),main="Age vs credit amount for Personal Status and Sex")

histogram(german$Credit.Amount~ german$Age..years. | german$Sex...Marital.Status, data = german, xlab = "Age",ylab = "credit amount",main="Distribution of Age and Personal status & sex")

#predictive model building

#baseline model : predicts most frequent outcome
base <- table(german$Creditability)
base
700/(300+700)
#accuracy : 70%

#model ALL variables
m <- glm(Creditability~.,data=Train,family = binomial)
summary(m)
#model4 <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Length.of.current.employment + Sex...Marital.Status,data = german,family = binomial)
#summary(model4)


#splitting data
library(caTools)
set.seed(88)
split <- sample.split(german$Creditability, SplitRatio = 0.7)

Train <- subset(german, split == TRUE)
Test <- subset(german, split == FALSE)

#training classifier model
model4 <- glm(Creditability ~ .-No.of.Credits.at.this.Bank -Most.valuable.available.asset-Instalment.per.cent-Duration.in.Current.address -Type.of.apartment-Occupation-No.of.dependents-Telephone-Foreign.Worker,data=Train,family=binomial)
summary(model4)
anova(m,model4,test="LRT")
step(m,direction = "both")


#making predictions
#predict5 <- predict(model5,newdata=Test,type="response")
predictedTest <- predict(model4,newdata=Test,type="response")
#predictedTrain <- predict(model4, type="response")
#analyze
#summary(predictedTrain)
tapply(predictedTrain,Train$Creditability, mean)


#table(Train$Creditability, predictedTrain > 0.5)
#(115+440)/(115+440+50+95)

#ROC curve
library(ROCR)

# Prediction function
ROCRpred = prediction(predictedTrain, Train$Creditability)


# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

#AUC calculation
AUCLog1=performance(ROCRpred, measure = "auc")@y.values[[1]]
cat("AUC: ",AUCLog1,"n")

#testing 
#making predictions

predictedTest <- predict(model4, newdata = Test, type="response")
#analyze
summary(predictedTest)
#tapply(predictedTrain,Train$Creditability, mean)


cM <- table(Test$Creditability, predictedTest > 0.5)
sum(cM[1:4])
(cM[1]+cM[4])/sum(cM[1:4])


#ROC curve
library(ROCR)

# Prediction function
ROCRpredT = prediction(predictedTest, Test$Creditability)
ROCRpredTFull = prediction(predictFull, Test$Creditability)

# Performance function
ROCRperfT = performance(ROCRpredT, "tpr", "fpr")
ROCRperfTL = performance(ROCRpredT, "lift", "rpp")
ROCRperfTFull = performance(ROCRpredTFull, "tpr", "fpr")


# Plot ROC curve
plot(ROCRperfT)
plot(ROCRperfTFull)

#plot Lift curve
plot(ROCRperfTL, main="Lift curve", colorize=TRUE)

# Add colors
plot(ROCRperfT, colorize=TRUE)

#AUC calculation
AUCLog2=performance(ROCRpredT, measure = "auc")@y.values[[1]]
cat("AUC: ",AUCLog2,"n")
AUCLog3=performance(ROCRpredTFull, measure = "auc")@y.values[[1]]
cat("AUC: ",AUCLog3,"n")

#add threshold
plot(ROCRperfT, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

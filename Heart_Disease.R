#load relevant library
suppressPackageStartupMessages(require(data.table))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(MASS))
suppressPackageStartupMessages(require(caret))
suppressPackageStartupMessages(require(caTools))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(caTools))
suppressPackageStartupMessages(require(plyr))
suppressPackageStartupMessages(require(ROSE))
suppressPackageStartupMessages(require(ggpubr))
suppressPackageStartupMessages(require(grid))
suppressPackageStartupMessages(require(rpart.plot))
suppressPackageStartupMessages(require(performance))
suppressPackageStartupMessages(require(scales))

setwd("~/Documents/BC2406/Sem2Team7 Project")
dataset.dt <- fread("heartdisease.csv")

## Data Cleaning
# Dataset check: Identify missing/erroneous values and datatype mismatch
summary(dataset.dt)

#Missing Values
colSums(is.na(dataset.dt))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

# Replace NA values with mode -> categorial
dataset.dt$education[is.na(dataset.dt$education)] <- Mode(dataset.dt$education)
dataset.dt$BPMeds[is.na(dataset.dt$BPMeds)] <- Mode(dataset.dt$BPMeds)
dataset.dt$totChol[is.na(dataset.dt$totChol)] <- Mode(dataset.dt$totChol)
dataset.dt$race[is.na(dataset.dt$race)] <- Mode(dataset.dt$race)

# Replace NA values with mean -> continuous
dataset.dt$cigsPerDay[is.na(dataset.dt$cigsPerDay)] <- mean(dataset.dt$cigsPerDay, na.rm = TRUE)
dataset.dt$BMI[is.na(dataset.dt$BMI)] <- mean(dataset.dt$BMI, na.rm = TRUE)
dataset.dt$heartRate[is.na(dataset.dt$heartRate)] <- mean(dataset.dt$heartRate, na.rm = TRUE)

## Data Cleaning
# Dataset check: Identify missing/erroneous values and datatype mismatch
summary(dataset.dt)

#Data Type mismatch
str(dataset.dt)
dataset.dt<-data.frame(lapply(dataset.dt,factor))
as.double.factor <- function(x) {as.integer(levels(x))[x]}

dataset.dt$age = as.double.factor(dataset.dt$age)
dataset.dt$cigsPerDay = as.double.factor(dataset.dt$cigsPerDay)
dataset.dt$totChol = as.double.factor(dataset.dt$totChol)
dataset.dt$sysBP = as.double.factor(dataset.dt$sysBP)
dataset.dt$diaBP = as.double.factor(dataset.dt$diaBP)
dataset.dt$BMI = as.double.factor(dataset.dt$BMI)
dataset.dt$heartRate = as.double.factor(dataset.dt$heartRate)

# Dataset check: After Data Cleaning Corrections
anyNA(dataset.dt)
summary(dataset.dt)
str(dataset.dt)

#-----------------------------------------------------------------------------------------------------------------
## Data Exploration - 1/4
# Boxplots to confirm the existence of outliers
# remove outliers

ggplot(data = dataset.dt, aes(cigsPerDay, CHD)) + geom_boxplot() 
outliers = which(dataset.dt$cigsPerDay>55)

ggplot(data = dataset.dt, aes(totChol, CHD)) + geom_boxplot() 
outliers = append(outliers, which(dataset.dt$totChol>500))

ggplot(data = dataset.dt, aes(sysBP, CHD)) + geom_boxplot()
outliers = append( outliers,which(dataset.dt$sysBP>250))

ggplot(data = dataset.dt, aes(heartRate, CHD)) + geom_boxplot()
outliers = append(outliers, which(dataset.dt$heartRate>130))

dataset.dt <- dataset.dt[-outliers, ]

#no serious outliers
ggplot(data = dataset.dt, aes(diaBP, CHD)) + geom_boxplot() 
#-----------------------------------------------------------------------------------------------------------------
## Data Exploration - 2/3
###Individual Variables 

#BMI
ggplot(dataset.dt, aes(BMI)) +       
  geom_histogram(binwidth=2, color="black", fill="pink") +
  geom_density(aes(y = ..density.. * (nrow(dataset.dt) * 2)), col = 2) +
  geom_vline(data=dataset.dt, aes(xintercept=mean(BMI), color="red"),
              linetype="dashed") +
  labs(title="Histogram of BMI", x = "BMI", y="Count")

#Age
ggplot(dataset.dt, aes(age)) +       
  geom_histogram(binwidth=2, color="black", fill="pink") +
  geom_density(aes(y = ..density.. * (nrow(dataset.dt) * 2)), col = 2) +
  geom_vline(data=dataset.dt, aes(xintercept=mean(age), color="red"),
             linetype="dashed") +
  labs(title="Histogram of Age", x = "Age", y="Count")

#CHD
ggplot(dataset.dt, aes(CHD)) +       
  geom_histogram(stat="count", color="black", fill="pink") +
  labs(title="Histogram of CHD", x = "CHD", y="Count") +
  scale_x_discrete(labels=c("0"="No","1"="Yes")) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)

#race
ggplot(dataset.dt, aes(race)) +       
  geom_histogram(stat="count", color="black", fill="pink") +
  labs(title="Histogram of Race", x = "Race", y="Count") +
  scale_x_discrete(labels=c("1"="White", "2"="Black", "3"="Hispanic", "4"="Non-Hispanic/Other Races",  "5"="Asian")) +
geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)

#gender
ggplot(dataset.dt, aes(male)) +       
  geom_histogram(stat="count", color="black", fill="pink") +
  labs(title="Histogram of Gender", x = "Gender", y="Count") +
  scale_x_discrete(labels=c("1"="Male", "0"="Female")) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)
#-----------------------------------------------------------------------------------------------------------------
## Data Exploration - 3/4
### CHD against each X variable ###
#Categorical X

## Gender
ggplot(data=dataset.dt, aes(male, fill=CHD)) + geom_bar(position= "fill") +
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + 
  labs(title="Heart Disease based on Gender", x = "Gender", y="Count") +
  scale_x_discrete(labels=c("0"="Female","1"="Male"))
# A higher proportion of males have heart disease as compared to females

## Education
ggplot(data=dataset.dt, aes(education, fill=CHD)) + geom_bar(position= "fill") +
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + 
  labs(title="Heart Disease based on Education", x = "Education", y="Count") +
  scale_x_discrete(labels=c("1"="Primary or Below", "2"="Secondary", "3"="High School", "4"="Tertiary"))
# A higher proportion of individuals with education of primary and below, have heart disease 
# as compared to have higher tertiary education.

## Smoking Status
ggplot(data=dataset.dt, aes(currentSmoker, fill=CHD)) + geom_bar(position= "fill") +
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + 
  labs(title="Heart Disease based on Smoking Status", x = "Smoking Status", y="Count") +
  scale_x_discrete(labels=c("0"="No","1"="Yes"))
# A higher proportion of Smokers have heart disease as compared to Non-Smokers.
# Slight difference

## Blood Pressure Medications
ggplot(data=dataset.dt, aes(BPMeds, fill=CHD)) + geom_bar(position= "fill") +
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + 
  labs(title="Heart Disease based on Blood Pressure", x = "Blood Pressure", y="Count") +
  scale_x_discrete(labels=c("0"="No","1"="Yes"))
# A higher proportion of individuals, who take Blood Pressure Medications, have heart disease as compared to those who do not.

## Stroke
ggplot(data=dataset.dt, aes(prevalentStroke, fill=CHD)) + geom_bar(position= "fill") +
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + 
  labs(title="Heart Disease based on Stroke", x = "Stroke", y="Count") +
  scale_x_discrete(labels=c("0"="No","1"="Yes"))
# A higher proportion of individuals, who previously had a stroke, have heart disease as compared to those who do not.

## Hypertension
ggplot(data=dataset.dt, aes(prevalentHyp, fill=CHD)) + geom_bar(position= "fill") +
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + 
  labs(title="Heart Disease based on Hypertension", x = "Hypertension", y="Count") +
  scale_x_discrete(labels=c("0"="No","1"="Yes"))
# A higher proportion of individuals, who is hypertensive, have heart disease as compared to those who is not.

## Diabetes
ggplot(data=dataset.dt, aes(diabetes, fill=CHD)) + geom_bar(position= "fill") +
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) +  
  labs(title="Heart Disease based on Diabetes", x = "Diabetes", y="Count") +
  scale_x_discrete(labels=c("0"="No","1"="Yes"))
# A higher portion of individuals, who have diabetes, have heart disease as compared to those who do not.

## Diet
ggplot(data=dataset.dt, aes(diet, fill=CHD)) + geom_bar(position= "fill") +
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + 
  labs(title="Heart Disease based on Diet", x = "Diet", y="Count") +
  scale_x_discrete(labels=c("0"="Less than once per day","1"="Once or more per day"))
# A higher portion of individuals, who does not consume at least 1 Fruit and 1 Vegetables per day, have heart disease 
# as compared to those who do.

## Physical Activity
ggplot(data=dataset.dt, aes(phyact, fill=CHD)) + geom_bar(position= "fill") +
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + 
  labs(title="Heart Disease based on Physical Activity", x = "Physical Activity", y="Count") +
  scale_x_discrete(labels=c("0"="No","1"="Yes"))
# A higher portion of individuals, who had no physical activity or exercise in the last 30 days, have heart disease 
# as compared to those who had.
# Slight Difference

## Family History
ggplot(data=dataset.dt, aes(famhist, fill=CHD)) + geom_bar(position= "fill") +
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + 
  labs(title="Heart Disease based on Family History", x = "Family History", y="Count") 
# A higher portion of individuals, who have a family history of CHD, have heart disease as compared to those who do not.

## Covid
#stacked
dataset.dt %>%
  dplyr::count(covid, CHD) %>%       
  group_by(covid) %>%
  
  ggplot() + aes(covid, n, fill=CHD) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + 
  geom_text(aes(label=paste0(n)),
            position=position_stack(vjust=0.5))+
  labs(title="Heart Disease based on Covid", x = "Covid", y="Count") +
  scale_x_discrete(labels=c("0"="No","1"="Yes"))

#percentage
dataset.dt %>%
  group_by(covid) %>%
  dplyr::count(covid, CHD) %>%  
  mutate(pct= prop.table(n) * 100) %>%
  
  ggplot() + aes(covid, pct, fill=CHD) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + 
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5))+
  labs(title="Heart Disease based on Covid", x = "Covid", y="Count") +
  scale_x_discrete(labels=c("0"="No","1"="Yes"))

## Race
#stacked
dataset.dt %>%
  dplyr::count(race, CHD) %>%       
  group_by(race) %>%
  
  ggplot() + aes(race, n, fill=CHD) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(name="Heart Diseease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + 
  geom_text(aes(label=paste0(n)),
            position=position_stack(vjust=0.5))+
  labs(title="Heart Disease based on Race", x = "Race", y="Count") +
  scale_x_discrete(labels=c("1"="White", "2"="Black", "3"="Hispanic", "4"="Non-Hispanic/Other Races",  "5"="Asian"))

#percentage
dataset.dt %>%
  dplyr::count(race, CHD) %>%       
  group_by(race) %>% 
  mutate(pct= prop.table(n) * 100) %>%
  
  ggplot() + aes(race, pct, fill=CHD) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + 
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  labs(title="Percentage of Heart Disease based on Race", x = "Race", y="Count") +
  scale_x_discrete(labels=c("1"="White", "2"="Black", "3"="Hispanic", "4"="Non-Hispanic/Other Races",  "5"="Asian"))

#Continuous X
#BMI
ggplot(dataset.dt,aes(BMI, fill = CHD)) + 
  geom_density(lwd = 2, show.legend = T, alpha = 0.5) + 
  labs(title = "Heart Disease Based on Body Mass Index", x = "BMI") + 
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + theme(legend.position="bottom")
# Individuals with Heart Disease generally have a higher BMI than Individuals without Heart Disease

#sysBP
ggplot(dataset.dt,aes(sysBP, fill = CHD)) + 
  geom_density(lwd = 2, show.legend = T, alpha = 0.5) + 
  labs(title = "Heart Disease based on systolic BP", x = "Systolic Blood Pressure") + 
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + theme(legend.position="bottom")
# Individuals with Heart Disease generally have a higher Systolic Blood Pressure than Individuals without Heart Disease

#diaBP
ggplot(dataset.dt,aes(diaBP, fill = CHD)) + 
  geom_density(lwd = 2, show.legend = T, alpha = 0.5) + 
  labs(title = "Heart Disease based on diastolic BP", x = "Diastolic Blood Pressure") + 
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + theme(legend.position="bottom")
# Individuals with Heart Disease generally have a higher Diastolic Blood Pressure level than Individuals without Heart Disease

#cigsPerDay
ggplot(dataset.dt,aes(cigsPerDay, fill = CHD)) + 
  geom_density(lwd = 2, show.legend = T, alpha = 0.5) + 
  labs(title = "Heart Disease based on Cigarettes Per Day", x = "Cigarettes Per Day") + 
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + theme(legend.position="bottom")
# Individuals without Heart Disease tend to smoke lesser Cigarettes Per Day as compared to Individuals with Heart Disease

#totChol
ggplot(dataset.dt,aes(totChol, fill = CHD)) + 
  geom_density(lwd = 2, show.legend = T, alpha = 0.5) + 
  labs(title = "Heart Disease based on Total Cholesterol", x = "Total Cholestrol") + 
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + theme(legend.position="bottom")
# Individuals with Heart Disease generally have a higher total cholesterol level than Individuals without Heart Disease

#Heart Rate
ggplot(dataset.dt,aes(heartRate, fill = CHD)) + 
  geom_density(lwd = 2, show.legend = T, alpha = 0.5) + 
  labs(title = "Heart Disease based on Heart Rate", x = "Heart Rate") + 
  scale_fill_manual(name="Heart Disease",labels=c("1"="Yes", "0"="No"), values = c("maroon","pink")) + theme(legend.position="bottom")
# Not Significant 
#-----------------------------------------------------------------------------------------------------------------
## Data Exploration - 4/4
### Multicollinearity 
LM.all <- glm(CHD ~ . , family = binomial, data = dataset.dt)
check_collinearity(LM.all)

#-----------------------------------------------------------------------------------------------------------------
## Modelling

#train-test split
set.seed(8)
train <- sample.split(Y = dataset.dt$CHD, SplitRatio = 0.7)
trainset <- subset(dataset.dt, train == T)
testset <- subset(dataset.dt, train == F)

#Correct imbalanced dataset on trainset data
trainset = ovun.sample(CHD~., data=trainset, method ="under")
trainset=trainset$data
summary(trainset)

#full model-----
full.model<-glm(CHD ~ ., data=trainset, family=binomial)
summary(full.model)
coef(full.model)

#step wise variable selection -------
step.model<-full.model %>% stepAIC(trace=FALSE) 
summary(step.model)
coef(step.model)

#prediction accuracy of full log reg model -----
probabilities2<-predict(full.model, testset, type="response")
predicted.classes2<-ifelse(probabilities2>0.5, 1, 0)
table2<-table(testdata = testset$CHD, logreg.predict=predicted.classes2, deparse.level = 2)
table2
observed.class2<-testset$CHD
round(mean(predicted.classes2 == observed.class2),4)

#prediction accuracy of stepwise log reg model -----
probabilities3<-predict(step.model, testset, type="response")
predicted.classes3<-ifelse(probabilities3>0.5, 1, 0)
table3<-table(testdata = testset$CHD, logreg.predict=predicted.classes3, deparse.level = 2)
table3
observed.class3<-testset$CHD
round(mean(predicted.classes3==observed.class3),4)

#to check baseline reference for categorical variables
levels(dataset.dt$male)
levels(dataset.dt$education)
levels(dataset.dt$currentSmoker)
levels(dataset.dt$BPMeds)
levels(dataset.dt$prevalentStroke)
levels(dataset.dt$prevalentHyp)
levels(dataset.dt$diabetes)
levels(dataset.dt$diet)
levels(dataset.dt$phyact)
levels(dataset.dt$covid)
levels(dataset.dt$famhist)
levels(dataset.dt$race)

#-----------------------------------------------------------------------------------------------------------------
#CART
set.seed(8)

model <- rpart(CHD ~ ., data = trainset, method = 'class',
               control = rpart.control(minsplit = 2, cp = 0))
printcp(model)
plotcp(model)
summary(trainset)
#-------------- Automated Search for Optimal Tree (CP)--------------------------------
CVerror.cap <- model$cptable[which.min(model$cptable[, "xerror"]), "xerror"] +
  model$cptable[which.min(model$cptable[, "xerror"]), "xstd"]
i <- 1; j <- 4
while (model$cptable[i,j]>CVerror.cap){
  i <- i + 1
}

cp.opt = ifelse(i > 1, sqrt(model$cptable[i,1] * model$cptable[i-1,1]), 1)
model2 <- prune(model, cp = cp.opt)
printcp(model2)

rpart.plot(model2,nn=T, main="CART For CHD")

cart.predict <- predict(model2, newdata = testset, type = "class")
table4 <- table(Testset.Actual = testset$CHD, cart.predict, deparse.level = 2)
table4
round(prop.table(table4), 3)
# Overall Accuracy
mean(cart.predict == testset$CHD) 

# Variable Importance
model$variable.importance
model2$variable.importance
#-----------------------------------------------------------------------------------------------------------------
#OUR MODELS VS FRAMINGHAM
#Predictive accuracy of stepwise logistic regression model
predicted.classes3f <- factor(predicted.classes3)
confusionMatrix(predicted.classes3f, observed.class3, mode = "everything", positive="1")
#Predictive accuracy of CART model
confusionMatrix(cart.predict, testset$CHD, mode = "everything", positive="1")

# Full logistic regression using only Framingham Risk Score variables
full.model2<-glm(CHD ~ male + age + currentSmoker + totChol + sysBP + BPMeds, data=trainset, family=binomial)
probabilities4<-predict(full.model2, testset, type="response")
predicted.classes4<-ifelse(probabilities4>0.5, 1, 0)
table5<-table(testdata = testset$CHD, logreg.predict=predicted.classes4, deparse.level = 2)
table5
observed.class4<-testset$CHD
round(mean(predicted.classes4==observed.class4),4)
#Predictive accuracy of log reg with FRS variables
predicted.classes4 <- factor(predicted.classes4)
confusionMatrix(predicted.classes4, observed.class4, mode = "everything", positive="1")

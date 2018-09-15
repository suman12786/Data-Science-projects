#Working Directory
# Assuming that the working directory will be set where unzip would have been done to get the data files.
# 

# Install the required packages


# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("corrplot")
#install.packages("gridExtra")
#install.packages("caTools")
#install.packages("e1071")
#install.packages("caret")#, dependencies = c("Depends", "Suggests"))
#install.packages("InformationValue")
#install.packages("ROCR")



library(MASS)
library(car)
library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot)
library(gridExtra)
library(caTools)
library(e1071)
library(caret)
library(ROCR)
library(InformationValue)



emp_survey <- read.csv("employee_survey_data.csv",stringsAsFactors = FALSE)
man_survey <- read.csv("manager_survey_data.csv",stringsAsFactors = FALSE)
in_time <- read.csv("in_time.csv",stringsAsFactors = FALSE)
out_time <- read.csv("out_time.csv",stringsAsFactors = FALSE)
general_data <- read.csv("general_data.csv",stringsAsFactors = FALSE)

# ---- CRISP-DM Framework - Stage 1 - Business Understanding  ---- #
# # Data given :
# 1) Manager Survey Data : By Employee ID
# 2) Employee Survey Data : By Employee ID
# 3) General Data about employee : By Employee ID
# 4) In time and out time : By Employee ID
#
# linkage key between all the data : Employee ID
# Attrition for employees based on historical data
# The attrition column in general_data  will provide
# information whether the employee left or not. 
# The level of attrition is high hence the HR Department wants to investigate on
# the possible driving factors which impacting this cause.
# Our GOAL: is to understand which factor the company/department should focus on, in order to control attrition.


# ---- CRISP-DM Framework - Stage 2 & 3 - Data Understanding & Data Preparation ---- #


# structure of the datasets
str(emp_survey) #4410 obs of 4 variables
str(man_survey) #4410 obs of 3 variables
str(in_time) #4410 obs of 262 variables
str(out_time) #4410 obs of 262 variables
str(general_data) # 4410 obs of 24 variables

# Data preparation on in_time and out_time #
colnames(in_time)[1] <- "EmployeeID"
colnames(in_time) <- gsub(pattern="X","",colnames(in_time))

colnames(out_time)[1] <- "EmployeeID"
colnames(out_time) <- gsub(pattern="X","",colnames(out_time))


# Check similar entery of employee IDs in both in_time and out_time
nrow(in_time) == length(unique(in_time$EmployeeID))
# TRUE
nrow(out_time) == length(unique(out_time$EmployeeID))
# TRUE

#We need to find holidays, one logic could be if both the columns in in and out file has NA, 
#then it's clear indication every one is absent and it's common holiday.

colnames(in_time) [which(sapply(in_time,function(x) sum(is.na(x))) == nrow(in_time))]
colnames(out_time)[which(sapply(out_time,function(x) sum(is.na(x))) == nrow(out_time))]

#So total 12 Holidays,  [1] "2015.01.01" "2015.01.14" "2015.01.26" "2015.03.05" "2015.05.01" "2015.07.17" "2015.09.17" "2015.10.02" "2015.11.09"
# [10] "2015.11.10" "2015.11.11" "2015.12.25"
#We can drop those columns.

in_time<- in_time[, colSums(is.na(in_time)) != nrow(in_time)]
out_time<- out_time[, colSums(is.na(out_time)) != nrow(out_time)]

ncol(in_time)  #250  Verified Dropped 12 columns
ncol(out_time) #250  Verified Dropped 12 columns


#Convert all the date columns from character to DateTime format
in_time[,-1] <- lapply(in_time[,-1], function(x) as_datetime(x))
out_time[,-1] <- lapply(out_time[,-1], function(x) as_datetime(x))

#Alternative
##in_time[,-1] <- sapply(in_time[,-1], ymd_hms) # character to datetime
##out_time[,-1] <- sapply(out_time[,-1], ymd_hms) # character to datetime


temp<-out_time[,-1]-in_time[,-1]
inOutTemp<-cbind(in_time[,c("EmployeeID")],temp)
names(inOutTemp)[1]<-paste("EmployeeID")
inOutTemp[,2:250] <- lapply(inOutTemp[,2:250], function(x) as.numeric(x))

#Calculaing the avg work hours of each employee
inOutTemp$avg_work_time <- rowMeans(inOutTemp[,2:250], na.rm = TRUE)

#write.csv(inOutTemp,"workAvgTime.csv")

#keep only employeeId and work hours

inOutTemp<-inOutTemp %>% select(EmployeeID,avg_work_time)

#merge all the dataframe for further processing.
#let's keep the NA value, better to treat them in combined data set. so we can evaluate the relationship.



hrData <- merge(emp_survey,general_data, by="EmployeeID", all = F)
hrData <- merge(hrData, man_survey, by="EmployeeID", all = F)
hrData <- merge(hrData, inOutTemp, by= "EmployeeID", all = F)

#str(hrData)




#Now we have Merged data tables along with derived metric avg_work_time
#Derive few more metric which is logically possible trigger fot attrition.

#employee is working mush less than the business hours on average.
hrData$inadequate_time <- ifelse(hrData$avg_work_time < 7, 1, 0)

#Employee is working more than the business hours on average. # Overtime, 1 indicates yes while 0 = no

hrData$over_time <- ifelse(hrData$avg_work_time > 8, 1, 0)

#Derived Metric Leave patter.
#We already Excluded Public holidays from in_time and out_time
#and in_time and out_time is symetric data frames. Calculating leftover NA from any one of the datframe
#will give the eployee leave time.

leave <- as.data.frame(ifelse(is.na(in_time[,-1]), 1, 0))

hrData$totalLeaveTaken<- rowSums(leave)

#View Final Merged data frame
#View(hrData)   

write.csv(hrData,"hrData.csv")


## Univariate Analysis - With segmentation for Attrition rate ##

#categorical variables first

plot1 <- ggplot(hrData) + 
  geom_bar(aes(x=BusinessTravel,fill=Attrition),position=position_dodge((width=0.7)))+
  labs(title="BusinessTravel & Attrition",x="Business Travel",y="No of Employees",fill="Attrition")

plot2 <- ggplot(hrData) + 
  geom_bar(aes(x=Department,fill=Attrition),position=position_dodge((width=0.7)))+
  labs(title="Department & Attrition",x="Department",y="No of Employees",fill="Attrition")

plot3 <- ggplot(hrData) + 
  geom_bar(aes(x=JobRole,fill=Attrition),position=position_dodge((width=0.7)))+
  labs(title="JobRole & Attrition",x="JobRole",y="No of Employees",fill="Attrition")

plot4 <- ggplot(hrData) + 
  geom_bar(aes(x=MaritalStatus,fill=Attrition),position=position_dodge((width=0.7)))+
  labs(title="MaritalStatus & Attrition",x="Marital Status",y="No of Employees",fill="Attrition")

plot5 <- ggplot(hrData) + 
  geom_bar(aes(x=StockOptionLevel,fill=Attrition),position=position_dodge((width=0.7)))+
  labs(title="StockOptionLevel & Attrition",x="StockOption Level",y="No of Employees",fill="Attrition")

plot6 <- ggplot(hrData) + 
  geom_bar(aes(x=EducationField,fill=Attrition),position=position_dodge((width=0.7)))+
  labs(title="Education Field & Attrition",x="Education Field",y="No of Employees",fill="Attrition")

plot7 <- ggplot(hrData) + 
  geom_bar(aes(x=Gender,fill=Attrition),position=position_dodge((width=0.7)))+
  labs(title="Gender & Attrition",x="Gender",y="No of Employees",fill="Attrition")

grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7)

#Numerical variable

plot8 <- ggplot(hrData) + geom_boxplot(aes(x=Attrition,y=Age,fill=Attrition)) +
  scale_y_continuous(breaks=seq(15,65,2)) +
  labs(title="Age and Attrition",x="Attrition",y="Age")

plot9 <- ggplot(hrData) + geom_histogram(aes(x=Age,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,250,10)) +
  scale_x_continuous(breaks=seq(18,60,2)) +
  labs(title="Distribution of Age",x="Age",y="No of Employees")


plot10 <- ggplot(hrData) + geom_boxplot(aes(x=Attrition,y=YearsAtCompany,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,40,4)) +
  labs(title="YearsAtCompany and Attrition",x="Attrition",y="YearsAtCompany")

plot11 <- ggplot(hrData) + 
  geom_histogram(aes(x=YearsAtCompany,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,600,30)) +
  labs(title="YearsAtCompany",x="YearsAtCompany",y="No of Employees")

plot12 <- ggplot(hrData) + geom_bar(aes(x=PerformanceRating,fill=Attrition),position=position_dodge((width=0.7)))+
  labs(title="PerformanceRating and Attrition",x="Employee Count",y="PerformanceRating")

grid.arrange(plot8,plot9,plot10,plot11,plot12)

#.......

####################### OUTLIER TREATMENTS ##################################

par(mfrow = c(4,2))
par(mar = rep(2,4))

boxplot(hrData$Age,main="Age")
boxplot(hrData$DistanceFromHome,main="DistanceFromHome")
boxplot(hrData$MonthlyIncome,main="MonthlyIncome")
boxplot(hrData$YearsAtCompany,main="YearsAtCompany")
boxplot(hrData$YearsSinceLastPromotion,main="YearsSinceLastPromotion")
boxplot(hrData$NumCompaniesWorked,main="NumCompaniesWorked")
boxplot(hrData$PercentSalaryHike,main="PercentSalaryHike")
boxplot(hrData$TrainingTimesLastYear,main="TrainingTimesLastYear")

#With Box plots we can identify the varibale which need outlier treatments.
#Our first candidate is MonthlyIncome

quantile(hrData$MonthlyIncome,seq(0,1,0.01))

plot_box_age <- ggplot() + geom_line(aes(x=c(0:100),y=quantile(hrData$MonthlyIncome,seq(0,1,0.01)))) + 
  scale_y_continuous(breaks=seq(0,200000,10000)) +
  labs(title="MonthlyIncome Quantile Increase",x="Quantile",y="MonthlyIncome")
grid.arrange(plot_box_age)

#There is a sharp shoot beyong 75 percentile, but also a part of 25% population can't be normalized.
#let's check the histogram.

plot_hist_age <- ggplot(hrData) + 
  geom_histogram(aes(x=MonthlyIncome,fill=Attrition),binwidth=5000) +
  labs(title="Monthly Income & Attrition",x="Monthly Income",y="No of employees",fill="Attrition")
grid.arrange(plot_hist_age)

#Higher attriations are in lower income group, we see no attrition in higher income group and our outlier
#exists in higher income group, so we feel no point of trating the outlier.


#......

###Now we have a nice understanding of data, safe to treat NA values if present.
#if we have na values in the dataset the prediction will be highly unstable
#Find out untrated NA values columns

sapply(hrData, function(x) sum(is.na(x)))


#cleaning NA for Environment Satisfaction(replace with median)
hrData$EnvironmentSatisfaction[which(is.na(hrData$EnvironmentSatisfaction))]<-median(hrData$EnvironmentSatisfaction,na.rm = TRUE)

#cleaning NA for Job Satisfaction(replace with median)
hrData$JobSatisfaction[which(is.na(hrData$JobSatisfaction))]<-median(hrData$JobSatisfaction,na.rm = TRUE)

#cleaning NA for WorkLifeBalance(replace with median)
hrData$WorkLifeBalance[which(is.na(hrData$WorkLifeBalance))]<-median(hrData$WorkLifeBalance,na.rm = TRUE)

#cleaning NA for NumCompaniesWorked(replace with mean)
hrData$NumCompaniesWorked[which(is.na(hrData$NumCompaniesWorked))]<-mean(hrData$NumCompaniesWorked,na.rm = TRUE)

#cleaning NA for TotalWorkingYears(replace with mean)
hrData$TotalWorkingYears[which(is.na(hrData$TotalWorkingYears))]<-mean(hrData$TotalWorkingYears,na.rm = TRUE)


#Verify if still any NA
sapply(hrData, function(x) sum(is.na(x)))


#Treating Categorical variable 
#With 2 levels
#Converting the "Attrition,Gender and Over18" attributes with 2 levels into numbers(0,1)
hrData$Attrition <- ifelse(hrData$Attrition == "Yes", 1,0)
hrData$Gender <- ifelse(hrData$Gender == "Female",1,0)
hrData$Over18 <- ifelse(hrData$Over18 == "Y", 1,0)

#With More Than 2 levels

#categorical attributes with more than 2 levels
colNamesWithMoreThan2Levels<-c("EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance",
                               "BusinessTravel","Department","EducationField", "Education",
                               "JobRole","MaritalStatus","JobInvolvement","JobLevel",
                               "PerformanceRating");

hrData_MoreThan2Level <- hrData[,colNamesWithMoreThan2Levels]

#Convert categorical attributes to factors
hrData_MoreThan2Level <- data.frame(sapply(hrData_MoreThan2Level, function(x) factor(x)))
str(hrData_MoreThan2Level)



#provide dummy attributes for each factor attributes
dummies <- data.frame(sapply(hrData_MoreThan2Level, function(x)
  data.frame(model.matrix(~x-1, data = hrData_MoreThan2Level))[,-1]))

#dropping the categorical attributes and adding the new dummy attributes.


colIndexWithMoreThan2Levels<-which(names(hrData) %in% colNamesWithMoreThan2Levels)

hrData <- cbind(hrData[,-colIndexWithMoreThan2Levels], dummies)


str(hrData) ##4410 obs. of  117 variables

################################Scaling If needed############

#scaling the numerical variables
ind<-c(2,4,7,8,10,c(12:18),21)

for(i in ind)
{
  hrData[,i]<-scale(x=hrData[,i],center = TRUE,scale = TRUE)
}



########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(hrData$EmployeeID, SplitRatio = 0.7)

train = hrData[indices,c(3,2,c(4:61))]

test = hrData[!(indices),c(3,2,c(4:61))]

########################################################################

##model Building###################################################
#

model1<-glm(Attrition~.,data=train,family = 'binomial')
summary(model1)

model2<-stepAIC(model1, direction="both")
summary(model2)

sort(vif(model2),decreasing = TRUE)
#removing YearsAtCompany (High vif and Insignificant)


model3<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear +  YearsSinceLastPromotion + 
              YearsWithCurrManager + over_time + EnvironmentSatisfaction.x2 + 
              EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
              JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
              WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
              BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
              Department.xResearch...Development + Department.xSales + 
              EducationField.xLife.Sciences + Education.x2 + JobRole.xManufacturing.Director + 
              JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
              MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
              JobLevel.x2 + JobLevel.x5, family = "binomial", data = train)

summary(model3)

sort(vif(model3),decreasing = TRUE)

#Removing EducationField.xLife.Sciences (Insignificant)

model4<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
              over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
              EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
              JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
              WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
              BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
              Department.xSales + Education.x2 + 
              JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
              JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
              JobInvolvement.x2 + JobInvolvement.x3 + JobLevel.x2 + JobLevel.x5, 
            family = "binomial", data = train)


summary(model4)

sort(vif(model4),decreasing = TRUE)
#Removing MaritalStatus.xMarried (Insignificant)

model5<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
              over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
              EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
              JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
              WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
              BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
              Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
              JobRole.xResearch.Director + JobRole.xSales.Executive + 
              MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
              JobLevel.x2 + JobLevel.x5, family = "binomial", data = train)


summary(model5)

sort(vif(model5),decreasing = TRUE)
#Removing JobRole.xSales.Executive (Insignificant) 

model6<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
              over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
              EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
              JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
              WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
              BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
              Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
              JobRole.xResearch.Director + 
              MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
              JobLevel.x2 + JobLevel.x5, family = "binomial", data = train)


summary(model6)

sort(vif(model6),decreasing = TRUE)

#Removing JobInvolvement.x2 (Insignificant)

#Going forward looks like no effect of VIF, let's concentrate on insignificant candidates.  


model7<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
              over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
              EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
              JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
              WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
              BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
              Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
              JobRole.xResearch.Director + 
              MaritalStatus.xSingle + JobInvolvement.x3 + 
              JobLevel.x2 + JobLevel.x5, family = "binomial", data = train)


summary(model7)


#Removing JobLevel.x5 (Insignificant)

model8<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
              over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
              EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
              JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
              WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
              BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
              Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
              JobRole.xResearch.Director + 
              MaritalStatus.xSingle + JobInvolvement.x3 + 
              JobLevel.x2, family = "binomial", data = train)


summary(model8)


#Removing Education.x2 (Insignificant)

model9<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
              over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
              EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
              JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
              WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
              BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
              Department.xSales + JobRole.xManufacturing.Director + 
              JobRole.xResearch.Director + 
              MaritalStatus.xSingle + JobInvolvement.x3 + 
              JobLevel.x2, family = "binomial", data = train)


summary(model9) 

#Removing JobLevel.x2 (Insignificant)  

model10<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + 
               MaritalStatus.xSingle + JobInvolvement.x3, family = "binomial", data = train)


summary(model10) 

#Removing JobRole.xResearch.Director  (Insignificant)

model11<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + JobRole.xManufacturing.Director + MaritalStatus.xSingle + JobInvolvement.x3, family = "binomial", data = train)


summary(model11) 

#Removing JobRole.xResearch.Director  (Insignificant)

model12<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + JobRole.xManufacturing.Director + MaritalStatus.xSingle + JobInvolvement.x3, family = "binomial", data = train)


summary(model12) 

#Removing Department.xResearch...Development  (Insignificant)

model13<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + JobRole.xManufacturing.Director + MaritalStatus.xSingle + JobInvolvement.x3, family = "binomial", data = train)


summary(model13) 

#Removing JobInvolvement.x3  (Insignificant)

model14<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + JobRole.xManufacturing.Director + MaritalStatus.xSingle, family = "binomial", data = train)


summary(model14) 

#Removing Age  (Insignificant)

model15<-glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + JobRole.xManufacturing.Director + MaritalStatus.xSingle, family = "binomial", data = train)


summary(model15) 

#Removing TrainingTimesLastYear  (Insignificant)

model16<-glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + JobRole.xManufacturing.Director + MaritalStatus.xSingle, family = "binomial", data = train)


summary(model16) 

#Removing JobRole.xManufacturing.Director  (Insignificant)

model17<-glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + MaritalStatus.xSingle, family = "binomial", data = train)


summary(model17) 

#Removing BusinessTravel.xTravel_Rarely (Insignificant)

model18<-glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               MaritalStatus.xSingle, family = "binomial", data = train)


summary(model18) 



final_model<-model18

#### CRISP-DM Framework - Stage 5 - Model Evaluation ---- #


# Model evaluation done on TEST data.

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])


summary(test_pred)

testActualAttrition<-factor(ifelse(test$Attrition==1,"Yes","No"))

#Let's test the "sensitivity", "specificity", "accuracy"
#Cutoff>=0.5
testPredictAttrition<-factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_conf <- caret::confusionMatrix(testPredictAttrition, testActualAttrition, positive = "Yes")
test_conf

#Accuracy : 0.8488
#Sensitivity : 0.25764
#Specificity : 0.97258

#Observation: Accuracy is very good but Sensitivity is very low
#Reference
#Prediction   No  Yes
#No  1064  170
#Yes   30   59

#Let's Find out Optimal cutoff point.
#We will use R package ROCR for this
# Reference https://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/


p <- predict(final_model, newdata=test[,-1], type="response")
pr <- prediction(p, test$Attrition)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(prf, pr))

#sensitivity 0.7554585
#specificity 0.7751371
#cutoff      0.1699306

#Let's use cutoff @0.16 


#Cutoff>=0.16
testPredictAttrition<-factor(ifelse(test_pred >= .16, "Yes", "No"))
test_conf <- caret::confusionMatrix(testPredictAttrition, testActualAttrition, positive = "Yes")

acc <-  test_conf$overall[1]
sens <- test_conf$byClass[1]
spec <- test_conf$byClass[2]

acc
sens
spec

#Accuracy    : 0.7657
#Sensitivity : 0.7555
#Specificity : 0.7678

#Since accuracy, sensitivity and specificity are almost same, so this a pretty good model.


##################################################################################################
### KS -statistic - Test Data ######

p <- predict(final_model, newdata=test[,-1], type="response")
pr <- prediction(p, test$Attrition)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

ks_table_test <- attr(prf, "y.values")[[1]] - (attr(prf, "x.values")[[1]])

max(ks_table_test)

quantile(ks_table_test, prob = seq(0, 1, length = 11), type = 5)


#0.5305956, Since KSS 53% ( more or equal to 40%)..  Our model is a good model.


####################################################################
# Lift & Gain  


testPredictAttrition <- ifelse(testPredictAttrition=="Yes",1,0)
testActualAttrition <- ifelse(testActualAttrition=="Yes",1,0)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(testActualAttrition, testPredictAttrition, groups = 10)



#Plot The Lift chart
plot(Attrition_decile$Cumlift, type="l", lwd=2, col="red",# lty=4,
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")
abline(h=1, col="brown")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)



#Plot the Gain Chart
ks_plot(testActualAttrition, testPredictAttrition) # Gain chart plot

#Interpreatation from the chart
# from the above chart for instance, by targeting first 40% of the population,
#the model will be able to capture 72.49% of total responders(Ones),
#while without the model, we can expect to capture only 40% of responders by random readings
#Hence Our model is is fitted and considered as a GOOD Model.






#bucket total totalresp Cumresp Gain  Cumlift
# 1    133        53     53    23.1    2.31
# 2    132        59     112   48.9    2.45
# 3    132        54     166   72.5    2.42
# 4    133        14     180   78.6    1.97
# 5    132         8     188   82.1    1.64
# 6    132         6     194   84.7    1.41
# 7    133        10     204   89.1    1.27
# 8    132         6     210   91.7    1.15
# 9    132         7     217   94.8    1.05
# 10   132        12     229   100      1   

#From the above data we can see model is close to the perfect model and gain is also as desired.



#############################################################################################################

##Final Conclusion
#
#EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance
#The Better above three are for employee the less their chance of attrition

#The more an employee do overtime the more are the chances for attrition. 

#The Longer Employee works with same manager chances are less for Attrition.

#More Experienced people less Attrtion.

#Unmaried Employee contribute to attrtion.

#Job Hoppers are more prone to leave the organization.













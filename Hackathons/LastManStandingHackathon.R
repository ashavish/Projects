# Set working directory

if ( getwd() != "C:\\Asha\\WorkRelated\\Analytics\\AV Hackathons\\Last Man Standing") 
{
  setwd("C:\\Asha\\WorkRelated\\Analytics\\AV Hackathons\\Last Man Standing")
}

# Load File

if(file.exists("C:\\Asha\\WorkRelated\\Analytics\\AV Hackathons\\Last Man Standing\\Train_File.csv"))
{
  traindata<- read.csv("C:\\Asha\\WorkRelated\\Analytics\\AV Hackathons\\Last Man Standing\\Train_File.csv",
                       header=TRUE,
                       col.names=c("ID","Estimated_Insects_Count","Crop_Type","Soil_Type","Pesticide_Use_Category","Number_Doses_Week",
                                   "Number_Weeks_Used","Number_Weeks_Quit","Season","Crop_Damage"),
                       colClasses=c("character","integer",rep("factor",3),rep("integer",3),rep("factor",2)))
}

# Data Exploration ------------------------

str(traindata)
summary(traindata)


hist(traindata$Estimated_Insects_Count,  right=FALSE, col=c("red"), xlab="Estimated Insects Count")

# Has a positive skew , sqrt made the curve more gaussian as compared to log

croptypetbl <- table(traindata$Crop_Type)
croptypetbl
prop.table(croptypetbl)

# Crop Type : 71.5 % - 0, 28.4 % - 1

soiltypetbl <- table(traindata$Soil_Type)
soiltypetbl
prop.table(soiltypetbl)

# Soil Type : 54 % - 0, 45.8 % - 1

pesticideusetbd <- table(traindata$Pesticide_Use_Category)
pesticideusetbd
prop.table(pesticideusetbd)

# Pesticide Used : 1 (Never) - 0.009 %, 2 (Previously Used) - 71.7 %, 3(Currently Using) - 27.3%

hist(traindata$Number_Doses_Week,  right=FALSE, col=c("red"), xlab="Number of Doses Per Week")

# Doses per week - Evidence of skewness or outliers

hist(traindata$Number_Weeks_Used,  right=FALSE, col=c("red"), xlab="Number of Weeks Used")

# Number of weeks used seems to follow a gaussian curve

hist(traindata$Number_Weeks_Quit,  right=FALSE, col=c("red"), xlab="Number of Weeks Quit")

# Number_Weeks_Quit - Majority at 0.Some tapering off till about 45 weeks

seasontbl <- table(traindata$Season)
seasontbl
prop.table(seasontbl)

# Season : 1- 30%, 2- 49.7%, 3 - 19.9 %

cropdamagetbl <- table(traindata$Crop_Damage)
cropdamagetbl
prop.table(cropdamagetbl)

# Crop Damage : 0 (Alive) - 83.5%, 1 (Damage due to other causes) - 13.8%, 2 (Damage due to pesticides) - 0.026% (Oversampling might be needed)



# Bivariate analysis


plot(traindata$Estimated_Insects_Count ~ traindata$Number_Doses_Week, data=traindata,pch=19, col="red") 
# As the number of doses per week goes beyond 60, the estimated insects count reduces

plot(traindata$Estimated_Insects_Count ~ traindata$Number_Weeks_Used, data=traindata,pch=19, col="red") 
# As the number of weeks used increases, the min estimated insects count seems to increase !!

hist(traindata[(traindata$Crop_Damage ==0),]$Number_Weeks_Used)
boxplot(traindata[(traindata$Crop_Damage ==0),]$Number_Weeks_Used~traindata[(traindata$Crop_Damage ==0),]$Crop_Type) 

plot(traindata[(traindata$Crop_Damage ==0),]$Number_Weeks_Used,data=traindata,pch=19, col=traindata$Crop_Type)

# Checking correlations

cor(traindata$Number_Doses_Week,traindata$Estimated_Insects_Count)
cor(traindata$Number_Weeks_Used,traindata$Estimated_Insects_Count)
cor(traindata$Number_Weeks_Quit ,traindata$Estimated_Insects_Count)
cor(traindata$Number_Weeks_Used ,traindata$Number_Doses_Week)

aov1 <- aov(traindata$Estimated_Insects_Count ~ traindata$Crop_Damage) 
summary(aov1)
aov2 <- aov(traindata$Estimated_Insects_Count ~ traindata$Crop_Type) 
summary(aov2)
aov3 <- aov(traindata$Estimated_Insects_Count ~ traindata$Soil_Type) 
summary(aov3)
aov4 <- aov(traindata$Estimated_Insects_Count ~ traindata$Season) 
summary(aov4)
aov5 <- aov(traindata$Estimated_Insects_Count ~ traindata$Pesticide_Use_Category) 
summary(aov5)


# Missing values Treatment ----------------

nrow(traindata[is.na(traindata$Number_Weeks_Used),]) / nrow(traindata$traindata)
# 10 % of values are missing
summary(traindata[is.na(traindata$Number_Weeks_Used),])

# Feature Engineering

# Remove records where Pesticides were never used but the reason of Crop Damage is 2
traindata <- traindata[!(traindata$Pesticide_Use_Category == 1 & traindata$Crop_Damage == 2),]  

featureengg <- function(df){

  # Since the overall data seems to be missing at random and there's no marked difference, we will use mean imputation
  
  df[df$Pesticide_Use_Category==1,]$Number_Weeks_Used[is.na(df[df$Pesticide_Use_Category==1,]$Number_Weeks_Used)]<- 0
  df[df$Pesticide_Use_Category!=1,]$Number_Weeks_Used[is.na(df[df$Pesticide_Use_Category!=1,]$Number_Weeks_Used)] <- mean(df[df$Pesticide_Use_Category!=1,]$Number_Weeks_Used, na.rm=TRUE) 
  


  df <- cbind(df,Total_Doses = df$Number_Weeks_Used * df$Number_Doses_Week)
  df <- cbind(df,Number_Doses_Week_XT = sqrt(df$Number_Doses_Week))
  df <- cbind(df,Estimated_Insects_Count_XT = sqrt(df$Estimated_Insects_Count))
  
  df$Season1Dummy <- as.numeric(df$Season == 1)
  df$Season2Dummy <- as.numeric(df$Season == 2)
  df$Season3Dummy <- as.numeric(df$Season == 3)
  
  df$CropType0Dummy <- as.numeric(df$Crop_Type == 0)
  df$CropType1Dummy <- as.numeric(df$Crop_Type == 1)
  
  df$SoilType0Dummy <- as.numeric(df$Soil_Type == 0)
  df$SoilType1Dummy <- as.numeric(df$Soil_Type == 1)
  
  df$PesticideUseCat1Dummy <- as.numeric(df$Pesticide_Use_Category == 1)
  df$PesticideUseCat2Dummy <- as.numeric(df$Pesticide_Use_Category == 2)
  df$PesticideUseCat3Dummy <- as.numeric(df$Pesticide_Use_Category == 3)
  
  
  df <- cbind(df,Dosage_Bucket_XT = cut(df$Total_Doses,4,include.lowest = TRUE,labels = c("low","medium","high","v high")))
  
  df$Overuse_XT <- as.factor(ifelse(df$Total_Doses > 2500,1,0))
  
  df$Optimum_XT <- ifelse(df$Crop_Type == 0 & df$Soil_Type==0,720,ifelse(df$Crop_Type == 1 & df$Soil_Type==0,480,ifelse(df$Crop_Type == 0 & df$Soil_Type==1,640,ifelse
                                                                                                                        (df$Crop_Type == 1 & df$Soil_Type==1,572,1))))
  
  df$Deviance_XT <- df$Total_Dose - df$Optimum_XT
  df$Last3MonthsExp_XT <- sqrt(ifelse(df$Number_Weeks_Quit < 12, ifelse(df$Number_Weeks_Used + df$Number_Weeks_Quit <= 12 ,df$Number_Weeks_Used , 12 - df
                                                                        $Number_Weeks_Quit ),0) * df$Number_Doses_Week)
  
  df$Last6MonthsExp_XT <- sqrt(ifelse(df$Number_Weeks_Quit < 24, ifelse(df$Number_Weeks_Used + df$Number_Weeks_Quit <= 24 ,df$Number_Weeks_Used , 24 - df
                                                                        $Number_Weeks_Quit ),0) * df$Number_Doses_Week)
  
  df$Last9MonthsExp_XT <- sqrt(ifelse(df$Number_Weeks_Quit < 36, ifelse(df$Number_Weeks_Used + df$Number_Weeks_Quit <= 36 ,df$Number_Weeks_Used , 36 - df
                                                                        $Number_Weeks_Quit ),0) * df$Number_Doses_Week)
  
  df$Last12MonthsExp_XT <- sqrt(ifelse(df$Number_Weeks_Quit < 48, ifelse(df$Number_Weeks_Used + df$Number_Weeks_Quit <= 48 ,df$Number_Weeks_Used , 48 - df
                                                                         $Number_Weeks_Quit ),0) * df$Number_Doses_Week)
  
  return(df)
  
}

traindata <- featureengg(traindata)
# Some more data exploration 

boxplot(traindata$Total_Doses~traindata$Crop_Damage,data=traindata)
plot(traindata$Estimated_Insects_Count ~ traindata$Total_Doses, data=traindata,pch=19, col=ifelse(traindata$Crop_Type==0, "red", "blue")) 

#plot(traindata[Soil_Type==0 & Crop_Type == 0,]$Estimated_Insects_Count ~ traindata[Soil_Type==0 & Crop_Type == 0,]$Total_Doses, data=traindata,pch=19) 

aovcroptype <- aov(traindata$Total_Doses ~ traindata$Crop_Type) 
summary(aovcroptype)
aovsoiltype <- aov(traindata$Total_Doses ~ traindata$Soil_Type) 
summary(aovsoiltype)
aovseason <- aov(traindata$Total_Doses ~ traindata$Season) 
summary(aovseason)

aggregate(traindata[, c("Total_Doses")], list(traindata$Crop_Type,traindata$Soil_Type), median)

plot(traindata[Soil_Type==0,]$Estimated_Insects_Count ~ traindata[Soil_Type==0,]$Total_Doses, data=traindata,pch=19, col=Crop_Type) 

plot(traindata$Estimated_Insects_Count ~ traindata$Total_Doses, data=traindata,pch=19, col=Crop_Damage) 

aov6 <- aov(traindata$Total_Doses ~ traindata$Crop_Damage) 
summary(aov6)


# Model building -------------------------------

# Test and training splits

set.seed(999)      

ind <- sample(2, nrow(traindata), replace=T, prob=c(0.60,0.40))   
trainsample<-traindata[ind==1,] 
testsample <- traindata[ind==2,]


# Using Random Forest  - 4th iteration With Oversampling

library(randomForest)
myNtree <- 500
myMtry <- 5
myImportance <- TRUE

rfmodel <- with(trainsample,randomForest(Crop_Damage ~ Estimated_Insects_Count_XT + CropType0Dummy + CropType1Dummy + SoilType0Dummy + SoilType1Dummy + 
                                           PesticideUseCat1Dummy + PesticideUseCat2Dummy + PesticideUseCat3Dummy + Number_Doses_Week_XT + Number_Weeks_Used + Number_Weeks_Quit + Season1Dummy + Season2Dummy + 
                                           Season3Dummy + Dosage_Bucket_XT + Overuse_XT + Deviance_XT + Last3MonthsExp_XT + Last6MonthsExp_XT + Last9MonthsExp_XT + Last12MonthsExp_XT, data = 
                                           trainsample,ntree=myNtree,mtry=myMtry,importance = myImportance))
varImpPlot(rfmodel)

summary(rfmodel)
rfmodelpruned <- with(trainsample,randomForest(Crop_Damage ~ Estimated_Insects_Count_XT + CropType0Dummy + CropType1Dummy + SoilType0Dummy + SoilType1Dummy + 
                                           PesticideUseCat1Dummy + PesticideUseCat2Dummy + PesticideUseCat3Dummy + Season1Dummy + Season2Dummy + 
                                           Season3Dummy + Dosage_Bucket_XT + Overuse_XT + Last9MonthsExp_XT , data = 
                                           trainsample,ntree=myNtree,mtry=myMtry,importance = myImportance))

rfmodelpruned2 <- with(trainsample,randomForest(Crop_Damage ~ Estimated_Insects_Count_XT + CropType1Dummy +  
                                                 PesticideUseCat1Dummy + PesticideUseCat2Dummy + PesticideUseCat3Dummy + Season2Dummy + 
                                                  Dosage_Bucket_XT +  Last9MonthsExp_XT , data = 
                                                 trainsample,ntree=myNtree,mtry=myMtry,importance = myImportance))



#fitrf <- predict(rfmodel, trainsample)
fitrf <- predict(rfmodelpruned, trainsample)
trainsample <- cbind(trainsample,"rf_pred"= fitrf )
table3 <- table(trainsample$Crop_Damage,trainsample$rf_pred)
table3



# Checking on test

fitrftest <- predict(rfmodelpruned,newdata=testsample)
#fitrftest <- predict(rfmodelpruned2,newdata=testsample) #84.2
testsample <- cbind(testsample,"rf_pred"= fitrftest )
tablerf = table(testsample$Crop_Damage,testsample$rf_pred)
tablerf




# Load final test file

if(file.exists("C:\\Asha\\WorkRelated\\Analytics\\AV Hackathons\\Last Man Standing\\Test_File.csv"))
{
  testdata<- read.csv("C:\\Asha\\WorkRelated\\Analytics\\AV Hackathons\\Last Man Standing\\Test_File.csv",
                      header=TRUE,
                      col.names=c("ID","Estimated_Insects_Count","Crop_Type","Soil_Type","Pesticide_Use_Category","Number_Doses_Week",
                                  "Number_Weeks_Used","Number_Weeks_Quit","Season"),
                      colClasses=c("character","integer",rep("factor",3),rep("integer",3),"factor"))
}


# Data Preperation

testdata <- featureengg(testdata)

fitrftest <- predict(rfmodelpruned,newdata=testdata)
testdata <- cbind(testdata,"Crop_Damage"= fitrftest )
summary(testdata$Crop_Damage)

# Export data

write.csv(testdata[,c("ID","Crop_Damage")],file="TestPredictionRF5_ashavish.csv")

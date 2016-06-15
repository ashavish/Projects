## Code for Loan Practice Problem using an ensemble of Random Forest & XGBoost

#Load Library

library(xgboost)
library(caret)
library(plyr)
library(ROCR)
library(randomForest)

# Path
path <- "C:\\Asha\\WorkRelated\\Analytics\\AV Hackathons\\Loan Prediction - Practice Problem\\Second Time"
setwd(path)

#Load initial data
train <- read.csv("./train.csv")
test <- read.csv("./test.csv")

X_train <- train
X_test <- test
X_comb <- rbind.fill(X_train, X_test)

# Impute Missing & Feature Engg
X_comb[is.na(X_comb$LoanAmount),]$LoanAmount <- mean(X_comb[!is.na(X_comb$LoanAmount),]$LoanAmount)
X_comb[is.na(X_comb$Credit_History),]$Credit_History <- 1
X_comb[is.na(X_comb$Loan_Amount_Term),]$Loan_Amount_Term <- 360
X_comb[X_comb$ApplicantIncome==0,]$ApplicantIncome <- mean(X_comb[X_comb$ApplicantIncome!=0,]$ApplicantIncome)

X_comb$LAJIRatio_XT <- log(X_comb$LoanAmount)/log(X_comb$ApplicantIncome+X_comb$CoapplicantIncome)
X_comb$LogEMIJI_XT <- log(((X_comb$LoanAmount*1000*10/1200)*(1+0.1/12)^(X_comb$Loan_Amount_Term/12) ) / ((1+0.1/12)^(X_comb$Loan_Amount_Term/12) - 1 ))/log(X_comb$ApplicantIncome+X_comb$CoapplicantIncome)
X_comb$Coapplicant_XT <- as.factor(ifelse(X_comb$CoapplicantIncome==0,"N","Y"))
X_comb$Priority_Area_XT <- as.factor(ifelse(X_comb$Property_Area=="Semiurban","Y","N"))

# Preparing Data for XGB Model

cat_features <- c("Gender","Married","Dependents","Education","Self_Employed","Property_Area","Loan_Amount_Term","Coapplicant_XT","Priority_Area_XT")

set.seed(100)

dummies <- dummyVars( ~ Gender + Married + Dependents + Education + Self_Employed + Property_Area + Loan_Amount_Term + Coapplicant_XT + Priority_Area_XT, data = X_comb)
X_comb_ohe <- as.data.frame(predict(dummies, newdata = X_comb))
X_comb_combined <- cbind(X_comb[,-c(which(colnames(X_comb) %in% cat_features))],X_comb_ohe)

X_train <- X_comb_combined[!is.na(X_comb_combined$Loan_Status),]
X_test <- X_comb_combined[is.na(X_comb_combined$Loan_Status),]
X_test$Loan_Status <- NULL
X_train$Loan_Status <- as.character(X_train$Loan_Status)
response <- as.numeric(ifelse(X_train$Loan_Status=="Y",1,0))

xgb_train = X_train[-grep('Loan_Status', colnames(X_train))]
xgb_train <- xgb_train[,-1]

# Prepating Data for Random Forest

X_rftrain <- X_comb[!is.na(X_comb$Loan_Status),]
X_rftest <- X_comb[is.na(X_comb$Loan_Status),]
X_rftest$Loan_Status <- NULL
rftrain <- X_rftrain[-1]

# Running the XGB model
param <- list("eta" = 0.01,
              "gamma" = 0,
              "max_depth" = 5, 
              "subsample" = 0.5,
              "colsample_bytree" = 0.4,
              "eval_metric" = "merror",
              "objective" = "multi:softmax",
              "num_class" = 2
)
set.seed(100)
cv.nround <- 500

bst.cv <- xgb.cv(
  param=param,
  data = data.matrix(xgb_train),
  label = response,
  nfold = 5,
  nrounds=cv.nround,
  prediction=T)

# Find the nround where min log loss occured on test data with 5 fold Cross Validation

min.loss.idx = which.min(bst.cv$dt[, test.merror.mean])
min.loss.idx
min(bst.cv$dt[, test.merror.mean]) 
bst.cv$dt[min.loss.idx,train.merror.mean]


xgb <- xgboost(data = data.matrix(xgb_train), 
               label = response, 
               param=param,
               nrounds=min.loss.idx 
)

#model <- xgb.dump(xgb, with.stats = T)
#model[1:10]
names <- colnames(xgb_train)
importance_matrix <- xgb.importance(names, model = xgb)
barplot(importance_matrix$Gain)
importance_matrix[order(importance_matrix$Gain,decreasing=TRUE),]
imp_feat <- head(importance_matrix[order(importance_matrix$Gain,decreasing=TRUE),]$Feature,11)

xgb <- xgboost(data = data.matrix(xgb_train), 
               label = response, 
               param=param,
               nrounds=min.loss.idx 
) 
 
X_test$Loan_Status_xgb <- predict(xgb,as.matrix(X_test[,-1]))
X_train$Loan_Status_xgbpred <- predict(xgb,as.matrix(xgb_train))


# Random Forest Model

rfmodel <- randomForest(Loan_Status ~ .,rftrain,ntree=500,nodesize=1)
rfmodel$err.rate
which.min(rfmodel$err.rate[,"OOB"])


# Finding the best mtry
mtry <- tuneRF(rftrain[-12],rftrain$Loan_Status,ntreeTry = 150,stepFactor = 1.5,improve=0.01,trace=TRUE,plot=TRUE)
print(mtry)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(best.m)

# Running the random forest model
rfmodel <- randomForest(Loan_Status ~ .,rftrain,mtry=2,ntree=150,nodesize=1)
important <- importance(rfmodel)
important <- important[order(important,decreasing = TRUE),]
imp_rf_feat <- head(names(important),10)

# Checking total predictors with Cross Validation and CV plot
set.seed(100)
rfcvmodel <- rfcv(rftrain[,-12],rftrain[,c("Loan_Status")],cv.fold=5,scale="log",step=0.5)
rfcvmodel$error.cv
with(rfcvmodel, plot(n.var, error.cv, log="x", type="o", lwd=2,
                     xlab="Number of Variables", ylab="Error Rate"))

# OOB Error - 19.06%

## Predicting & Combining results from RF and XGB

X_test$Loan_Status_rf <- predict(rfmodel,X_rftest[,-1],type = "response")
X_train$Loan_Status_rfpred <- predict(rfmodel,X_rftrain[,-1],type = "response")

X_test$Loan_Status_rf <- ifelse(as.character(X_test$Loan_Status_rf)=="Y",1,0)
X_train$Loan_Status_rfpred <- ifelse(as.character(X_train$Loan_Status_rfpred)=="Y",1,0)

X_test$Loan_Status <- X_test$Loan_Status_rf + X_test$Loan_Status_xgb
X_train$Loan_Status_pred <- X_train$Loan_Status_rfpred + X_train$Loan_Status_xgbpred

X_test$Loan_Status<- ifelse(as.character(X_test$Loan_Status)<= 1,"N","Y")
X_train$Loan_Status_pred<- ifelse(as.character(X_train$Loan_Status_pred)<= 1,"N","Y")

# Creating the submission file
submit <- X_test[,c("Loan_ID","Loan_Status")]
write.csv(submit,"xgb_rf_allfe_prediction.csv",row.names=FALSE)

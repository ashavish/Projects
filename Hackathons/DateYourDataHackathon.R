## setting working directory
path <- "C:\\Asha\\WorkRelated\\Analytics\\AV Hackathons\\Date Your Data"
setwd(path)


## loading libraries
library(plyr)
library(randomForest)
library(ROCR)

## loading data
train <- read.csv("./train.csv",stringsAsFactors=FALSE)
test <- read.csv("./test.csv",stringsAsFactors=FALSE)
internship <- read.csv("./Internship.csv",stringsAsFactors=FALSE)
student <- read.csv("./Student.csv",stringsAsFactors=FALSE)
student_x <- student[,1:14]
student_unique <- unique(student_x)

X_train <- merge(train,internship,by= "Internship_ID")
X_train <- merge(X_train,student_unique,by= "Student_ID")

X_test <- merge(test,internship,by= "Internship_ID")
X_test <- merge(X_test,student_unique,by= "Student_ID")

X_comb <- rbind.fill(X_train,X_test)

# Date correction Function
correctDate <- function(y) {
  HyphenDates <- grep("-",y)
  SlashDates <- grep("/",y)
  X_comb[HyphenDates,"Day"] <- (as.vector(substr(y,1,regexpr("-",y)-1)))
  X_comb[HyphenDates,"Day"] <- ifelse(nchar(X_comb$Day)<2,paste("0",X_comb$Day,sep=""),X_comb$Day)
  
  X_comb[SlashDates,"Day"] <- (as.vector(substr(y,1,regexpr("/",y)-1)))
  X_comb[SlashDates,"Day"] <- ifelse(nchar(X_comb$Day)<2,paste("0",X_comb$Day,sep=""),X_comb$Day)
  
  X_comb[HyphenDates,"Month"] <- (as.vector(substr(y,regexpr("-",y)+1,regexpr("-([^-]*)$",y)-1)))
  X_comb[SlashDates,"Month"] <- (as.vector(substr(y,regexpr("/",y)+1,regexpr("/([^/]*)$",y)-1)))
  
  X_comb$Month <- ifelse(X_comb$Month=="Jan",1,X_comb$Month)
  X_comb$Month <- ifelse(X_comb$Month=="Feb",2,X_comb$Month)
  X_comb$Month <- ifelse(X_comb$Month=="Mar",3,X_comb$Month)
  X_comb$Month <- ifelse(X_comb$Month=="Apr",4,X_comb$Month)
  X_comb$Month <- ifelse(X_comb$Month=="May",5,X_comb$Month)
  X_comb$Month <- ifelse(X_comb$Month=="Jun",6,X_comb$Month)
  X_comb$Month <- ifelse(X_comb$Month=="Jul",7,X_comb$Month)
  X_comb$Month <- ifelse(X_comb$Month=="Aug",8,X_comb$Month)
  X_comb$Month <- ifelse(X_comb$Month=="Sep",9,X_comb$Month)
  X_comb$Month <- ifelse(X_comb$Month=="Oct",10,X_comb$Month)
  X_comb$Month <- ifelse(X_comb$Month=="Nov",11,X_comb$Month)
  X_comb$Month <- ifelse(X_comb$Month=="Dec",12,X_comb$Month)
  
  X_comb$Month <- ifelse(nchar(X_comb$Month)<2,paste("0",X_comb$Month,sep=""),X_comb$Month)
  
  X_comb[HyphenDates,"Year"] <- (as.vector(substr(y,regexpr("-([^-]*)$",y)+1,nchar(y))))
  X_comb[SlashDates,"Year"] <- (as.vector(substr(y,regexpr("/([^/]*)$",y)+1,nchar(y))))
  
  X_comb$Year <- ifelse(nchar(X_comb$Year)<3,paste("20",X_comb$Year,sep=""),X_comb$Year)
  
  X_comb$CalDateStr <- paste(X_comb$Day,X_comb$Month,X_comb$Year,sep="-")
  y <- as.Date(X_comb$CalDateStr,"%d-%m-%Y")
  X_comb$Day <- NULL
  X_comb$Month <- NULL
  X_comb$Year <- NULL
  X_comb$CalDateStr <- NULL
  return(y)
}

X_comb$Earliest_Start_Date <- correctDate(X_comb$Earliest_Start_Date)
X_comb$Start_Date <- correctDate(X_comb$Start_Date)
X_comb$Internship_deadline <- correctDate(X_comb$Internship_deadline)

X_comb$Internship_Type <- as.factor(X_comb$Internship_Type)
X_comb$Internship_category <- as.factor(X_comb$Internship_category)
X_comb$Stipend_Type <- as.factor(X_comb$Stipend_Type)

X_comb[X_comb$Stipend1=="NULL","Stipend1"]<- 0
X_comb$Stipend1 <- as.numeric(X_comb$Stipend1)
X_comb[X_comb$Stipend2=="NULL","Stipend2"] <- 0
X_comb$Stipend2 <- as.numeric(X_comb$Stipend2)
X_comb$Stipend_Bucket_Int <- 0

X_comb[X_comb$Stipend2!=0,]$Stipend_Bucket_Int <- ifelse(X_comb[X_comb$Stipend2!=0,]$Stipend2<=2000,1,ifelse(X_comb[X_comb$Stipend2!=0,]$Stipend2<=5000,2,ifelse(X_comb[X_comb$Stipend2!=0,]$Stipend2<=10000,3,4)))
X_comb[X_comb$Stipend_Bucket_Int==0,]$Stipend_Bucket_Int <- ifelse(X_comb[X_comb$Stipend_Bucket_Int==0,]$Stipend1>=2000,2,ifelse(X_comb[X_comb$Stipend_Bucket_Int==0,]$Stipend2>=5000,3,ifelse(X_comb[X_comb$Stipend_Bucket_Int==0,]$Stipend2>=10000,4,0)))

X_comb$Expected_Stipend_Int <- ifelse(X_comb$Expected_Stipend=="2-5K",1,ifelse(X_comb$Expected_Stipend=="5-10K",2,ifelse(X_comb$Expected_Stipend=="10K+",3,0)))

X_comb$Stipend_Match <- ifelse(X_comb$Expected_Stipend <= X_comb$Stipend_Bucket, 1,0)
X_comb$Is_Shortlisted <- as.factor(X_comb$Is_Shortlisted)


X_comb$Institute_Category <- as.factor(X_comb$Institute_Category)
X_comb$Current_year <- as.factor(X_comb$Current_year)

X_comb$Location_Match <- 0
X_comb[,"Location_Match"] <- ifelse(X_comb$Internship_Location == X_comb$Preferred_location, 2,0)
X_comb[X_comb$Location_Match==0,]$Location_Match <- ifelse(X_comb[X_comb$Location_Match==0,]$Internship_Location ==X_comb[X_comb$Location_Match==0,]$Institute_location,1,0)
X_comb[X_comb$Location_Match==0,]$Location_Match <- ifelse(X_comb[X_comb$Location_Match==0,]$Internship_Location == X_comb[X_comb$Location_Match==0,]$hometown,1,0)
X_comb$Location_Match <- as.factor(X_comb$Location_Match)

X_comb$Time_Match = 0
X_comb$Time_Match <- ifelse(X_comb$Internship_category=="Full Time" & X_comb$Is_Part_Time == 0, 1, 0)
X_comb$Time_Match <- ifelse(X_comb$Internship_category=="Part Time" & X_comb$Is_Part_Time == 1, 1, X_comb$Time_Match)

# Imputing outliers
m <- round(mean(X_comb[X_comb$Internship_Duration.Months<=36 & X_comb$Internship_Duration.Months > 0 ,]$Internship_Duration.Months))
X_comb$Internship_Duration.Months <- ifelse(X_comb$Internship_Duration.Months>36 | X_comb$Internship_Duration.Months == 0 ,m,X_comb$Internship_Duration.Months)

X_comb$Start_Date_Day_Diff <- as.numeric(X_comb$Earliest_Start_Date-X_comb$Start_Date)
X_comb$Starting_Late <- as.factor(ifelse(X_comb$Start_Date_Day_Diff>0,1,0))
X_comb$Student_End_Date <- X_comb$Earliest_Start_Date + (X_comb$Minimum_Duration * 30)
X_comb$Internship_End_Date <- X_comb$Start_Date + (X_comb$Internship_Duration.Months * 30)
X_comb$End_Date_Day_Diff <- as.numeric(X_comb$Student_End_Date -X_comb$Internship_End_Date)
X_comb$Ending_Early <- as.factor(ifelse(X_comb$End_Date_Day_Diff<0,1,0))

X_comb$stdt <- as.Date(ifelse(X_comb$Start_Date_Day_Diff<=0,X_comb$Start_Date,X_comb$Earliest_Start_Date),origin = "1970-01-01")
X_comb$enddt <- as.Date(ifelse(X_comb$End_Date_Day_Diff>=0,X_comb$Internship_End_Date,X_comb$Student_End_Date),origin = "1970-01-01")
X_comb$actualdur <- X_comb$enddt - X_comb$stdt
X_comb$actualdur <- ifelse(X_comb$Internship_End_Date<X_comb$Earliest_Start_Date | X_comb$Student_End_Date<X_comb$Start_Date ,0,X_comb$actualdur)

X_comb$Duration_Match_Pct <- 0
X_comb[X_comb$Internship_Duration.Months!=0,]$Duration_Match_Pct <- X_comb[X_comb$Internship_Duration.Months!=0,]$actualdur /(30* X_comb[X_comb$Internship_Duration.Months!=0,]$Internship_Duration.Months) *100



# Skill sets
X_comb$Internship_SkillSet <- "General"
Marketing <- grep("Marketing",X_comb$Internship_Profile)
X_comb[Marketing,]$Internship_SkillSet <- "Marketing"
Business <- grep("Business|Strategy",X_comb$Internship_Profile)
X_comb[Business,"Internship_SkillSet"] <- ifelse(X_comb[Business,]$Internship_SkillSet=="General","Business",X_comb[Business,]$Internship_SkillSet)
Technology <- grep("Web|Database|App|Android|iOS|Python|Analytics|UI/UX",X_comb$Internship_Profile)
X_comb[Technology,"Internship_SkillSet"] <- ifelse(X_comb[Technology,]$Internship_SkillSet=="General","Technology",X_comb[Technology,]$Internship_SkillSet)
Finance <- grep("Finance|Account|Commerce",X_comb$Internship_Profile)
X_comb[Finance,"Internship_SkillSet"] <- ifelse(X_comb[Finance,]$Internship_SkillSet=="General","Finance",X_comb[Finance,]$Internship_SkillSet)
HR <- grep("HR|Recruitment|Hiring",X_comb$Internship_Profile)
X_comb[HR,"Internship_SkillSet"] <- ifelse(X_comb[HR,]$Internship_SkillSet=="General","HR",X_comb[HR,]$Internship_SkillSet)
Communication <- grep("Communication|Media|PR",X_comb$Internship_Profile)
X_comb[Communication,"Internship_SkillSet"] <- ifelse(X_comb[Communication,]$Internship_SkillSet=="General","Communication",X_comb[Communication,]$Internship_SkillSet)


X_comb$Student_SkillSet <- "General"
Marketing <- grep("Marketing",X_comb$Stream)
X_comb[Marketing,]$Student_SkillSet <- "Marketing"
Marketing <- grep("Marketing",X_comb$Degree)
X_comb[Marketing,]$Student_SkillSet <- "Marketing"

Business <- grep("MBA|Management",X_comb$Stream)
X_comb[Business,]$Student_SkillSet <- ifelse(X_comb[Business,]$Student_SkillSet=="General","Business",X_comb[Business,]$Student_SkillSet)
Business <- grep("MBA|Management",X_comb$Degree)
X_comb[Business,]$Student_SkillSet <- ifelse(X_comb[Business,]$Student_SkillSet=="General","Business",X_comb[Business,]$Student_SkillSet)

Technology <- grep("Information|Technology|Computing|Computers|Electronics|Electrical|Mechanical|Regular|Software",X_comb$Stream)
X_comb[Technology,]$Student_SkillSet <- ifelse(X_comb[Technology,]$Student_SkillSet=="General","Technology",X_comb[Technology,]$Student_SkillSet)

HR <- grep("HR|Human Resources",X_comb$Stream)
X_comb[HR,]$Student_SkillSet <- ifelse(X_comb[HR,]$Student_SkillSet=="General","HR",X_comb[HR,]$Student_SkillSet)

Finance <- grep("Finance|Commerce",X_comb$Stream)
X_comb[Finance,]$Student_SkillSet <- ifelse(X_comb[Finance,]$Student_SkillSet=="General","Finance",X_comb[Finance,]$Student_SkillSet)
Finance <- grep("B.Com|Finance|M.Com",X_comb$Degree)
X_comb[Finance,]$Student_SkillSet <- ifelse(X_comb[Finance,]$Student_SkillSet=="General","Finance",X_comb[Finance,]$Student_SkillSet)

Communication <- grep("Media|Communication",X_comb$Stream)
X_comb[Communication,]$Student_SkillSet <- ifelse(X_comb[Communication,]$Student_SkillSet=="General","Communication",X_comb[Communication,]$Student_SkillSet)
Communication <- grep("Media|Communication",X_comb$Degree)
X_comb[Communication,]$Student_SkillSet <- ifelse(X_comb[Communication,]$Student_SkillSet=="General","Communication",X_comb[Communication,]$Student_SkillSet)

X_comb$SkillMatch = as.factor(ifelse(X_comb$Student_SkillSet==X_comb$Internship_SkillSet,1,0))
X_comb$Student_SkillSet <- as.factor(X_comb$Student_SkillSet)
X_comb$Internship_SkillSet <- as.factor(X_comb$Internship_SkillSet)

X_comb$Professional <- 0
Professional <- grep("B.E|B.Tech|MBA|M.S|M.E|Dual|M.Tech",X_comb$Degree)
X_comb[Professional,]$Professional <- 1
X_comb$Professional <- as.factor(X_comb$Professional)

# Academics
X_comb$Performance_PG <- X_comb$Performance_PG/X_comb$PG_scale
X_comb$Performance_UG <- X_comb$Performance_UG/X_comb$UG_Scale
X_comb$Performance_12th <- ifelse(X_comb$Performance_12th < 10, X_comb$Performance_12th/10,X_comb$Performance_12th/100)
X_comb$Performance_10th <- ifelse(X_comb$Performance_10th < 10, X_comb$Performance_10th/10,X_comb$Performance_10th/100)
X_comb$EducationLevel <- "Not Known"
X_comb[X_comb$EducationLevel=="Not Known",]$EducationLevel <- ifelse(X_comb[X_comb$EducationLevel=="Not Known",]$Performance_PG!=0,"PG","Not Known")
X_comb[X_comb$EducationLevel=="Not Known",]$EducationLevel <- ifelse(X_comb[X_comb$EducationLevel=="Not Known",]$Performance_UG!=0,"UG","Not Known")
X_comb[X_comb$EducationLevel=="Not Known",]$EducationLevel <- ifelse(X_comb[X_comb$EducationLevel=="Not Known",]$Performance_12th!=0,"12TH","Not Known")
X_comb[X_comb$EducationLevel=="Not Known",]$EducationLevel <- ifelse(X_comb[X_comb$EducationLevel=="Not Known",]$Performance_10th!=0,"10TH","Not Known")

X_comb$Academic_Performance <- -1
X_comb$Academic_Performance <- ifelse(X_comb$EducationLevel=="PG",X_comb$Performance_PG,X_comb$Academic_Performance)
X_comb$Academic_Performance <- ifelse(X_comb$EducationLevel=="UG",X_comb$Performance_UG,X_comb$Academic_Performance)
X_comb$Academic_Performance <- ifelse(X_comb$EducationLevel=="12TH",X_comb$Performance_12th,X_comb$Academic_Performance)
X_comb$Academic_Performance <- ifelse(X_comb$EducationLevel=="10TH",X_comb$Performance_10th,X_comb$Academic_Performance)

# Average
#X_comb$Academic_Performance <- ifelse(X_comb$Performance_PG!=0,X_comb$Performance_PG,X_comb$Academic_Performance)
#X_comb$Academic_Performance_dn <- ifelse(X_comb$Performance_PG!=0,1,0)
#X_comb$Academic_Performance <- ifelse(X_comb$Performance_UG!=0,X_comb$Performance_UG + X_comb$Academic_Performance,X_comb$Academic_Performance)
#X_comb$Academic_Performance_dn <- ifelse(X_comb$Performance_PG!=0,X_comb$Academic_Performance_dn + 1,X_comb$Academic_Performance_dn)
#X_comb$Academic_Performance <- ifelse(X_comb$Performance_12th!=0,X_comb$Performance_12th + X_comb$Academic_Performance,X_comb$Academic_Performance)
#X_comb$Academic_Performance_dn <- ifelse(X_comb$Performance_12th!=0,X_comb$Academic_Performance_dn + 1,X_comb$Academic_Performance_dn)
#X_comb$Academic_Performance <- ifelse(X_comb$Performance_10th!=0,X_comb$Performance_10th + X_comb$Academic_Performance,X_comb$Academic_Performance)
#X_comb$Academic_Performance_dn <- ifelse(X_comb$Performance_10th!=0,X_comb$Academic_Performance_dn + 1,X_comb$Academic_Performance_dn)

#X_comb[X_comb$Academic_Performance_dn==0,]$Academic_Performance <- -1
#X_comb[X_comb$Academic_Performance_dn!=0,]$Academic_Performance <- X_comb[X_comb$Academic_Performance_dn!=0,]$Academic_Performance/X_comb[X_comb$Academic_Performance_dn!=0,]$Academic_Performance_dn

X_comb$Academic_Performance_Level <-round(X_comb$Academic_Performance*10)
X_comb$Academic_Performance_Level <-as.factor(ifelse(X_comb$Academic_Performance_Level<3,-1,X_comb$Academic_Performance_Level))

X_comb$EducationLevel <- as.factor(X_comb$EducationLevel)

# Splitting into Train & Test
X_train_inp <- X_comb[!is.na(X_comb$Is_Shortlisted),]
X_test_inp <- X_comb[is.na(X_comb$Is_Shortlisted),]
X_test_inp$Is_Shortlisted <- NULL

set.seed(999)      

ind <- sample(5, nrow(X_train_inp), replace=T, prob=c(0.2,0.2,0.2,0.2,0.2))   



# Clean Up

rm(train)
rm(test)
rm(internship)
rm(student)
rm(student_x)
rm(student_unique)
rm(X_train)
rm(X_test)
rm(Business)
rm(Communication)
rm(Finance)
rm(HR)
rm(Marketing)
rm(Professional)
rm(Technology)

# Random Forest

myNtree <- 500
myMtry <- 3
myImportance <- TRUE
traincols <- c("Internship_Type","Location_Match","Time_Match","Duration_Match_Pct",
               "Start_Date_Day_Diff","Stipend_Type","Stipend_Match","End_Date_Day_Diff",
                "No_of_openings","Institute_Category","Current_year","Internship_SkillSet",
               "Student_SkillSet","Academic_Performance_Level","Is_Shortlisted")

testcols <- c("Internship_Type","Location_Match","Time_Match","Duration_Match_Pct",
              "Start_Date_Day_Diff","Stipend_Type","Stipend_Match","End_Date_Day_Diff",
               "No_of_openings","Institute_Category","Current_year","Internship_SkillSet",
              "Student_SkillSet","Academic_Performance_Level")

for (i in 1:1) {
trainsample1 <-X_train_inp[ind==i,] 
testsample <- X_train_inp[ind!=i,]
rfinput <- trainsample1[,traincols]
testinput <- testsample[,traincols]
n <- length(names(rfinput))

rfmodel <- randomForest(y=as.factor(rfinput$Is_Shortlisted), x = rfinput[,-n],ntree=myNtree,mtry=myMtry,importance = myImportance,do.trace=T)
fitrf <- predict(rfmodel, testinput[,-n])
testinput <- cbind(testinput,"rf_pred" = fitrf)
table(testinput$Is_Shortlisted,testinput$rf_pred)

fitrf1 <- predict(rfmodel, X_test_inp[,testcols])
rfcol <- paste("rf_pred",i)
X_test_inp <- cbind(X_test_inp,rfcol= as.numeric(fitrf1)-1)
l <- length(names(X_test_inp))
names(X_test_inp)[l] <- paste("rf_pred",i,sep="")

#rm(rfmodel)
rm(fitrf)
rm(fitrf1)
}

importance(rfmodel)
#rf.pr = predict(rfmodel,type="prob",testinput[,-12])[,2]
#rf.pred = prediction(rf.pr, testinput$Is_Shortlisted)
#rf.perf = performance(rf.pred,"tpr","fpr")
#plot(rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
#abline(a=0,b=1,lwd=2,lty=2,col="gray")

# Running it on Test



#X_test_inp <- cbind(X_test_inp,"rf_pred"= X_test_inp$rf_pred1+X_test_inp$rf_pred2+X_test_inp$rf_pred3+X_test_inp$rf_pred4+X_test_inp$rf_pred5)
X_test_inp <- cbind(X_test_inp,"rf_pred"= X_test_inp$rf_pred1)
X_test_inp$Is_Shortlisted <- ifelse(X_test_inp$rf_pred > 0,1,0)



table(X_test_inp$Is_Shortlisted)

write.csv(X_test_inp[,c("Internship_ID","Student_ID","Is_Shortlisted")],file="TestPrediction_R1_26.csv",row.names=FALSE)

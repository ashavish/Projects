
# Read data
all_villages_backup <- all_villages 
target_set <- c("ComputerAccess_pct","MobilePenetration_pct","InternetAccess_pct","Landline_pct")

# Columns to check for correlations
corr_villages <- import_colnames_md[import_colnames_md$Check_Correlations=="Yes" & import_colnames_md$Data %in% c("Households","PCA","DCHB_Village","New_Features_Common","New_Features_Village"),]$Col_Name
corr_towns <- import_colnames_md[import_colnames_md$Check_Correlations=="Yes" & import_colnames_md$Data %in% c("Households","PCA","DCHB_Town","New_Features_Common","New_Features_Town"),]$Col_Name
col_groups <- import_colnames_md[import_colnames_md$Keep_in_dataset=="Yes" ,c("Col_Name","Category")]

all_villages[,corr_villages] <- apply(all_villages[,corr_villages],2,as.numeric)
all_villages[,target_set] <- apply(all_villages[,target_set],2,as.numeric)

# Checking correlations for Villages
KarVillage_All_Corr <- check_cor(corr_villages,target_set,all_villages[all_villages$State_Name_Households=="KARNATAKA",])
APVillage_All_Corr <- check_cor(corr_villages,target_set,all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",])

# Checking correlations for Towns
KarTown_All_Corr <- check_cor(corr_towns,target_set,all_towns[all_towns$State_Name_Households=="KARNATAKA",])
APTown_All_Corr <- check_cor(corr_towns,target_set,all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",])

# Merging all correlations
KarVillage_All_Corr$Location <- "Karnataka Villages"
APVillage_All_Corr$Location <- "AP Villages"
KarTown_All_Corr$Location <- "Karnataka Towns"
APTown_All_Corr$Location <- "AP Towns"


All_Corr <- rbind(KarVillage_All_Corr,APVillage_All_Corr,KarTown_All_Corr,APTown_All_Corr)
All_Corr <- merge(All_Corr,col_groups,by.x="Features",by.y="Col_Name",all.x=TRUE)
All_Corr$ComputerAccessCorMoreThanPoint5 <- ifelse(abs(All_Corr$ComputerAccess_pct)>=0.5,"Y","N")
All_Corr$MobilePenetrationCorMoreThanPoint5 <- ifelse(abs(All_Corr$MobilePenetration_pct)>=0.5,"Y","N")
All_Corr$InternetCorMoreThanPoint5 <- ifelse(abs(All_Corr$InternetAccess_pct)>=0.5,"Y","N")
All_Corr$LandlineCorMoreThanPoint5 <- ifelse(abs(All_Corr$Landline_pct)>=0.5,"Y","N")

write.csv(All_Corr,"All_Correlations.csv",row.names = FALSE)

# Checking Correlation between Digital Inclusion Paramaters

cor(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$ComputerAccess_pct,all_villages[all_villages$State_Name_Households=="KARNATAKA",]$MobilePenetration_pct)
cor(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$ComputerAccess_pct,all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$MobilePenetration_pct)

cor(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$ComputerAccess_pct,all_towns[all_towns$State_Name_Households=="KARNATAKA",]$MobilePenetration_pct)
cor(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$ComputerAccess_pct,all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$MobilePenetration_pct)

cor(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$ComputerAccess_pct,all_villages[all_villages$State_Name_Households=="KARNATAKA",]$Landline_pct)
cor(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$ComputerAccess_pct,all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$Landline_pct)
cor(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$ComputerAccess_pct,all_towns[all_towns$State_Name_Households=="KARNATAKA",]$Landline_pct)
cor(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$ComputerAccess_pct,all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$Landline_pct)

cor(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$InternetAccess_pct,all_villages[all_villages$State_Name_Households=="KARNATAKA",]$Landline_pct)
cor(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$InternetAccess_pct,all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$Landline_pct)
cor(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$InternetAccess_pct,all_towns[all_towns$State_Name_Households=="KARNATAKA",]$Landline_pct)
cor(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$InternetAccess_pct,all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$Landline_pct)

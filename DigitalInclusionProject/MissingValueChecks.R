# Script to check and correct missing values

missing_values_villages <- apply(all_villages,2,function(x) length(which(is.na(x))))
missing_values_villages <- as.data.frame(missing_values_villages)
missing_values_villages$fields <- row.names(missing_values_villages)
row.names(missing_values_villages) <- NULL
missing_values_villages <- missing_values_villages[missing_values_villages$missing_values_villages>0,]
missing_values_villages$percent_missing <- missing_values_villages$missing_values_villages/nrow(all_villages)*100
missing_values_villages[order(missing_values_villages$missing_values_villages,decreasing = TRUE),]
write.csv(missing_values_villages,"MissingValues.csv",row.names = FALSE)

# Missing values present in village data sets

missing_values_towns <- apply(all_towns,2,function(x) length(which(is.na(x))))
missing_values_towns <- missing_values_towns[missing_values_towns>0]

# Observation: No missing values in towns

# Impute mean for Nearest District,Sub-District, Statutary Town, Town distance in Km

mean_district_dist <- aggregate(.~District_Head_Quarter_Name,all_villages[,c("District_Head_Quarter_Name","District_Head_Quarter_Distance_in_km")],mean)
miss_district <- unique(all_villages[is.na(all_villages$District_Head_Quarter_Distance_in_km),]$District_Head_Quarter_Name)
for (i in miss_district) {
  all_villages[is.na(all_villages$District_Head_Quarter_Distance_in_km) & all_villages$District_Head_Quarter_Name == i,]$District_Head_Quarter_Distance_in_km <- round(mean_district_dist[mean_district_dist$District_Head_Quarter_Name==i,]$District_Head_Quarter_Distance_in_km,0)
}

mean_sub_district_dist <- aggregate(.~Sub_District_Head_Quarter_Name,all_villages[,c("Sub_District_Head_Quarter_Name","Sub_District_Head_Quarter_Distance_in_km")],mean)
miss_sub_district <- unique(all_villages[is.na(all_villages$Sub_District_Head_Quarter_Distance_in_km),]$Sub_District_Head_Quarter_Name)
for (i in miss_sub_district) {
  all_villages[is.na(all_villages$Sub_District_Head_Quarter_Distance_in_km) & all_villages$Sub_District_Head_Quarter_Name == i,]$Sub_District_Head_Quarter_Distance_in_km <- round(mean_sub_district_dist[mean_sub_district_dist$Sub_District_Head_Quarter_Name==i,]$Sub_District_Head_Quarter_Distance_in_km,0)
}
all_villages$Nearest_Statutory_Town_Distance_in_km <- gsub("'","",all_villages$Nearest_Statutory_Town_Distance_in_km)
all_villages$Nearest_Statutory_Town_Distance_in_km <- as.numeric(all_villages$Nearest_Statutory_Town_Distance_in_km)

mean_st_town <- aggregate(.~Nearest_Statutory_Town_Name,all_villages[,c("Nearest_Statutory_Town_Name","Nearest_Statutory_Town_Distance_in_km")],mean)
miss_st_town <- unique(all_villages[is.na(all_villages$Nearest_Statutory_Town_Distance_in_km) & !is.na(all_villages$Nearest_Statutory_Town_Name),]$Nearest_Statutory_Town_Name)
for (i in miss_st_town) {
  all_villages[is.na(all_villages$Nearest_Statutory_Town_Distance_in_km) & !is.na(all_villages$Nearest_Statutory_Town_Name)  & all_villages$Nearest_Statutory_Town_Name == i,]$Nearest_Statutory_Town_Distance_in_km <- round(mean_st_town[mean_st_town$Nearest_Statutory_Town_Name==i,]$Nearest_Statutory_Town_Distance_in_km,0)
}
all_villages[is.na(all_villages$Nearest_Statutory_Town_Distance_in_km),]$Nearest_Statutory_Town_Distance_in_km <- mean(all_villages$Nearest_Statutory_Town_Distance_in_km,na.rm = TRUE)


mean_town <- aggregate(.~Nearest_Town_Name,all_villages[,c("Nearest_Town_Name","Nearest_Town_Distance_from_Village_in_Km.")],mean)
miss_town <- unique(all_villages[is.na(all_villages$Nearest_Town_Distance_from_Village_in_Km.) & !is.na(all_villages$Nearest_Town_Name),]$Nearest_Town_Name)
for (i in miss_town) {
  all_villages[is.na(all_villages$Nearest_Town_Distance_from_Village_in_Km.) & !is.na(all_villages$Nearest_Town_Name)  & all_villages$Nearest_Town_Name == i,]$Nearest_Town_Distance_from_Village_in_Km. <- round(mean_town[mean_town$Nearest_Town_Name==i,]$Nearest_Town_Distance_from_Village_in_Km.,0)
}
all_villages[is.na(all_villages$Nearest_Town_Distance_from_Village_in_Km.),]$Nearest_Town_Distance_from_Village_in_Km. <- mean(all_villages$Nearest_Town_Distance_from_Village_in_Km.,na.rm = TRUE)

# Imputing 0 for School/Educational Institutions where not available. Missing is 1 in all below cases

all_villages[is.na(all_villages$Private_Middle_School_Numbers),]$Private_Middle_School_Numbers <- 0
all_villages[is.na(all_villages$Community_Health_Centre_Doctors_Total_Strength_Numbers),]$Community_Health_Centre_Doctors_Total_Strength_Numbers <- 0
all_villages[is.na(all_villages$Community_Health_Centre_Para_Medical_Staff_Total_Strength_Numbers),]$Community_Health_Centre_Para_Medical_Staff_Total_Strength_Numbers <- 0
all_villages[is.na(all_villages$Primary_Health_Centre_Numbers),]$Primary_Health_Centre_Numbers<- 0
all_villages[is.na(all_villages$Primary_Health_Centre_Doctors_Total_Strength_Numbers),]$Primary_Health_Centre_Doctors_Total_Strength_Numbers<- 0
all_villages[is.na(all_villages$Primary_Health_Centre_Para_Medical_Staff_Total_Strength_Numbers),]$Primary_Health_Centre_Para_Medical_Staff_Total_Strength_Numbers <- 0

# Correction of Gender Ratio for Missing Female values
all_villages[all_villages$Gender_Ratio=="Inf",]$Gender_Ratio <- 1

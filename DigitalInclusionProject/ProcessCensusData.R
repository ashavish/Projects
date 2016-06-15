ProcessCensusData <- function(statevar){

# Load merged file
villagefilename <- paste(statevar,"Village.csv",sep="")
townfilename <- paste(statevar,"Town.csv",sep="")
villagedata <- read.csv(villagefilename,header = TRUE)
towndata <- read.csv(townfilename,header = TRUE)

add_features <- function(dataset,statevar){

# Setting Digital Inclusion Parameters in terms of computer access and mobile penetration
dataset$ComputerAccess_pct <- dataset$Availability_of_assets_Computer_Laptop_With_Internet_pct + dataset$Availability_of_assets_Computer_Laptop_Without_Internet_pct
dataset$MobilePenetration_pct <- dataset$Availability_of_assets_Mobile_only_pct + dataset$Availability_of_assets_Both_Landline_Mobile_pct
dataset$ComputerAccess <- round(dataset$ComputerAccess_pct*dataset$No_Households/100)
dataset$MobilePenetration <- round(dataset$MobilePenetration_pct*dataset$No_Households/100)
dataset$InternetAccess <- round(dataset$Availability_of_assets_Computer_Laptop_With_Internet_pct*dataset$No_Households/100)
dataset$InternetAccess_pct <- dataset$Availability_of_assets_Computer_Laptop_With_Internet_pct
dataset$Landline_pct <- dataset$Availability_of_assets_Both_Landline_Mobile_pct + dataset$Availability_of_assets_Landline_only_pct
dataset$Landline <- round(dataset$Landline_pct * dataset$No_Households/100)

# Features for Households & PCA data

dataset$Households_Good_Livable_pct <- as.numeric(dataset$Households_Good_pct + dataset$Households_Livable_pct)
dataset$Total_Residence_Good_Livable_pct <- as.numeric(dataset$Residence_Good_pct + dataset$Residence_Livable_pct + dataset$ResidenceCumOtherUse_Good_pct + dataset$ResidenceCumOtherUse_Livable_pct)
dataset$Roof_Material_Pucca_pct <- as.numeric(dataset$RoofMaterial_Hand_made_Tiles_pct + dataset$RoofMaterial_Machine_made_Tiles_pct + dataset$RoofMaterial_Burnt_Brick_pct + dataset$RoofMaterial_Stone_Slate_pct + dataset$RoofMaterial_Concrete_pct)
dataset$Wall_Material_Pucca_pct <- as.numeric(dataset$WallMaterial_Stone_not_packed_with_mortar_pct + dataset$WallMaterial_Stone_packed_with_mortar_pct + dataset$WallMaterial_Burnt_brick_pct + dataset$WallMaterial_Concrete_pct )
dataset$Floor_Material_Pucca_pct <- as.numeric(dataset$FloorMaterial_Burnt_Brick_pct +   dataset$FloorMaterial_Stone_pct + dataset$FloorMaterial_Cement_pct + dataset$FloorMaterial_Mosaic_Floor_tiles_pct )
dataset$Rooms_Exclusive_Yes_pct <-   as.numeric(dataset$Rooms_One_room_pct +   dataset$Rooms_Two_rooms_pct + dataset$Rooms_Three_rooms_pct +  dataset$Rooms_Four_rooms_pct +   dataset$Rooms_Five_rooms_pct +  dataset$Rooms_Six_rooms_and_above_pct )
dataset$Household_size_4_and_below_pct <- as.numeric(dataset$Household_size_1_pct + dataset$Household_size_2_pct + dataset$Household_size_3_pct + dataset$Household_size_4_pct)
dataset$Married_couple_2_and_below_pct <- as.numeric(dataset$Married_couple_1_pct + dataset$Married_couple_2_pct )
dataset$DrinkingWaterSrc_Available_pct <- as.numeric(dataset$DrinkingWaterSrc_Tapwater_from_treated_source_pct + dataset$DrinkingWaterSrc_Tapwater_from_untreated_source_pct + dataset$DrinkingWaterSrc_Covered_well_pct + dataset$DrinkingWaterSrc_Un_covered_well_pct + dataset$DrinkingWaterSrc_Handpump_pct + dataset$DrinkingWaterSrc_Tubewell_Borehole_pct )
dataset$LightingSrc_NonElectricity_pct <- as.numeric(dataset$LightingSrc_Kerosene_pct +  dataset$LightingSrc_Solar_energy_pct + dataset$LightingSrc_Other_oil_pct +   dataset$LightingSrc_Any_other_pct )
dataset$Bathing_fac_available_pct <- as.numeric(dataset$Bathing_fac_within_premises_Bathroom_pct + dataset$Bathing_fac_within_premises_Enclosure_without_roof_pct) 
dataset$Waster_water_drainage_available_pct <- as.numeric(dataset$Waste_water_outlet_conn_to_Closed_drainage_pct + dataset$Waste_water_outlet_conn_to_Open_drainage_pct )
dataset$Cooking_Fuel_available_pct <- as.numeric(dataset$Cooking_Fuel_Fire_wood_pct +  dataset$Cooking_Fuel_Crop_residue_pct + dataset$Cooking_Fuel_Cowdung_cake_pct + dataset$Cooking_Fuel_Coal_Lignite_Charcoal_pct + dataset$Cooking_Fuel_Kerosene_pct + dataset$Cooking_Fuel_LPG_PNG_pct +  dataset$Cooking_Fuel_Electricity_pct + dataset$Cooking_Fuel_Biogas_pct + dataset$Cooking_Fuel_Any_other_pct)
dataset$Kitchen_facility_available_pct <- as.numeric(dataset$Kitchen_facility_Cooking_inside_house_pct + dataset$Kitchen_facility_Cooking_outside_house_pct )
dataset$Gender_Ratio <- as.numeric(dataset$Total_Male/dataset$Total_Female)
dataset$SC_ST_pct <- as.numeric((dataset$Population_SC + dataset$Population_ST)/dataset$Total_Population*100)
dataset$Population_below_6 <- as.numeric((dataset$Male_06+dataset$Female_06)/dataset$Total_Population*100)
dataset$Literacy_pct <- as.numeric((dataset$Male_Literate + dataset$Female_Literate)/ dataset$Total_Population*100)
dataset$Female_Literacy_pct <- as.numeric(dataset$Female_Literate/ dataset$Total_Population*100)
dataset$Worker_Pop_pct <- as.numeric(dataset$Total_Worker_Population/dataset$Total_Population*100)
dataset$MainWorker_Pop_pct <- as.numeric(dataset$MainWorker_Population/dataset$Total_Population*100)
dataset$Main_Cultivator_Pop_pct <- as.numeric(dataset$Main_Cultivator_Population/dataset$Total_Population*100)
dataset$Main_Agri_Labour_Pop_pct <- as.numeric(dataset$Main_Agri_Labour_Population/dataset$Total_Population*100)
dataset$Main_Households_Pop_pct <- as.numeric(dataset$Main_Households_Population/dataset$Total_Population*100)
dataset$Main_OT_Pop_pct <- as.numeric(dataset$Main_OT_Population/dataset$Total_Population*100)
dataset$MarginalWorker_Pop_pct <- as.numeric(dataset$MarginalWorker_Population/dataset$Total_Population*100)
dataset$Marginal_Cult_Pop_pct <- as.numeric(dataset$Marginal_Cultivator_Population/dataset$Total_Population*100)
dataset$Marginal_Agri_Labour_Pop_pct <- as.numeric(dataset$Marginal_Agri_Labour_Population/dataset$Total_Population*100)
dataset$Marginal_Households_Pop_pct <- as.numeric(dataset$Marginal_Households_Population/dataset$Total_Population*100)
dataset$Non_Worker_Pop_pct <- as.numeric(dataset$Non_Worker_Population/dataset$Total_Population*100)
dataset$Non_Worker_Male_pct <- as.numeric(dataset$Non_Worker_Male/dataset$Total_Population*100)

return(dataset)
}

# Function to add features specific to villages

add_village_features <- function(dataset,statevar,fields){
  
distance_encode <- function(x)
  {
  y <- as.numeric(ifelse(is.na(x),1,ifelse(x=="a",2,ifelse(x=="b",3,ifelse(x=="c",4,1)))))
  return(y)
}  
for (i in fields) {
  print(i)
  j <- sub("Distance_range_code_where","Nearest",i)
  j <- sub("is_available","Distance_Code",j)
  print(j)
  dataset[,c(j)] <- distance_encode(dataset[,c(i)])
}


dataset$Govt_Colleges_Number <- ifelse(is.na(dataset$Govt_Arts_and_Science_Degree_College_Numbers),0,dataset$Govt_Arts_and_Science_Degree_College_Numbers) + ifelse(is.na(dataset$Govt_Engineering_College_Numbers),0,dataset$Govt_Engineering_College_Numbers) + ifelse(is.na(dataset$Govt_Medicine_College_Numbers),0,dataset$Govt_Medicine_College_Numbers)  + ifelse(is.na(dataset$Govt_Management_Institute_Numbers),0,dataset$Govt_Management_Institute_Numbers) + ifelse(is.na(dataset$Govt_Polytechnic_Numbers),0,dataset$Govt_Polytechnic_Numbers)   
dataset$Pvt_Colleges_Number <- ifelse(is.na(dataset$Private_Arts_and_Science_Degree_College_Numbers),0,dataset$Private_Arts_and_Science_Degree_College_Numbers) + ifelse(is.na(dataset$Private_Engineering_College_Numbers),0,dataset$Private_Engineering_College_Numbers) + ifelse(is.na(dataset$Private_Medicine_College_Numbers),0,dataset$Private_Medicine_College_Numbers) + ifelse(is.na(dataset$Private_Management_Institute_Numbers),0,dataset$Private_Management_Institute_Numbers) + ifelse(is.na(dataset$Private_Polytechnic_Numbers),0,dataset$Private_Polytechnic_Numbers) 
dataset$Govt_Training_Institutes_Number <- ifelse(is.na(dataset$Govt_Vocational_Training_School_ITI_Numbers),0,dataset$Govt_Vocational_Training_School_ITI_Numbers) +   ifelse(is.na(dataset$Government_Non_Formal_Training_Centre_Numbers),0,dataset$Government_Non_Formal_Training_Centre_Numbers)
dataset$Pvt_Training_Institutes_Number <- ifelse(is.na(dataset$Private_Vocational_Training_School_ITI_Numbers),0,dataset$Private_Vocational_Training_School_ITI_Numbers) + ifelse(is.na(dataset$Private_Non_Formal_Training_Centre_Numbers),0,dataset$Private_Non_Formal_Training_Centre_Numbers) 

# Calculating nearest college distance from village
dataset$Nearest_College_Distance_Code <- ifelse((dataset$Govt_Colleges_Number + dataset$Pvt_Colleges_Number) >0,1,0)
dataset[dataset$Nearest_College_Distance_Code==0,]$Nearest_College_Distance_Code <- as.numeric(apply(dataset[dataset$Nearest_College_Distance_Code==0,c("Nearest_Arts_and_Science_Degree_College_Distance_Code","Nearest_Engineering_College_Distance_Code","Nearest_Medicine_College_Distance_Code","Nearest_Management_Institute_Distance_Code","Nearest_Polytechnic_Distance_Code")],1,min))


# Calculating nearest training institute distance from village
dataset$Nearest_Training_Instt_Distance_Code <-ifelse((dataset$Govt_Training_Institutes_Number + dataset$Pvt_Training_Institutes_Number) >0,1,0)
dataset[dataset$Nearest_Training_Instt_Distance_Code==0,]$Nearest_Training_Instt_Distance_Code <- apply(dataset[dataset$Nearest_Training_Instt_Distance_Code==0,c("Nearest_Vocational_Training_School_Distance_Code","Nearest_Non_Formal_Training_School_Distance_Code")],1,min)


# Calculating nearest school from village
dataset[is.na(dataset$Private_Middle_School_Numbers),]$Private_Middle_School_Numbers <- 0
dataset$Nearest_School_Distance_Code <-ifelse((dataset$Govt_Pre_Primary_School_Nursery_LKG_UKG_Numbers +
                                                 dataset$Private_Pre_Primary_School_Nursery_LKG_UKG_Numbers +
                                                 dataset$Govt_Primary_School_Numbers + 
                                                 dataset$Private_Primary_School_Numbers + dataset$Govt_Middle_School_Numbers +
                                                 dataset$Private_Middle_School_Numbers + dataset$Govt_Secondary_School_Numbers + 
                                                 dataset$Private_Secondary_School_Numbers + dataset$Govt_Senior_Secondary_School_Numbers +
                                                 dataset$Private_Senior_Secondary_School_Numbers) >0,1,0)


dataset[dataset$Nearest_School_Distance_Code==0,]$Nearest_School_Distance_Code <- apply(dataset[dataset$Nearest_School_Distance_Code==0,c("Nearest_pre_primary_Distance_Code","Nearest_primary_Distance_Code",
                                                                                                                                          "Nearest_middle_Distance_Code","Nearest_secondary_Distance_Code","Nearest_sr_secondary_Distance_Code")],1,min)

dataset$Govt_Schools_A1_NA2 <- ifelse((dataset$Govt_Pre_Primary_School_Nursery_LKG_UKG_Numbers + dataset$Govt_Primary_School_Numbers + 
                                         dataset$Govt_Middle_School_Numbers + dataset$Govt_Secondary_School_Numbers + 
                                         dataset$Govt_Senior_Secondary_School_Numbers) >0,1,2)

dataset$Private_Schools_A1_NA2 <- ifelse((dataset$Private_Pre_Primary_School_Nursery_LKG_UKG_Numbers + dataset$Private_Primary_School_Numbers + 
                                            dataset$Private_Middle_School_Numbers + dataset$Private_Secondary_School_Numbers + 
                                            dataset$Private_Senior_Secondary_School_Numbers) >0,1,2)


dataset$Schools_1_5 <- 0
dataset[dataset$Schools_1_5==0,]$Schools_1_5 <- ifelse((dataset[dataset$Schools_1_5==0,]$Private_Senior_Secondary_School_Numbers + dataset[dataset$Schools_1_5 ==0,]$Govt_Senior_Secondary_School_Numbers)>0,5,dataset[dataset$Schools_1_5==0,]$Schools_1_5)
dataset[dataset$Schools_1_5 ==0,]$Schools_1_5 <- ifelse((dataset[dataset$Schools_1_5 ==0,]$Private_Secondary_School_Numbers + dataset[dataset$Schools_1_5 ==0,]$Govt_Secondary_School_Numbers)>0,4,dataset[dataset$Schools_1_5 ==0,]$Schools_1_5)
dataset[dataset$Schools_1_5 ==0,]$Schools_1_5 <- ifelse((dataset[dataset$Schools_1_5 ==0,]$Private_Middle_School_Numbers + dataset[dataset$Schools_1_5 ==0,]$Govt_Middle_School_Numbers)>0,3,dataset[dataset$Schools_1_5 ==0,]$Schools_1_5)
dataset[dataset$Schools_1_5 ==0,]$Schools_1_5 <- ifelse((dataset[dataset$Schools_1_5 ==0,]$Private_Primary_School_Numbers + dataset[dataset$Schools_1_5 ==0,]$Govt_Primary_School_Numbers )>0,2,dataset[dataset$Schools_1_5 ==0,]$Schools_1_5)
dataset[dataset$Schools_1_5 ==0,]$Schools_1_5 <- ifelse((dataset[dataset$Schools_1_5 ==0,]$Private_Pre_Primary_School_Nursery_LKG_UKG_Numbers + dataset[dataset$Schools_1_5 ==0,]$Govt_Pre_Primary_School_Nursery_LKG_UKG_Numbers)>0,1,dataset[dataset$Schools_1_5 ==0,]$Schools_1_5)

return(dataset)
}

# Function to add town specific features
add_town_features <- function(dataset,statevar){
# To be done as part of Town Processing  
}  

# Creating new features for different data sets
villagedata_p <- add_features(villagedata,statevar)
towndata_p <- add_features(towndata,statevar)

import_colnames_md <- read.xlsx("Import_Metadata.xlsx",sheet="Columns")
fields_distance_encoding <- import_colnames_md[import_colnames_md$Data %in% c("DCHB_Village") & import_colnames_md$Distance_Encode=="Yes",]$Col_Name

villagedata_p <- add_village_features(villagedata_p,statevar,fields_distance_encoding)
# Keeping only required values in Housing Data File
#RemoveFields = c("State_Code","State_Name","District_Code","District_Name","Tehsil_Code","Tehsil_Name","Ward_No")
#HousingData_XT <- HousingData_XT[,!(names(HousingData_XT) %in% RemoveFields)]

villagefilename <- paste(statevar,"VillageProcessed.csv",sep="")
townfilename <- paste(statevar,"TownProcessed.csv",sep="")

write.csv(villagedata_p,villagefilename,row.names=FALSE)
write.csv(towndata_p,townfilename,row.names=FALSE)

}
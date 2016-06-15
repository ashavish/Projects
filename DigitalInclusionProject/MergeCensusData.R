MergeCensusData <-function(statevar){
# Source other files
source("GenericFunctions.R")

# Load census metadata for Karnataka
import_md <- read.xlsx("Import_Metadata.xlsx",sheet="ImportExcels")
import_colnames_md <- read.xlsx("Import_Metadata.xlsx",sheet="Columns")
household_md <- import_md[import_md$Data=="Household" & import_md$State == statevar,]
pca_md <- import_md[import_md$Data =="PCA" & import_md$State==statevar,]
dchb_village_md <- import_md[import_md$Data=="DCHB_Village" & import_md$State == statevar,]
dchb_town_md <- import_md[import_md$Data=="DCHB_Town" & import_md$State == statevar,]

## Housing Data ---------------------------------------------------------------------
# Read Housing Data
HousingData <- data.frame()
for(i in 1:nrow(household_md)){
data <- load_data(household_md[i,c("Excel_Name")],household_md[i,"Rel_Folder_Loc"],sheet=1,startRow=8,colNames=FALSE)
print(paste("Housing data loaded for district",household_md[i,"District"],":",nrow(data)))
HousingData <- rbind(HousingData,data)
}
names(HousingData) <- import_colnames_md[import_colnames_md$Data == "Households",]$Col_Name

# Remove data at District & Sub District Level
HousingData_XT <- HousingData[HousingData$Town_Village !="000000",]
# Remove data at Ward level
HousingData_XT <- HousingData_XT[HousingData_XT$Ward =="0000",]


## PCA Data ------------------------------------------------------------------------
# Read PCA data
PCAData <-data.frame()
for(i in 1:nrow(pca_md)){
  data <- load_data(pca_md[i,c("Excel_Name")],pca_md[i,"Rel_Folder_Loc"],sheet=1,startRow=8,colNames=FALSE)
  print(paste("PCA data loaded for district",pca_md[i,"District"],":",nrow(data)))
  PCAData <- rbind(PCAData,data)
}
names(PCAData) <- import_colnames_md[import_colnames_md$Data == "PCA",]$Col_Name
PCAData$No_Households <- as.numeric(PCAData$No_Households)

# Remove District,Sub-District & Ward Levels from PCA data
PCAData_XT <- PCAData[PCAData$Level %in% c("VILLAGE","TOWN"),]

# Remove duplicates 
# Retain correct subdistricts for Towns & Villages based on cross referencing census data(http://vlist.in/)

if(statevar =="AndhraPradesh"){
  PCAData_XT <- PCAData_XT[!(PCAData_XT$Town_Village == "802900" & PCAData_XT$Subdistt != "04350"),]
  PCAData_XT <-PCAData_XT[!(PCAData_XT$Town_Village == "802918" & PCAData_XT$Subdistt != "04502"),]
  PCAData_XT <-PCAData_XT[!(PCAData_XT$Town_Village == "802930" & PCAData_XT$Subdistt != "04677"),]
  PCAData_XT <-PCAData_XT[!(PCAData_XT$Town_Village == "802947" & PCAData_XT$Subdistt != "04869"),]
  PCAData_XT <-PCAData_XT[!(PCAData_XT$Town_Village == "802969" & PCAData_XT$Subdistt != "05024"),]
  PCAData_XT <-PCAData_XT[!(PCAData_XT$Town_Village == "802994" & PCAData_XT$Subdistt != "05210"),]
  PCAData_XT <-PCAData_XT[!(PCAData_XT$Town_Village == "802998" & PCAData_XT$Subdistt != "05226"),]
  PCAData_XT <-PCAData_XT[!(PCAData_XT$Town_Village == "803002" & PCAData_XT$Subdistt != "05258"),]
}

# Remove rest of duplicates by removing the Area names with OG for Urban
x <- PCAData_XT[which(duplicated(PCAData_XT$Town_Village)),]$Town_Village
y <- PCAData_XT[grep("+ OG",PCAData_XT$Name),]$Name

PCAData_XT1 <- PCAData_XT[(PCAData_XT$Town_Village %in% x & !PCAData_XT$Name %in% y),]
PCAData_XT2 <- PCAData_XT[(!PCAData_XT$Town_Village %in% x),]
PCAData_XT <- rbind(PCAData_XT1,PCAData_XT2)


#1943 records are present in Karnataka which have 0 households/population. Removing such records
PCAData_XT <- PCAData_XT[PCAData_XT$Total_Population!=0,]

# Read DHCB Village data --------------------------------------------------------------------
DCHB_Village_Data <-data.frame()
for(i in 1:nrow(dchb_village_md)){
  data <- load_data(dchb_village_md[i,c("Excel_Name")],dchb_village_md[i,"Rel_Folder_Loc"],sheet=1,startRow=2,colNames=FALSE)
  print(paste("DCHB data loaded for State",dchb_village_md[i,"State"],":",nrow(data)))
  DCHB_Village_Data <- rbind(DCHB_Village_Data,data)
}
x <- import_colnames_md[import_colnames_md$Data == "DCHB_Village",]$Col_Name
if(statevar == "Karnataka"){names(DCHB_Village_Data) <- x[-c(11,260,313)]
DCHB_Village_Data$Distance_range_code_where_footpath_is_available <- NA
DCHB_Village_Data$Distance_range_code_where_village_pin_code_is_available <- NA}

if(statevar == "AndhraPradesh"){names(DCHB_Village_Data) <- x[-c(11,130,260,289,305,309,311,313,337)]
DCHB_Village_Data$Distance_range_code_where_village_pin_code_is_available <- NA
DCHB_Village_Data$Distance_range_code_where_rickshaw_machine_is_available <- NA
DCHB_Village_Data$Distance_range_code_where_kuchcha_is_available <- NA
DCHB_Village_Data$Distance_range_code_where_all_weather_road_is_available <- NA
DCHB_Village_Data$Distance_range_code_where_waterway_is_available <- NA
DCHB_Village_Data$Distance_range_code_where_footpath_is_available <- NA
DCHB_Village_Data$Distance_range_code_where_nutritional_center_others_is_available <- NA
}

DCHB_Village_Data$Village_Code <- as.character(DCHB_Village_Data$Village_Code)
DCHB_Village_Data$Village_Code <- gsub("e\\+05","00000",DCHB_Village_Data$Village_Code)


# Read DHCB Town data -------------------------------------------------------------------------
DCHB_Town_Data <-data.frame()
for(i in 1:nrow(dchb_town_md)){
  data <- load_data(dchb_town_md[i,c("Excel_Name")],dchb_town_md[i,"Rel_Folder_Loc"],sheet=1,startRow=2,colNames=FALSE)
  print(paste("DCHB data loaded for State",dchb_town_md[i,"State"],":",nrow(data)))
  DCHB_Town_Data <- rbind(DCHB_Town_Data,data)
}
x <- import_colnames_md[import_colnames_md$Data == "DCHB_Town",]$Col_Name
if(statevar == "Karnataka"){names(DCHB_Town_Data) <- x[-160]}
if(statevar == "AndhraPradesh"){names(DCHB_Town_Data) <- x[-c(86:93,188:190,216:218,223:225,230:232,237:239,244:246,307:309,314:316)]}

DCHB_Town_Data$Town_Code <- as.character(DCHB_Town_Data$Town_Code)

# Removing duplicates of 803080 and 803160
if(statevar == "Karnataka") {
DCHB_Town_Data <- DCHB_Town_Data[!(DCHB_Town_Data$Town_Code== 803080 & DCHB_Town_Data$Sub_District_Code==99999),]
DCHB_Town_Data <- DCHB_Town_Data[!(DCHB_Town_Data$Town_Code== 803160 & DCHB_Town_Data$Sub_District_Code==99999),]
}




## Merging ------------------------------------------------------------------------------------

# Merge Housing Data & PCA to get No of Households
HousingData_XT <- merge(HousingData_XT,PCAData_XT,by.x = c("Town_Village_Households","Tehsil_Code_Households"),
                        by.y = c("Town_Village","Subdistt"))

# Changing Housing Percent to Actual Numbers to enable hierarchy rollups and to derive digital inclusion parameters
#for (i in names(HousingData_XT[11:145])) {
#  HousingData_XT[,c(i)] <- round((HousingData_XT[,c(i)] * HousingData_XT$No_Households/100))
# }


# Writing intermediate files to disk
filename1 <- paste(statevar,"HousingPCA.csv",sep="")
write.csv(HousingData_XT,filename1,row.names=FALSE)



# Merged File for Village Analysis
HousingDataVillage_XT <- merge(HousingData_XT[HousingData_XT$Rural_Urban=="Rural",],DCHB_Village_Data,by.x = c("Town_Village_Households","Tehsil_Code_Households"),
                        by.y = c("Village_Code","Sub_District_Code"),all.x=TRUE)

filename2 <- paste(statevar,"Village.csv",sep="")
write.csv(HousingDataVillage_XT,filename2,row.names=FALSE)

# Merged File for Town Analysis
HousingDataTown_XT <- merge(HousingData_XT[HousingData_XT$Rural_Urban=="Urban",],DCHB_Town_Data,by.x = c("Town_Village_Households","Tehsil_Code_Households"),
                               by.y = c("Town_Code","Sub_District_Code"),all.x=TRUE)

filename3 <- paste(statevar,"Town.csv",sep="")
write.csv(HousingDataTown_XT,filename3,row.names=FALSE)

}
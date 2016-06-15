## Script for merging, extracting and analyzing census data. This is the main script which needs to be run
## Submitted by Asha Vishwanathan

## The entire loading process is managed using a import metadata excel file.
## For any changes like changing folder location etc, only the metadata file needs to be changed

# Set the working directory
path <- "C://Asha//WorkRelated//Analytics//Jigsaw//Jigsaw Contest"
setwd(path)

# Install required libraries & load
req_packages <- c("dplyr","openxlsx","readr","sp","RColorBrewer","ggplot2","cowplot")
new.packages <- req_packages[!(req_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(req_packages,require,character.only=TRUE)

# Source required scripts

source("GenericFunctions.R")
source("MergeCensusData.R")
source("ProcessCensusData.R")


# Merge and store information at Village & Town Level

MergeCensusData("Karnataka")
MergeCensusData("AndhraPradesh")

# Retrieve the merged files for Feature Engineering 

ProcessCensusData("Karnataka")
ProcessCensusData("AndhraPradesh")

# Load and Merge States
KarVillages <- read.csv("KarnatakaVillageProcessed.csv",header = TRUE,stringsAsFactors = FALSE)
APVillages <- read.csv("AndhraPradeshVillageProcessed.csv",header = TRUE,stringsAsFactors = FALSE)
KarTowns <- read.csv("KarnatakaTownProcessed.csv",header = TRUE,stringsAsFactors = FALSE)
APTowns <- read.csv("AndhraPradeshTownProcessed.csv",header = TRUE,stringsAsFactors = FALSE)

import_colnames_md <- read.xlsx("Import_Metadata.xlsx",sheet="Columns")
fields_villages <- import_colnames_md[import_colnames_md$Data %in% c("Households","PCA","DCHB_Village","New_Features_Common","New_Features_Village") & import_colnames_md$Keep_in_dataset=="Yes",]$Col_Name
fields_towns <- import_colnames_md[import_colnames_md$Data %in% c("Households","PCA","DCHB_Town","New_Features_Common","New_Features_Town") & import_colnames_md$Keep_in_dataset=="Yes",]$Col_Name

all_villages <- rbind(KarVillages[,fields_villages],APVillages[,fields_villages])
all_towns <- rbind(KarTowns[,fields_towns],APTowns[,fields_towns])

source("MissingValueChecks.R",verbose = TRUE)

write.csv(all_villages,"KarAP_Villages.csv",row.names = FALSE)
write.csv(all_towns,"KarAP_Towns.csv",row.names = FALSE)


# Run Map Concentrations
source("MapConcentrations.R",verbose = TRUE)

# Check Correlations,Merge Villages and Towns across States
source("CheckCorrelations.R",verbose=TRUE)

# Check associations for factor variables
source("CheckAssociations.R")

# Generate Plots
source("plots.R",verbose=TRUE)

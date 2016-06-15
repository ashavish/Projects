# Digital Inclusion Project
This project was done to understand Digital Inclusion using Census Data for the states of Karnataka and Andhra Pradesh. The project was jointly organized by IIM Faculty and Jigsaw Academy.The presentation of the findings was adjudjed as one of the finalist of the contest.

# Process
Data From Census considered for this analysis -
Household Data
PCA Data
DCHB Village Data
DCHB Town Data

Note : For the purposes of this study,  Telangana villages have been considered as part of Andhra Pradesh as per data from 2011 census

#Approach

A metadata driven approach for all data loading & processing
An excel sheet called “Import_Metadata.xls” was maintained which stored list of all excels available for the states of Karnataka & Andhra Pradesh.The sheet stores the folder location of the actual excels and also stores the column names for the different file types.The sheet “Import_Metadata.xls” drives the entire loading of files into R.It also maintains the column names for all the data that was imported for the different types of files. Apart from Columns, it stores some processing requirements which were used to identify relevant columns for processing,

In Housing data, all data was deleted where Town Code / Village Code was “000000”. As this indicated district / sub district level data.In Housing data, all data where ward was equal to “0000” was removed, as ward level data was to be deleted.PCA Data – Data was retained only for Levels in Village / Town. All other data was removed
PCA Data - Duplicates were found. Many Towns / Village codes had multiple records with different sub-districts.
For 8 codes, which were replicated many times, data from http://vlist.in/ (Village listing site of India) was cross checked and a single record retained.For all the other duplicates present for Urban areas, the Area names with “OG” was discarded to remove duplicates.1943 records were present in Karnataka which had 0 households/population. Such records were discarded.Duplicate data found in Karnataka Town DCHB excels for 803080 and 803160 was discarded by removing the ones mapped to Sub District Code 99999

# Merging

Housing data was merged with PCA data using Town/Village Code and Sub district code. This removed the duplicate town/village codes mapped to different sub districts from Housing data as well.
Merged Housing-PCA data was further merged with Village level data to generate a merged Village level dataset
And merged Housing-PCA data was merged with Town level data to generate a merged Town level dataset
Data was merged without removing any fields. Fields are narrowed down progressively.

# Details of Scripts

DigitalInclusionAnalysis.R ( Script which invokes all other scripts)
GenericFunctions.R (Script with some generic functions)
MergeCensusData.R (Script to merge initial data sets)
ProcessCensusData.R (Script to process, add new features and generate the processed sheets)
MissingValuesCheck.s.R (Script to check missing values)
MapConcentrations.R ( Script to create map concentrations)
CheckCorrelations.R (Script to check correlations with continuous variables)
CheckAssociations.R (Script to check ANOVA for categorical variables)
Plots.R (Script to create all plots)
DescriptiveAnalysis.R (Scripts with some basic descriptive checks on data)
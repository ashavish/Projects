
# Generic loading function
load_data <- function(filename,location,startrow,...){
  location <- ifelse(location != "" && !is.na(location),gsub("\\","//",location,fixed=TRUE),location)
  fullpath <- (if(location != "" && !is.na(location)){
    paste(location,"//",filename,".xlsx",sep="")
  }
  else { paste(filename,".xlsx",sep="")})
  print(fullpath)
  data <- read.xlsx(fullpath,...)  
  return(data)
}

# Function to check correlations
check_cor <- function(set1,set2,dataset){
  cor_dataset <- as.data.frame(cor(dataset[,set1],dataset[,set2]))
  cor_dataset$Features <- row.names(cor_dataset)
  row.names(cor_dataset) <- NULL
  return(cor_dataset)
}

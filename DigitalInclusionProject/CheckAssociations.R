cols_assoc <- import_colnames_md[import_colnames_md$Check_Associations=="Yes",]$Col_Name
col_groups <- import_colnames_md[import_colnames_md$Keep_in_dataset=="Yes" ,c("Col_Name","Category")]
all_villages[,cols_assoc] <- apply(all_villages[,cols_assoc],2,FUN = function(x) as.factor(x))

aov_table <- data.frame()
target_set <- c("ComputerAccess_pct","MobilePenetration_pct","InternetAccess_pct","Landline_pct")

for (i in cols_assoc) {
for (y1 in target_set) {
state <- "KARNATAKA"
if(length(unique(all_villages[all_villages$State_Name_Households==state,c(i)]))>1){
a <- aov(all_villages[all_villages$State_Name_Households==state,c(y1)] ~ all_villages[all_villages$State_Name_Households==state,c(i)],data=all_villages[all_villages$State_Name_Households==state,])
s <- summary(a)
print(i)
print(summary(a))
rec <- cbind(i,y1,state,s[[1]]$F[1],s[[1]]$Pr[1])
aov_table <- rbind(aov_table,rec)
}
state <- "ANDHRA PRADESH"
if(length(unique(all_villages[all_villages$State_Name_Households==state,c(i)]))>1){
a <- aov(all_villages[all_villages$State_Name_Households==state,c(y1)] ~ all_villages[all_villages$State_Name_Households==state,c(i)],data=all_villages[all_villages$State_Name_Households==state,])
s <- summary(a)
print(i)
print(summary(a))
rec <- cbind(i,y1,state,s[[1]]$F[1],s[[1]]$Pr[1])
aov_table <- rbind(aov_table,rec)
}
}
}
names(aov_table) <- c("Feature","Digital_Inclusion_Factor","State","F_value","Significance")
aov_table <- merge(aov_table,col_groups,by.x="Feature",by.y="Col_Name",all.x=TRUE)

write.csv(aov_table,"All_Associations.csv",row.names = FALSE)

 

library(cowplot)
library(RColorBrewer)
# Needs folder structure Plots/StateDistributions inside working directory
# Needs folder structure Plots/Correlations inside working directory


## BOX PLOTS -----------------------------------------------------------
# Box Plots for Computer Access Pct across states, districts
# State rollup based on Villages
statemeans <- aggregate(ComputerAccess_pct ~  State_Name_Households, all_villages, mean)
p <- ggplot(all_villages,aes(State_Name_Households,ComputerAccess_pct)) + labs(title="Computer Access % - Rollup from Villages",x = "", y="Computer Access %" ) + geom_boxplot(aes(color=as.factor(State_Name_Households))) + scale_y_continuous(limit = c(0, 25)) +   theme(legend.position = "none") 
p <- p + geom_point(data = statemeans, aes(colour="black",position = ComputerAccess_pct)) + geom_text(data = statemeans, aes(label = round(ComputerAccess_pct,2), y = ComputerAccess_pct + 1))
save_plot(paste(path,"//Plots//StateDistributions//CompAccessPctByState_Villages.png",sep=""), p,base_aspect_ratio = 1.3)


# State rollup based on Towns
statemeans <- aggregate(ComputerAccess_pct ~  State_Name_Households, all_towns, mean)
p <- ggplot(all_towns,aes(State_Name_Households,ComputerAccess_pct)) + labs(title="Computer Access % - Rollup from Towns",x = "", y="Computer Access %" ) + geom_boxplot(aes(color=as.factor(State_Name_Households))) + scale_y_continuous(limit = c(0, 25)) +   theme(legend.position = "none") 
p <- p + geom_point(data = statemeans, aes(colour="black",position = ComputerAccess_pct)) + geom_text(data = statemeans, aes(label = round(ComputerAccess_pct,2), y = ComputerAccess_pct + 1))
save_plot(paste(path,"//Plots//StateDistributions//CompAccessPctByState_Towns.png",sep=""), p,base_aspect_ratio = 1.3)


# Karnataka Villages
districtmeans_K <- aggregate(ComputerAccess_pct ~  District_Name_Households, all_villages[all_villages$State_Name_Households=="KARNATAKA",], mean)
p <- ggplot(all_villages[all_villages$State_Name_Households=="KARNATAKA",],aes(District_Name_Households,ComputerAccess_pct)) + labs(title="Computer Access % - KAR Villages",x = "", y="Computer Access %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") + scale_y_continuous(limit = c(0, 25)) 
p <- p + geom_point(data = districtmeans_K, aes(colour="black",position = ComputerAccess_pct)) + geom_text(data = districtmeans_K, aes(label = round(ComputerAccess_pct,2), y = ComputerAccess_pct + 0.8))
save_plot(paste(path,"//Plots//StateDistributions//CompAccessPctByDistrict_KarnatakaVillages.png",sep=""), p,base_aspect_ratio = 3)

# AP Villages
districtmeans_AP <- aggregate(ComputerAccess_pct ~  District_Name_Households, all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",], mean)
p <- ggplot(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",],aes(District_Name_Households,ComputerAccess_pct)) + labs(title="Computer Access % - AP Villages",x = "", y="Computer Access %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + scale_y_continuous(limit = c(0, 25)) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_AP, aes(colour="black",position = ComputerAccess_pct)) + geom_text(data = districtmeans_AP, aes(label = round(ComputerAccess_pct,2), y = ComputerAccess_pct + 0.8))
save_plot(paste(path,"//Plots//StateDistributions//CompAccessPctByDistrict_APVillages.png",sep=""), p,base_aspect_ratio = 3)



#AP Towns
districtmeans_APTowns <- aggregate(ComputerAccess_pct ~  District_Name_Households, all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",], mean)
p <- ggplot(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",],aes(x=District_Name_Households,y=ComputerAccess_pct)) + labs(title="Computer Access % - AP Towns",x = "", y="Computer Access %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_APTowns, aes(colour="black",position = ComputerAccess_pct)) + geom_text(data = districtmeans_APTowns, aes(label = round(ComputerAccess_pct,2), y = ComputerAccess_pct + 1.5))
save_plot(paste(path,"//Plots//StateDistributions//CompAccessPctByDistrict_APTowns.png",sep=""), p,base_aspect_ratio = 3)

# Karnataka Towns
districtmeans_KarTowns <- aggregate(ComputerAccess_pct ~  District_Name_Households, all_towns[all_towns$State_Name_Households=="KARNATAKA",], mean)
p <- ggplot(all_towns[all_towns$State_Name_Households=="KARNATAKA",],aes(x=District_Name_Households,y=ComputerAccess_pct)) + labs(title="Computer Access % - KAR Towns",x = "", y="Computer Access %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_KarTowns, aes(colour="black",position = ComputerAccess_pct)) + geom_text(data = districtmeans_KarTowns, aes(label = round(ComputerAccess_pct,2), y = ComputerAccess_pct + 1.5))
save_plot(paste(path,"//Plots//StateDistributions//CompAccessPctByDistrict_KarTowns.png",sep=""), p,base_aspect_ratio = 3)


# Box Plots for Mobile Penetration Pct across States and districts
# Mobile Penetration rolled up based on villages
statemeans <- aggregate(MobilePenetration_pct ~  State_Name_Households, all_villages, mean)
p <- ggplot(all_villages,aes(State_Name_Households,MobilePenetration_pct)) + labs(title="Mobile Penetration % - Rollup from Villages",x = "", y="Mobile Penetration %" ) + geom_boxplot(aes(color=as.factor(State_Name_Households))) + scale_y_continuous(limit = c(0, 60)) +   theme(legend.position = "none") 
p <- p + geom_point(data = statemeans, aes(colour="black",position = MobilePenetration_pct)) + geom_text(data = statemeans, aes(label = round(MobilePenetration_pct,2), y = MobilePenetration_pct + 2))
save_plot(paste(path,"//Plots//StateDistributions//MobPentrPctByState_Villages.png",sep=""), p,base_aspect_ratio = 1.3)

# Mobile Penetration rolled up based on towns
statemeans <- aggregate(MobilePenetration_pct ~  State_Name_Households, all_towns, mean)
p <- ggplot(all_towns,aes(State_Name_Households,MobilePenetration_pct)) + labs(title="Mobile Penetration % - Rollup from Towns",x = "", y="Mobile Penetration %" ) + geom_boxplot(aes(color=as.factor(State_Name_Households))) +   theme(legend.position = "none") 
p <- p + geom_point(data = statemeans, aes(colour="black",position = MobilePenetration_pct)) + geom_text(data = statemeans, aes(label = round(MobilePenetration_pct,2), y = MobilePenetration_pct + 2))
save_plot(paste(path,"//Plots//StateDistributions//MobPentrPctByState_Towns.png",sep=""), p,base_aspect_ratio = 1.3)

# Karnataka Villages
districtmeans_K <- aggregate(MobilePenetration_pct ~  District_Name_Households, all_villages[all_villages$State_Name_Households=="KARNATAKA",], mean)
p <- ggplot(all_villages[all_villages$State_Name_Households=="KARNATAKA",],aes(District_Name_Households,MobilePenetration_pct)) + labs(title="Mobile Penetration % - KAR Villages",x = "", y="Mobile Penetration %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_K, aes(colour="black",position = MobilePenetration_pct)) + geom_text(data = districtmeans_K, aes(label = round(MobilePenetration_pct,2),y = MobilePenetration_pct + 3))
save_plot(paste(path,"//Plots//StateDistributions//MobPentrPctByDistrict_KarnatakaVillages.png",sep=""), p,base_aspect_ratio = 3)


# AP Villages
districtmeans_AP <- aggregate(MobilePenetration_pct ~  District_Name_Households, all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",], mean)
p <- ggplot(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",],aes(District_Name_Households,MobilePenetration_pct)) + labs(title="Mobile Penetration % - AP Villages",x = "", y="Mobile Penetration %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_AP, aes(colour="black",position = MobilePenetration_pct)) + geom_text(data = districtmeans_AP, aes(label = round(MobilePenetration_pct,2), y = MobilePenetration_pct + 3))
save_plot(paste(path,"//Plots//StateDistributions//MobPentrPctByDistrict_APVillages.png",sep=""), p,base_aspect_ratio = 3)

# Karnataka Towns
districtmeans_K <- aggregate(MobilePenetration_pct ~  District_Name_Households, all_towns[all_towns$State_Name_Households=="KARNATAKA",], mean)
p <- ggplot(all_towns[all_towns$State_Name_Households=="KARNATAKA",],aes(District_Name_Households,MobilePenetration_pct)) + labs(title="Mobile Penetration % - KAR Towns",x = "", y="Mobile Penetration %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_K, aes(colour="black",position = MobilePenetration_pct)) + geom_text(data = districtmeans_K, aes(label = round(MobilePenetration_pct,2),y = MobilePenetration_pct + 3))
save_plot(paste(path,"//Plots//StateDistributions//MobPentrPctByDistrict_KarnatakaTowns.png",sep=""), p,base_aspect_ratio = 3)


# AP Towns
districtmeans_AP <- aggregate(MobilePenetration_pct ~  District_Name_Households, all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",], mean)
p <- ggplot(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",],aes(District_Name_Households,MobilePenetration_pct)) + labs(title="Mobile Penetration % - AP Towns",x = "", y="Mobile Penetration %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_AP, aes(colour="black",position = MobilePenetration_pct)) + geom_text(data = districtmeans_AP, aes(label = round(MobilePenetration_pct,2), y = MobilePenetration_pct + 3))
save_plot(paste(path,"//Plots//StateDistributions//MobPentrPctByDistrict_APTowns.png",sep=""), p,base_aspect_ratio = 3)


# Box Plots for Internet Access  Pct across States and districts
# Internet Access rolled up based on villages
statemeans <- aggregate(InternetAccess_pct ~  State_Name_Households, all_villages, mean)
p <- ggplot([all_villages$State_Name_Households=="KARNATAKA",],aes(State_Name_Households,InternetAccess_pct)) + labs(title="Internet Access % - Rollup from Villages",x = "", y="Internet Access %" ) + geom_boxplot(aes(color=as.factor(State_Name_Households))) + scale_y_continuous(limit = c(0, 60)) +   theme(legend.position = "none") 
p <- p + geom_point(data = statemeans, aes(colour="black",position = InternetAccess_pct)) + geom_text(data = statemeans, aes(label = round(InternetAccess_pct,2), y = InternetAccess_pct + 2))
save_plot(paste(path,"//Plots//StateDistributions//InternetPctByState_Villages.png",sep=""), p,base_aspect_ratio = 1.3)

# Internet Access rolled up based on towns
statemeans <- aggregate(InternetAccess_pct ~  State_Name_Households, all_towns, mean)
p <- ggplot(all_towns,aes(State_Name_Households,InternetAccess_pct)) + labs(title="Internet Access % - Rollup from Towns",x = "", y="Internet Access %" ) + geom_boxplot(aes(color=as.factor(State_Name_Households))) +   theme(legend.position = "none") 
p <- p + geom_point(data = statemeans, aes(colour="black",position = InternetAccess_pct)) + geom_text(data = statemeans, aes(label = round(InternetAccess_pct,2), y = InternetAccess_pct + 2))
save_plot(paste(path,"//Plots//StateDistributions//InternetPctByState_Towns.png",sep=""), p,base_aspect_ratio = 1.3)

# Karnataka Villages
districtmeans_K <- aggregate(InternetAccess_pct ~  District_Name_Households, all_villages[all_villages$State_Name_Households=="KARNATAKA",], mean)
p <- ggplot(all_villages[all_villages$State_Name_Households=="KARNATAKA",],aes(District_Name_Households,InternetAccess_pct)) + labs(title="Internet Access % - KAR Villages",x = "", y="Internet Access %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_K, aes(colour="black",position = InternetAccess_pct)) + geom_text(data = districtmeans_K, aes(label = round(InternetAccess_pct,2),y = InternetAccess_pct + 3))
save_plot(paste(path,"//Plots//StateDistributions//InternetPctByDistrict_KarnatakaVillages.png",sep=""), p,base_aspect_ratio = 3)


# AP Villages
districtmeans_AP <- aggregate(InternetAccess_pct ~  District_Name_Households, all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",], mean)
p <- ggplot(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",],aes(District_Name_Households,InternetAccess_pct)) + labs(title="Internet Access % - AP Villages",x = "", y="Internet Access %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_AP, aes(colour="black",position = InternetAccess_pct)) + geom_text(data = districtmeans_AP, aes(label = round(InternetAccess_pct,2), y = InternetAccess_pct + 3))
save_plot(paste(path,"//Plots//StateDistributions//InternetPctByDistrict_APVillages.png",sep=""), p,base_aspect_ratio = 3)

#AP Towns
districtmeans_APTowns <- aggregate(InternetAccess_pct ~  District_Name_Households, all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",], mean)
p <- ggplot(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",],aes(x=District_Name_Households,y=InternetAccess_pct)) + labs(title="Internet Access % - AP Towns",x = "", y="Internet Access %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_APTowns, aes(colour="black",position = InternetAccess_pct)) + geom_text(data = districtmeans_APTowns, aes(label = round(InternetAccess_pct,2), y = InternetAccess_pct + 2))
save_plot(paste(path,"//Plots//StateDistributions//InternetPctByDistrict_APTowns.png",sep=""), p,base_aspect_ratio = 3)


# Karnataka Towns
districtmeans_KarTowns <- aggregate(InternetAccess_pct ~  District_Name_Households, all_towns[all_towns$State_Name_Households=="KARNATAKA",], mean)
p <- ggplot(all_towns[all_towns$State_Name_Households=="KARNATAKA",],aes(x=District_Name_Households,y=InternetAccess_pct)) + labs(title="Internet Access % - KAR Towns",x = "", y="Internet Access %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_KarTowns, aes(colour="black",position = InternetAccess_pct)) + geom_text(data = districtmeans_KarTowns, aes(label = round(InternetAccess_pct,2), y = InternetAccess_pct + 2))
save_plot(paste(path,"//Plots//StateDistributions//InternetPctByDistrict_KarTowns.png",sep=""), p,base_aspect_ratio = 3)


# Box Plots for Landline  Pct across States and districts
# Landline rolled up based on villages
statemeans <- aggregate(Landline_pct ~  State_Name_Households, all_villages, mean)
p <- ggplot(all_villages,aes(State_Name_Households,Landline_pct)) + labs(title="Landline % - Rollup from Villages",x = "", y="Landline %" ) + geom_boxplot(aes(color=as.factor(State_Name_Households))) + scale_y_continuous(limit = c(0, 60)) +   theme(legend.position = "none") 
p <- p + geom_point(data = statemeans, aes(colour="black",position = Landline_pct)) + geom_text(data = statemeans, aes(label = round(Landline_pct,2), y = Landline_pct + 2))
save_plot(paste(path,"//Plots//StateDistributions//LandlinePctByState_Villages.png",sep=""), p,base_aspect_ratio = 1.3)

# Landline rolled up based on towns
statemeans <- aggregate(Landline_pct ~  State_Name_Households, all_towns, mean)
p <- ggplot(all_towns,aes(State_Name_Households,Landline_pct)) + labs(title="Landline % - Rollup from Towns",x = "", y="Landline %" ) + geom_boxplot(aes(color=as.factor(State_Name_Households))) +   theme(legend.position = "none") 
p <- p + geom_point(data = statemeans, aes(colour="black",position = Landline_pct)) + geom_text(data = statemeans, aes(label = round(Landline_pct,2), y = Landline_pct + 2))
save_plot(paste(path,"//Plots//StateDistributions//LandlinePctByState_Towns.png",sep=""), p,base_aspect_ratio = 1.3)

# Karnataka Villages
districtmeans_K <- aggregate(Landline_pct ~  District_Name_Households, all_villages[all_villages$State_Name_Households=="KARNATAKA",], mean)
p <- ggplot(all_villages[all_villages$State_Name_Households=="KARNATAKA",],aes(District_Name_Households,Landline_pct)) + labs(title="Landline % - KAR Villages",x = "", y="Landline %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_K, aes(colour="black",position = Landline_pct)) + geom_text(data = districtmeans_K, aes(label = round(Landline_pct,2),y = Landline_pct + 3))
save_plot(paste(path,"//Plots//StateDistributions//LandlinePctByDistrict_KarnatakaVillages.png",sep=""), p,base_aspect_ratio = 3)


# AP Villages
districtmeans_AP <- aggregate(Landline_pct ~  District_Name_Households, all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",], mean)
p <- ggplot(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",],aes(District_Name_Households,Landline_pct)) + labs(title="Landline % - AP Villages",x = "", y="Landline %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_AP, aes(colour="black",position = Landline_pct)) + geom_text(data = districtmeans_AP, aes(label = round(Landline_pct,2), y = Landline_pct + 3))
save_plot(paste(path,"//Plots//StateDistributions//LandlinePctByDistrict_APVillages.png",sep=""), p,base_aspect_ratio = 3)

#AP Towns
districtmeans_APTowns <- aggregate(Landline_pct ~  District_Name_Households, all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",], mean)
p <- ggplot(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",],aes(x=District_Name_Households,y=Landline_pct)) + labs(title="Landline % - AP Towns",x = "", y="Landline %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_APTowns, aes(colour="black",position = Landline_pct)) + geom_text(data = districtmeans_APTowns, aes(label = round(Landline_pct,2), y = Landline_pct + 2))
save_plot(paste(path,"//Plots//StateDistributions//LandlinePctByDistrict_APTowns.png",sep=""), p,base_aspect_ratio = 3)


# Karnataka Towns
districtmeans_KarTowns <- aggregate(Landline_pct ~  District_Name_Households, all_towns[all_towns$State_Name_Households=="KARNATAKA",], mean)
p <- ggplot(all_towns[all_towns$State_Name_Households=="KARNATAKA",],aes(x=District_Name_Households,y=Landline_pct)) + labs(title="Landline % - KAR Towns",x = "", y="Landline %" ) + geom_boxplot(aes(color=as.factor(District_Name_Households))) + theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "none") 
p <- p + geom_point(data = districtmeans_KarTowns, aes(colour="black",position = Landline_pct)) + geom_text(data = districtmeans_KarTowns, aes(label = round(Landline_pct,2), y = Landline_pct + 2))
save_plot(paste(path,"//Plots//StateDistributions//LandlinePctByDistrict_KarTowns.png",sep=""), p,base_aspect_ratio = 3)






## INDEX PLOTS -------------------------------------------------------------------------
# Index Plot for All Towns in Karnataka - Computer Access %
all_towns$Districts <- as.factor(all_towns$District_Name_Households)
p <- ggplot(all_towns[all_towns$State_Name_Households=="KARNATAKA",],aes(x=seq_along(ComputerAccess_pct),y=ComputerAccess_pct)) +  labs(title = "Karnataka - Towns",x="", y = "Computer Access %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//KarTowns_CA.png",sep=""), p,base_aspect_ratio = 3)
 
# Index Plot for All Towns in AP - Computer Access %
p <- ggplot(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",],aes(x=seq_along(ComputerAccess_pct),y=ComputerAccess_pct)) +  labs(title = "Andhra Pradesh - Towns",x="", y = "Computer Access %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//APTowns_CA.png",sep=""), p,base_aspect_ratio = 3)

# Index Plot for All Villages in Karnataka - Computer Access %
all_villages$Districts <- as.factor(all_villages$District_Name_Households)
p <- ggplot(all_villages[all_villages$State_Name_Households=="KARNATAKA",],aes(x=seq_along(ComputerAccess_pct),y=ComputerAccess_pct)) +  labs(title = "Karnataka - Villages",x="", y = "Computer Access %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//KarVillages_CA.png",sep=""), p,base_aspect_ratio = 3)
 

# Index Plot for All Villages in AP - Computer Access %
p <- ggplot(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",],aes(x=seq_along(ComputerAccess_pct),y=ComputerAccess_pct)) +  labs(title = "Andhra Pradesh - Villages",x="", y = "Computer Access %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//APVillages_CA.png",sep=""), p,base_aspect_ratio = 3)


# Index Plot for All Towns in Karnataka - Mobile Penetration %
all_towns$Districts <- as.factor(all_towns$District_Name_Households)
p <- ggplot(all_towns[all_towns$State_Name_Households=="KARNATAKA",],aes(x=seq_along(MobilePenetration_pct),y=MobilePenetration_pct)) +  labs(title = "Karnataka - Towns",x="", y = "Mobile Penetration %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//KarTowns_MP.png",sep=""), p,base_aspect_ratio = 3)

 
# Index Plot for All Towns in AP - Mobile Penetration %
p <- ggplot(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",],aes(x=seq_along(MobilePenetration_pct),y=MobilePenetration_pct)) +  labs(title = "Andhra Pradesh - Towns",x="", y = "Mobile Penetration %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//APTowns_MP.png",sep=""), p,base_aspect_ratio = 3)
 

# Index Plot for All Villages in Karnataka - Mobile Penetration %
all_villages$Districts <- as.factor(all_villages$District_Name_Households)
p <- ggplot(all_villages[all_villages$State_Name_Households=="KARNATAKA",],aes(x=seq_along(MobilePenetration_pct),y=MobilePenetration_pct)) +  labs(title = "Karnataka - Villages",x="", y = "Mobile Penetration %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//KarVillages_MP.png",sep=""), p,base_aspect_ratio = 3)
 

# Index Plot for All Villages in AP - Mobile Penetration %
p <- ggplot(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",],aes(x=seq_along(MobilePenetration_pct),y=MobilePenetration_pct)) +  labs(title = "Andhra Pradesh - Villages",x="", y = "Mobile Penetration %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//APVillages_MP.png",sep=""), p,base_aspect_ratio = 3)

 

# Index Plot for All Villages in Karnataka - Internet Access %
all_villages$Districts <- as.factor(all_villages$District_Name_Households)
p <- ggplot(all_villages[all_villages$State_Name_Households=="KARNATAKA",],aes(x=seq_along(InternetAccess_pct),y=InternetAccess_pct)) +  labs(title = "Karnataka - Villages",x="", y = "Internet Access %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//KarVillages_IA.png",sep=""), p,base_aspect_ratio = 3)


# Index Plot for All Towns in Karnataka - Internet Access %
all_towns$Districts <- as.factor(all_towns$District_Name_Households)
p <- ggplot(all_towns[all_towns$State_Name_Households=="KARNATAKA",],aes(x=seq_along(InternetAccess_pct),y=InternetAccess_pct)) +  labs(title = "Karnataka - Towns",x="", y = "Internet Access %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//KarTowns_IA.png",sep=""), p,base_aspect_ratio = 3)

# Index Plot for All Villages in AP - Internet Access %
p <- ggplot(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",],aes(x=seq_along(InternetAccess_pct),y=InternetAccess_pct)) +  labs(title = "Andhra Pradesh - Villages",x="", y = "Internet Access %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//APVillages_IA.png",sep=""), p,base_aspect_ratio = 3)

# Index Plot for All Towns in AP - Internet Access %
p <- ggplot(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",],aes(x=seq_along(InternetAccess_pct),y=InternetAccess_pct)) +  labs(title = "Andhra Pradesh - Towns",x="", y = "Internet Access %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//APTowns_IA.png",sep=""), p,base_aspect_ratio = 3)


# Index Plot for All Villages in Karnataka - Landline %
all_villages$Districts <- as.factor(all_villages$District_Name_Households)
p <- ggplot(all_villages[all_villages$State_Name_Households=="KARNATAKA",],aes(x=seq_along(Landline_pct),y=Landline_pct)) +  labs(title = "Karnataka - Villages",x="", y = "Landline %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//KarVillages_LP.png",sep=""), p,base_aspect_ratio = 3)


# Index Plot for All Towns in Karnataka - Landline %
all_towns$Districts <- as.factor(all_towns$District_Name_Households)
p <- ggplot(all_towns[all_towns$State_Name_Households=="KARNATAKA",],aes(x=seq_along(Landline_pct),y=Landline_pct)) +  labs(title = "Karnataka - Towns",x="", y = "Landline %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//KarTowns_LP.png",sep=""), p,base_aspect_ratio = 3)

# Index Plot for All Villages in AP - Landline %
p <- ggplot(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",],aes(x=seq_along(Landline_pct),y=Landline_pct)) +  labs(title = "Andhra Pradesh - Villages",x="", y = "Landline %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//APVillages_LP.png",sep=""), p,base_aspect_ratio = 3)

# Index Plot for All Towns in AP - Landline %
p <- ggplot(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",],aes(x=seq_along(Landline_pct),y=Landline_pct)) +  labs(title = "Andhra Pradesh - Towns",x="", y = "Landline %") 
p <- p + geom_point(aes(colour=Districts))
save_plot(paste(path,"//Plots//StateDistributions//APTowns_LP.png",sep=""), p,base_aspect_ratio = 3)


# Scatter Plots
# Plotting Correlations


p <- ggplot(data=All_Corr[All_Corr$Location=="Karnataka Villages",],aes(x=Features,y=ComputerAccess_pct)) + labs(title = "Karnata Villages- Computer Access %",x="", y = "Correlation") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//KarVillagesCA_Cor.png",sep=""), p,base_aspect_ratio = 3)


p <- ggplot(data=All_Corr[All_Corr$Location=="AP Villages",],aes(x=Features,y=ComputerAccess_pct)) + labs(title = "Andhra Villages - Computer Access % ",x="", y = "Correlation") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//APVillagesCA_Cor.png",sep=""), p,base_aspect_ratio = 3)


p <- ggplot(data=All_Corr[All_Corr$Location=="Karnataka Towns",],aes(x=Features,y=ComputerAccess_pct)) + labs(title = "Karnata Towns - Computer Access % ",x="", y = "Correlation") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//KarTownsCA_Cor.png",sep=""), p,base_aspect_ratio = 3)


p <- ggplot(data=All_Corr[All_Corr$Location=="AP Towns",],aes(x=Features,y=ComputerAccess_pct)) + labs(title = "Andhra Towns - Computer Access %",x="", y = "Correlation") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//APTownsCA_Cor.png",sep=""), p,base_aspect_ratio = 3)


# Plotting Correlations
p <- ggplot(data=All_Corr[All_Corr$Location=="Karnataka Villages",],aes(x=Features,y=MobilePenetration_pct)) + labs(title = "Karnata Villages - Correlation",x="", y = "Mobile Penetration %") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//KarVillagesMP_Cor.png",sep=""), p,base_aspect_ratio = 3)



p <- ggplot(data=All_Corr[All_Corr$Location=="AP Villages",],aes(x=Features,y=MobilePenetration_pct)) + labs(title = "Andhra Villages - Correlations ",x="", y = "Mobile Penetration %") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//APVillagesMP_Cor.png",sep=""), p,base_aspect_ratio = 3)


p <- ggplot(data=All_Corr[All_Corr$Location=="Karnataka Towns",],aes(x=Features,y=MobilePenetration_pct)) + labs(title = "Karnata Towns - Correlations ",x="", y = "Mobile Penetration %") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//KarTownsMP_Cor.png",sep=""), p,base_aspect_ratio = 3)



p <- ggplot(data=All_Corr[All_Corr$Location=="AP Towns",],aes(x=Features,y=MobilePenetration_pct)) + labs(title = "Andhra Towns - Correlations ",x="", y = "Mobile Penetration %") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//APTownsMP_Cor.png",sep=""), p,base_aspect_ratio = 3)

# Plotting Correlations for Internet Access

p <- ggplot(data=All_Corr[All_Corr$Location=="Karnataka Villages",],aes(x=Features,y=InternetAccess_pct)) + labs(title = "Karnata Villages - Correlation",x="", y = "Internet Access %") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//KarVillagesIA_Cor.png",sep=""), p,base_aspect_ratio = 3)



p <- ggplot(data=All_Corr[All_Corr$Location=="AP Villages",],aes(x=Features,y=InternetAccess_pct)) + labs(title = "Andhra Villages - Correlations ",x="", y = "Internet Access %") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//APVillagesIA_Cor.png",sep=""), p,base_aspect_ratio = 3)


p <- ggplot(data=All_Corr[All_Corr$Location=="Karnataka Towns",],aes(x=Features,y=InternetAccess_pct)) + labs(title = "Karnata Towns - Correlations ",x="", y = "Internet Access %") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//KarTownsIA_Cor.png",sep=""), p,base_aspect_ratio = 3)



p <- ggplot(data=All_Corr[All_Corr$Location=="AP Towns",],aes(x=Features,y=InternetAccess_pct)) + labs(title = "Andhra Towns - Correlations ",x="", y = "Internet Access %") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//APTownsIA_Cor.png",sep=""), p,base_aspect_ratio = 3)

# Plotting Correlations for Landline
p <- ggplot(data=All_Corr[All_Corr$Location=="Karnataka Villages",],aes(x=Features,y=Landline_pct)) + labs(title = "Karnata Villages - Correlation",x="", y = "Landline %") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//KarVillagesLP_Cor.png",sep=""), p,base_aspect_ratio = 3)



p <- ggplot(data=All_Corr[All_Corr$Location=="AP Villages",],aes(x=Features,y=Landline_pct)) + labs(title = "Andhra Villages - Correlations ",x="", y = "Landline %") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//APVillagesLP_Cor.png",sep=""), p,base_aspect_ratio = 3)


p <- ggplot(data=All_Corr[All_Corr$Location=="Karnataka Towns",],aes(x=Features,y=Landline_pct)) + labs(title = "Karnata Towns - Correlations ",x="", y = "Landline %") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//KarTownsLP_Cor.png",sep=""), p,base_aspect_ratio = 3)



p <- ggplot(data=All_Corr[All_Corr$Location=="AP Towns",],aes(x=Features,y=Landline_pct)) + labs(title = "Andhra Towns - Correlations ",x="", y = "Landline %") + theme(axis.text.x=element_blank(),axis.ticks=element_blank())
p <- p + geom_point(aes(colour=Category))
save_plot(paste(path,"//Plots//Correlations//APTownsLP_Cor.png",sep=""), p,base_aspect_ratio = 3)

# Correlations for some important variables
plot_vars <- import_colnames_md[import_colnames_md$Scatter_Plots=="Yes",]$Col_Name
for (i in plot_vars) {
print(i)
title_y <- gsub("_"," ",i)
title_y <- gsub("pct","%",title_y)
title_y <- gsub("Availability of assets ","Avl. of ",title_y)
y1 <- "MobilePenetration_pct"
p1 <- ggplot(data=all_villages,aes_string(i,y1)) + labs(title = "Villages ",x="", y = title_y) + theme(axis.text.x=element_blank(),axis.ticks=element_blank(),axis.title.y=element_text(size = 7)) + theme(legend.position="none")
p1 <- p1 + geom_point(aes(colour=MobilePenetration_pct)) + facet_grid(.~State_Name_Households) + stat_smooth(method="lm",colour="red") 
p2 <- ggplot(data=all_towns,aes_string(i,y1)) + labs(title = "Towns ",x="Mobile Penetration %", y = title_y) + theme(axis.text.x=element_blank(),axis.ticks=element_blank(),axis.title.y=element_text(size = 7)) + theme(legend.position="none")
p2 <- p2 + geom_point(aes(colour=MobilePenetration_pct)) + facet_grid(.~State_Name_Households) + stat_smooth(method="lm",colour="red") 
p <- plot_grid(p1, p2,ncol = 1, nrow = 2)
save_plot(paste(path,"//Plots//Correlations//",i,"_MP.png",sep=""), p,base_aspect_ratio = 2) 

y1 <- "ComputerAccess_pct"
p1 <- ggplot(data=all_villages,aes_string(i,y1)) + labs(title = "Villages ",x="", y = title_y) + theme(axis.text.x=element_blank(),axis.ticks=element_blank(),axis.title.y=element_text(size = 7)) + theme(legend.position="none")
p1 <- p1 + geom_point(aes(colour=ComputerAccess_pct)) + facet_grid(.~State_Name_Households) + stat_smooth(method="lm",colour="red") 
p2 <- ggplot(data=all_towns,aes_string(i,y1)) + labs(title = "Towns ",x="Computer Access %", y = title_y) + theme(axis.text.x=element_blank(),axis.ticks=element_blank(),axis.title.y=element_text(size = 7)) + theme(legend.position="none")
p2 <- p2 + geom_point(aes(colour=ComputerAccess_pct)) + facet_grid(.~State_Name_Households) + stat_smooth(method="lm",colour="red") 
p <- plot_grid(p1, p2,ncol = 1, nrow = 2)
save_plot(paste(path,"//Plots//Correlations//",i,"_CA.png",sep=""), p,base_aspect_ratio = 2) 

y1 <- "InternetAccess_pct"
p1 <- ggplot(data=all_villages,aes_string(i,y1)) + labs(title = "Villages ",x="", y = title_y) + theme(axis.text.x=element_blank(),axis.ticks=element_blank(),axis.title.y=element_text(size = 7)) + theme(legend.position="none")
p1 <- p1 + geom_point(aes(colour=InternetAccess_pct)) + facet_grid(.~State_Name_Households) + stat_smooth(method="lm",colour="red") 
p2 <- ggplot(data=all_towns,aes_string(i,y1)) + labs(title = "Towns ",x="Internet Access %", y = title_y) + theme(axis.text.x=element_blank(),axis.ticks=element_blank(),axis.title.y=element_text(size = 7)) + theme(legend.position="none")
p2 <- p2 + geom_point(aes(colour=InternetAccess_pct)) + facet_grid(.~State_Name_Households) + stat_smooth(method="lm",colour="red") 
p <- plot_grid(p1, p2,ncol = 1, nrow = 2)
save_plot(paste(path,"//Plots//Correlations//",i,"_IA.png",sep=""), p,base_aspect_ratio = 2) 

y1 <- "Landline_pct"
p1 <- ggplot(data=all_villages,aes_string(i,y1)) + labs(title = "Villages ",x="", y = title_y) + theme(axis.text.x=element_blank(),axis.ticks=element_blank(),axis.title.y=element_text(size = 7)) + theme(legend.position="none")
p1 <- p1 + geom_point(aes(colour=Landline_pct)) + facet_grid(.~State_Name_Households) + stat_smooth(method="lm",colour="red") 
p2 <- ggplot(data=all_towns,aes_string(i,y1)) + labs(title = "Towns ",x="Landline %", y = title_y) + theme(axis.text.x=element_blank(),axis.ticks=element_blank(),axis.title.y=element_text(size = 7)) + theme(legend.position="none")
p2 <- p2 + geom_point(aes(colour=Landline_pct)) + facet_grid(.~State_Name_Households) + stat_smooth(method="lm",colour="red") 
p <- plot_grid(p1, p2,ncol = 1, nrow = 2)
save_plot(paste(path,"//Plots//Correlations//",i,"_LP.png",sep=""), p,base_aspect_ratio = 2) 
}

boxplot_vars <- import_colnames_md[import_colnames_md$Box_Plots=="Yes",]$Col_Name

for (i in boxplot_vars){
digital_inclusion <- c("ComputerAccess_pct","InternetAccess_pct","MobilePenetration_pct","Landline_pct")
for (y in digital_inclusion) {
title_x <- gsub("_"," ",i)
title_y <- gsub("_pct"," %",y)
p <- ggplot(all_villages,aes_string(i,y)) + geom_boxplot(aes(colour=as.factor(i))) + facet_grid(.~State_Name_Households) + theme(legend.position="none") + labs(title = "Villages ",x=title_x, y = title_y)
save_plot(paste(path,"//Plots//Correlations//",i,"_",y,".png",sep=""), p,base_aspect_ratio = 2) 
}
}

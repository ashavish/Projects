# Set the working directory
# Needs folder structure Plots/Maps inside working directory

library(sp)
library(RColorBrewer)
districtmap <- readRDS("IND_adm2.rds")
karnatakamap <- districtmap[districtmap$NAME_1 == "Karnataka",]
## QUESTION - DO I SEPARATE TELANGANA OR SHOW IT COMBINED ?
APmap <- districtmap[districtmap$NAME_1 %in% c("Andhra Pradesh","Telangana"),]
#APmap <- APmap[APmap$NAME_2 != "Hyderabad",]


# Plot Function for Map
plot_map <- function(dataset,by,title,plotcolor){
  sl1 <- list('sp.text', coordinates(dataset), txt=as.character(dataset$NAME_2), cex=0.7,col='black')
  scheme <- switch(plotcolor,"CA"="Blues","MP"="Greens","IA"="Oranges","LP"="Purples")
  spplot(dataset,by,sp.layout=list(sl1),main=title,col.regions=colorRampPalette(brewer.pal(11, scheme))(20))
}
# Karnataka Villages

KarVillages$District_Name_Households <- trimws(KarVillages$District_Name_Households)
KarVillages[KarVillages$District_Name_Households=="Chikkaballapura",]$District_Name_Households<- "Chikballapura"
KarVillages[KarVillages$District_Name_Households=="Chamarajanagar",]$District_Name_Households<- "Chamrajnagar"

KarnatakaVillage_XT <-KarVillages[,c("District_Name_Households","ComputerAccess","MobilePenetration","InternetAccess","Landline","No_Households")]
KarSubdistLevel_Village <- aggregate(. ~ District_Name_Households,data=KarnatakaVillage_XT,FUN="sum")
KarSubdistLevel_Village$PercentComputerAccess <- KarSubdistLevel_Village$ComputerAccess/KarSubdistLevel_Village$No_Households * 100
KarSubdistLevel_Village$PercentMobPenetration <- KarSubdistLevel_Village$MobilePenetration/KarSubdistLevel_Village$No_Households * 100
KarSubdistLevel_Village$PercentInternetAccess <- KarSubdistLevel_Village$InternetAccess/KarSubdistLevel_Village$No_Households * 100
KarSubdistLevel_Village$PercentLandline <- KarSubdistLevel_Village$Landline/KarSubdistLevel_Village$No_Households * 100

karnatakavillagemap <- merge(karnatakamap,KarSubdistLevel_Village,by.x = "NAME_2",by.y="District_Name_Households",all.x=TRUE)

jpeg(filename = paste(path,"//Plots//Maps//KarVill_CompAccess.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(karnatakavillagemap,c("PercentComputerAccess"),"Karnataka Villages - Computer Access % by Districts","CA")
dev.off()

jpeg(filename = paste(path,"//Plots//Maps//KarVill_MobPen.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(karnatakavillagemap,c("PercentMobPenetration"),"Karnataka Villages - Mobile Penetration % by Districts","MP")
dev.off()

jpeg(filename = paste(path,"//Plots//Maps//KarVill_Internet.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(karnatakavillagemap,c("PercentInternetAccess"),"Karnataka Villages - Internet Access % by Districts","IA")
dev.off()

jpeg(filename = paste(path,"//Plots//Maps//KarVill_Landline.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(karnatakavillagemap,c("PercentLandline"),"Karnataka Villages - Landline Penetration % by Districts","LP")
dev.off()

# Karnataka Towns
KarTowns$District_Name_Households <- trimws(KarTowns$District_Name_Households)
KarTowns[KarTowns$District_Name_Households=="Chikkaballapura",]$District_Name_Households<- "Chikballapura"
KarTowns[KarTowns$District_Name_Households=="Chamarajanagar",]$District_Name_Households<- "Chamrajnagar"

KarnatakaTown_XT <-KarTowns[,c("District_Name_Households","ComputerAccess","MobilePenetration","InternetAccess","Landline","No_Households")]
KarSubdistLevel_Town <- aggregate(. ~ District_Name_Households,data=KarnatakaTown_XT,FUN="sum")
KarSubdistLevel_Town$PercentComputerAccess <- KarSubdistLevel_Town$ComputerAccess/KarSubdistLevel_Town$No_Households * 100
KarSubdistLevel_Town$PercentMobPenetration <- KarSubdistLevel_Town$MobilePenetration/KarSubdistLevel_Town$No_Households * 100
KarSubdistLevel_Town$PercentInternetAccess <- KarSubdistLevel_Town$InternetAccess/KarSubdistLevel_Town$No_Households * 100
KarSubdistLevel_Town$PercentLandline <- KarSubdistLevel_Town$Landline/KarSubdistLevel_Town$No_Households * 100

karnatakatownmap <- merge(karnatakamap,KarSubdistLevel_Town,by.x = "NAME_2",by.y="District_Name_Households",all.x=TRUE)

jpeg(filename = paste(path,"//Plots//Maps//KarTown_CompAccess.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(karnatakatownmap,c("PercentComputerAccess"),"Karnataka Towns - Computer Access % by Districts","CA")
dev.off()

jpeg(filename = paste(path,"//Plots//Maps//KarTown_MobPen.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(karnatakatownmap,c("PercentMobPenetration"),"Karnataka Towns - Mobile Penetration % by Districts","MP")
dev.off()

jpeg(filename = paste(path,"//Plots//Maps//KarTown_Internet.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(karnatakatownmap,c("PercentInternetAccess"),"Karnataka Towns - Internet Access % by Districts","IA")
dev.off()

jpeg(filename = paste(path,"//Plots//Maps//KarTown_Landline.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(karnatakatownmap,c("PercentLandline"),"Karnataka Towns - Landline Penetration % by Districts","LP")
dev.off()

# AP Villages

APVillages$District_Name_Households <- trimws(APVillages$District_Name_Households)
APVillages[APVillages$District_Name_Households == "Rangareddy",]$District_Name_Households = "Ranga Reddy"
APVillages[APVillages$District_Name_Households == "Sri Potti Sriramulu Nellore",]$District_Name_Households = "Nellore"

APVillage_XT <-APVillages[,c("District_Name_Households","ComputerAccess","MobilePenetration","InternetAccess","Landline","No_Households")]
APSubdistLevel_Village <- aggregate(. ~ District_Name_Households,data=APVillage_XT,FUN="sum")
APSubdistLevel_Village$PercentComputerAccess <- APSubdistLevel_Village$ComputerAccess/APSubdistLevel_Village$No_Households * 100
APSubdistLevel_Village$PercentMobPenetration <- APSubdistLevel_Village$MobilePenetration/APSubdistLevel_Village$No_Households * 100
APSubdistLevel_Village$PercentInternetAccess <- APSubdistLevel_Village$InternetAccess/APSubdistLevel_Village$No_Households * 100
APSubdistLevel_Village$PercentLandline <- APSubdistLevel_Village$Landline/APSubdistLevel_Village$No_Households * 100

APvillagemap <- merge(APmap,APSubdistLevel_Village,by.x = "NAME_2",by.y="District_Name_Households",all.x=TRUE)

jpeg(filename = paste(path,"//Plots//Maps//APVill_CompAccess.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(APvillagemap,c("PercentComputerAccess"),"AP Villages - Computer Access % by Districts","CA")
dev.off()

jpeg(filename = paste(path,"//Plots//Maps//APVill_MobPen.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(APvillagemap,c("PercentMobPenetration"),"AP Villages - Mobile Penetration % by Districts","MP")
dev.off()

jpeg(filename = paste(path,"//Plots//Maps//APVill_Internet.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(APvillagemap,c("PercentInternetAccess"),"AP Villages - Internet Access % by Districts","IA")
dev.off()

jpeg(filename = paste(path,"//Plots//Maps//APVill_Landline.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(APvillagemap,c("PercentLandline"),"AP Villages - Landline Penetration % by Districts","LP")
dev.off()



# AP Towns
APTowns$District_Name_Households <- trimws(APTowns$District_Name_Households)
APTowns[APTowns$District_Name_Households == "Rangareddy",]$District_Name_Households = "Ranga Reddy"
APTowns[APTowns$District_Name_Households == "Sri Potti Sriramulu Nellore",]$District_Name_Households = "Nellore"

APTown_XT <-APTowns[,c("District_Name_Households","ComputerAccess","MobilePenetration","InternetAccess","Landline","No_Households")]
APSubdistLevel_Town <- aggregate(. ~ District_Name_Households,data=APTown_XT,FUN="sum")
APSubdistLevel_Town$PercentComputerAccess <- APSubdistLevel_Town$ComputerAccess/APSubdistLevel_Town$No_Households * 100
APSubdistLevel_Town$PercentMobPenetration <- APSubdistLevel_Town$MobilePenetration/APSubdistLevel_Town$No_Households * 100
APSubdistLevel_Town$PercentInternetAccess <- APSubdistLevel_Town$InternetAccess/APSubdistLevel_Town$No_Households * 100
APSubdistLevel_Town$PercentLandline <- APSubdistLevel_Town$Landline/APSubdistLevel_Town$No_Households * 100

APtownmap <- merge(APmap,APSubdistLevel_Town,by.x = "NAME_2",by.y="District_Name_Households",all.x=TRUE)

jpeg(filename = paste(path,"//Plots//Maps//APTown_CompAccess.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(APtownmap,c("PercentComputerAccess"),"AP Towns - Computer Access % by Districts","CA")
dev.off()

jpeg(filename = paste(path,"//Plots//Maps//APTown_MobPen.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(APtownmap,c("PercentMobPenetration"),"AP Towns - Mobile Penetration % by Districts","MP")
dev.off()

jpeg(filename = paste(path,"//Plots//Maps//APTown_Internet.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(APtownmap,c("PercentInternetAccess"),"AP Towns - Internet Access % by Districts","IA")
dev.off()

jpeg(filename = paste(path,"//Plots//Maps//APTown_Landline.jpg",sep=""),width = 600, height = 700,quality=400,res=100)
plot_map(APtownmap,c("PercentLandline"),"AP Towns - Landline Penetration % by Districts","LP")
dev.off()

# Aggregating State Level Numbers

Town_XT <-all_towns[,c("State_Name_Households","District_Name_Households","ComputerAccess","MobilePenetration","InternetAccess","Landline","No_Households")]
SubdistLevel_Town <- aggregate(. ~ District_Name_Households + State_Name_Households ,data=Town_XT,FUN="sum")
SubdistLevel_Town$Location <- "Urban"

Village_XT <-all_villages[,c("State_Name_Households","District_Name_Households","ComputerAccess","MobilePenetration","InternetAccess","Landline","No_Households")]
SubdistLevel_Village <- aggregate(. ~ District_Name_Households + State_Name_Households ,data=Village_XT,FUN="sum")
SubdistLevel_Village$Location <- "Rural"

District_Consolidated <- rbind(SubdistLevel_Town,SubdistLevel_Village)
District_Consolidated$ComputerAccess_pct <- round(District_Consolidated$ComputerAccess/District_Consolidated$No_Households*100,2)
District_Consolidated$MobilePenetration_pct <- round(District_Consolidated$MobilePenetration/District_Consolidated$No_Households*100,2)
District_Consolidated$InternetAccess_pct <- round(District_Consolidated$InternetAccess/District_Consolidated$No_Households*100,2)
District_Consolidated$Landline_pct <- round(District_Consolidated$Landline/District_Consolidated$No_Households*100,2)

write.csv(District_Consolidated,"District_Consolidated.csv",row.names = FALSE)

#
Town_XT <-all_towns[,c("State_Name_Households","District_Name_Households","Tehsil_Name_Households","Town_Village_Households","Area_Name","ComputerAccess","MobilePenetration","InternetAccess","Landline","No_Households")]
Town_XT$Location <- "Urban"
Village_XT <-all_villages[,c("State_Name_Households","District_Name_Households","Tehsil_Name_Households","Town_Village_Households","Area_Name","ComputerAccess","MobilePenetration","InternetAccess","Landline","No_Households")]
Village_XT$Location <- "Rural"

All_Consolidated <- rbind(Town_XT,Village_XT)
All_Consolidated$ComputerAccess_pct <- round(All_Consolidated$ComputerAccess/All_Consolidated$No_Households*100,2)
All_Consolidated$MobilePenetration_pct <- round(All_Consolidated$MobilePenetration/All_Consolidated$No_Households*100,2)
All_Consolidated$InternetAccess_pct <- round(All_Consolidated$InternetAccess/All_Consolidated$No_Households*100,2)
All_Consolidated$Landline_pct <- round(All_Consolidated$Landline/All_Consolidated$No_Households*100,2)

write.csv(All_Consolidated,"DigitalInclusion_Consolidated.csv",row.names = FALSE)
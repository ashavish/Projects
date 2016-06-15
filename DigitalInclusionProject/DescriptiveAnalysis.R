## Karnataka Villages
# Quick Stats
nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA",])
sum(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$No_Households)
sum(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$ComputerAccess)
sum(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$InternetAccess)
sum(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$MobilePenetration)
sum(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$Landline)

# Computer Access %
summary(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$ComputerAccess_pct)
sd(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$ComputerAccess_pct)

nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA" & all_villages$ComputerAccess_pct==100, ])
nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA" & all_villages$ComputerAccess_pct>75, ])

all_villages[all_villages$State_Name_Households=="KARNATAKA" & all_villages$ComputerAccess_pct==100, ]$Area_Name
nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA" & all_villages$ComputerAccess_pct > 4.60,])/nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA",])*100
ggplot(all_villages[all_villages$State_Name_Households=="KARNATAKA",],aes(ComputerAccess_pct)) + geom_histogram() + labs(title="Computer Access % - Karnataka Villages",x = "Computer Access %", y="Count" )

nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA" & all_villages$ComputerAccess_pct>25,])

# Internet Access %
summary(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$InternetAccess_pct)
sd(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$InternetAccess_pct)

table(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$Internet_Cafes__Common_Service_Centre_CSC_Status_A1_NA2)/nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA",])* 100
table(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$Nearest_Internet_CSC_Distance_Code)

sum(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$InternetAccess)/sum(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$ComputerAccess)*100

ggplot(all_villages[all_villages$State_Name_Households=="KARNATAKA",],aes(InternetAccess_pct)) + geom_histogram() + labs(title="Internet Access % - Karnataka Villages",x = "Internet Access %", y="Count" )

nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA" & all_villages$InternetAccess_pct>10,])
quantile(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$InternetAccess_pct, c(.05, .95))
nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA" & all_villages$InternetAccess_pct<0,])
nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA" & all_villages$InternetAccess_pct>2.5,])

# Mobile Penetration %
summary(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$MobilePenetration_pct)
sd(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$MobilePenetration_pct)

all_villages$Nearest_telephone_landlines_Distance_Code
ggplot(all_villages[all_villages$State_Name_Households=="KARNATAKA",],aes(MobilePenetration_pct)) + geom_histogram() + labs(title="Mobile Penetration % - Karnataka Villages",x = "Mobile Penetration %", y="Count" )

table(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$Mobile_Phone_Coverage_Status_A1_NA2)/nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA",])
# Landline %

summary(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$Landline_pct)
sd(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$Landline_pct)
nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA" & all_villages$Landline_pct == 100,])

table(all_villages[all_villages$State_Name_Households=="KARNATAKA",]$Telephone_landlines_Status_A1_NA2)/nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA",])

unique(all_villages[all_villages$Telephone_landlines_Status_A1_NA2==2,]$Nearest_telephone_landlines_Distance_Code)
ggplot(all_villages[all_villages$State_Name_Households=="KARNATAKA",],aes(Landline_pct)) + geom_histogram() + labs(title="Landline % - Karnataka Villages",x = "Landline %", y="Count" )

nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA" & all_villages$Landline_pct>75,])

# Cross checking the 1360 villages which all high percentages 

nrow(all_villages[all_villages$State_Name_Households=="KARNATAKA" ,])



## Karnataka Towns
# Quick Stats
nrow(all_towns[all_towns$State_Name_Households=="KARNATAKA",])
sum(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$No_Households)
sum(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$ComputerAccess)
sum(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$InternetAccess)
sum(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$MobilePenetration)
sum(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$Landline)

# Computer Access %
summary(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$ComputerAccess_pct)
sd(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$ComputerAccess_pct)

nrow(all_towns[all_towns$State_Name_Households=="KARNATAKA" & all_towns$ComputerAccess_pct==100, ])
nrow(all_towns[all_towns$State_Name_Households=="KARNATAKA" & all_towns$ComputerAccess_pct>20, ])

all_towns[all_towns$State_Name_Households=="KARNATAKA" & all_towns$ComputerAccess_pct==100, ]$Area_Name
nrow(all_towns[all_towns$State_Name_Households=="KARNATAKA" & all_towns$ComputerAccess_pct > 4.60,])/nrow(all_towns[all_towns$State_Name_Households=="KARNATAKA",])*100
ggplot(all_towns[all_towns$State_Name_Households=="KARNATAKA",],aes(ComputerAccess_pct)) + geom_histogram() + labs(title="Computer Access % - Karnataka Towns",x = "Computer Access %", y="Count" )

nrow(all_towns[all_towns$State_Name_Households=="KARNATAKA" & all_towns$ComputerAccess_pct>25,])

# Internet Access %
summary(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$InternetAccess_pct)
sd(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$InternetAccess_pct)


sum(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$InternetAccess)/sum(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$ComputerAccess)*100

ggplot(all_towns[all_towns$State_Name_Households=="KARNATAKA",],aes(InternetAccess_pct)) + geom_histogram() + labs(title="Internet Access % - Karnataka Towns",x = "Internet Access %", y="Count" )

nrow(all_towns[all_towns$State_Name_Households=="KARNATAKA" & all_towns$InternetAccess_pct>7,])
nrow(all_towns[all_towns$State_Name_Households=="KARNATAKA" & all_towns$InternetAccess_pct>2.5,])

# Mobile Penetration %
summary(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$MobilePenetration_pct)
sd(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$MobilePenetration_pct)


ggplot(all_towns[all_towns$State_Name_Households=="KARNATAKA",],aes(MobilePenetration_pct)) + geom_histogram() + labs(title="Mobile Penetration % - Karnataka Towns",x = "Mobile Penetration %", y="Count" )

table(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$Mobile_Phone_Coverage_Status_A1_NA2)/nrow(all_towns[all_towns$State_Name_Households=="KARNATAKA",])
nrow(all_towns[all_towns$State_Name_Households=="KARNATAKA" & all_towns$MobilePenetration_pct<55,])
# Landline %

summary(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$Landline_pct)
sd(all_towns[all_towns$State_Name_Households=="KARNATAKA",]$Landline_pct)
nrow(all_towns[all_towns$State_Name_Households=="KARNATAKA" & all_towns$Landline_pct == 100,])
nrow(all_towns[all_towns$State_Name_Households=="KARNATAKA" & all_towns$Landline_pct>30,])

unique(all_towns[all_towns$Telephone_landlines_Status_A1_NA2==2,]$Nearest_telephone_landlines_Distance_Code)
ggplot(all_towns[all_towns$State_Name_Households=="KARNATAKA",],aes(Landline_pct)) + geom_histogram() + labs(title="Landline % - Karnataka Towns",x = "Landline %", y="Count" )

nrow(all_towns[all_towns$State_Name_Households=="KARNATAKA" & all_towns$Landline_pct>75,])

# Cross checking the 1360 Towns which all high percentages 

nrow(all_towns[all_towns$State_Name_Households=="KARNATAKA" ,])


## Andhra Pradesh Villages
# Quick Stats
nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",])
sum(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$No_Households)
sum(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$ComputerAccess)
sum(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$InternetAccess)
sum(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$MobilePenetration)
sum(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$Landline)

# Computer Access %
summary(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$ComputerAccess_pct)
sd(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$ComputerAccess_pct)

nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$ComputerAccess_pct==100, ])
nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$ComputerAccess_pct>20, ])

all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$ComputerAccess_pct==100, ]$Area_Name
nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$ComputerAccess_pct > 4.60,])/nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",])*100
ggplot(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",],aes(ComputerAccess_pct)) + geom_histogram() + labs(title="Computer Access % - AP Villages",x = "Computer Access %", y="Count" )

nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$ComputerAccess_pct>25,])

# Internet Access %
summary(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$InternetAccess_pct)
sd(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$InternetAccess_pct)

nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$InternetAccess_pct>5,])
nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$InternetAccess_pct==100,])

table(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$Internet_Cafes__Common_Service_Centre_CSC_Status_A1_NA2)/nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",])* 100
table(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$Nearest_Internet_CSC_Distance_Code)

sum(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$InternetAccess)/sum(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$ComputerAccess)*100

ggplot(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",],aes(InternetAccess_pct)) + geom_histogram() + labs(title="Internet Access % - AP Villages",x = "Internet Access %", y="Count" )

nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$InternetAccess_pct>10,])
quantile(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$InternetAccess_pct, c(.05, .95))
nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$InternetAccess_pct<0,])
nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$InternetAccess_pct>2.5,])

# Mobile Penetration %
summary(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$MobilePenetration_pct)
sd(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$MobilePenetration_pct)

all_villages$Nearest_telephone_landlines_Distance_Code
ggplot(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",],aes(MobilePenetration_pct)) + geom_histogram() + labs(title="Mobile Penetration % - AP Villages",x = "Mobile Penetration %", y="Count" )

nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$MobilePenetration_pct==0,])
nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$MobilePenetration_pct<10,])
table(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$Mobile_Phone_Coverage_Status_A1_NA2)/nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",])
# Landline %

summary(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$Landline_pct)
sd(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$Landline_pct)
nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$Landline_pct == 100,])
nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$Landline_pct > 25,])
table(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$Telephone_landlines_Status_A1_NA2)/nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",])*100

table(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",]$Nearest_telephone_landlines_Distance_Code)

ggplot(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH",],aes(Landline_pct)) + geom_histogram() + labs(title="Landline % - AP Villages",x = "Landline %", y="Count" )

nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" & all_villages$Landline_pct>75,])

# Cross checking the 1360 villages which all high percentages 

nrow(all_villages[all_villages$State_Name_Households=="ANDHRA PRADESH" ,])



## Andhra Towns
# Quick Stats
nrow(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",])
sum(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$No_Households)
sum(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$ComputerAccess)
sum(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$InternetAccess)
sum(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$MobilePenetration)
sum(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$Landline)

# Computer Access %
summary(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$ComputerAccess_pct)
sd(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$ComputerAccess_pct)

nrow(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH" & all_towns$ComputerAccess_pct==100, ])
nrow(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH" & all_towns$ComputerAccess_pct>20, ])

all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH" & all_towns$ComputerAccess_pct==100, ]$Area_Name
nrow(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH" & all_towns$ComputerAccess_pct > 4.60,])/nrow(all_towns[all_towns$State_Name_Households=="Andhra Pradesh",])*100
ggplot(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",],aes(ComputerAccess_pct)) + geom_histogram() + labs(title="Computer Access % - AP Towns",x = "Computer Access %", y="Count" )

nrow(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH" & all_towns$ComputerAccess_pct>25,])

# Internet Access %
summary(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$InternetAccess_pct)
sd(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$InternetAccess_pct)


sum(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$InternetAccess)/sum(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$ComputerAccess)*100

ggplot(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",],aes(InternetAccess_pct)) + geom_histogram() + labs(title="Internet Access % - AP Towns",x = "Internet Access %", y="Count" )

nrow(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH" & all_towns$InternetAccess_pct>7,])
nrow(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH" & all_towns$InternetAccess_pct>2.5,])

# Mobile Penetration %
summary(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$MobilePenetration_pct)
sd(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$MobilePenetration_pct)


ggplot(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",],aes(MobilePenetration_pct)) + geom_histogram() + labs(title="Mobile Penetration % - AP Towns",x = "Mobile Penetration %", y="Count" )

nrow(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH" & all_towns$MobilePenetration_pct<50,])

# Landline %

summary(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$Landline_pct)
sd(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",]$Landline_pct)
nrow(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH" & all_towns$Landline_pct == 100,])
nrow(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH" & all_towns$Landline_pct>20,])

unique(all_towns[all_towns$Telephone_landlines_Status_A1_NA2==2,]$Nearest_telephone_landlines_Distance_Code)
ggplot(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH",],aes(Landline_pct)) + geom_histogram() + labs(title="Landline % - AP Towns",x = "Landline %", y="Count" )

nrow(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH" & all_towns$Landline_pct>75,])

# Cross checking the 1360 Towns which all high percentages 

nrow(all_towns[all_towns$State_Name_Households=="ANDHRA PRADESH" ,])

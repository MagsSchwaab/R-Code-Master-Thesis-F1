##----------Loading csv Data----------##
penalty_df <- read.csv2("__Generic_Recipe___Get_Table_Data__91723582.csv", sep = ",", header=T, na.strings=c("","NA"))
races_df <- read.csv2("races.csv", sep = ",")
constructors_df <- read.csv2("constructors.csv", sep = ",")
circuits_df <- read.csv2("circuits.csv", sep = ",")
drivers_df <- read.csv2("drivers.csv", sep = ",")
laptimes_df <- read.csv2("lap_times.csv", sep = ",")
pitstops_df <- read.csv2("pit_stops.csv", sep = ",")
qualifying_df <- read.csv2("qualifying.csv", sep = ",")
races_df <- read.csv2("races.csv", sep = ",")
results_df <- read.csv2("results.csv", sep = ",")
seasons_df <- read.csv2("seasons.csv", sep = ",")
status_df <- read.csv2("status.csv", sep = ",")
rrdcl_df <- read.csv("rrdcl_df.csv")
penalty_df <- read.csv("penalty_df_clean.csv")
F1_full_2019_df <- read.csv("F1_full_2019_df.csv")

##----------Saving Data-frames----------## 
write.csv(rrd_df,"rrd_df.csv", row.names = T)
write.csv(rr_df,"rr_df.csv", row.names = T)
write.csv(drivers_df,"drivers_df_clean.csv", row.names = T)
write.csv(races_df,"races_df_clean.csv", row.names = T)
write.csv(penalty_df,"penalty_df_clean.csv", row.names = T)
write.csv(rrdclp_df,"rrdclp_df.csv", row.names = T)
write.csv(rrdc_df,"rrdc_df.csv", row.names = T)
write.csv(penaltyAT_df,"penaltyAT_df.csv", row.names = T)
write.csv(F1_full_2019_df,"F1_full_2019_df.csv", row.names = T)

##----------Loading Packages/Libraries----------##
library(tidyverse)


##----------Data Cleaning and Preparation----------##
## Dropping unnecessary columns from penalty data-frame
penalty_df = subset(penalty_df, select = -c(Column.6, Column.8, Column.9, Column.10, Column.11, Column.12, 
                                            Column.13, Column.14, Column.15, Column.16, Column.17, 
                                            Column.18, Column.19, Column.20))

## Dropping empty row 
penalty_df[rowSums(is.na(penalty_df)) != ncol(penalty_df),]

## Removing first row (empty) 
penalty_df = penalty_df[-1,]

## Renaming columns 
colnames(penalty_df)
names(penalty_df)[names(penalty_df) == "Column.1"] <- "raceId"
names(penalty_df)[names(penalty_df) == "Column.2"] <- "driverId"
names(penalty_df)[names(penalty_df) == "Column.3"] <- "constructorId"
names(penalty_df)[names(penalty_df) == "Column.4"] <- "session"
names(penalty_df)[names(penalty_df) == "Column.5"] <- "penalty"
names(penalty_df)[names(penalty_df) == "Column.7"] <- "reason"

## Replacing driver name with driverId to be able to merge 
penalty_df$driverId[penalty_df$driverId == "Sergio Perez"] <- "815"
penalty_df$driverId[penalty_df$driverId == "Lando Norris"] <- "846"
penalty_df$driverId[penalty_df$driverId == "Kevin Magnussen"] <- "825"
penalty_df$driverId[penalty_df$driverId == "Nico Hulkenberg"] <- "807"
penalty_df$driverId[penalty_df$driverId == "Romain Grosjean"] <- "154"
penalty_df$driverId[penalty_df$driverId == "Sebastian Vettel"] <- "20"
penalty_df$driverId[penalty_df$driverId == "Carlos Sainz Jnr"] <- "832"
penalty_df$driverId[penalty_df$driverId == "Max Verstappen"] <- "830"
penalty_df$driverId[penalty_df$driverId == "Daniil Kvyat"] <- "826"
penalty_df$driverId[penalty_df$driverId == "Antonio Giovinazzi"] <- "841"
penalty_df$driverId[penalty_df$driverId == "Daniel Ricciardo"] <- "817"
penalty_df$driverId[penalty_df$driverId == "Alexander Albon"] <- "848"
penalty_df$driverId[penalty_df$driverId == "Pierre Gasly"] <- "842"
penalty_df$driverId[penalty_df$driverId == "Kimi Raikkonen"] <- "8"
penalty_df$driverId[penalty_df$driverId == "Robert Kubica"] <- "9"
penalty_df$driverId[penalty_df$driverId == "George Russell"] <- "847"
penalty_df$driverId[penalty_df$driverId == "Lance Stroll"] <- "840"
penalty_df$driverId[penalty_df$driverId == "Charles Leclerc"] <- "844"
penalty_df$driverId[penalty_df$driverId == "Lewis Hamilton"] <- "1"
penalty_df$driverId[is.na(penalty_df$driverId)] <- "825" 

## Replacing RaceId name with number 
penalty_df$raceId[penalty_df$raceId == "02. Bahrain"] <- "1011"
penalty_df$raceId[penalty_df$raceId == "03. China"] <- "1012"
penalty_df$raceId[penalty_df$raceId == "04. Azerbaijan"] <- "1013"
penalty_df$raceId[penalty_df$raceId == "05. Spain"] <- "1014"
penalty_df$raceId[penalty_df$raceId == "06. Monaco"] <- "1015"
penalty_df$raceId[penalty_df$raceId == "07. Canada"] <- "1016"
penalty_df$raceId[penalty_df$raceId == "08. France"] <- "1017"
penalty_df$raceId[penalty_df$raceId == "09. Austria"] <- "1018"
penalty_df$raceId[penalty_df$raceId == "10. Great Britain"] <- "1019"
penalty_df$raceId[penalty_df$raceId == "11. Germany"] <- "1020"
penalty_df$raceId[penalty_df$raceId == "13. Belgium"] <- "1022"
penalty_df$raceId[penalty_df$raceId == "14. Italy"] <- "1023"
penalty_df$raceId[penalty_df$raceId == "15. Singapore"] <- "1024"
penalty_df$raceId[penalty_df$raceId == "16. Russia"] <- "1025"
penalty_df$raceId[penalty_df$raceId == "17. Japan"] <- "1026"

## Dropping penalties from qualifying and practice sessions
penalty_df <- penalty_df[penalty_df$session != "Q", ] 
penalty_df <- penalty_df[penalty_df$session != "P1", ]
penalty_df <- penalty_df[penalty_df$session != "P2", ]
penalty_df <- penalty_df[penalty_df$session != "P3", ]

## Examining Data 
str(penalty_df)

## Dropping columns from races_df, drivers_df, constructors_df 
races_df = subset(races_df, select = -c(date, time, url))
drivers_df = subset(drivers_df, select = -c(code, forename, surname, url, dob, nationality))
constructors_df = subset(constructors_df, select = -c(nationality, url, name))
pitstops_df = subset(pitstops_df, select = -c(time))
rrdclp_df = subset(rrdclp_df, select = -c(X, positionOrder))
penalty_df = subset(penalty_df, select = -c(X, constructorId, session))

## Dropping rows from penatly_df where no penalty was given 
penaltyAT_df <- penalty_df[penalty_df$reason != "No action taken", ]

## Merging  data frames 
rr_df <- merge(races_df, results_df, by.x="raceId", by.y="raceId", all = T)
rrd_df <- merge(rr_df, drivers_df, by.x= c("number"), by.y= c("number"), all = T)
rrdc_df <- merge(rrd_df, constructors_df, by.x= c("constructorId"), by.y= c("constructorId"), all = T)
rrdcl_df <- merge(rrdc_df, laptimes_df, by.x= c("driverId.x", "raceId"), by.y= c("driverId", "raceId"), all = T)
rrdclp_df <- merge(rrdcl_df, pitstops_df, by.x= c("driverId.x", "raceId", "lap"), by.y= c("driverId", "raceId", "lap"), all = T)

## Renaming columns rrd_df 
colnames(rrdclp_df)
names(rrdclp_df)[names(rrdclp_df) == "race.time..t5."] <- "RaceTimeT5" # Only race time for top 5 (t5) is available 
names(rrdclp_df)[names(rrdclp_df) == "race.time.in.milliseconds..t5."] <- "RaceTimeMillisecondsT5"
names(rrdclp_df)[names(rrdclp_df) == "position"] <- "positionPerLap"
names(rrdclp_df)[names(rrdclp_df) == "Grid.Position.Start"] <- "gridPos"
names(rrdclp_df)[names(rrdclp_df) == "Finishing.Position"] <- "finishingPos"
names(rrdclp_df)[names(rrdclp_df) == "total.laps"] <- "totalLaps"
names(rrdclp_df)[names(rrdclp_df) == "milliseconds.x"] <- "laptimeMilliseconds"
names(rrdclp_df)[names(rrdclp_df) == "milliseconds.y"] <- "PStimeMilliseconds"
names(rrdclp_df)[names(rrdclp_df) == "time"] <- "timePerLap"
names(rrdclp_df)[names(rrdclp_df) == "number"] <- "driverNo"
names(rrdclp_df)[names(rrdclp_df) == "Grand.Prix.Name"] <- "GPName"
names(rrdclp_df)[names(rrdclp_df) == "compRound"] <- "competitionRound"
names(rrdclp_df)[names(rrdclp_df) == "duration"] <- "PSDuration"

## Reordering dataframes 
rrdclp_df <- rrdclp_df[, c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 3, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 )] # reordering columngs 
rrdc_df <-rrdc_df[order(rrdc_df$round),] # making "round" variable ascending


## Filter for specific races only (2019)
rrdclp_df <- rrdclp_df[rrdclp_df$raceId %in% c("1010", "1011", "1012","1013", "1014", "1015", "1016","1017", "1018", "1019", "1020","1021", 
                                               "1022", "1023", "1024","1025", "1026", "1027", "1028","1029", "1030"), ]

## One more df 
rrdclpp_df <- merge(rrdclp_df, penalty_df, by.x= c("raceId", "driverId.x"), by.y= c("raceId", "driverId"), all = T)

## Filtering df to be only from 2019
races_df <- races_df %>% filter(year == "2019")
rr_df_19 <- rr_df %>% filter(year == "2019")
rrd_df_19 <- rrd_df %>% filter(year == "2019")
rrdc_df_19 <- rrdc_df %>% filter(year == "2019")
rrdcl_df_19 <- rrdcl_df %>% filter(year == "2019")
rrdclp_df_19 <- rrdclp_df %>% filter(year == "2019")
rrdclpp_df_19 <- rrdclpp_df %>% filter(year == "2019")

## Filter specific race
rrdclpp_df_19_Aus <- rrdclpp_df_19 %>% filter(raceId == "1010")
rrdclpp_df_19_Bah <- rrdclpp_df_19 %>% filter(raceId == "1011")
rrdclpp_df_19_Mon <- rrdclpp_df_19 %>% filter(raceId == "1015")
rrdclpp_df_19_Brit <- rrdclpp_df_19 %>% filter(raceId == "1019")

## Delete old drivers 
rrdclpp_df_19_Aus <- rrdclpp_df_19_Aus[rrdclpp_df_19_Aus$driverRef != "chilton", ]
rrdclpp_df_19_Aus <- rrdclpp_df_19_Aus[rrdclpp_df_19_Aus$driverRef != "kobayashi", ]
rrdclpp_df_19_Aus <- rrdclpp_df_19_Aus[rrdclpp_df_19_Aus$driverRef != "haryanto", ]
rrdclpp_df_19_Aus <- rrdclpp_df_19_Aus[rrdclpp_df_19_Aus$driverRef != "sutil", ]

## Split up race in below point threshold and above 
rrdclpp_df_19_Aus_np <- rrdclpp_df_19_Aus %>% filter(points == "0")
rrdclpp_df_19_Aus_p <- rrdclpp_df_19_Aus %>% filter(points > "0")

## Filter for lap 
rrdclpp_df_19_Aus_p_l1 <- rrdclpp_df_19_Aus_p %>% filter(lap == "1") %>% arrange(time.y)
rrdclpp_df_19_Aus_l1 <- rrdclpp_df_19_Aus %>% filter(lap == "1")
rrdclpp_df_19_Aus_l2 <- rrdclpp_df_19_Aus %>% filter(lap == "2")

## Arrange by lap position
rrdclpp_df_19_Aus_l1 <- rrdclpp_df_19_Aus_l1 %>% arrange(position.y)

## Filter specific driver
rrdclpp_df_19_Aus_HAM <- rrdclpp_df_19_Aus %>% filter(number == "44")
rrdclpp_df_19_Aus_KUB <- rrdclpp_df_19_Aus %>% filter(driverRef == "kubica") %>% arrange(lap)

##----------Variable Creation----------##
rrdclpp_df_19_Aus_l1$ProxComp <- ave(rrdclpp_df_19_Aus_l1$milliseconds.y, FUN = function(x) c(0, diff(x)))
# DeltTime = Full Dataframe: Difference of laptimes per driver 
rrdclpp_df_19_Aus_l1$DeltTime <- ave(rrdclpp_df_19_Aus_l1$milliseconds.y, FUN = function(x) c(0, diff(x)))#can't use in first lap
rrdclpp_df_19_Aus_l1$RewardDisp <- ave(rrdclpp_df_19_Aus_l1$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Aus_l1$IntRank <- ifelse(rrdclpp_df_19_Aus_l1$points > 0, 1, 0)
rrdclpp_df_19_Aus_l1$Retirement <- ifelse(rrdclpp_df_19_Aus_l1$positionText == "R", 1, 0)
rrdclpp_df_19_Aus_l1$Penalty <- ifelse(is.na(rrdclpp_df_19_Aus_l1$penalty), 0, 1)
rrdclpp_df_19_Aus_l1$AvgPitstop <- ifelse(rrdclpp_df_19_Aus_l1$duration > 23.80954, 1, 0)
rrdclpp_df_19_Aus_l1$PLG <- ave(rrdclpp_df_19_Aus_l1$position.y, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Aus_l1$Overtake <- ifelse(rrdclpp_df_19_Aus_l1$PLG <= -1, 1, 0)
rrdclpp_df_19_Aus_l1$PosLoss <- ifelse(rrdclpp_df_19_Aus_l1$PLG >= 1, 1, 0)

rrdclpp_df_19_Aus_l2$ProxComp <- ave(rrdclpp_df_19_Aus_l2$milliseconds.y, FUN = function(x) c(0, diff(x)))
# DeltTime = Full Dataframe: Difference of laptimes per driver 
rrdclpp_df_19_Aus_l2$DeltTime <- ave(rrdclpp_df_19_Aus_l2$milliseconds.y, FUN = function(x) c(0, diff(x)))#can't use in first lap
rrdclpp_df_19_Aus_l2$RewardDisp <- ave(rrdclpp_df_19_Aus_l2$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Aus_l2$IntRank <- ifelse(rrdclpp_df_19_Aus_l2$points > 0, 1, 0)
rrdclpp_df_19_Aus_l2$Retirement <- ifelse(rrdclpp_df_19_Aus_l2$positionText == "R", 1, 0)
rrdclpp_df_19_Aus_l2$Penalty <- ifelse(is.na(rrdclpp_df_19_Aus_l2$penalty), 0, 1)
rrdclpp_df_19_Aus_l2$AvgPitstop <- ifelse(rrdclpp_df_19_Aus_l2$duration > 23.80954, 1, 0)
rrdclpp_df_19_Aus_l2$PLG <- ave(rrdclpp_df_19_Aus_l2$position.y, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Aus_l2$Overtake <- ifelse(rrdclpp_df_19_Aus_l2$PLG <= -1, 1, 0)
rrdclpp_df_19_Aus_l2$PosLoss <- ifelse(rrdclpp_df_19_Aus_l2$PLG >= 1, 1, 0)

## Changing variable types
rrdclpp_df_19_Aus_l1$points <- as.numeric(rrdclpp_df_19_Aus_l1$points)
rrdclpp_df_19_Aus_l2$points <- as.numeric(rrdclpp_df_19_Aus_l2$points)
rrdclpp_df_19_Aus_l1$FinPos <- as.factor(rrdclpp_df_19_Aus_l1$FinPos)
rrdclpp_df_19_Aus_l1$Overtake <- as.numeric(rrdclpp_df_19_Aus_l1$Overtake)
rrdclpp_df_19_Aus_l1$Retirement <- as.numeric(rrdclpp_df_19_Aus_l1$Retirement)
rrdclpp_df_19_Aus_l1$IntRank <- as.numeric(rrdclpp_df_19_Aus_l1$IntRank)
rrdclpp_df_19_Aus_l1$AvgPitstop <- as.numeric(rrdclpp_df_19_Aus_l1$AvgPitstop)

## Reworking the main data frame 
## Examine
str(rrdclpp_df_19)

## Store in new frame just in case 
f1full_df <- rrdclpp_df_19

## Dropping unnecessary columns 
rrdclpp_df_19 = subset(rrdclpp_df_19, select = -c(driverId.y, resultId, positionOrder, constructorId.y, X, statusId, session, time, rank))

## Reordering columns 
names(rrdclpp_df_19)
rrdclpp_df_19 <- rrdclpp_df_19[, c(6, 7, 1, 14, 8, 9, 3, 2, 21, 5, 4, 22, 11, 12, 13, 18, 15, 16, 17, 19, 20, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32)]

## Searching for duplicates  
sum(duplicated(rrdclpp_df_19))

## Checking for/Deleting wrong drivers 
rrdclpp_df_19$driverRef
rrdclpp_df_19 <- rrdclpp_df_19[rrdclpp_df_19$driverRef != "chilton", ]
rrdclpp_df_19 <- rrdclpp_df_19[rrdclpp_df_19$driverRef != "kobayashi", ]
rrdclpp_df_19 <- rrdclpp_df_19[rrdclpp_df_19$driverRef != "haryanto", ]
rrdclpp_df_19 <- rrdclpp_df_19[rrdclpp_df_19$driverRef != "sutil", ]

## Renaming Columns 
colnames(rrdclpp_df_19)
names(rrdclpp_df_19)[names(rrdclpp_df_19) == "name"] <- "circuitName"
names(rrdclpp_df_19)[names(rrdclpp_df_19) == "laps"] <- "totalLaps"
names(rrdclpp_df_19)[names(rrdclpp_df_19) == "driverId.x"] <- "driverId"
names(rrdclpp_df_19)[names(rrdclpp_df_19) == "number"] <- "driverNum"
names(rrdclpp_df_19)[names(rrdclpp_df_19) == "constructorId.x"] <- "constructorId"
names(rrdclpp_df_19)[names(rrdclpp_df_19) == "position.x"] <- "finPos"
names(rrdclpp_df_19)[names(rrdclpp_df_19) == "time.x"] <- "finTime"
names(rrdclpp_df_19)[names(rrdclpp_df_19) == "milliseconds.x"] <- "finTimeMili"
names(rrdclpp_df_19)[names(rrdclpp_df_19) == "position.y"] <- "positionPerLap"
names(rrdclpp_df_19)[names(rrdclpp_df_19) == "time.y"] <- "lapTime"
names(rrdclpp_df_19)[names(rrdclpp_df_19) == "milliseconds.y"] <- "lapTimeMili"
names(rrdclpp_df_19)[names(rrdclpp_df_19) == "stop"] <- "pitstop"
names(rrdclpp_df_19)[names(rrdclpp_df_19) == "duration"] <- "pitstopTime"
names(rrdclpp_df_19)[names(rrdclpp_df_19) == "milliseconds"] <- "pitstopTimeMili"

## Changing variable types 
str(rrdclpp_df_19)

rrdclpp_df_19$finPos <- as.numeric(rrdclpp_df_19$finPos)
rrdclpp_df_19$points <- as.numeric(rrdclpp_df_19$points)
rrdclpp_df_19$positionPerLap <- as.numeric(rrdclpp_df_19$positionPerLap)
rrdclpp_df_19$lapTimeMili <- as.numeric(rrdclpp_df_19$lapTimeMili)
rrdclpp_df_19$finTimeMili <- as.numeric(rrdclpp_df_19$finTimeMili)
rrdclpp_df_19$pitstop <- as.numeric(rrdclpp_df_19$pitstop)
rrdclpp_df_19$pitstopTimeMili <- as.numeric(rrdclpp_df_19$pitstopTimeMili)
rrdclpp_df_19$pitstopTime <- as.numeric(rrdclpp_df_19$pitstopTime)
rrdclpp_df_19$penalty <- as.numeric(rrdclpp_df_19$penalty)

## Replacing NA#s with zeros 
rrdclpp_df_19[is.na(rrdclpp_df_19)] <- 0

## Chnage penalty column to be binary 
rrdclpp_df_19$penalty <- ifelse(rrdclpp_df_19$reason == 0, 0, 1)

## New data frame for each race 
unique(rrdclpp_df_19$circuitName)
unique(rrdclpp_df_19$raceId)

rrdclpp_df_19_Aus <- rrdclpp_df_19 %>% filter(raceId == "1010")
rrdclpp_df_19_Bah <- rrdclpp_df_19 %>% filter(raceId == "1011")
rrdclpp_df_19_Chi <- rrdclpp_df_19 %>% filter(raceId == "1012")
rrdclpp_df_19_Aze <- rrdclpp_df_19 %>% filter(raceId == "1013")
rrdclpp_df_19_Span <- rrdclpp_df_19 %>% filter(raceId == "1014")
rrdclpp_df_19_Mon <- rrdclpp_df_19 %>% filter(raceId == "1015")
rrdclpp_df_19_Can <- rrdclpp_df_19 %>% filter(raceId == "1016")
rrdclpp_df_19_Fre <- rrdclpp_df_19 %>% filter(raceId == "1017")
rrdclpp_df_19_Aust <- rrdclpp_df_19 %>% filter(raceId == "1018")
rrdclpp_df_19_Brit <- rrdclpp_df_19 %>% filter(raceId == "1019")
rrdclpp_df_19_Ger <- rrdclpp_df_19 %>% filter(raceId == "1020")
rrdclpp_df_19_Hun <- rrdclpp_df_19 %>% filter(raceId == "1021")
rrdclpp_df_19_Bel <- rrdclpp_df_19 %>% filter(raceId == "1022")
rrdclpp_df_19_Ita <- rrdclpp_df_19 %>% filter(raceId == "1023")
rrdclpp_df_19_Sin <- rrdclpp_df_19 %>% filter(raceId == "1024")
rrdclpp_df_19_Rus <- rrdclpp_df_19 %>% filter(raceId == "1025")
rrdclpp_df_19_Jap <- rrdclpp_df_19 %>% filter(raceId == "1026")
rrdclpp_df_19_Mex <- rrdclpp_df_19 %>% filter(raceId == "1027")
rrdclpp_df_19_USA <- rrdclpp_df_19 %>% filter(raceId == "1028")
rrdclpp_df_19_Bra <- rrdclpp_df_19 %>% filter(raceId == "1029")
rrdclpp_df_19_Abu <- rrdclpp_df_19 %>% filter(raceId == "1030")

##----------Variable Creation----------##
## Has to be done for every country 
#-----Australia-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Aus$DeltaTime <- ave(rrdclpp_df_19_Aus$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Aus$PLG <- ave(rrdclpp_df_19_Aus$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Aus <- rrdclpp_df_19_Aus %>%  arrange(positionPerLap)
rrdclpp_df_19_Aus <- rrdclpp_df_19_Aus %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Aus$ProxComp <- ave(rrdclpp_df_19_Aus$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Aus$RewardDisp <- ave(rrdclpp_df_19_Aus$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Aus$IntRank <- ifelse(rrdclpp_df_19_Aus$points > 0, 1, 0)
rrdclpp_df_19_Aus$Retirement <- ifelse(rrdclpp_df_19_Aus$positionText == "R", 1, 0)
rrdclpp_df_19_Aus$AvgPitstop <- ifelse(rrdclpp_df_19_Aus$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Aus$Overtake <- ifelse(rrdclpp_df_19_Aus$PLG <= -1, 1, 0)
rrdclpp_df_19_Aus$PosLoss <- ifelse(rrdclpp_df_19_Aus$PLG >= 1, 1, 0)

#-----Bahrain-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Bah$DeltaTime <- ave(rrdclpp_df_19_Bah$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Bah$PLG <- ave(rrdclpp_df_19_Bah$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Bah <- rrdclpp_df_19_Bah %>%  arrange(positionPerLap)
rrdclpp_df_19_Bah <- rrdclpp_df_19_Bah %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Bah$ProxComp <- ave(rrdclpp_df_19_Bah$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Bah$RewardDisp <- ave(rrdclpp_df_19_Bah$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Bah$IntRank <- ifelse(rrdclpp_df_19_Bah$points > 0, 1, 0)
rrdclpp_df_19_Bah$Retirement <- ifelse(rrdclpp_df_19_Bah$positionText == "R", 1, 0)
rrdclpp_df_19_Bah$AvgPitstop <- ifelse(rrdclpp_df_19_Bah$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Bah$Overtake <- ifelse(rrdclpp_df_19_Bah$PLG <= -1, 1, 0)
rrdclpp_df_19_Bah$PosLoss <- ifelse(rrdclpp_df_19_Bah$PLG >= 1, 1, 0)

#-----China-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Chi$DeltaTime <- ave(rrdclpp_df_19_Chi$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Chi$PLG <- ave(rrdclpp_df_19_Chi$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Chi <- rrdclpp_df_19_Chi %>%  arrange(positionPerLap)
rrdclpp_df_19_Chi <- rrdclpp_df_19_Chi %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Chi$ProxComp <- ave(rrdclpp_df_19_Chi$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Chi$RewardDisp <- ave(rrdclpp_df_19_Chi$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Chi$IntRank <- ifelse(rrdclpp_df_19_Chi$points > 0, 1, 0)
rrdclpp_df_19_Chi$Retirement <- ifelse(rrdclpp_df_19_Chi$positionText == "R", 1, 0)
rrdclpp_df_19_Chi$AvgPitstop <- ifelse(rrdclpp_df_19_Chi$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Chi$Overtake <- ifelse(rrdclpp_df_19_Chi$PLG <= -1, 1, 0)
rrdclpp_df_19_Chi$PosLoss <- ifelse(rrdclpp_df_19_Chi$PLG >= 1, 1, 0)

#-----Azerbaijan-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Aze$DeltaTime <- ave(rrdclpp_df_19_Aze$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Aze$PLG <- ave(rrdclpp_df_19_Aze$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Aze <- rrdclpp_df_19_Aze %>%  arrange(positionPerLap)
rrdclpp_df_19_Aze <- rrdclpp_df_19_Aze %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Aze$ProxComp <- ave(rrdclpp_df_19_Aze$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Aze$RewardDisp <- ave(rrdclpp_df_19_Aze$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Aze$IntRank <- ifelse(rrdclpp_df_19_Aze$points > 0, 1, 0)
rrdclpp_df_19_Aze$Retirement <- ifelse(rrdclpp_df_19_Aze$positionText == "R", 1, 0)
rrdclpp_df_19_Aze$AvgPitstop <- ifelse(rrdclpp_df_19_Aze$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Aze$Overtake <- ifelse(rrdclpp_df_19_Aze$PLG <= -1, 1, 0)
rrdclpp_df_19_Aze$PosLoss <- ifelse(rrdclpp_df_19_Aze$PLG >= 1, 1, 0)

#-----Spain-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Span$DeltaTime <- ave(rrdclpp_df_19_Span$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Span$PLG <- ave(rrdclpp_df_19_Span$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Span <- rrdclpp_df_19_Span %>%  arrange(positionPerLap)
rrdclpp_df_19_Span <- rrdclpp_df_19_Span %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Span$ProxComp <- ave(rrdclpp_df_19_Span$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Span$RewardDisp <- ave(rrdclpp_df_19_Span$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Span$IntRank <- ifelse(rrdclpp_df_19_Span$points > 0, 1, 0)
rrdclpp_df_19_Span$Retirement <- ifelse(rrdclpp_df_19_Span$positionText == "R", 1, 0)
rrdclpp_df_19_Span$AvgPitstop <- ifelse(rrdclpp_df_19_Span$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Span$Overtake <- ifelse(rrdclpp_df_19_Span$PLG <= -1, 1, 0)
rrdclpp_df_19_Span$PosLoss <- ifelse(rrdclpp_df_19_Span$PLG >= 1, 1, 0)

#-----Monaco-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Mon$DeltaTime <- ave(rrdclpp_df_19_Mon$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Mon$PLG <- ave(rrdclpp_df_19_Mon$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Mon <- rrdclpp_df_19_Mon %>%  arrange(positionPerLap)
rrdclpp_df_19_Mon <- rrdclpp_df_19_Mon %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Mon$ProxComp <- ave(rrdclpp_df_19_Mon$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Mon$RewardDisp <- ave(rrdclpp_df_19_Mon$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Mon$IntRank <- ifelse(rrdclpp_df_19_Mon$points > 0, 1, 0)
rrdclpp_df_19_Mon$Retirement <- ifelse(rrdclpp_df_19_Mon$positionText == "R", 1, 0)
rrdclpp_df_19_Mon$AvgPitstop <- ifelse(rrdclpp_df_19_Mon$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Mon$Overtake <- ifelse(rrdclpp_df_19_Mon$PLG <= -1, 1, 0)
rrdclpp_df_19_Mon$PosLoss <- ifelse(rrdclpp_df_19_Mon$PLG >= 1, 1, 0)

#-----Canada-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Can$DeltaTime <- ave(rrdclpp_df_19_Can$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Can$PLG <- ave(rrdclpp_df_19_Can$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Can <- rrdclpp_df_19_Can %>%  arrange(positionPerLap)
rrdclpp_df_19_Can <- rrdclpp_df_19_Can %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Can$ProxComp <- ave(rrdclpp_df_19_Can$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Can$RewardDisp <- ave(rrdclpp_df_19_Can$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Can$IntRank <- ifelse(rrdclpp_df_19_Can$points > 0, 1, 0)
rrdclpp_df_19_Can$Retirement <- ifelse(rrdclpp_df_19_Can$positionText == "R", 1, 0)
rrdclpp_df_19_Can$AvgPitstop <- ifelse(rrdclpp_df_19_Can$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Can$Overtake <- ifelse(rrdclpp_df_19_Can$PLG <= -1, 1, 0)
rrdclpp_df_19_Can$PosLoss <- ifelse(rrdclpp_df_19_Can$PLG >= 1, 1, 0)

#-----France-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Fre$DeltaTime <- ave(rrdclpp_df_19_Fre$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Fre$PLG <- ave(rrdclpp_df_19_Fre$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Fre <- rrdclpp_df_19_Fre %>%  arrange(positionPerLap)
rrdclpp_df_19_Fre <- rrdclpp_df_19_Fre %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Fre$ProxComp <- ave(rrdclpp_df_19_Fre$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Fre$RewardDisp <- ave(rrdclpp_df_19_Fre$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Fre$IntRank <- ifelse(rrdclpp_df_19_Fre$points > 0, 1, 0)
rrdclpp_df_19_Fre$Retirement <- ifelse(rrdclpp_df_19_Fre$positionText == "R", 1, 0)
rrdclpp_df_19_Fre$AvgPitstop <- ifelse(rrdclpp_df_19_Fre$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Fre$Overtake <- ifelse(rrdclpp_df_19_Fre$PLG <= -1, 1, 0)
rrdclpp_df_19_Fre$PosLoss <- ifelse(rrdclpp_df_19_Fre$PLG >= 1, 1, 0)

#-----Austria-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Aust$DeltaTime <- ave(rrdclpp_df_19_Aust$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Aust$PLG <- ave(rrdclpp_df_19_Aust$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Aust <- rrdclpp_df_19_Aust %>%  arrange(positionPerLap)
rrdclpp_df_19_Aust <- rrdclpp_df_19_Aust %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Aust$ProxComp <- ave(rrdclpp_df_19_Aust$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Aust$RewardDisp <- ave(rrdclpp_df_19_Aust$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Aust$IntRank <- ifelse(rrdclpp_df_19_Aust$points > 0, 1, 0)
rrdclpp_df_19_Aust$Retirement <- ifelse(rrdclpp_df_19_Aust$positionText == "R", 1, 0)
rrdclpp_df_19_Aust$AvgPitstop <- ifelse(rrdclpp_df_19_Aust$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Aust$Overtake <- ifelse(rrdclpp_df_19_Aust$PLG <= -1, 1, 0)
rrdclpp_df_19_Aust$PosLoss <- ifelse(rrdclpp_df_19_Aust$PLG >= 1, 1, 0)

#-----Britain-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Brit$DeltaTime <- ave(rrdclpp_df_19_Brit$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Brit$PLG <- ave(rrdclpp_df_19_Brit$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Brit <- rrdclpp_df_19_Brit %>%  arrange(positionPerLap)
rrdclpp_df_19_Brit <- rrdclpp_df_19_Brit %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Brit$ProxComp <- ave(rrdclpp_df_19_Brit$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Brit$RewardDisp <- ave(rrdclpp_df_19_Brit$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Brit$IntRank <- ifelse(rrdclpp_df_19_Brit$points > 0, 1, 0)
rrdclpp_df_19_Brit$Retirement <- ifelse(rrdclpp_df_19_Brit$positionText == "R", 1, 0)
rrdclpp_df_19_Brit$AvgPitstop <- ifelse(rrdclpp_df_19_Brit$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Brit$Overtake <- ifelse(rrdclpp_df_19_Brit$PLG <= -1, 1, 0)
rrdclpp_df_19_Brit$PosLoss <- ifelse(rrdclpp_df_19_Brit$PLG >= 1, 1, 0)

#-----Germany-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Ger$DeltaTime <- ave(rrdclpp_df_19_Ger$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Ger$PLG <- ave(rrdclpp_df_19_Ger$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Ger <- rrdclpp_df_19_Ger %>%  arrange(positionPerLap)
rrdclpp_df_19_Ger <- rrdclpp_df_19_Ger %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Ger$ProxComp <- ave(rrdclpp_df_19_Ger$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Ger$RewardDisp <- ave(rrdclpp_df_19_Ger$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Ger$IntRank <- ifelse(rrdclpp_df_19_Ger$points > 0, 1, 0)
rrdclpp_df_19_Ger$Retirement <- ifelse(rrdclpp_df_19_Ger$positionText == "R", 1, 0)
rrdclpp_df_19_Ger$AvgPitstop <- ifelse(rrdclpp_df_19_Ger$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Ger$Overtake <- ifelse(rrdclpp_df_19_Ger$PLG <= -1, 1, 0)
rrdclpp_df_19_Ger$PosLoss <- ifelse(rrdclpp_df_19_Ger$PLG >= 1, 1, 0)

#-----Hungary-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Hun$DeltaTime <- ave(rrdclpp_df_19_Hun$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Hun$PLG <- ave(rrdclpp_df_19_Hun$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Hun <- rrdclpp_df_19_Hun %>%  arrange(positionPerLap)
rrdclpp_df_19_Hun <- rrdclpp_df_19_Hun %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Hun$ProxComp <- ave(rrdclpp_df_19_Hun$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Hun$RewardDisp <- ave(rrdclpp_df_19_Hun$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Hun$IntRank <- ifelse(rrdclpp_df_19_Hun$points > 0, 1, 0)
rrdclpp_df_19_Hun$Retirement <- ifelse(rrdclpp_df_19_Hun$positionText == "R", 1, 0)
rrdclpp_df_19_Hun$AvgPitstop <- ifelse(rrdclpp_df_19_Hun$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Hun$Overtake <- ifelse(rrdclpp_df_19_Hun$PLG <= -1, 1, 0)
rrdclpp_df_19_Hun$PosLoss <- ifelse(rrdclpp_df_19_Hun$PLG >= 1, 1, 0)

#-----Belgium-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Bel$DeltaTime <- ave(rrdclpp_df_19_Bel$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Bel$PLG <- ave(rrdclpp_df_19_Bel$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Bel <- rrdclpp_df_19_Bel %>%  arrange(positionPerLap)
rrdclpp_df_19_Bel <- rrdclpp_df_19_Bel %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Bel$ProxComp <- ave(rrdclpp_df_19_Bel$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Bel$RewardDisp <- ave(rrdclpp_df_19_Bel$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Bel$IntRank <- ifelse(rrdclpp_df_19_Bel$points > 0, 1, 0)
rrdclpp_df_19_Bel$Retirement <- ifelse(rrdclpp_df_19_Bel$positionText == "R", 1, 0)
rrdclpp_df_19_Bel$AvgPitstop <- ifelse(rrdclpp_df_19_Bel$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Bel$Overtake <- ifelse(rrdclpp_df_19_Bel$PLG <= -1, 1, 0)
rrdclpp_df_19_Bel$PosLoss <- ifelse(rrdclpp_df_19_Bel$PLG >= 1, 1, 0)

#-----Italy-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Ita$DeltaTime <- ave(rrdclpp_df_19_Ita$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Ita$PLG <- ave(rrdclpp_df_19_Ita$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Ita <- rrdclpp_df_19_Ita %>%  arrange(positionPerLap)
rrdclpp_df_19_Ita <- rrdclpp_df_19_Ita %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Ita$ProxComp <- ave(rrdclpp_df_19_Ita$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Ita$RewardDisp <- ave(rrdclpp_df_19_Ita$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Ita$IntRank <- ifelse(rrdclpp_df_19_Ita$points > 0, 1, 0)
rrdclpp_df_19_Ita$Retirement <- ifelse(rrdclpp_df_19_Ita$positionText == "R", 1, 0)
rrdclpp_df_19_Ita$AvgPitstop <- ifelse(rrdclpp_df_19_Ita$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Ita$Overtake <- ifelse(rrdclpp_df_19_Ita$PLG <= -1, 1, 0)
rrdclpp_df_19_Ita$PosLoss <- ifelse(rrdclpp_df_19_Ita$PLG >= 1, 1, 0)

#-----Singapore-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Sin$DeltaTime <- ave(rrdclpp_df_19_Sin$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Sin$PLG <- ave(rrdclpp_df_19_Sin$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Sin <- rrdclpp_df_19_Sin %>%  arrange(positionPerLap)
rrdclpp_df_19_Sin <- rrdclpp_df_19_Sin %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Sin$ProxComp <- ave(rrdclpp_df_19_Sin$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Sin$RewardDisp <- ave(rrdclpp_df_19_Sin$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Sin$IntRank <- ifelse(rrdclpp_df_19_Sin$points > 0, 1, 0)
rrdclpp_df_19_Sin$Retirement <- ifelse(rrdclpp_df_19_Sin$positionText == "R", 1, 0)
rrdclpp_df_19_Sin$AvgPitstop <- ifelse(rrdclpp_df_19_Sin$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Sin$Overtake <- ifelse(rrdclpp_df_19_Sin$PLG <= -1, 1, 0)
rrdclpp_df_19_Sin$PosLoss <- ifelse(rrdclpp_df_19_Sin$PLG >= 1, 1, 0)

#-----Russia-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Rus$DeltaTime <- ave(rrdclpp_df_19_Rus$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Rus$PLG <- ave(rrdclpp_df_19_Rus$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Rus <- rrdclpp_df_19_Rus %>%  arrange(positionPerLap)
rrdclpp_df_19_Rus <- rrdclpp_df_19_Rus %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Rus$ProxComp <- ave(rrdclpp_df_19_Rus$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Rus$RewardDisp <- ave(rrdclpp_df_19_Rus$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Rus$IntRank <- ifelse(rrdclpp_df_19_Rus$points > 0, 1, 0)
rrdclpp_df_19_Rus$Retirement <- ifelse(rrdclpp_df_19_Rus$positionText == "R", 1, 0)
rrdclpp_df_19_Rus$AvgPitstop <- ifelse(rrdclpp_df_19_Rus$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Rus$Overtake <- ifelse(rrdclpp_df_19_Rus$PLG <= -1, 1, 0)
rrdclpp_df_19_Rus$PosLoss <- ifelse(rrdclpp_df_19_Rus$PLG >= 1, 1, 0)

#-----Japan-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Jap$DeltaTime <- ave(rrdclpp_df_19_Jap$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Jap$PLG <- ave(rrdclpp_df_19_Jap$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Jap <- rrdclpp_df_19_Jap %>%  arrange(positionPerLap)
rrdclpp_df_19_Jap <- rrdclpp_df_19_Jap %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Jap$ProxComp <- ave(rrdclpp_df_19_Jap$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Jap$RewardDisp <- ave(rrdclpp_df_19_Jap$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Jap$IntRank <- ifelse(rrdclpp_df_19_Jap$points > 0, 1, 0)
rrdclpp_df_19_Jap$Retirement <- ifelse(rrdclpp_df_19_Jap$positionText == "R", 1, 0)
rrdclpp_df_19_Jap$AvgPitstop <- ifelse(rrdclpp_df_19_Jap$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Jap$Overtake <- ifelse(rrdclpp_df_19_Jap$PLG <= -1, 1, 0)
rrdclpp_df_19_Jap$PosLoss <- ifelse(rrdclpp_df_19_Jap$PLG >= 1, 1, 0)

#-----Mexico-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Mex$DeltaTime <- ave(rrdclpp_df_19_Mex$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Mex$PLG <- ave(rrdclpp_df_19_Mex$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Mex <- rrdclpp_df_19_Mex %>%  arrange(positionPerLap)
rrdclpp_df_19_Mex <- rrdclpp_df_19_Mex %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Mex$ProxComp <- ave(rrdclpp_df_19_Mex$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Mex$RewardDisp <- ave(rrdclpp_df_19_Mex$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Mex$IntRank <- ifelse(rrdclpp_df_19_Mex$points > 0, 1, 0)
rrdclpp_df_19_Mex$Retirement <- ifelse(rrdclpp_df_19_Mex$positionText == "R", 1, 0)
rrdclpp_df_19_Mex$AvgPitstop <- ifelse(rrdclpp_df_19_Mex$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Mex$Overtake <- ifelse(rrdclpp_df_19_Mex$PLG <= -1, 1, 0)
rrdclpp_df_19_Mex$PosLoss <- ifelse(rrdclpp_df_19_Mex$PLG >= 1, 1, 0)

#-----USA-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_USA$DeltaTime <- ave(rrdclpp_df_19_USA$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_USA$PLG <- ave(rrdclpp_df_19_USA$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_USA <- rrdclpp_df_19_USA %>%  arrange(positionPerLap)
rrdclpp_df_19_USA <- rrdclpp_df_19_USA %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_USA$ProxComp <- ave(rrdclpp_df_19_USA$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_USA$RewardDisp <- ave(rrdclpp_df_19_USA$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_USA$IntRank <- ifelse(rrdclpp_df_19_USA$points > 0, 1, 0)
rrdclpp_df_19_USA$Retirement <- ifelse(rrdclpp_df_19_USA$positionText == "R", 1, 0)
rrdclpp_df_19_USA$AvgPitstop <- ifelse(rrdclpp_df_19_USA$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_USA$Overtake <- ifelse(rrdclpp_df_19_USA$PLG <= -1, 1, 0)
rrdclpp_df_19_USA$PosLoss <- ifelse(rrdclpp_df_19_USA$PLG >= 1, 1, 0)

#-----Brazil-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Bra$DeltaTime <- ave(rrdclpp_df_19_Bra$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Bra$PLG <- ave(rrdclpp_df_19_Bra$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Bra <- rrdclpp_df_19_Bra %>%  arrange(positionPerLap)
rrdclpp_df_19_Bra <- rrdclpp_df_19_Bra %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Bra$ProxComp <- ave(rrdclpp_df_19_Bra$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Bra$RewardDisp <- ave(rrdclpp_df_19_Bra$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Bra$IntRank <- ifelse(rrdclpp_df_19_Bra$points > 0, 1, 0)
rrdclpp_df_19_Bra$Retirement <- ifelse(rrdclpp_df_19_Bra$positionText == "R", 1, 0)
rrdclpp_df_19_Bra$AvgPitstop <- ifelse(rrdclpp_df_19_Bra$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Bra$Overtake <- ifelse(rrdclpp_df_19_Bra$PLG <= -1, 1, 0)
rrdclpp_df_19_Bra$PosLoss <- ifelse(rrdclpp_df_19_Bra$PLG >= 1, 1, 0)

#-----Abu Dhabi-----#
# DeltaTime = Full Dataframe: Difference of laptimes per driver 
# DeltaTime and PLG have to be done first, after that df is reordered 
rrdclpp_df_19_Abu$DeltaTime <- ave(rrdclpp_df_19_Abu$lapTimeMili, FUN = function(x) c(0, diff(x))) #can't use in first lap
rrdclpp_df_19_Abu$PLG <- ave(rrdclpp_df_19_Abu$positionPerLap, FUN = function(x) c(0, diff(x)))

## Reordering by lap
rrdclpp_df_19_Abu <- rrdclpp_df_19_Abu %>%  arrange(positionPerLap)
rrdclpp_df_19_Abu <- rrdclpp_df_19_Abu %>%  arrange(lap)

## Calculating remaining variables 
rrdclpp_df_19_Abu$ProxComp <- ave(rrdclpp_df_19_Abu$lapTimeMili, FUN = function(x) c(0, diff(x)))
rrdclpp_df_19_Abu$RewardDisp <- ave(rrdclpp_df_19_Abu$points, FUN = function(x) c(0, diff(x))) 
rrdclpp_df_19_Abu$IntRank <- ifelse(rrdclpp_df_19_Abu$points > 0, 1, 0)
rrdclpp_df_19_Abu$Retirement <- ifelse(rrdclpp_df_19_Abu$positionText == "R", 1, 0)
rrdclpp_df_19_Abu$AvgPitstop <- ifelse(rrdclpp_df_19_Abu$pitstopTime > 23.80954, 1, 0)
rrdclpp_df_19_Abu$Overtake <- ifelse(rrdclpp_df_19_Abu$PLG <= -1, 1, 0)
rrdclpp_df_19_Abu$PosLoss <- ifelse(rrdclpp_df_19_Abu$PLG >= 1, 1, 0)

## Merging together all the country dfs to one 
F1_full_2019_dfNEW <-rbind(rrdclpp_df_19_Aus, rrdclpp_df_19_Bah, rrdclpp_df_19_Chi, rrdclpp_df_19_Aze, rrdclpp_df_19_Span, rrdclpp_df_19_Mon,
                      rrdclpp_df_19_Can, rrdclpp_df_19_Fre, rrdclpp_df_19_Aust, rrdclpp_df_19_Brit, rrdclpp_df_19_Ger, rrdclpp_df_19_Hun, 
                      rrdclpp_df_19_Bel, rrdclpp_df_19_Ita, rrdclpp_df_19_Sin, rrdclpp_df_19_Rus, rrdclpp_df_19_Jap, rrdclpp_df_19_Mex, 
                      rrdclpp_df_19_USA, rrdclpp_df_19_Bra, rrdclpp_df_19_Abu)

## Making one risk-taking variable
F1_full_2019_df <- F1_full_2019_df %>%
  mutate(riskTaking = select(., penalty, Retirement, Overtake) %>% rowSums(na.rm = TRUE))


## Factor Analysis 
install.packages("psych")
library(psych)
install.packages("nFactors")
library(nFactors)
install.packages("GPArotation")
library(GPArotation)
library(stargazer)
names(F1_full_2019_dfNEW)
str(F1_full_2019_dfNEW)
## Correlation matrix 
corr.test(as.matrix(F1_full_2019_dfNEW[,29:34]))
corr_matrix <- cor(F1_full_2019_dfNEW[,29:34], method = "pearson", use = "complete.obs")
stargazer(corr_matrix)
## Scree test and eigenvalues 
nScree(F1_full_2019_dfNEW[,27:44]) 
eigen(cor(F1_full_2019_dfNEW[,27:44]))

## factor rotation 
library(GPArotation)
factanal(F1_full_2019_dfNEW[,27:44], factors=10, rotation= "varimax") 
factanal(F1_full_2019_dfNEW[,27:44], factors=10, rotation= "oblimin")
factanal(F1_full_2019_dfNEW[,27:44], factors=10, rotation= "none")

## Factor 1 = crowding from below, Factor 2 = Incident, Factor 3 = speed, Factor 4 = Overtaking, factor 4 = Position 

## Shows whhich factor affects which driver the most
factor_res = factanal(F1_full_2019_dfNEW[,27:44], factors=10, scores= "Bartlett")

driver.scores <- data.frame(factor_res$scores) 
driver.scores$driverRef <- F1_full_2019_dfNEW$driverRef
head(driver.scores)

(driver.mean <- aggregate(driver.scores[,1:10], list(driver.scores[,11]), mean))

## Making dummies for constructorId variable
F1_full_2019_df$Mercedes <- ifelse(F1_full_2019_df$constructorId == 131, 1, 0)
F1_full_2019_df$Ferrari <- ifelse(F1_full_2019_df$constructorId == 6, 1, 0)
F1_full_2019_df$RedBull <- ifelse(F1_full_2019_df$constructorId == 9, 1, 0)
F1_full_2019_df$ToroRosso <- ifelse(F1_full_2019_df$constructorId == 5, 1, 0)
F1_full_2019_df$McLaren <- ifelse(F1_full_2019_df$constructorId == 1, 1, 0)
F1_full_2019_df$Alfa <- ifelse(F1_full_2019_df$constructorId == 51, 1, 0)
F1_full_2019_df$Renault <- ifelse(F1_full_2019_df$constructorId == 4, 1, 0)
F1_full_2019_df$Haas <- ifelse(F1_full_2019_df$constructorId == 210, 1, 0)
F1_full_2019_df$RacingPoint <- ifelse(F1_full_2019_df$constructorId == 211, 1, 0)
F1_full_2019_df$Williams <- ifelse(F1_full_2019_df$constructorId == 3, 1, 0)

F1_full_2019_dfNEW$Mercedes <- ifelse(F1_full_2019_dfNEW$constructorId == 131, 1, 0)
F1_full_2019_dfNEW$Ferrari <- ifelse(F1_full_2019_dfNEW$constructorId == 6, 1, 0)
F1_full_2019_dfNEW$RedBull <- ifelse(F1_full_2019_dfNEW$constructorId == 9, 1, 0)
F1_full_2019_dfNEW$ToroRosso <- ifelse(F1_full_2019_dfNEW$constructorId == 5, 1, 0)
F1_full_2019_dfNEW$McLaren <- ifelse(F1_full_2019_dfNEW$constructorId == 1, 1, 0)
F1_full_2019_dfNEW$Alfa <- ifelse(F1_full_2019_dfNEW$constructorId == 51, 1, 0)
F1_full_2019_dfNEW$Renault <- ifelse(F1_full_2019_dfNEW$constructorId == 4, 1, 0)
F1_full_2019_dfNEW$Haas <- ifelse(F1_full_2019_dfNEW$constructorId == 210, 1, 0)
F1_full_2019_dfNEW$RacingPoint <- ifelse(F1_full_2019_dfNEW$constructorId == 211, 1, 0)
F1_full_2019_dfNEW$Williams <- ifelse(F1_full_2019_dfNEW$constructorId == 3, 1, 0)

str(F1_full_2019_df)

F1_full_2019_df$penalty <- as.numeric(F1_full_2019_df$penalty)
F1_full_2019_df$Retirement <- as.numeric(F1_full_2019_df$Retirement)
F1_full_2019_df$IntRank <- as.numeric(F1_full_2019_df$IntRank)
F1_full_2019_df$AvgPitstop <- as.numeric(F1_full_2019_df$AvgPitstop)
F1_full_2019_df$Overtake <- as.numeric(F1_full_2019_df$Overtake)
F1_full_2019_df$PosLoss <- as.numeric(F1_full_2019_df$PosLoss)
F1_full_2019_df$Mercedes <- as.numeric(F1_full_2019_df$Mercedes)
F1_full_2019_df$Ferrari <- as.numeric(F1_full_2019_df$Ferrari)
F1_full_2019_df$RedBull <- as.numeric(F1_full_2019_df$RedBull)
F1_full_2019_df$ToroRosso <- as.numeric(F1_full_2019_df$ToroRosso)
F1_full_2019_df$McLaren <- as.numeric(F1_full_2019_df$McLaren)
F1_full_2019_df$Alfa <- as.numeric(F1_full_2019_df$Alfa)
F1_full_2019_df$Renault <- as.numeric(F1_full_2019_df$Renault)
F1_full_2019_df$Haas <- as.numeric(F1_full_2019_df$Haas)
F1_full_2019_df$RacingPoint <- as.numeric(F1_full_2019_df$RacingPoint)

F1_full_2019_df$penalty <- as.factor(F1_full_2019_df$penalty)
F1_full_2019_df$Retirement <- as.factor(F1_full_2019_df$Retirement)
F1_full_2019_df$IntRank <- as.factor(F1_full_2019_df$IntRank)
F1_full_2019_df$AvgPitstop <- as.factor(F1_full_2019_df$AvgPitstop)
F1_full_2019_df$Overtake <- as.factor(F1_full_2019_df$Overtake)
F1_full_2019_df$PosLoss <- as.factor(F1_full_2019_df$PosLoss)
F1_full_2019_df$Mercedes <- as.factor(F1_full_2019_df$Mercedes)
F1_full_2019_df$Ferrari <- as.factor(F1_full_2019_df$Ferrari)
F1_full_2019_df$RedBull <- as.factor(F1_full_2019_df$RedBull)
F1_full_2019_df$ToroRosso <- as.factor(F1_full_2019_df$ToroRosso)
F1_full_2019_df$McLaren <- as.factor(F1_full_2019_df$McLaren)
F1_full_2019_df$Alfa <- as.factor(F1_full_2019_df$Alfa)
F1_full_2019_df$Renault <- as.factor(F1_full_2019_df$Renault)
F1_full_2019_df$Haas <- as.factor(F1_full_2019_df$Haas)
F1_full_2019_df$RacingPoint <- as.factor(F1_full_2019_df$RacingPoint)
F1_full_2019_df$Williams <- as.factor(F1_full_2019_df$Williams)

## Create Variable TotalLaps (laps per race)
# Create new variable with only 0 values 
F1_full_2019_df$TotalLaps <- replicate(23928,0)

# Add total laps for each race in the empty variable
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Australian Grand Prix",
                                    58, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Bahrain Grand Prix",
                                    57, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Chinese Grand Prix",
                                    56, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Azerbaijan Grand Prix",
                                    51, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Spanish Grand Prix",
                                    66, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Monaco Grand Prix",
                                    78, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Canadian Grand Prix",
                                    69, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "French Grand Prix",
                                    52, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Austrian Grand Prix",
                                    71, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "British Grand Prix",
                                    52, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "German Grand Prix",
                                    64, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Hungarian Grand Prix",
                                    70, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Belgian Grand Prix",
                                    44, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Italian Grand Prix",
                                    53, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Singapore Grand Prix",
                                    61, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Russian Grand Prix",
                                    53, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Japanese Grand Prix",
                                    53, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Mexican Grand Prix",
                                    71, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "United States Grand Prix",
                                    56, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Brazilian Grand Prix",
                                    71, F1_full_2019_df$TotalLaps)
F1_full_2019_df$TotalLaps <- ifelse(F1_full_2019_df$circuitName == "Abu Dhabi Grand Prix",
                                    55, F1_full_2019_df$TotalLaps)

## Rename totalLaps and TotalLaps
names(F1_full_2019_df)[names(F1_full_2019_df) == "totalLaps"] <- "totalLapsPerDriver"
names(F1_full_2019_df)[names(F1_full_2019_df) == "TotalLaps"] <- "totalLapsPerRace"

install.packages("car")
library(car)
library(ggplot2)

ggplot(F1_full_2019_dfNEW, 
       aes(x = lap, 
           fill = penalty)) + geom_bar()


ggplot(F1_full_2019_dfNEW, 
       aes(x = lap, 
           fill = Overtake)) + 
  geom_bar(position = "stack")



##----------Multiple Mediations Analysis----------## 
install.packages("lavaan")
library(readr)
library(lavaan)

rm(list=ls(all=TRUE))

# standardize your continuous variables here 
F1_full_2019_df$RewardDisp = scale(F1_full_2019_df$RewardDisp)
F1_full_2019_df$DeltaTime = scale(F1_full_2019_df$DeltaTime)
F1_full_2019_df$ProxComp = scale(F1_full_2019_df$ProxComp)
F1_full_2019_df$finPos = scale(F1_full_2019_df$finPos)
F1_full_2019_df$totalLaps = scale(F1_full_2019_df$totalLaps)
F1_full_2019_df$lap = scale(F1_full_2019_df$lap)
F1_full_2019_df$totalLapsPerRace = scale(F1_full_2019_df$totalLapsPerRace)

# make sure your binary IVs are coded as -1/+1 to have effect coding [add all variables that are dummy below - i already added one as an example]
levels(F1_full_2019_df$IntRank) = c(-1,1)
contrasts(F1_full_2019_df$IntRank) = c(-1,1)

levels(F1_full_2019_df$Overtake) = c(-1,1)
contrasts(F1_full_2019_dfNEW$Overtake) = c(-1,1)

levels(F1_full_2019_df$AvgPitstop) = c(-1,1)
contrasts(F1_full_2019_dfNEW$AvgPitstop) = c(-1,1)

levels(F1_full_2019_df$Retirement) = c(-1,1)
contrasts(F1_full_2019_df$Retirement) = c(-1,1)

levels(F1_full_2019_df$PosLoss) = c(-1,1)
contrasts(F1_full_2019_df$PosLoss) = c(-1,1)

levels(F1_full_2019_df$penalty) = c(-1,1)
contrasts(F1_full_2019_df$penalty) = c(-1,1)

levels(F1_full_2019_df$Mercedes) = c(-1,1)
contrasts(F1_full_2019_df$Mercedes) = c(-1,1)

levels(F1_full_2019_df$Alfa) = c(-1,1)
contrasts(F1_full_2019_df$Alfa) = c(-1,1)

levels(F1_full_2019_df$RedBull) = c(-1,1)
contrasts(F1_full_2019_df$RedBull) = c(-1,1)

levels(F1_full_2019_df$McLaren) = c(-1,1)
contrasts(F1_full_2019_df$McLaren) = c(-1,1)

levels(F1_full_2019_df$Haas) = c(-1,1)
contrasts(F1_full_2019_df$Haas) = c(-1,1)

levels(F1_full_2019_df$Ferrari) = c(-1,1)
contrasts(F1_full_2019_df$Ferrari) = c(-1,1)

levels(F1_full_2019_df$Renault) = c(-1,1)
contrasts(F1_full_2019_df$Renault) = c(-1,1)

levels(F1_full_2019_df$RacingPoint) = c(-1,1)
contrasts(F1_full_2019_df$RacingPoint) = c(-1,1)

levels(F1_full_2019_df$Williams) = c(-1,1)
contrasts(F1_full_2019_df$Williams) = c(-1,1)

levels(F1_full_2019_df$ToroRosso) = c(-1,1)
contrasts(F1_full_2019_df$ToroRosso) = c(-1,1)


##----------Specify mediation model----------##
multipleMEdiation <- '

finPos ~ b1 * Overtake + b2 * penalty + 
                  c1 * PosLoss + c2 * RewardDisp + c3 * IntRank + c4 * ProxComp + c5 * DeltaTime + c6 * AvgPitstop + lap + totalLapsPerRace + Mercedes + RacingPoint +
                  Ferrari + Haas + Alfa + Renault + RedBull + Williams + McLaren 
Overtake ~ a11 * PosLoss + a21 * RewardDisp + a31 * IntRank + a41 * ProxComp + a51 * DeltaTime + a61 * AvgPitstop + lap + totalLapsPerRace + Mercedes + RacingPoint +
                  Ferrari + Haas + Alfa + Renault + RedBull + Williams + McLaren
penalty ~ a12 * PosLoss + a22 * RewardDisp + a32 * IntRank + a42 * ProxComp + a52 * DeltaTime + a62 * AvgPitstop + lap + totalLapsPerRace + Mercedes + RacingPoint +
                  Ferrari + Haas + Alfa + Renault + RedBull + Williams + McLaren

Overtake ~~ penalty

direct.PosLoss      := c1 
direct.RewardDisp   := c2
direct.IntRank      := c3
direct.ProxComp     := c4
direct.DeltaTime    := c5
direct.AvgPitstop   := c6
 
indirect.Posloss.via.Overtake     := a11*b1
indirect.Posloss.via.penalty      := a12*b2
total.indirect.Posloss            := (a11*b1) + (a12*b2)  

total.Posloss   := c1 + total.indirect.Posloss

proportion.mediated.PL.Ov := indirect.Posloss.via.Overtake / (direct.PosLoss + indirect.Posloss.via.Overtake)
proportion.mediated.PL.P := indirect.Posloss.via.penalty / (direct.PosLoss + indirect.Posloss.via.penalty)
total.proportion.mediated.PL := proportion.mediated.PL.Ov + proportion.mediated.PL.P

indirect.RewardDisp.via.Overtake   := a21*b1
indirect.RewardDisp.via.penalty   := a22*b2
total.indirect.RewardDisp           := (a21*b1) + (a22*b2) 

total.RewardDisp   := c2 + total.indirect.RewardDisp

proportion.mediated.RD.Ov := indirect.RewardDisp.via.Overtake / (direct.RewardDisp + indirect.RewardDisp.via.Overtake)
proportion.mediated.RD.P := indirect.RewardDisp.via.penalty / (direct.RewardDisp + indirect.RewardDisp.via.penalty)
total.proportion.mediated.RD := proportion.mediated.RD.Ov + proportion.mediated.RD.P

indirect.IntRank.via.Overtake   := a31*b1
indirect.IntRank.via.penalty   := a32*b2
total.indirect.IntRank           := (a31*b1) + (a32*b2)

total.IntRank   := c3 + total.indirect.IntRank

proportion.mediated.IR.Ov := indirect.IntRank.via.Overtake / (direct.IntRank + indirect.IntRank.via.Overtake)
proportion.mediated.IR.P := indirect.IntRank.via.penalty / (direct.IntRank + indirect.IntRank.via.penalty)
total.proportion.mediated.IR := proportion.mediated.IR.Ov + proportion.mediated.IR.P

indirect.ProxComp.via.Overtake   := a41*b1
indirect.ProxComp.via.penalty   := a42*b2
total.indirect.ProxComp          := (a41*b1) + (a42*b2)

total.ProxComp   := c4 + total.indirect.ProxComp

proportion.mediated.PC.Ov := indirect.ProxComp.via.Overtake / (direct.ProxComp + indirect.ProxComp.via.Overtake)
proportion.mediated.PC.P := indirect.ProxComp.via.penalty / (direct.ProxComp + indirect.ProxComp.via.penalty)
total.proportion.mediated.PC := proportion.mediated.PC.Ov + proportion.mediated.PC.P

indirect.DeltaTime.via.Overtake   := a51*b1
indirect.DeltaTime.via.penalty   := a52*b2
total.indirect.DeltaTime           := (a51*b1) + (a52*b2)

total.DeltaTime   := c5 + total.indirect.DeltaTime

proportion.mediated.DT.Ov := indirect.DeltaTime.via.Overtake / (direct.DeltaTime + indirect.DeltaTime.via.Overtake)
proportion.mediated.DT.P := indirect.DeltaTime.via.penalty / (direct.DeltaTime + indirect.DeltaTime.via.penalty)
total.mediated.DT := proportion.mediated.DT.Ov + proportion.mediated.DT.P

indirect.AvgPitstop.via.Overtake   := a61*b1
indirect.AvgPitstop.via.penalty   := a62*b2
total.indirect.AvgPitstop           := (a61*b1) + (a62*b2)

total.AvgPitstop   := c6 + total.indirect.AvgPitstop

proportion.mediated.AP.Ov := indirect.AvgPitstop.via.Overtake / (direct.AvgPitstop  + indirect.AvgPitstop .via.Overtake)
proportion.mediated.AP.P := indirect.AvgPitstop.via.penalty / (direct.AvgPitstop  + indirect.AvgPitstop .via.penalty)
total.mediated.AP := proportion.mediated.AP.Ov + proportion.mediated.AP.P

'
## add calculations
## error terms of mediators are correlated 

# above, i already added some things we did not discuss yet:
# i calculated the direct effects of your IVs on the final DV (direct.xxx)
# as well as the indirect effects of your IVs mediated via the 3 mediators (overtake, penalty and retirement) (indirect.xxx.via.xxx)
# from there, you can sum both to calculate the total effect = direct + indirect (total.xxx)
# I did not write it for all IVs yet -> please add the others following the same structure as done above

#----------Model Estimation----------#

set.seed(1985) # you can pick any seed you want
myfit = sem(model = multipleMEdiation,
            ordered = c("Overtake", "penalty"),
            estimator = "DWLS",
            se = "bootstrap", # standard errors are calculated with bootstrapping
            bootstrap = 10000, # do more iterations once the model is tested and runs well (10000 is usual standard but takes a lot of time - only do at last)
            data = F1_full_2019_df)

summary(myfit,
        fit.measures=TRUE, 
        standardize=TRUE, 
        rsquare=TRUE, 
        estimates = TRUE, 
        ci = TRUE)


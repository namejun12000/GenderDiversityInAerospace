# import library
library(pdftools)
# unzip file
zipFile =  unzip("Data/Aerospace.zip")
# pdfs
pdfFile1 <- "Data/Diversity_in_STEM_REPORT.pdf"
pdfFile2 <- zipFile[19]
# function of extracting text in pdf (median earning by each race & gender)
raceEarnFunc <- function(pdfFile) 
{
  # data frame per page (one row for each textbox)
  data <-pdf_data(pdfFile)
  # set page 
  page <- data[[12]]
  # extract only required text
  raceEarning <- page$text[220:which(page$text == "Note:")-1]
  # list each items
  earn <- raceEarning[c(1,4,7,10,17,18,21,24)]
  gender <- raceEarning[c(3,6,9,12,14,16,20,23)]
  race <- raceEarning[c(2,5,8,11,13,15,19,22)]
  
  # create data frame
  dfRaceEarn <- data.frame(race, earn, gender)
  # change column names
  names(dfRaceEarn) <- c("Race", "AvgEarning", "Gender")
  # create csv file and saving
  dfRaceEarn <- write.csv(dfRaceEarn, "Data/raceEarn.csv", row.names=FALSE)
}

# function of extracting text in pdf (Leadership position by gender)
leaderPosition <- function(pdfFile) 
{
  # data frame per page (one row for each textbox)
  data <-pdf_data(pdfFile2)
  # set page 
  page <- data[[20]]
  # extract only required text and list of each items
  # gender
  gender <- page$text[c(74,74,74,74,74,74,72,72,72,72,72,72)]
  # ratio
  ratio <- page$text[75:which(page$text == "37%")] # only women
  mratio <- page$text[84:which(page$text == "63%")] # only men
  ratio[7:12] <- mratio # merge two lists
  # position
  position <- page$text[c(92:93,100,102:103)]
  HR <- page$text[c(104:105)] 
  HR <- paste(HR[1],HR[2], sep=" ")
  position[6] <- HR
  # create data frame
  dfPosition <- data.frame(gender, ratio, position)
  # change column names
  names(dfPosition) <- c("Gender", "Ratio", "LeadershipPosition")
  # create csv file and saving
  dfPosition <- write.csv(dfPosition, "Data/leadershipPosition.csv", row.names=FALSE)
}


# function of extracting text in pdf (ratio of gender in aviation)
aviationRatio <- function(pdfFile)
{
  # data frame per page (one row for each textbox)
  data <-pdf_data(pdfFile2)
  # set pages for use
  page14 <- data[[14]]
  page17 <- data[[17]]
  page19 <- data[[19]]
  page21 <- data[[21]]
  page22 <- data[[22]]
  page24 <- data[[24]]
  page25 <- data[[25]]
  page26 <- data[[26]]
  # extract only required text and 
  # info of Aircraft Pilot
  AP <- page14$text[c(59:63)]
  AP <- paste(AP[1],AP[2],AP[3],AP[4],AP[5], sep= " ")
  AircraftPilot <- page14$text[c(55,57)]
  AircraftPilot[3] <- AP
  # info of Remote Pilot
  RP <- page17$text[c(9:10)]
  RP <- paste(RP[1], RP[2], sep = " ")
  RemotePilot <- page17$text[c(14,18)]
  RemotePilot[3] <- RP
  # info of Main Mechanic
  MH <- page17$text[c(27,28)]
  MH <- paste(MH[1], MH[2], sep = " ")
  MainMechanics <- page17$text[c(39,42)]
  MainMechanics[3] <- MH
  # info of Air Maechanic
  AMST <- page19$text[c(6:10)]
  AMST <- paste(AMST[1],AMST[2],AMST[3],AMST[4],AMST[5], sep = " ")
  AirMechanics <- page19$text[c(55,62)]
  AirMechanics[3] <- AMST
  # info of Aerospace Engineer
  AE <- page21$text[c(64,65)]
  AE <- paste(AE[1],AE[2], sep= " ")
  AeroEngineers <- page21$text[c(67,71)]
  AeroEngineers[3] <- AE
  # info of Air Traffic Controller
  AT <- page22$text[c(78,79,80)]
  AT <-paste(AT[1],AT[2],AT[3], sep= " ")
  AirTraffic <- page22$text[c(63,68)]
  AirTraffic[3] <- AT
  # info of Dispatcher
  Dispatchers <- page24$text[c(12,15,16)]
  # info of Flight Attendants
  FA <- page25$text[c(50,51)]
  FA <- paste(FA[1],FA[2],sep= " ")
  FlightAttendants <- page25$text[c(48,52)]
  FlightAttendants[3] <- FA
  # info of Airport Manager
  AM <- page26$text[c(58,59)]
  AM <- paste(AM[1],AM[2],sep=" ")
  AirportManagers <- page26$text[c(63,69)]
  AirportManagers[3] <- AM
  # list of each items
  gender <- c(AircraftPilot[1], AirMechanics[2], RemotePilot[2],
              MainMechanics[2],AeroEngineers[2],AirTraffic[2],
              Dispatchers[2], FlightAttendants[2], AirportManagers[2])
  ratio <- c(AircraftPilot[2], AirMechanics[1], RemotePilot[1],
             MainMechanics[1],AeroEngineers[1],AirTraffic[1],
             Dispatchers[1], FlightAttendants[1], AirportManagers[1])
  occupation <- c(AircraftPilot[3], AirMechanics[3], RemotePilot[3],
                  MainMechanics[3],AeroEngineers[3],AirTraffic[3],
                  Dispatchers[3], FlightAttendants[3], AirportManagers[3])
  # create data frame
  dfAviation <- data.frame(gender, ratio, occupation)
  # change column names
  names(dfAviation) <- c("Gender", "Ratio", "Occupation")
  # remove special character % 
  dfAviation$Ratio <- gsub('%','',dfAviation$Ratio)
  # convert data type (numeric)
  dfAviation$Ratio <- as.numeric(dfAviation$Ratio)
  m <- "Men"
  percent <- 100
  # create men data from women data using for-loop
  for (i in 1:9) {
    menInfo <- dfAviation[nrow(9)+i,] <- c(m, percent-as.numeric(dfAviation$Ratio[0+i]),
                                           dfAviation$Occupation[0+i])
    dfAviation <- rbind(dfAviation,menInfo) # append men data
  }
  # create csv file and saving
  dfAviation <- write.csv(dfAviation, "Data/aviationRatio.csv", row.names=FALSE)
}


raceEarnFunc(pdfFile1)
aviationRatio(pdfFile2)
leaderPosition(pdfFile2)

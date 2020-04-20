library(data.table)
library(gapminder)
library(tibble)
library(openxlsx)
library(tidyverse)
library(plyr)
data <- fread("/Users/Jihoon/URAP_2020/SPEEDMerged/ccasr_speed_three_country_sample_v1.0.0.csv", 
              select = c('DL0004', 'DL0006', 'DL0007', 'DL0010', 'DL0011', 'TE0010', 'TE0011', 'TE0012', 'TE0013'))
data2 <- fread("/Users/Jihoon/URAP_2020/SPEEDMerged/SPEED_6_Country_Sample_Data_v1.1.1_Draft-1.csv", 
               select = c('DL0004', 'DL0006', 'DL0007', 'DL0010', 'DL0011', 'TE0010', 'TE0011', 'TE0012', 'TE0013'))
#DL0004: Average Date for the Event
#DL0006: Country Name
#DL0007: Lowest-level spatial unit
#DL0010: Latitutde
#DL0011: Longitude
#TE0010: High number of persons killed 
#TE0011: Low number of persons killed
#TE0012: High numbers of persons injured
#TE0013: Low number of persons injured
is.data.frame(data) #check if the data imported is a data frame

#Rename each columns
names(data)[names(data) == "DL0004"] <- "Date"
names(data)[names(data) == "DL0006"] <- "Country"
names(data)[names(data) == "DL0007"] <- "Region"
names(data)[names(data) == "DL0010"] <- "Latitude"
names(data)[names(data) == "DL0011"] <- "Longitude"
names(data)[names(data) == "TE0010"] <- "Killed_High"
names(data)[names(data) == "TE0011"] <- "Killed_Low"
names(data)[names(data) == "TE0012"] <- "Injured_High"
names(data)[names(data) == "TE0013"] <- "Injured_Low"

names(data2)[names(data2) == "DL0004"] <- "Date"
names(data2)[names(data2) == "DL0006"] <- "Country"
names(data2)[names(data2) == "DL0007"] <- "Region"
names(data2)[names(data2) == "DL0010"] <- "Latitude"
names(data2)[names(data2) == "DL0011"] <- "Longitude"
names(data2)[names(data2) == "TE0010"] <- "Killed_High"
names(data2)[names(data2) == "TE0011"] <- "Killed_Low"
names(data2)[names(data2) == "TE0012"] <- "Injured_High"
names(data2)[names(data2) == "TE0013"] <- "Injured_Low"

sapply(data, class)                                                      #shows current data type of each columns
data$Date <- as.Date(data$Date, "%m/ %d/ %Y")                            #change the data type of the column to date value
sapply(data, class)                                                      #shows current data type of each columns
data = add_column(data, eventYear = as.numeric(format(data$Date, '%Y'))) #add column "eventYear" to the data frame
data <- data[, c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9)]                         #change the order of the columns
data[is.na(data)]=0                                                      #replace NA with 0
dataSL = data[data$Country == "Sierra Leone"]                            #keep only the events occured in Sierra Leone
dataLI = data[data$Country == "Liberia"]   
dataPH = data[data$Country == "Philippines"]

sapply(data2, class)                                                      #shows current data type of each columns
data2$Date <- as.Date(data2$Date, "%m/ %d/ %Y")                            #change the data type of the column to date value
sapply(data2, class)                                                      #shows current data type of each columns
data2 = add_column(data2, eventYear = as.numeric(format(data2$Date, '%Y'))) #add column "eventYear" to the data frame
data2 <- data2[, c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9)]                         #change the order of the columns
data2[is.na(data2)]=0                                                      #replace NA with 0
dataGU = data[data$Country == "Guatemala"]                            #keep only the events occured in Sierra Leone
dataES = data[data$Country == "El Salvador"]   
dataNI = data[data$Country == "Nicaragua"]


scopeDataSL <- data.frame(eventDate=as.Date(character()), eventType=character(), stringsAsFactors=FALSE) #Creates a blank dataframe
scopeDataSL[nrow(scopeDataSL)+1,] = c("2008-07-05", "local")
scopeDataSL[nrow(scopeDataSL)+1,] = c("2007-08-11", "presidential & parliamentary")
scopeDataSL[nrow(scopeDataSL)+1,] = c("2002-05-14", "presidential & parliamentary")
scopeDataSL[nrow(scopeDataSL)+1,] = c("1996-02-27", "presidential & parliamentary")
scopeDataSL[nrow(scopeDataSL)+1,] = c("1991-08-30", "constitutional referendum")
scopeDataSL[nrow(scopeDataSL)+1,] = c("1986-05-30", "parliamentary")
scopeDataSL[nrow(scopeDataSL)+1,] = c("1985-10-01", "presidential")
scopeDataSL[nrow(scopeDataSL)+1,] = c("1982-05-01", "parliamentary")
scopeDataSL$scopeStartDate <- scopeDataSL$eventDate - 30
scopeDataSL = add_column(scopeDataSL, eventYear = substring(scopeDataSL$eventDate, 1, 4))
write.csv(scopeDataSL, "SPEEDMerged/Election Merged/Sierra Leone.csv")


scopeDataLI <- data.frame(eventDate=as.Date(character()), eventType=character(), stringsAsFactors=FALSE)
scopeDataLI[nrow(scopeDataLI)+1,] = c("2005-10-11", "presidential & legislative")
scopeDataLI[nrow(scopeDataLI)+1,] = c("1997-07-19", "presidential & legislative")
scopeDataLI[nrow(scopeDataLI)+1,] = c("1985-10-15", "presidential & legislative")
scopeDataLI[nrow(scopeDataLI)+1,] = c("1984-07-03", "constitutional referendum")
scopeDataLI$scopeStartDate <- scopeDataLI$eventDate - 30
scopeDataLI = add_column(scopeDataLI, eventYear = substring(scopeDataLI$eventDate, 1, 4))
write.csv(scopeDataLI, "SPEEDMerged/Election Merged/Liberia.csv")

scopeDataPH <- data.frame(eventDate=as.Date(character()), eventType=character(), stringsAsFactors=FALSE)
scopeDataPH[nrow(scopeDataPH)+1,] = c("2007-05-14", "legislative & local")
scopeDataPH[nrow(scopeDataPH)+1,] = c("2004-05-10", "presidential, legislative & local")
scopeDataPH[nrow(scopeDataPH)+1,] = c("2001-05-14", "legislative & local")
scopeDataPH[nrow(scopeDataPH)+1,] = c("1998-05-11", "presidential, legislative & local")
scopeDataPH[nrow(scopeDataPH)+1,] = c("1995-05-08", "legislative & local")
scopeDataPH[nrow(scopeDataPH)+1,] = c("1992-05-11", "presidential & legislative")
scopeDataPH[nrow(scopeDataPH)+1,] = c("1988-01-18", "local")
scopeDataPH[nrow(scopeDataPH)+1,] = c("1987-05-11", "legislative")
scopeDataPH[nrow(scopeDataPH)+1,] = c("1987-02-02", "constitutional plebiscite")
scopeDataPH[nrow(scopeDataPH)+1,] = c("1986-02-07", "presidential")
scopeDataPH[nrow(scopeDataPH)+1,] = c("1984-05-14", "parliamentary")
scopeDataPH[nrow(scopeDataPH)+1,] = c("1984-01-27", "national & local constitutional plebiscite")
scopeDataPH[nrow(scopeDataPH)+1,] = c("1981-06-16", "presidential")
scopeDataPH[nrow(scopeDataPH)+1,] = c("1981-04-07", "constitutional referendum")
scopeDataPH$scopeStartDate <- scopeDataPH$eventDate - 30
scopeDataPH = add_column(scopeDataPH, eventYear = substring(scopeDataPH$eventDate, 1, 4))
write.csv(scopeDataPH, "SPEEDMerged/Election Merged/Philippines.csv")

scopeDataGU <- data.frame(eventDate=as.Date(character()), eventType=character(), stringsAsFactors=FALSE) #Creates a blank dataframe
scopeDataGU[nrow(scopeDataGU)+1,] = c("2007-09-09", "presidential & parliamentary")
scopeDataGU[nrow(scopeDataGU)+1,] = c("2003-11-09", "presidential & congressional")
scopeDataGU[nrow(scopeDataGU)+1,] = c("1999-11-07", "presidential & congressional")
scopeDataGU[nrow(scopeDataGU)+1,] = c("1995-11-12", "presidential & congressional")
scopeDataGU[nrow(scopeDataGU)+1,] = c("1994-01-30", "constitutional referendum")
scopeDataGU[nrow(scopeDataGU)+1,] = c("1993-06-05", "presidental")
scopeDataGU[nrow(scopeDataGU)+1,] = c("1990-11-11", "presidental & congressional")
scopeDataGU[nrow(scopeDataGU)+1,] = c("1985-11-03", "presidental & congressional")
scopeDataGU[nrow(scopeDataGU)+1,] = c("1984-07-01", "constitutional assembly")
scopeDataGU[nrow(scopeDataGU)+1,] = c("1982-03-07", "presidental & congressional")
scopeDataGU$scopeStartDate <- scopeDataGU$eventDate - 30
scopeDataGU = add_column(scopeDataGU, eventYear = substring(scopeDataGU$eventDate, 1, 4))
write.csv(scopeDataGU, "SPEEDMerged/Election Merged/Guatemala.csv")

scopeDataES <- data.frame(eventDate=as.Date(character()), eventType=character(), stringsAsFactors=FALSE)
scopeDataES[nrow(scopeDataES)+1,] = c("2009-03-15", "presidential")
scopeDataES[nrow(scopeDataES)+1,] = c("2006-03-12", "legislative")
scopeDataES[nrow(scopeDataES)+1,] = c("2004-03-21", "presidential")
scopeDataES[nrow(scopeDataES)+1,] = c("2003-03-16", "legislative")
scopeDataES[nrow(scopeDataES)+1,] = c("2000-03-12", "legislative")
scopeDataES[nrow(scopeDataES)+1,] = c("1999-03-07", "presidential")
scopeDataES[nrow(scopeDataES)+1,] = c("1997-03-16", "legislative")
scopeDataES[nrow(scopeDataES)+1,] = c("1994-03-20", "presidential & legislative")
scopeDataES[nrow(scopeDataES)+1,] = c("1991-03-10", "legislative")
scopeDataES[nrow(scopeDataES)+1,] = c("1989-03-19", "presidential")
scopeDataES[nrow(scopeDataES)+1,] = c("1988-03-20", "legislative")
scopeDataES[nrow(scopeDataES)+1,] = c("1985-03-31", "legislative")
scopeDataES[nrow(scopeDataES)+1,] = c("1984-03-25", "presidential")
scopeDataES[nrow(scopeDataES)+1,] = c("1982-04-29", "indirect presidential")
scopeDataES$scopeStartDate <- scopeDataES$eventDate - 30
scopeDataES = add_column(scopeDataES, eventYear = substring(scopeDataES$eventDate, 1, 4))
write.csv(scopeDataES, "SPEEDMerged/Election Merged/El Salvador.csv")

scopeDataNI <- data.frame(eventDate=as.Date(character()), eventType=character(), stringsAsFactors=FALSE) #Creates a blank dataframe
scopeDataNI[nrow(scopeDataNI)+1,] = c("2008-11-09", "local")
scopeDataNI[nrow(scopeDataNI)+1,] = c("2006-11-05", "presidential & parliamentary")
scopeDataNI[nrow(scopeDataNI)+1,] = c("2004-11-07", "local")
scopeDataNI[nrow(scopeDataNI)+1,] = c("2001-11-04", "presidential & national assembly")
scopeDataNI[nrow(scopeDataNI)+1,] = c("2000-11-05", "local")
scopeDataNI[nrow(scopeDataNI)+1,] = c("1996-10-20", "presidential & national assembly")
scopeDataNI[nrow(scopeDataNI)+1,] = c("1990-02-25", "presidential & national assembly")
scopeDataNI[nrow(scopeDataNI)+1,] = c("1984-11-04", "presidential & legislative")
scopeDataNI$scopeStartDate <- scopeDataNI$eventDate - 30
scopeDataNI = add_column(scopeDataNI, eventYear = substring(scopeDataNI$eventDate, 1, 4))
write.csv(scopeDataNI, "SPEEDMerged/Election Merged/Nicaragua.csv")

for(i in 1:nrow(scopeDataSL)){
  endDate <- scopeDataSL[i, 1]
  type <- scopeDataSL[i, 2]
  startDate <- scopeDataSL[i, 3]
  year <- scopeDataSL[i, 4]
  dataTemp <- subset(dataSL, Date >= startDate & Date <= endDate)
  dataTemp <- subset(dataTemp, select = c(Region, Killed_High, Killed_Low, Injured_High, Injured_Low, Latitude, Longitude))
  fileName = paste(year, type, sep = " ")
  regionTemp <- data.frame(region=character(), totalCasualtiesHighLowAvg=numeric(), latitude=character(), longitude=character(), stringsAsFactors=FALSE)
  if(nrow(dataTemp) != 0){
    for(j in 1:nrow(dataTemp)){
      if(!dataTemp[j,1] %in% regionTemp$region){
        regionTemp[nrow(regionTemp)+1,] = c(dataTemp[j,1], round(rowMeans(dataTemp[j,2:5]), digits = 0), dataTemp[j,6], dataTemp[j,7])
      }
      else{
        for(k in 1:nrow(regionTemp)){
          if(dataTemp[j,1] %in% regionTemp[k,1]){
            regionTemp[k,] <- c(regionTemp[k, 1], regionTemp[k, 2] + round(rowMeans(dataTemp[j,2:5]), digits = 0), dataTemp[j,6], dataTemp[j,7])
          }
        }
      }
    }
  }
  write.csv(regionTemp, sprintf("SPEEDMerged/Sierra Leone/%s average.csv", fileName))
}

for(i in 1:nrow(scopeDataLI)){
  endDate <- scopeDataLI[i, 1]
  type <- scopeDataLI[i, 2]
  startDate <- scopeDataLI[i, 3]
  year <- scopeDataLI[i, 4]
  dataTemp <- subset(dataLI, Date >= startDate & Date <= endDate)
  dataTemp <- subset(dataTemp, select = c(Region, Killed_High, Killed_Low, Injured_High, Injured_Low, Latitude, Longitude))
  fileName = paste(year, type, sep = " ")
  regionTemp <- data.frame(region=character(), totalCasualtiesHighLowAvg=numeric(), latitude=character(), longitude=character(), stringsAsFactors=FALSE)
  if(nrow(dataTemp) != 0){
    for(j in 1:nrow(dataTemp)){
      if(!dataTemp[j,1] %in% regionTemp$region){
        regionTemp[nrow(regionTemp)+1,] = c(dataTemp[j,1], round((rowMeans(dataTemp[j,2:3])+(rowMeans(dataTemp[j,4:5]))), digits = 0), dataTemp[j,6], dataTemp[j,7])
      }
      else{
        for(k in 1:nrow(regionTemp)){
          if(dataTemp[j,1] %in% regionTemp[k,1]){
            regionTemp[k,] <- c(regionTemp[k, 1], regionTemp[k, 2] + round((rowMeans(dataTemp[j,2:3])+(rowMeans(dataTemp[j,4:5]))), digits = 0), dataTemp[j,6], dataTemp[j,7])
          }
        }
      }
    }
  }
  write.csv(regionTemp, sprintf("/Users/Jihoon/URAP_2020/SPEEDMerged/Liberia/%s average.csv", fileName))
}

for(i in 1:nrow(scopeDataPH)){
  endDate <- scopeDataPH[i, 1]
  type <- scopeDataPH[i, 2]
  startDate <- scopeDataPH[i, 3]
  year <- scopeDataPH[i, 4]
  dataTemp <- subset(dataPH, Date >= startDate & Date <= endDate)
  dataTemp <- subset(dataTemp, select = c(Region, Killed_High, Killed_Low, Injured_High, Injured_Low, Latitude, Longitude))
  fileName = paste(year, type, sep = " ")
  regionTemp <- data.frame(region=character(), totalCasualtiesHighLowAvg=numeric(), latitude=character(), longitude=character(), stringsAsFactors=FALSE)
  if(nrow(dataTemp) != 0){
    for(j in 1:nrow(dataTemp)){
      if(!dataTemp[j,1] %in% regionTemp$region){
        regionTemp[nrow(regionTemp)+1,] = c(dataTemp[j,1], round((rowMeans(dataTemp[j,2:3])+(rowMeans(dataTemp[j,4:5]))), digits = 0), dataTemp[j,6], dataTemp[j,7])
      }
      else{
        for(k in 1:nrow(regionTemp)){
          if(dataTemp[j,1] %in% regionTemp[k,1]){
            regionTemp[k,] <- c(regionTemp[k, 1], regionTemp[k, 2] + round((rowMeans(dataTemp[j,2:3])+(rowMeans(dataTemp[j,4:5]))), digits = 0), dataTemp[j,6], dataTemp[j,7])
          }
        }
      }
    }
  }
  write.csv(regionTemp, sprintf("/Users/Jihoon/URAP_2020/SPEEDMerged/Philippines/%s average.csv", fileName))
}

for(i in 1:nrow(scopeDataGU)){
  endDate <- scopeDataGU[i, 1]
  type <- scopeDataGU[i, 2]
  startDate <- scopeDataGU[i, 3]
  year <- scopeDataGU[i, 4]
  dataTemp <- subset(dataGU, Date >= startDate & Date <= endDate)
  dataTemp <- subset(dataTemp, select = c(Region, Killed_High, Killed_Low, Injured_High, Injured_Low, Latitude, Longitude))
  fileName = paste(year, type, sep = " ")
  regionTemp <- data.frame(region=character(), totalCasualtiesHighLowAvg=numeric(), latitude=character(), longitude=character(), stringsAsFactors=FALSE)
  if(nrow(dataTemp) != 0){
    for(j in 1:nrow(dataTemp)){
      if(!dataTemp[j,1] %in% regionTemp$region){
        regionTemp[nrow(regionTemp)+1,] = c(dataTemp[j,1], round((rowMeans(dataTemp[j,2:3])+(rowMeans(dataTemp[j,4:5]))), digits = 0), dataTemp[j,6], dataTemp[j,7])
      }
      else{
        for(k in 1:nrow(regionTemp)){
          if(dataTemp[j,1] %in% regionTemp[k,1]){
            regionTemp[k,] <- c(regionTemp[k, 1], regionTemp[k, 2] + round((rowMeans(dataTemp[j,2:3])+(rowMeans(dataTemp[j,4:5]))), digits = 0), dataTemp[j,6], dataTemp[j,7])
          }
        }
      }
    }
  }
  write.csv(regionTemp, sprintf("SPEEDMerged/Guatemala/%s average.csv", fileName))
}

for(i in 1:nrow(scopeDataES)){
  endDate <- scopeDataES[i, 1]
  type <- scopeDataES[i, 2]
  startDate <- scopeDataES[i, 3]
  year <- scopeDataES[i, 4]
  dataTemp <- subset(dataES, Date >= startDate & Date <= endDate)
  dataTemp <- subset(dataTemp, select = c(Region, Killed_High, Killed_Low, Injured_High, Injured_Low, Latitude, Longitude))
  fileName = paste(year, type, sep = " ")
  regionTemp <- data.frame(region=character(), totalCasualtiesHighLowAvg=numeric(), latitude=character(), longitude=character(), stringsAsFactors=FALSE)
  if(nrow(dataTemp) != 0){
    for(j in 1:nrow(dataTemp)){
      if(!dataTemp[j,1] %in% regionTemp$region){
        regionTemp[nrow(regionTemp)+1,] = c(dataTemp[j,1], round((rowMeans(dataTemp[j,2:3])+(rowMeans(dataTemp[j,4:5]))), digits = 0), dataTemp[j,6], dataTemp[j,7])
      }
      else{
        for(k in 1:nrow(regionTemp)){
          if(dataTemp[j,1] %in% regionTemp[k,1]){
            regionTemp[k,] <- c(regionTemp[k, 1], regionTemp[k, 2] + round((rowMeans(dataTemp[j,2:3])+(rowMeans(dataTemp[j,4:5]))), digits = 0), dataTemp[j,6], dataTemp[j,7])
          }
        }
      }
    }
  }
  write.csv(regionTemp, sprintf("SPEEDMerged/El Salvador/%s average.csv", fileName))
}

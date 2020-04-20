library(data.table)
library(gapminder)
library(tibble)
library(openxlsx)
library(tidyverse)
library(plyr)

# Tasks (week of 4/13/2020): 
# FIRST OF ALL: Add a readme file. 
# (1) Add a column for the data source
# (2) Add certainty of region as a new variable
# (3) Change the name of "fatalities" to "ave_fatalities", then add "low" and "high" estimates as two additional variables
# (4) Add both start, end dates, and average date for the event. 
# As a fouth column (if there is any information about certainty/ precision of the date)
# (For any dataset that gives only one single event date, then you can fill both variables with that single date)
# (5) Wait to hear from Tolgahan about access to Cline Center's Global News Archive



# Tasks (week of 4/6/2020): 
# DONE: (1) Make the code work faster! (Check "Rfast" package)
# DONE: Tolgahan may place questions onto the code, once he goes over it. (2) Put an explanation.
# DONE: (3) Put the notes (re: ) whichever ones have it (ACLED does), for the ones that don't add the most informative column
# DONE: (4) Please add country, region, smallest region, how certain they are about the region, 
# Tolgahan will look at this. (5) Particularly in ACLED, figure out "Fatalities". What about injuries? Don't they report injuries? Please check the codebook.
# (6) Average the all the casualties related variables (but do include the original low and high estimates)
# (7) Previous comment goes for dates, as well.

# The following files are on Github. 
surveys <- fread("~/URAP_2020/Afrobarometer Violence Identification/Afrobarometer Survey Dates and Dates of Violence.csv")
EDACSLbr <- fread("~/URAP_2020/Afrobarometer Violence Identification/EDACS/EDACS_LBR__20130405.csv", select = c('Country', 'Location', 'Start', 'End', 'FatMx', 'FatMn', 'EvDesc'))
EDACSSie <- fread("~/URAP_2020/Afrobarometer Violence Identification/EDACS/EDACS_SIE__20130405.csv", select = c('Country', 'Location', 'Start', 'End', 'FatMx', 'FatMn', 'EvDesc'))
SPEED <- fread("~/URAP_2020/Afrobarometer Violence Identification/SPEED/ccasr_speed_three_country_sample_v1.0.0.csv", select = c('DL0002', 'DL0003', 'DL0004', 'DL0006', 'DL0007','TE0010', 'TE0011', 'TE0012', 'TE0013'))
ACLED <- fread("~/URAP_2020/Afrobarometer Violence Identification/ACLED/1900-01-01-2020-04-06-Central_America-Eastern_Africa-Middle_Africa-Northern_Africa-Southern_Africa-Western_Africa.csv", select = c('country', 'admin2', 'event_date', 'geo_precision', 'fatalities', 'notes'))
UCDP <- fread("~/URAP_2020/Afrobarometer Violence Identification/UCDP/ged191.csv", select = c('country', 'region', 'date_start', 'date_end', 'best','high','low', 'source_headline', 'where_prec' ))

# convert the column data type from character to date
surveys$`Survey Start Date` <- as.Date(surveys$`Survey Start Date`)
surveys$`Survey End Date` <- as.Date(surveys$`Survey End Date`)
ACLED$event_date <- as.Date(ACLED$event_date)
EDACSLbr$Start <- as.Date(EDACSLbr$Start)
EDACSLbr$End <- as.Date(EDACSLbr$End)
EDACSSie$Start <- as.Date(EDACSSie$Start)
EDACSSie$End <- as.Date(EDACSSie$End)
SPEED$DL0002 <- as.Date(SPEED$DL0002)
SPEED$DL0003 <- as.Date(SPEED$DL0003)
SPEED$DL0004 <- as.Date(SPEED$DL0004)
UCDP$date_start <- as.Date((UCDP$date_start))
UCDP$date_end <- as.Date(UCDP$date_end)

# replace NA with 0
ACLED[is.na(ACLED)]=0         
EDACSLbr[is.na(EDACSLbr)]=0
EDACSSie[is.na(EDACSSie)]=0
SPEED[is.na(SPEED)]=0
UCDP[is.na(UCDP)]=0


# change the name of the datafiles and add new columns so that 
# data[,1] = country, data[,2] = region, data[,3] = regCr(region certainty), data[,4] = dateAvg, 
# data[,5] = dateStart, data[,6] = dateEnd, data[,7] = casualties, data[,8] = fatAv(average fatalities), 
# data[,9] = fatLow (fatalities low estimate), data[,10] = fatHigh(fatalities high estimate), data[,11] = injuries, data[,12] = notes, data[,13] = source
names(ACLED)[names(ACLED) == "admin2"] <- "region"
names(ACLED)[names(ACLED) == "event_date"] <- "dateAvg"
names(ACLED)[names(ACLED) == "geo_precision"] <- "regCr"
names(ACLED)[names(ACLED) == "fatalities"] <- "fatAv"
ACLED[, "dateStart"] <- ACLED$dateAvg
ACLED[, "dateEnd"] <- ACLED$dateAvg
ACLED[, "fatHigh"] <- ACLED$fatAv
ACLED[, "fatLow"] <- ACLED$fatAv
ACLED[, "casualties"] <- NA
ACLED$casualties <- as.integer(ACLED$casualties)
ACLED[, "injuries"] <- NA
ACLED$injuries <- as.integer(ACLED$injuries)
ACLED[, "source"] <- "ACLED"
ACLED <- ACLED[,c("country", "region", "regCr", "dateAvg", "dateStart", "dateEnd", "casualties", "fatAv", "fatLow", "fatHigh", "injuries", "notes", "source")]

names(EDACSLbr)[names(EDACSLbr) == "Country"] <- "country"
names(EDACSLbr)[names(EDACSLbr) == "Location"] <- "region"
names(EDACSLbr)[names(EDACSLbr) == "Start"] <- "dateStart"
names(EDACSLbr)[names(EDACSLbr) == "End"] <- "dateEnd"
names(EDACSLbr)[names(EDACSLbr) == "EvDesc"] <- "notes"
names(EDACSLbr)[names(EDACSLbr) == "FatMx"] <- "fatHigh"
names(EDACSLbr)[names(EDACSLbr) == "FatMn"] <- "fatLow"
EDACSLbr[, "casualties"] <- NA
EDACSLbr$casualties <- as.integer(EDACSLbr$casualties)
EDACSLbr[, "injuries"] <- NA
EDACSLbr$injuries <- as.integer(EDACSLbr$injuries)
EDACSLbr[, "regCr"] <- NA
EDACSLbr$regCr <- as.integer(EDACSLbr$regCr)
EDACSLbr <- transform(EDACSLbr, fatAv = (fatHigh + fatLow) %/% 2)
EDACSLbr[, "dateAvg"] <- as.Date((as.integer(EDACSLbr$dateStart) + as.integer(EDACSLbr$dateEnd)) / 2, origin = "1970-01-01")
EDACSLbr[, "source"] <- "EDACS"
EDACSLbr <- EDACSLbr[,c("country", "region", "regCr", "dateAvg", "dateStart", "dateEnd", "casualties", "fatAv", "fatLow", "fatHigh", "injuries", "notes", "source")]

names(EDACSSie)[names(EDACSSie) == "Country"] <- "country"
names(EDACSSie)[names(EDACSSie) == "Location"] <- "region"
names(EDACSSie)[names(EDACSSie) == "Start"] <- "dateStart"
names(EDACSSie)[names(EDACSSie) == "End"] <- "dateEnd"
names(EDACSSie)[names(EDACSSie) == "EvDesc"] <- "notes"
names(EDACSSie)[names(EDACSSie) == "FatMx"] <- "fatHigh"
names(EDACSSie)[names(EDACSSie) == "FatMn"] <- "fatLow"
EDACSSie[, "casualties"] <- NA
EDACSSie$casualties <- as.integer(EDACSSie$casualties)
EDACSSie[, "injuries"] <- NA
EDACSSie$injuries <- as.integer(EDACSSie$injuries)
EDACSSie[, "regCr"] <- NA
EDACSSie$regCr <- as.integer(EDACSSie$regCr)
EDACSSie <- transform(EDACSSie, fatAv = (fatHigh + fatLow) %/% 2)
EDACSSie[, "dateAvg"] <- as.Date((as.integer(EDACSSie$dateStart) + as.integer(EDACSSie$dateEnd)) / 2, origin = "1970-01-01")
EDACSSie[, "source"] <- "EDACS"
EDACSSie <- EDACSSie[,c("country", "region", "regCr", "dateAvg", "dateStart", "dateEnd", "casualties", "fatAv", "fatLow", "fatHigh", "injuries", "notes", "source")]

names(SPEED)[names(SPEED) == "DL0002"] <- "dateStart"
names(SPEED)[names(SPEED) == "DL0003"] <- "dateEnd"
names(SPEED)[names(SPEED) == "DL0004"] <- "dateAvg"
names(SPEED)[names(SPEED) == "DL0006"] <- "country"
names(SPEED)[names(SPEED) == "DL0007"] <- "region"
names(SPEED)[names(SPEED) == "TE0010"] <- "fatHigh"
names(SPEED)[names(SPEED) == "TE0011"] <- "fatLow"
SPEED[, "regCr"] <- NA
SPEED$regCr <- as.integer(SPEED$regCr)
SPEED <- transform(SPEED, fatAv = (fatHigh + fatLow) %/% 2)
SPEED <- transform(SPEED, injuries = (TE0012 + TE0013) %/% 2)
SPEED <- transform(SPEED, casualties = (fatAv + injuries) %/% 2)
SPEED[, "notes"] <- ""
SPEED[, "source"] <- "SPEED"
SPEED <- SPEED[,c("country", "region", "regCr", "dateAvg", "dateStart", "dateEnd", "casualties", "fatAv", "fatLow", "fatHigh", "injuries", "notes", "source")]

names(UCDP)[names(UCDP) == "date_start"] <- "dateStart"
names(UCDP)[names(UCDP) == "date_end"] <- "dateEnd"
names(UCDP)[names(UCDP) == "best"] <- "fatAv"
names(UCDP)[names(UCDP) == "high"] <- "fatHigh"
names(UCDP)[names(UCDP) == "low"] <- "fatLow"
names(UCDP)[names(UCDP) == "source_headline"] <- "notes"
names(UCDP)[names(UCDP) == "where_prec"] <- "regCr"
UCDP[, "casualties"] <- NA
UCDP$casualties <- as.integer(UCDP$casualties)
UCDP[, "injuries"] <- NA
UCDP$injuries <- as.integer(UCDP$injuries)
UCDP[, "source"] <- "UCDP"
UCDP[, "dateAvg"] <- as.Date((as.integer(UCDP$dateStart) + as.integer(UCDP$dateEnd)) / 2, origin = "1970-01-01")
UCDP$regCr <- as.integer(UCDP$regCr)
UCDP <- UCDP[,c("country", "region", "regCr", "dateAvg", "dateStart", "dateEnd", "casualties", "fatAv", "fatLow", "fatHigh", "injuries", "notes", "source")]

# sorting the data first by country name (ascending order- default), then date (descending order)
ACLED <- plyr::arrange(ACLED, ACLED$country, desc(ACLED$dateAvg))
EDACSLbr <- plyr::arrange(EDACSLbr, EDACSLbr$country, desc(EDACSLbr$dateAvg))
EDACSSie <- plyr::arrange(EDACSSie, EDACSSie$country, desc(EDACSSie$dateAvg))
SPEED <- plyr::arrange(SPEED, SPEED$country, desc(SPEED$dateAvg))
UCDP <- plyr::arrange(UCDP, UCDP$country, desc(UCDP$dateAvg))


# create a dataframe(list of countries where the surveys were taken)
surveyCountries <- data.frame(country=character(), stringsAsFactors=FALSE) 
for(i in 1:nrow(surveys)){
  if(!surveys[i,1] %in% surveyCountries$country){
    surveyCountries[nrow(surveyCountries)+1,] <- c(surveys[i,1])
  }
}

# dataList is a list of dataframes(input files) in the order below
dataList <- list(ACLED, EDACSLbr, EDACSSie, SPEED, UCDP)

# dataCountryIndexList contains dataframe for countries and their starting index
dataCountryIndexList <- list()

# iterate over the dataList and select data from only the countries where the survey has taken place
for(i in 1:length(dataList)){
  dataList[[i]] <-subset(dataList[[i]], dataList[[i]]$country %in% surveyCountries$country)
  
  # create a temp dataframe tracking countries and their starting index for each input files
  tempdf = data.frame(country = character(), index = integer(), stringsAsFactors=FALSE)
  j <- 1
  while(j <= nrow(dataList[[i]])){
    if(j == 1){
      tempdf[1, ] <- c(dataList[[i]][j,1], j)
    }
    else{
      if(!(dataList[[i]][j-1,1] %in% dataList[[i]][j,1])){
        tempdf[nrow(tempdf)+1, ] <- c(dataList[[i]][j,1], j)
      }
    }
    j <- j + 1
  }
  # append the temp dataframe to dataCountryIndexList
  dataCountryIndexList <- append(dataCountryIndexList, list(tempdf))
}

# add new columns to the output dataframe and set to NA and 0, respectively
surveys[, "Region"] <- ""
surveys[, "Credibility of Region"] <- NA
surveys[, "Average Dates of Violence"] <- NA
surveys$`Average Dates of Violence` <- as.Date(surveys$`Average Dates of Violence`)
surveys[, "Start Dates of Violence"] <- NA
surveys$`Start Dates of Violence` <- as.Date(surveys$`Start Dates of Violence`)
surveys[, "End Dates of Violence"] <- NA
surveys$`End Dates of Violence` <- as.Date(surveys$`End Dates of Violence`)
surveys[, "Casualties"] <- NA
surveys$Casualties <- as.integer(surveys$Casualties)
surveys[, "Fatalities High Estimate"] <- NA
surveys$`Fatalities High Estimate` <- as.integer(surveys$`Fatalities High Estimate`)
surveys[, "Fatalities Low Estimate"] <- NA
surveys$`Fatalities Low Estimate` <- as.integer(surveys$`Fatalities Low Estimate`)
surveys[, "Fatalities Average"] <- NA
surveys$`Fatalities Average` <- as.integer(surveys$`Fatalities Average`)
surveys[, "Injuries"] <- NA
surveys$Injuries <- as.integer(surveys$Injuries)
surveys[, "Notes"] <- ""
surveys[, "Sources"] <- ""

i <- 1
# iterate while i is smaller or equal to the length of the dataList
while(i <= length(dataList)){
  j <- 1
  # iterate while j is smaller or equal to the number of rows in the output dataframe
  while(j <= nrow(surveys)){
    # if the country and round number of this row and the next row is equal, this row does not have to be explored, thus move to next row
    if(j < nrow(surveys) & surveys[j,1] %in% surveys[j+1,1] & surveys[j,2] %in% surveys[j+1,2]){
      j <- j + 1
    }
    else{
      start <- as.integer(1)
      end <- as.integer(nrow(dataList[[i]]))
      for(l in 1 : nrow(dataCountryIndexList[[i]])){
        if(surveys[j,1] %in% dataCountryIndexList[[i]][l,1]){
          start <- dataCountryIndexList[[i]][l,2]
          end <- if(l < nrow(dataCountryIndexList[[i]])) dataCountryIndexList[[i]][l+1,2] else as.integer(nrow(dataList[[i]]))
        }
      }
      k <- start
      # iterate while k is smaller or equal to the number of rows in the input dataframe(which is each elements of the dataList)
      while(k <= end){
        # print statement showing the current progress of the loop
        print(sprintf("k: %s / %s, j: %s / %s, scope: %s / %s", k, end, j, nrow(surveys), i, length(dataList)))
        
        # if the alphabetical order of the country of the current row of the input dataframe is larger than that of the current row of the survey,
        # OR if the country of the current row of the input dataframe is equal to that of the current row of the survey AND the date of input is before the survey start date - 30,
        # break the loop and move on to the next row of the survey because all rows of the input dataframe below are not in scope
        # this is possible because the input dataframes are sorted in alphabetically ascending order(country) and then in numerically descending order(date)
        # Tip: comparison of string or characters: (a > banana == FALSE), (banana < c == TRUE), (banana < bee == TRUE)
        if(dataList[[i]][k,1] > surveys[j,1] | ((dataList[[i]][k,1] %in% surveys[j,1])  & (dataList[[i]][k,4] < surveys[j,3]-30))){
          break
        }
        
        # if the previous conditions are not met, continue to iterate
        else{
          # if the country names are equal and the date is between the range of the survey date(start - 30 to end)
          if(dataList[[i]][k,1] %in% surveys[j,1] & dataList[[i]][k,4] >= surveys[j,3]-30 & dataList[[i]][k,4] <= surveys[j,4]){
            print("Reached2")
            # if none of the events have been added to the current survey(Date of Violence is NA, which is the initial value),
            # add the Dates of Violence and fatalities to current survey row
            if(is.na(surveys[j,8])){
              print("new in")
              surveys[j, ] <- c(surveys[j,1], surveys[j,2], surveys[j,3], surveys[j,4], surveys[j,5], dataList[[i]][k,2], dataList[[i]][k,3], dataList[[i]][k,4], dataList[[i]][k,5], dataList[[i]][k,6], dataList[[i]][k,7], dataList[[i]][k,10], dataList[[i]][k,9], dataList[[i]][k,8], dataList[[i]][k,11], dataList[[i]][k,12], dataList[[i]][k,13])
            }
            
            # else if an event has been already added to the current survey
            else{
              # the lines below convert lists to vectors
              print("add row in")
              country <- unlist(surveys[j,1], use.names = FALSE)
              round <- unlist(surveys[j,2], use.names = FALSE)
              start <- as.Date(as.integer(unlist(surveys[j,3], use.names = FALSE)), origin = "1970-01-01")
              end <- as.Date(as.integer(unlist(surveys[j,4], use.names = FALSE)), origin = "1970-01-01")
              duration <- as.integer(unlist(surveys[j,5], use.names = FALSE))
              reg <- unlist(dataList[[i]][k,2], use.names = FALSE)
              regCrd <- as.integer(unlist(dataList[[i]][k,3], use.names = FALSE))
              eventDateAvg <- as.Date(as.integer(unlist(dataList[[i]][k,4], use.names = FALSE)), origin = "1970-01-01")
              eventDateStart <- as.Date(as.integer(unlist(dataList[[i]][k,5], use.names = FALSE)), origin = "1970-01-01")
              eventDateEnd <- as.Date(as.integer(unlist(dataList[[i]][k,6], use.names = FALSE)), origin = "1970-01-01")
              cas <- as.integer(unlist(dataList[[i]][k,7], use.names = FALSE))
              fatalAvg <- as.integer(unlist(dataList[[i]][k,8], use.names = FALSE))
              fatalLow <- as.integer(unlist(dataList[[i]][k,9], use.names = FALSE))
              fatalHigh <- as.integer(unlist(dataList[[i]][k,10], use.names = FALSE))
              inj <- as.integer(unlist(dataList[[i]][k,11], use.names = FALSE))
              note <- unlist(dataList[[i]][k,12], use.names = FALSE)
              src <- unlist(dataList[[i]][k,13], use.names = FALSE)
              
              # because the add_row fuction takes vectors as parameters
              # with the .after flag, new row is added AFTER j, which is the currently iterated row of the surveys dataframe
              surveys <- surveys %>% add_row(Country = country, `Afrobarometer Round Number` = round, `Survey Start Date` = start, `Survey End Date` = end, `Survey Duration` = duration, Region = reg, `Credibility of Region` = regCrd, `Average Dates of Violence` = eventDateAvg, `Start Dates of Violence`=eventDateStart,`End Dates of Violence`=eventDateEnd,  Casualties = cas,`Fatalities High Estimate`=fatalHigh, `Fatalities Low Estimate`=fatalLow, `Fatalities Average` = fatalAvg, Injuries = inj, Notes = note, Sources = src, .after = j)
              
              # because a row has been added after j, increase j by 1. Otherwise events will be added in the undesired order
              j <- j + 1
            }
          }
        }
        # after the loop is complete, increase k
        k <- k + 1
      }
      # after the loop is complete, increase j
      j <- j + 1
    }
  }
  # after the loop is complete, increase i
  i <- i + 1
}
print(surveys)

# export to the output file
write.csv(surveys, "~/URAP_2020/Afrobarometer Violence Identification/Afrobarometer Interrupted Surveys List.csv")

### Files needed to run extractor
# CSV output from data acquisition program
#   colnames = Time, UVout1, Pressure, MFC, UVout2, UVout3, NO, NO2, Signal, Online, Lock, C3F6, Shutter, Notes, Cycle
# CSV Power log with datetime and mW/V conversion for 308 and 355
#   colnames = datetime, mW_V_308, W_V_355


### Packages used within Extractor

library(dplyr)
library(zoo)
library(data.table)

### Functions used within Extractor

char_to_time <- function(TimeX){
  formats = c("%m/%d/%Y %H:%M:%S", "%m/%d/%Y %I:%M:%S %p", "%Y-%m-%d %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y", "%Y-%m-%d", "%H:%M:%S")
  #Add extra formats to character vector above
  #formats tested in order, so go from most specific to least specific, 
  #as the first one that works will be used
  for (i in 1:length(formats)){
    test = as.POSIXct(TimeX, format = formats[i])
    if (length(na.omit(test)) == length(test)) {
      TimeX = as.POSIXct(TimeX, format = formats[i])
      break
    } else if (i == length(formats)){
      print("Could not find adequate format, char_to_time function needs to be edited")
    }
  }
  return(TimeX)
}

### Main Extractor Start:

#Import data, assumes one space-delimited file for all data (may need to edit based on program output)
alldata = read.table(file.choose(), header = TRUE, sep = ",")
#alldata = read.table(file.choose(), header = TRUE, sep = " ")
alldata = alldata[,!(names(alldata) %in% c("X"))]
alldata = alldata[,!(names(alldata) %in% c("TimeX", "dttm"))]
setDT(alldata)
power = read.table(file.choose(), header = TRUE, sep = ",")
setDT(power)
#Edit classes of variables
power$datetime = char_to_time(power$datetime)
if (!class(power$datetime)[1] == "POSIXct"){
  print("Error in time conversion")
  break
}

if (!("DateTime" %in% names(alldata))){
  alldata$DateTime = paste(alldata$Date, alldata$Time)
}
if ("Time" %in% names(alldata)){
  alldata$Time = char_to_time(alldata$Time)
  if (!class(alldata$Time)[1] == "POSIXct"){
    print("Error in time conversion")
    break
  }
}
if ("Date" %in% names(alldata)){
  alldata$Date = char_to_time(alldata$Date)
  if (!class(alldata$Date)[1] == "POSIXct"){
    print("Error in time conversion")
    break
  }
}

alldata$DateTime = char_to_time(alldata$DateTime)
if (!class(alldata$DateTime)[1] == "POSIXct"){
  print("Error in time conversion")
  break
}
#Separate out notes, as the rest of the data is numeric
#Still need to find a way to incorporate notes back into final data files
notes = alldata$Notes
alldata = alldata[,-c("Notes")]


#Change all classes from generic "factor" to numeric
for (i in colnames(alldata)){
  if (class(alldata[[i]])[1]=="factor"){
    alldata[[i]] = as.numeric(as.character(alldata[[i]]))
  } else if (class(alldata[[i]])[1]=="logical"){
    alldata[[i]] = as.numeric(alldata[[i]])
  }
}

#Cycle column indicates one online+offline cycle, could speed extractor if put in program output

if (!"Cycle" %in% colnames(alldata)){
  #Create new column of zeros
  alldata$Cycle = 0
  #creates new column that's the difference between the Online value for each row and the previous
  alldata = alldata %>% mutate(newcol = Online - lag(Online))
  #Finds rows that go from online to offline
  test = which(alldata$newcol == 1)
  #Change data in Cycle column (takes a long time)
  for (i in 2:length(test)){
    alldata$Cycle[test[i-1]:test[i]-1] = i-1
  }
  #Remove newcol used to get cycles

  alldata = alldata[,-c("newcol")]
}

#Add locks (2 before lock indicators and 1 after) to remove all points where laser is moving

#find rows that currently have lock indicator
test = which(alldata$Lock == 1)
#define initial variable
test2 = test[1]
for (i in 1:length(test)){
  test2 = append(test2, (test[i]-2):(test[i]+1))
}
test2 = unique(test2)
alldata$Lock[test2] = 1
rawdata = alldata[alldata$Lock == 0, -c("Lock")]
rm(test)
rm(test2)
if (sum(rawdata$LS)>0){
  LS = rawdata[rawdata$LS != 0, ]
  rawdata = rawdata[rawdata$LS == 0, ]
}
if (sum(rawdata$PD)>0){
  PD = rawdata[rawdata$PD != 0, ]
  rawdata = rawdata[rawdata$PD == 0, ]
}
rawdata = rawdata[,-c("LS", "PD")]


#Remove first point of cycle where shutter hasn't opened yet

#Get new column that's the difference between shutter status for a row and the row before it
rawdata = rawdata %>% mutate(newcol = Shutter - lag(Shutter))
#Find first point of opened shutter (start of HONO cycles)
test = which(rawdata$newcol == -1)
#Row before is last point of closed shutter
test = test-1
#Remove points from rawdata and newcol from dataframe
rawdata = rawdata[-test, -c("newcol")]


rm(test)
#get averages for each cycle for the variables that don't depend on cycling
#aggregate returns a dataframe, so the subsetting method changes
rawavg = aggregate(rawdata, list(rawdata$Cycle), mean)
rawsd = aggregate(rawdata, list(rawdata$Cycle), sd)
rawsd$Cycle = rawavg$Cycle
rawsd = rawsd[ ,!(names(rawsd) %in% c("Group.1"))]

#Separate and average onlines and offlines
onl = rawdata[rawdata$Online == 1, -c("Online")]
off = rawdata[rawdata$Online == 0, -c("Online")]

#Averages based on Cycle number
onlavg = aggregate(onl, list(onl$Cycle), mean)
offavg = aggregate(off, list(off$Cycle), mean)
#Counts within each cycle
onlct = aggregate(onl[,1], list(onl$Cycle), length)
offct = aggregate(off[,1], list(off$Cycle), length)
#sd within each cycle
onlsd = aggregate(onl, list(onl$Cycle), sd)
offsd = aggregate(off, list(off$Cycle), sd)
#append counts and signal sd to rest of data
onlavg$count = onlct[[2]]
offavg$count = offct[[2]]
onlavg$sigsd = onlsd$Signal
offavg$sigsd = offsd$Signal

onlavg = onlavg[ , !(names(onlavg) %in% c("Group.1"))]
offavg = offavg[ , !(names(offavg) %in% c("Group.1"))]

#If data set starts on an offline, removes the first offline to match online
#if either on or offline has an extra cycle at the beginning, then the rawdata
#will also have an extra

while(!offavg$Cycle[1] == onlavg$Cycle[1]){
  if(offavg$Cycle[1]<onlavg$Cycle[1]){
    offavg = offavg[-1, ]
    rawavg = rawavg[-1, ]
    rawsd = rawsd[-1, ]
  } else if (offavg$Cycle[1]>onlavg$Cycle[1]){
    onlavg = onlavg[-1, ]
    rawavg = rawavg[-1, ]
    rawsd = rawsd[-1, ]
  }
  print("In while loop, stop code if you see this too many times")
}

if (!(length(offavg$UVout1) == length(onlavg$UVout1))){
  if(length(offavg$UVout1)>length(onlavg$UVout1)){
    notinboth = setdiff(offavg$Cycle, onlavg$Cycle)
    offavg = offavg[!(offavg$Cycle %in% notinboth), ]
    rawavg = rawavg[!(rawavg$Cycle %in% notinboth), ]
    rawsd = rawsd[!(rawsd$Cycle %in% notinboth), ]
  } else {
    notinboth = setdiff(onlavg$Cycle, offavg$Cycle)
    onlavg = onlavg[!(onlavg$Cycle %in% notinboth), ]
    rawavg = rawavg[!(rawavg$Cycle %in% notinboth), ]
    rawsd = rawsd[!(rawsd$Cycle %in% notinboth), ]
  }
}
#Get cycle averages

col_names = colnames(onlavg)
cycles = data.frame(matrix(ncol = length(col_names), nrow = length(onlavg$UVout1)))
names(cycles) = col_names
for (i in col_names){
  if (i == "UVout1"){
    cycles[[i]] = (onlavg[[i]]*onlavg$count+offavg[[i]]*offavg$count)/(onlavg$count+offavg$count)
  } else if(i == "Signal") {
    cycles[[i]] = ((onlavg[[i]]/onlavg$UVout1)-(offavg[[i]]/offavg$UVout1))*((onlavg$count*onlavg$UVout1)+(offavg$count*offavg$UVout1))/(onlavg$count+offavg$count)
  } else if(i == "sigsd"){
    cycles[[i]] = onlavg[[i]]
  } else {
    cycles[[i]] = rawavg[[i]]
  }
}
cycles$bkgsd = offavg$sigsd

#Correct UVout1 for 355 interference
cycles$Shutter[cycles$Shutter == 0 & cycles$UVout3 < 0.01] = 1

cycles_shutter = cycles[cycles$Shutter == 1, ]
cycles_shutter$newcol = 0
cycles_shutter = cycles_shutter %>%
  mutate(newcol = zoo::rollmean(UVout1, k = 10, fill = NA))
test2 = which(is.na(cycles_shutter$newcol))
for (i in 1:length(test2)){
  if (i < (length(cycles_shutter$newcol)/2)){
    cycles_shutter$newcol[test2[i]]=mean(cycles_shutter$UVout1[1:10])
  } else {
    cycles_shutter$newcol[test2[i]] = mean(cycles_shutter$UVout1[length((cycles_shutter$UVout1)-10):length(cycles_shutter$UVout1)])
  }
}
cycles_shutter = cycles_shutter[,c("Cycle", "newcol")]
setDT(cycles_shutter)
setDT(cycles)
setkey(cycles_shutter, "Cycle")
setkey(cycles, "Cycle")
cycles = cycles_shutter[cycles, roll = TRUE]
cycles$UVout1[cycles$Shutter == 0] = cycles$newcol[cycles$Shutter == 0]
test2 = which(is.na(cycles$newcol))
for (i in 1:length(test2)){
  if (i < length(cycles$newcol)/2){
    cycles$newcol[test2[i]]=mean(cycles$newcol[1:10], na.rm = TRUE)
  } else {
    cycles$newcol[test2[i]] = mean(cycles$newcol[length((cycles$newcol)-10):length(cycles$newcol)], na.rm = TRUE)
  }
}

#apply P308 and P355 conversion, add columns of ROH and PE

setDT(power)
setkey(power, datetime)
setkey(cycles, DateTime)
cycles = power[cycles, roll = TRUE]
cycles$P308 = cycles$UVout1 * cycles$mW_V_308
cycles$P355 = cycles$UVout3 * cycles$W_V_355
cycles = cycles[ , -c("newcol")]
cycles$ROH = 2.374E-8/(1+(146.4*(cycles$H2O/100)))
cycles$PE = 0.0035

#Separate out OH, HONO, and HO2


OH = cycles[cycles$Shutter == 1 & cycles$C3F6 == 0, ]
OH$Conc = OH$Signal/OH$ROH/OH$P308
HO2 = cycles[cycles$Shutter == 1 & cycles$C3F6 == 1, ]
HO2$Conc = HO2$Signal/HO2$ROH/HO2$P308/0.1466
HONO = cycles[cycles$Shutter == 0 & cycles$C3F6 == 0, ]
HONO$Conc = HONO$Signal/HONO$ROH/HONO$P308/HONO$PE/2.4606E10
HONOC3F6 = cycles[cycles$Shutter == 0 & cycles$C3F6 == 1, ]

#Subtract OH from HONO and either C3F6 from OH or OH from HO2

# OHavg = aggregate(OH, list(OH$MeasCycle), mean)
# HONOavg = aggregate(HONO, list(HONO$MeasCycle), mean)
# HO2avg = aggregate(HO2, list(HO2$MeasCycle), mean)


#apply calibrations to get time series of OH, HONO, INT (HO2) along with averages of other measured compounds

#Calculate LOD

#Binned averages of radicals with LOD


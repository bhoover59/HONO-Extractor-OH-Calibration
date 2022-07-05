# OH Calibration Shiny
# Clear memory
rm(list = ls()) 

# Packages
library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(tibble)
library(lubridate)
library(ggplot2)
library(readr)
library(zoo)
library(data.table)
library(ggpmisc)
library(minpack.lm) 
library(grid)
library(gtable)
library(gridExtra)
library(rsconnect)
# To publish:
# rsconnect::deployApp('W:\\Lab\\Lab-pstevens\\Bode\\Extractors\\OH Cal RShiny')
# rsconnect::deployApp('path\\to\\file) # file must be app.R
#shiny::devmode(TRUE)
################################################################################
################################################################################
# Define UI for application
ui <- fluidPage(
  useShinyjs(),
  tabsetPanel(
    tabPanel('HONO Extractor',
             sidebarLayout(
               sidebarPanel(
                 fileInput('raw_data_file_HONO', label = 'Raw data file (txt):', multiple = FALSE, placeholder = "No file selected"),
                 strong("Laser Parameters"),
                 fluidRow(
                   column(7, numericInput('P308_start_HONO', value = 3.05, step = 0.01, label = '308 Laser Power (mW):')),
                   column(5, numericInput('UVout1_start_HONO', value = 0.0058, step = 0.0001, label = '308 UVout1 (V):'))
                 ),
                 fluidRow(
                   column(7, numericInput('P355_start_HONO', value = 18.00, step = 0.01, label = '355 Laser Power (mW):')),
                   column(5, numericInput('UVout3_start_HONO', value = 0.0045, step = 0.0001, label = '355 UVout3 (V):'))
                 ),
                 column(12, style = "border-top:1px solid"),
                 strong("Remove Lock Points"),
                 fluidRow(
                   column(7, numericInput('delete_before_HONO', value = 2, min = 1, step = 1, label = "Before ( > 2)")),
                   column(5, numericInput('delete_after_HONO', value = 2, min = 1, step = 1, label = "After ( > 1)"))
                 ),
                 column(12, style = "border-top:1px solid"),
                 strong("OH Sensitivity from OH Calibration"),
                 fluidRow(
                   column(7, numericInput('OHCal_yint', value = 5e-7, min = 0, step = 1e-9, label = "y int")),
                   column(5, numericInput('OHCal_slope', value = -0.003, min = 1, step = 1, label = "Slope"))
                 ),
                 column(12, style = "border-top:1px solid"),
                 strong("Best Fit Parameters from OH Calibration"),
                 numericInput('dry_sensitivity_HONOExtractor', value = 1.77e-8, min = 0, step = 0.1e-8, label = "Dry Sensitivity"),
                 numericInput('water_dependence_HONOExtractor', value = 103.3, min = 1, step = 0.1, label = "Water Dependence"),
                 column(12, style = "border-top:1px solid"),
                 numericInput('water_source_HONO', value = 2, min = 1, step = 1, label = "Water Source: enter 1 for box monitor or 2 for probe"),
                 numericInput('calibrator_HONO', value = 2, min = 1, step = 1, label = "Calibrator used (1 or 2)"),
                 numericInput('LowerROH_HONO', value = 3e-12, min = 1e-13, step = 1e-12, label = "Lower ROH (3e-12)"), # Lower limit for ROH to remove bad points during calibration
                 numericInput('SNR_HONO', value = 3, min = 1, step = 1, label = "SNR"), # Signal to noise ratio
                 numericInput('RH_HONO', value = 0.01, min = 0.01, step = 0.01, label = "RH to test (1% default). Enter as decimal "),
                 numericInput('N_background_HONOExtractor_HONO', value = 15, min = 1, step = 1, label = "Background Time (min)"),
                 numericInput('HONO_PE_HONO_Extractor', value = 0.0041, min = 0.0001, step = 0.0001, label = "HONO PE"),
                 p("Author: Bode Hoover (bodehoov@iu.edu)"),
                 p("Adapted from VBA code written by Emily Reidy and HOx Extractor by Ian Spink"),
                 p("Last updated: June 29, 2022")
               ),
               mainPanel(
                 p("Extractor for data from the HONO cell"),
                 p("Must ctrl + F replace spaces with commas and add comma after MeasCycle in original txt before uploading"),
                 br(),
                 actionButton('HONOExtractorStart', 'Start HONO Extractor'),
                 br(), br(),
                 column(12, style = "border-top:1px solid"),
                 h4('Download HONO Extracted Data:'),
                 # textInput('filenameHONO', label = NULL, width = "70%", placeholder = '.csv'),
                 fluidRow(
                   column(8, textInput('filenameHONO', label = NULL, placeholder = '.csv')),
                   column(4, downloadButton(outputId = 'downloadHONOExtractor', label = 'Download .csv File'))
                 ),
                 column(12, style = "border-top:1px solid"),
                 h4("Output:"),
                 tabsetPanel(
                   tabPanel("Raw Data", dataTableOutput("table1_HONOExtractor")),
                   tabPanel("OH Plot", plotOutput("plot1_HONOExtractor")),
                   tabPanel("HONO Plot", plotOutput("plot2_HONOExtractor"))
                 )
               )
             )
    ),
    tabPanel('OH Calibration',
             sidebarLayout(
               sidebarPanel(
                 fileInput('raw_data_file', label = 'Raw data file (txt):', multiple = FALSE, placeholder = "No file selected"),
                 strong("Laser Parameters"),
                 fluidRow(
                   column(7, numericInput('P308_start', value = 3.05, step = 0.01, label = '308 Laser Power (mW):')),
                   column(5, numericInput('UVout1_start', value = 0.0058, step = 0.0001, label = '308 UVout1 (V):'))
                 ),
                 fluidRow(
                   column(7, numericInput('P355_start', value = 18.00, step = 0.01, label = '355 Laser Power (mW):')),
                   column(5, numericInput('UVout3_start', value = 0.0045, step = 0.0001, label = '355 UVout3 (V):'))
                 ),
                 column(12, style = "border-top:1px solid"),
                 strong("Remove Lock Points"),
                 fluidRow(
                   column(7, numericInput('delete_before', value = 2, min = 1, step = 1, label = "Before ( > 2)")),
                   column(5, numericInput('delete_after', value = 2, min = 1, step = 1, label = "After ( > 1)"))
                 ),
                 column(12, style = "border-top:1px solid"),
                 numericInput('water_source', value = 2, min = 1, step = 1, label = "Water Source: enter 1 for box monitor or 2 for probe"),
                 numericInput('calibrator', value = 2, min = 1, step = 1, label = "Calibrator used (1 or 2)"),
                 numericInput('LowerROH', value = 3e-12, min = 1e-13, step = 1e-12, label = "Lower ROH (3e-12)"), # Lower limit for ROH to remove bad points during calibration
                 numericInput('SNR', value = 3, min = 1, step = 1, label = "SNR"), # Signal to noise ratio
                 numericInput('RH', value = 0.01, min = 0.01, step = 0.01, label = "RH to test (1% default). Enter as decimal "),
                 numericInput('N_background', value = 15, min = 1, step = 1, label = "Background Time (min)"),
                 numericInput('HONO_PE', value = 0.0041, min = 0.0001, step = 0.0001, label = "HONO PE"),
                 p("Author: Bode Hoover (bodehoov@iu.edu)"),
                 p("Adapted from VBA code written by Emily Reidy and HOx Extractor by Ian Spink"),
                 p("Last updated: June 29, 2022")
               ),
               mainPanel(
                 p("OH Calibration for data from the HONO cell"),
                 p("HONO Extractor does not need to be run first"),
                 p("Must ctrl + F replace spaces with commas and add comma after MeasCycle in original txt before uploading"),
                 br(),
                 actionButton('OHCalStart', 'Start OH Calibration'),
                 br(), br(),
                 column(12, style = "border-top:1px solid"),
                 h4('Download Total OH Calibration Data:'),
                 # textInput('filenameOH', label = NULL, width = "70%", placeholder = '.csv'),
                 fluidRow(
                   column(8, textInput('filenameOH', label = NULL, placeholder = '.csv')),
                   column(4, downloadButton(outputId = 'downloadOHCal', label = 'Download .csv File'))
                 ),
                 column(12, style = "border-top:1px solid"),
                 h4("Output:"),
                 tabsetPanel(
                   tabPanel("Raw Data", dataTableOutput("table1")),
                   tabPanel("Results", dataTableOutput("table2")),
                   tabPanel("OH Calibration Plot", plotOutput("plot1"))
                 )
               )
             )
    ),
  )
)


################################################################################
################################################################################  
server <- function(input, output, session) {
  # calibrator_HONO Constants @ 10 LPM, needs periodically updated
  Cal1_ozone <- 131.49
  Cal1_O2_cross_section <- 1.12e-20 
  Cal1_ROH <- 0.483 # Wall loss
  Cal2_ozone <- 389.23
  Cal2_O2_cross_section <- 7.805e-21
  Cal2_ROH <- 0.601
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
  
  ##################################################################################  
  data_retrieve <- reactiveValues(data = NULL)
  # Retrieve raw data file and edit names 
  observeEvent(input$raw_data_file, {
    # INPUT DATA
    # must be single txt file
    raw_data_file <- input$raw_data_file
    df <- read.table(raw_data_file$datapath, header = TRUE, sep = ",")
    df <- df[,!(names(df) %in% c("TimeX", "dttm"))]
    setDT(df) # converts list to data table
    colnames(df)[28] <- "Notes" # Renames column X to Notes 
    # Not sure why R sees X instead of Notes from output. File output by LabView shows Notes as column title
    data_retrieve$data <- df
  })
  ###############################################################################################
  data_retrieve_HONO <- reactiveValues(data = NULL)
  # Retrieve raw data file and edit names 
  observeEvent(input$raw_data_file_HONO, {
    # INPUT DATA
    # must be single txt file
    raw_data_file_HONO <- input$raw_data_file_HONO
    df_HONO <- read.table(raw_data_file_HONO$datapath, header = TRUE, sep = ",")
    df_HONO <- df_HONO[,!(names(df_HONO) %in% c("TimeX", "dttm"))]
    setDT(df_HONO) # converts list to data table
    colnames(df_HONO)[28] <- "Notes" # Renames column X to Notes 
    # Not sure why R sees X instead of Notes from output. File output by LabView shows Notes as column title
    data_retrieve_HONO$data <- df_HONO
  })
  
  ###############################################################################################
  observeEvent(input$OHCalStart, {
    req(input$raw_data_file)
    # CALIBRATOR
    if(input$calibrator == 1){
      O3 <- Cal1_ozone
      O2_cross_section <- Cal1_O2_cross_section
      WallLoss <- Cal1_ROH
      HONO_PE <- input$HONO_PE
    } else{
      O3 <- Cal2_ozone
      O2_cross_section <- Cal2_O2_cross_section
      WallLoss <- Cal2_ROH
      HONO_PE <- input$HONO_PE
    }
    # WATER SOURCE
    # If using box monitor (1), RH and Temp must be calculated from box values. 
    # Must remove rows without notes due to incorrect RH during transitions
    if(input$water_source == 1){
      # removes row if there are no notes
      pos_del <- which(data_retrieve$data$Notes == delete | data_retrieve$data$Notes == Delete)
      len_pos_del <- length(pos_del) # might need to replace length with nrow
      data_retrieve$data <- data_retrieve$data[-(1:len_pos_del), ] # delete rows equal to and before delete or Delete in notes
      data_retrieve$data <- data_retrieve$data[!(data_retrieve$data$Notes == "" | is.na(data_retrieve$data$Notes)), ] # deletes rows with no notes or NA
      data_retrieve$data$Temp1 <- (data_retrieve$data$Notes + 40) * 5 - 40
      data_retrieve$data$Temp2 <- data_retrieve$data$Temp1
      data_retrieve$data$RH1 <- data_retrieve$data$Notes * 5
      data_retrieve$data$RH2 <- data_retrieve$data$RH1
    } # if select 2, then nothing needs to change. Use direct probe output without corrections
    exp <- 10**((7.591386 * data_retrieve$data$Temp1) / (240.7263 + data_retrieve$data$Temp1)) # separated to make equation easier to read
    data_retrieve$data$Water <- 6.117823 * (data_retrieve$data$RH1/100) * exp / (1013.25 - (6.116441 * exp * (data_retrieve$data$RH1/100)))
    
    # Edit classes of variables
    if (!("DateTime" %in% names(data_retrieve$data))){
      data_retrieve$data$DateTime = paste(data_retrieve$data$Date, data_retrieve$data$Time)
    }
    # data_retrieve$data$LocalTime <- data_retrieve$data$Time
    if ("Time" %in% names(data_retrieve$data)){
      data_retrieve$data$Time = char_to_time(data_retrieve$data$Time)
      if (!class(data_retrieve$data$Time)[1] == "POSIXct"){
        print("Error in time conversion")
        break
      }
    }
    if ("Date" %in% names(data_retrieve$data)){
      data_retrieve$data$Date = char_to_time(data_retrieve$data$Date)
      if (!class(data_retrieve$data$Date)[1] == "POSIXct"){
        print("Error in time conversion")
        break
      }
    }
    data_retrieve$data$DateTime = char_to_time(data_retrieve$data$DateTime)
    if (!class(data_retrieve$data$DateTime)[1] == "POSIXct"){
      print("Error in time conversion")
      break
    }
    
    ### NOTES ###
    # Separate out notes, as the rest of the data is numeric
    # Still need to find a way to incorporate notes back into final data files
    # notes = data_retrieve$data$Notes # store notes for later
    data_retrieve$data = data_retrieve$data[,-c("Notes")] # delete notes. They have been used by now to calculate RH and Temp
    # Gets replaced by DateTime somehow
    
    # Change all classes from generic "factor" to numeric
    for (i in colnames(data_retrieve$data)){
      if (class(data_retrieve$data[[i]])[1] == "factor"){
        data_retrieve$data[[i]] = as.numeric(as.character(data_retrieve$data[[i]]))
      } else if (class(data_retrieve$data[[i]])[1] == "logical"){
        data_retrieve$data[[i]] = as.numeric(data_retrieve$data[[i]])
      }
    }
    
    # Cycle column indicates one online+offline cycle, could speed extractor if put in program output
    if (!"Cycle" %in% colnames(data_retrieve$data)){
      #Create new column of zeros
      data_retrieve$data$Cycle <- 0
      #creates new column that's the difference between the Online value for each row and the previous
      # mutate creates new variable newcol
      # lag finds previous value
      # %>% translates to "and then" meaning it passes value onto next thing
      data_retrieve$data$newcol <- data_retrieve$data$Online - lag(data_retrieve$data$Online)
      #data_retrieve$data <- data_retrieve$data %>% mutate(newcol = Online - lag(Online)) 
      # Finds rows that go from online to offline
      # which returns position of element meeting criteria
      test <- which(data_retrieve$data$newcol == 1)
      # Change data in Cycle column (takes a long time)
      for (i in 2:length(test)){
        data_retrieve$data$Cycle[test[i-1]:test[i]-1] = i-1
      }
      # Remove newcol used to get cycles
      data_retrieve$data <- data_retrieve$data[,-c("newcol")]
    }
    
    ### LOCKS & DELETE ###
    # Add locks (at least 2 before lock indicators and 1 after) to remove all points where laser is moving
    # Find rows that currently have lock indicator
    test <- which(data_retrieve$data$Lock == 1) # returns positions when lock is on
    # Define initial variable
    test2 <- test[1] # starting with first lock
    for (i in 1:length(test)){
      test2 <- append(test2, (test[i] - input$delete_before):(test[i] + input$delete_after))
    } # adds points previous to, equal to, and after lock to test2
    test2 <- unique(test2) # removes duplicates
    data_retrieve$data$Lock[test2] <- 1 # when lock is online, changes to 1
    rawdata <- data_retrieve$data[data_retrieve$data$Lock == 0, -c("Lock")] # data frame without lock column when lock off
    rawdata$Cycle <- as.numeric(as.character(rawdata$Cycle))
    
    rm(test)
    rm(test2)
    
    ### ARCHAIC TESTS ###
    # Laser Scatter (LS) Test
    if (sum(rawdata$LS) > 0){
      LS <- rawdata[rawdata$LS != 0, ]
      rawdata <- rawdata[rawdata$LS == 0, ] # removes points where LS is not 0
    } 
    # Photo-diode (PD) Test
    if (sum(rawdata$PD) > 0){
      PD <- rawdata[rawdata$PD != 0, ]
      rawdata <- rawdata[rawdata$PD == 0, ] # removes points where PD is not 0
    }
    rawdata <- rawdata[,-c("LS", "PD")] # remove LS and PD columns
    
    # Remove first point of cycle where shutter hasn't opened yet
    # Get new column that's the difference between shutter status for a row and the row before it
    rawdata$newcol <- rawdata$Shutter - lag(rawdata$Shutter)
    #rawdata <- rawdata$ %>% mutate(newcol = Shutter - lag(Shutter))
    # Find first point of opened shutter (start of HONO cycles)
    test <- which(rawdata$newcol == -1)
    # Row before is last point of closed shutter
    test <- test-1
    # Remove points from rawdata and newcol from dataframe
    rawdata <- rawdata[-test, -c("newcol")]
    rm(test)
    
    # Get averages for each cycle for the variables that don't depend on cycling
    # Aggregate returns a dataframe, applies function to dataframe by grouping via list
    rawavg <- aggregate(rawdata, list(rawdata$Cycle), mean)
    rawsd <- aggregate(rawdata, list(rawdata$Cycle), sd)
    rawsd$Cycle <- rawavg$Cycle
    rawsd <- rawsd[ ,!(names(rawsd) %in% c("Group.1"))]
    
    # Separate and average onlines and offlines
    onl = rawdata[rawdata$Online == 1, -c("Online")]
    off = rawdata[rawdata$Online == 0, -c("Online")]
    
    # Averages based on Cycle number
    onlavg = aggregate(onl, list(onl$Cycle), mean)
    offavg = aggregate(off, list(off$Cycle), mean)
    # Counts within each cycle
    onlct = aggregate(onl[,1], list(onl$Cycle), length)
    offct = aggregate(off[,1], list(off$Cycle), length)
    # sd within each cycle
    onlsd = aggregate(onl, list(onl$Cycle), sd)
    offsd = aggregate(off, list(off$Cycle), sd)
    # Append counts and signal sd to rest of data
    onlavg$count = onlct[[2]]
    offavg$count = offct[[2]]
    onlavg$sigsd = onlsd$Signal
    offavg$sigsd = offsd$Signal
    
    onlavg = onlavg[ , !(names(onlavg) %in% c("Group.1"))]
    offavg = offavg[ , !(names(offavg) %in% c("Group.1"))]
    
    # If data set starts on an offline, removes the first offline to match online
    # if either on or offline has an extra cycle at the beginning, then the rawdata
    # will also have an extra
    
    while(!offavg$Cycle[1] == onlavg$Cycle[1]){
      if(offavg$Cycle[1] < onlavg$Cycle[1]){
        offavg = offavg[-1, ]
        rawavg = rawavg[-1, ]
        rawsd = rawsd[-1, ]
      } else if (offavg$Cycle[1] > onlavg$Cycle[1]){
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
    
    ### CYCLE AVERAGES ###
    #Get cycle averages
    col_names <- colnames(onlavg)
    cycles <- data.frame(matrix(ncol = length(col_names), nrow = length(onlavg$UVout1)))
    names(cycles) <- col_names
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
    cycles$WeightedNetSignal <- cycles$Signal
    cycles$bkgsd = offavg$sigsd
    
    # Correct UVout1 for 355 interference
    cycles$Shutter[cycles$Shutter == 0 & cycles$UVout3 < 0.01] = 1 # return columns where shutter off and UVout3 less than 0.01
    cycles_shutter = cycles[cycles$Shutter == 1, ] # returns rows where shutter is on
    cycles_shutter$newcol = 0
    cycles_shutter = cycles_shutter %>%
      mutate(newcol = zoo::rollmean(UVout1, k = 10, fill = NA)) #average every 10 points? 10 min resolution?
    # change 10 to variable so it can be 15 or 30 min?
    test2 = which(is.na(cycles_shutter$newcol))
    for (i in 1:length(test2)){
      if (i < (length(cycles_shutter$newcol)/2)){
        cycles_shutter$newcol[test2[i]] = mean(cycles_shutter$UVout1[1:10])
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
    
    # Default Dry Sensitivity and Water Dependence
    # need to vary these values to minimize sum of square of differences
    dry_sensitivity <- 1.77e-8
    water_dependence <- 103.3 
    
    # Calculations for Sensitivities and LODs
    if(input$calibrator == 1){
      cycles$Water <- cycles$Water
    } else{
      cycles$Water <- cycles$Water * 100
    }
    water_background <- 0 # always 0 in Excel extractor
    UVout2_background <- 0 # always 0 in Excel extractor
    setkey(cycles, DateTime)
    cycles$P308 <- cycles$UVout1 * (input$P308_start / input$UVout1_start)
    cycles$P355 <- cycles$UVout3 * (input$P355_start / input$UVout3_start)
    cycles <- cycles[ , -c("newcol")] # not found. When is it created and deleted?
    cycles$EstROH <- dry_sensitivity / (1 + (water_dependence * (cycles$Water)))
    cycles$PE <- HONO_PE
    cycles$NetWater <- cycles$Water - water_background  
    cycles$NetUVout2 <- cycles$UVout2 - UVout2_background
    cycles$O3 <- cycles$NetUVout2 * O3
    cycles$NetOH <- cycles$O3 * 1e-9 * cycles$NetWater * 2.4e19 * 7.1e-20 / 2 / 0.2 / O2_cross_section
    cycles$ROH <- cycles$WeightedNetSignal / cycles$NetOH / cycles$P308 / WallLoss 
    cycles$ROH_scaled <- cycles$ROH * 1e8
    cycles$EstROH_scaled <- cycles$EstROH * 1e8
    cycles$DiffSquare <- (cycles$EstROH_scaled - cycles$ROH_scaled)^2
    
    # Delete row if ROH below 5e-10 which indicates error
    # cycles <- subset(cycles, cycles$ROH > 3e-12)
    cycles <- subset(cycles, cycles$ROH > input$LowerROH)
    
    # Linear Fit
    fit <- lm(formula = cycles$ROH ~ cycles$NetWater)
    cf <- coef(fit)
    OH_sensitivity_linear <- cf["(Intercept)"]
    slope_linear <- cf["cycles$NetWater"]
    
    # Non Linear Fit 
    EstROH_Fit <- function(x,a,b){
      a / (1 + b * x) 
    } # x is net water, a is dry sensitivity, b is water dependence
    y <- cycles$ROH
    x <- cycles$NetWater
    # CANNOT use nls function. Will get "singular gradient" error
    EstROH_nlsLM_fit <- nlsLM(y ~ EstROH_Fit(x,a,b), start = list(a = 1e-10, b = 100))
    Fit_coeff <- coef(EstROH_nlsLM_fit)
    dry_sensitivity <- coef(EstROH_nlsLM_fit)[["a"]] 
    water_dependence <- coef(EstROH_nlsLM_fit)[["b"]] 
    # Need to apply this correction to change EstROH values
    cycles$EstROH <- EstROH_Fit(cycles$NetWater, dry_sensitivity, water_dependence)
    fit2 <- lm(formula = cycles$EstROH ~ cycles$NetWater)
    cf2 <- coef(fit2)
    OH_sensitivity_SV <- cf["(Intercept)"]
    slope_SV <- cf["cycles$NetWater"]
    
    # Background
    background <- subset(off, off$RH1 < 10) # reassigning off cycles to background data frame. Only want offline when RH = 0 and laser off. RH = 0 impossible so filter with less than 10
    N_background <- input$N_background * 60 # point per second
    sd_background <- sd(background$Signal)
    avg_background <- mean(background$Signal)
    
    # Calculate OH LOD (molec/cm^3)
    P308_avg <- mean(cycles$P308)
    #RH <- 0.01 # test at relative humidity of 1%
    SNR <- 3 # signal to noise ratio (1-3)
    ROH_1perc_linear <- slope_linear * input$RH + OH_sensitivity_linear
    ROH_1perc_SV <- dry_sensitivity / (1 + water_dependence * input$RH)
    
    # For visual output
    OH_LOD_linear <- (SNR * sd_background) / (sqrt(N_background) * ROH_1perc_linear * P308_avg)
    OH_LOD_SV <- (SNR * sd_background) / (sqrt(N_background) * ROH_1perc_SV * P308_avg)
    HONO_LOD_linear <- OH_LOD_linear/ HONO_PE / 2.4606e10
    HONO_LOD_SV <- OH_LOD_SV / HONO_PE / 2.4606e10
    
    Variable <- c("OH Sensitivity y int", "OH Sensitivity Slope", "OH Sensitivity y int", "OH Sensitivity Slope", "OH LOD", "OH LOD", "HONO LOD", "HONO LOD", "sd Background", "Average Background")
    Value <- c(OH_sensitivity_linear, slope_linear, OH_sensitivity_SV, slope_SV, OH_LOD_linear, OH_LOD_SV, HONO_LOD_linear, HONO_LOD_SV, sd_background, avg_background)
    Units <- c("NA", "NA", "NA", "NA", "molec/cm3", "molec/cm3", "ppb", "ppb", "counts", "counts")
    Type <- c("linear", "linear", "SV", "SV", "linear", "SV", "linear", "SV",  "NA", "NA")
    outputDataVisual <- data.frame(Variable, Value, Units, Type)
    #format(outputDataVisual$Value, scientific = TRUE)
    outputDataVisual$Value <- formatC(outputDataVisual$Value, format="e", digits = 4)
    #outputDataVisual <- format(round(outputDataVisual, 4), nsmall = 4)
    #outputDataVisual$Value <- round(outputDataVisual$Value[5:10], digits = 4) # works for limiting to 4 decimals. Issues with small values though, need scientific notation
    #format(outputDataVisual, digits = 4, scientific = TRUE)
    # updateTextInput(session, "filenameOH", value = paste(format(cycles$Date[1], "%Y%m%d"), "OH Data.csv", sep = ""))
    updateTextInput(session, "filenameOH", value = "OH Data.csv")
    
    ################################################################################
    #Display OH cal: linear and SV corrected fits
    output$plot1 <- renderPlot({
      req(input$raw_data_file)
      ggplot(cycles, aes(x = NetWater, y = ROH)) +
        geom_smooth(method='lm', formula= y ~ x) +
        geom_point(size=2, shape=16) +
        theme_bw() +
        xlab("Net Water") +
        ylab("ROH") +
        #ggtitle("OH Calibration") +
        geom_point(data = cycles, mapping = aes(x = NetWater, y = EstROH), color = "red", shape = 23)
    })
    
    # Display output data
    output$table2 <- renderDataTable({
      req(input$raw_data_file)
      outputDataVisual
    })
   
    output$downloadOHCal <- downloadHandler(
      filename = function() {
        paste("OH Cal ", Sys.Date(), ".csv", sep="") # can replace Sys.Date() with input$filenameOH
      },
      content = function(file) {
        write.csv(cycles, file)
      })
    
  })
  
###############################################################################################
  observeEvent(input$HONOExtractorStart, {
    req(input$raw_data_file_HONO)
    # calibrator_HONO
    if(input$calibrator_HONO == 1){
      O3_HONO <- Cal1_ozone
      O2_cross_section_HONO <- Cal1_O2_cross_section
      WallLoss_HONO <- Cal1_ROH
      HONO_PE_HONO_Extractor <- input$HONO_PE_HONO_Extractor
    } else{
      O3_HONO <- Cal2_ozone
      O2_cross_section_HONO <- Cal2_O2_cross_section
      WallLoss_HONO <- Cal2_ROH
      HONO_PE_HONO_Extractor <- input$HONO_PE_HONO_Extractor
    }
    # WATER SOURCE
    # If using box monitor (1), RH and Temp must be calculated from box values. 
    # Must remove rows without notes due to incorrect RH during transitions
    if(input$water_source_HONO == 1){
      # removes row if there are no notes
      pos_del_HONOExtractor <- which(data_retrieve_HONO$data$Notes == delete | data_retrieve_HONO$data$Notes == Delete)
      len_pos_del_HONOExtractor <- length(pos_del_HONOExtractor) # might need to replace length with nrow
      data_retrieve_HONO$data <- data_retrieve_HONO$data[-(1:len_pos_del_HONOExtractor), ] # delete rows equal to and before delete or Delete in notes
      data_retrieve_HONO$data <- data_retrieve_HONO$data[!(data_retrieve_HONO$data$Notes == "" | is.na(data_retrieve_HONO$data$Notes)), ] # deletes rows with no notes or NA
      data_retrieve_HONO$data$Temp1 <- (data_retrieve_HONO$data$Notes + 40) * 5 - 40
      data_retrieve_HONO$data$Temp2 <- data_retrieve_HONO$data$Temp1
      data_retrieve_HONO$data$RH1 <- data_retrieve_HONO$data$Notes * 5
      data_retrieve_HONO$data$RH2 <- data_retrieve_HONO$data$RH1
    } # if select 2, then nothing needs to change. Use direct probe output without corrections
    exp_HONO <- 10**((7.591386 * data_retrieve_HONO$data$Temp1) / (240.7263 + data_retrieve_HONO$data$Temp1)) # separated to make equation easier to read
    data_retrieve_HONO$data$Water <- 6.117823 * (data_retrieve_HONO$data$RH1/100) * exp_HONO / (1013.25 - (6.116441 * exp_HONO * (data_retrieve_HONO$data$RH1/100)))
    
    # Edit classes of variables
    if (!("DateTime" %in% names(data_retrieve_HONO$data))){
      data_retrieve_HONO$data$DateTime = paste(data_retrieve_HONO$data$Date, data_retrieve_HONO$data$Time)
    }
    # data_retrieve_HONO$data$LocalTime <- data_retrieve_HONO$data$Time
    if ("Time" %in% names(data_retrieve_HONO$data)){
      data_retrieve_HONO$data$Time = char_to_time(data_retrieve_HONO$data$Time)
      if (!class(data_retrieve_HONO$data$Time)[1] == "POSIXct"){
        print("Error in time conversion")
        break
      }
    }
    if ("Date" %in% names(data_retrieve_HONO$data)){
      data_retrieve_HONO$data$Date = char_to_time(data_retrieve_HONO$data$Date)
      if (!class(data_retrieve_HONO$data$Date)[1] == "POSIXct"){
        print("Error in time conversion")
        break
      }
    }
    data_retrieve_HONO$data$DateTime = char_to_time(data_retrieve_HONO$data$DateTime)
    if (!class(data_retrieve_HONO$data$DateTime)[1] == "POSIXct"){
      print("Error in time conversion")
      break
    }
    
    ### NOTES ###
    # Separate out notes, as the rest of the data is numeric
    # Still need to find a way to incorporate notes back into final data files
    # notes = data_retrieve_HONO$data$Notes # store notes for later
    data_retrieve_HONO$data = data_retrieve_HONO$data[,-c("Notes")] # delete notes. They have been used by now to calculate RH and Temp
    # Gets replaced by DateTime somehow
    
    # Change all classes from generic "factor" to numeric
    for (i in colnames(data_retrieve_HONO$data)){
      if (class(data_retrieve_HONO$data[[i]])[1] == "factor"){
        data_retrieve_HONO$data[[i]] = as.numeric(as.character(data_retrieve_HONO$data[[i]]))
      } else if (class(data_retrieve_HONO$data[[i]])[1] == "logical"){
        data_retrieve_HONO$data[[i]] = as.numeric(data_retrieve_HONO$data[[i]])
      }
    }
    
    # Cycle column indicates one online+offline cycle, could speed extractor if put in program output
    if (!"Cycle" %in% colnames(data_retrieve_HONO$data)){
      #Create new column of zeros
      data_retrieve_HONO$data$Cycle <- 0
      #creates new column that's the difference between the Online value for each row and the previous
      # mutate creates new variable newcol
      # lag finds previous value
      # %>% translates to "and then" meaning it passes value onto next thing
      data_retrieve_HONO$data$newcol <- data_retrieve_HONO$data$Online - lag(data_retrieve_HONO$data$Online)
      #data_retrieve_HONO$data <- data_retrieve_HONO$data %>% mutate(newcol = Online - lag(Online)) 
      # Finds rows that go from online to offline
      # which returns position of element meeting criteria
      test_HONOExtractor <- which(data_retrieve_HONO$data$newcol == 1)
      # Change data in Cycle column (takes a long time)
      for (i in 2:length(test_HONOExtractor)){
        data_retrieve_HONO$data$Cycle[test_HONOExtractor[i-1]:test_HONOExtractor[i]-1] = i-1
      }
      # Remove newcol used to get cycles_HONOExtractor
      data_retrieve_HONO$data <- data_retrieve_HONO$data[,-c("newcol")]
    }
    
    ### LOCKS & DELETE ###
    # Add locks (at least 2 before lock indicators and 1 after) to remove all points where laser is moving
    # Find rows that currently have lock indicator
    test_HONOExtractor <- which(data_retrieve_HONO$data$Lock == 1) # returns positions when lock is on
    # Define initial variable
    test_HONOExtractor2 <- test_HONOExtractor[1] # starting with first lock
    for (i in 1:length(test_HONOExtractor)){
      test_HONOExtractor2 <- append(test_HONOExtractor2, (test_HONOExtractor[i] - input$delete_before_HONO):(test_HONOExtractor[i] + input$delete_after_HONO))
    } # adds points previous to, equal to, and after lock to test_HONOExtractor2
    test_HONOExtractor2 <- unique(test_HONOExtractor2) # removes duplicates
    data_retrieve_HONO$data$Lock[test_HONOExtractor2] <- 1 # when lock is online, changes to 1
    rawdata_HONOExtractor <- data_retrieve_HONO$data[data_retrieve_HONO$data$Lock == 0, -c("Lock")] # data frame without lock column when lock off
    rawdata_HONOExtractor$Cycle <- as.numeric(as.character(rawdata_HONOExtractor$Cycle))
    
    rm(test_HONOExtractor)
    rm(test_HONOExtractor2)
    
    ### ARCHAIC test_HONOExtractorS ###
    # Laser Scatter (LS) Test
    if (sum(rawdata_HONOExtractor$LS) > 0){
      LS_HONOExtractor <- rawdata_HONOExtractor[rawdata_HONOExtractor$LS != 0, ]
      rawdata_HONOExtractor <- rawdata_HONOExtractor[rawdata_HONOExtractor$LS == 0, ] # removes points where LS is not 0
    } 
    # Photo-diode (PD) Test
    if (sum(rawdata_HONOExtractor$PD) > 0){
      PD_HONOExtractor <- rawdata_HONOExtractor[rawdata_HONOExtractor$PD != 0, ]
      rawdata_HONOExtractor <- rawdata_HONOExtractor[rawdata_HONOExtractor$PD == 0, ] # removes points where PD is not 0
    }
    rawdata_HONOExtractor <- rawdata_HONOExtractor[,-c("LS", "PD")] # remove LS and PD columns
    
    # Remove first point of cycle where shutter hasn't opened yet
    # Get new column that's the difference between shutter status for a row and the row before it
    rawdata_HONOExtractor$newcol <- rawdata_HONOExtractor$Shutter - lag(rawdata_HONOExtractor$Shutter)
    #rawdata_HONOExtractor <- rawdata_HONOExtractor$ %>% mutate(newcol = Shutter - lag(Shutter))
    # Find first point of opened shutter (start of HONO cycles_HONOExtractor)
    test_HONOExtractor <- which(rawdata_HONOExtractor$newcol == -1)
    # Row before is last point of closed shutter
    test_HONOExtractor <- test_HONOExtractor-1
    # Remove points from rawdata_HONOExtractor and newcol from dataframe
    rawdata_HONOExtractor <- rawdata_HONOExtractor[-test_HONOExtractor, -c("newcol")]
    rm(test_HONOExtractor)
    
    # Get averages for each cycle for the variables that don't depend on cycling
    # Aggregate returns a dataframe, applies function to dataframe by grouping via list
    rawavg_HONOExtractor <- aggregate(rawdata_HONOExtractor, list(rawdata_HONOExtractor$Cycle), mean)
    rawsd_HONOExtractor <- aggregate(rawdata_HONOExtractor, list(rawdata_HONOExtractor$Cycle), sd)
    rawsd_HONOExtractor$Cycle <- rawavg_HONOExtractor$Cycle
    rawsd_HONOExtractor <- rawsd_HONOExtractor[ ,!(names(rawsd_HONOExtractor) %in% c("Group.1"))]
    
    # Separate and average onlines and offlines
    onl_HONOExtractor <- rawdata_HONOExtractor[rawdata_HONOExtractor$Online == 1, -c("Online")]
    off_HONOExtractor <- rawdata_HONOExtractor[rawdata_HONOExtractor$Online == 0, -c("Online")]
    
    # Averages based on Cycle number
    onlavg_HONOExtractor = aggregate(onl_HONOExtractor, list(onl_HONOExtractor$Cycle), mean)
    offavg_HONOExtractor = aggregate(off_HONOExtractor, list(off_HONOExtractor$Cycle), mean)
    # Counts within each cycle
    onlct_HONOExtractor = aggregate(onl_HONOExtractor[,1], list(onl_HONOExtractor$Cycle), length)
    offct_HONOExtractor = aggregate(off_HONOExtractor[,1], list(off_HONOExtractor$Cycle), length)
    # sd within each cycle
    onlsd_HONOExtractor = aggregate(onl_HONOExtractor, list(onl_HONOExtractor$Cycle), sd)
    offsd_HONOExtractor = aggregate(off_HONOExtractor, list(off_HONOExtractor$Cycle), sd)
    # Append counts and signal sd to rest of data
    onlavg_HONOExtractor$count = onlct_HONOExtractor[[2]]
    offavg_HONOExtractor$count = offct_HONOExtractor[[2]]
    onlavg_HONOExtractor$sigsd = onlsd_HONOExtractor$Signal
    offavg_HONOExtractor$sigsd = offsd_HONOExtractor$Signal
    
    onlavg_HONOExtractor = onlavg_HONOExtractor[ , !(names(onlavg_HONOExtractor) %in% c("Group.1"))]
    offavg_HONOExtractor = offavg_HONOExtractor[ , !(names(offavg_HONOExtractor) %in% c("Group.1"))]
    
    # If data set starts on an offline, removes the first offline to match online
    # if either on or offline has an extra cycle at the beginning, then the rawdata_HONOExtractor
    # will also have an extra
    
    while(!offavg_HONOExtractor$Cycle[1] == onlavg_HONOExtractor$Cycle[1]){
      if(offavg_HONOExtractor$Cycle[1] < onlavg_HONOExtractor$Cycle[1]){
        offavg_HONOExtractor = offavg_HONOExtractor[-1, ]
        rawavg_HONOExtractor = rawavg_HONOExtractor[-1, ]
        rawsd_HONOExtractor = rawsd_HONOExtractor[-1, ]
      } else if (offavg_HONOExtractor$Cycle[1] > onlavg_HONOExtractor$Cycle[1]){
        onlavg_HONOExtractor = onlavg_HONOExtractor[-1, ]
        rawavg_HONOExtractor = rawavg_HONOExtractor[-1, ]
        rawsd_HONOExtractor = rawsd_HONOExtractor[-1, ]
      }
      print("In while loop, stop code if you see this too many times")
    }
    if (!(length(offavg_HONOExtractor$UVout1) == length(onlavg_HONOExtractor$UVout1))){
      if(length(offavg_HONOExtractor$UVout1)>length(onlavg_HONOExtractor$UVout1)){
        notinboth = setdiff(offavg_HONOExtractor$Cycle, onlavg_HONOExtractor$Cycle)
        offavg_HONOExtractor = offavg_HONOExtractor[!(offavg_HONOExtractor$Cycle %in% notinboth), ]
        rawavg_HONOExtractor = rawavg_HONOExtractor[!(rawavg_HONOExtractor$Cycle %in% notinboth), ]
        rawsd_HONOExtractor = rawsd_HONOExtractor[!(rawsd_HONOExtractor$Cycle %in% notinboth), ]
      } else {
        notinboth = setdiff(onlavg_HONOExtractor$Cycle, offavg_HONOExtractor$Cycle)
        onlavg_HONOExtractor = onlavg_HONOExtractor[!(onlavg_HONOExtractor$Cycle %in% notinboth), ]
        rawavg_HONOExtractor = rawavg_HONOExtractor[!(rawavg_HONOExtractor$Cycle %in% notinboth), ]
        rawsd_HONOExtractor = rawsd_HONOExtractor[!(rawsd_HONOExtractor$Cycle %in% notinboth), ]
      }
    }
    
    ### CYCLE AVERAGES ###
    #Get cycle averages
    col_names_HONOExtractor <- colnames(onlavg_HONOExtractor)
    cycles_HONOExtractor <- data.frame(matrix(ncol = length(col_names_HONOExtractor), nrow = length(onlavg_HONOExtractor$UVout1)))
    names(cycles_HONOExtractor) <- col_names_HONOExtractor
    for (i in col_names_HONOExtractor){
      if (i == "UVout1"){
        cycles_HONOExtractor[[i]] = (onlavg_HONOExtractor[[i]]*onlavg_HONOExtractor$count+offavg_HONOExtractor[[i]]*offavg_HONOExtractor$count)/(onlavg_HONOExtractor$count+offavg_HONOExtractor$count)
      } else if(i == "Signal") {
        cycles_HONOExtractor[[i]] = ((onlavg_HONOExtractor[[i]]/onlavg_HONOExtractor$UVout1)-(offavg_HONOExtractor[[i]]/offavg_HONOExtractor$UVout1))*((onlavg_HONOExtractor$count*onlavg_HONOExtractor$UVout1)+(offavg_HONOExtractor$count*offavg_HONOExtractor$UVout1))/(onlavg_HONOExtractor$count+offavg_HONOExtractor$count)
      } else if(i == "sigsd"){
        cycles_HONOExtractor[[i]] = onlavg_HONOExtractor[[i]]
      } else {
        cycles_HONOExtractor[[i]] = rawavg_HONOExtractor[[i]]
      } 
    }
    cycles_HONOExtractor$WeightedNetSignal <- cycles_HONOExtractor$Signal
    cycles_HONOExtractor$bkgsd = offavg_HONOExtractor$sigsd
    
    # Correct UVout1 for 355 interference
    cycles_HONOExtractor$Shutter[cycles_HONOExtractor$Shutter == 0 & cycles_HONOExtractor$UVout3 < 0.01] = 1 # return columns where shutter off and UVout3 less than 0.01
    cycles_HONOExtractor_shutter = cycles_HONOExtractor[cycles_HONOExtractor$Shutter == 1, ] # returns rows where shutter is on
    cycles_HONOExtractor_shutter$newcol = 0
    cycles_HONOExtractor_shutter = cycles_HONOExtractor_shutter %>%
      mutate(newcol = zoo::rollmean(UVout1, k = 10, fill = NA)) #average every 10 points? 10 min resolution?
    # change 10 to variable so it can be 15 or 30 min?
    test_HONOExtractor2 = which(is.na(cycles_HONOExtractor_shutter$newcol))
    for (i in 1:length(test_HONOExtractor2)){
      if (i < (length(cycles_HONOExtractor_shutter$newcol)/2)){
        cycles_HONOExtractor_shutter$newcol[test_HONOExtractor2[i]] = mean(cycles_HONOExtractor_shutter$UVout1[1:10])
      } else {
        cycles_HONOExtractor_shutter$newcol[test_HONOExtractor2[i]] = mean(cycles_HONOExtractor_shutter$UVout1[length((cycles_HONOExtractor_shutter$UVout1)-10):length(cycles_HONOExtractor_shutter$UVout1)])
      }
    }
    cycles_HONOExtractor_shutter = cycles_HONOExtractor_shutter[,c("Cycle", "newcol")]
    setDT(cycles_HONOExtractor_shutter)
    setDT(cycles_HONOExtractor)
    setkey(cycles_HONOExtractor_shutter, "Cycle")
    setkey(cycles_HONOExtractor, "Cycle")
    cycles_HONOExtractor = cycles_HONOExtractor_shutter[cycles_HONOExtractor, roll = TRUE]
    cycles_HONOExtractor$UVout1[cycles_HONOExtractor$Shutter == 0] = cycles_HONOExtractor$newcol[cycles_HONOExtractor$Shutter == 0]
    test_HONOExtractor2 = which(is.na(cycles_HONOExtractor$newcol))
    for (i in 1:length(test_HONOExtractor2)){
      if (i < length(cycles_HONOExtractor$newcol)/2){
        cycles_HONOExtractor$newcol[test_HONOExtractor2[i]]=mean(cycles_HONOExtractor$newcol[1:10], na.rm = TRUE)
      } else {
        cycles_HONOExtractor$newcol[test_HONOExtractor2[i]] = mean(cycles_HONOExtractor$newcol[length((cycles_HONOExtractor$newcol)-10):length(cycles_HONOExtractor$newcol)], na.rm = TRUE)
      }
    }
    
    # Default Dry Sensitivity and Water Dependence
    # need to vary these values to minimize sum of square of differences
    dry_sensitivity_HONOExtractor <- input$dry_sensitivity_HONOExtractor # 1.77e-8
    water_dependence_HONOExtractor <- input$water_dependence_HONOExtractor # 103.3 
    
    # Calculations for Sensitivities and LODs
    if(input$calibrator_HONO == 1){
      cycles_HONOExtractor$Water <- cycles_HONOExtractor$Water
    } else{
      cycles_HONOExtractor$Water <- cycles_HONOExtractor$Water * 100
    }
    
    water_background_HONOExtractor_HONOExtractor <- 0 # always 0 in Excel extractor
    UVout2_background_HONOExtractor_HONOExtractor <- 0 # always 0 in Excel extractor
    setkey(cycles_HONOExtractor, DateTime)
    cycles_HONOExtractor$P308 <- cycles_HONOExtractor$UVout1 * (input$P308_start_HONO / input$UVout1_start_HONO)
    cycles_HONOExtractor$P355 <- cycles_HONOExtractor$UVout3 * (input$P355_start_HONO / input$UVout3_start_HONO)
    cycles_HONOExtractor <- cycles_HONOExtractor[ , -c("newcol")] # not found. When is it created and deleted?
    cycles_HONOExtractor$EstROH <- dry_sensitivity_HONOExtractor /(1 + (water_dependence_HONOExtractor * (cycles_HONOExtractor$Water)))
    cycles_HONOExtractor$PE <- HONO_PE_HONO_Extractor
    cycles_HONOExtractor$NetWater <- cycles_HONOExtractor$Water - water_background_HONOExtractor_HONOExtractor  
    cycles_HONOExtractor$NetUVout2 <- cycles_HONOExtractor$UVout2 - UVout2_background_HONOExtractor_HONOExtractor
    cycles_HONOExtractor$O3 <- cycles_HONOExtractor$NetUVout2 * O3_HONO
    cycles_HONOExtractor$NetOH <- cycles_HONOExtractor$O3 * 1e-9 * cycles_HONOExtractor$NetWater * 2.4e19 * 7.1e-20 / 2 / 0.2 / O2_cross_section_HONO
    #cycles_HONOExtractor$ROH <- input$OHCal_slope * cycles_HONOExtractor$NetWater + input$OHCal_yint # Applying OH calibration 
    cycles_HONOExtractor$ROH <- cycles_HONOExtractor$WeightedNetSignal / cycles_HONOExtractor$NetOH / cycles_HONOExtractor$P308 / WallLoss_HONO
    cycles_HONOExtractor$ROH_scaled <- cycles_HONOExtractor$ROH * 1e8
    cycles_HONOExtractor$EstROH2 <- dry_sensitivity_HONOExtractor / (1 + (cycles_HONOExtractor$Water * water_dependence_HONOExtractor))
    cycles_HONOExtractor$EstROH_scaled <- cycles_HONOExtractor$EstROH * 1e8
    cycles_HONOExtractor$DiffSquare <- (cycles_HONOExtractor$EstROH_scaled - cycles_HONOExtractor$ROH_scaled)^2
    OH_sig_HONOExtractor <- which(cycles_HONOExtractor$Shutter == 1) # find when shutter is on (No HONO photolysis)
    cycles_HONOExtractor$OH[OH_sig_HONOExtractor] <- cycles_HONOExtractor$WeightedNetSignal / (cycles_HONOExtractor$ROH * cycles_HONOExtractor$P308)
    HONO_sig_HONOExtractor <- which(cycles_HONOExtractor$Shutter == 0) # find when shutter is off (355 laser on, HONO being photolyzed)
    cycles_HONOExtractor$HONO[HONO_sig_HONOExtractor] <- cycles_HONOExtractor$WeightedNetSignal / (cycles_HONOExtractor$ROH * cycles_HONOExtractor$P308 * HONO_PE_HONO_Extractor * 2.46e10)

    # Delete row if ROH below 5e-10 which indicates error
    # cycles_HONOExtractor <- subset(cycles_HONOExtractor, cycles_HONOExtractor$ROH > 3e-12)
    cycles_HONOExtractor <- subset(cycles_HONOExtractor, cycles_HONOExtractor$ROH > input$LowerROH_HONO)
    
    updateTextInput(session, "filenameHONO", value = "HONO Extracted Data.csv")
    
    ################################################################################
    #Display OH cal: linear and SV corrected fits
    # Need to change to [HONO] vs time
    output$plot1_HONOExtractor <- renderPlot({
      req(input$raw_data_file_HONO)
      ggplot(cycles_HONOExtractor, aes(x = Time, y = OH)) +
        #geom_smooth(method='lm', formula= y ~ x) +
        geom_point(size=2, shape=16) +
        theme_bw() +
        xlab("Time") +
        ylab("OH (molec/cm3)")
    })
    output$plot2_HONOExtractor <- renderPlot({
      req(input$raw_data_file_HONO)
      ggplot(cycles_HONOExtractor, aes(x = Time, y = HONO)) +
        #geom_smooth(method='lm', formula= y ~ x) +
        geom_point(size=2, shape=16) +
        theme_bw() +
        xlab("Time") +
        ylab("HONO (ppb)")
    })
    
    # Display output data
    output$downloadHONOExtractor <- downloadHandler(
      filename = function() {
        paste("HONO Extracted Data ", Sys.Date(), ".csv", sep="") # can replace Sys.Date() with input$filenameHONO
      },
      content = function(file) {
        write.csv(cycles_HONOExtractor, file)
      })
    
  })
  
  # Automatically does this once file uploaded
  # Display raw OH data file
  output$table1 <- renderDataTable({
    req(input$raw_data_file)
    data_retrieve$data
  })
  
  # Display raw HONO data file
  output$table1_HONOExtractor <- renderDataTable({
    req(input$raw_data_file_HONO)
    data_retrieve_HONO$data
  })
  
  
  ################################################################################
  
}


################################################################################
# Run the application 
shinyApp(ui = ui, server = server)



########################################
##
## Title: GCaMP_TLanalysis.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón
##
## Last Date modified: 2023-02-19
##
## Description: Imports intensity tables obtained from ExtractionIntensityValsTomGC.ijm, creates a single table with all
## experimentsincluding metadata. Pressure values are added to the table (if available).
## dF/F and dR/R metrics are calculated for each time point. A rounded time framework is established to account
## for different acquisition rates.
##
## Input files: Intensity tables and pressure tables. A table with info. on each experiment is also needed.
##
## Output files: A single table in CSV format that includes the metrics mentioned in the description.
##
## Comments:
##
## Publication:
##
##########################################
#Initialize-------

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

#Loading libraries -------
library(dplyr)
library(gtools)
library(ggplot2)
library(here)
library(palmerpenguins)
library(pspline)
library(purrr)
library(stringr)
library(tidyr)
library(tidyverse)


#switch to project directory
ProjectDir <- here()
setwd(ProjectDir)

#Part1 Analysis of metrics--------------------------------------------------
##Defining  paths
###Path intensity values

INDirFluo <- "Data/CaImagingDataAnalysis/TLanalysis/AllIntensityMeasurements/"#Indicate the folder with data.
Txtpattern <- ".txt"
FluoFiles <- list.files(path = INDirFluo, pattern = Txtpattern, recursive = TRUE)
print(FluoFiles)
###Path pressure values

INDirPressure <- "Data/CaImagingDataAnalysis/TLanalysis/PressureLogs/"
FilesLogCaIm <- list.files(path = INDirPressure, pattern = Txtpattern, recursive = TRUE)
print(FilesLogCaIm)

###Path experiment information table

FileTable <- read.table("Data/CaImagingDataAnalysis/ExperimentTables/ListExperimentsTLPressureCaImaging.csv",
                        header = TRUE, sep = ",")
FileTable$Pressure_Level <- as.character(FileTable$Pressure_Level)

###Path cells analysed-------------------------------------------------

CellTable <- read.table("Data/CaImagingDataAnalysis/ExperimentTables/Cells.csv",
                        header = TRUE, sep = ",")

##Defining  variables

ChannelName <- "GC"

##Defining the actual files to analyse

Listpotential <- paste(do.call(paste0, expand.grid(FileTable$Experiment_ID, sep = "_",
                                                   ChannelName, sep = "_", CellTable$Cells)))
List2Search <- intersect(paste(Listpotential, ".txt", sep = ""), FluoFiles)
List2Search <- str_split(List2Search, ".txt", simplify = TRUE)[, 1]
FinalFluoTable <- tibble(.rows = NULL)

##Importing intensity and pressure tables and compiling them into a single table------

for (m in seq_along(List2Search)) {
  ResultsTableSignalGC <- read.table(paste(INDirFluo, List2Search[m], ".txt", sep = ""),
                                     header = TRUE, na.strings = " ",
                                     col.names = c("Time", "RawFluo")) #If errors reading file appear try to replace all # signs with nothing (use sed -i "" 's/#//g' *.txt). Or recursively:  find . -type f -name "*.txt" -print0 | xargs -0 sed -i '' -e 's/#//g'
  IndexExp <- which(str_detect(List2Search[m], FileTable$Experiment_ID) ==    TRUE)
  ResultsTableSignalGC$Time <- FileTable[IndexExp, "Time_Frame"] * ResultsTableSignalGC$Time
  if (FileTable[IndexExp, "Ratiometric"] ==   "YES") {
    SplitList <- str_split(List2Search[m], "GC", simplify = TRUE) # list that stores name of file and cell to analyse.
    ResultsTableSignalTom <- read.table(paste(INDirFluo, FluoFiles[grep(paste(SplitList[, 1],
                                                                              "Tom",
                                                                              SplitList[, 2],
                                                                              sep = ""),
                                                                        FluoFiles)], sep = ""),
                                        header = TRUE,
                                        na.strings = " ",
                                        col.names = c("Time", "RawFluo"))  #If errors reading file appear try to replace all # signs with nothing (use sed -i "" 's/#//g' *.txt). Or recursively:  find . -type f -name "*.txt" -print0 | xargs -0 sed -i '' -e 's/#//g'
  } else {
    ResultsTableSignalTom <- ResultsTableSignalGC
    ResultsTableSignalTom$RawFluo <- 0
  }
  ExpID <- FileTable[IndexExp, "Experiment_ID"]
  PressureID <- FileTable[IndexExp, "Pressure_Level"]
  LarvaID <- FileTable[IndexExp, "Larva_ID"]
  CellID <- CellTable[which(str_detect(List2Search[m], CellTable$Cells) ==    TRUE), "Cells"]
  StimStart <- FileTable[IndexExp, "Prior_Duration"]
  Rmetric <- FileTable[IndexExp, "Ratiometric"]
  TypeExp <- FileTable[IndexExp, "Type_Experiment"]
  St <- FileTable[IndexExp, "Stage"]
  ###Adding pressure values##
  NewTable <- tibble(ExperimentID = ExpID,
                     AbsTime = ResultsTableSignalGC$Time,
                     RelTime = ResultsTableSignalGC$Time - StimStart,
                     GC = ResultsTableSignalGC$RawFluo,
                     Tom = ResultsTableSignalTom$RawFluo,
                     Cell = CellID,
                     Pressure_Level = PressureID,
                     LarvaID = LarvaID,
                     Ratiometric = Rmetric,
                     Type_Experiment = TypeExp,
                     Stage = St)
  ListIntervals <- 5000 + NewTable$AbsTime * 1000
  NewTable$PressVal <- NA
  IndexLogP <- which(str_detect(List2Search[m],
                                paste(str_replace(unlist(lapply(str_split(FilesLogCaIm,
                                                                          pattern = "/"),
                                                                tail,
                                                                n = 1L)),
                                                  ".txt", ""), "_", sep = "")) ==    TRUE)
  if (length(IndexLogP) > 0) {
    PressureTableCaI <- read.table(paste(INDirPressure, FilesLogCaIm[IndexLogP], sep = ""),
                                   na.strings = " ", colClasses = c("double",
                                                                   "NULL",
                                                                   "double",
                                                                   "NULL"),
                                   header = TRUE)
    PressureTableCaI <- tibble(PressureTableCaI)
    colnames(PressureTableCaI) <- c("Time", "RawPressureVolt")
    for (g in 1:(length(ListIntervals) - 1)) {
      NewTable[g, "PressVal"] <- 200 * mean(filter(PressureTableCaI,
                                                  between(Time,
                                                          ListIntervals[g],
                                                          ListIntervals[g + 1]))$RawPressureVolt)
    }
  } else {
    for (g in 1:(length(ListIntervals) - 1)) {
      NewTable[g, "PressVal"] <- NA
    }
  }
  FinalFluoTable <- bind_rows(FinalFluoTable, NewTable)
}

###Calculating prior fluorescence


PriorStimMean <- FinalFluoTable %>%
                  group_by(ExperimentID,
                           Cell,
                           Stage,
                           Type_Experiment) %>%
                  filter(PressVal ==   0 & RelTime <= 0 & RelTime >= -10) %>%
                  summarise(MeanGC = mean(GC, na.rm = TRUE),
                            MeanTom = mean(Tom,  na.rm = TRUE)
                            )
###grouping the table
PriorStimMean <- PriorStimMean %>%
                  group_by(ExperimentID,
                           Cell,
                           Stage,
                           Type_Experiment)
###Calculating dF and dR

FinalFluoTable <- FinalFluoTable %>%
                   group_by(ExperimentID,
                            Cell,
                            Stage,
                            Type_Experiment) %>%
                   mutate(dR = ((GC * PriorStimMean$MeanTom[cur_group_id()])
                                / (Tom * PriorStimMean$MeanGC[cur_group_id()]) - 1),
                          dF = (GC - PriorStimMean$MeanGC[cur_group_id()])
                          / PriorStimMean$MeanGC[cur_group_id()])

##Rounding time to the nearest second to calculate means for recordings at different times.
FinalFluoTable <- (FinalFluoTable %>%
                   mutate(RoundRelTime = case_when(RelTime < 0 ~ ceiling(RelTime),
                                                   RelTime >=   0 ~ floor(RelTime))
                          )
                 )

####Saving fluorescence table-------------------------------------------------
SavePath <- "Data/TablesResults/"
FlName <- "dRdFPressure_Caimaging_demo.csv"
write.csv(FinalFluoTable, paste(SavePath, FlName, sep = ""), row.names = FALSE)

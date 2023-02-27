########################################
##
## Title:CBFcalculation.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón
##
## Last Date modified: 2023-02-16
##
## Description: This script reads the maxima value in FFT images that were the output of the imageJ macro
## and integrates the output with the stimulus table provided. This table has information on each experiment
## (type of sample, stimulus,etc). The pressure stimulus for each time unit is also added by reading the pressure log.
##
## Input files:
##  1. List of pressure logs.
##  2. List of files (one file per bin) with maximum intensity values in FFT images.
## Output files:
##  1. List of CBF and beat state values for each bin.
##  2. List of Closure duration for each bin.
## Comments:
##  Input files included in this repo are only a sample of the entire dataset used for the analysis.
#########################################

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


# Part 1: Calculating CBFs--------



##Reading pressure log files-------
IndirPressureCil <- "Data/Behaviour/PressureLogs/CiliaryDynamics/"

TxtPattern  <-  ".txt"

FilesLog <-
  list.files(path = IndirPressureCil,
             pattern = TxtPattern, recursive  =  TRUE)


## Reading stimulus table-------
StimulusTable <- read.table("Data/InputTables/CBF_Experiments.csv", header = TRUE, sep = ",")
StimulusTable$Trial_ID <- str_replace(StimulusTable$Trial_ID, ".txt", "")
StimulusTable$Pressure_Level <- as.character(StimulusTable$Pressure_Level)
## Creating table for all CBFs and closures per experiment-------
InputPath <- "Data/Behaviour/RecordingsMeasurements/"
### Reading FFT files -------
FinalCBFClos <- tibble(.rows = NULL)
FinalCloseDur <- tibble(.rows = NULL)

for (a in seq_along(StimulusTable$Trial_ID)){
  TxtPattern  <- paste(StimulusTable[a, "Trial_ID"], "_Kymo-", sep = "") ##File format as output by CBFrollingAvgSubKimoUnsup.ijm
  Files <- list.files(path = InputPath, pattern = TxtPattern, recursive = TRUE)
  if (length(Files) > 0) {
    Files <- mixedsort((Files), decreasing = TRUE)
    NumFiles <- length(Files)
    FPS  <- StimulusTable[a, "Frame_rate"]
    TimeFrame  <- 1 / FPS
    Interval <- TimeFrame * StimulusTable[a, "Bin_size"]
    CBFClosureTrial <-
      tibble(Time = seq(TimeFrame * StimulusTable[a, "Bin_size"],
                        (TimeFrame * StimulusTable[a, "Bin_size"] * NumFiles),
                        by = Interval),
             RelTime = Time - StimulusTable[a, "Prior"],
             Trial_ID = StimulusTable[a, "Trial_ID"],
             Larva_ID = StimulusTable[a, "Larva_ID"],
             Genotype = StimulusTable[a, "Genotype"],
             State = StimulusTable[a, "State"],
             Transducer_factor = StimulusTable[a, "Transducer_factor"],
             Pressure_Level = StimulusTable[a, "Pressure_Level"],
             Plasmid = StimulusTable[a, "Plasmid"])
    SuperTable <-
      lapply(Files, function(X) {
        read.table(file = paste(InputPath, X, sep = ""),
                    header = TRUE,
                    na.strings = " ",
                    sep = "\t")
        })

    SuperTable <-
      tibble(R = map(SuperTable, "R"),
             Theta = map(SuperTable, "Theta"),
             FileName = unlist(lapply(str_split(Files, pattern = "/"), tail, n = 1L))) # Or the same: SuperTable %>% transpose() %>% as_tibble() %>% mutate(FileName = unlist(lapply(str_split(Files,TxtPattern = "/"), tail,n = 1L)))
    Thetavals <- str_split(StimulusTable[a, "Theta_range"], pattern = "-")
    SuperTable <-
      SuperTable %>%
      unnest(cols = c(R, Theta))  %>%
      group_by(FileName)  %>%
      summarise(CBF = FPS / mean(R[R >= 8 & R <= 17 & Theta < as.numeric(Thetavals[[1]][2]) &
                                     Theta > as.numeric(Thetavals[[1]][1])])) %>%
      mutate(Beat = case_when(is.na(CBF) ~ 0, TRUE ~ 1)) %>%
      slice(mixedorder(FileName, decreasing = TRUE))
    CBFClosureTrial <-
      CBFClosureTrial %>%
      mutate(Period = case_when(RelTime <= 0 ~ "Before",
                                RelTime > 0 & RelTime <= 30  ~ "During_1",
                                RelTime > 30 & RelTime <= StimulusTable[a, "Duration_Stimulus"]  ~ "During_2",
                                RelTime > StimulusTable[a, "Duration_Stimulus"] ~ "After"))
    CBFClosureTrial <- bind_cols(CBFClosureTrial, SuperTable)
    ClosureDurTrial <-
      CBFClosureTrial %>%
      group_by(Trial_ID, Pressure_Level, Period, Genotype) %>%
      summarise(StringBeatList = unlist(str_split(paste(as.character(Beat),
                                                        collapse = ""),
                                                  "1"))) %>% #making '0' strings separated by '1's.
      group_by(Trial_ID, Pressure_Level, Period, Genotype) %>%
      summarise(OnlyZero = StringBeatList[StringBeatList != ""]) %>%  #removing the empty strings.
      group_by(Trial_ID, Pressure_Level, Period, Genotype) %>%
      summarise(countzero = str_count(OnlyZero, "0") * Interval) %>% #counting the number of '0's in each string, translating it into the time interval between observations.
      group_by(Trial_ID, Pressure_Level, Period, Genotype) %>%
      nest(ListDurationClosure = countzero)
    ClosureDurTrial$Larva_ID <- StimulusTable[a, "Larva_ID"]
    ###Importing Pressure logs-------
    IndexPress <- which(str_detect(paste("\\b", str_replace(unlist(lapply(str_split(FilesLog,
                                                                                    pattern = "/"),
                                                                          tail, n = 1L)),
                                                            ".txt", ""), "\\b", sep = ""),
                                   StimulusTable[a, "Trial_ID"]) == TRUE)
    CBFClosureTrial$PressVal <- NA
    if (length(IndexPress) > 0) {
      PressureTableCil <- read.table(paste(IndirPressureCil, FilesLog[IndexPress], sep = ""), na.strings = " ",
                                     colClasses = c("double", "NULL", "double", "NULL"),
                                     header = TRUE)
      PressureTableCil <- tibble(PressureTableCil)
      colnames(PressureTableCil) <- c("Time", "RawPressureVolt")
      TimeInter <- 5000 + CBFClosureTrial$Time * 1000
      Tfactor <- StimulusTable[a, "Transducer_factor"]
      for (g in 1:(length(TimeInter) - 1)) {
        if (Tfactor == 2) {# For 0V baseline transducers.
          CBFClosureTrial[g, "PressVal"] <- 200 * mean(filter(PressureTableCil,
                                                              between(Time,
                                                                      TimeInter[g],
                                                                      TimeInter[g + 1]))$RawPressureVolt)
        } else { # For 0.5V baseline transducers.
          CBFClosureTrial[g, "PressVal"] <- (Tfactor * (mean(filter(PressureTableCil,
                                                                 between(Time,
                                                                         TimeInter[g],
                                                                         TimeInter[g + 1])
                                                                 )$RawPressureVolt) - 0.5)
                                             ) / 0.01
        }
      }
    }
    FinalCBFClos <- bind_rows(FinalCBFClos, CBFClosureTrial)
    FinalCloseDur <- bind_rows(FinalCloseDur, ClosureDurTrial)
  }
}

#Writing tables-------
SavePath <- "Data/TablesResults/"
FlNameCBFClos <- "CBF-Closure_CiliaryDynamics_demo.csv"
write.csv(FinalCBFClos, paste(SavePath, FlNameCBFClos, sep = ""), row.names = FALSE)

SavePath <- "Data/TablesResults/"
FlNameDurCl <- "ClosureDuration_CiliaryDynamics_demo.csv"
write.csv(FinalCloseDur %>% unnest(ListDurationClosure), paste(SavePath, FlNameDurCl, sep = ""), row.names = FALSE)

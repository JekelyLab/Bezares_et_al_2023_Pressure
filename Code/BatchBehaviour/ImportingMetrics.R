########################################
##
## Title:ImportingMetrics.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón
##
## Last Date modified: 2023-02-16
##
## Description: Script for importing metrics and pressure readings for each experiment.
##
## Input files:
##  1.Metric tables for each experiment (output files of script MeasuresTracks.R)
##  2.Pressure logs
##  3.Particle count for each time unit and for each bin across the FOV (optional, set PrtCountON to TRUE).
##    Output of ScriptParticleCounter.ijm
## Output files:
##  Table with each metric for each file for each time bin. Pressure value is also added to each corresponding row.
## Comments:
##
##########################################

#Initialize----
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

#Setting up libraries----
library(dplyr)
library(ggplot2) # For plotting results
library(here)
library(pspline) # For calculating rates of change
library(readr)
library(reshape2) # for creating melted tables
library(stringr)
library(tidyr)

#switch to project directory
ProjectDir  <- here()
setwd(ProjectDir)

#Part1 Analysis of metrics-----
##Defining  input directories ####
###coordinate files####
INDirMetric <- "Data/Behaviour/RecordingsMeasurements/BatchExperiments/MetricsTables/" #Indicate the folder with all the Measurements from MeasuresTracks.R.

SuffPattern <- "400-tracks.txt"
Files <- list.files(path = INDirMetric,
                    pattern = SuffPattern,
                    recursive = TRUE)

###Optional particle count variables (set PrtCountON =TRUE if needed)----
NumBins <- 5
PrtCountON <- FALSE
if (PrtCountON) {
  DirParticle <- "Data/Behaviour/ParticleCountTablesAllLong/"
  DP <- list.files(path = DirParticle,
                   pattern = ".csv",
                   recursive = TRUE)
}

INDirPressure <- "Data/Behaviour/PressureLogs/BatchExperiments/"
TxtPattern <- ".txt"

FilesPressLog <- list.files(path = INDirPressure,
                            pattern = TxtPattern,
                            recursive = TRUE)

StartProgramdelay <- 5

## Reading stimulus table####
StimulusTable <- read.table("Data/InputTables/StepWT3dpfBatchExperiments.csv", header = TRUE, sep = ",")
StimulusTable$Trial_ID <- str_replace(StimulusTable$Trial_ID, ".txt", "")
StimulusTable$Stimulus_Level <- as.character(StimulusTable$Stimulus_Level)
StimulusTable$Category_stimulus <- as.character(StimulusTable$Category_stimulus)
##Define the interval to be analyzed.#####

##Tibble storing values#####

CompleteTable <- tibble(.rows = NULL)

##Extraction, correction and format of results tables####
#if files need to be renamed to match Stimulus Table names use this command: find . -type f -exec rename 's/YOUR_STRING_TO_REPLACE//' '{}' \;
for (l in seq_along(StimulusTable$Trial_ID)) { # this FOR loops through all the files per pressure value tested.
      Fileindex <- grep(paste(paste(do.call(paste0,
                                            expand.grid(StimulusTable[l, "Trial_ID"],
                                                        c(".txt", "_"),
                                                        sep = "")
                                            )
                                    ),
                              collapse = "|"),
                        Files) #Add characters to the list of suffixes as needed in the paste argument in case needed.
    if (length(Fileindex) > 0) {
      FPS <- StimulusTable[l, "Frame_rate"]
      TimeFrame <- 1 / FPS
      Interval <- TimeFrame * StimulusTable[l, "Bin_size"]
      FullPathFile <- paste(INDirMetric, Files[Fileindex], sep = "")
      ResultsTable <- read.table(FullPathFile,
                                 header = TRUE,
                                 sep = ",",
                                 na.strings = c(" ", "NA"))  #If errors reading file appear try to replace all # signs with nothing (use sed -i "" 's/#//g' *.txt). Or recursively:  find . -type f -name "*.txt" -print0 | xargs -0 sed -i '' -e 's/#//g'
      ResultsTable <- tibble(ResultsTable)
      #FullDurationSec <- as.integer(StimulusTable[l,"Prior_Duration"])+as.integer(StimulusTable[l,"Stimulus_Duration"])+as.integer(StimulusTable[l,"Post_Duration"])
      ResultsTable$Trial_ID <- StimulusTable[l, "Trial_ID"]
      ResultsTable$Batch_ID <- StimulusTable[l, "Batch_ID"]
      ResultsTable$Genotype <- StimulusTable[l, "Genotype"]
      ResultsTable$StimulusDuration <- StimulusTable[l, "Stimulus_Duration"]
      ResultsTable$Type_Experiment <- StimulusTable[l, "Type_Experiment"]
      ResultsTable$Category_stimulus <- StimulusTable[l, "Category_stimulus"]
      ResultsTable$Fraction_increase <- StimulusTable[l, "Fraction_increase"]
      ResultsTable$Basal_pressure <- StimulusTable[l, "Basal_pressure"]
      ResultsTable$Time <- seq(TimeFrame * StimulusTable[l, "Bin_size"],
                               (TimeFrame * StimulusTable[l, "Bin_size"] * length(ResultsTable$Frame)), by = Interval)
      ResultsTable$RelTime <- ResultsTable$Time - (as.integer(StimulusTable[l, "Prior_Duration"]))
      ResultsTable$Stimulus_Level <- as.character(StimulusTable[[l, "Stimulus_Level"]])
      ###Importing Pressure logs####
      IndexPress <- which(str_detect(paste("\\b",
              str_replace(
                unlist(lapply(str_split(FilesPressLog,
                                        pattern = "/"), tail, n = 1L)),
                ".txt", ""), "\\b", sep = ""),
        paste(StimulusTable[l, "Trial_ID"], "\\b", sep = "")) == TRUE)
      ResultsTable$PressVal <- NA
      if (length(IndexPress) > 0) {
        PressureTable <- read_table(paste(INDirPressure,
                                          FilesPressLog[IndexPress],
                                          sep = "")) #colClasses = c("double","NULL","double","NULL"),,na.strings = " ",header=TRUE
        PressureTable <- PressureTable %>% select(time, PresV)
        colnames(PressureTable) <- c("Time", "RawPressureVolt")
        TimeInter <- StartProgramdelay * 1000 + (ResultsTable$Time - 1) * 1000
        TransFactor <- StimulusTable[l, "Transducer_factor"]
        for (g in 1:(length(TimeInter) - 1)) {
          if (TransFactor == 2) {# For 0V baseline transducers.
            ResultsTable[g, "PressVal"] <- 200 * mean(filter(PressureTable,
                                                             between(Time,
                                                                     TimeInter[g],
                                                                     TimeInter[g + 1]))$RawPressureVolt)
          } else { # For 0.5V baseline transducers.
            ResultsTable[g, "PressVal"] <- (TransFactor * (mean(filter(PressureTable,
                                                                  between(Time,
                                                                          TimeInter[g],
                                                                          TimeInter[g + 1])
                                                                  )$RawPressureVolt) - 0.5)) / 0.01
          }
        }
      }
      ###Importing particle distribution####
      if (StimulusTable[l, "Particles_Counted"] == "YES") {
        DistIndex <- grep(paste(paste(do.call(paste0,
                                              expand.grid(StimulusTable[l, "Trial_ID"],
                                                          c(".avi"), sep = "")))),
                          DP)
        TablesDP <- lapply(paste(DirParticle, DP[DistIndex], sep = ""),
                           function(X) {
          read.table(file = X,
                     na.strings = " ",
                     sep = ",")
                             }
          )
        TablesDP <- tibble(data.table::rbindlist(TablesDP))
        ###Importing particle count for each time point for the # bins used.
        ResultsTable <- bind_cols(ResultsTable, (TablesDP %>%
                                                   select(Slice, Count)  %>%
                                                   nest(ParticleCount = Count) %>%
                                                   select(ParticleCount)))
        ###Adding bin no identity as a coulum.
        TablesDP$BinNo <- rep(rep(1:NumBins, by = 1),
                              length(DP[DistIndex]))
        ResultsTable <- bind_cols(ResultsTable, (TablesDP %>%
                                                select(Slice, BinNo)  %>%
                                                nest(Bins = BinNo) %>%
                                                select(Bins)))
        ResultsTable <- ResultsTable %>%
          unnest(c(Bins, ParticleCount)) %>%
          group_by(RelTime) %>%
          mutate(LarvaD_Pc = 100 * Count / sum(Count)) %>%
          nest(ParticleCount = Count, Bins = BinNo, PartPc = LarvaD_Pc)
      } else {
        ResultsTable$ParticleCount <- NA
        ResultsTable$Bins <- NA
        ResultsTable$PartPc <- NA
      }
      MeanPriors <- ResultsTable %>%
        ungroup() %>%
        filter(RelTime < 0) %>%
        relocate(PressVal, .before = Avg_Speed) %>%
        summarise(across(PressVal:Num_Tracks_Down,
                         mean,
                         na.rm = TRUE))
      ResultsTable <- bind_cols(ResultsTable, ResultsTable %>%
                                  relocate(PressVal, .before = Avg_Speed) %>%
                                  summarise(across(PressVal:Num_Tracks_Down,
                                                 ~.x - MeanPriors[[cur_column()]],
                                                 .names = "Corr_{.col}"),
                                          PressInc = ((PressVal + 1000) *
                                                        100 / (MeanPriors[["PressVal"]] + 1000)) -
                                            100) %>%
                                select(starts_with("Corr"), PressInc))
      ###Reordering columns####
      ResultsTable <- ResultsTable %>%
        relocate(Trial_ID,
                 Batch_ID,
                 Time,
                 Frame,
                 RelTime,
                 Stimulus_Level,
                 Genotype,
                 StimulusDuration,
                 Type_Experiment,
                 Category_stimulus,
                 Fraction_increase,
                 Basal_pressure,
                 Bins)
      CompleteTable <- bind_rows(CompleteTable, ResultsTable)
    }
}

#Writing tables to file-------
  SavePath <- "Data/TablesResults/"
  FullMetricsTable <- "Stepmetrics3dpf_demo.csv"
  write.csv(CompleteTable %>% unnest(c(ParticleCount, Bins, PartPc)),
            paste(SavePath, FullMetricsTable, sep = ""),
            row.names = FALSE)

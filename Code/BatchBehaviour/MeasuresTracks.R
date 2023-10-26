########################################
##
## Title:MeasuresTracks.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón
##
## Last Date modified: 2023-02-16
##
## Description:Script to import table formatted by ReadMtrack2files.R and compute a list of metrics:
##  -speed
##  -vertical movement
##  -vertical displacement
##  -Tortuosity
##  -Num. of upward and downward tracks
##  -horizontal and vertical Path straightness
## Input files: XY coordinates in R-readable format.
##
## Output files: Tables for each experiment with computed metrics for each time unit.
##
## Comments: The script 'TrackingMetricsFunctions.R' actually computes all of the metrics.
##
##########################################

#Initialize----
  rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
  gc() #free up memory and report the memory usage.

#Setting up libraries----

  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  library(readr)
  source("Code/BatchBehaviour/TrackingMetricsFunctions.R")

## enter directory for importing formtted table of XY coordinates:------
TableINdirXY  <- "Data/Behaviour/RecordingsMeasurements/BatchExperiments/ExtractedTrackCoordinates/"


FilePathsXY <- fs::dir_ls(TableINdirXY, regexp = "\\.txt$")
TableOUtdirMetrics <- "Data/Behaviour/RecordingsMeasurements/BatchExperiments/MetricsTables/"

## Reading stimulus table-----
StimulusTable <- read.table("Data/InputTables/StepWT3dpfBatchExperiments.csv", header = TRUE, sep = ",")
StimulusTable$Trial_ID <- str_replace(StimulusTable$Trial_ID, ".txt", "")
StimulusTable$Stimulus_Level <- as.character(StimulusTable$Stimulus_Level)
StimulusTable$Category_stimulus <- as.character(StimulusTable$Category_stimulus)


##Parameters----
MaxTracks2Analyse <- 400 #maximum number of tracks to analyse.
## read in data and prepare separate wide tables ---------------------------

Parameters <- tibble(.rows = NULL)
Timestamp <- Sys.Date()
write.csv(Parameters, paste(TableOUtdirMetrics, "Parameters_run_", Timestamp, ".txt", sep = ""),
          append = TRUE,
          row.names = FALSE)

for (j in seq_along(FilePathsXY)) {
  IndexFile <- which(str_detect(unlist(lapply(str_split(FilePathsXY[[j]],
                                                        pattern = "/"),
                                              tail, n = 1L)),
                                StimulusTable$Trial_ID) == TRUE)
  if (length(IndexFile) > 0) {
    Pxmm <- str_split(StimulusTable[IndexFile, "Pxmm"], "-")
    Mm <- as.numeric(Pxmm[[1]][2]) #distance in mm corresponding to the px value in px variable
    Px <- as.numeric(Pxmm[[1]][1]) #number of pixels corresponding to mm in the mm variable.
    Mmpx1 <- Mm / Px #calculating the mm per pixel
    FrameRate <- StimulusTable[IndexFile, "Frame_rate"]
    XYcoords <- read.csv(FilePathsXY[[j]])  #Reading the XYcoord file.
    XYcoords <- tibble(XYcoords)
    FinaltabSplit <- XYcoords %>%
      group_by(TimeSec) %>%
      drop_na() %>%
      filter(if (length(unique(TrackID)) > MaxTracks2Analyse)  # if there are more tracks than the limit number, only a subsample of the limit size is taken at random.
        TrackID %in% sample(unique(TrackID), MaxTracks2Analyse)
        else TrackID %in% unique(TrackID)) %>%
      ungroup() %>%
      group_split(TimeSec) #The XYcoord table of each trial is split by second choosing pre-specified number of tracks (or as many as the file has).
    MetricsTable <- NULL
    MetricsTable$TimeSec <- unique(XYcoords$TimeSec)
    MetricsTable$Filename <- unique(XYcoords$FileName)
    MetricsTable <- as_tibble(MetricsTable)
    MetricsTable$Avg_Speed <- NA
    MetricsTable$Avg_Y_displacement <- NA
    MetricsTable$Avg_Y_displacement_Up <- NA
    MetricsTable$Avg_Y_displacement_Down <- NA
    MetricsTable$Avg_Y_movement <- NA
    MetricsTable$Num_Tracks_Up <- NA
    MetricsTable$Num_Tracks_Down <- NA
    MetricsTable$Tortuosity <- NA
    MetricsTable$Straightness_X <- NA
    MetricsTable$Straightness_Y <- NA
    for (k in seq_along(FinaltabSplit)) {  # Loop through each bin (usually each second) of the file.
      if (nrow(FinaltabSplit[[k]]) > 1) {
        Metrics <- MetricCalc(FinaltabSplit[[k]], Mmpx1, FrameRate) #Each of the metrics is calculated for each of the second in the trial.
        MetricsTable[k, "Avg_Speed"] <- Metrics[1]
        MetricsTable[k, "Avg_Y_displacement"] <- Metrics[2]
        MetricsTable[k, "Avg_Y_displacement_Up"] <- Metrics[3]
        MetricsTable[k, "Avg_Y_displacement_Down"] <- Metrics[4]
        MetricsTable[k, "Avg_Y_movement"] <- Metrics[5]
        MetricsTable[k, "Num_Tracks_Up"] <- Metrics[6]
        MetricsTable[k, "Num_Tracks_Down"] <- Metrics[7]
        MetricsTable[k, "Tortuosity"] <- Metrics[8]
        MetricsTable[k, "Straightness_X"] <- Metrics[9]
        MetricsTable[k, "Straightness_Y"] <- Metrics[10]
      } else {  # In case, that particular bin does not have any track to analyse.
        MetricsTable[k, "Avg_Speed"] <- 0
        MetricsTable[k, "Avg_Y_displacement"] <- 0
        MetricsTable[k, "Avg_Y_displacement_Up"] <- 0
        MetricsTable[k, "Avg_Y_displacement_Down"] <- 0
        MetricsTable[k, "Avg_Y_movement"] <- 0
        MetricsTable[k, "Num_Tracks_Up"] <- 0
        MetricsTable[k, "Num_Tracks_Down"] <- 0
        MetricsTable[k, "Tortuosity"] <- 0
        MetricsTable[k, "Straightness_X"] <- 0
        MetricsTable[k, "Straightness_Y"] <- 0
      }
    }
    colnames(MetricsTable) <- c("Frame",
                               "File_name",
                               "Avg_Speed",
                               "Avg_Y_displacement",
                               "Avg_Y_displacement_Up",
                               "Avg_Y_displacement_Down",
                               "Avg_Y_movement",
                               "Num_Tracks_Up",
                               "Num_Tracks_Down",
                               "Tortuosity",
                               "Straightness_X",
                               "Straightness_Y")
    write.csv(MetricsTable, paste(TableOUtdirMetrics,
                                  paste("Results_",
                                        unique(XYcoords$Trial),
                                        "_",
                                        as.character(MaxTracks2Analyse),
                                        "-tracks.txt",
                                        sep = ""),
                                  sep = ""),
              row.names = FALSE)
    Parameters[nrow(Parameters) + 1, "mm"] <- Mm
    Parameters[nrow(Parameters) + 1, "px"] <- Px
    Parameters[nrow(Parameters) + 1, "frame_rate"] <- FrameRate
    Parameters[nrow(Parameters) + 1, "file"] <- FilePathsXY[[j]]
    write.csv(Parameters, paste(TableOUtdirMetrics, "Parameters_run_",
                                Timestamp, ".txt",
                                sep = ""),
              append = TRUE,
              row.names = FALSE)
    }
}

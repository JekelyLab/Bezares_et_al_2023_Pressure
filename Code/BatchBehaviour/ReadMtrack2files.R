########################################
##
## Title:ReadMtrack2files.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón
##
## Last Date modified: 2023-02-16
##
## Description: Read and rearrange csv files created by Mtrack2 Fiji plugin through
## the script 'BatchBehaviourTrackExtraction.ijm'.
##
## Input files: The script will read in .res files that contain XY coordinates of tracked objects.
## There is a .res file for each time unit. Tracks are in Mtrack2 format.
##
## Output files: Tables with XY coordinates in a R-readable format for each time unit.
##
## Comments:
##
##########################################

#Setting up libraries----
library(dplyr)
library(ggplot2)
library(here)
library(tidyverse)
library(readr)


#Initialize----
  rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
  gc() #free up memory and report the memory usage.

  #switch to project directory
  ProjectDir <- here()
  setwd(ProjectDir)
  ## enter OUT and IN directories:
  OutDir <- "Data/Behaviour/RecordingsMeasurements/BatchExperiments/ExtractedTrackCoordinates/"
  MtrackFileDir <- "Data/Behaviour/RecordingsMeasurements/BatchExperiments/Mtrack2OutFiles/"
  ## Sourcing custom-written functions
  source("Code/BatchBehaviour/Trackextraction.R")
  source("Code/BatchBehaviour/WriteSplittabs.R")
  ## read in data and prepare separate wide tables ---------------------------
FilePaths <- fs::dir_ls(MtrackFileDir, regexp = "\\.res$", recurse = TRUE)
# check they are all there and that only your data files for processing are present.
print(FilePaths)
FileList <- list()
TrialName <- ""
OldTrialName <- ""
Finaltab <- tibble(.rows = NULL)
Time <- 1
for (j in seq_along(FilePaths)) {
    TrialName <- head(unlist(str_split((tail(unlist(str_split(FilePaths[[j]], pattern = "/")), n = 1)),
                                       pattern = ".avi")),
                      n = 1)
    if (TrialName != OldTrialName) {
      Time <- 1
    }
    FileList[[j]] <- read.table(file = FilePaths[[j]],
                                sep = "\t",
                                header = TRUE,
                                na.strings = c(""),
                                fill = TRUE)
    NameFile <- tail(unlist(str_split(FilePaths[[j]], pattern = "/")), n = 1)
    ###Commands for cleaning up the table##
    if (ncol(FileList[[j]]) > 1) {
      FlagCol <- grep("Flag", colnames(FileList[[j]]), invert = TRUE)
      FileList[[j]] <- FileList[[j]][, FlagCol]
      TrackRow <- grep("Tracks", FileList[[j]][, 1], invert = TRUE)
      FileList[[j]] <- FileList[[j]][TrackRow, ]
      Tracksum <- grep("Track ", FileList[[j]][, 1])
      FileList[[j]] <- FileList[[j]][seq(1, Tracksum - 1), ]
      Frames <- length(unique(FileList[[j]][, "Frame"]))
      Ngroups <- nrow(FileList[[j]]) / Frames
      FileList[[j]] <- FileList[[j]] %>%
        mutate(index = rep(1:Ngroups,
                           each = length(unique(FileList[[j]][, "Frame"])))) #[seq(.data)])
      Splittable <- FileList[[j]] %>% group_split(index)
      TibleTrack <- Trackextraction(Time, Splittable)
      Pivotedtab <- TibleTrack %>%
        pivot_longer(!Frame, names_to = "TrackID", values_to = "XYPos") %>%
        mutate(XYPos = ifelse(XYPos == "NA,NA", NA, XYPos),
               TimeSec = Time,
               FileName = NameFile,
               Trial = TrialName) %>%
        drop_na() %>%
        arrange(TrackID)
      Finaltab <- bind_rows(Finaltab, Pivotedtab)
      OldTrialName <- TrialName
      print(FilePaths[[j]])
      print(Time)
      Time <- Time + 1
    } else {
      Frame <- "1"
      TrackID <- ""
      XYPos <- "0,0"
      TimeSec <- Time
      FileName <- NameFile
      Trial <- TrialName
      Pivotedtab <- tibble(Frame, TrackID, XYPos, TimeSec, FileName, Trial)
      Finaltab <- bind_rows(Finaltab, Pivotedtab)
      print(Time)
      Time <- Time + 1
      OldTrialName <- TrialName
      print(FilePaths[[j]])
    }
}


Finaltab <- Finaltab %>% separate(XYPos, sep = ",", into = c("X", "Y"))
Finaltab$X <- as.numeric(Finaltab$X)
Finaltab$Y <- as.numeric(Finaltab$Y)


SplitFinal <- Finaltab %>% group_split(Trial)
#Write to file------
WriteSplittabs(OutDir, SplitFinal)

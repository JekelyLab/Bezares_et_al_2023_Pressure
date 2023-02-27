########################################
##
## Title:VolumecPRCanalysis.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón
##
## Last Date modified: 2023-02-20
##
## Description: It searches for matching files and makes a table with all the volume measurments appended to
## the information of the sample, genotype,stage etc.
##
## Input files: List of files with the volume measurements.
##
## Output files: A single table with the volume measurments for each experiment.
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

#Initialize-------

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

#switch to project directory
  ProjectDir <- here()
  setwd(ProjectDir)
INDirMetric <- "Data/cPRCMorphology/" #Indicate the folder with data.
Txtpattern <- ".txt"

FilesNITVols <- list.files(path = INDirMetric, pattern = Txtpattern, recursive = TRUE)

ScansTable <- read.csv("Data/InputTables/NITAcTubScanTable.csv", header = TRUE, sep = ",")


for (l in seq_along(ScansTable$File_name)) {
  Fileindex <- grep(ScansTable[l, "File_name"], FilesNITVols)
  if (length(Fileindex) > 0) {
    TableVolume <- read.table(FilesNITVols[Fileindex], header = TRUE, sep = "\t", na.strings = " ")
    ScansTable[l, "Volume"] <- sum(TableVolume$Volume)
  }
}

#Writing tables to file-------
  SavePath <- "Data/TablesResults/"
  FullMetricsTable <- "NITAcTub_VolcPRC_WT-Cops_Table_demo.csv"
  write.csv(ScansTable, paste(SavePath, FullMetricsTable, sep = ""), row.names = FALSE)

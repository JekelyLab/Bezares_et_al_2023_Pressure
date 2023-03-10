########################################
##
## Title:FigureSupplement5.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón
##
## Last Date modified: 2023-02-25
##
## Description: Code to generate Figure Supplement 6. Further examples of cPRCs activation by pressure.
##
## Input files: 
##
## Output files:
##
## Comments:
##
## Publication:
##
##########################################
#Initialize----
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

# Loading libraries--------------------------------------------------
library(cowplot)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(here)
library(png)


#switch to project directory
ProjectDir  <- here()
setwd(ProjectDir )

# generate figure composite panel grid ------------------------------------

  Img1 <- readPNG("Manuscript/pictures/CaimagingNovs750mbpanel_supp.png")
  panel_GC_noPvsP <-  ggdraw() + draw_image(Img1)
  

  
  FigSupp5 <- 
    panel_GC_noPvsP 
  
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_5.pdf", 
    FigSupp5, width = 2305, height = 2305,
    units = "px"
  )
  
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_5.png", 
    FigSupp5, width = 2305, height = 2305,
    units = "px"
  )


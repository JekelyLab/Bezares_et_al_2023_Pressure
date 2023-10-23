########################################
##
## Title:Figure3-FigureSupplement1.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón
##
## Last Date modified: 2023-02-25
##
## Description: Code to generate Figure3-FigureSupplement2. WT vs Cops comparison cPRC ultrastructure.
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
CbbPalette <- c( "#56B4E9", "#009E73",  "#CC79A7", "#E69F00", "#0072B2","#F0E442","#000000", "#D55E00")



# Loading libraries--------------------------------------------------

library(png)
library(patchwork)
library(rbokeh)
library(tidyverse)

#switch to project directory
ProjectDir  <- here()
setwd(ProjectDir)

# generate figure composite panel grid ------------------------------------


imgEM <- readPNG("Manuscript/pictures/EMcPRC_suppnolabs.png")
Fontsize = 10
panel_EMWTvsCops <-ggdraw() + 
    draw_image(imgEM, scale = 1) +
    draw_label(paste("1", "\u00B5", "m", sep = ""), 
               x = 0.31, y = 0.2, size = Fontsize, color = "black") +
    draw_label(paste("1", "\u00B5", "m", sep = ""), 
               x = 0.86, y = 0.2, size = Fontsize, color = "black") +
    draw_label(expression(italic(paste("c-ops-",1^{"∆8/∆8"}))),
               x = 0.75, y = 0.84, size = Fontsize,color = "black") +
    draw_label(expression(italic(paste("WT"))),
               x = 0.2, y = 0.84, size = Fontsize,color = "black") 
    
  layout <- "
  AAAAAA
  AAAAAA
  "
  
  FigSupp8 <- panel_EMWTvsCops +
    plot_layout(design = layout) +
    plot_annotation(tag_levels = list(
      c("A"))) &
    theme(plot.tag = element_text(size = 12, face = "plain"))
  
  
    
  ggsave(
    filename = "Manuscript/Figures/Figure3-FigureSupplement2.pdf", 
    FigSupp8, width = 1850, height = 1000,
    units = "px"
  )
  ggsave(
    filename = "Manuscript/Figures/Figure3-FigureSupplement2.png", 
    FigSupp8, width = 1850, height = 1000,
    units = "px"
  )



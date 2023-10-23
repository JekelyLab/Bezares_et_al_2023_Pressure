########################################
##
## Title:FigureSupplement2.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón
##
## Last Date modified: 2023-02-25
##
## Description: Code to generate Figure Supplement 2.Pressure increase with "real" hydrostatic pressure.
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



#Loading libraries--------------------------------------------------

library(cowplot)
library(ggplot2)
library(ggpubr)
library(here)
library(magick)
library(png)
library(patchwork)
library(rstatix)
library(stringr)
library(tidyverse)
library(viridis)
library(zoo)
#Sourcing functions
source("Code/Figures/Functions_figures.R")




#switch to project directory
project_dir <- here()
setwd(project_dir)
#define ggplot theme--------------------------------------------------

theme_plot <- theme(
  axis.text.x = element_text(size = 7, angle = 90),
  axis.text.y = element_text(size = 7),
  legend.text = element_text(size = 7),
  legend.title = element_text(size = 9),
  legend.title.align = 0.5,
  legend.spacing.x = unit(0.2, "mm"),
  legend.box.spacing = margin(0, 0, 0, 0),
  axis.title = element_text(size = 10),
  panel.background = element_blank()
)





##1dpf--------------------------------------------------

### read data

TableIndStep1dpf <- read_csv("Data/SourceData_elife/Figure1-SourceData5.csv")


###define pressure levels

TableIndStep1dpf$Pressure_Level <- 
  factor(TableIndStep1dpf$Pressure_Level,
         levels =  c("0","32.5", "85","160",
                     "237.5", "556", "778" , "988" ))


##Visually assessing track number to decide cut off
cutoff1dpf = 50

(
  ggplot(TableIndStep1dpf,aes(x = RelTime, y = Num_Tracks_Up + Num_Tracks_Down)) +
    geom_line() +
    geom_hline(yintercept = cutoff1dpf, color = "red") +
    facet_wrap(~Pressure_Level)
  )


###Compute Averages of each metric

TableAvgStep1dpf <- TableIndStep1dpf %>% 
  filter(Num_Tracks_Up + Num_Tracks_Down >= cutoff1dpf) %>%
  group_by(Pressure_Level,
           Type_Experiment,
           Genotype,
           RelTime) %>% 
  summarise(across(Avg_Speed:Corr_Num_Tracks_Down,
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE),
                        se = ~sd(.x/sqrt(length(.x)), na.rm = TRUE)))
  )

TableAvgStep1dpf$n_trials <- count(TableIndStep1dpf %>%
                                     filter(Num_Tracks_Up + Num_Tracks_Down >= cutoff1dpf) %>%
                                     group_by(Pressure_Level,Type_Experiment,Genotype,RelTime))$n
distinct(TableAvgStep1dpf,n_trials)


###Plots---------------

####Pressure logs


PlotAveragePressure1dpf <- (
  ggplot(TableAvgStep1dpf,
         aes(RelTime,PressVal_mean,col = Pressure_Level)) +
    theme_plot +
    geom_line() +
    geom_hline(yintercept = 0) +
    scale_colour_viridis(discrete = TRUE, 
                         option = 'D', 
                         direction = 1,
                         end = 0.5) +
    scale_x_continuous(breaks = seq(0,100,20),limits = c(-20,230)) +
    scale_y_continuous(breaks = seq(0,1000,100),limits = c(-100,1000)) +
    coord_cartesian(xlim = c(-10,80), ylim = c(0,1000)) + 
    geom_vline(xintercept = 0,color = "black") +
    geom_errorbar(aes(ymin = PressVal_mean, ymax = PressVal_mean+PressVal_se)) +
    geom_vline(xintercept = 60,color = "gray",linetype = 2) +
    labs(x = "time after stimulus (s)",
         y = "pressure (mb)",
         color =str_wrap("set pressure (mbar)", width = 15))  +
    guides(color = guide_legend(keyheight = 0.3))
)

PlotAveragePressure1dpf

##### Saving PNG of plot


ggsave("Manuscript/pictures/PanelAvgPressure_step1dpf.png",
       plot = PlotAveragePressure1dpf,
       width = 2200,height = 2000,units = "px",device = "png")




### Plot displacement 1 dpf

PlotDispAvg1dpf <- (ggplot(
  TableAvgStep1dpf,
  aes(RelTime, Avg_Y_displacement_mean, col = Pressure_Level)) +
    theme_plot +
    guides(colour = "none") +
    geom_line() +
    scale_colour_viridis(
      discrete = TRUE,
      option = "D",
      direction = 1,
      end = 0.5
    ) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(0,100,20),limits = c(-20,130)) +
    coord_cartesian(xlim = c(-10,80), ylim = c(-0.8,0.2)) +
    scale_y_continuous(breaks = seq(-1,2,0.2),limits = c(-1,2)) +
    geom_vline(xintercept = 0, color = "black") +
    geom_errorbar(aes(ymin = Avg_Y_displacement_mean, ymax = Avg_Y_displacement_mean + Avg_Y_displacement_se)) + # error bar is only shown on top
    labs(
      x = "time after stimulus (s)",
      y = " vertical displacement (mm/s)",
      color = str_wrap("pressure (mb)", width = 20)
    ) +
    geom_vline(
      xintercept = 60, color = "gray",
      linetype = 2
    )# +
    #guides(color = guide_legend(keyheight = 0.3))
)
PlotDispAvg1dpf


#### Saving PNG of plot


ggsave("Manuscript/pictures/PanelTvsAVGDisp_step1dpf.png",
       width = 1000, height = 800, units = "px", device = "png"
)


#Hose----------
### read data

  TableIndHose3dpf <- read_csv("Data/SourceData_elife/Figure1-SourceData6.csv")
  
  
  ##Visually assessing track number to decide cut off
  cutoffHose = 50
  
  (
    ggplot(TableIndHose3dpf,aes(x = RelTime, y = Num_Tracks_Up + Num_Tracks_Down)) +
      geom_line() +
      geom_hline(yintercept = cutoffHose, color = "red") +
      facet_wrap(~Pressure_Level)
  )
  
  ###define pressure levels
  
  TableIndHose3dpf$Pressure_Level <- factor(TableIndHose3dpf$Pressure_Level,levels =  c(as.character(seq(10,150,10)),"200"))

  ### Smoothing curves to get maximal values
  TableIndHose3dpf <- 
    TableIndHose3dpf %>%
    group_by(Trial_ID) %>% 
    mutate(across(Avg_Speed:Corr_Num_Tracks_Down,
                  ~rollmean(rollmean(.x, k = 5, na.pad = T),
                            k = 5, na.pad = T), 
                  .names = "STA5_{.col}"))
  
  
  ###Calculating aggregate tables
  ####AVG. values
  
  TableAvgHose3dpf <- TableIndHose3dpf %>% 
    filter(Num_Tracks_Up + Num_Tracks_Down >= cutoffHose) %>%
    group_by(Pressure_Level,Type_Experiment,Genotype,RelTime) %>% 
    summarise(across(Avg_Speed:STA5_Corr_Num_Tracks_Down,list(mean = ~mean(.x, na.rm = T),
                                                         sd = ~sd(.x,na.rm = T),
                                                         se = ~sd(.x/sqrt(length(.x)),na.rm = T))))
  TableAvgHose3dpf$n_trials <- count(TableIndHose3dpf %>% 
                                       filter(Num_Tracks_Up + Num_Tracks_Down >= cutoffHose) %>%
                                       group_by(Pressure_Level,Type_Experiment,Genotype,RelTime))$n
  unique(distinct(TableAvgHose3dpf,n_trials)$n_trials)
  
  ####Max. values
  TableMaxHose3dpf <- 
    TableIndHose3dpf %>% 
    filter(Num_Tracks_Up + Num_Tracks_Down >= cutoffHose &
             RelTime>0 & RelTime <=  StimulusDuration) %>%
    group_by(Trial_ID,
             Pressure_Level,
             Type_Experiment,
             Genotype,
             Batch_ID) %>%
    summarise(across(Avg_Speed:STA5_Corr_Num_Tracks_Down,
                     ~max(.x,na.rm = TRUE),.names = "max_{.col}"))
  
  TableMaxHose3dpf["max_PressVal"][TableMaxHose3dpf["max_PressVal"] == -Inf] <- NA
  TableMaxHose3dpf["max_STA5_PressVal"][TableMaxHose3dpf["max_STA5_PressVal"] == -Inf] <- NA
  
  AvgMaxHose3dpf <- 
    TableMaxHose3dpf %>%
    group_by(Pressure_Level,Type_Experiment,Genotype) %>% 
    summarise(across(max_Avg_Speed:max_STA5_Corr_Num_Tracks_Down,
                     list(mean = ~mean(.x, na.rm = TRUE),
                          sd = ~sd(.x, na.rm = TRUE),
                          se = ~sd(.x/sqrt(length(.x)), na.rm = TRUE))))
  


### Plot displacement  Hose 3 dpf

PlotDispAvgHose3dpf <- (ggplot(
  TableAvgHose3dpf,
  aes(RelTime, Avg_Y_displacement_mean, col = Pressure_Level)) +
    theme_plot +
    guides(colour = "none") +
    geom_line() +
    scale_colour_viridis(
      discrete = TRUE,
      option = "D",
      direction = 1,
      end = 0.5
    ) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(0,100,10),limits = c(-60,130)) +
    scale_y_continuous(breaks = seq(-0.2,2,0.2),limits = c(-0.4,1.2)) +
    coord_cartesian(xlim = c(-10,80), ylim = c(-0.2,1)) +
    geom_vline(xintercept = 0, color = "black") +
    geom_errorbar(aes(ymin = Avg_Y_displacement_mean, ymax = Avg_Y_displacement_mean + Avg_Y_displacement_se)) + # error bar is only shown on top
    labs(
      x = "time after stimulus (s)",
      y = " vertical displacement (mm/s)",
      color =  str_wrap("set pressure (mbar)", width = 15)
    ) +
    geom_vline(
      xintercept = 20, color = "gray",
      linetype = 2
    ) +
    guides(color = guide_legend(keyheight = 0.3))
)
PlotDispAvgHose3dpf

#### Saving PNG of plot


ggsave("Manuscript/pictures/PanelTvsAVGDisp_stepHose3dpf.png",
       width = 1000, height = 800, units = "px", device = "png"
)

### Plot Max.displacement vs Pressure 2 dpf



PlotAVGPressVSMaxDispHose3dpf <- (
  ggplot(
    TableMaxHose3dpf,
    aes(Pressure_Level, max_STA5_Corr_Avg_Y_displacement, col = Pressure_Level)
  ) +
   theme_plot +
   # background_grid(major = "none", minor = "none") +
    geom_point(size = 2, shape = 20, alpha = 0.5) +
    geom_line(aes(group = Batch_ID), 
              alpha = 0.5) +
    scale_colour_viridis(
      discrete = TRUE, 
      option = 'D',
      direction = 1,
      end = 0.5
    ) +
    scale_x_discrete(
      breaks = as.character(seq(-20,200,20)),
      limits = as.character(seq(0,205))) +
    scale_y_continuous(
      breaks = seq(-0.2,1.6,0.2),
      limits = c(-1,2.0)
    ) +
    coord_cartesian(ylim = c(0,1)) +
    geom_vline(
      xintercept = as.character(0),
      color = "black",
      size = 0.5
    ) + 
    geom_hline(
      yintercept = 0,
      color = "black",
      size = 0.5
    ) +
     labs(
      x = "pressure (mb)",
      y = expression(paste("max. ",Delta,"  vertical displacement (mm/s)",sep = "")),
      color =  str_wrap("set pressure (mbar)", width = 15)
    )  +
    geom_point(
      data = AvgMaxHose3dpf,
      aes(x = Pressure_Level, y = max_STA5_Corr_Avg_Y_displacement_mean),
      size = 3,
      shape = 1
    ) + 
    guides(color = guide_legend(keyheight = 0.3))
)

PlotAVGPressVSMaxDispHose3dpf

#### Saving PNG of plot


ggsave(
  "Manuscript/pictures/Panel_prVSMaxAVGdisp_stepHose3dpf.png",
  width = 1200, height = 800, units = "px", device = "png", bg = "white"
)


# generate figure composite panel grid ------------------------------------

  Fontsize = 10
  imgHosechamber <- readPNG("Manuscript/pictures/ChamberDiagramplatyHose_Paper1nolabs.png")
  
  panel_chamber <- ggdraw() + 
    draw_image(imgHosechamber) +
    draw_label("C", x = 0.54, y = 0.06, size = Fontsize,color = "black", angle = 0) +
    draw_label("10 cm", x = 0.85, y = 0.37, size = Fontsize,color = "black", angle = 0) +
    draw_label("7 cm", x = 0.46, y = 0.15, size = Fontsize,color = "black", angle = 0) +
    draw_label("10–200 cm", x = 0.7, y = 0.8, size = Fontsize,color = "black", angle = 0) +
    draw_label("larvae", x = 0.45, y = 0.51, size = Fontsize,color = "black", angle = 0) +
    draw_label("Pch", x = 0.7, y = 0.62, size = Fontsize,color = "black", angle = 0) +
    draw_label("850 nm\n LEDs", x = 0.2, y = 0.08, size = Fontsize,color = "black", angle = 0) 
  
  panel_1dpf <- ggdraw(PlotDispAvg1dpf) +
    draw_label("1 dpf", x = 0.54, y = 0.96, size = Fontsize,color = "black", angle = 0)
  
  layout <- 
    "ABBC
     DDEE"


FigSupp2 <- 
  ggdraw(PlotAveragePressure1dpf) +
  panel_1dpf + 
  panel_chamber +
  ggdraw(PlotDispAvgHose3dpf) +
  ggdraw(PlotAVGPressVSMaxDispHose3dpf)+ 
  plot_layout(nrow = 1 , design = layout, heights = c(1, 1)) + 
  plot_annotation(tag_levels = list(
    c("A", "B","C", "D", "E"))) &
  theme(plot.tag = element_text(size = 12, face = "plain"))

ggsave(
  filename = "Manuscript/Figures/Figure1-FigureSupplement2.pdf", 
  FigSupp2, width = 2600, height = 2400,
  units = "px"
)

ggsave(
  filename = "Manuscript/Figures/Figure1-FigureSupplement2.png", 
  FigSupp2, width = 2600, height = 2400,
  units = "px"
)



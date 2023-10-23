########################################
##
## Title:Figure1-FigureSupplement1.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón and Gáspár Jékely
##
## Last Date modified: 2023-03-25
##
## Description: Code to generate Figure1-FigureSupplement1.Descriptive statistics  of the pressure response and swimming trajectories of 2 and 3 day-old larvae.
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
  library(grid)
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

ThemePlot <- theme(
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



##2dpf--------------------------------------------------

### read data
TableIndStep2dpf <- read_csv("Data/SourceData_elife/Figure1-SourceData1.csv")


###Ratio Up/Down tracks
TableIndStep2dpf <- 
  TableIndStep2dpf %>%
  group_by(Trial_ID,RelTime) %>% 
  mutate(LarvaNumRatio = Num_Tracks_Up/Num_Tracks_Down) %>%
  relocate(LarvaNumRatio,.after = Num_Tracks_Down)

###define pressure levels

TableIndStep2dpf$Pressure_Level <- factor(TableIndStep2dpf$Pressure_Level,levels =  c("3.125", "12.5",  "22.5" ,"32.5", "42.5", "65",  "85" , "137.5" ,"237.5","556","778","988"))

### Smoothing curves to get maximal values

TableIndStep2dpf <- 
  TableIndStep2dpf %>%
  group_by(Trial_ID) %>% 
  mutate(across(Avg_Speed:Corr_Num_Tracks_Down,
                ~rollmean(rollmean(.x, k = 5, na.pad = T),
                          k = 5, na.pad = T), 
                .names = "STA5_{.col}"))


###Calculating aggregate tables
##Visually assessing track number to decide cut off
cutoff2dpf = 100

(
  ggplot(TableIndStep2dpf,aes(x = RelTime, y = Num_Tracks_Up + Num_Tracks_Down)) +
    geom_line() +
    geom_hline(yintercept = cutoff2dpf, color = "red") +
    facet_wrap(~Pressure_Level)
  )



###Calculating aggregate tables


####AVG. values
TableAvgStep2dpf <- TableIndStep2dpf %>% 
  filter(Num_Tracks_Up + Num_Tracks_Down >= cutoff2dpf) %>%
  group_by(Pressure_Level,Type_Experiment,Genotype,RelTime) %>% 
  summarise(across(Avg_Speed:STA5_Corr_Num_Tracks_Down,list(mean = ~mean(.x, na.rm = TRUE),
                                                            sd = ~sd(.x, na.rm = TRUE),
                                                            se = ~sd(.x/sqrt(length(.x)), na.rm = TRUE))
                   ))

TableAvgStep2dpf$n_trials <- count(TableIndStep2dpf %>%
                                     filter(Num_Tracks_Up + Num_Tracks_Down >= cutoff2dpf) %>%
                                     group_by(Pressure_Level,Type_Experiment,Genotype,RelTime))$n

####Max. values
TableMaxStep2dpf <- 
  TableIndStep2dpf %>% 
  filter(Num_Tracks_Up + Num_Tracks_Down >= cutoff2dpf &
           RelTime>0 & RelTime <=  StimulusDuration) %>%
  group_by(Trial_ID,
           Pressure_Level,
           Type_Experiment,
           Genotype,
           Batch_ID) %>% 
  summarise(across(Avg_Speed:STA5_Corr_Num_Tracks_Down,
                   ~max(.x,na.rm = TRUE),.names = "max_{.col}")
  )

TableMaxStep2dpf["max_PressVal"][TableMaxStep2dpf["max_PressVal"] == -Inf] <- NA
TableMaxStep2dpf["max_STA5_PressVal"][TableMaxStep2dpf["max_STA5_PressVal"] == -Inf] <- NA

AvgMaxStep2dpfWT <- 
  TableMaxStep2dpf %>%
  group_by(Pressure_Level,Type_Experiment,Genotype) %>% 
  summarise(across(max_Avg_Speed:max_STA5_Corr_Num_Tracks_Down,
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE),
                        se = ~sd(.x/sqrt(length(.x)), na.rm = TRUE))))



###Plots--------


####Pressure logs

PlotAveragePressure2dpf <-(
  ggplot(TableAvgStep2dpf,
         aes(RelTime, PressVal_mean, col = Pressure_Level)) +
  ThemePlot +
  background_grid(major = 'none', minor = 'none') +
  geom_line() +
  geom_hline(yintercept = 0) +
  scale_colour_viridis(
    discrete = TRUE, 
    option = 'D', 
    direction = 1,
    end = 0.5) +
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(-10, 130)) +
  scale_y_continuous(breaks = seq(0,1000,200), limits = c(-10, 1000)) +
  coord_cartesian(xlim = c(-10,100)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_errorbar(aes(ymin = PressVal_mean, ymax = PressVal_mean+PressVal_se),
                linewidth = 0.2) +
  geom_vline(xintercept = 60, color = "gray", linetype = 2) +
  labs(x = "time after stimulus (s)",
       y = "pressure (mb)",
       color = str_wrap("set pressure (mbar)", width = 15)) +
  theme(legend.key.size = unit(3, 'mm')) +
  guides(color = guide_legend(keyheight = 0.3))
  )

PlotAveragePressure2dpf

##### Saving PNG of plot

ggsave("Manuscript/pictures/PanelAvgPressure_step2dpf.png",
       plot = PlotAveragePressure2dpf,
       width = 2200,height = 2000,units = "px",device = "png")


####Speed vs Time

PlotSpeedAvg2dpf <- (
  ggplot(TableAvgStep2dpf,aes(RelTime,
                              Avg_Speed_mean,
                              col = Pressure_Level)) +
    ThemePlot +
    background_grid(major = 'none', minor = 'none') +
    guides(colour = "none") +
    scale_colour_viridis(discrete = TRUE, option = 'D', direction = 1,end = 0.5) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(0,100,20),limits = c(-10,130)) +
    scale_y_continuous(breaks = seq(0,3,0.5),limits = c(0,3.0)) +
    coord_cartesian(xlim = c(-10,100)) +
    geom_vline(xintercept = 0,color = "black") +
    geom_line(data = TableAvgStep2dpf,
              aes(x = RelTime,y = Avg_Speed_mean, col = Pressure_Level),
              linewidth = 0.5, alpha = 1) +
    geom_errorbar(data = TableAvgStep2dpf,
                  aes(ymin = Avg_Speed_mean, ymax = Avg_Speed_mean+Avg_Speed_se)) +
    labs(x = "time after stimulus (s)",y = "mean speed (mm/s)", color = str_wrap("pressure (mb)",width = 10)) +
    geom_vline(xintercept = 60,color = "gray",linetype = 2))

PlotSpeedAvg2dpf

##### Saving PNG of plot

ggsave("Manuscript/pictures/PanelAvgSpeed_step2dpf.png",plot = PlotSpeedAvg2dpf,
       width = 2200,height = 2000,units = "px",device = "png")

####Change Speed vs Time

PlotCorSpeedAvg2dpf <- (
  ggplot(TableAvgStep2dpf,aes(RelTime,
                              Corr_Avg_Speed_mean,
                              col = Pressure_Level)) +
    ThemePlot +
    background_grid(major = 'none', minor = 'none') +
    guides(colour = "none") +
    scale_colour_viridis(discrete = TRUE, option = 'D', direction = 1,end = 0.5) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(0,100,20), limits = c(-10,130)) +
    scale_y_continuous(breaks = seq(-0.2, 3, 0.2), limits = c(-0.4, 1.3)) +
    coord_cartesian(xlim = c(-10,100),ylim = c(-0.2,0.8)) +
    geom_vline(xintercept = 0,color = "black") +
    geom_line(data = TableAvgStep2dpf,
              aes(x = RelTime,y = Corr_Avg_Speed_mean, col = Pressure_Level),
              linewidth = 0.5, alpha = 1) +
    geom_errorbar(data = TableAvgStep2dpf,
                  aes(ymin = Corr_Avg_Speed_mean, ymax = Corr_Avg_Speed_mean+Corr_Avg_Speed_se),
                  linewidth = 0.2) +
    labs(x = "time after stimulus (s)", y = str_wrap(" ∆ mean speed (mm/s)",
                                                    width = 15), color = str_wrap("pressure (mb)",width = 20)) +
    geom_vline(xintercept = 60,color = "gray",linetype = 2))
  
PlotCorSpeedAvg2dpf

##### Saving PNG of plot

ggsave("Manuscript/pictures/PanelChangeAvgSpeed_step2dpf.png",plot = PlotCorSpeedAvg2dpf,
       width = 2200,height = 2000,units = "px",device = "png")



####Vert. movement vs Time

PlotMovAvg2dpf <- 
  (ggplot(TableAvgStep2dpf,aes(RelTime,Avg_Y_movement_mean,col = Pressure_Level)) +
     ThemePlot +
     background_grid(major = 'none', minor = 'none') +
     guides(colour = "none") +
     scale_colour_viridis(discrete = TRUE, option = 'D', direction = 1,end = 0.5) +
     geom_hline(yintercept = 0) +
     scale_x_continuous(breaks = seq(0,100,20),limits = c(-10,130)) +
     scale_y_continuous(breaks = seq(-0.2, 3, 0.2), limits = c(-0.4, 1.2)) +
     coord_cartesian(xlim = c(-10,100)) +
     geom_vline(xintercept = 0,color = "black") +
     geom_line(data = TableAvgStep2dpf,aes(x = RelTime,y = Avg_Y_movement_mean, col = Pressure_Level),linewidth = 0.5, alpha = 1) +
     geom_errorbar(
       data = TableAvgStep2dpf,
       aes(ymin = Avg_Y_movement_mean, ymax = Avg_Y_movement_mean+Avg_Y_movement_se),
       linewidth = 0.2) +
     labs(x = "time after stimulus (s)",y = str_wrap("mean vert. mov. (mm/s)",
                                                     width = 15), color = str_wrap("pressure (mb)",width = 20)) +
     geom_vline(xintercept = 60,color = "gray",linetype = 2))

PlotMovAvg2dpf

##### Saving PNG of plot

ggsave("Manuscript/pictures/PanelAvgVmov_step2dpf.png",plot = PlotMovAvg2dpf,
       width = 2200,height = 2000,units = "px",device = "png")

####straightness index vs Time


PlotTorAvg2dpf <- 
  (ggplot(TableAvgStep2dpf,aes(RelTime,Tortuosity_mean,col = Pressure_Level)) +
     ThemePlot +
     background_grid(major = 'none', minor = 'none') +
     guides(colour = "none") +
     scale_colour_viridis(discrete = TRUE, option = 'D', direction = 1,end = 0.5) +
     geom_hline(yintercept = 0) +
     scale_x_continuous(breaks = seq(0,100,20),limits = c(-10,130)) +
     scale_y_continuous(breaks = seq(0,1,0.2),limits = c(0,0.8)) +
     coord_cartesian(xlim = c(-10,100)) +
     geom_vline(xintercept = 0,color = "black") +
     geom_line(data = TableAvgStep2dpf,aes(x = RelTime,y = Tortuosity_mean, col = Pressure_Level),linewidth = 0.5, alpha = 1) +
     geom_errorbar(
       data = TableAvgStep2dpf,
       aes(ymin = Tortuosity_mean, ymax = Tortuosity_mean+Tortuosity_se),
       linewidth = 0.2) +
     labs(x = "time after stimulus (s)",y = str_wrap("straightness index",
                                                     width = 15), color = str_wrap("pressure (mb)",width = 20)) +
     geom_vline(xintercept = 60,color = "gray",linetype = 2)) 

PlotTorAvg2dpf

##### Saving PNG of plot

ggsave("Manuscript/pictures/PanelAvgTor_step2dpf.png",plot = PlotTorAvg2dpf,
       width = 2200,height = 2000,units = "px",device = "png")

####Vert. Straightness vs Time

PlotVertStraightAvg2dpf <- 
  (ggplot(TableAvgStep2dpf,aes(RelTime,Straightness_Y_mean,col = Pressure_Level)) +
     ThemePlot +
     background_grid(major = 'none', minor = 'none') +
     guides(colour = "none") +
     scale_colour_viridis(discrete = TRUE, option = 'D', direction = 1,end = 0.5) +
     geom_hline(yintercept = 0) +
     scale_x_continuous(breaks = seq(0,100,20),limits = c(-10,130)) +
     scale_y_continuous(breaks = seq(-0.1,2,0.1),limits = c(-0.1,0.5)) +
     coord_cartesian(xlim = c(-10,100)) +
     geom_vline(xintercept = 0,color = "black") +
     geom_line(data = TableAvgStep2dpf,aes(x = RelTime,y = Straightness_Y_mean, col = Pressure_Level),linewidth = 0.5, alpha = 1) +
     geom_errorbar(
       data = TableAvgStep2dpf,
       aes(ymin = Straightness_Y_mean, ymax = Straightness_Y_mean+Straightness_Y_se),
       linewidth = 0.2) +
     labs(x = "time after stimulus (s)",y = "mean vertical straightness",color = str_wrap("pressure (mb)",width = 20)) +
     geom_vline(xintercept = 60,color = "gray",linetype = 2))

PlotVertStraightAvg2dpf

##### Saving PNG of plot

ggsave("Manuscript/pictures/PanelAvgvertStraight_step2dpf.png",plot = PlotVertStraightAvg2dpf,
       width = 2200,height = 2000,units = "px",device = "png")

####Ratio up/down tracks

PlotAvgRatioTracks2dpf <- 
  (ggplot(TableAvgStep2dpf,aes(RelTime,LarvaNumRatio_mean, col = Pressure_Level))+
     ThemePlot +
     background_grid(major = 'none', minor = 'none') +
     guides(colour = "none") +
     scale_colour_viridis(discrete = TRUE, option = 'D', direction = 1,end = 0.5) +
     geom_hline(yintercept = 0) +
     scale_x_continuous(breaks = seq(0,100,20),limits = c(-10,130)) +
     coord_cartesian(xlim = c(-10,100)) +
     geom_vline(xintercept = 0,color = "black") +
     geom_line(data = TableAvgStep2dpf,aes(x = RelTime,y = LarvaNumRatio_mean, col = Pressure_Level),linewidth = 0.5, alpha = 1) +
     geom_errorbar(
       data = TableAvgStep2dpf,
       aes(ymin = LarvaNumRatio_mean, ymax = LarvaNumRatio_mean + LarvaNumRatio_se),
       linewidth = 0.2) +
     labs(x = "time after stimulus (s)",y = str_wrap("upward track index",
                                                     width = 15),color = str_wrap("pressure (mb)",width = 20)) +
     geom_vline(xintercept = 60,color = "gray",linetype = 2))

PlotAvgRatioTracks2dpf



##### Saving PNG of plot


ggsave("Manuscript/pictures/PanelTvsIndDispBP_step2dpf.png",
       width = 2200,height = 2000,units = "px",device = "png")



##3dpf--------------------------------------------------

### read data

TableIndStep3dpf <- read_csv("Data/SourceData_elife/Figure1-SourceData2.csv")

###Ratio Up/Down tracks
TableIndStep3dpf <- 
  TableIndStep3dpf %>%
  group_by(Trial_ID,RelTime) %>% 
  mutate(LarvaNumRatio = Num_Tracks_Up/Num_Tracks_Down) %>%
  relocate(LarvaNumRatio,.after = Num_Tracks_Down)

##Visually assessing track number to decide cut off
cutoff3dpf = 100

(
  ggplot(TableIndStep3dpf,aes(x = RelTime, y = Num_Tracks_Up + Num_Tracks_Down)) +
    geom_line() +
    geom_hline(yintercept = cutoff3dpf, color = "red") +
    facet_wrap(~Pressure_Level))

###define pressure levels
#("3.125","7.5", "12.5","17.5",  "22.5" , "27.5","32.5" ,"37.5", "42.5" ,"47.5","55", "65","75"  ,  "85" ,"95"  , "137.5" ,"162.5","187.5","212.5","237.5" )

TableIndStep3dpf$Pressure_Level <- factor(TableIndStep3dpf$Pressure_Level,
                                          levels =  c("3.125", "12.5", "22.5",
                                                      "32.5", "42.5", "65",
                                                      "85", "137.5" ,"237.5",
                                                      "556","778","988"))

#Smoothing curves to get maximal values

TableIndStep3dpf <- 
  TableIndStep3dpf %>%
  group_by(Trial_ID) %>% 
  mutate(across(Avg_Speed:Corr_Num_Tracks_Down,
                ~rollmean(rollmean(.x, k = 5, na.pad = T),
                          k = 5, na.pad = T), 
                .names = "STA5_{.col}"))


#Calculating aggregate tables

####AVG. values
TableAvgStep3dpf <- TableIndStep3dpf %>% 
  filter(Num_Tracks_Up + Num_Tracks_Down >= cutoff3dpf) %>%
  group_by(Pressure_Level,Type_Experiment,Genotype,RelTime) %>% 
  summarise(across(Avg_Speed:STA5_Corr_Num_Tracks_Down,list(mean = ~mean(.x, na.rm = TRUE),
                                                            sd = ~sd(.x, na.rm = TRUE),
                                                            se = ~sd(.x/sqrt(length(.x)), na.rm = TRUE))))

TableAvgStep3dpf$n_trials <- count(TableIndStep3dpf %>%
                                     filter(Num_Tracks_Up + Num_Tracks_Down >= cutoff3dpf) %>%
                                     group_by(Pressure_Level,Type_Experiment,Genotype,RelTime))$n


####Max. values

TableMaxStep3dpf <- 
  TableIndStep3dpf %>% 
  filter(Num_Tracks_Up + Num_Tracks_Down >= cutoff3dpf &
           RelTime>0 & RelTime <=  StimulusDuration) %>%
  group_by(Trial_ID,
           Pressure_Level,
           Type_Experiment,
           Genotype,
           Batch_ID) %>% 
  summarise(across(Avg_Speed:STA5_Corr_Num_Tracks_Down,
                   ~max(.x,na.rm = TRUE),.names = "max_{.col}")
  )

TableMaxStep3dpf["max_PressVal"][TableMaxStep3dpf["max_PressVal"] == -Inf] <- NA
TableMaxStep3dpf["max_STA5_PressVal"][TableMaxStep3dpf["max_STA5_PressVal"] == -Inf] <- NA


AvgMaxStep3dpfWT <- 
  TableMaxStep3dpf %>% 
  group_by(
    Pressure_Level,
    Type_Experiment,
    Genotype
  ) %>% 
  summarise(
    across(
      max_Avg_Speed:max_STA5_Corr_Num_Tracks_Down,
      list(mean = ~mean(.x, na.rm = T),
           sd = ~sd(.x, na.rm = T),
           se = ~sd(.x/sqrt(length(.x)), na.rm = T)))
  )

###Plots--------

####Pressure logs

PlotAveragePressure3dpf <-(
  ggplot(TableAvgStep3dpf %>%
           filter(Pressure_Level %in% c("3.125", "12.5", "22.5",
                                        "32.5", "42.5", "65",
                                        "85", "137.5" ,"237.5",
                                        "556","778","988")),
         aes(RelTime,PressVal_mean,col = Pressure_Level)) +
    ThemePlot +
    background_grid(major = 'none', minor = 'none') +
    geom_line() +
    geom_hline(yintercept = 0) +
    scale_colour_viridis(discrete = TRUE, 
                         option = 'D', 
                         direction = 1,
                         end = 0.5) +
    scale_x_continuous(breaks = seq(0,100,20),limits = c(-10,130)) +
    scale_y_continuous(breaks = seq(0,1000,50),limits = c(-10,250)) +
    coord_cartesian(xlim = c(-10,100)) +
    geom_vline(xintercept = 0,color = "black") +
    geom_errorbar(aes(ymin = PressVal_mean, ymax = PressVal_mean+PressVal_se),
                  linewidth = 0.2) +
    geom_vline(xintercept = 60,color = "gray",linetype = 2) +
  labs(x = "time after stimulus (s)",
       y = "pressure (mb)",
       color = str_wrap("set pressure (mbar)", width = 15)) +
  theme(legend.key.size = unit(3, 'mm')) +
  guides(color = guide_legend(keyheight = 0.3)))

PlotAveragePressure3dpf

##### Saving PNG of plot


ggsave("Manuscript/pictures/PanelAvgPressure_step3dpf.png",
       plot = PlotAveragePressure3dpf,
       width = 2200,height = 2000,units = "px",device = "png")


####Speed vs Time 

PlotSpeedAvg3dpf <- (
  ggplot(TableAvgStep3dpf,aes(RelTime,
                              Avg_Speed_mean,
                              col = Pressure_Level)) +
    ThemePlot +
    background_grid(major = 'none', minor = 'none') +
    guides(colour = "none") +
    scale_colour_viridis(discrete = TRUE, option = 'D', direction = 1,end = 0.5) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(0,100,20),limits = c(-10,130)) +
    scale_y_continuous(breaks = seq(0,3,0.5),limits = c(0,3.0)) +
    coord_cartesian(xlim = c(-10,100)) +
    geom_vline(xintercept = 0,color = "black") +
    geom_line(data = TableAvgStep3dpf,
              aes(x = RelTime,y = Avg_Speed_mean, col = Pressure_Level),
              linewidth = 0.5, alpha = 1) +
    geom_errorbar(data = TableAvgStep3dpf,
                  aes(ymin = Avg_Speed_mean, ymax = Avg_Speed_mean+Avg_Speed_se),
                  linewidth = 0.2) +
    labs(x = "time after stimulus (s)",y = "mean speed (mm/s)", color = str_wrap("pressure (mb)",width = 20)) +
    geom_vline(xintercept = 60,color = "gray",linetype = 2))

PlotSpeedAvg3dpf

##### Saving PNG of plot

ggsave("Manuscript/pictures/PanelAvgSpeed_step3dpf.png",plot = PlotSpeedAvg3dpf,
       width = 2200,height = 2000,units = "px",device = "png")

####Change Speed vs Time 

PlotCorSpeedAvg3dpf <- (
  ggplot(TableAvgStep3dpf,aes(RelTime,
                              Corr_Avg_Speed_mean,
                              col = Pressure_Level)) +
    ThemePlot +
    background_grid(major = 'none', minor = 'none') +
    guides(colour = "none") +
    scale_colour_viridis(discrete = TRUE, option = 'D', direction = 1,end = 0.5) +
    scale_alpha_continuous(range = c(0.1,0.5)) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(0,100,20),limits = c(-10,130)) +
    scale_y_continuous(breaks = seq(-0.2, 3, 0.2), limits = c(-0.1, 0.8)) +
    coord_cartesian(xlim = c(-10,100)) +
    geom_vline(xintercept = 0,color = "black") +
    geom_line(data = TableAvgStep3dpf,
              aes(x = RelTime,y = Corr_Avg_Speed_mean, col = Pressure_Level),
              linewidth = 0.5, alpha = 1) +
    geom_errorbar(data = TableAvgStep3dpf,
                  aes(ymin = Corr_Avg_Speed_mean, ymax = Corr_Avg_Speed_mean+Corr_Avg_Speed_se),
                  linewidth = 0.2) +
    labs(x = "time after stimulus (s)",y = str_wrap(" ∆ mean speed (mm/s)",
                                                    width = 15), color = str_wrap("pressure (mb)",width = 20)) +
    geom_vline(xintercept = 60,color = "gray",linetype = 2))

PlotCorSpeedAvg3dpf

##### Saving PNG of plot

ggsave("Manuscript/pictures/PanelChangeAvgSpeed_step3dpf.png",plot = PlotCorSpeedAvg3dpf,
       width = 2200,height = 2000,units = "px",device = "png")



####Vert. movement vs Time

PlotMovAvg3dpf <- 
  ggplot(TableAvgStep3dpf,aes(RelTime,Avg_Y_movement_mean,col = Pressure_Level)) +
     ThemePlot +
     background_grid(major = 'none', minor = 'none') +
     guides(colour = "none") +
     scale_colour_viridis(discrete = TRUE, option = 'D', direction = 1,end = 0.5) +
     geom_hline(yintercept = 0) +
     scale_x_continuous(breaks = seq(0,100,20),limits = c(-10,130)) +
     scale_y_continuous(breaks = seq(-0.8,2,0.2), limits = c(-1, 1.3)) +
     coord_cartesian(xlim = c(-10,100), ylim = c(-0.6,1.3)) +
      geom_vline(xintercept = 0,color = "black") +
     geom_line(
       data = TableAvgStep3dpf,
       aes(x = RelTime,y = Avg_Y_movement_mean, col = Pressure_Level),
       linewidth = 0.5) +
     geom_errorbar(
       data = TableAvgStep3dpf,
       aes(ymin = Avg_Y_movement_mean, ymax = Avg_Y_movement_mean+Avg_Y_movement_se),
       linewidth = 0.2) +
     labs(
       x = "time after stimulus (s)",
       y = str_wrap("mean vert. mov. (mm/s)",
                    width = 15), 
       width = 10, 
       color = str_wrap("pressure (mb)")) +
     geom_vline(xintercept = 60,color = "gray",linetype = 2) 

PlotMovAvg3dpf

##### Saving PNG of plot

ggsave("Manuscript/pictures/PanelAvgVmov_step3dpf.png",plot = PlotMovAvg3dpf,
       width = 2200,height = 2000,units = "px",device = "png")

####Straightness index vs Time


PlotTorAvg3dpf <- 
  (ggplot(TableAvgStep3dpf,aes(RelTime,Tortuosity_mean,col = Pressure_Level)) +
     ThemePlot +
     background_grid(major = 'none', minor = 'none') +
     guides(colour = "none") +
     scale_colour_viridis(discrete = TRUE, option = 'D', direction = 1,end = 0.5) +
     geom_hline(yintercept = 0) +
     scale_x_continuous(breaks = seq(0,100,20),limits = c(-10,130)) +
     scale_y_continuous(breaks = seq(0,1,0.2),limits = c(0,1)) +
     coord_cartesian(xlim = c(-10,100), ylim = c(0,0.8)) +
     geom_vline(xintercept = 0,color = "black") +
     geom_line(data = TableAvgStep3dpf,aes(x = RelTime,y = Tortuosity_mean, col = Pressure_Level), linewidth = 1, alpha = 0.5) +
     geom_errorbar(
       data = TableAvgStep3dpf,
       aes(ymin = Tortuosity_mean, ymax = Tortuosity_mean + Tortuosity_se),
       linewidth = 0.2) +
     labs(x = "time after stimulus (s)",y = str_wrap("straightness index",
                                                     width = 15), color = str_wrap("pressure (mb)",width = 10)) +
     geom_vline(xintercept = 60,color = "gray",linetype = 2)) 

PlotTorAvg3dpf

##### Saving PNG of plot

ggsave("Manuscript/pictures/PanelAvgTor_step3dpf.png",plot = PlotTorAvg3dpf,
       width = 2200,height = 2000,units = "px",device = "png")

####Vert. Straightness vs Time

PlotVertStraightAvg3dpf <- 
  (ggplot(TableAvgStep3dpf,aes(RelTime,Straightness_Y_mean,col = Pressure_Level)) +
     ThemePlot +
     background_grid(major = 'none', minor = 'none') +
     guides(colour = "none") +
     scale_colour_viridis(discrete = TRUE, option = 'D', direction = 1,end = 0.5) +
     geom_hline(yintercept = 0) +
     scale_x_continuous(breaks = seq(0,100,20),limits = c(-10,130)) +
     scale_y_continuous(breaks = seq(-0.2, 3, 0.2), limits = c(-0.2, 0.6)) +
     coord_cartesian(xlim = c(-10,100)) +
     geom_vline(xintercept = 0,color = "black") +
     geom_line(data = TableAvgStep3dpf,aes(x = RelTime,y = Straightness_Y_mean, col = Pressure_Level),linewidth = 0.5, alpha = 1) +
     geom_errorbar(
       data = TableAvgStep3dpf,
       aes(ymin = Straightness_Y_mean, ymax = Straightness_Y_mean+Straightness_Y_se),
       linewidth = 0.2) +
     labs(x = "time after stimulus (s)",y = "mean vertical straightness",
          color = str_wrap("pressure (mb)", width = 10)) +
     geom_vline(xintercept = 60, color = "gray", linetype = 2))

PlotVertStraightAvg3dpf

##### Saving PNG of plot

ggsave("Manuscript/pictures/PanelAvgvertStraight_step3dpf.png",plot = PlotVertStraightAvg3dpf,
       width = 2200,height = 2000,units = "px",device = "png")

####Ratio up/down tracks

PlotAvgRatioTracks3dpf <- 
  (ggplot(TableAvgStep3dpf,aes(RelTime,LarvaNumRatio_mean, col = Pressure_Level))+
     ThemePlot +
     background_grid(major = 'none', minor = 'none') +
     guides(colour = "none") +
     scale_colour_viridis(discrete = TRUE, option = 'D', direction = 1,end = 0.5) +
     geom_hline(yintercept = 0) +
     scale_x_continuous(breaks = seq(0,100,20),limits = c(-10,130)) +
     coord_cartesian(xlim = c(-10,100)) +
     geom_vline(xintercept = 0,color = "black") +
     geom_line(data = TableAvgStep3dpf,aes(x = RelTime,y = LarvaNumRatio_mean, col = Pressure_Level),linewidth = 0.5, alpha = 1) +
     geom_errorbar(
       data = TableAvgStep3dpf,
       aes(ymin = LarvaNumRatio_mean, ymax = LarvaNumRatio_mean + LarvaNumRatio_se),
       linewidth = 0.2) +
     labs(x = "time after stimulus (s)",y = str_wrap("upward track index",
                                                     width = 15),
          color = str_wrap("pressure (mb)",width = 20)) +
     geom_vline(xintercept = 60,color = "gray",linetype = 2))

PlotAvgRatioTracks3dpf


# generate figure composite panel grid ------------------------------------


rectCoor <- -0.045
rectW <- 5.6
rectH <- 33.5
Rect1 <- rectGrob(
  x = rectCoor,
  y = 0.151,
  width = unit(rectW, "mm"),
  height = unit(rectH, "mm"),
  hjust = 0, vjust = 0,
  gp = gpar(fill = NULL, alpha = 1 ,lwd = 1.5))

Rect2 <- rectGrob(
  x = rectCoor,
  y = 0.501,
  width = unit(rectW, "mm"),
  height = unit(rectH, "mm"),
  hjust = 0, vjust = 0,
  gp = gpar(fill = NULL, alpha = 1 ,lwd = 1.5))

rectCoor2 <- 0
rectW2 <- 134
rectH2 <- 5.6
Rect3 <- rectGrob(
  x = rectCoor2,
  y = 0.09,
  width = unit(rectW2, "mm"),
  height = unit(rectH2, "mm"),
  hjust = 0, vjust = 0,
  gp = gpar(fill = NULL, alpha = 1 ,lwd = 1.5))

rectCoorY <- 0.85
rectCoor3 <- 0
rectW3 <- 33
rectH3 <- 5.6
Rect4 <- rectGrob(
  x = rectCoor3,
  y = rectCoorY,
  width = unit(rectW3, "mm"),
  height = unit(rectH3, "mm"),
  hjust = 0, vjust = 0,
  gp = gpar(fill = NULL, alpha = 1 ,lwd = 1.5))

rectCoor4 <- 0.254
Rect5 <- rectGrob(
  x = rectCoor4,
  y = rectCoorY,
  width = unit(rectW3, "mm"),
  height = unit(rectH3, "mm"),
  hjust = 0, vjust = 0,
  gp = gpar(fill = NULL, alpha = 1 ,lwd = 1.5))

rectCoor5 <- 0.501
Rect6 <- rectGrob(
  x = rectCoor5,
  y = rectCoorY,
  width = unit(rectW3, "mm"),
  height = unit(rectH3, "mm"),
  hjust = 0, vjust = 0,
  gp = gpar(fill = NULL, alpha = 1 ,lwd = 1.5))

rectCoor6 <- 0.754
Rect7 <- rectGrob(
  x = rectCoor6,
  y = rectCoorY,
  width = unit(rectW3, "mm"),
  height = unit(rectH3, "mm"),
  hjust = 0, vjust = 0,
  gp = gpar(fill = NULL, alpha = 1 ,lwd = 1.5))

posTmeLab <- 0.876
scalepos <- 0.21
stagelab <- 0.12
ImgTrajPeriod_2d <- readPNG("Manuscript/pictures/TrajectoriesbyPeriod_panel2d.png")
Fontsize = 10
PanelTrajPer_2d <- ggdraw() +
  draw_image(ImgTrajPeriod_2d) +
  draw_label(paste("1 ", "mm", sep = ""),
             x = 0.965, y = scalepos, color = "white",
             fontfamily = "sans", size = 7) +
  draw_label("t ->",
             x = 0.04, y = scalepos, color = "white",
             fontfamily = "sans", size = 7) +
  draw_label("85 mb",, angle = 90, x = -0.025, y = 0.66, size = Fontsize, color = "black", fontface = "plain") +
  draw_label("778 mb", angle = 90, x = -0.025, y = 0.32, size = Fontsize, color = "black", fontface = "plain") +
  draw_label("2 dpf", angle = 0, x = 0.5 , y = stagelab, size = Fontsize,color = "black") +
  draw_label("-10 s", angle = 0, x = 0.12 , y = posTmeLab, size = Fontsize,color = "black") +
  draw_label("10 s", angle = 0, x = 0.38 , y = posTmeLab, size = Fontsize,color = "black") +
  draw_label("20 s", angle = 0, x = 0.63 , y = posTmeLab, size = Fontsize,color = "black") +
  draw_label("30 s", angle = 0, x = 0.88 , y = posTmeLab, size = Fontsize,color = "black") +
  draw_grob(Rect1) +
  draw_grob(Rect2) +
  draw_grob(Rect3) +
  draw_grob(Rect4) +
  draw_grob(Rect5) +
  draw_grob(Rect6) +
  draw_grob(Rect7)


ImgTrajPeriod_3d <- readPNG("Manuscript/pictures/TrajectoriesbyPeriod_panel3d.png")

PanelTrajPer_3d <- ggdraw() +
  draw_image(ImgTrajPeriod_3d) +
  draw_label(paste("1 ", "mm", sep = ""),
             x = 0.965, y = scalepos , color = "white",
             fontfamily = "sans", size = 7) +
  draw_label("t ->",
             x = 0.04, y = scalepos, color = "white",
             fontfamily = "sans", size = 7) +
  draw_label("85 mb",, angle = 90, x = -0.028, y = 0.66, size = Fontsize, color = "black", fontface = "plain") +
  draw_label("500 mb", angle = 90, x = -0.028, y = 0.32, size = Fontsize, color = "black", fontface = "plain") +
  draw_label("3 dpf", angle = 0, x = 0.5 , y = stagelab, size = Fontsize,color = "black") +
  draw_label("-10 s", angle = 0, x = 0.12 , y = posTmeLab, size = Fontsize,color = "black") +
  draw_label("10 s", angle = 0, x = 0.38 , y = posTmeLab, size = Fontsize,color = "black") +
  draw_label("20 s", angle = 0, x = 0.63 , y = posTmeLab, size = Fontsize,color = "black") +
  draw_label("30 s", angle = 0, x = 0.88 , y = posTmeLab, size = Fontsize,color = "black") +
  draw_grob(Rect1) +
  draw_grob(Rect2) +
  draw_grob(Rect3) +
  draw_grob(Rect4) +
  draw_grob(Rect5) +
  draw_grob(Rect6) +
  draw_grob(Rect7)
# ImgTraj <- readPNG("Manuscript/pictures/Trajectories_panel.png")
# Fontsize = 10
# PanelTraj <- ggdraw() +
#   draw_image(ImgTraj) +
#   draw_label(paste("1 ", "mm", sep = ""),
#              x = 0.9, y = 0.5, color = "white",
#              fontfamily = "sans", size = 7) +
#   draw_label("12.5 mb", x = 0.04, y = 0.47, size = Fontsize, color = "white", fontface = "plain") +
#   draw_label("32 mb", x = 0.29, y = 0.47, size = Fontsize, color = "white", fontface = "plain") +
#   draw_label("85 mb", x = 0.54, y = 0.47, size = Fontsize, color = "white", fontface = "plain") +
#   draw_label("500 mb", x = 0.79, y = 0.47, size = Fontsize, color = "white", fontface = "plain") +
#   draw_label("35 mb", x = 0.04, y = 0.97, size = Fontsize, color = "white", fontface = "plain") +
#   draw_label("85 mb", x = 0.29, y = 0.97, size = Fontsize, color = "white", fontface = "plain") +
#   draw_label("250 mb", x = 0.54, y = 0.97, size = Fontsize, color = "white", fontface = "plain") +
#   draw_label("1000 mb", x = 0.79, y = 0.97, size = Fontsize, color = "white", fontface = "plain") +
#   draw_label("2 dpf", angle = 90, x = 0 , y = 0.74, size = Fontsize,color = "black") +
#   draw_label("3 dpf",angle = 90, x = 0, y = 0.25, size = Fontsize,color = "black") 
 
layout <- "
#AAABBCCDDEE
############
#FFFGGHHIIJJ
#KKKKK#LLLLL
#KKKKK#LLLLL
"

FigSuppl1 <- 
  ggdraw(PlotAveragePressure2dpf) + ggdraw(PlotCorSpeedAvg2dpf) +  
  ggdraw(PlotMovAvg2dpf) + ggdraw(PlotTorAvg2dpf) + 
  ggdraw(PlotAvgRatioTracks2dpf) +
  ggdraw(PlotAveragePressure3dpf) + ggdraw(PlotCorSpeedAvg3dpf) +  
  ggdraw(PlotMovAvg3dpf) + ggdraw(PlotTorAvg3dpf) + 
  ggdraw(PlotAvgRatioTracks3dpf) + 
  PanelTrajPer_2d +
  PanelTrajPer_3d +
  plot_layout(design = layout, heights = c(1, 0.1,1,1,1), widths = c(0.2,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5), guides = 'collect') +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 12, face = "plain"))


####Saving Figure
ggsave(
  filename = "Manuscript/Figures/Figure1-FigureSupplement1.pdf",
  FigSuppl1, width = 3200, height = 2000, units = "px"
)
ggsave(
  filename = "Manuscript/Figures/Figure1-FigureSupplement1.png",
  FigSuppl1, width = 3600, height = 2400, units = "px"
)


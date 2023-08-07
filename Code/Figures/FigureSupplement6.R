########################################
##
## Title:FigureSupplement6.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón
##
## Last Date modified: 2023-02-25
##
## Description: Code to generate Figure Supplement 6. cPRC calcium imaging.
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
library(magick)
library(png)
library(patchwork)
library(rbokeh)
library(rstatix)
library(scales)
library(stringi)
library(stringr)
library(tidyverse)
library(viridis)
library(zoo)
#Sourcing functions
source("Code/Figures/Functions_figures.R")


#switch to project directory
ProjectDir  <- here()
setwd(ProjectDir )

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


### read data Fluo intensity --------------------------------------------------

FinalFluoTable <- read_csv("Data/TablesResults/dRdFPressure_Caimaging.csv")

#Grouping ratiometric measurements


OnlyRatio <-
  FinalFluoTable %>% 
  group_by(ExperimentID,
           Cell,Stage,
           Type_Experiment, 
           LarvaID) %>%
  filter(Ratiometric %in% c("YES"))


### Smoothing curves to get maximal values


OnlyRatio <- 
  OnlyRatio %>%
  group_by(ExperimentID) %>% 
  mutate(dR_sta31 = rollmean(rollmean(dR, k = 31, na.pad = T),
                             k = 31, na.pad = T)) %>%
  relocate(dR_sta31, .after = dR)

###define pressure levels


OnlyRatio$Pressure_Level <- (
  factor(OnlyRatio$Pressure_Level,
         levels = c("100","250","500","750", "1000" )
  )
)

OnlyRatio$Cell <- factor(OnlyRatio$Cell,
                         levels = c("cPRC1_l", "cPRC2_l", "cPRC1_r", "cPRC2_r"   , 
                    "NOS1_r", "NOS1_l" , "NOS2_r", "NOS2_l", 
                    "MC", "MNv3_l", "SNd1_unp"  , "SNastc_r", 
                    "cPRCcilia_l" ,"MNv2_l", "Ser-h_l" ),
                    labels = c("cPRC_l1", "cPRC_l2", "cPRC_r1", "cPRC_r2"   , 
                    "NOS_r1", "NOS_l1" , "NOS_r2", "NOS_l2",  
                    "MC", "MNv3_l", expression(paste("S",N^{"d1_unp"}))  ,expression(paste("SN",r^{"astc"})) , 
                    "cPRCcilia_l" ,"MNvl_2", expression(paste("ser-h",1^r)))
  )

##Averaging metrics

###Average of ratiometric trials by rounded time
#####General
AvgORRound <- 
  OnlyRatio %>% 
  group_by(
    Pressure_Level,
    RoundRelTime,
    Type_Experiment,
    Stage,
    Cell)  %>% 
  summarise(across(
    PressVal:dF,
    list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x ,na.rm = TRUE),
      se = ~sd(.x/sqrt(length(.x)), na.rm = TRUE)
    ))
  )


####Only cPRCs for pressure average

AvgORRoundcPRC <- 
  OnlyRatio %>% 
  filter(Cell %in% c("cPRC_l1","cPRC_l2","cPRC_r1","cPRC_r2")) %>%
  group_by(
    Pressure_Level,
    RoundRelTime,
    Type_Experiment,
    Stage)  %>% 
  summarise(
    across(
      PressVal:dF,
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x ,na.rm = TRUE),
        se = ~sd(.x/sqrt(length(.x)), na.rm = TRUE)
      ))
  )




######sample size
OnlyRatio %>% ungroup() %>% 
  filter(Cell %in% c("cPRC_l1") & 
           Type_Experiment %in% c("Titration") & 
           Stage %in% c("2dpf")) %>% group_by(Pressure_Level,Cell,ExperimentID) %>%
  distinct(Pressure_Level,Stage) %>% arrange(Pressure_Level) %>%  group_by(
    Pressure_Level,
    Cell) %>% 
  count()


####Counting sample number and adding to the table

AvgORRoundcPRC$n <- count(
  OnlyRatio %>% 
    filter(Cell %in% c("cPRC_l1","cPRC_l2","cPRC_r1","cPRC_r2")) %>%
    group_by(
      Pressure_Level,
      RoundRelTime,
      Type_Experiment,
      Stage))$n

###Maximal pressure, dF and dR values of ratiometric trials during stimulus for each trial

MaxIndORRoundR <- 
  OnlyRatio %>%
  group_by(
    Pressure_Level,
    Cell,
    Type_Experiment,
    Stage,
    ExperimentID,
    LarvaID
  ) %>% 
  filter(RoundRelTime >=  0 & RoundRelTime <=  60 ) %>% 
  filter(dR_sta31 ==  max(dR_sta31))


###Average of maximal pressure, dF and dR values during stimulus, only ratiometric 

AvgORRoundMAxdR <- 
  MaxIndORRoundR %>% 
  group_by(
    Pressure_Level,
    Cell,
    Type_Experiment,
    Stage
  ) %>% 
  summarise(
    across(
      PressVal:dF,
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        se = ~sd(.x/sqrt(length(.x)), na.rm = TRUE)
      )))

####Counting sample number and adding to the table

AvgORRoundMAxdR$n <- count(
  MaxIndORRoundR %>% 
    group_by(
      Pressure_Level,
      Cell,
      Type_Experiment,
      Stage
    ))$n

####Statistical test
##### Testing differences between Pressure levels for each cell for each pressure level(paired one tail wilcox)

#cPRCs
stat.testdRmaxcPRC <- MaxIndORRoundR %>%
  filter(
    Cell %in% c("cPRC_l2","cPRC_r1","cPRC_r2")  & 
      Type_Experiment %in% c("Titration") & 
      Stage %in% c("2dpf")) %>% 
  group_by(Cell) %>%
  t_test(dR_sta31 ~ Pressure_Level, alternative = "less", paired = F) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testdRmaxcPRC

print(stat.testdRmaxcPRC, n = 100)

stat.testdRmaxcPRC <- stat.testdRmaxcPRC %>% 
  add_y_position()

stat.testdRmaxcPRC$y.position <- stat.testdRmaxcPRC$y.position -3 # to reduce y spacing.
stat.testdRmaxcPRC$xmin <- as.numeric(stat.testdRmaxcPRC$group1) # manually setting xmin based on pressure levels.
stat.testdRmaxcPRC$xmax <- as.numeric(stat.testdRmaxcPRC$group2)

stat.testdRmaxcPRC$p.adj <- round(stat.testdRmaxcPRC$p.adj,3)


###Plots--------------------------------------------------
#####time vs avg Pressure rounded time of remaining cPRCs

PPresAvAllcPRC <- (ggplot(AvgORRoundcPRC %>%
                                filter(Type_Experiment %in% c("Titration") & 
                                         Stage %in% c("2dpf")),
                              aes(x = RoundRelTime,
                                  y = PressVal_mean,
                                  col = Pressure_Level)) +
  ThemePlot +
  theme(legend.position="right") + 
  background_grid(major = "none", minor = "none") +
  geom_line() +
  scale_colour_viridis(
    discrete = TRUE, 
    option = "D", 
    direction = 1, 
    end = 0.5
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(ymin = PressVal_mean, 
                    ymax = PressVal_mean+PressVal_se,
                    col = Pressure_Level)
  ) +
  scale_x_continuous(breaks = seq(-30,120,30),
                     limits = c(-30,90)
  ) +
  scale_y_continuous(breaks = seq(0,1000,200),
                     limits = c(-1,1000)
  ) +
  labs(x = "time after stimulus (s)",
       y = "pressure (mb)",
       color = str_wrap("set pressure (mbar)", width = 15)) +
  geom_vline(xintercept = 60,colour = "grey",linetype = 2) +
  guides(color = guide_legend(keyheight = 0.3))
)
PPresAvAllcPRC


###### Saving PNG of plot

ggsave("Manuscript/pictures/PanelRoundRelTimvsAVGPressure-cPRCs_step2dpf.png",
       width = 2200, height = 2000, units = "px", device = "png"
)





#####time vs avg dR rounded time


PFluoAvR_cPRCs <- (
  ggplot(AvgORRound %>%
           filter(Cell %in% c("cPRC_l2","cPRC_r1","cPRC_r2") & 
                    Type_Experiment %in% c("Titration") & 
                    Stage %in% c("2dpf")),
         aes(x = RoundRelTime,
             y = dR_sta31_mean,
             col = Pressure_Level)
  ) +
    ThemePlot +
    theme(legend.position="none", 
          legend.key.size = unit(0.3, 'cm'),
          legend.key.width= unit(0.3, 'cm'),
          legend.box.spacing = unit(0.03, 'cm')) +
    background_grid(major = "none", minor = "none") +
    geom_line() +
    guides(colour = "none") +
    scale_colour_viridis(
      discrete = TRUE, 
      option = "D", 
      direction = 1, 
      end = 0.5
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) + 
    geom_errorbar(aes(ymin = dR_sta31_mean, 
                      ymax = dR_sta31_mean+dR_sta31_se,
                      col = Pressure_Level)
    ) +
    scale_x_continuous(breaks = seq(-10,120,20),
                       limits = c(-30,120)
    ) +
    theme(strip.text.x = element_text(size = 10),
          strip.background = element_blank()
    ) +
    coord_cartesian(xlim = c(-10,90)) +
    labs(x = "time after stimulus (s)",
         y = expression(paste(Delta, " R/R", sep = "")),
         color = str_wrap("set pressure (mbar)", width = 15)
    ) +
    geom_vline(
      xintercept = 60, color = "gray",
      linetype = 2
    ) +
    facet_wrap(~Cell) 
)

PFluoAvR_cPRCs


####tvsdRcPRCs individual values and average

PFluoAvR_cPRCsind <- (
  ggplot(OnlyRatio %>%
           filter(Cell %in% c("cPRC_l1","cPRC_l2","cPRC_r1","cPRC_r2") & 
                    Type_Experiment %in% c("Titration") & 
                    Stage %in% c("2dpf")),
         aes(x = RoundRelTime,
             y = dR_sta31)
  ) +
    ThemePlot +
    theme(legend.position="none", 
          legend.key.size = unit(0.3, 'cm'),
          legend.key.width= unit(0.3, 'cm'),
          legend.box.spacing = unit(0.03, 'cm')) +
    background_grid(major = "none", minor = "none") +
    geom_line(aes(group = ExperimentID, col = Pressure_Level), 
               alpha = 0.5,
               size = 0.5) +
    guides(colour = "none") +
    scale_colour_viridis(
      discrete = TRUE, 
      option = "D", 
      direction = 1, 
      end = 0.5
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) + 
    geom_line(data = AvgORRound %>%
                filter(Cell %in% c("cPRC_l1","cPRC_l2","cPRC_r1","cPRC_r2")  & 
                         Type_Experiment %in% c("Titration") & 
                         Stage %in% c("2dpf")), 
              aes(y = dR_sta31_mean , col = Pressure_Level),
              size = 1) +
    # geom_errorbar(aes(ymin = dR_mean, 
    #                   ymax = dR_mean+dR_se,
    #                   col = Pressure_Level)
    # ) +
    scale_x_continuous(breaks = seq(-10,120,20),
                       limits = c(-30,120)
    ) +
    coord_cartesian(xlim = c(-30,90)) +
    theme(strip.text.x = element_text(size = 10),
          strip.background = element_blank()
    ) +
    labs(x = "time after stimulus (s)",
         y = expression(paste("max. ",
                              Delta,
                              " R/R",
                              sep = " ")
         ),
         color = str_wrap("pressure (mbar)",
                          width = 20)
    ) +
    geom_vline(
      xintercept = 60, color = "gray",
      linetype = 2
    ) +
    facet_grid(vars(Pressure_Level),vars(Cell))
)

PFluoAvR_cPRCsind


#####Press vs ind. Max dR

PressVSmaxdR_cPRCs <- (
  ggplot(MaxIndORRoundR %>%
           group_by(Cell, Pressure_Level) %>% 
           filter(Cell %in% c("cPRC_l2","cPRC_r1","cPRC_r2") &
                    Type_Experiment %in% c("Titration") & 
                    Stage %in% c("2dpf")),
         aes(PressVal,
             dR_sta31, col = Pressure_Level)
  ) +
    ThemePlot + 
    background_grid(major = 'none',
                    minor = 'none') +
    geom_violin(aes(group = Pressure_Level),alpha = 0.7, size = 0.3,scale = "count", width = 0.4) +
    geom_point(alpha = 0.5, size = 2 , shape = 20) +
    geom_line(aes(group = LarvaID), alpha = 0.5) +
    geom_hline(yintercept = 0) +
    scale_colour_viridis(discrete = TRUE, 
                         option = 'D',
                         direction = 1,
                         end = 0.5
    ) +
    stat_pvalue_manual(
      stat.testdRmaxcPRC,
      trans = 'log10',
      bracket.nudge.y = 0, 
      tip.length = 0, 
      hide.ns = TRUE, 
      step.increase = 0, 
      label = "p.adj",
      label.size = 3
    ) +
    scale_x_continuous(trans = 'log10', breaks = c(0,10,100,200,500,750,1000)) +
    scale_y_continuous(breaks = seq(0,14,2),
                       limits = c(0,6)
    ) +
    geom_vline(xintercept = 0,
               color = "black") +
    theme(strip.text.x = element_text(size = 10),
          strip.background = element_blank()
    ) +
    labs(x = "pressure (mbar)",
         y = expression(paste("max. ",
                              Delta,
                              " R/R",
                              sep = " ")
         ),
         color = str_wrap("set pressure (mbar)", width = 15)) + 
    facet_wrap(~Cell) +
    guides(color = guide_legend(keyheight = 0.3))
)

PressVSmaxdR_cPRCs

#SNd1_unp-----

####Only SNdunp for pressure average

AvgORRoundSNd <- 
  OnlyRatio %>% 
  filter(Cell %in% expression(paste("S",N^{"d1_unp"}))) %>%
  group_by(
    Pressure_Level,
    RoundRelTime,
    Stage)  %>% 
  summarise(
    across(
      PressVal:dF,
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x ,na.rm = TRUE),
        se = ~sd(.x/sqrt(length(.x)), na.rm = TRUE)
      ))
  )

OnlyRatio %>% ungroup() %>% 
  filter(Cell %in% expression(paste("S",N^{"d1_unp"})) & 
           #Type_Experiment %in% c("Titration") & 
           Stage %in% c("2dpf")) %>% group_by(Pressure_Level,ExperimentID) %>%
  distinct(Pressure_Level) %>% arrange(Pressure_Level) %>%  group_by(
    Pressure_Level) %>% 
  count()

####plot

PFluoInR_SN <- (
  ggplot(OnlyRatio %>%
           filter(Cell %in% expression(paste("S",N^{"d1_unp"})) & 
                    Stage %in% c("2dpf") &
                    Pressure_Level %in% c("750")),
         aes(x = RoundRelTime,
             y = dR,
             col = Pressure_Level)
  ) +
    ThemePlot +
    theme(legend.position= c(0.9, 0.8), 
          legend.key.size = unit(0.3, 'cm'),
          legend.key.width= unit(0.3, 'cm'),
          legend.box.spacing = unit(0.03, 'cm'),
          strip.background = element_blank()) +
    background_grid(major = "none", minor = "none") +
    geom_line(aes(group = ExperimentID, col = Pressure_Level), 
              alpha = 0.25,
              size = 0.5) +
    guides(colour = "none") +
    scale_color_manual(values = "black") +
    # scale_colour_viridis(
    #   discrete = TRUE, 
    #   option = "D", 
    #   direction = 1, 
    #   end = 0.5
    # ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) + 
    geom_line(data = AvgORRoundSNd %>%
                filter(Stage %in% c("2dpf") &
                         Pressure_Level %in% c("750")), 
              aes(y = dR_sta31_mean ,col = Pressure_Level ),
              size = 1) +
    scale_x_continuous(breaks = seq(-10,120,20),
                       limits = c(-30,120)
    ) +
    coord_cartesian(xlim = c(-30,90)) +
    labs(x = "time after stimulus (s)",
         y = expression(paste(Delta, " R/R", sep = "")),
         color = str_wrap("set pressure (mbar)",
                          width = 15)
    ) +
    geom_vline(
      xintercept = 60, color = "gray",
      linetype = 2
    ) +
    guides(color = guide_legend(keyheight = 0.3))
)

PFluoInR_SN


# generate figure composite panel grid ------------------------------------
Fontsize = 10
PaneldRcPRCind <- ggdraw(PFluoAvR_cPRCsind) +
  draw_label("set pressure (mb)", x = 1, y = 0.5, angle = -90, size = Fontsize,color ="black", fontface = "plain") 


PanelInR_SN <- ggdraw(PFluoInR_SN) +
  draw_label(expression("SN"^{"d1_unp"}), x = 0.5, y = 0.9, size = Fontsize,color = "black") 
  

  layout <- "
AABBCCDD
AABBCCDD
EEEEEEEE
EEEEEEEE
EEEEEEEE
EEEEEEEE
  "
  
  FigSupp6 <- 
    ggdraw(PPresAvAllcPRC) + 
    ggdraw(PFluoAvR_cPRCs) + 
    ggdraw(PressVSmaxdR_cPRCs) +
    PanelInR_SN + 
    PaneldRcPRCind +
    plot_layout(design = layout, heights = c(1, 1)) + 
    plot_annotation(tag_levels = list(
      c("A", "B", "C", "D", "E"))) &
    theme(plot.tag = element_text(size = 12, face = "plain"))
  
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_6.pdf", 
    FigSupp6, width = 3000, height = 3000,
    units = "px"
  )
  
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_6.png", 
    FigSupp6, width = 4000, height = 3500,
    units = "px"
  )


########################################
##
## Title: Figure1.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón and Gáspár Jékely
##
## Last Date modified: 2023-02-19
##
## Description: Code to generate Figure 1. It includes results describing the behavioural response
## of Platynereis larvae to pressure at the population and individual level.
##
## Input files: Metrics tables computed with scripts in dir Code/BatchBehaviour and Code/CBFmeasurement.
##
## Output files: Figure 1 PDF and PNG files
##
## Comments:
##
## Publication: Unpublished
##
##########################################


#Initialize----
  rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
  gc() #free up memory and report the memory usage.
  CbbPalette <- c("#000000", "#009E73", "#0072B2", "#56B4E9", "#F0E442", "#E69F00", "#CC79A7")


# Loading libraries--------------------------------------------------
  library(cowplot)
  library(dplyr)
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
setwd(ProjectDir)

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

# 2dpf--------------------------------------------------

### read data

TableIndStep2dpf <- read_csv("Data/SourceData_elife/Figure1-SourceData1.csv")

###define pressure levels

TableIndStep2dpf$Pressure_Level <- factor(TableIndStep2dpf$Pressure_Level,
                                          levels =  c("3.125", "12.5", "22.5",
                                                      "32.5", "42.5", "65",
                                                      "85", "137.5", "237.5",
                                                      "556", "778", "988"))
### Smoothing curves to get maximal values

TableIndStep2dpf <-
  TableIndStep2dpf %>%
  group_by(Trial_ID) %>%
  mutate(across(Avg_Speed:Corr_Num_Tracks_Down,
                ~rollmean(rollmean(.x, k = 5, na.pad = TRUE),
                          k = 5, na.pad = TRUE),
                .names = "STA5_{.col}"))


###Calculating aggregate tables
##Visually assessing track number to decide cut off
Cutoff2dpf <- 100

(
  ggplot(TableIndStep2dpf, aes(x = RelTime, y = Num_Tracks_Up + Num_Tracks_Down)) +
    geom_line() +
    geom_hline(yintercept = Cutoff2dpf, color = "red") +
    facet_wrap(~Pressure_Level))


####AVG. values-
TableAvgStep2dpf <- TableIndStep2dpf %>%
  filter(Num_Tracks_Up + Num_Tracks_Down >= Cutoff2dpf) %>%
  group_by(Pressure_Level, Type_Experiment, Genotype, RelTime) %>%
  summarise(across(Avg_Speed:STA5_Corr_Num_Tracks_Down, list(mean = ~mean(.x, na.rm = TRUE),
                                                       sd = ~sd(.x, na.rm = TRUE),
                                                       se = ~sd(.x / sqrt(length(.x)), na.rm = TRUE))))

TableAvgStep2dpf$n_trials <- count(TableIndStep2dpf %>%
                                     filter(Num_Tracks_Up + Num_Tracks_Down >= Cutoff2dpf) %>%
                                     group_by(Pressure_Level, Type_Experiment, Genotype, RelTime))$n

####Max. values-
TableMaxStep2dpf <-
  TableIndStep2dpf %>%
   filter(Num_Tracks_Up + Num_Tracks_Down >= Cutoff2dpf &
            RelTime > 0 & RelTime <=  StimulusDuration) %>%
   group_by(Trial_ID,
           Pressure_Level,
           Type_Experiment,
           Genotype,
           Batch_ID) %>%
  summarise(across(Avg_Speed:STA5_Corr_Num_Tracks_Down,
                   ~max(.x, na.rm = TRUE), .names = "max_{.col}")
  )

TableMaxStep2dpf["max_PressVal"][TableMaxStep2dpf["max_PressVal"] == -Inf] <- NA
TableMaxStep2dpf["max_STA5_PressVal"][TableMaxStep2dpf["max_STA5_PressVal"] == -Inf] <- NA

AvgMaxStep2dpfWT <-
  TableMaxStep2dpf %>%
  group_by(Pressure_Level, Type_Experiment, Genotype) %>%
  summarise(across(max_Avg_Speed:max_STA5_Corr_Num_Tracks_Down,
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE),
                        se = ~sd(.x / sqrt(length(.x)), na.rm = TRUE))))




###Regression model-
##nesting data for mapping
TableMaxStep2dpf <-
  TableMaxStep2dpf %>%
  ungroup()  %>%
  nest(values = -Genotype)

##calculating a polynomial regression model on the data grouped by genotype.
TableMaxStep2dpf <- TableMaxStep2dpf %>%
  mutate(model1 = map(values, ~lm(data = .x, max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressVal)),
         model2 = map(values, ~lm(data = .x, max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressVal +
                                    I(max_STA5_PressVal^2))),
         model3 = map(values, ~lm(data = .x, max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressVal +
                                   I(max_STA5_PressVal^2) +
                                   I(max_STA5_PressVal^3))),
         tidied1 = map(model1, tidy),
         tidied2 = map(model2, tidy),
         tidied3 = map(model3, tidy),
         # intera = map(values,~lm(data = .x, STA5_Corr_Avg_Y_displacement ~ STA5_PressVal*Genotype)),
         predict1 = map(model1, predict),
         predict2 = map(model2, predict),
         predict3 = map(model3, predict),
         aov_1 = map2(model1, model2, anova), # Comparing simple vs polynomial for each rate.
         aov_2 = map2(model2, model3, anova)
  )

TableMaxStep2dpf %>% unnest(tidied3)


##Plots----


#### Plot displacement 2 dpf

PlotDispAvg2dpf <- ggplot(
    TableAvgStep2dpf,
    aes(RelTime, Avg_Y_displacement_mean, col = Pressure_Level)) +
    ThemePlot +
    geom_line() +
    scale_colour_viridis(
      discrete = TRUE,
      option = "D",
      direction = 1,
      end = 0.5
    ) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-10, 130)) +
    scale_y_continuous(breaks = seq(-0.4, 2, 0.2), limits = c(-0.5, 2)) +
    coord_cartesian(xlim = c(-10, 90), ylim = c(-0.4, 1.2)) +
    geom_vline(xintercept = 0, color = "black") +
    geom_errorbar(aes(ymin = Avg_Y_displacement_mean,
                      ymax = Avg_Y_displacement_mean + Avg_Y_displacement_se)) + # error bar is only shown on top
    labs(
      x = "time after stimulus (s)",
      y = " vertical displacement (mm/s)",
      color = str_wrap("set pressure (mbar)", width = 15)
    ) +
    geom_vline(
      xintercept = 60, color = "gray",
      linetype = 2
    ) +
    guides(color = guide_legend(keyheight = 0.3))

PlotDispAvg2dpf

##### Saving PNG of plot -


ggsave("Manuscript/pictures/PanelTvsAVGDisp_step2dpf.png",
       width = 1000, height = 800, units = "px", device = "png"
)

### Plot Max.displacement vs Pressure 2 dpf
PlotAVGPressVSMaxDisp2dpf <- (
    ggplot(
      TableMaxStep2dpf %>%
        unnest(c(values, predict3)),
      aes(max_STA5_PressVal, max_STA5_Corr_Avg_Y_displacement, col = Pressure_Level)
           ) +
      ThemePlot +
      background_grid(major = "none", minor = "none") +
      geom_violin(alpha = 0.7, size = 0.3) +
      geom_point(size = 2, shape = 20, alpha = 0.5) +
      geom_line(aes(group = Batch_ID),
                alpha = 0.3) +
      geom_line(aes(group = Genotype, y = predict3, col = Pressure_Level), linewidth = 1) +
      scale_colour_viridis(
        discrete = TRUE,
        option = "D",
        direction = 1,
        end = 0.5
        ) +
      scale_x_continuous(
        breaks = c(0, 250, 500, 750, 1000),
        limits = c(-1, 1000)
        ) +
      scale_y_continuous(
        breaks = seq(-0.2, 1.6, 0.2),
        limits = c(-0.1, 2)
        ) +
      coord_cartesian(ylim = c(0, 1.4)) +
      geom_vline(
        xintercept = 0,
        color = "black",
        linewidth = 0.5
        ) +
      geom_hline(
        yintercept = 0,
        color = "black",
        linewidth = 0.5
      ) +
      labs(
        x = "pressure (mbar)",
        y = str_wrap("max. ∆ vertical displacement (mm/s)", width = 20),
        color = str_wrap("set pressure (mbar)", width = 15)
        ) +
      geom_point(
        data = AvgMaxStep2dpfWT,
        aes(x = max_STA5_PressVal_mean, y = max_STA5_Corr_Avg_Y_displacement_mean),
        size = 3,
        shape = 1
        ) +
      guides(color = guide_legend(keyheight = 0.3))
)

PlotAVGPressVSMaxDisp2dpf

#### Saving PNG of plot -


ggsave("Manuscript/pictures/Panel_prVSMaxAVGdisp_step2dpf.png",
       width = 1200, height = 800, units = "px", device = "png", bg = "white"
)

#3 dpf --------------------------------------------------
### read data 3 dpf

TableIndStep3dpf <- read.csv("Data/SourceData_elife/Figure1-SourceData2.csv",
                             header = TRUE, sep = ",")
TableIndStep3dpf <- as_tibble(TableIndStep3dpf)

##Visually assessing track number to decide cut off
Cutoff3dpf <- 100

(
  ggplot(TableIndStep3dpf, aes(x = RelTime, y = Num_Tracks_Up + Num_Tracks_Down)) +
    geom_line() +
    geom_hline(yintercept = Cutoff3dpf, color = "red") +
    facet_wrap(~Pressure_Level))

TableIndStep3dpf$Pressure_Level <- factor(TableIndStep3dpf$Pressure_Level,
                                          levels =  c("3.125", "12.5", "22.5",
                                                      "32.5", "42.5", "65",
                                                      "85", "137.5", "237.5",
                                                      "556", "778", "988"))

#Smoothing curves to get maximal values

TableIndStep3dpf <-
  TableIndStep3dpf %>%
  group_by(Trial_ID) %>%
  mutate(across(Avg_Speed:Corr_Num_Tracks_Down,
                ~rollmean(rollmean(.x, k = 5, na.pad = TRUE),
                          k = 5, na.pad = TRUE),
                .names = "STA5_{.col}"))


#Calculating aggregate tables

####AVG. values-
TableAvgStep3dpf <- TableIndStep3dpf %>%
  filter(Num_Tracks_Up + Num_Tracks_Down >= Cutoff3dpf) %>%
  group_by(Pressure_Level, Type_Experiment, Genotype, RelTime) %>%
  summarise(across(Avg_Speed:STA5_Corr_Num_Tracks_Down, list(mean = ~mean(.x, na.rm = TRUE),
                                                            sd = ~sd(.x, na.rm = TRUE),
                                                            se = ~sd(.x / sqrt(length(.x)), na.rm = TRUE))))

TableAvgStep3dpf$n_trials <- count(TableIndStep3dpf %>%
                                     filter(Num_Tracks_Up + Num_Tracks_Down >= Cutoff3dpf) %>%
                                     group_by(Pressure_Level, Type_Experiment, Genotype, RelTime))$n

####Max. values-

TableMaxStep3dpf <-
  TableIndStep3dpf %>%
  filter(Num_Tracks_Up + Num_Tracks_Down >= Cutoff3dpf &
           RelTime > 0 & RelTime <=  StimulusDuration) %>%
  group_by(Trial_ID,
           Pressure_Level,
           Type_Experiment,
           Genotype,
           Batch_ID) %>%
  summarise(across(Avg_Speed:STA5_Corr_Num_Tracks_Down,
                   ~max(.x, na.rm = TRUE), .names = "max_{.col}")
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
        list(mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        se = ~sd(.x / sqrt(length(.x)), na.rm = TRUE)))
      )

###Regression model
##nesting data for mapping
TableMaxStep3dpf <-
  TableMaxStep3dpf  %>%
  filter(Pressure_Level %in%  c("3.125", "12.5", "22.5",
                                "32.5", "42.5", "65",
                                "85", "137.5", "237.5",
                                "556", "778", "988"))  %>%
  ungroup()  %>%
  nest(values = -Genotype)

##calculating a polynomial regression model on the data grouped by genotype.
TableMaxStep3dpf <- TableMaxStep3dpf %>%
  mutate(model1 = map(values, ~lm(data = .x, max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressVal)),
         model2 = map(values, ~lm(data = .x, max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressVal +
                                    I(max_STA5_PressVal^2))),
         model3 = map(values, ~lm(data = .x, max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressVal +
                                    I(max_STA5_PressVal^2) +
                                    I(max_STA5_PressVal^3))),
         tidied1 = map(model1, tidy),
         tidied2 = map(model2, tidy),
         tidied3 = map(model3, tidy),
         # intera = map(values,~lm(data = .x, STA5_Corr_Avg_Y_displacement ~ STA5_PressVal*Genotype)),
         predict1 = map(model1, predict),
         predict2 = map(model2, predict),
         predict3 = map(model3, predict),
         aov_1 = map2(model1, model2, anova), # Comparing simple vs polynomial for each rate.
         aov_2 = map2(model2, model3, anova),
  )

TableMaxStep3dpf %>% unnest(aov_2) %>% select("Pr(>F)")




##Plots-----
#### Plot displacement 3dpf
PlotDispAvg3dpf <- (
  ggplot(
    TableAvgStep3dpf %>%
      filter(Pressure_Level %in%  c("3.125", "12.5", "22.5", "32.5",
                                    "42.5", "65", "85", "137.5",
                                    "237.5", "556", "778", "988")),
    aes(RelTime, Avg_Y_displacement_mean, col = Pressure_Level)
  ) +
    ThemePlot +
    background_grid(major = "none", minor = "none") +
    geom_hline(yintercept = 0) +
    geom_line() +
    guides(colour = "none") +
    scale_colour_viridis(
      discrete = TRUE,
      option = "D",
      direction = 1,
      end = 0.5
    ) +
    scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-10, 130)) +
    scale_y_continuous(breaks = seq(-0.4, 2, 0.2), limits = c(-1, 2)) +
    coord_cartesian(xlim = c(-10, 90), ylim = c(-0.5, 1.2)) +
    geom_vline(xintercept = 0, color = "black") +
    geom_errorbar(aes(ymin = Avg_Y_displacement_mean,
                      ymax = Avg_Y_displacement_mean + Avg_Y_displacement_se)) + # error bar is only shown on top
    labs(
      x = "time after stimulus (s)",
      y = " vertical displacement (mm/s)",
      color = str_wrap("set pressure (mbar)", width = 15)
    ) +
    geom_vline(xintercept = 60, color = "gray", linetype = 2) +
    guides(color = guide_legend(keyheight = 0.3))
)

PlotDispAvg3dpf
#### Saving PNG of plot

ggsave("Manuscript/pictures/PanelTvsAVGDisp_step3dpf.png",
       width = 1000, height = 800, units = "px", device = "png"
)

### Plot Max.displacement vs Pressure 3 dpf

PlotAVGPressVSMaxDisp3dpf <-
  TableMaxStep3dpf  %>%
  unnest(c(values, predict3))  %>%
  ggplot(aes(max_STA5_PressVal,
             max_STA5_Corr_Avg_Y_displacement,
             col = Pressure_Level)) +
  geom_violin(alpha = 0.7, size = 0.3) +
  geom_point(size = 2, shape = 20, alpha = 0.5) +
  geom_line(aes(group = Batch_ID),
            alpha = 0.3) +
  geom_line(aes(group = Genotype,
                 y = predict3,
                 col = Pressure_Level),
            size = 1) +
  geom_vline(
      xintercept = 0,
      color = "black",
      size = 0.5
  ) +
  geom_hline(
      yintercept = 0,
      color = "black",
      size = 0.5
  ) +
  scale_colour_viridis(
      discrete = TRUE,
      option = "D",
      direction = 1,
      end = 0.5
  ) +
  scale_x_continuous(
      breaks = c(0, 50, 100, 150, 200, 250),
      limits = c(0, 250)
  ) +
  scale_y_continuous(
    breaks = seq(-0.2, 1.6, 0.2),
    limits = c(-0.1, 1.6)
  ) +
  coord_cartesian(xlim = c(-10, 250), ylim = c(0, 1.6)) +
  labs(
    x = "pressure (mbar)",
    y = str_wrap("max. ∆ vertical displacement (mm/s)", width = 20),
    color = str_wrap("set pressure (mbar)", width = 15)
  ) +
  geom_point(
    data = AvgMaxStep3dpfWT %>%
      filter(Pressure_Level %in%  c("3.125", "12.5", "22.5", "32.5",
                                    "42.5", "65", "85",
                                    "137.5", "237.5", "556",
                                    "778", "988")),
    aes(x = max_STA5_PressVal_mean, y = max_STA5_Corr_Avg_Y_displacement_mean, col = Pressure_Level),
    size = 3,
    shape = 1
  ) +
  ThemePlot +
  background_grid(major = "none", minor = "none") +
  guides(color = guide_legend(keyheight = 0.3))

PlotAVGPressVSMaxDisp3dpf

#### Saving PNG of plot


ggsave(
  "Manuscript/pictures/Panel_prVSMaxAVGdisp_step3dpf.png",
  width = 1200, height = 800, units = "px", device = "png", bg = "white"
)

# Long displacement 3dpf -----
### read data
TableIndLongDisp <- read_csv("Data/SourceData_elife/Figure1-SourceData3.csv")

##Visually assessing track number to decide cut off
CutoffLong <- 100

(
  ggplot(TableIndLongDisp, aes(x = RelTime, y = Num_Tracks_Up + Num_Tracks_Down)) +
    geom_line() +
    geom_hline(yintercept = CutoffLong, color = "red") +
    facet_wrap(~Pressure_Level))
###define pressure levels

TableIndLongDisp$Pressure_Level <- factor(
  TableIndLongDisp$Pressure_Level,
  levels = c("0", "3.125", "4", "12.5", "22.5", "32.5", "42.5", "65",
             "85", "103.75", "112.5", "122.5", "132.5", "137.5", "142.5",
             "165", "185", "187.5", "237.5", "514", "534", "642", "686",
             "738", "988")
)
TableIndLongDisp$Category_pressure <- factor(
  TableIndLongDisp$Category_pressure,
  levels = c("0", "3.125", "12.5", "22.5", "32.5", "42.5", "65",
             "85", "137.5", "142.5", "187.5", "237.5", "488")
)
TableIndLongDisp$Basal_pressure <-
  factor(TableIndLongDisp$Basal_pressure,
         levels = c("0", "100", "500"))


TableIndLongDisp$Fraction_increase <- factor(TableIndLongDisp$Fraction_increase,
                                             levels = c(as.character(sort(unique(TableIndLongDisp$Fraction_increase)))))
TableIndLongDisp$Fraction_increase <- stri_replace_all_regex(TableIndLongDisp$Fraction_increase,
                       pattern = levels(TableIndLongDisp$Fraction_increase),
                       replacement = as.character(round(as.numeric(levels(TableIndLongDisp$Fraction_increase)), 2)),
                       vectorize = FALSE)
TableIndLongDisp$Fraction_increase <- as_factor(TableIndLongDisp$Fraction_increase)
TableIndLongDisp$Fraction_increase <- factor(TableIndLongDisp$Fraction_increase,
                                             levels = as.character(sort(as.numeric(
                                               levels(TableIndLongDisp$Fraction_increase)))))
#Smoothing curves to get maximal values

TableIndLongDisp <-
  TableIndLongDisp %>%
  group_by(Trial_ID) %>%
  mutate(across(Avg_Speed:PressInc,
                ~rollmean(rollmean(.x, k = 5, na.pad = TRUE),
                          k = 5, na.pad = TRUE),
                .names = "STA5_{.col}"))


#Calculating aggregate tables
####AVG. values
TableAvgLongStep3dpf <- (
  TableIndLongDisp %>%
    filter(Num_Tracks_Up + Num_Tracks_Down >= CutoffLong) %>%
    group_by(
      Pressure_Level,
      Type_Experiment,
      Genotype,
      RelTime,
      Category_pressure,
      Basal_pressure,
      Fraction_increase) %>%
    summarise(
      across(Avg_Speed:PressInc,
        list(mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        se = ~sd(.x / sqrt(length(.x)), na.rm = TRUE))))
)

TableAvgLongStep3dpf$n_trials <-
 count(TableIndLongDisp %>%
filter(Num_Tracks_Up + Num_Tracks_Down >= CutoffLong) %>%
  group_by(
    Pressure_Level,
    Type_Experiment,
    Genotype,
    RelTime,
    Category_pressure,
    Fraction_increase,
    Basal_pressure))$n

####Max. values
TableLongMaxStep3dpf <- (
  TableIndLongDisp %>%
    filter(Num_Tracks_Up + Num_Tracks_Down >= CutoffLong &
             RelTime > 0 & RelTime <=  StimulusDuration) %>%
    group_by(
      Trial_ID,
      Pressure_Level,
      Type_Experiment,
      Genotype,
      Batch_ID,
      Category_pressure,
      Basal_pressure,
      Fraction_increase) %>%
    summarise(
      across(Avg_Speed:STA5_PressInc,
             ~max(.x, na.rm = TRUE), .names = "max_{.col}")
      )
)
TableLongMaxStep3dpf["max_PressVal"][TableLongMaxStep3dpf["max_PressVal"] == -Inf] <- NA
TableLongMaxStep3dpf["max_STA5_PressVal"][TableLongMaxStep3dpf["max_STA5_PressVal"] == -Inf] <- NA
TableLongMaxStep3dpf["max_PressInc"][TableLongMaxStep3dpf["max_PressInc"] == -Inf] <- NA
TableLongMaxStep3dpf["max_STA5_PressInc"][TableLongMaxStep3dpf["max_STA5_PressInc"] == -Inf] <- NA


AvgLongMaxStep3dpfWT <- (
  TableLongMaxStep3dpf %>%
    group_by(
      Pressure_Level,
      Type_Experiment,
      Genotype,
      Category_pressure,
      Basal_pressure,
      Fraction_increase) %>%
    summarise(
      across(
        max_Avg_Speed:max_STA5_PressInc,
        list(mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        se = ~sd(.x / sqrt(length(.x)), na.rm = TRUE)))
      )
)


###Regression model
##nesting data for mapping
TableLongMaxStep3dpf <-
  TableLongMaxStep3dpf  %>%
  drop_na() %>%
  ungroup()  %>%
  nest(values = -Basal_pressure)

##calculating a polynomial regression model on the data grouped by genotype.
TableLongMaxStep3dpf <- TableLongMaxStep3dpf %>%
  mutate(model1 = map(values, ~lm(data = .x, max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressInc)),
         model2 = map(values, ~lm(data = .x, max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressInc +
                                    I(max_STA5_PressInc^2))),
         model3 = map(values, ~lm(data = .x, max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressInc +
                                    I(max_STA5_PressInc^2) +
                                    I(max_STA5_PressInc^3))),
         tidied1 = map(model1, tidy),
         tidied2 = map(model2, tidy),
         tidied3 = map(model3, tidy),
         # intera = map(values,~lm(data = .x, STA5_Corr_Avg_Y_displacement ~ STA5_PressVal*Genotype)),
         predict1 = map(model1, predict),
         predict2 = map(model2, predict),
         predict3 = map(model3, predict),
         aov_1 = map2(model1, model2, anova), # Comparing simple vs polynomial for each rate.
         aov_2 = map2(model2, model3, anova),
  )

TableLongMaxStep3dpf %>% unnest(aov_1)


### Plot displacement 3dpf long pressure treatment ----


PlotDisp3dpfLong <- (
  ggplot(TableAvgLongStep3dpf %>%
           filter(!Fraction_increase %in% c("0", "0.27")),
         aes(
           RelTime,
           Avg_Y_displacement_mean,
           col = Fraction_increase)
  ) +
    ThemePlot +
    theme(strip.text.x = element_text(size = 10),
          strip.background = element_blank()) +
    geom_line() +
    background_grid(major = "none", minor = "none") +
    geom_hline(yintercept = 0) +
    scale_colour_viridis(
      discrete = TRUE,
      option = "D",
      direction = 1,
      end = 0.5
    ) +
    scale_x_continuous(breaks = seq(0, 100, 25), limits = c(-10, 100)) +
    scale_y_continuous(breaks = seq(-0.4, 1.5, 0.2), limits = c(-0.5, 1.2)) +
    geom_vline(xintercept = 0, color = "black") +
    geom_errorbar(
      aes(
        ymin = Avg_Y_displacement_mean,
        ymax = Avg_Y_displacement_mean + Avg_Y_displacement_se)
    ) + # error bar is only shown on top
    labs(
      x = "time after stimulus (s)",
      y = " vertical displacement (mm/s)",
      color = str_wrap("set % pressure increment", width = 15)
    ) +
    geom_vline(xintercept = 60, color = "gray", linetype = 2) +
    facet_wrap(~Basal_pressure) +
    guides(color = guide_legend(keyheight = 0.3))
)

PlotDisp3dpfLong

#### Saving PNG of plot

ggsave("Manuscript/pictures/PanelTvsAvgDisp_Long3dpf.png",
       width = 2000, height = 800, units = "px", device = "png", bg = "white"
)

###pressInc vs MaxDisp-

DispVSPressIncLong <- TableLongMaxStep3dpf %>%
  unnest(c(values, predict2))  %>%
  ggplot(
    aes(
      max_STA5_PressInc,
      max_STA5_Corr_Avg_Y_displacement,
      col = Basal_pressure,
      group = Category_pressure),
  ) +
  ThemePlot +
  background_grid(major = "none", minor = "none") +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0) +
  #   geom_violin(scale = "count",alpha = 0.7, size = 0.3) +
  geom_point(alpha = 0.5, size = 2, shape = 20) +
  geom_line(aes(group = Batch_ID), alpha = 0.3) +
  geom_line(aes(group = Basal_pressure,
                 y = predict2,
                 col = Basal_pressure),
            linewidth = 1.5) +
  scale_color_manual(values = CbbPalette) +
  scale_y_continuous(breaks = seq(0, 1.6, 0.2), limits = c(0, 1.6)) +
  labs(x = "% pressure increment",
       y = str_wrap("max. ∆ vertical displacement (mm/s)", width = 20),
       color = str_wrap("acclimation pressure (mbar)", width = 15)
  ) +
  geom_point(
    data = AvgLongMaxStep3dpfWT,
    aes(x = max_STA5_PressInc_mean, y = max_STA5_Corr_Avg_Y_displacement_mean, col = Basal_pressure),
    size = 3,
    shape = 1
  )

DispVSPressIncLong


#### Saving PNG of plot

ggsave("Manuscript/pictures/PanelPcIncvsDisp_Long3dpf.png",
       width = 1200, height = 800, units = "px", device = "png", bg = "white"
)


# CB ----------------------------------------------------------------------

### read data 2 dpf
TableCiliaNonbinned <- read_csv("Data/SourceData_elife/Figure1-SourceData4.csv")


###define pressure levels
TableCiliaNonbinned$Pressure_Level <- factor(TableCiliaNonbinned$Pressure_Level,
                               levels = c("0", "3.125", "32.5",
                                          "85", "237.5",
                                          "556", "988")
)

TableCiliaNonbinned$Period <- factor(TableCiliaNonbinned$Period,
                       levels = c("Before", "During_1", "During_2", "After"),
                       labels = c("Before", "Stimulus", "During_2", "After")
                       )

TableCiliaNonbinned$Genotype <- factor(TableCiliaNonbinned$Genotype ,
                                       levels = c("WT", "Cops8bD"),
                                       labels = c("WT",'"c-ops-1"^"∆8/∆8"')
                                       )

### Calculating metrics
####SMA/STA-CBF
TableCiliaNonbinned <-
  TableCiliaNonbinned %>%
  group_by(Trial_ID) %>%
  mutate(CBF_sma3 = rollmean(CBF, k = 3, na.pad = TRUE),
         CBF_sta3 = rollmean(rollmean(CBF, k = 3, na.pad = TRUE),
                             k = 3, na.pad = TRUE),
         CBFmoda_sma3 = rollmean(CBF_MODA, k = 3, na.pad = TRUE),
         CBFmoda_sta3 = rollmean(rollmean(CBF_MODA, k = 3, na.pad = TRUE),
                             k = 3, na.pad = TRUE))


####dCBF value

PriorCBFMean <-
  TableCiliaNonbinned %>%
  ungroup() %>%
  filter(Period %in% "Before") %>%
  group_by(Trial_ID) %>%
  summarise(MeanPrior_staCBF = mean(CBF_sta3[Beat == 1], na.rm = TRUE),
            MeanPrior_staCBFmoda = mean(CBFmoda_sta3[Beat == 1], na.rm = TRUE)) %>%
  arrange(Trial_ID) %>%
  group_by(Trial_ID)


TableCiliaNonbinned <-
  TableCiliaNonbinned %>%
  group_by(Trial_ID) %>%
  mutate(dstaCBF = CBF_sta3 - PriorCBFMean$MeanPrior_staCBF[cur_group_id()],
         dstaCBFmoda = CBFmoda_sta3 - PriorCBFMean$MeanPrior_staCBFmoda[cur_group_id()]) %>%
  relocate(dstaCBF, dstaCBFmoda, CBF_sma3, CBF_sta3,CBF_MODA, CBFmoda_sma3, CBFmoda_sta3, .after = CBF)

####max.CBFs
MxCBFbeat <- (
  TableCiliaNonbinned %>%
    group_by(Pressure_Level,
             Genotype,
             Trial_ID,
             Period) %>%
    # filter(RelTime > 60 | RelTime <= 30) %>% (in case comparing same size intervals)
  summarise(across(CBF:CBFmoda_sta3, ~max(.x[Beat == 1], na.rm = TRUE), .names = "max_{.col}")) %>%
    arrange(Trial_ID)
)

MxCBFbeat["max_CBF"][MxCBFbeat["max_CBF"] == -Inf] <- NA
MxCBFbeat["max_dstaCBF"][MxCBFbeat["max_dstaCBF"] == -Inf] <- NA
MxCBFbeat["max_dstaCBFmoda"][MxCBFbeat["max_dstaCBFmoda"] == -Inf] <- NA
MxCBFbeat["max_CBF_sma3"][MxCBFbeat["max_CBF_sma3"] == -Inf] <- NA
MxCBFbeat["max_CBF_sta3"][MxCBFbeat["max_CBF_sta3"] == -Inf] <- NA
MxCBFbeat["max_CBFmoda_sta3"][MxCBFbeat["max_CBFmoda_sta3"] == -Inf] <- NA
MxCBFbeat["max_CBFmoda_sma3"][MxCBFbeat["max_CBFmoda_sma3"] == -Inf] <- NA


#### CBF relation to pressure stimulus

RefPeriod <- "Stimulus"
Metric <- "max_CBFmoda_sta3"
MxCBFbeat <- (MxCBFbeat %>%
            group_by(Trial_ID,
                     Pressure_Level,
                     Genotype) %>%
                 filter(all(!is.na(max_CBFmoda_sta3))) %>%
                 group_modify(~DirectionCBF(., RefPeriod, Metric))
               )



### Statistical test


ggplot(MxCBFbeat,aes(x =max_CBFmoda_sta3)) + geom_histogram()

##### Testing differences between Periods for each pressure level(paired one tail t-test)
StatTestCBF <- MxCBFbeat %>%
  filter(Genotype %in% c("WT") &
           Period %in% c("Before", "Stimulus") &
           Pressure_Level %in% c("3.125", "85", "237.5", "556", "988")) %>%
  group_by(Pressure_Level, Genotype) %>%
  t_test(max_CBFmoda_sta3 ~ Period, alternative = "less", paired = TRUE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
StatTestCBF

print(StatTestCBF, n = 100)

StatTestCBF <- StatTestCBF %>%
  add_xy_position(x = "Period")




### Plotting with P-values-----

MaxPlot <- (
  ggplot(
    MxCBFbeat %>%
      filter(Genotype %in% c("WT") &
                Period %in% c("Before", "Stimulus") &
                Pressure_Level %in% c("3.125", "85", "237.5", "556", "988")),
    aes(x = Period, y = max_CBFmoda_sta3)
    )  +
    ThemePlot +
    theme(strip.text.x = element_text(size = 10),
          strip.background = element_blank()
    ) +
    geom_violin(scale = "count", width = 0.4) +
    geom_point(aes(group = Trial_ID, col = CBFrel), size = 1) +
    labs(
      x = "",
      y = " Max. CBF",
      color = str_wrap("", width = 20)
    ) +
    geom_line(aes(group = Trial_ID, col = CBFrel)) +
    stat_pvalue_manual(
      StatTestCBF %>% filter(Genotype %in% c("WT") &
                                group1 %in% c("Before") &
                                group2 %in% c("Stimulus") &
                                Pressure_Level %in% c("3.125", "85", "237.5", "556", "988")),
      bracket.nudge.y = 0,
      tip.length = 0,
      hide.ns = TRUE,
      step.increase = 0,
      #position = position_jitterdodge(dodge.width = 1),
      label = "p.adj",
      label.size = 3
    ) +
    background_grid(major = "none", minor = "none") +
    geom_hline(yintercept = 0) +
    scale_color_manual(
      values = c("Decrease" = "red",
      "Equal" = "gray",
      "Increase" = "steelblue")
      ) +
    guides(color = "none") +
    scale_y_continuous(
      breaks = seq(0, 30, 5),
      limits = c(0, 30),
      expand = expansion(mult = c(0, 0.1))
    ) +
    coord_cartesian(ylim = c(0 , 25)) +
    facet_wrap(~Pressure_Level, nrow = 1) +
    labs(color = str_wrap("CBF", width = 15)) +
    guides(color = guide_legend(keyheight = 0.3))
)

MaxPlot
#### Saving PNG of plot -

ggsave(
  "Manuscript/pictures/PanelPeriodvsmaxCBF_2dpf.png",
  width = 2000, height = 1000, units = "px", device = "png", bg = "white"
)


# generate figure composite panel grid ------------------------------------

Imgchamb <- readPNG("Manuscript/pictures/ChamberDiagramplaty_Paper1nolabs.png")
Img3dpf <- readPNG("Manuscript/pictures/snapshots_larvae/2022-01-26_Batch2022-01-23_3dpf_Experiment-13_s20SB_50um.png")
Img2dpf <- readPNG("Manuscript/pictures/snapshots_larvae/2022-01-23_Batch2022-01-21_2dpf_Snap-77_crSB50.png")


Xcoord1 <- 0
Fontsize <- 10

PanelChamber <- ggdraw() +
  draw_image(Imgchamb) +
  draw_label("CA = Compressed Air", x = Xcoord1, y = 0.4, size = Fontsize, color = "black", hjust = 0) +
  draw_label("PT = Pressure Transducer", x = Xcoord1, y = 0.35, size = Fontsize, color = "black", hjust = 0) +
  draw_label("SV = Solenoid Valve", x = Xcoord1, y = 0.3, size = Fontsize, color = "black", hjust = 0) +
  draw_label("PCh = Pressure Chamber", x = Xcoord1, y = 0.25, size = Fontsize, color = "black", hjust = 0) +
  draw_label("C = Camera", x = Xcoord1, y = 0.2, size = Fontsize, color = "black", hjust = 0) +
  draw_label("Arduino", x = 0.15, y = 0.56, size = Fontsize, color = "black", hjust = 0) +
  draw_label("air", x = 0.46, y = 0.85, size = Fontsize, color = "black") +
  draw_label("CA", x = 0.05, y = 0.81, size = Fontsize, color = "white") +
  draw_label("SV1", x = 0.16, y = 0.81, size = Fontsize, color = "black") +
  draw_label("SV2", x = 0.29, y = 0.7, size = Fontsize, color = "white", angle = 90) +
  draw_label("PT", x = 0.41, y = 0.74, size = Fontsize, color = "white", angle = 90) +
  draw_label("larvae", x = 0.7, y = 0.66, size = Fontsize, color = "black", angle = 0) +
  draw_label("Pch", x = 0.83, y = 0.77, size = Fontsize, color = "black", angle = 0) +
  draw_label("850 nm\n LEDs", x = 0.55, y = 0.25, size = Fontsize, color = "black", angle = 0) +
  draw_label("C", x = 0.76, y = 0.19, size = Fontsize, color = "black", angle = 0) +
  draw_label("10 cm", x = 0.94, y = 0.55, size = Fontsize, color = "black", angle = 0) +
  draw_label("7 cm", x = 0.72, y = 0.29, size = Fontsize, color = "black", angle = 0)

LarvSca <- 1.2
Larva2d <- ggdraw() + draw_image(Img2dpf, scale = LarvSca)
Larva3d <- ggdraw() + draw_image(Img3dpf, scale = LarvSca)
Xcoord2 <- 0.2

Panel2d <- ggdraw(PlotDispAvg2dpf) +
  draw_label("2 dpf", x = 0.35, y = 0.96, fontfamily = "sans",
             fontface = "plain", size = 11) +
  inset_element(Larva2d,
                left = 0.5,
                bottom = 0.8,
                right = 1,
                top = 1,
                align_to = "panel") +
  draw_label(paste("50 ", "\u00B5", "m", sep = ""),
             x = 0.7, y = 0.05, color = "black",
             fontfamily = "sans", size = 7) +
  geom_segment(aes(x = Xcoord2,
                   y = 0.85,
                   xend = Xcoord2,
                   yend = 0.6,
                   color = "black"),
               arrow = arrow(type = "closed", length = unit(2, "mm")),
               color = "black") +
  geom_segment(aes(x = Xcoord2,
                   y = 0.6,
                   xend = Xcoord2,
                   yend = 0.85,
                   color = "black"),
               arrow = arrow(type = "closed", length = unit(2, "mm")),
               color = "black") +
  draw_label("d", x = Xcoord2, y = 0.9, size = Fontsize, color = "black") +
  draw_label("v", x = Xcoord2, y = 0.5, size = Fontsize, color = "black")


Panel2dDose <- ggdraw(PlotAVGPressVSMaxDisp2dpf)

Xcoord3 <- 0.3

Panel3d <- ggdraw(PlotDispAvg3dpf)  +
  draw_label("3 dpf", x = 0.37, y = 0.96, fontfamily = "sans",
             fontface = "plain", size = 11) +
  inset_element(Larva3d,
                left = 0.5,
                bottom = 0.8,
                right = 1,
                top = 1,
                align_to = "panel") +
 # draw_line(c(0.4,0.5), c(0.07,0.07), color = "white", size = 0.5) +
  draw_label(paste("50 ", "\u00B5", "m", sep = ""),
             x = 0.7, y = 0.1, color = "black",
             fontfamily = "sans", size = 7) +
  geom_segment(aes(x = Xcoord3,
                   y = 0.85,
                   xend = Xcoord3,
                   yend = 0.6,
                   color = "black"),
               arrow = arrow(type = "closed", length = unit(2, "mm")),
               color = "black") +
  geom_segment(aes(x = Xcoord3,
                   y = 0.6,
                   xend = Xcoord3,
                   yend = 0.85,
                   color = "black"),
               arrow = arrow(type = "closed", length = unit(2, "mm")),
               color = "black") +
  draw_label("a", x = Xcoord3, y = 0.9, size = Fontsize, color = "black") +
  draw_label("p", x = Xcoord3, y = 0.5, size = Fontsize, color = "black")

Panel3dDose <- ggdraw(PlotAVGPressVSMaxDisp3dpf)

PanelLongDisp <- ggdraw(PlotDisp3dpfLong) +
  draw_label("acclimation pressure (mb)", x = 0.45, y = 0.99, size = Fontsize, color = "black", fontface = "plain")

PanelCBF <- ggdraw(MaxPlot) +
  draw_label("set pressure (mb)", x = 0.45, y = 0.99, size = Fontsize, color = "black", fontface = "plain")

LayoutFig <- "
AAAABBBCCC
##########
DDDEEEFFFF
##########
GGGGHHHHHH
"

Fig1 <- PanelChamber +
  Panel2d +
  Panel3d +
  Panel2dDose +
  Panel3dDose +
  PanelLongDisp +
  ggdraw(DispVSPressIncLong) +
  PanelCBF +
  plot_layout(design = LayoutFig, heights = c(1, 0.05, 1, 0.05, 1)) +
  plot_annotation(tag_levels = list(
    c("A", "B", " ", "C", " ", "D",
      "E", "F", "G", "H"))) &
  theme(plot.tag = element_text(size = 12, face = "plain"))

ggsave(
    filename = "Manuscript/Figures/Figure1.pdf",
    Fig1, width = 2800, height = 2800,
    units = "px"
)

ggsave(
  filename = "Manuscript/Figures/Figure1.png",
  Fig1, width = 2800, height = 3500,
  units = "px"
)

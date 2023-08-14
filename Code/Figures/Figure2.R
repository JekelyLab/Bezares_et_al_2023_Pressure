########################################
##
## Title: Figure2.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón and Gáspár Jékely
##
## Last Date modified: 2023-02-19
##
## Description: Code to generate Figure 2. It includes results describing the 
## calcium imaging data showing cPRCs are responsive to pressure.
##
## Input files: Metrics tables computed with scripts in dir Code/CaImaging..
##
## Output files: Figure 2 PDF and PNG files
##
## Comments:
##
## Publication: Unpublished
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

# read data Fluo intensity--------------------------------------------------

FinalFluoTable <- read_csv("Data/TablesResults/dRdFPressure_Caimaging.csv")

#Grouping ratiometric measurements

OnlyRatio <-
  FinalFluoTable %>%
    group_by(ExperimentID,
             Cell, Stage,
             Type_Experiment,
             LarvaID) %>%
    filter(Ratiometric %in% c("YES"))

### Smoothing curves to get maximal values

OnlyRatio <-
  OnlyRatio %>%
  group_by(ExperimentID) %>%
  mutate(dRSta31 = rollmean(rollmean(dR, k = 31, na.pad = TRUE),
                             k = 31, na.pad = TRUE)) %>%
  relocate(dRSta31, .after = dR)

###define pressure levels

OnlyRatio$Pressure_Level <- (
  factor(OnlyRatio$Pressure_Level,
         levels = c("100", "250", "500", "750", "1000")
  )
)

#Calculating metrics

###Average of ratiometric trials by rounded time

AvgORRound <-
  OnlyRatio %>%
    group_by(
      Pressure_Level,
      Cell,
      RoundRelTime,
      Type_Experiment,
      Stage)  %>%
    summarise(
      across(
        PressVal:dF,
        list(
          mean = ~mean(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          se = ~sd(.x / sqrt(length(.x)), na.rm = TRUE)
          ))
      )

####Counting sample number and adding to the table

AvgORRound$n <- count(
  OnlyRatio %>%
    group_by(
      Pressure_Level,
      Cell,
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
  filter(RoundRelTime >=  0 & RoundRelTime <=  60) %>%
  filter(dRSta31 ==  max(dRSta31))

####Statistical test
##### Testing differences between Pressure levels for each cell for each pressure level(paired one tail wilcox)
StatTestdRmax <- MaxIndORRoundR %>%
  filter(
    Cell %in% c("cPRC1_l") &
      Type_Experiment %in% c("Titration") &
      Stage %in% c("2dpf")) %>%
  group_by(Cell) %>%
  t_test(dRSta31 ~ Pressure_Level, alternative = "less", paired = FALSE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
StatTestdRmax

print(StatTestdRmax, n = 100)

StatTestdRmax <- StatTestdRmax %>%
  add_y_position()

StatTestdRmax$y.position <- StatTestdRmax$y.position - 2 # to reduce y spacing.
StatTestdRmax$xmin <- as.numeric(StatTestdRmax$group1) # manually setting xmin based on pressure levels.
StatTestdRmax$xmax <- as.numeric(StatTestdRmax$group2)

StatTestdRmax$p.adj <- round(StatTestdRmax$p.adj, 3)

###Regression model-
##nesting data for mapping
MaxIndORRoundR <-
  MaxIndORRoundR %>%
  filter(
    Cell %in% c("cPRC1_l") &
      Type_Experiment %in% c("Titration") &
      Stage %in% c("2dpf")) %>%
  ungroup()  %>%
  nest(values = -Cell)

##calculating a polynomial regression model on the data grouped by genotype.
MaxIndORRoundR <- MaxIndORRoundR %>%
  mutate(model = map(values, ~lm(data = .x, dRSta31 ~ PressVal +
                                   I(PressVal^2) +
                                   I(PressVal^3))),
         #  tidied = map(model, tidy),
         # intera = map(values,~lm(data = .x, STA5_Corr_Avg_Y_displacement ~ STA5_PressVal*Genotype)),
         predict = map(model, predict)
  )
###Plots--------------------------------------------------

####time vs avg dR rounded time

PFluoAvR <- (
  ggplot(AvgORRound %>%
           filter(Cell %in% c("cPRC1_l") &
           Type_Experiment %in% c("Titration") &
           Stage %in% c("2dpf")),
         aes(x = RoundRelTime,
             y = dRSta31_mean,
             col = Pressure_Level)
  ) +
    ThemePlot +
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
    geom_errorbar(aes(ymin = dRSta31_mean,
                      ymax = dRSta31_mean + dRSta31_se,
                      col = Pressure_Level)
    ) +
    scale_x_continuous(
      breaks = seq(-10, 120, 20),
      limits = c(-30, 120)
      ) +
    coord_cartesian(xlim = c(-10, 90)) +
    labs(
      x = "time after stimulus (s)",
      y = expression(paste(
        "cPRC mean ",
        Delta,
        " R/R",
        sep = "")
        ),
      color =  str_wrap("set pressure (mbar)", width = 15)
      ) +
    geom_vline(
      xintercept = 60, color = "gray",
      linetype = 2
    ) +
    guides(col = guide_legend(keyheight = 0.3))
)

PFluoAvR
##### Saving PNG of plot

ggsave("Manuscript/pictures/PanelRoundRelTimvsAVGdRsta-cPRC1_l_step2dpf.png",
       width = 1200, height = 800, units = "px", device = "png"
)

####Press vs ind. Max dR

PressVSmaxdR <- (
  ggplot(MaxIndORRoundR %>%
           unnest(c(values, predict)),
         aes(PressVal,
             dRSta31, col = Pressure_Level)
  ) +
    ThemePlot +
    background_grid(major = "none",
                    minor = "none") +
    geom_point(alpha = 0.5, size = 2, shape = 20) +
    geom_line(aes(group = LarvaID), alpha = 0.3) +
    geom_line(aes(group = Cell,
                   y = predict,
                   col = Pressure_Level),
              size = 1) +
    geom_hline(yintercept = 0) +
    scale_colour_viridis(discrete = TRUE,
                         option = "D",
                         direction = 1,
                         end = 0.5
    ) +
    stat_pvalue_manual(
      StatTestdRmax,
      trans = "log10",
      bracket.nudge.y = 0,
      tip.length = 0,
      hide.ns = TRUE,
      step.increase = 0,
      #position = position_jitterdodge(dodge.width = 1),
      label = "p.adj",
      label.size = 3
    ) +
    scale_x_continuous(breaks = c(0, 100, 250, 500, 750, 1000)) +
    scale_y_continuous(breaks = seq(0, 14, 2),
                       limits = c(0, 9)
    ) +
    geom_vline(xintercept = 0,
               color = "black") +
    labs(x = "pressure (mb)",
         y = expression(paste("cPRC max. ",
                              Delta,
                              " R/R",
                              sep = " ")
         ),
         color =  str_wrap("set pressure (mbar)", width = 15)) +
    guides(col = guide_legend(keyheight = 0.3))
)

PressVSmaxdR


##### Saving PNG of plot

ggsave("Manuscript/pictures/PanelPressvsmaxdR_cPRC1_l_step2dpf.png",
       width = 2200, height = 2000, units = "px", device = "png"
)

# generate figure composite panel grid ------------------------------------
Img1 <- readPNG("Manuscript/pictures/CaImaging_Schematic.png")
Img2  <- readPNG("Manuscript/pictures/CaimagingNovs750mbpanelnolab.png")
Img3  <- readPNG("Manuscript/pictures/2022-09-29_M2_L1_MAX_combined_hyperimage__FINAL_MAX66-97_cyGC_magNIT_annotnolabs.png")
Img4 <- readPNG("Manuscript/pictures/cPRCtimelapsesnapsnolab.png")

PanelSChemPrep <-  ggdraw() + draw_image(Img1)
Fontsize <- 10
###NoPvsP

  Xcoord1 <- 0.02
  Xcoord2 <- 0.85
  Xcoord3 <- 0.42
  Ycoord1 <- 0.95
  Ycoord2 <- 0.47
  Panelnoprvspr <- ggdraw() +
    draw_image(Img2) +
    geom_segment(aes(x = Xcoord1,
                     y = 0.62,
                     xend = Xcoord1,
                     yend = 0.55,
                     color = "white"),
                 arrow = arrow(type = "closed", length = unit(2, "mm")),
                 color = "white") +
    geom_segment(aes(x = Xcoord1,
                     y = 0.55,
                     xend = Xcoord1,
                     yend = 0.62,
                     color = "white"),
                 arrow = arrow(type = "closed", length = unit(2, "mm")),
                 color = "white") +
    draw_label("d", x = Xcoord1, y = 0.65, size = Fontsize, color = "white") +
    draw_label("v", x = Xcoord1, y = 0.53, size = Fontsize, color = "white") +
    draw_label("+ 0 mb", x = Xcoord3, y = Ycoord1, size = Fontsize, color = "white") +
    draw_label("+ 750 mb", x = Xcoord2, y = Ycoord1, size = Fontsize, color = "white") +
    draw_label("+ 0 mb", x = Xcoord3, y = Ycoord2, size = Fontsize, color = "white") +
    draw_label("+ 750 mb", x = Xcoord2, y = Ycoord2, size = Fontsize, color = "white") +
    draw_label("GCaMP6s", x = 0.39, y = 0.56, size = Fontsize, color = "white") +
    draw_label("GCaMP6s", x = 0.39, y = 0.09, size = Fontsize, color = "white") +
    draw_label(paste("50 ", "\u00B5", "m", sep = ""),
               x = 0.92, y = 0.55, size = Fontsize, color = "white") +
    draw_label(paste("20 ", "\u00B5", "m", sep = ""),
              x = 0.9, y = 0.08, size = Fontsize, color = "white")
  

###NitGC cPRC

  Xcoord4 <- 0.19
  Xcoord5 <- 0.61
  Xcoord6 <- 0.73
  Ycoord3 <- 0.98
  Ycoord4 <- 0.47
  PanelNitcPR <- ggdraw() +
    draw_image(Img3) +
    geom_segment(aes(x = Xcoord4,
                     y = 0.7,
                     xend = Xcoord4,
                     yend = 0.6,
                     color = "white"),
                 arrow = arrow(type = "closed", length = unit(2, "mm")),
                 color = "white") +
    geom_segment(aes(x = Xcoord4,
                     y = 0.6,
                     xend = Xcoord4,
                     yend = 0.7,
                     color = "white"),
                 arrow = arrow(type = "closed", length = unit(2, "mm")),
                 color = "white") +
    draw_label("d", x = Xcoord4, y = 0.72, size = Fontsize, color = "white") +
    draw_label("v", x = Xcoord4, y = 0.58, size = Fontsize, color = "white") +
    draw_label("IF", x = Xcoord4, y = Ycoord3, size = Fontsize, color = "cyan") +
    draw_label("live(+ 750 mb)", x = Xcoord5, y = Ycoord3, size = Fontsize, color = "white") +
    draw_label("IF", x = Xcoord4, y = Ycoord4, size = Fontsize, color = "cyan") +
    draw_label("live(+ 750 mb)", x = Xcoord5, y = Ycoord4, size = Fontsize, color = "white") +
    draw_label("GCaMP6s", x = 0.3, y = 0.53, size = Fontsize, color = "white") +
    draw_label("GCaMP6s", x = 0.3, y = 0.03, size = Fontsize, color = "white") +
    draw_label("cilia", x = 0.51, y = 0.15, size = Fontsize, color = "white") +
    draw_label("cilia", x = 0.31, y = 0.73, size = Fontsize, color = "white") +
    draw_label("cPRC_r1", x = 0.4, y = 0.92, size = Fontsize, color = "white") +
    draw_label("cPRC_l1", x = 0.72, y = 0.93, size = Fontsize, color = "white") +
    draw_label("cPRC_r2", x = 0.27, y = 0.87, size = Fontsize, color = "white") +
    draw_label("cPRC_l2", x = 0.72, y = 0.85, size = Fontsize, color = "white") +
    draw_label(paste("50 ", "\u00B5", "m", sep = ""),
               x = Xcoord6, y = 0.55, size = Fontsize, color = "white") +
    draw_label(paste("10 ", "\u00B5", "m", sep = ""),
               x = Xcoord6, y = 0.05, size = Fontsize, color = "white")
  PanelNitcPR

###TLcPCR

  Xcoord7 <- 0.11
  Xcoord8 <- 0.59
  Ycoord5 <- 0.94
  Ycoord6 <- 0.52
  Ycoord7 <- 0.47
  Ycoord8 <- 0.09
  PanelTLcPR <- ggdraw() +
    draw_image(Img4) +
    draw_label("-0.12 s", x = Xcoord7, y = Ycoord5, size = Fontsize, color = "white") +
    draw_label("1.9 s", x = 0.56, y = Ycoord5, size = Fontsize, color = "white") +
    draw_label("3.5 s", x = Xcoord7, y = Ycoord7, size = Fontsize, color = "white") +
    draw_label("after", x = 0.57, y = Ycoord7, size = Fontsize, color = "white") +
    draw_label("GCaMP6s", x = 0.39, y = 0.56, size = Fontsize, color = "white") +
    draw_label("cilia", x = 0.87, y = 0.55, size = Fontsize, color = "white") +
    draw_label("cilia", x = 0.42, y = 0.10, size = Fontsize, color = "white") +
    draw_label("cPRC", x = 0.35, y = 0.35, size = Fontsize, color = "white") +
    draw_label("+ 0 mb", x = Xcoord7, y = Ycoord6, size = Fontsize, color = "white") +
    draw_label("+ 750 mb", x = 0.62, y = Ycoord6, size = Fontsize, color = "white") +
    draw_label("+ 0 mb", x = Xcoord8, y = Ycoord8, size = Fontsize, color = "white") +
    draw_label("+ 750 mb", x = 0.12, y = Ycoord8, size = Fontsize, color = "white") +
    draw_label(paste("10 ", "\u00B5", "m", sep = ""),
               x = 0.88, y = Ycoord8, size = Fontsize, color = "white")
  PanelTLcPR

###assembly
LayoutFig <- "
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
DDDDDDDDDDDDDDDDDDDDDDDDEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
"

Fig2 <-
  PanelSChemPrep + Panelnoprvspr +
  PanelTLcPR + PanelNitcPR +
  ggdraw(PFluoAvR) + ggdraw(PressVSmaxdR) +
  plot_layout(design = LayoutFig, heights = c(1, 1)) +
  plot_annotation(tag_levels = list(
    c("A", "B", "C", "D", "E", "F"))) &
  theme(plot.tag = element_text(size = 12, face = "plain"))


ggsave(
  filename = "Manuscript/Figures/Figure2.png",
  Fig2, width = 2800, height = 2000,
  units = "px"
)

ggsave(
  filename = "Manuscript/Figures/Figure2.pdf",
  Fig2, width = 2800, height = 2020,
  units = "px"
)

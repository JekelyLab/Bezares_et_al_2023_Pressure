########################################
##
## Title:FigureSupplement4.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón
##
## Last Date modified: 2023-02-25
##
## Description: CBF assay schematic, additional CBF metrics.
##
## Input files: metrics tables
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

  library(broom)
  library(cowplot)
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  library(here)
  library(magick)
  library(png)
  library(patchwork)
  library(purrr)
  library(readr)
  library(rstatix)
  library(stringr)
  library(tidyr)
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
  legend.spacing.x = unit(0.2,'mm'),
  legend.box.spacing = margin(0, 0, 0, 0),
  axis.title = element_text(size = 10),
  legend.position = "right",
  panel.background = element_blank()
)

# CB ----------------------------------------------------------------------

 
  ### read data 2 dpf
  TableCiliaNonbinned <- read_csv("Data/TablesResults/CBF_MODA-Closure_CiliaryDynamics_WTCops.csv")
  
  
  ###define pressure levels
  TableCiliaNonbinned$Pressure_Level <- factor(TableCiliaNonbinned$Pressure_Level,
                                               levels = c("0", "3.125", "32.5",
                                                          "85", "237.5",
                                                          "556", "988")
  )
  
  TableCiliaNonbinned$Period <- factor(TableCiliaNonbinned$Period,
                                       levels = c("Before", "During_1", "During_2", "After"),
                                       labels = c("Before", "Stimulus", "Stimulus", "After"))
  
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
    summarise(MeanPrior_staCBF = mean(CBF_sta3, na.rm = TRUE),
              MeanPrior_staCBFmoda = mean(CBFmoda_sta3, na.rm = TRUE)) %>%
    arrange(Trial_ID) %>%
    group_by(Trial_ID)
  
  TableCiliaNonbinned <- 
    TableCiliaNonbinned %>%
    group_by(Trial_ID) %>% 
    mutate(dstaCBF = CBF_sta3 - PriorCBFMean$MeanPrior_staCBF[cur_group_id()],
           dstaCBFmoda = CBFmoda_sta3 - PriorCBFMean$MeanPrior_staCBFmoda[cur_group_id()],
           PcstaCBF = (100*
                         (CBF_sta3 - PriorCBFMean$MeanPrior_staCBF[cur_group_id()])
                       /
                         (PriorCBFMean$MeanPrior_staCBF[cur_group_id()])),
           PcstaCBFmoda = (100*
                         (CBFmoda_sta3 - PriorCBFMean$MeanPrior_staCBFmoda[cur_group_id()])
                       /
                         (PriorCBFMean$MeanPrior_staCBFmoda[cur_group_id()])))  %>%
    relocate(dstaCBF,
             dstaCBFmoda,
             CBF_sma3,
             CBF_sta3,
             PcstaCBF,
             CBF_MODA,
             CBFmoda_sma3,
             CBFmoda_sta3,
             PcstaCBFmoda,
             .after = CBF)
  
  TableAvgCilia <- TableCiliaNonbinned %>% 
  relocate(PressVal, .before = CBF) %>% 
  group_by(Pressure_Level,
           Genotype,
           RelTime,
           Period) %>% 
  mutate(across(CBF:PcstaCBFmoda,
                   list(mean = ~mean(.x[Beat == 1], 
                                     na.rm = TRUE),
                        sd = ~sd(.x[Beat == 1],
                                 na.rm = TRUE),
                        se = ~sd(.x[Beat == 1]/sqrt(length(.x[Beat == 1])),
                                 na.rm = TRUE)))) %>%
    mutate(PressVal_mean = mean(PressVal, 
                                       na.rm = TRUE),
              PressVal_sd = sd(PressVal,
                                   na.rm = TRUE),
              PressVal_se = sd(PressVal/sqrt(length(PressVal)),
                                   na.rm = TRUE))
  
### Time lapse CBF mean
  
  TableCiliaMean <- TableCiliaNonbinned %>%
    group_by(RelTime,
             Pressure_Level,
             Genotype) %>%
    filter(Beat == 1) %>%
    summarise(meanCBFmodaSTA3 = mean(CBFmoda_sta3, na.rm = TRUE)) %>%
    relocate(meanCBFmodaSTA3) %>% print(n = 1000)
  
  
####max.CBFs
  MxCBFbeat <- (
    TableCiliaNonbinned %>%
      group_by(Pressure_Level,
               Genotype,
               Trial_ID,
               Period,
               Larva_ID) %>%
      # filter(RelTime > 60 | RelTime <= 30) %>% (in case comparing same size intervals)
      summarise(across(CBF:PcstaCBFmoda, ~max(.x[Beat == 1], na.rm = TRUE), .names = "max_{.col}")) %>%
      arrange(Trial_ID)
    )
  
  MxCBFbeat["max_CBF"][MxCBFbeat["max_CBF"] == -Inf] <- NA
  MxCBFbeat["max_dstaCBF"][MxCBFbeat["max_dstaCBF"] == -Inf] <- NA
  MxCBFbeat["max_dstaCBFmoda"][MxCBFbeat["max_dstaCBFmoda"] == -Inf] <- NA
  MxCBFbeat["max_CBF_sma3"][MxCBFbeat["max_CBF_sma3"] == -Inf] <- NA
  MxCBFbeat["max_CBF_sta3"][MxCBFbeat["max_CBF_sta3"] == -Inf] <- NA
  MxCBFbeat["max_CBFmoda_sta3"][MxCBFbeat["max_CBFmoda_sta3"] == -Inf] <- NA
  MxCBFbeat["max_CBFmoda_sma3"][MxCBFbeat["max_CBFmoda_sma3"] == -Inf] <- NA
  MxCBFbeat["max_PcstaCBF"][MxCBFbeat["max_PcstaCBF"] == -Inf] <- NA
  MxCBFbeat["max_PcstaCBFmoda"][MxCBFbeat["max_PcstaCBFmoda"] == -Inf] <- NA
  
  
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
  ggplot(MxCBFbeat,aes(x =max_PcstaCBFmoda)) + geom_histogram()
  
  ##### Testing differences in dCBF between pressure levels for WT (paired one tail wilcox)
  
  #dCBF
  stat.testdCBFpressLevelWT <- MxCBFbeat %>%
    filter(Period %in% c("Stimulus") & 
             Genotype %in% c("WT") &
             Pressure_Level %in% c("3.125","85","237.5","556","988")) %>%
    group_by(Period) %>%
    t_test(max_dstaCBFmoda ~ Pressure_Level, alternative = "less", paired = F) %>%
    adjust_pvalue(method = "bonferroni") %>%
    add_significance()
  stat.testdCBFpressLevelWT
  print(stat.testdCBFpressLevelWT, n = 100)
  stat.testdCBFpressLevelWT <- 
    stat.testdCBFpressLevelWT %>% 
    add_y_position()
  stat.testdCBFpressLevelWT$p.adj <- round(stat.testdCBFpressLevelWT$p.adj,4)
  
  #Pc_dCBF
  stat.testPcCBFpressLevelWT <- MxCBFbeat %>%
    filter(Period %in% c("Stimulus") & 
             Genotype %in% c("WT") &
             Pressure_Level %in% c("3.125","85","237.5","556","988")) %>%
    group_by(Period) %>%
    t_test(max_PcstaCBFmoda ~ Pressure_Level, alternative = "less", paired = F) %>%
    adjust_pvalue(method = "bonferroni") %>%
    add_significance()
  stat.testPcCBFpressLevelWT
  print(stat.testPcCBFpressLevelWT, n = 100)
  stat.testPcCBFpressLevelWT <- 
    stat.testPcCBFpressLevelWT %>% 
    add_y_position()
  stat.testPcCBFpressLevelWT$p.adj <- round(stat.testPcCBFpressLevelWT$p.adj,6)
  
  stat.testPcCBFpressLevelWT$y.position <- stat.testPcCBFpressLevelWT$y.position - 90
  

#Plots-----------


####Pressure logs


PlotAveragePressureCBF <- (
  ggplot(TableAvgCilia %>%
           filter(Pressure_Level %in% c("3.125","85","237.5","556","988")),
         aes(RelTime,PressVal_mean,col = Pressure_Level)) +
    ThemePlot +
    theme(legend.position = "right", 
          legend.key.size = unit(0.3, 'cm'),
          legend.key.width= unit(0.1, 'cm')) +
    background_grid(major = 'none', minor = 'none') +
    geom_line() +
    geom_hline(yintercept = 0) +
    scale_colour_viridis(discrete = TRUE, 
                         option = 'D', 
                         direction = 1,
                         end = 0.5) +
    scale_x_continuous(breaks = seq(-30,100,20),limits = c(-30,130)) +
    scale_y_continuous(breaks = seq(0,1000,200),limits = c(-10,2000)) +
    coord_cartesian(xlim = c(-30,90), ylim = c(0,1000)) +
    geom_vline(xintercept = 0,color = "black") +
    geom_errorbar(aes(ymin = PressVal_mean, ymax = PressVal_mean+PressVal_se)) +
    geom_vline(xintercept = 60,color = "gray",linetype = 2) +
    labs(x = "time after stimulus (s)",
         y = "pressure (mb)",
         color =  str_wrap("set pressure (mbar)", width = 15)) +
    guides(col=guide_legend(keyheight = 0.3)) 
  )

PlotAveragePressureCBF

#### Saving PNG of plot-

ggsave(
  "Manuscript/pictures/Panel_tVSPress_CBF_2dpf.png",
  width = 2000, height = 600, units = "px", device = "png", bg = "white"
)

### maxdCBF vs Pressure

MaxDCBFvsPress <- (
  ggplot(
      MxCBFbeat %>% 
        filter(Genotype %in% c("WT") &
                 Period %in% c("Stimulus") &
                 Pressure_Level %in% c("3.125","85","237.5","556","988"))
    ,
    aes(x = Pressure_Level, y = max_dstaCBFmoda, col = Pressure_Level)
  )  +
    ThemePlot +
    theme(legend.position = "right", 
          legend.key.size = unit(0.3, 'cm'),
          legend.key.width= unit(0.1, 'cm')) +
    geom_violin(alpha = 0.7, size = 0.3,scale = "count",  width = 0.4) +
    geom_point(alpha = 0.3, size = 2 , shape = 20) +
    geom_line(aes(group = Larva_ID),alpha = 0.3) + 
    scale_colour_viridis(discrete = TRUE, 
                         option = 'D', 
                         direction = 1,
                         end = 0.5) +
    labs(
      x = "pressure (mb)",
      y = expression(paste("Max. ",Delta," CBF",sep = "")),
      color = str_wrap("", width = 20)
    ) +
    background_grid(major = "none", minor = "none") +
    geom_hline(yintercept = 0) +
    stat_pvalue_manual(
      stat.testdCBFpressLevelWT  %>% 
        filter(Period %in% c("Stimulus") &
                 group1 %in% c("3.125","85","237.5","556","988") &
                 group2 %in% c("3.125","85","237.5","556","988"))
        ,
      bracket.nudge.y = 0, 
      tip.length = 0, 
      hide.ns = TRUE, 
      step.increase = 0, 
      label = "p.adj",
      label.size = 3
    ) +
    scale_y_continuous(
      breaks = seq(0, 30, 5), 
      limits = c(0, 12), 
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(color =  str_wrap("set pressure (mbar)", width = 15)) +
    guides(col=guide_legend(keyheight = 0.3)) 
)

MaxDCBFvsPress

#### Saving PNG of plot-

ggsave(
  "Manuscript/pictures/PanelPressvsdmaxCBF_duringPeriod2dpf.png",
  width = 2000, height = 600, units = "px", device = "png", bg = "white"
)

### max%dCBF vs Pressure

MaxPc_dCBFvsPress <- (
  ggplot(
    MxCBFbeat %>% 
      filter(Genotype %in% c("WT") &
               Period %in% c("Stimulus") &
               Pressure_Level %in% c("3.125","85","237.5","556","988"))
    ,
    aes(x = Pressure_Level, y = max_PcstaCBFmoda, col = Pressure_Level)
  )  +
    ThemePlot +
    theme(legend.position = "right", 
          legend.key.size = unit(0.3, 'cm'),
          legend.key.width= unit(0.1, 'cm')) +
    geom_violin(alpha = 0.7, size = 0.3,scale = "count",  width = 0.4) +
    geom_point(alpha = 0.3, size = 2 , shape = 20) +
    geom_line(aes(group = Larva_ID),alpha = 0.3) + 
    scale_colour_viridis(discrete = TRUE, 
                         option = 'D', 
                         direction = 1,
                         end = 0.5) +
    labs(
      x = "pressure (mb)",
      y = expression(paste("max. % ",Delta," CBF",sep = "")),
      color = str_wrap("", width = 20)
    ) +
    background_grid(major = "none", minor = "none") +
    geom_hline(yintercept = 0) +
    stat_pvalue_manual(
      stat.testPcCBFpressLevelWT  %>% 
        filter(Period %in% c("Stimulus") &
                 group1 %in% c("3.125","85","237.5","556","988") &
                 group2 %in% c("3.125","85","237.5","556","988"))
      ,
      bracket.nudge.y = 0, 
      tip.length = 0, 
      hide.ns = TRUE, 
      step.increase = 0, 
      label = "p.adj",
      label.size = 3,
      alpha = 0.8
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, 20), 
      limits = c(0,100), 
      expand = expansion(mult = c(0, 0.1))
    ) +
    coord_cartesian(ylim = c(0,100)) +
    labs(color =  str_wrap("set pressure (mbar)", width = 15)) +
    guides(col=guide_legend(keyheight = 0.3)) 
)

MaxPc_dCBFvsPress

#### Saving PNG of plot-

ggsave(
  "Manuscript/pictures/PanelPressvsdmaxCBF_duringPeriod2dpf.png",
  width = 2000, height = 600, units = "px", device = "png", bg = "white"
)


## Plot CBF time lapse



PlotCBFtime <- ggplot(
  TableCiliaNonbinned %>%
    filter(Genotype %in% c("WT") &
             Pressure_Level %in% c("3.125", "85", "237.5", "556", "988") &
             Beat == 1),
  aes(x = RelTime, y = CBFmoda_sta3, col = Pressure_Level)
)  +
  ThemePlot +
  theme(strip.text.x = element_text(size = 10),
        strip.background = element_blank()
  ) +
  background_grid(major = "none", minor = "none") +
  geom_line(aes(group = Trial_ID, col = Pressure_Level), 
            alpha = 0.25,
            size = 0.5) +
  scale_colour_viridis(
    discrete = TRUE, 
    option = "D", 
    direction = 1, 
    end = 0.5
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) + 
  geom_vline(
    xintercept = 60, color = "gray",
    linetype = 2
  ) +
  geom_line(data = TableCiliaMean %>%
              filter(Genotype %in% c("WT") &
                       Pressure_Level %in% c("3.125", "85", "237.5", "556", "988")), 
            aes(y = meanCBFmodaSTA3 ,col = Pressure_Level ),
            size = 1) +
  labs(x = "time after stimulus (s)",
       y = "CBF",
       color =  str_wrap("set pressure (mbar)", width = 15)) +
  facet_wrap(~Pressure_Level, nrow = 1)


PlotCBFtime

ggsave(
  "Manuscript/pictures/PanelReltvsCBF2dpf.png",
  width = 2000, height = 1000, units = "px", device = "png", bg = "white"
)


# generate figure composite panel grid ------------------------------------

  img1 <- readPNG("Manuscript/pictures/CBF_assaySchematic4nolabs.png")
  Fontsize = 10
  arrow_Xpos = 0.07
  labScaleY <- 0.05
  panel_assay <-  ggdraw() + 
    draw_image(img1) +
    draw_label("glue", x = 0.08, y = 0.04, size = Fontsize,color = "black") +
    draw_label("ciliary band", x = 0.65, y = 0.46, size = Fontsize,color = "white") +
    draw_label("es", x = 0.4, y = 0.3, size = Fontsize,color = "black") +
    draw_label("pressure \n vessel", x = 0.26, y = 0.92, size = Fontsize,color = "black") +
    draw_label("larva", x = 0.1, y = 0.73, size = Fontsize,color = "black") +
    draw_label("air\ninlet/outlet", x = 0.60, y = 0.97, size = Fontsize,color = "black") +
    draw_label("cuvette", x = 0.17, y = 0.83, size = Fontsize,color = "black") +
    draw_label("> 600 nm", x = 0.31, y = 0.53, size = Fontsize,color = "black") +
    geom_segment(aes(x = arrow_Xpos,
                   y = 0.43,
                   xend = arrow_Xpos,
                   yend = 0.35,
                   color = "black"),
               arrow = arrow(type = 'closed', length = unit(2, "mm")),
               color = "black") +
    geom_segment(aes(x = arrow_Xpos,
                     y = 0.35,
                     xend = arrow_Xpos,
                     yend = 0.43,
                     color = "black"),
                 arrow = arrow(type = 'closed', length = unit(2, "mm")),
                 color = "black") + 
    draw_label("d", x = arrow_Xpos, y = 0.45, size = Fontsize ,color = "black") +
    draw_label("v", x = arrow_Xpos, y = 0.33, size = Fontsize,color = "black") +
    draw_label(paste("50", "\u00B5", "m", sep = ""), 
               x = 0.42, y = labScaleY, size = Fontsize, color = "black") +
    draw_label(paste("50", "\u00B5", "m", sep = ""), 
               x = 0.9, y = labScaleY, size = Fontsize, color = "white")
   
  Panel_CBFtime <-  ggdraw(PlotCBFtime) +
    draw_label("set pressure (mb)", x = 0.5, y = 1, size = Fontsize,color ="black", fontface = "plain") 

  layout <- "
  AB
  ##
  CD
  "
    
  
  FigSupp4 <- panel_assay +
    ggdraw(PlotAveragePressureCBF) +
    Panel_CBFtime +
    ggdraw(MaxPc_dCBFvsPress) +
       plot_layout(design = layout, heights = c(1, 0.1,1)) + 
    plot_annotation(tag_levels = list(
      c("A", "B", "C", "D"))) &
    theme(plot.tag = element_text(size = 12, face = "plain"))

####Saving Figure-
ggsave(
  filename = "Manuscript/Figures/FigureSupplement_4.pdf",
  FigSupp4, width = 2500, height = 1000,units = "px"
)
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_4.png", 
    FigSupp4, width = 2500, height = 2500,
    units = "px"
  )




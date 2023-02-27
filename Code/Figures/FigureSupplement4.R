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
  setwd(ProjectDir )
  
#define ggplot theme--------------------------------------------------
theme_plot <- theme(
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

  ### read data CBF
  TableCiliaNonbinned <- read_csv("Data/TablesResults/CBF-Closure_CiliaryDynamics2dpf_WT-Cops_nonbinned.csv")

  ###define pressure levels
  TableCiliaNonbinned$Pressure_Level <- factor(TableCiliaNonbinned$Pressure_Level, 
                                               levels = c("0", "3.125", "32.5",
                                                          "85", "237.5",
                                                          "556", "988")
  )
  
  TableCiliaNonbinned$Period <- factor(TableCiliaNonbinned$Period, 
                                       levels = c("Before", "During_1","During_2", "After")
  )
  
  levels(TableCiliaNonbinned$Period) <- gsub("During_1", "Stimulus", levels(TableCiliaNonbinned$Period)) 
  
  ### Calculating metrics
  ####SMA/STA-CBF
  TableCiliaNonbinned <- 
    TableCiliaNonbinned %>%
    group_by(Trial_ID) %>% 
    mutate(CBF_sma3 = rollmean(CBF, k = 3, na.pad = T),
           CBF_sta3 = rollmean(rollmean(CBF, k =3, na.pad = T),
                               k = 3, na.pad = T))
  
  
  ####dCBF value
  
  PriorCBFMean <- 
    TableCiliaNonbinned %>%
    ungroup() %>%
    filter(Period %in% "Before") %>%
    group_by(Trial_ID) %>%
    summarise(MeanPrior_staCBF= mean(CBF_sta3, na.rm = TRUE)) %>%
    arrange(Trial_ID) %>%
    group_by(Trial_ID) 
  
  
  TableCiliaNonbinned <- 
    TableCiliaNonbinned %>%
    group_by(Trial_ID) %>% 
    mutate(dstaCBF = CBF_sta3 - PriorCBFMean$MeanPrior_staCBF[cur_group_id()],
           PcstaCBF = (100*
                         (CBF_sta3 - PriorCBFMean$MeanPrior_staCBF[cur_group_id()])
                       /
                         (PriorCBFMean$MeanPrior_staCBF[cur_group_id()])))  %>%
    relocate(dstaCBF, PcstaCBF , CBF_sma3, CBF_sta3, .after = CBF)
  
  TableAvgCilia <- TableCiliaNonbinned %>% 
  relocate(PressVal, .before = CBF) %>% 
  group_by(Pressure_Level,
           Genotype,
           RelTime,
           Period) %>% 
  summarise(across(PressVal:CBF,
                   list(mean = ~mean(.x, 
                                     na.rm = TRUE),
                        sd = ~sd(.x,
                                 na.rm = TRUE),
                        se = ~sd(.x/sqrt(length(.x)),
                                 na.rm = TRUE))))
  
  ####max.CBFs
  MxCBF <- (
    TableCiliaNonbinned %>% 
      group_by(Pressure_Level,
               Genotype,
               Trial_ID,
               Period,
               Larva_ID) %>% 
      # filter(RelTime > 60 | RelTime <= 30) %>% (in case comparing same size intervals)
      summarise(across(CBF:CBF_sta3, ~max(.x,na.rm = TRUE),.names = "max_{.col}")) %>%
      arrange(Trial_ID)
  )
  
  MxCBF["max_CBF"][MxCBF["max_CBF"] == -Inf] <- NA
  MxCBF["max_dstaCBF"][MxCBF["max_dstaCBF"] == -Inf] <- NA
  MxCBF["max_CBF_sma3"][MxCBF["max_CBF_sma3"] == -Inf] <- NA
  MxCBF["max_CBF_sta3"][MxCBF["max_CBF_sta3"] == -Inf] <- NA
  MxCBF["max_PcstaCBF"][MxCBF["max_PcstaCBF"] == -Inf] <- NA
  
  #### CBF relation to pressure stimulus
  
  Ref_period = "Stimulus"
  metric = "max_CBF_sta3"
  MxCBF <-(MxCBF %>% 
             group_by(Trial_ID,
                      Pressure_Level,
                      Genotype) %>% 
             filter(all(!is.na(max_CBF_sta3))) %>% 
             group_modify(~DirectionCBF(.,Ref_period,metric))
  )
  
  

  
  
  
  ### Statistical test
  ggplot(MxCBF,aes(x =max_dstaCBF)) + geom_histogram()
  ggplot(MxCBF,aes(x =max_PcstaCBF)) + geom_histogram()
  
  ##### Testing differences in dCBF between pressure levels for WT (paired one tail wilcox)
  
  #dCBF
  stat.testdCBFpressLevelWT <- MxCBF %>%
    filter(Period %in% c("Stimulus") & 
             Genotype %in% c("WT") &
             Pressure_Level %in% c("3.125","85","237.5","556","988")) %>%
    group_by(Period) %>%
    t_test(max_dstaCBF ~ Pressure_Level, alternative = "less", paired = F) %>%
    adjust_pvalue(method = "bonferroni") %>%
    add_significance()
  stat.testdCBFpressLevelWT
  print(stat.testdCBFpressLevelWT, n = 100)
  stat.testdCBFpressLevelWT <- 
    stat.testdCBFpressLevelWT %>% 
    add_y_position()
  stat.testdCBFpressLevelWT$p.adj <- round(stat.testdCBFpressLevelWT$p.adj,4)
  
  #Pc_dCBF
  stat.testPcCBFpressLevelWT <- MxCBF %>%
    filter(Period %in% c("Stimulus") & 
             Genotype %in% c("WT") &
             Pressure_Level %in% c("3.125","85","237.5","556","988")) %>%
    group_by(Period) %>%
    t_test(max_PcstaCBF ~ Pressure_Level, alternative = "less", paired = F) %>%
    adjust_pvalue(method = "bonferroni") %>%
    add_significance()
  stat.testPcCBFpressLevelWT
  print(stat.testPcCBFpressLevelWT, n = 100)
  stat.testPcCBFpressLevelWT <- 
    stat.testPcCBFpressLevelWT %>% 
    add_y_position()
  stat.testPcCBFpressLevelWT$p.adj <- round(stat.testPcCBFpressLevelWT$p.adj,4)
  
  stat.testPcCBFpressLevelWT$y.position <- stat.testPcCBFpressLevelWT$y.position - 30
  

#Plots-----------


####Pressure logs


PlotAveragePressureCBF <- (
  ggplot(TableAvgCilia %>%
           filter(Pressure_Level %in% c("3.125","85","237.5","556","988")),
         aes(RelTime,PressVal_mean,col = Pressure_Level)) +
    theme_plot +
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
      MxCBF %>% 
        filter(Genotype %in% c("WT") &
                 Period %in% c("Stimulus") &
                 Pressure_Level %in% c("3.125","85","237.5","556","988"))
    ,
    aes(x = Pressure_Level, y = max_dstaCBF, col = Pressure_Level)
  )  +
    theme_plot +
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
    MxCBF %>% 
      filter(Genotype %in% c("WT") &
               Period %in% c("Stimulus") &
               Pressure_Level %in% c("3.125","85","237.5","556","988"))
    ,
    aes(x = Pressure_Level, y = max_PcstaCBF, col = Pressure_Level)
  )  +
    theme_plot +
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
      label.size = 3
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, 20), 
      limits = c(0,100), 
      expand = expansion(mult = c(0, 0.1))
    ) +
    coord_cartesian(ylim = c(0,65)) +
    labs(color =  str_wrap("set pressure (mbar)", width = 15)) +
    guides(col=guide_legend(keyheight = 0.3)) 
)

MaxPc_dCBFvsPress

#### Saving PNG of plot-

ggsave(
  "Manuscript/pictures/PanelPressvsdmaxCBF_duringPeriod2dpf.png",
  width = 2000, height = 600, units = "px", device = "png", bg = "white"
)





# generate figure composite panel grid ------------------------------------

  img1 <- readPNG("Manuscript/pictures/CBF_assaySchematic4nolabs.png")
  Fontsize = 10
  arrow_Xpos = 0.04
  panel_assay <-  ggdraw() + 
    draw_image(img1) +
    draw_label("glue", x = 0.07, y = 0.09, size = Fontsize,color = "black") +
    draw_label("glue", x = 0.79, y = 0.69, size = Fontsize,color = "black") +
    draw_label("ciliary band", x = 0.65, y = 0.46, size = Fontsize,color = "white") +
    draw_label("eyespot", x = 0.39, y = 0.31, size = Fontsize,color = "black") +
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
               x = 0.4, y = 0.1, size = Fontsize, color = "black") +
    draw_label(paste("50", "\u00B5", "m", sep = ""), 
               x = 0.92, y = 0.1, size = Fontsize, color = "white")
    

  layout <- "
  ABC
  "
    
  
  FigSupp4 <- panel_assay + ggdraw(PlotAveragePressureCBF) + ggdraw(MaxPc_dCBFvsPress) +
     plot_layout(design = layout, heights = c(1, 1)) + 
    plot_annotation(tag_levels = list(
      c("A", "B", "C"))) &
    theme(plot.tag = element_text(size = 12, face = "plain"))

####Saving Figure-
ggsave(
  filename = "Manuscript/Figures/FigureSupplement_4.pdf",
  FigSupp4, width = 2500, height = 1000,units = "px"
)
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_4.png", 
    FigSupp4, width = 2500, height = 1000,
    units = "px"
  )




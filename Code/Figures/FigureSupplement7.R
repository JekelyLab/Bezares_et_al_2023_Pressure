########################################
##
## Title:FigureSupplement7.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón
##
## Last Date modified: 2023-02-25
##
## Description: Code to generate Figure Supplement 7. WT vs Cops comparison pressure response.
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
  cbbPalette <- c("#000000", "#D55E00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#E69F00", "#CC79A7")




# Loading libraries--------------------------------------------------
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
    legend.spacing.x = unit(0.2, "mm"),
    legend.box.spacing = margin(0, 0, 0, 0),
    axis.title = element_text(size = 10),
    panel.background = element_blank()
  )
  

# WTCops--------------------------------------------------

### read data 

TableIndStepWTCops <- read_csv("Data/TablesResults/SmallChamber1WTvs1Copsstep_metrics3dpf.csv")

##Visually assessing track number to decide cut off
cutoffWC = 50

(
  ggplot(TableIndStepWTCops,aes(x = RelTime, y = Num_Tracks_Up + Num_Tracks_Down)) +
    geom_line() +
    geom_hline(yintercept = cutoffWC, color = "red") +
    facet_wrap(~Pressure_Level))

###define levels
TableIndStepWTCops$Pressure_Level <- factor(TableIndStepWTCops$Pressure_Level,levels =  c("3.125", "12.5",  "22.5" ,"32.5", "42.5" ,  "85" , "212.5", "988"))
TableIndStepWTCops$Genotype <- factor(TableIndStepWTCops$Genotype ,
                                      levels = c("WT", "Cops8bD"), 
                                      labels = c(expression(italic("WT")),expression(italic(paste("c-ops-",1^{"∆8/∆8"})))))

###Calculating aggregate tables
####AVG. values

TableAvgStepWTCops <- TableIndStepWTCops %>% 
  filter(Num_Tracks_Up + Num_Tracks_Down >= cutoffWC) %>%
  group_by(Pressure_Level,Type_Experiment,Genotype,RelTime) %>% 
  summarise(across(Avg_Speed:Corr_Num_Tracks_Down,
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE),
                        se = ~sd(.x/sqrt(length(.x)),na.rm = TRUE)),
                   na.rm = TRUE))

TableAvgStepWTCops$n_trials <- count(TableIndStepWTCops %>% 
                                       filter(Num_Tracks_Up + Num_Tracks_Down >= 50) %>%
                                       group_by(Pressure_Level,Type_Experiment,Genotype,RelTime))$n

####AVG. prior speed values
AvgPriorSpeed <- TableIndStepWTCops %>%
  filter( Num_Tracks_Up + Num_Tracks_Down >= cutoffWC &
         !Pressure_Level %in% c("988") &
            RelTime < 0) %>%
  group_by(Batch_ID, Genotype) %>%
  summarise(meanBatchSpeed = mean(Avg_Speed)) %>%
  ungroup()

#####Stat.test

ggplot(AvgPriorSpeed,aes(x =meanBatchSpeed)) + geom_histogram()

stat.testSpeed <- AvgPriorSpeed %>%
  wilcox_test(meanBatchSpeed ~ Genotype, alternative = "g", paired = F) %>%
  add_significance()
stat.testSpeed
print(stat.testSpeed, n = 100)

stat.testSpeed <- stat.testSpeed %>% 
  add_y_position()
stat.testSpeed
stat.testSpeed$p <- round(stat.testSpeed$p,3)

## Plots WTvsCops--------


####Pressure logs
  
PlotAveragePressureWTcops <- (
  ggplot(TableAvgStepWTCops %>%
           filter(!Pressure_Level %in% c("988")),
         aes(RelTime,PressVal_mean,col = Pressure_Level)) +
    theme_plot +
    theme(strip.text.x = element_text(size = 10),
          strip.background = element_blank()
    ) +
    background_grid(major = 'none', minor = 'none') +
    geom_line() +
    geom_hline(yintercept = 0) +
    scale_colour_viridis(discrete = TRUE, 
                         option = 'D', 
                         direction = 1,
                         end = 0.5) +
    scale_x_continuous(breaks = seq(0,100,20),limits = c(-10,80)) +
    scale_y_continuous(expand = expansion(), breaks = seq(0,1000,50),limits = c(-10,250)) +
    geom_vline(xintercept = 0,color = "black") +
    geom_errorbar(aes(ymin = PressVal_mean, ymax = PressVal_mean+PressVal_se)) +
    geom_vline(xintercept = 60,color = "gray",linetype = 2) +
    labs(x = "time relative to stimulus (s)",
         y = "pressure (mbar)",
         color = str_wrap("set pressure (mbar)",
                          width = 20)) + 
    facet_wrap(~Genotype,labeller = label_parsed) +#, labeller = labeller(Genotype = Genotype.labs)) +
    guides(color = guide_legend(keyheight = 0.3,ncol = 2))
)

PlotAveragePressureWTcops

##### Saving PNG of plot 

ggsave("Manuscript/pictures/PanelAvgPressure_stepWTCops.png",
       plot = PlotAveragePressureWTcops,
       width = 2200,height = 2000,units = "px",device = "png")


### Plot displacement WT vs Cops  3dpf 
PlotDispAvgWTCops <- (
  ggplot(
    TableIndStepWTCops  %>% 
      filter(Num_Tracks_Up + Num_Tracks_Down >= cutoffWC &
               !Pressure_Level %in% c("988")),
    aes(RelTime, Corr_Avg_Y_displacement)
  ) +
    theme(strip.text.x = element_text(size = 10),
          strip.background = element_blank()
    ) +
    theme_plot +
    background_grid(major = "none", minor = "none") +
    guides(colour = "none") +
    geom_point(aes(col = Genotype), 
               alpha = 0.25,
               size = 0.5) +
    geom_line(data = TableAvgStepWTCops %>%
                filter( Pressure_Level %in% c("3.125", "12.5",  "22.5" 
                                              ,"32.5", "42.5" ,  "85" ,
                                              "212.5")), 
              aes(y = Corr_Avg_Y_displacement_mean ,col = Genotype ),
              linewidth = 1) +
    scale_color_manual(values = cbbPalette) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(0,100,40),limits = c(-10,120)) +
    scale_y_continuous(breaks = seq(-1,2,0.4),limits = c(-1,1.5)) +
    geom_vline(xintercept = 0, color = "black") +
    labs(
      x = "time relative to stimulus (s)",
      y = "∆ vertical displacement (mm/s)",
      color = "genotype"
    ) +
    geom_vline(
      xintercept = 60, color = "gray",
      linetype = 2
    ) +
    facet_grid(vars(Genotype),vars(Pressure_Level),labeller = label_parsed)# +
  #guides(color = guide_legend(keyheight = 1))
)
PlotDispAvgWTCops
#### Saving PNG of plot 

ggsave("Manuscript/pictures/PanelTvsAVGDisp_stepWTCops.png",
       width = 2200, height = 2000, units = "px", device = "png"
)


### Plot speed prior to stimulus WT vs Cops  3dpf 

Glabels <-  rev(parse(text=unique(as.character(AvgPriorSpeed$Genotype))))

PlotPriorSpeedWTCops <- (
  ggplot(AvgPriorSpeed,
         aes(Genotype, meanBatchSpeed, col = Genotype)
  ) +
    theme_plot +
    theme(axis.text.x = element_text(size = 10, angle = 0, colour="black")) +
    background_grid(major = "none", minor = "none") +
    guides(colour = "none") +
    geom_violin()+
    geom_point(alpha = 0.25,
               size = 0.5) +
    scale_color_manual(values = cbbPalette) +
    stat_pvalue_manual(
      stat.testSpeed,
      bracket.nudge.y = 0, 
      tip.length = 0,
      step.increase = 0.05, 
      label = "p") +  
    geom_hline(yintercept = 0) +
     scale_y_continuous(breaks = seq(-1,2,0.4),limits = c(0,1.5)) +
    scale_x_discrete(labels= Glabels) +
    labs(
      x = "",
      y = str_wrap("avg. batch speed (mm/s)",width = 15),
      color = "genotype"
    ) 
  #guides(color = guide_legend(keyheight = 1))
)
PlotPriorSpeedWTCops

#### Saving PNG of plot

ggsave("Manuscript/pictures/Panel_PriorbatchSpeed_WTCops.png",
       width = 2200, height = 2000, units = "px", device = "png"
)


# CB ----------------------------------------------------------------------


### read data 2 dpf
TableCiliaNonbinned <- read_csv("Data/TablesResults/CBF_MODA-Closure_CiliaryDynamics_demo.csv")


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
                                       labels = c(expression(italic("WT")),expression(italic(paste("c-ops-",1^{"∆8/∆8"})))))

Glabels <-  rev(parse(text=unique(as.character(TableCiliaNonbinned$Genotype))))

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
  group_by(Trial_ID,
           Larva_ID,
           Genotype) %>%
  summarise(MeanPrior_staCBF = mean(CBF_sta3, na.rm = TRUE),
            MeanPrior_staCBFmoda = mean(CBFmoda_sta3, na.rm = TRUE),
            MeanPrior_smaCBF= mean(CBF_sma3, na.rm = TRUE),
            MeanPrior_smaCBFmoda= mean(CBFmoda_sma3, na.rm = TRUE)) %>%
  group_by(Larva_ID) %>%
  mutate(Mean_staCBFlarva = mean(MeanPrior_staCBF,  na.rm = TRUE),
         MeanMODA_staCBFlarva = mean(MeanPrior_staCBFmoda,  na.rm = TRUE),
         Mean_smaCBFlarva = mean(MeanPrior_smaCBF,  na.rm = TRUE),
         MeanMODA_smaCBFlarva = mean(MeanPrior_smaCBFmoda,  na.rm = TRUE)) %>%
  arrange(Trial_ID) %>%
  ungroup() %>%
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
  summarise(across(PressVal:PcstaCBFmoda,
                   list(mean = ~mean(.x[Beat == 1], 
                                     na.rm = TRUE),
                        sd = ~sd(.x[Beat == 1],
                                 na.rm = TRUE),
                        se = ~sd(.x[Beat == 1]/sqrt(length(.x[Beat == 1])),
                                 na.rm = TRUE))))

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



####Test CBF differences between WT and Cops prior to stimulus
ggplot(PriorCBFMean,aes(x =MeanMODA_staCBFlarva )) + geom_histogram()

stat.test_CBFprior <- PriorCBFMean %>%
  ungroup() %>% 
  select(Larva_ID,Genotype,MeanMODA_staCBFlarva ,MeanMODA_smaCBFlarva) %>% 
  distinct() %>%
  wilcox_test(MeanMODA_staCBFlarva ~ Genotype, alternative = "less", paired = F) %>%
  add_significance()
stat.test_CBFprior
print(stat.test_CBFprior, n = 100)

stat.test_CBFprior <- stat.test_CBFprior %>% 
  add_y_position()
stat.test_CBFprior$p <- round(stat.test_CBFprior$p,3)

###Plots--------

####avg. prior CBF

PriorCBFlarvaWTCops <- (
  ggplot(
    PriorCBFMean,
    aes(Genotype, MeanMODA_staCBFlarva, col = Genotype)
  ) +
    theme_plot +
    theme(axis.text.x = element_text(size = 10, angle = 0,  colour="black")) +
    geom_violin() +
    geom_point(alpha = 0.25,
               size = 0.5) + 
    scale_y_continuous(
      breaks = seq(0, 30, 5), 
      limits = c(0, 25), 
      expand = expansion(mult = c(0, 0.1))
    ) +
    coord_cartesian(ylim = c(0 , 20)) +
    background_grid(major = "none", minor = "none") +
    geom_hline(yintercept = 0) +
    guides(color = "none") +
    scale_color_manual(values = cbbPalette) +
    stat_pvalue_manual(
      stat.test_CBFprior,
      bracket.nudge.y = 0, 
      tip.length = 0,
      step.increase = 0.05,
      label = "p") +   
    scale_x_discrete(labels= Glabels) +
    labs(
      x = "",
      y = " avg. CBF"
    ) 
)
PriorCBFlarvaWTCops

##### Saving PNG of plot


ggsave("Manuscript/pictures/Panel_priormeanCBF_WTCops.png",
       width = 2200, height = 2000, units = "px", device = "png"
)


### Statistical test dCBF----
ggplot(MxCBFbeat,aes(x =max_dstaCBFmoda )) + geom_histogram()

##### dCBF for Cops mutants for each pressure level(non-paired one tail wilcox)
stat.testdCBFpressLevelCops <- MxCBFbeat %>%
  group_by(Period)  %>%
  filter(!Pressure_Level %in% c("0") &
           Genotype %in% c('italic(paste("c-ops-", 1^{\n    "∆8/∆8"\n}))') &
           Period %in% c("Stimulus")) %>%
  drop_na () %>%
  t_test(max_dstaCBFmoda ~ Pressure_Level, alternative = "less", paired = F) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testdCBFpressLevelCops
print(stat.testdCBFpressLevelCops, n = 100)

stat.testdCBFpressLevelCops <- stat.testdCBFpressLevelCops %>% 
  add_y_position()
stat.testdCBFpressLevelCops$y.position = stat.testdCBFpressLevelCops$y.position-3
stat.testdCBFpressLevelCops$p.adj <- round(stat.testdCBFpressLevelCops$p.adj,3)

##### Pc_dCBF for Cops mutants for each pressure level(non-paired one tail wilcox)
stat.testPc_dCBFpressLevelCops <- MxCBFbeat %>%
  group_by(Period)  %>%
  filter(!Pressure_Level %in% c("0") &
           Genotype %in% c('italic(paste("c-ops-", 1^{\n    "∆8/∆8"\n}))')  &
           Period %in% c("Stimulus")) %>%
  drop_na () %>%
  t_test(max_PcstaCBFmoda ~ Pressure_Level, alternative = "less", paired = F) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testPc_dCBFpressLevelCops
print(stat.testPc_dCBFpressLevelCops, n = 100)

stat.testPc_dCBFpressLevelCops <- stat.testPc_dCBFpressLevelCops %>% 
  add_y_position()
stat.testPc_dCBFpressLevelCops$y.position = stat.testPc_dCBFpressLevelCops$y.position-60
stat.testPc_dCBFpressLevelCops$p.adj <- round(stat.testPc_dCBFpressLevelCops$p.adj,4)


#### maxPc_dCBF vs Pressure

Max_Pc_dCBFPlotCops <- (
  ggplot(
    MxCBFbeat %>% 
      filter(Genotype %in% c('italic(paste("c-ops-", 1^{\n    "∆8/∆8"\n}))')  &
               Period %in% c("Stimulus") &
               Pressure_Level %in% c("3.125","85","237.5","556","988"))
    ,
    aes(x = Pressure_Level, y = max_PcstaCBFmoda, col = Pressure_Level)
  )  +
    theme_plot +
    geom_violin(alpha = 0.7, size = 0.3,scale = "count",  width = 0.4) +
    geom_point(alpha = 0.3, size = 2 , shape = 20) +
    geom_line(aes(group = Larva_ID),alpha = 0.3) + 
    scale_colour_viridis(discrete = TRUE, 
                         option = 'D', 
                         direction = 1,
                         end = 0.5) +
    labs(
      x = "pressure (mb)",
      y = expression(paste("max. %",Delta," CBF",sep = "")),
      color = str_wrap("", width = 20)
    ) +
    background_grid(major = "none", minor = "none") +
    geom_hline(yintercept = 0) +
    guides(color = "none") +
    stat_pvalue_manual(
      stat.testPc_dCBFpressLevelCops,
      bracket.nudge.y = 0, 
      tip.length = 0, 
      hide.ns = TRUE, 
      step.increase = 0, 
      #position = position_jitterdodge(dodge.width = 1), 
      label = "p.adj",
      label.size = 3
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, 20), 
      limits = c(0,100), 
      expand = expansion(mult = c(0, 0.1))
    ) +
    coord_cartesian(ylim = c(0,100)) 
  # facet_grid( ~ Genotype)
)

Max_Pc_dCBFPlotCops

##### Saving PNG of plot


ggsave("Manuscript/pictures/Panel_MaxPc_dCBF_WTCops.png",
       width = 2200, height = 2000, units = "px", device = "png"
)




# generate figure composite panel grid ------------------------------------

  Fontsize = 10
  img1 <- readPNG("Manuscript/pictures/cPRCciliaWTCops_collagenolabs.png")
  x_coord_1 = 0.085
  rectCoor <- -0.008
  rectW <- 5.7
  rectH <- 34.12
  Rect1 <- rectGrob(
    x = rectCoor,
    y = 0.062,
    width = unit(rectW, "mm"),
    height = unit(rectH, "mm"),
    hjust = 0, vjust = 0,
    gp = gpar(fill = NULL, alpha = 1 ,lwd = 1.5)
  )
  Rect2 <- rectGrob(
    x = rectCoor,
    y = 0.505,
    width = unit(rectW, "mm"),
    height = unit(rectH, "mm"),
    hjust = 0, vjust = 0,
    gp = gpar(fill = NULL, alpha = 1 ,lwd = 1.5)
  )
  
  panel_cPRC_collage <- ggdraw() + 
    draw_image(img1,scale = 0.95) +
    draw_label(expression(italic("WT")), angle = 90, x = 0.01 , y = 0.7, size = Fontsize,color = "black") +
    draw_label(expression(italic(paste("c-ops-",1^{"∆8/∆8"}))),angle = 90, x = 0.01, y = 0.3, size = Fontsize,color = "black") +
    draw_label("AcTub", x = x_coord_1, y = 0.11, size = Fontsize,color = "white", hjust = 1) +
    draw_label("α-cPRCcilia", x = x_coord_1, y = 0.16, size = Fontsize,color = "cyan") +
    draw_label(paste("2 ", "\u00B5", "m", sep = ""), 
             x = 0.95, y = 0.12, size = Fontsize, color = "white") +
    draw_grob(Rect1) +
    draw_grob(Rect2)

    PanelPcdCBF <- ggdraw(Max_Pc_dCBFPlotCops) +
      draw_label(expression(italic(paste("c-ops-",1^{"∆8/∆8"}))), x = 0.5, y = 0.9, size = Fontsize,color ="black", fontface = "italic") 
    
    PanelAvgWTCopsDisp <- ggdraw(PlotDispAvgWTCops)  +
      draw_label("set pressure (mb)", x = 0.5, y = 0.99, size = Fontsize, color ="black", fontface = "plain") 
    
    
   layout <- "
  AB
  CC
  CC
  DE
  DE
  FF
  FF
  "
  
  
  FigSupp7 <-
     ggdraw(PlotAveragePressureWTcops) +
     ggdraw(PlotPriorSpeedWTCops) +
     PanelAvgWTCopsDisp +
     ggdraw(PriorCBFlarvaWTCops) +
     PanelPcdCBF +
     ggdraw(panel_cPRC_collage) +
     plot_layout(design = layout, heights = c(1, 1, 1, 1, 0.8, 1, 1)) +
     plot_annotation(tag_levels = list(
       c("A", "B", "C", "D", "E","F"))) &
     theme(plot.tag = element_text(size = 12, face = "plain"))
  
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_7.pdf", 
    FigSupp7, width = 2200, height = 3200,
    units = "px"
  )
  
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_7.png", 
    FigSupp7, width = 2200, height = 3200,
    units = "px"
  )
  
  
 


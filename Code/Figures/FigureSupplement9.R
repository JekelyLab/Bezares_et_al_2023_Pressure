########################################
##
## Title:FigureSupplement9.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón
##
## Last Date modified: 2023-02-25
##
## Description:
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
  cbbPalette <- c("#009E73", "#0072B2","#CC79A7", "#56B4E9", "#000000", "#F0E442", "#E69F00" )


# Loading libraries--------------------------------------------------
  library(broom)
  library(cowplot)
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  library(grid)
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
  
# CBF--------------------------------------------------

### read data
TableCBFTetAndCont <- read_csv("Data/TablesResults/CBF_MODA-Closure_CiliaryDynamics_TetXLC.csv")

###define pressure levels
TableCBFTetAndCont$Pressure_Level <- factor(TableCBFTetAndCont$Pressure_Level, 
                                            levels = c("0", "3.125", "32.5",
                                                       "85", "237.5",
                                                       "556", "988")
)

TableCBFTetAndCont$Period <- factor(TableCBFTetAndCont$Period, 
                                    levels = c("Before", "During_1","During_2", "After"),
                                    labels = c("Before", "Stimulus", "During_2", "After")
)

TableCBFTetAndCont$Plasmid <- factor(TableCBFTetAndCont$Plasmid ,
                                     levels = c("pLB253", "pLB316"),
                                     labels = c("TPHp::tdT","TPHp::tdT-P2A-TeTxLC")
)



### Calculating metrics
####SMA/STA-CBF
TableCBFTetAndCont <- 
  TableCBFTetAndCont %>%
  group_by(Trial_ID) %>%
  mutate(CBF_sma3 = rollmean(CBF, k = 3, na.pad = TRUE),
         CBF_sta3 = rollmean(rollmean(CBF, k = 3, na.pad = TRUE),
                             k = 3, na.pad = TRUE),
         CBFmoda_sma3 = rollmean(CBF_MODA, k = 3, na.pad = TRUE),
         CBFmoda_sta3 = rollmean(rollmean(CBF_MODA, k = 3, na.pad = TRUE),
                                 k = 3, na.pad = TRUE))


####dCBF value

PriorCBFMean <- 
  TableCBFTetAndCont %>%
  ungroup() %>%
  filter(Period %in% "Before") %>%
  group_by(Trial_ID,
           Plasmid,
           Larva_ID) %>%
  summarise(MeanPrior_staCBF = mean(CBF_sta3[Beat == 1], na.rm = TRUE),
            MeanPrior_staCBFmoda = mean(CBFmoda_sta3[Beat == 1], na.rm = TRUE)) %>%
  group_by(Larva_ID) %>%
  mutate(Mean_staCBFlarva = mean(MeanPrior_staCBF,  na.rm = TRUE),
         MeanMODA_staCBFlarva = mean(MeanPrior_staCBFmoda,  na.rm = TRUE)) %>%
  arrange(Trial_ID) %>%
  ungroup() %>%
  group_by(Trial_ID) %>%
  arrange(Trial_ID) %>%
  group_by(Trial_ID) 


TableCBFTetAndCont <- 
  TableCBFTetAndCont %>%
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
                           (PriorCBFMean$MeanPrior_staCBFmoda[cur_group_id()]))) %>%
  relocate(dstaCBF, dstaCBFmoda, CBF_sma3, CBF_sta3,CBF_MODA, CBFmoda_sma3, CBFmoda_sta3, .after = CBF)

### Time lapse CBF mean

TableCiliaTetMean <- TableCBFTetAndCont %>%
  group_by(RelTime,
           Pressure_Level,
           Plasmid) %>%
  filter(Beat == 1 & State %in% ("positive")) %>%
  summarise(meanCBFmodaSTA3 = mean(CBFmoda_sta3, na.rm = TRUE)) %>%
  relocate(meanCBFmodaSTA3) %>% print(n = 1000)


####max.CBFs
MxCBF_Tet <- (
  TableCBFTetAndCont %>% 
    group_by(State,
             Pressure_Level,
             Trial_ID,
             Plasmid,
             Period) %>%
    # filter(RelTime > 60 | RelTime <= 30) %>% (in case comparing same size intervals)
    summarise(across(CBF:PcstaCBFmoda, ~max(.x[Beat == 1], na.rm = TRUE), .names = "max_{.col}")) %>%
    arrange(Trial_ID)
)

MxCBF_Tet["max_CBF"][MxCBF_Tet["max_CBF"] == -Inf] <- NA
MxCBF_Tet["max_dstaCBF"][MxCBF_Tet["max_dstaCBF"] == -Inf] <- NA
MxCBF_Tet["max_dstaCBFmoda"][MxCBF_Tet["max_dstaCBFmoda"] == -Inf] <- NA
MxCBF_Tet["max_CBF_sma3"][MxCBF_Tet["max_CBF_sma3"] == -Inf] <- NA
MxCBF_Tet["max_CBF_sta3"][MxCBF_Tet["max_CBF_sta3"] == -Inf] <- NA
MxCBF_Tet["max_CBFmoda_sta3"][MxCBF_Tet["max_CBFmoda_sta3"] == -Inf] <- NA
MxCBF_Tet["max_CBFmoda_sma3"][MxCBF_Tet["max_CBFmoda_sma3"] == -Inf] <- NA
MxCBF_Tet["max_PcstaCBF"][MxCBF_Tet["max_PcstaCBF"] == -Inf] <- NA
MxCBF_Tet["max_PcstaCBFmoda"][MxCBF_Tet["max_PcstaCBFmoda"] == -Inf] <- NA

#### CBF relation to pressure stimulus

Ref_period = "Stimulus"
metric = "max_CBFmoda_sta3"
MxCBF_Tet <-(MxCBF_Tet %>% 
               group_by(Trial_ID,
                        Pressure_Level,
                        Genotype) %>% 
               filter(all(!is.na(max_CBF_sta3))) %>% 
               group_modify(~DirectionCBF(.,Ref_period,metric))
)




### Statistical test
### dCBF

ggplot(MxCBF_Tet,aes(x = max_PcstaCBFmoda)) + geom_histogram()

##### Pc_dCBF for Cops mutants for each pressure level(non-paired one tail wilcox)
stat.testPc_dCBFpressLevelPlasmid <- MxCBF_Tet %>%
  group_by(Pressure_Level)  %>%
  filter(Period %in% "Stimulus") %>%
  drop_na () %>%
  t_test(max_PcstaCBFmoda ~ Plasmid, alternative = "greater", paired = F) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testPc_dCBFpressLevelPlasmid
print(stat.testPc_dCBFpressLevelPlasmid, n = 100)

stat.testPc_dCBFpressLevelPlasmid <- stat.testPc_dCBFpressLevelPlasmid %>% 
  add_y_position()

###Plots--------
Max_Pc_dCBFPlotContVsTetx <- (
  ggplot(
    MxCBF_Tet %>% 
      filter(Period %in% c("Stimulus"))
    ,
    aes(x = Plasmid, y = max_PcstaCBFmoda, col = Pressure_Level)
  )  +
    ThemePlot +                                                              
    theme(strip.text.x = element_text(size = 7)) +
    geom_violin(alpha = 0.7, size = 0.3,scale = "count",  width = 0.4) +
    geom_point(alpha = 0.3, size = 2 , shape = 20) + 
    scale_colour_viridis(discrete = TRUE, 
                         option = 'D', 
                         direction = 1,
                         end = 0.5) +
    labs(
      x = "",
      y = expression(paste("max. %",Delta," CBF",sep = "")),
      color = str_wrap("", width = 20)
    ) +
    background_grid(major = "none", minor = "none") +
    geom_hline(yintercept = 0) +
    guides(color = "none") +
    stat_pvalue_manual(
      stat.testPc_dCBFpressLevelPlasmid,
      bracket.nudge.y = 0, 
      tip.length = 0, 
      hide.ns = TRUE, 
      step.increase = 0, 
      label = "p.adj",
      label.size = 2,
      alpha = 0.8
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, 20), 
      limits = c(0,100), 
      expand = expansion(mult = c(0, 0.1))
    ) +
    coord_cartesian(ylim = c(0,100)) +
    facet_grid(~ Pressure_Level) 
)

Max_Pc_dCBFPlotContVsTetx


## Plot CBF time lapse



PlotCBFTetTime <- ggplot(
  TableCBFTetAndCont %>%
    filter(State %in% c("positive") &
             Pressure_Level %in% c("0", "85", "237.5", "556", "988") &
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
  geom_line(data = TableCiliaTetMean %>%
              filter(Pressure_Level %in% c("0", "85", "237.5", "556", "988")), 
            aes(y = meanCBFmodaSTA3 ,col = Pressure_Level),
            size = 1) +
  labs(x = "time after stimulus (s)",
       y = "CBF",
       color =  str_wrap("set pressure (mbar)", width = 15)) +
  facet_grid(vars(Plasmid),vars(Pressure_Level))


PlotCBFTetTime


# generate figure composite panel grid ------------------------------------
 
  layout <- "
  AABB
  "
  
  
  FigSupp9 <- 
  ggdraw(Max_Pc_dCBFPlotContVsTetx) +
  ggdraw(PlotCBFTetTime) + 
    plot_layout(design = layout, heights = c(1, 1, 0.05, 1), widths = c(1,1,0.1,0.1,1,1,1,1) ) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 12, face = "plain"))
  
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_9.pdf", 
    FigSupp9, width = 2800, height = 2400,
    units = "px"
  )
  
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_9.png", 
    FigSupp9, width = 2800, height = 2800,
    units = "px"
  )

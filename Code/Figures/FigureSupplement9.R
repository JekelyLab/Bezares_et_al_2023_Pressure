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
  
# read data Fluo intensity --------------------------------------------------

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
        sd = ~sd(.x ,na.rm = TRUE),
        se = ~sd(.x/sqrt(length(.x)), na.rm = TRUE)
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
  filter(RoundRelTime >=  0 & RoundRelTime <=  60 ) %>% 
  filter(dR_sta31 ==  max(dR_sta31))




####MC

#### Plotting tvsdRcPRCs individual values and average------

PFluoAvR_MCsind <- (
  ggplot(OnlyRatio %>%
           filter(Cell %in% c("MC") & 
                    Type_Experiment %in% c("Titration") & 
                    Stage %in% c("2dpf")),
         aes(x = RoundRelTime,
             y = dR_sta31)
  ) +
    ThemePlot +
    theme(legend.position="none", 
          legend.key.size = unit(0.3, 'cm'),
          legend.key.width= unit(0.3, 'cm'),
          legend.box.spacing = unit(0.03, 'cm'),
          strip.text.x = element_text(size = 10),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 0)
    ) +
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
                filter(Cell %in% c("MC")  & 
                         Type_Experiment %in% c("Titration") & 
                         Stage %in% c("2dpf")), 
              aes(y = dR_sta31_mean , col = Pressure_Level),
              size = 1) +
    scale_x_continuous(breaks = seq(-10,120,20),
                       limits = c(-30,120)
    ) +
    coord_cartesian(xlim = c(-30,90)) +
    labs(x = "time after stimulus (s)",
         y = expression(paste(
           Delta,
           " R/R",
           sep = "")
         ),
         color = str_wrap("pressure (mbar)",
                          width = 20)
    ) +
    geom_vline(
      xintercept = 60, color = "gray",
      linetype = 2
    ) +
    facet_grid(vars(Pressure_Level))
)

PFluoAvR_MCsind

####tvsdR_MC-cPRCs-------

commonExp <- inner_join(OnlyRatio %>%
                          filter(Cell %in% c("MC") & !Type_Experiment %in% c("Gradual") ) %>%
                          distinct(ExperimentID),
                        OnlyRatio %>% 
                          filter(Cell %in% c("cPRC_l1","cPRC_l2","cPRC_r1","cPRC_r2") & !Type_Experiment %in% c("Gradual") ) %>% 
                          distinct(ExperimentID)
                        )

CorrTable <- read.csv("/ebio/ag-jekely/share/Luis/Writing/Pressure_paper/Pressure-sensation-in-zooplankton/Data/CaImagingDataAnalysis/CorrelationAnalysis/AllTogehter/CorrTable/OutCorrMean.csv")

MC_cPRC <- 
  OnlyRatio %>% 
  filter(ExperimentID %in% commonExp$ExperimentID & 
           Cell %in% c("MC","cPRC_l1","cPRC_l2","cPRC_r1" ,"cPRC_r2")) %>%
  ungroup() %>%
  group_by(ExperimentID) %>% 
  mutate(MeanCorr = mean(CorrTable[which(str_detect(CorrTable$File_name,
                                                    unique(as.character(ExperimentID))) == TRUE),
                                   "Mean_corr"],
                         na.rm = T)
  ) 
  



####Corr. plot -----
CorrPlot <- 
  MC_cPRC %>%
  ggplot(aes(x=Pressure_Level,y = MeanCorr, col = Pressure_Level)) + 
  scale_colour_viridis(
    discrete = TRUE, 
    option = "D", 
    direction = 1, 
    end = 0.5
  ) +
  background_grid(major = "none", minor = "none") +
  geom_hline(yintercept = 0) +
  ThemePlot + 
  theme(axis.text.x =  element_text(angle = 0) ) +
  geom_violin(alpha = 0.7, size = 0.3,scale = "count",  width = 0.4) +
  geom_point(alpha = 0.3, size = 2 , shape = 20) +
  #geom_line(aes(group = LarvaID),alpha = 0.3) +
  labs(
    x = "set pressure (mb)",
    y = "Pearson correlation"
  ) + 
  theme(legend.position="none") 
CorrPlot    


##visualizing MC cPRC activity-------

PFluocPRc_MC <- (
  ggplot(MC_cPRC,
         aes(x = RoundRelTime,
             y = dR_sta31,
             col = Cell)
  ) +
    ThemePlot +
    theme(
      legend.key.size = unit(0.3, 'cm'),
      legend.key.width= unit(0.3, 'cm'),
      legend.box.spacing = unit(0.03, 'cm'),
      legend.spacing.x = unit(2, 'mm'),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) +
    background_grid(major = "none", minor = "none") +
    geom_line(aes(col = Cell), 
              alpha = 1,
              size = 0.5) +
    guides(colour = "none") +
    scale_color_manual(values = cbbPalette) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) + 
    scale_x_continuous(breaks = seq(-10,120,20),
                       limits = c(-30,120)
    ) +
    coord_cartesian(xlim = c(-30,90)) +
    labs(x = "time after stimulus (s)",
         y = expression(paste(
           Delta,
           " R/R",
           sep = "")
         )
    ) +
    geom_vline(
      xintercept = 60, color = "gray",
      linetype = 2
    ) +
    facet_wrap(~Pressure_Level + ExperimentID) +
    guides(color = guide_legend(keyheight = 1, nrow = 1,title = element_blank()))
)

PFluocPRc_MC

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



####max.CBFs
MxCBF_Tet <- (
  TableCBFTetAndCont %>% 
    group_by(State,
             Pressure_Level,
             Genotype,
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
GlabelsCB <-  rev(parse(text=unique(as.character(MxCBF_Tet$Plasmid))))

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
      label.size = 3,
      alpha = 0.8
    ) +
    scale_x_discrete(labels= GlabelsCB) +
    scale_y_continuous(
      breaks = seq(0, 100, 20), 
      limits = c(0,100), 
      expand = expansion(mult = c(0, 0.1))
    ) +
    coord_cartesian(ylim = c(0,100)) +
    facet_grid( ~ Pressure_Level) 
)

Max_Pc_dCBFPlotContVsTetx

# generate figure composite panel grid ------------------------------------
  img1 <- readPNG("Manuscript/pictures/CorrProjSnapshotnolabs.png")
  
  Fontsize = 10
  
  PaneldRMC <- ggdraw(PFluoAvR_MCsind) +
    draw_label("set pressure (mb)", angle = -90, x = 1 , y = 0.5, size = Fontsize,color = "black")
  
  x_coord_1 = 0.22
  x_coord_2 = 0.86  
  panel_pearson <- ggdraw() + 
    draw_image(img1) +
    geom_segment(aes(x = x_coord_1,
                     y = 0.93,
                     xend = x_coord_1,
                     yend = 0.85,
                     color = "black"),
                 arrow = arrow(type = 'closed', length = unit(2, "mm")),
                 color = "black") +
    geom_segment(aes(x = x_coord_1,
                     y = 0.85,
                     xend = x_coord_1,
                     yend = 0.93,
                     color = "black"),
                 arrow = arrow(type = 'closed', length = unit(2, "mm")),
                 color = "black") + 
    draw_label("a", x = x_coord_1, y = 0.96, size = Fontsize ,color = "black") +
    draw_label("p", x = x_coord_1, y = 0.83, size = Fontsize,color = "black") + 
    draw_label("1", x = x_coord_2, y = 1, size = Fontsize ,color = "black") +
    draw_label("0.75", x = x_coord_2, y = 0.87, size = Fontsize ,color = "black") +
    draw_label("0.5", x = x_coord_2, y = 0.75, size = Fontsize ,color = "black") +
    draw_label("0.25", x = x_coord_2, y = 0.63, size = Fontsize ,color = "black") +
    draw_label("0", x = x_coord_2, y = 0.5, size = Fontsize ,color = "black") +
    draw_label("-0.25", x = x_coord_2, y = 0.38, size = Fontsize ,color = "black") +
    draw_label("-0.5", x = x_coord_2, y = 0.27, size = Fontsize ,color = "black") +
    draw_label("-0.75", x = x_coord_2, y = 0.14, size = Fontsize ,color = "black") +
    draw_label("-1", x = x_coord_2, y = 0.01, size = Fontsize ,color = "black") +
    draw_label("MC", x = 0.37, y = 0.52, size = Fontsize,color = "black") +
    draw_label("cPRC_l2", x = 0.7, y = 0.5, size = Fontsize,color = "black") +
    draw_label(paste("25 ", "\u00B5", "m", sep = ""), 
               x = 0.68, y = 0.09, size = Fontsize, color = "black")
  
    
  
  layout <- "
  AA#BB
  AA#BB
  #####
  CC#DD
  "
  
  
  FigSupp9 <- 
    PaneldRMC +
    ggdraw(shift_legend(PFluocPRc_MC)) +
    panel_pearson + 
    ggdraw(CorrPlot) + 
    plot_layout(design = layout, heights = c(1, 1, 0.05, 1), widths = c(0.05,1) ) +
    plot_annotation(tag_levels = list(
      c("A", "B" ,"C" , "D"))) &
    theme(plot.tag = element_text(size = 12, face = "plain"))
  
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_9.pdf", 
    FigSupp9, width = 2800, height = 2400,
    units = "px"
  )
  
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_9.png", 
    FigSupp9, width = 2800, height = 2400,
    units = "px"
  )

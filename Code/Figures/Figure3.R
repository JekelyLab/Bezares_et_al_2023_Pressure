########################################
##
## Title: Figure3.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón and Gáspár Jékely
##
## Last Date modified: 2023-02-19
##
## Description: Code to generate Figure 3. It includes results describing the comparisons 
## at behavioral and ultrastructural level between WT and C-ops-1 mutant larvae.
##
## Input files: Metrics tables computed with scripts in dir Code/BatchBehaviour and Code/CBFmeasurement
##
## Output files: Figure 3 PDF and PNG files
##
## Comments:
##
## Publication: Unpublished
##
##########################################

#Initialize----
  rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
  gc() #free up memory and report the memory usage.
  CbbPalette <- c( "#56B4E9", "#009E73",  "#CC79A7", "#E69F00", "#0072B2","#F0E442","#000000", "#D55E00")

# Loading libraries--------------------------------------------------

  library(broom)
  library(catmaid)
  library(cowplot)
  library(ggplot2)
  library(ggpubr)
  library(here)
  library(magick)
  library(nat)
  library(png)
  library(patchwork)
  library(rbokeh)
  library(rstatix)
  library(stringr)
  library(tidyverse)
  library(viridis)
  library(zoo) 
  #Sourcing functions
  source("Code/Figures/Functions_figures.R")
  #Sourcing connection to CATMAID private server
  source("~/R/conn.R")


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

# WTCopsBatch--------------------------------------------------

### read data

TableIndStepWTCops <- read_csv("Data/TablesResults/SmallChamber1WTvs1Copsstep_metrics3dpf.csv")


### Smoothing curves to get maximal values

TableIndStepWTCops <- 
  TableIndStepWTCops %>%
  group_by(Trial_ID) %>% 
  mutate(across(Avg_Speed:Corr_Num_Tracks_Down,
                ~rollmean(rollmean(.x, k = 5, na.pad = T),
                          k = 5, na.pad = T), 
                .names = "STA5_{.col}"))



##Visually assessing track number to decide cut off
cutoffWC = 50

(
  ggplot(TableIndStepWTCops,aes(x = RelTime, y = Num_Tracks_Up + Num_Tracks_Down)) +
    geom_line() +
    geom_hline(yintercept = cutoffWC, color = "red") +
    facet_wrap(~Pressure_Level))


###define levels

TableIndStepWTCops$Pressure_Level <- factor(TableIndStepWTCops$Pressure_Level,levels =  c("3.125", "12.5",  "22.5" ,"32.5", "42.5" ,  "85" , "212.5","988"))
TableIndStepWTCops$Genotype <- factor(TableIndStepWTCops$Genotype,
                                      levels =  c("WT", "Cops8bD"),
                                      labels = c("WT",'"c-ops-1"^"∆8/∆8"')
                                      )
####Max. values

TableMaxStepWTCops <- 
  TableIndStepWTCops %>% 
  filter(Num_Tracks_Up + Num_Tracks_Down >= cutoffWC & 
           RelTime>0 & RelTime <=  StimulusDuration) %>% 
  group_by(Trial_ID,
           Batch_ID,
           Pressure_Level,
           Type_Experiment,
           Genotype) %>% 
  summarise(across(Avg_Speed:STA5_Corr_Num_Tracks_Down,
                   ~max(.x,na.rm = TRUE),.names = "max_{.col}")) # %>% summarise(MaxYdi = max(Avg_Y_displacement,na.rm = TRUE))


AvgMaxStepWTCops <- TableMaxStepWTCops %>% 
  group_by(Pressure_Level,Type_Experiment,Genotype) %>% 
  summarise(across(max_Avg_Speed:max_STA5_Corr_Num_Tracks_Down,
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE),
                        se = ~sd(.x/sqrt(length(.x)), na.rm = TRUE)),
                   na.rm = TRUE))



### Plot Max.displacement vs Pressure Cops-WT --------------------------------------------------

### Statistical test

ggplot(TableMaxStepWTCops,aes(x =max_STA5_Corr_Avg_Y_displacement)) + geom_histogram()

##### Testing differences in change of max. Vert. displacement between genotypes for each pressure level(paired one tail wilcox)

######by genotype
stat.testGenot <- TableMaxStepWTCops %>%
  filter(!Pressure_Level %in% c("988")) %>%
  group_by(Pressure_Level) %>%
  wilcox_test(max_STA5_Corr_Avg_Y_displacement ~ Genotype, alternative = "g", paired = F) %>%
  adjust_pvalue(method = "hochberg") %>%
  add_significance()
stat.testGenot
print(stat.testGenot, n = 100)



###Regression model
##nesting data for mapping
TableMaxStepWTCops <- 
  TableMaxStepWTCops %>% 
  filter(!Pressure_Level %in% c("988")) %>%
  ungroup()  %>%
  nest(values= -Genotype)

##calculating a polynomial regression model on the data grouped by genotype.
TableMaxStepWTCops <- TableMaxStepWTCops %>% 
  mutate(model2 = map(values, ~lm(data = .x,max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressVal + 
                               I(max_STA5_PressVal^2) +
                               I(max_STA5_PressVal^3))),
         model1 = map(values, ~lm(data = .x,max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressVal)),
        
        #  tidied = map(model, tidy),
        # intera = map(values,~lm(data = .x, max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressVal*Genotype)),
        predict1 = map(model1,predict),
        predict2 = map(model2,predict)
          )


###Comparing slopes, etc
#https://statisticsbyjim.com/regression/comparing-regression-lines/
#https://stats.stackexchange.com/questions/33013/what-test-can-i-use-to-compare-slopes-from-two-or-more-regression-models





TableMaxStepWTCops <- TableMaxStepWTCops %>% unnest(c(values,predict1,predict2)) %>%
  filter(!Pressure_Level %in% c("988")) %>%  group_by(Genotype)

M1 <- lm(max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressVal,data=TableMaxStepWTCops %>% group_by(Genotype))

M2 <- lm(max_STA5_Corr_Avg_Y_displacement~max_STA5_PressVal*Genotype,data=TableMaxStepWTCops %>% group_by(Genotype))

M3 <- lm(data = TableMaxStepWTCops %>% group_by(Genotype),
         max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressVal + 
           I(max_STA5_PressVal^2) +
           I(max_STA5_PressVal^3))
M4 <- lm(data = TableMaxStepWTCops %>% group_by(Genotype),
         max_STA5_Corr_Avg_Y_displacement ~ max_STA5_PressVal*Genotype + 
           I(max_STA5_PressVal^2)*Genotype +
           I(max_STA5_PressVal^3)*Genotype)
summary(M2)
summary(M3)
summary(M4)
anova(M1,M2,M3,M4)


#### Plotting max.displ- ----------------------------------------------------------
PlotPressVSMaxDispWTCops <- (
  ggplot(
    TableMaxStepWTCops,
      aes(
        max_STA5_PressVal, 
        max_STA5_Corr_Avg_Y_displacement)
    ) +
       theme_plot +
    theme(legend.position  = "right",
          legend.title = element_blank()) +
      background_grid(major = "none", minor = "none") +
       geom_point(aes(group = Trial_ID, col = Genotype),size = 2, shape = 20, alpha = 0.5) +
      geom_line(aes(group = Batch_ID, 
                    col = Genotype), 
                alpha = 0.3) +
    scale_color_manual(values = c("#000000", "#D55E00"), labels = c(expression(italic("WT")),expression(italic(paste("c-ops-",1^{"∆8/∆8"}))))) +
    geom_line(aes( group = Genotype,
                   y = predict2, 
                   col = Genotype), 
              size = 1.0,
              linetype = "solid") +
      scale_x_continuous(
        breaks = seq(0, 250,50),
        limits = c(-1,250)
      ) +    
      scale_y_continuous(
        breaks = seq(-0.2,1.6,0.2),
        limits = c(-0.2,1.4)
      ) +
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
    coord_cartesian(xlim = c(0,250),ylim = c(0,1.4)) + 
    labs(
        x = "pressure (mbar)",
        y =  str_wrap("max. ∆ vertical displacement (mm/s)",width = 20)
      ) +
      geom_point(
        data = AvgMaxStepWTCops,
        aes(x = max_STA5_PressVal_mean, 
            y = max_STA5_Corr_Avg_Y_displacement_mean, 
            col = Genotype),
        size = 3,
        shape = 1
      ) +
    guides(color = guide_legend(label.hjust = 0,keyheight = 1, nrow = 2))
  )
  
PlotPressVSMaxDispWTCops


#### Saving PNG of plot


ggsave("Manuscript/pictures/Panel_prVSMaxdispInd-AVG_step2dpf.png",
       width = 2200, height = 1600, units = "px", device = "png", bg = "white"
)


# CB ----------------------------------------------------------------------

### read data 2 dpf
TableCiliaNonbinned <- read_csv("Data/TablesResults/CBF-Closure_CiliaryDynamics2dpf_WT-Cops_nonbinned.csv")

###define levels
TableCiliaNonbinned$Pressure_Level <- factor(TableCiliaNonbinned$Pressure_Level, 
                               levels = c("0", "3.125", 
                                          "32.5", "85", "237.5",
                                          "556", "988")
)

TableCiliaNonbinned$Period <- factor(TableCiliaNonbinned$Period, 
                       levels = c("Before", "During_1","During_2", "After"),
                       labels = c("Before", "Stimulus","During_2", "After")
                       
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
  mutate(CBF_sma3 = rollmean(CBF, k = 3, na.pad = T),
         CBF_sta3 = rollmean(rollmean(CBF, k =3, na.pad = T),
                             k = 3, na.pad = T)) %>%
  relocate(CBF_sma3, CBF_sta3, .after = CBF)


####dCBF value

PriorCBFMean <- 
  TableCiliaNonbinned %>%
  ungroup() %>%
  filter(Period %in% "Before") %>%
  group_by(Trial_ID, Larva_ID, Genotype) %>%
  summarise(MeanPrior_staCBF= mean(CBF_sta3, na.rm = TRUE),
            MeanPrior_smaCBF= mean(CBF_sma3, na.rm = TRUE)) %>%
  group_by(Larva_ID) %>%
  mutate(Mean_staCBFlarva = mean(MeanPrior_staCBF,  na.rm = TRUE),
         Mean_smaCBFlarva = mean(MeanPrior_smaCBF,  na.rm = TRUE)) %>%
  arrange(Trial_ID) %>%
  ungroup() %>%
  group_by(Trial_ID) 


TableCiliaNonbinned <- 
  TableCiliaNonbinned %>%
  group_by(Trial_ID) %>% 
  mutate(dstaCBF= CBF - PriorCBFMean$MeanPrior_staCBF[cur_group_id()],
         dsmaCBF= CBF - PriorCBFMean$MeanPrior_smaCBF[cur_group_id()],
         PcstaCBF = (100*
                       (CBF_sta3 - PriorCBFMean$MeanPrior_staCBF[cur_group_id()])
                     /
                       (PriorCBFMean$MeanPrior_staCBF[cur_group_id()])))  %>%
  relocate(dsmaCBF, dstaCBF, PcstaCBF , CBF_sma3, CBF_sta3, .after = CBF)




####max.CBFs
MxCBF <- 
  TableCiliaNonbinned %>% 
    group_by(Pressure_Level,
             Genotype,
             Trial_ID,
             Period,
             Larva_ID) %>% 
    # filter(RelTime > 60 | RelTime <= 30) %>% (in case comparing same size intervals)
    summarise(across(CBF:CBF_sta3, ~max(.x,na.rm = TRUE),.names = "max_{.col}")) %>%
    arrange(Trial_ID)


MxCBF["max_CBF"][MxCBF["max_CBF"] == -Inf] <- NA
MxCBF["max_dsmaCBF"][MxCBF["max_dsmaCBF"] == -Inf] <- NA
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

ggplot(MxCBF,aes(x =max_CBF_sta3)) + geom_histogram()

##### Testing differences between Periods for each pressure level(paired one tail t-test)
stat.testCBFCops <- MxCBF %>%
  filter(Genotype %in% c('"c-ops-1"^"∆8/∆8"') & 
           Period %in% c("Stimulus","Before")) %>%
  group_by(Pressure_Level,Genotype) %>%
  t_test(max_CBF_sta3 ~ Period, alternative = "less", paired = T) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCBFCops
print(stat.testCBFCops, n = 100)

stat.testCBFCops <- stat.testCBFCops %>% 
  add_xy_position(x = "Period")
stat.testCBFCops$p.adj <- round(stat.testCBFCops$p.adj,2)

### Plotting Max.CBF with P-values--------------------------------------------------

MaxPlotCops <- (
  ggplot(
    subset(
      MxCBF,
      Genotype %in% c('"c-ops-1"^"∆8/∆8"') &
        Period %in% c("Before","Stimulus") & 
        Pressure_Level %in% c("3.125","85","237.5","556","988")
    ),
    aes(x = Period, y = max_CBF_sta3)
  )  +
    theme_plot +
    theme(strip.text.x = element_text(size = 10),
          strip.background = element_blank()
    ) +
    geom_violin(scale = "count", width = 0.4) +
    geom_point(aes(group = Trial_ID, col = CBFrel), size = 1) +
    labs(
      x = "",
      y = " max. CBF",
      color = str_wrap("", width = 20)
    ) +
    geom_line(aes(group = Trial_ID, col = CBFrel)) +
    stat_pvalue_manual(
      stat.testCBFCops %>% filter(Genotype %in% c('"c-ops-1"^"∆8/∆8"') &
                                    group1 %in% c("Before") &
                                    group2 %in% c("Stimulus") &
                                    Pressure_Level %in% c("3.125","85","237.5","556","988")),
      bracket.nudge.y = 0, 
      tip.length = 0, 
      step.increase = 0, 
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
    scale_y_continuous(
      breaks = seq(0, 30, 5), 
      limits = c(0, 30), 
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(color = str_wrap("CBF", width = 15)) +
    facet_wrap(~Pressure_Level, nrow = 1) +
    guides(color = guide_legend(keyheight = 0.3)) 
)

MaxPlotCops

#### Saving PNG of plot

ggsave(
  "Manuscript/pictures/PanelPeriodvsmaxCBF_2dpfCops.png",
  width = 2000, height = 1200, units = "px", device = "png", bg = "white"
)



#Volume analysis cPRCs --------------------------------------------------

### read data cPRC volumes
TableVols <- read_csv("Data/TablesResults/cPRCcilia_AcTub_VolcPRC_WT-Cops_Table.csv")

###define levels

TableVols$Genotype <- factor(TableVols$Genotype,
                             levels =  c("WT", "Cops8bD"),
                             labels = c("WT",'"c-ops-1"^"∆8/∆8"')
                             )

TableVols$Body_side <- factor(TableVols$Body_side,
                              levels =  c("left", "right"),
                              labels =  c("left cPRC cilia", "right cPRC cilia")
                              )
### Statistical test
ggplot(TableVols,aes(x =Volume)) + geom_histogram()

stat.testVols <- TableVols %>% 
  filter(stage %in% c("2dpf")) %>%
  na.omit(T) %>%
  group_by(Body_side) %>%
  t_test(Volume ~ Genotype,alternative = "g",paired = F) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testVols 
print(stat.testVols,n=100)
stat.testVols <- stat.testVols %>% add_xy_position(x = 'Body_side' )
stat.testVols$p.adj <- round(stat.testVols$p.adj,3)


### Plotting with P-values--------------------------------------------------
Glabels <-  parse(text=unique(as.character(TableVols$Genotype)))

VolumePlot <- (
  ggplot(TableVols %>%
           filter(stage %in% c("2dpf")),
         aes(x=Body_side,y=Volume,col=Genotype)) +
    theme_plot +
    theme(legend.position = "right",
          axis.text.x = element_text(size = 9, angle = 0 , colour="black"),
          legend.title = element_blank()) +
    background_grid(major = "none", minor = "none") +
    geom_violin() +
    scale_color_manual(values =  c("#000000", "#D55E00"), labels = c(expression(italic("WT")),expression(italic(paste("c-ops-",1^{"∆8/∆8"}))))) +
    geom_point(position=position_jitterdodge(dodge.width = 1)) +
    geom_hline(yintercept = 0, color = "black") +
    stat_pvalue_manual(stat.testVols,
                       bracket.nudge.y = 0, 
                       tip.length = 0,
                       step.increase = 0.05,
                       label = "p.adj") + 
    scale_y_continuous(breaks = seq(-0,800,100),limits = c(0,700) ,expand = expansion()) + 
    labs(x = "",
         y=expression(paste("volume (",~µm^3,")",sep = ""))
    ) +
    guides(color = guide_legend(label.hjust = 0,keyheight = 1, nrow = 2,keywidth = 1,title.position = "top"))
  )

VolumePlot

ggsave("/ebio/ag-jekely/share/Luis/Writing/Pressure_paper/Pressure-sensation-in-zooplankton/Manuscript/pictures/PanelVolumecPRCs.png",
       width = 2000, height = 2000,units = "px",device = "png")


#cPRC EM----

##Loading skeletons

  Cilia_cPRCs_WT <- nlapply(read.neurons.catmaid("^Cilia_cPRC$", pid=21),
                                 function(x) smooth_neuron(x, sigma=200))
  CableLWT <- summary(Cilia_cPRCs_WT)$cable.length
  
  cellWT <- nlapply(read.neurons.catmaid("^cell$", pid=21),
                    function(x) smooth_neuron(x, sigma=200))
  
  Cilia_cPRCs_Copsmut <- nlapply(read.neurons.catmaid("^Cilia_cPRC$", pid=31),
                         function(x) smooth_neuron(x, sigma=200))
  CableLmut <- summary(Cilia_cPRCs_Copsmut)$cable.length
  cellmut <- nlapply(read.neurons.catmaid("^cell$", pid=31),
                    function(x) smooth_neuron(x, sigma=200))
  

  

##3D Plotting----
##WT
plot3d(Cilia_cPRCs_WT, lwd=2, alpha = 1, soma= T, col = colorRampPalette(CbbPalette))
plot3d(cellWT, lwd=2,  alpha =0.1,soma = T,col = "black",add = T )
#plot3d(scale_bar_250nm, lwd = 2, col = 'black')
par3d(zoom=0.75)
nview3d("frontal")
rgl.postscript("Manuscript/pictures/cPRCcilia_WT.eps")
close3d()
##cops1mut
plot3d(Cilia_cPRCs_Copsmut, soma = T, lwd=2, alpha = 1,col = colorRampPalette(CbbPalette) )
plot3d(cellmut, lwd=2,  alpha =0.1,col = "black",add = T )


##3d Plotting cilia by strahler----
#Copsmut
{
  Index1stStrOrdernode <- which(strahler_order(Cilia_cPRCs_Copsmut[[5]])$points == 1)
  plot3d(subset(Cilia_cPRCs_Copsmut[[5]],Index1stStrOrdernode), soma = T, lwd = 7,col = CbbPalette[1])
  Index2ndStrOrdernode <- which(strahler_order(Cilia_cPRCs_Copsmut[[5]])$points == 2)
  plot3d(subset(Cilia_cPRCs_Copsmut[[5]],Index2ndStrOrdernode), soma = T,lwd = 7,col = CbbPalette[2], add = T)
  Index3rdStrOrdernode <- which(strahler_order(Cilia_cPRCs_Copsmut[[5]])$points == 3)
  plot3d(subset(Cilia_cPRCs_Copsmut[[5]],Index3rdStrOrdernode), soma = T,lwd = 7,col = CbbPalette[3], add = T)
  Index4thStrOrdernode <- which(strahler_order(Cilia_cPRCs_Copsmut[[5]])$points == 4)
  plot3d(subset(Cilia_cPRCs_Copsmut[[5]],Index4thStrOrdernode),soma = T, lwd = 7,col = CbbPalette[4], add = T)
  plot3d(Cilia_cPRCs_Copsmut[[5]], lwd = 7,col = "black",soma = T, add = T)
  plot3d(Cilia_cPRCs_Copsmut, lwd = 2,col = colorRampPalette(c("black","grey")), soma = T,add = T, alpha = 0.2)
  plot3d(cellmut, lwd=5,  alpha =0.1,col = "black",add = T)
  par3d(zoom=0.75)
}


rgl.snapshot("Manuscript/pictures/snapshots_EM/Cops8bD/finalset/StrCopsMut3Dplot2430899.png")
close3d()

#WT
{
  Index1stStrOrdernode <- which(strahler_order(Cilia_cPRCs_WT[[10]])$points == 1)
  plot3d(subset(Cilia_cPRCs_WT[[10]],Index1stStrOrdernode), soma = T, lwd = 5,col = CbbPalette[1])
  Index2ndStrOrdernode <- which(strahler_order(Cilia_cPRCs_WT[[10]])$points == 2)
  plot3d(subset(Cilia_cPRCs_WT[[10]],Index2ndStrOrdernode), soma = T,lwd = 5,col = CbbPalette[2], add = T)
  Index3rdStrOrdernode <- which(strahler_order(Cilia_cPRCs_WT[[10]])$points == 3)
  plot3d(subset(Cilia_cPRCs_WT[[10]],Index3rdStrOrdernode), soma = T,lwd = 5,col = CbbPalette[3], add = T)
  Index4thStrOrdernode <- which(strahler_order(Cilia_cPRCs_WT[[10]])$points == 4)
  plot3d(subset(Cilia_cPRCs_WT[[10]],Index4thStrOrdernode),soma = T, lwd = 5,col = CbbPalette[4], add = T)
  plot3d(Cilia_cPRCs_WT[[10]], lwd = 5,col = "black",soma = T, add = T)
  plot3d(Cilia_cPRCs_WT, lwd = 2,col = colorRampPalette(c("black","grey")), soma = T,add = T, alpha = 0.2)
  plot3d(cellWT, lwd=5,  alpha =0.1,soma = T, col = "black",add = T)
  par3d(zoom=0.75)
}

rgl.snapshot("Manuscript/pictures/snapshots_EM/WT/finalset/StrCopsWT3Dplot1291275.png")
close3d()
# 
# 
# #CiliaColouredbyStrahler 2D
# #copsMut

  n=Cilia_cPRCs_Copsmut[[5]]
  so=strahler_order(n)
  orders=1:max(so$points)
  plot(n, col='black',lwd = 6,main = element_blank(),PointAlpha = 1, axes=F,xlab = element_blank(), ylab = element_blank() )
  for (i in orders) {
    plot(subset(n, so$points==i), col=CbbPalette[i], add = i, lwd = 6,PointAlpha = 0.1)
  }
  

# generate figure composite panel grid ------------------------------------

  imgLM <- readPNG("Manuscript/pictures/cPRC_WTvsCopsnolabs.png")
  imgEM <- readPNG("Manuscript/pictures/EMcPRCnolabsFinal.png")

    Fontsize = 10
####plotCBF
  
  CBFpriorduring <- ggdraw(MaxPlotCops) + 
    draw_label("set pressure (mb)", x = 0.45, y = 0.99, size = Fontsize,color ="black", fontface = "plain") +
    draw_label(expression(italic(paste("c-ops-",1^{"∆8/∆8"}))), x = 0.87, y = 0.8, size = Fontsize,color ="black", fontface = "italic") 
###cPRC stain

    x_coord_1= 0.05
    x_coord_2 = 0.55  
     panel_cPRC_staining <- ggdraw() +
       draw_image(imgLM) +
       draw_label(expression(italic("WT")),
                  x = x_coord_1,
                  y = 0.9,
                  size = Fontsize,color = "white") +
       draw_label(expression(italic(paste("c-ops-",1^{"∆8/∆8"}))),
                  x = 0.62,
                  y = 0.9,
                  size = Fontsize,
                  color = "white") +
       draw_label("AcTub", x = x_coord_1,
                  y = 0.1,
                  size = Fontsize,
                  color = "white",
                  hjust = 0.2) +
       draw_label("α-cPRCcilia", x = x_coord_1,
                  y = 0.16,
                  size = Fontsize,
                  color = "cyan",
                  hjust = 0.2) +
       draw_label("cPRC cilia",
                  x = 0.18,
                  y = 0.55,
                  size = Fontsize,
                  color = "white",
                  hjust = 0.2) +
       draw_label("cPRC cilia",
                  x = 0.6,
                  y = 0.2,
                  size = Fontsize,
                  color = "white",
                  hjust = 0.2) +
       draw_label(paste("2", "\u00B5", "m", sep = ""), 
                  x = 0.93,
                  y = 0.15,
                  size = Fontsize,
                  color = "white")


###3d EMsnapshots
  Rect1 <- rectGrob(
    x = -0.032,
    y = 0.048,
    width = unit(7.08, "mm"),
    height = unit(56.1, "mm"),
    hjust = 0, vjust = 0,
    gp = gpar(fill = NULL, alpha = 1 ,lwd = 1.5)
  )
  Rect2 <- rectGrob(
    x = -0.032,
    y = 0.504,
    width = unit(7.08, "mm"),
    height = unit(56.1, "mm"),
    hjust = 0, vjust = 0,
    gp = gpar(fill = NULL, alpha = 1 ,lwd = 1.5)
  )
   ##EM composite
  {
    x_bar1 = 0.22
    x_bar2 = 0.46
    x_bar3 = 0.71 
    y_bar1 = 0.55
    y_bar2 = 0.09
    panel_cPRC_EM <- ggdraw() + 
    draw_image(imgEM, scale = 1) +
    draw_label(expression(italic("WT")), angle = 90, x = -0.02 , y = 0.74, size = Fontsize,color = "black") +
    draw_label(expression(italic(paste("c-ops-",1^{"∆8/∆8"}))),angle = 90, x = -0.02, y = 0.25, size = Fontsize,color = "black") +
    draw_label("cb", x = 0.2, y = 0.9, size = Fontsize,color = "black") +
    draw_label("cb", x = 0.2, y = 0.46, size = Fontsize,color = "black") +
    draw_label("n", x = 0.12, y = 0.92, size = Fontsize,color = "black") +
    draw_label("bb", x = 0.04, y = 0.65, size = Fontsize,color = "black") +
    draw_label("bb", x = 0.07, y = 0.22, size = Fontsize,color = "black") +
    draw_label("*", x = 0.14, y = 0.26, size = Fontsize,color = "black",fontface = "bold") +
    draw_label(paste("2", "\u00B5", "m", sep = ""), 
                 x = x_bar1, y = y_bar1, size = Fontsize, color = "black") +
    draw_label(paste("2", "\u00B5", "m", sep = ""), 
                 x = 0.22, y = y_bar2, size = Fontsize, color = "black") +
    draw_label(paste("2", "\u00B5", "m", sep = ""), 
               x = x_bar2, y = y_bar1, size = Fontsize, color = "black") +
    draw_label(paste("2", "\u00B5", "m", sep = ""), 
               x = x_bar2, y = y_bar2, size = Fontsize, color = "black") +
    draw_label("500 nm", 
             x = x_bar3, y = y_bar1, size = Fontsize, color = "black") +
    draw_label("500 nm", 
               x = 0.7, y = y_bar2, size = Fontsize, color = "black") +
      draw_label("str. order", x = 0.8, y = 0.23, size = Fontsize,color = "black") +
      draw_label("1", x = 0.78, y = 0.19, size = Fontsize,color = CbbPalette[1]) +
      draw_label("2", x = 0.78, y = 0.15, size = Fontsize,color = CbbPalette[2]) +
      draw_label("3", x = 0.78, y = 0.11, size = Fontsize,color = CbbPalette[3]) +
      draw_label("4", x = 0.78, y = 0.07, size = Fontsize,color = CbbPalette[4]) +
      draw_label("bb", x = 0.79, y = 0.88, size = Fontsize,color = "black") +
      draw_label("cb", x = 0.94, y = 0.9, size = Fontsize,color = "black") +
      draw_label("bb", x = 0.81, y = 0.32, size = Fontsize,color = "black") +
      draw_label("cb", x = 0.9, y = 0.42, size = Fontsize,color = "black") +
      draw_label(paste("2", "\u00B5", "m", sep = ""), 
                 x = 0.96, y = y_bar1, size = Fontsize, color = "black") +
      draw_label(paste("2", "\u00B5", "m", sep = ""), 
                 x = 0.97, y = y_bar2, size = Fontsize, color = "black") +
      draw_grob(Rect1) +
      draw_grob(Rect2)
    
     }

###full composite
   {
   layout <- "
    #AABB
    #CCDD
    #EEEE
    #EEEE
    "
    
    Fig3 <- 
    ggdraw(PlotPressVSMaxDispWTCops)  + 
    CBFpriorduring +
    panel_cPRC_staining + 
    ggdraw(VolumePlot) + 
    panel_cPRC_EM +
    # cPRCciliaStr +
    # LengthBase + 
    plot_layout(design = layout, heights = c(1,1,1,1,1,1,1), widths = c(0.05,1)) +
    plot_annotation(tag_levels = list(
      c("A", "B", "C", "D", "E"))) &
    theme(plot.tag = element_text(size = 12, face = "plain"))
    
      
   
    ggsave(
      filename = "Manuscript/Figures/Figure3.pdf", 
      Fig3, width = 2800, height = 3000,
      units = "px"
    )
    ggsave(
      filename = "Manuscript/Figures/Figure3.png", 
      Fig3, width = 2800, height = 3000,
      units = "px"
    )
  }
  
 
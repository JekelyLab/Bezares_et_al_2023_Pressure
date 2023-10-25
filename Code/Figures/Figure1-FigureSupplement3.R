########################################
##
## Title:FigureSupplement3.R
##
##########################################
##
## Author: Luis Alberto Bezares Calderón
##
## Last Date modified: 2023-02-25
##
## Description: Linear increase pressure, distribution larvae acclimation experiments prior to trials.
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
  cbbPalette <- c("#000000", "#009E73", "#0072B2","#56B4E9",  "#F0E442", "#E69F00", "#CC79A7")


#Loading libraries--------------------------------------------------

  library(cowplot)
  library(ggplot2)
  library(ggpmisc)
  library(ggpubr)
  library(here)
  library(magick)
  library(png)
  library(patchwork)
  library(rstatix)
  library(stringi)
  library(stringr)
  library(tidyverse)
  library(viridis)
  #Sourcing functions
  source("Code/Figures/Functions_figures.R")
  



#switch to project directory
project_dir <- here()
setwd(project_dir)

#define ggplot theme--------------------------------------------------
theme_plot <- theme(
  axis.text.x = element_text(size = 7, angle = 90),
  axis.text.y = element_text(size = 7),
  legend.text = element_text(size = 7),
  legend.title = element_text(size = 9),
  legend.spacing.x = unit(0.2,'mm'),
  legend.title.align = 0.5,
  legend.box.spacing = margin(0, 0, 0, 0),
  axis.title = element_text(size = 10),
  legend.position = "right",
  panel.background = element_blank()
)


#Linear increase -------------


  ### read data 
  
TableLinearIndDisp <- read_csv("Data/SourceData_elife/Figure1-SourceData7.csv")


##Visually assessing track number to decide cut off
cutoffLin = 100

(
  ggplot(TableLinearIndDisp,aes(x = RelTime, y = Num_Tracks_Up + Num_Tracks_Down)) +
    geom_line() +
    geom_hline(yintercept = cutoffLin, color = "red") +
    facet_wrap(~Stimulus_Level))



###define pressure levels

TableLinearIndDisp$Stimulus_Level <- factor(TableLinearIndDisp$Stimulus_Level,levels =  c( "0.05","0.07","0.1","0.15" ,"0.2","0.3","0.4", "0.45","0.5","0.6","0.65", "0.9","1" ,"1.2","1.3","1.4","1.5", "1.6","1.8","2", "2.1" ))
TableLinearIndDisp$Category_stimulus <- factor(TableLinearIndDisp$Category_stimulus,levels =  c( "0.05-0.2","0.3-0.7", "0.9-1.3","1.4-1.8","2-2.6" ))

#TableLinearIndDisp$Category_stimulus <- factor(TableLinearIndDisp$Category_stimulus,levels =  c( "0.05-0.07","0.1-0.2","0.3-0.45","0.5-0.65", "0.9-1" ,"1.2-1.3","1.4-1.5","1.6-1.8","2-2.1" ))

###Calculating aggregate tables
####AVG. values-
TableAvgLinear3dpf <- TableLinearIndDisp %>% 
  filter(Num_Tracks_Up + Num_Tracks_Down >= cutoffLin) %>%
  group_by(Type_Experiment,Genotype,RelTime,Category_stimulus) %>% 
  summarise(across(PressVal:Corr_Num_Tracks_Down,list(mean = ~mean(.x, na.rm = TRUE),
                                                       sd = ~sd(.x, na.rm = TRUE),
                                                       se = ~sd(.x/sqrt(length(.x)),na.rm = TRUE))))
TableAvgLinear3dpf$n_trials <- count(TableLinearIndDisp %>% 
                                       filter(Num_Tracks_Up + Num_Tracks_Down >= cutoffLin) %>%
                                       group_by(Category_stimulus,Type_Experiment,Genotype,RelTime))$n

distinct(TableAvgLinear3dpf,n_trials)




###Calculating mean rate per average PvsDisp

##nesting data for mapping
#Category stimulus
TableLinearIndDispsubSt  <- 
  TableLinearIndDisp %>%
  filter(RelTime > 0 & RelTime <= 30) %>%
ungroup()  %>%
  nest(values= -Category_stimulus)
##trial
TableLinearIndDispsubTr  <- 
  TableLinearIndDisp %>%
  filter(RelTime > 0 & RelTime <= 30) %>%
  ungroup()  %>%
  nest(values= -Trial_ID)


##calculating a polynomial regression model on the data grouped by genotype.
TableLinearIndDispsubSt <- TableLinearIndDispsubSt %>% 
  mutate(model1 = map(values, ~lm(data = .x,PressVal ~ RelTime)),
         tidied1 = map(model1, tidy),
         # intera = map(values,~lm(data = .x, STA5_Corr_Avg_Y_displacement ~ STA5_PressVal*Genotype)),
         predict1 = map(model1,predict)
  )

TableLinearIndDispsubSt %>% unnest(tidied1)  %>%  print(n = 1000)

#trialRatecalc
TableLinearIndDispsubTr <- TableLinearIndDispsubTr %>% 
  mutate(model1 = map(values, ~lm(data = .x,PressVal ~ RelTime)),
         tidied1 = map(model1, tidy),
         # intera = map(values,~lm(data = .x, STA5_Corr_Avg_Y_displacement ~ STA5_PressVal*Genotype)),
         predict1 = map(model1,predict)
  )

TableLinearIndDispsubTr %>% unnest(tidied1)  %>%  print(n = 1000)


##Plots---------

####Pressure logs



#####average-

PlotAveragePressureLinear3dpf <- (
  ggplot(TableAvgLinear3dpf,
         aes(RelTime,Corr_PressVal_mean,col = Category_stimulus)) +
    theme_plot +
    theme(legend.position = "none",
          #axis.title.x = element_blank()) +
          axis.text.x = element_text(size = 7, angle = 0)) +
    background_grid(major = 'none', minor = 'none') +
    geom_line(alpha = 0.3) +
    geom_line(data = TableLinearIndDispsubSt %>%
                unnest(c(values, predict1)),aes(x= RelTime,y=predict1, col = Category_stimulus)) + 
    geom_hline(yintercept = 0) +
    scale_colour_viridis(discrete = TRUE, 
                         option = 'D', 
                         direction = 1,
                         end = 0.5) +
    scale_x_continuous(breaks = seq(0,100,10),limits = c(-30,250)) +
    scale_y_continuous(breaks = seq(0,1000,20),limits = c(-300,4000)) +
    coord_cartesian(xlim = c(1,29), ylim = c(0,70)) +
    geom_vline(xintercept = 0,color = "black") +
    geom_errorbar(alpha = 0.4, aes(ymin = PressVal_mean, ymax = PressVal_mean+PressVal_se)) +
    geom_vline(xintercept = 60,color = "gray",linetype = 2) +
    labs(x = "time after stimulus (s)",
         y = "pressure (mb)",
         color = str_wrap("pressure level (mb)",
                          width = 20)))

PlotAveragePressureLinear3dpf

##### Saving PNG of plot -


ggsave("Manuscript/pictures/PanelAvgPressure_linear3dpf.png",
       plot = PlotAveragePressureLinear3dpf,
       width = 2200,height = 2000,units = "px",device = "png")


### Plot displacement Linear 3 dpf 

#####Average
PlotDispAvgLinear3dpf  <- (ggplot(
  TableAvgLinear3dpf,
  aes(RelTime, Corr_Avg_Y_displacement_mean, col = Category_stimulus)) +
    theme_plot +
    theme(legend.position = c(0.4, 0.55),
          legend.background = element_blank()) +
    geom_line() +
    scale_colour_viridis(
      discrete = TRUE,
      option = "D",
      direction = 1,
      end = 0.5
    ) +
     geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(0,100,10),limits = c(-10,250)) +
    scale_y_continuous(breaks = seq(-0.2,2,0.4),limits = c(-1,2)) +
    coord_cartesian(xlim = c(0,30), ylim = c(0,1.1)) + 
    geom_vline(xintercept = 0, color = "black") +
    geom_errorbar(aes(ymin = Corr_Avg_Y_displacement_mean, ymax = Corr_Avg_Y_displacement_mean + Corr_Avg_Y_displacement_se)) + # error bar is only shown on top
    labs(
      x = "time after stimulus (s)",
      y = str_wrap("∆ vertical displacement (mm/s)",width = 20),
      colour = expression(paste("pressure rate (",mb ~s^-1,")",
                                sep = "")
                          )
      ) +
    guides(color = guide_legend(keyheight = 0.3))
  )
PlotDispAvgLinear3dpf 

#####Saving PNG of plot -


ggsave("Manuscript/pictures/PanelTvsAVGdDisp_Linear3dpf.png",
       width = 1000, height = 800, units = "px", device = "png"
)


###AvgPressvsIndDisp.

####Regression model-
#####nesting data for mapping
TableLinearIndDispM <- 
  TableLinearIndDisp %>%
  filter(RelTime > 0 & RelTime <=  30) %>%
  ungroup()  %>%
  nest(values= -Category_stimulus)

##calculating a polynomial regression model on the data grouped by genotype.
TableLinearIndDispM <- TableLinearIndDispM %>% 
  mutate(model1 = map(values, ~lm(data = .x,Corr_Avg_Y_displacement ~ Corr_PressVal)),
         model2 = map(values, ~lm(data = .x,Corr_Avg_Y_displacement ~ Corr_PressVal + I(Corr_PressVal^2))),
         model3 = map(values, ~lm(data = .x,Corr_Avg_Y_displacement ~ Corr_PressVal + 
                                    I(Corr_PressVal^2) + I(Corr_PressVal^3))),
         
         # intera = map(values,~lm(data = .x, STA5_Corr_Avg_Y_displacement ~ STA5_PressVal*Genotype)),
         predict1 = map(model1,predict),
         predict2 = map(model2,predict),
         predict3 = map(model3,predict),
         aov_1 = map2(model1, model2, anova), # Comparing simple vs polynomial for each rate.
         aov_2 = map2(model2, model3, anova), # Comparing simple vs polynomial for each rate.
         #tidied_aov1 = map(aov_1, tidy),
         #tidied_aov2 = map(aov_2, tidy)
  )

#https://statisticsbyjim.com/regression/interpret-coefficients-p-values-regression/
#https://statisticsbyjim.com/regression/check-residual-plots-regression-analysis/
summary(TableLinearIndDispM$model1[[1]])
summary(TableLinearIndDispM$model2[[5]])

TableLinearIndDispM %>% unnest(aov_1)


###Plot

PlotAVGPressVSIndDispLinear3dpf <- (
  ggplot(
    TableLinearIndDispM %>%  
      unnest(c(values,predict1,predict2)),
    aes(PressVal, Corr_Avg_Y_displacement, col = Category_stimulus)
  ) +
    theme_plot +
    theme(strip.background = element_blank()) +
    guides(colour = "none") +
    geom_point(size = 2, alpha = 0.3) +
    geom_line(aes(group = Trial_ID), alpha = 0.3) +
    scale_colour_viridis(
      discrete = TRUE, 
      option = 'D',
      direction = 1,
      end = 0.5
    ) +
    geom_line(aes(y = predict1, 
                   col = Category_stimulus), 
              linewidth = 1.0,
              linetype = "dashed") +
    geom_line(aes(y = predict2, 
                   col = Category_stimulus), 
              linewidth = 1.0,
              linetype = "solid") +
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
    labs(
      x = "pressure (mb)",
      y = str_wrap("∆ vertical displacement (mm/s)",width = 20),
      color = expression(paste("pressure rate (",mb ~s^-1,")",sep = ""))
    ) +
     facet_wrap(~Category_stimulus, nrow = 1,  scales = "free") #+
    
)

PlotAVGPressVSIndDispLinear3dpf
##### Saving PNG of plot -


ggsave(
  "Manuscript/pictures/Panel_prVScorrInddisp_Linear3dpf.png",
  width = 1200, height = 800, units = "px", device = "png", bg = "white"
)



##3dpf long--------------------------------------------------

### read data 

TableIndLongDisp <- read_csv("Data/SourceData_elife/Figure1-SourceData3.csv")

##Visually assessing track number to decide cut off
cutoffLong = 100

(
  ggplot(TableIndLongDisp,aes(x = RelTime, y = Num_Tracks_Up + Num_Tracks_Down)) +
    geom_line() +
    geom_hline(yintercept = cutoffLong, color = "red") +
    facet_wrap(~Pressure_Level))

###define pressure levels

#Full levels (not implemented)
#("3.125","7.5", "12.5","17.5",  "22.5" , "27.5","32.5" ,"37.5", "42.5" ,"47.5","55", "65","75"  ,  "85" ,"95"  , "137.5" ,"162.5","187.5","212.5","237.5" )
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
             "85", "137.5","142.5", "187.5", "237.5", "488")
)

TableIndLongDisp$Basal_pressure <- 
  factor(TableIndLongDisp$Basal_pressure,
         levels = c("0","100", "500"))

TableIndLongDisp$BinNo <- factor(TableIndLongDisp$BinNo,
                                 levels = c("5","4", "3","2","1"))

TableIndLongDisp$Fraction_increase <- factor(TableIndLongDisp$Fraction_increase, levels = c(as.character(sort(unique(TableIndLongDisp$Fraction_increase)))))
TableIndLongDisp$Fraction_increase <- stri_replace_all_regex(TableIndLongDisp$Fraction_increase,
                                                             pattern = levels(TableIndLongDisp$Fraction_increase),
                                                             replacement = as.character(round(as.numeric(levels(TableIndLongDisp$Fraction_increase)),2)),
                                                             vectorize=FALSE)
TableIndLongDisp$Fraction_increase <- as_factor(TableIndLongDisp$Fraction_increase)
TableIndLongDisp$Fraction_increase <- factor(TableIndLongDisp$Fraction_increase,levels = as.character(sort(as.numeric(levels(TableIndLongDisp$Fraction_increase)))))
levels(TableIndLongDisp$Fraction_increase) <- sub("temp", "0.",
                                                  sub("^0","release",
                                                      sub("^[0]\\.", "temp",
                                                          levels(TableIndLongDisp$Fraction_increase)) 
                                                      )
                                                  )

levels(TableIndLongDisp$Fraction_increase) <- sub("0.27","release",levels(TableIndLongDisp$Fraction_increase) )

#Smoothing curves to get maximal values

TableIndLongDisp <- 
  TableIndLongDisp %>%
  group_by(Trial_ID) %>% 
  mutate(across(Avg_Speed:PressInc,
                ~rollmean(rollmean(.x, k = 5, na.pad = T),
                          k = 5, na.pad = T), 
                .names = "STA5_{.col}"))


#Calculating aggregate tables
####AVG. values-
TableAvgLongStep3dpf <- (
  TableIndLongDisp %>%
    filter(Num_Tracks_Up + Num_Tracks_Down >= cutoffLong) %>%
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
             list(mean = ~mean(.x, na.rm = T),
                  sd = ~sd(.x, na.rm = T),
                  se = ~sd(.x/sqrt(length(.x)), na.rm = T))))
)

TableAvgLongStep3dpf$n_trials <- 
  count(TableIndLongDisp %>%
          filter(Num_Tracks_Up + Num_Tracks_Down >= cutoffLong) %>%
          group_by(
            Pressure_Level,
            Type_Experiment,
            Genotype,
            RelTime,
            Category_pressure,
            Fraction_increase,
            Basal_pressure))$n

##MIN. values-

TableLongminStep3dpf <- (
  TableIndLongDisp %>% 
    filter(
      RelTime>0 & 
        Num_Tracks_Up + Num_Tracks_Down >= cutoffLong) %>%
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
             ~min(.x, na.rm = TRUE),.names = "min_{.col}")
    )
)
TableLongminStep3dpf["min_PressVal"][TableLongminStep3dpf["min_PressVal"] == -Inf] <- NA
TableLongminStep3dpf["min_STA5_PressVal"][TableLongminStep3dpf["min_STA5_PressVal"] == -Inf] <- NA
TableLongminStep3dpf["min_PressInc"][TableLongminStep3dpf["min_PressInc"] == -Inf] <- NA
TableLongminStep3dpf["min_STA5_PressInc"][TableLongminStep3dpf["min_STA5_PressInc"] == -Inf] <- NA


AvgLongminStep3dpfWT <- (
  TableLongminStep3dpf %>% 
    group_by(
      Pressure_Level,
      Type_Experiment,
      Genotype,
      Category_pressure,
      Basal_pressure,
      Fraction_increase) %>% 
    summarise(
      across(
        min_Avg_Speed:min_STA5_PressInc,
        list(mean = ~mean(.x, na.rm = TRUE),
             sd = ~sd(.x,na.rm = TRUE),
             se = ~sd(.x/sqrt(length(.x)),na.rm = TRUE)))
    )
)




###Average particle count prior to stimulus


DispPcAvgBinBasal <- TableIndLongDisp %>% 
  filter(Num_Tracks_Up + Num_Tracks_Down >= cutoffLong & 
           RelTime <= 0) %>% 
  group_by(Basal_pressure,BinNo,Batch_ID,Genotype) %>%
  summarise(meanDistPrior = mean(LarvaD_Pc,na.rm = T))






###Plots--------

####Pressure logs

PlotAveragePressureLong3dpf <- 
  (ggplot(TableAvgLongStep3dpf %>% 
            filter(!Fraction_increase %in% c("release")),
          aes(RelTime,PressVal_mean,col = Fraction_increase)) +
     theme_plot +
     theme(legend.position = "bottom",
           legend.key.size = unit(0.2, 'cm'),
           legend.spacing.x = unit(2, 'mm'),
           legend.text = element_text(size=7),
           legend.title = element_text(size=8)
           ) +
     background_grid(major = 'none', minor = 'none') +
     geom_line() +
     geom_hline(yintercept = 0) +
     scale_colour_viridis(discrete = TRUE, 
                          option = 'D', 
                          direction = 1,
                          end = 0.5) +
     scale_x_continuous(breaks = seq(-10,80,40),
                        limits = c(-30,250)) +
     scale_y_continuous(breaks = seq(0,1000,200),
                        limits = c(-1000,2000)) +
     coord_cartesian(xlim = c(-10,100), ylim = c(0,1000)) + 
     geom_vline(xintercept = 0,color = "black") +
     geom_errorbar(aes(ymin = PressVal_mean, 
                       ymax = PressVal_mean+PressVal_se)) +
     labs(x = "time after stimulus (s)",
          y = "pressure (mb)",
          color = str_wrap("set % pressure increment", width = 15)) +
     geom_vline(xintercept = 60,color = "gray",linetype = 2) +
     facet_wrap(~Basal_pressure) +
     theme(strip.text.x = element_text(size = 10),
           strip.background = element_blank()
     ) +
     guides(color = guide_legend(keyheight = 0.3,nrow = 3))
   )

PlotAveragePressureLong3dpf

##### Saving PNG of plot


ggsave("Manuscript/pictures/PanelAvgPressure_stepLong3dpf.png",
       plot = PlotAveragePressureLong3dpf,
       width = 2200,height = 2000,units = "px",device = "png")

###Particle distribution

ggplot(DispPcAvgBinBasal, aes(x = meanDistPrior,fill = Basal_pressure)) + geom_histogram(binwidth = 1) + facet_wrap(~Basal_pressure)
####Stat.test

G0 <- filter(DispPcAvgBinBasal,Basal_pressure %in% c("0"))$meanDistPrior
G100 <- filter(DispPcAvgBinBasal,Basal_pressure %in% c("100"))$meanDistPrior
G500 <- filter(DispPcAvgBinBasal,Basal_pressure %in% c("500"))$meanDistPrior

TableStatsKS <- bind_rows(bind_rows(tidy(ks.test(G0,G100,alternative = "two.sided")),
tidy(ks.test(G0,G500,alternative = "two.sided"))),tidy(ks.test(G100,G500,alternative = "two.sided"))) %>%
  select(statistic,p.value)

TableStatsKS <- bind_cols(as_tibble(c("0–100","0–500","100–500")),TableStatsKS)
colnames(TableStatsKS) <- c("comparison","D","p-value")
              
TableStatsKS$D <- round(TableStatsKS$D,2)
TableStatsKS$`p-value` <- round(TableStatsKS$`p-value`,2)

###PlotPartDist----

PlotLongPartDispPressureBox3dpf <- (
  ggplot(DispPcAvgBinBasal,aes(BinNo,meanDistPrior,col = Basal_pressure)) +
     theme_plot +
     theme(legend.position = "right",
           legend.key.size = unit(0.5, 'cm'),
           legend.text = element_text(size=7),
           legend.title = element_text(size=7)) +
    background_grid(major = 'none', minor = 'none') +
    geom_violin( alpha = 0.7, size = 0.3,scale = "count", 
                width =1 ,
                position = position_dodge(width = 1)) +
    geom_point(alpha = 0.5, size = 2 , shape = 20,
               position = position_jitterdodge(seed = 1,
                                               dodge.width = 1)) +
     scale_color_manual(values = cbbPalette) +
    scale_y_continuous(breaks = seq(0,100,20),limits = c(0,45)) +
     geom_hline(yintercept = 0,color = "black") +
    labs(x = "Bin",
          y = "avg. % larvae",
          color = str_wrap("acclimation pressure (mb)",width = 20)) +
    coord_flip()+
    facet_grid()
  )

PlotLongPartDispPressureBox3dpf


Tab <- annotate(geom = "table_npc",
                npcx = "right",
                npcy = "bottom",
                label = list(TableStatsKS),
                size = 1.6)

PlotLongTabPartDispPressureBox3dpf <- ggdraw(PlotLongPartDispPressureBox3dpf) + Tab 


##### Saving PNG of plot


ggsave("Manuscript/pictures/PanelAvgPPartDispstepLong3dpf.png",
       plot = PlotLongPartDispPressureBox3dpf,
       width = 2200,height = 2000,units = "px",device = "png")



###Plotting release of pressure  displacement response---------
scale=1000
PlotDisp3Long0 <- 
  (ggplot(TableAvgLongStep3dpf %>%
            filter(Fraction_increase %in% c("release")),
          aes(RelTime, Corr_Avg_Y_displacement_mean )
          ) +
     theme_plot +
    theme(legend.position = "none") +
     geom_line(aes(col = Basal_pressure)) +
     geom_line(aes(y = PressVal_mean/scale ,col = Basal_pressure)) +
     scale_color_manual(values = c( "#009E73", "#0072B2")) +
     geom_hline(yintercept = 0) +
     scale_x_continuous(breaks = seq(-10,100,20),limits = c(-30,230)) +
     scale_y_continuous(sec.axis = sec_axis(~.*scale, name="Pressure (mb)"), limits = c(-0.2,0.8)) +
     coord_cartesian(xlim = c(-10,100)) +
     geom_vline(xintercept = 0,color = "black") +
     geom_errorbar(aes(x = RelTime, ymin = Corr_Avg_Y_displacement_mean, ymax = Corr_Avg_Y_displacement_mean + Corr_Avg_Y_displacement_se, col = Basal_pressure)) +
     geom_errorbar(aes(x = PressVal_mean/scale, ymin = PressVal_mean/scale, ymax = PressVal_mean/scale + PressVal_se/scale, col = Basal_pressure)) +
     labs(x = "time after stimulus (s)",y = str_wrap(" ∆ vertical displacement (mm/s)",width = 20), color = str_wrap("acclimation pressure (mb)",width = 20)) +
     geom_vline(xintercept = 60,color = "gray",linetype = 2)
   )
PlotDisp3Long0

##### Saving PNG of plot


ggsave("Manuscript/pictures/PanelTvsAvgDisp_Long3dpf0wPress.png",
       plot = PlotAveragePressureLong3dpf,
       width = 2200,height = 2000,units = "px",device = "png")


###Plotting min. displacement---------

BasalVSMinDisp <- TableLongminStep3dpf %>%  
   ggplot(
    aes(
      Basal_pressure,
      min_STA5_Corr_Avg_Y_displacement,
      col = Fraction_increase),
  ) + 
  theme_plot +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0),
        legend.spacing.x = unit(2, 'mm')) +
  scale_colour_viridis(discrete = TRUE, 
                       option = 'D', 
                       direction = 1,
                       end = 0.5) +
  geom_violin( alpha = 0.7, size = 0.3,scale = "count",
               width =1 ,
               position = position_dodge(width = 1)) +
  geom_point(alpha = 0.5, size = 2 , shape = 20,
             position = position_jitterdodge(seed = 1,
                                             dodge.width = 1)) +
  labs(x = "acclimation pressure (mb)",
       y = str_wrap("min. ∆ vertical displacement (mm/s)",width = 20),
       color = str_wrap("set % pressure increment", width = 10)) +
  geom_hline(yintercept = 0,color = "black") +
  geom_vline(xintercept = 0,color = "black") +
  guides(color = guide_legend(keyheight = 0.3, keywidth = 0.5, nrow = 5))
BasalVSMinDisp


# generate figure composite panel grid ------------------------------------

  imgchamber <- readPNG("Manuscript/pictures/LarvaDistBinsSchematic.png")
  Fontsize <- 10
  panel_bins <- ggdraw() + draw_image(imgchamber, scale = 0.95)                                            

  layout <- "
  ABBCCC
  ###CCC
  DDDCCC
  ######
  EFFGHH
  "
  combinedLinear <- ggdraw(PlotAveragePressureLinear3dpf + PlotDispAvgLinear3dpf) + 
 draw_label("time after stimulus (s)", x = 0.5, y = 0.1, fontfamily = "sans", 
                                               fontface = "plain", size = Fontsize)

  PlotRatePress <- ggdraw(PlotAVGPressVSIndDispLinear3dpf) +
    draw_label(expression(paste("pressure rate (",mb ~s^-1,")",
                                 sep = "")), x = 0.55, y = 1, fontfamily = "sans", 
               fontface = "plain", size = 11)
  
  PanelPressLong <- ggdraw(PlotAveragePressureLong3dpf) +
    draw_label("acclimation pressure (mb)", x = 0.5, y = 1, size = Fontsize,color ="black", fontface = "plain") 
  
  
  Fig1Supp3 <- ggdraw(PlotAveragePressureLinear3dpf) +
    ggdraw(PlotDispAvgLinear3dpf) +
    PlotRatePress +
    PanelPressLong +
    panel_bins +
    PlotLongTabPartDispPressureBox3dpf +
    ggdraw(PlotDisp3Long0) +
    ggdraw(BasalVSMinDisp) +
    plot_layout(design = layout, heights = c(1,0.05,1,0.05)) +
    plot_annotation(tag_levels = list(
    c("A", "B", "C", 
      "D", "E","F",
      "G", "H"))) &
  theme(plot.tag = element_text(size = 12, face = "plain"))

ggsave(
  filename = "Manuscript/Figures/Figure1-FigureSupplement3.pdf", 
  Fig1Supp3, width = 3500, height = 3000,
  units = "px"
)
ggsave(
  filename = "Manuscript/Figures/Figure1-FigureSupplement3.png", 
  Fig1Supp3, width = 3500, height = 3000,
  units = "px"
)



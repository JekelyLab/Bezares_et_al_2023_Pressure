###Code to generate Figure Supplement 8 of pressure sensation paper

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
  
  #scale_bar_250nm = read.neurons.catmaid("^scale_bar_250nm$", pid=31)
  



##Measuring branch lengths per Strahler number

  AllCilia <- c(Cilia_cPRCs_WT,Cilia_cPRCs_Copsmut)
  
  GtypeNames <- c(rep( "WT",length(Cilia_cPRCs_WT[,"skid"])),rep( '"c-ops-1"^"∆8/∆8"',length(Cilia_cPRCs_Copsmut[,"skid"])))
  
  StoreMetrics <- tibble(skids= AllCilia[,"skid"], sknames = AllCilia[,"name"], Genotype = GtypeNames, CableLen = c(CableLWT,CableLmut))
  
  for(i in seq_along(AllCilia)){
    StoreMetrics[i,"maxStr"] <- max(strahler_order(AllCilia[[i]])$segments)
    for (j in 1:as.numeric(StoreMetrics[i,"maxStr"])) {
      AvgStrLen <- Strahler_seg_avglength(AllCilia[[i]],j)
      nameVar <- paste("avgL",as.character(j),"StrOr",sep = '')
      StoreMetrics[i,nameVar] <- AvgStrLen
    }
  }
  StoreMetrics <- 
    StoreMetrics %>% 
    pivot_longer(cols =avgL1StrOr:avgL4StrOr,
                 names_to = "Strahler_number",
                 names_pattern = "avgL(.)StrOr", 
                 values_to = "average_length") %>% 
    drop_na() %>%
    mutate(Pc_average_length = (100*(average_length/CableLen)))
  
  StoreMetrics$Genotype <- factor(StoreMetrics$Genotype,levels =  c("WT", '"c-ops-1"^"∆8/∆8"'))

   
  ## Statistical test cable length
 { 
   ggplot(StoreMetrics %>%
           ungroup(),aes(x =CableLen)) + geom_histogram()
  
  stat.testcable <- StoreMetrics %>%
    distinct(skids,sknames,CableLen,Genotype) %>%
    wilcox_test(CableLen ~ Genotype, alternative = "g", paired = F) %>%
    #adjust_pvalue(method = "bonferroni") %>%
    add_significance()
  stat.testcable
  
  stat.testcable <- stat.testcable %>% 
    add_y_position()
  stat.testcable$p <- round(stat.testcable$p,3)
  stat.testcable$y.position <- round(stat.testcable$y.position,1)
  }
##Plotting ciliary length feach genotype----
  
  Glabels <-  (parse(text=unique(as.character(StoreMetrics$Genotype))))
  
  PlotCiliaLength <- (
    ggplot(StoreMetrics  %>%
             distinct(skids,sknames,CableLen,Genotype),
           aes(x=Genotype,y = round(CableLen)/1000, col = Genotype)) +
      theme_minimal() +
      theme_plot +
      background_grid(major = "none", minor = "none") +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 10, angle = 0 , colour="black")) +
      geom_violin() + 
      geom_point(position=position_jitterdodge(dodge.width = 1)) +
      scale_color_manual(values =  c("#000000", "#D55E00")) +
      stat_pvalue_manual(
        stat.testcable,
        bracket.nudge.y = 0, 
        tip.length = 0,
        step.increase = 0.05, 
        label = "p") +  
      scale_y_continuous(breaks = seq(0,1000000/1000,100000/1000),limits = c(0,2000000/1000)) +
      scale_x_discrete(labels= Glabels) +
      geom_hline(yintercept = 0) +
      coord_cartesian(ylim = c(0,350000/1000)) + 
      labs(
        x = "",
        y = str_wrap("cPRC cilia  length (µm)",width = 15),
        color = "genotype"
      )
  )
  PlotCiliaLength
  
  ##External to internal length comparison-----
  {
  InternalMetric <- StoreMetrics %>%
    filter(Strahler_number != 1 & Strahler_number != maxStr) %>% 
    group_by(skids) %>% print(n = 100) %>% summarise(sumInternal = sum(average_length))
  
  
  ExternalMetric <- StoreMetrics %>%
    group_by(skids) %>%
    filter(Strahler_number == 1 & Strahler_number != maxStr) %>% 
    mutate(Ex2InLen = average_length/InternalMetric$sumInternal[cur_group_id()]) %>% 
    print(n = 100)
  
  ## Statistical test Ext. vs internal ratio
  ##Comparing normalised length of main branch.
  ggplot(ExternalMetric,aes(x =Ex2InLen)) + geom_histogram()
  
  ##### Testing differences between Periods for each pressure level(paired one tail t-test)
  stat.Ex2Int <- ExternalMetric %>%
    ungroup() %>%
    wilcox_test(Ex2InLen ~ Genotype, alternative = "g", paired = F) %>%
    #adjust_pvalue(method = "bonferroni") %>%
    add_significance()
  stat.Ex2Int
  
  stat.Ex2Int <- stat.Ex2Int %>% 
    add_y_position()
  stat.Ex2Int$p <- round(stat.Ex2Int$p,3)
  }
##Plotting Ext. to Int. branch length bet. genotype----
  
  ExternalMetric$Genotype <- factor(ExternalMetric$Genotype,levels =  c("WT", '"c-ops-1"^"∆8/∆8"'))
  Glabels <-  (parse(text=unique(as.character(ExternalMetric$Genotype))))
  
  Ex2IntPlot <- (
    ggplot(ExternalMetric,
           aes(x=Genotype,y = Ex2InLen, col = Genotype)) +
      theme_minimal() +
      theme_plot +
      background_grid(major = "none", minor = "none") +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 10, angle = 0 , colour="black")) +
      geom_violin() + 
      geom_point(position=position_jitterdodge(dodge.width = 1)) +
      scale_color_manual(values =  c("#000000", "#D55E00")) +
      stat_pvalue_manual(
        stat.Ex2Int,
        bracket.nudge.y = 0, 
        tip.length = 0,
        step.increase = 0.05, 
        label = "p") +  
      scale_y_continuous(breaks = seq(0,100,5),limits = c(0,100)) +
      scale_x_discrete(labels= Glabels) +
      geom_hline(yintercept = 0) +
      coord_cartesian(ylim = c(0,20)) + 
      labs(
        x = "",
        y = str_wrap("terminal/internal branch length",width = 15),
        color = "genotype"
      )
  )
  Ex2IntPlot
  
  
  ## Statistical test basal body to first branching point
  ##Comparing normalized length of main branch.
  ggplot(StoreMetrics %>%
           ungroup() %>%
           filter(maxStr == Strahler_number),aes(x =Pc_average_length)) + geom_histogram()
  
  stat.testRootBranching <- StoreMetrics %>%
    ungroup() %>%
    filter(maxStr == Strahler_number) %>%
    wilcox_test(Pc_average_length ~ Genotype, alternative = "l", paired = F) %>%
    #adjust_pvalue(method = "bonferroni") %>%
    add_significance()
  stat.testRootBranching
  print(stat.testRootBranching, n = 100)
  
  stat.testRootBranching <- stat.testRootBranching %>% 
    add_y_position()
  stat.testRootBranching$p <- round(stat.testRootBranching$p,3)
  
  ##Plotting pc length of main branch bet. genotype----
  
  StoreMetrics$Genotype <- factor(StoreMetrics$Genotype,levels =  c("WT", '"c-ops-1"^"∆8/∆8"'))
  
  LengthBase <- (
    ggplot(StoreMetrics %>%
             filter(maxStr == Strahler_number),
           aes(x=Genotype,y = Pc_average_length, col = Genotype)) +
      theme_minimal() +
      theme_plot +
      background_grid(major = "none", minor = "none") +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 9, angle = 0 , colour="black")) +
      geom_violin() + 
      geom_point(position=position_jitterdodge(dodge.width = 1)) +
      scale_color_manual(values =  c("#000000", "#D55E00")) +
      stat_pvalue_manual(
        stat.testRootBranching,
        bracket.nudge.y = 0, 
        tip.length = 0,
        step.increase = 0.05, 
        label = "p") +  
      scale_y_continuous(breaks = seq(0,100,2),limits = c(0,100)) +
      scale_x_discrete(labels= Glabels) +
      geom_hline(yintercept = 0) +
      coord_cartesian(ylim = c(0,8)) + 
      labs(
        x = "",
        y = str_wrap("cPRC cilia base length (% total length)",width = 23),
        color = "genotype"
      )
  )
  LengthBase
  
  # ## Statistical test internal branch length
  # ##Comparing normalised length of internal branches.
  # {ggplot(StoreMetrics %>%
  #          ungroup() %>%
  #          filter(Strahler_number != 1 & Strahler_number != maxStr & Pc_average_length < 10),
  #        aes(x =Pc_average_length)) + 
  #   geom_histogram()
  # 
  # stat.testInternalBranching <- StoreMetrics %>%
  #   ungroup() %>%
  #   filter(Strahler_number != 1 & Strahler_number != maxStr & Pc_average_length < 10) %>%
  #   wilcox_test(Pc_average_length ~ Genotype, alternative = "l", paired = F) %>%
  #   #adjust_pvalue(method = "bonferroni") %>%
  #   add_significance()
  # stat.testInternalBranching
  # 
  # stat.testInternalBranching <- stat.testInternalBranching %>% 
  #   add_y_position()
  # stat.testInternalBranching$p <- round(stat.testInternalBranching$p,3)
  # 
  # ##Plotting pc length of internal branches bet. genotype
  # 
  # PlotLengthInternal <- (
  #   ggplot(StoreMetrics %>%
  #            filter(Strahler_number != 1 & Strahler_number != maxStr & Pc_average_length < 10),
  #          aes(x=Genotype,y = Pc_average_length, col = Genotype)) +
  #     theme_minimal() +
  #     theme_plot +
  #     background_grid(major = "none", minor = "none") +
  #     theme(legend.position = "none",
  #           axis.text.x = element_text(size = 10, angle = 0 , colour="black")) +
  #     geom_violin() + 
  #     geom_point(position=position_jitterdodge(dodge.width = 1)) +
  #     scale_color_manual(values =  c("#000000", "#D55E00")) +
  #     stat_pvalue_manual(
  #       stat.testInternalBranching,
  #       bracket.nudge.y = 0, 
  #       tip.length = 0,
  #       step.increase = 0.05, 
  #       label = "p") +  
  #     scale_y_continuous(breaks = seq(0,100,2),limits = c(0,100)) +
  #     scale_x_discrete(labels= Glabels) +
  #     geom_hline(yintercept = 0) +
  #     coord_cartesian(ylim = c(0,10)) + 
  #     labs(
  #       x = "",
  #       y = str_wrap("cPRC cilia internal branch size (% total ciliary length)",width = 15),
  #       color = "genotype"
  #     )
  # )
  # PlotLengthInternal
  # 
  # }
  # ## Statistical test terminal branch length
  # ##Comparing normalised length of terminal branches.
  # ggplot(StoreMetrics %>%
  #          ungroup() %>%
  #          filter(Strahler_number == 1 & Strahler_number != maxStr),
  #        aes(x =Pc_average_length)) + 
  #   geom_histogram()
  # 
  # stat.testTerminalBranching <- StoreMetrics %>%
  #   ungroup() %>%
  #   filter(Strahler_number == 1 & Strahler_number != maxStr) %>%
  #   wilcox_test(Pc_average_length ~ Genotype, alternative = "l", paired = F) %>%
  #   #adjust_pvalue(method = "bonferroni") %>%
  #   add_significance()
  # stat.testTerminalBranching
  # 
  # stat.testTerminalBranching <- stat.testTerminalBranching %>% 
  #   add_y_position()
  # stat.testTerminalBranching$p <- round(stat.testTerminalBranching$p,3)
  # 
  # ##Plotting pc length of Terminal branches bet. genotype
  # 
  # PlotLengthTerminal <- (
  #   ggplot(StoreMetrics %>%
  #            filter(Strahler_number == 1 & Strahler_number != maxStr & Pc_average_length < 10),
  #          aes(x=Genotype,y = Pc_average_length, col = Genotype)) +
  #     theme_minimal() +
  #     theme_plot +
  #     background_grid(major = "none", minor = "none") +
  #     theme(legend.position = "none",
  #           axis.text.x = element_text(size = 10, angle = 0 , colour="black")) +
  #     geom_violin() + 
  #     geom_point(position=position_jitterdodge(dodge.width = 1)) +
  #     scale_color_manual(values =  c("#000000", "#D55E00")) +
  #     stat_pvalue_manual(
  #       stat.testTerminalBranching,
  #       bracket.nudge.y = 0, 
  #       tip.length = 0,
  #       step.increase = 0.05, 
  #       label = "p") +  
  #     scale_y_continuous(breaks = seq(0,100,2),limits = c(0,100)) +
  #     scale_x_discrete(labels= Glabels) +
  #     geom_hline(yintercept = 0) +
  #     coord_cartesian(ylim = c(0,10)) + 
  #     labs(
  #       x = "",
  #       y = str_wrap("cPRC cilia Terminal branch size (% total ciliary length)",width = 15),
  #       color = "genotype"
  #     )
  # )
  # PlotLengthTerminal

# generate figure composite panel grid ------------------------------------


imgEM <- readPNG("Manuscript/pictures/EMcPRC_suppnolabs.png")
Fontsize = 10
Rect1 <- rectGrob(
  x = -0.05,
  y = 0.025,
  width = unit(7.05, "mm"),
  height = unit(93, "mm"),
  hjust = 0, vjust = 0,
  gp = gpar(fill = NULL, alpha = 1 ,lwd = 2)
)
  panel_EMWTvsCops <-ggdraw() + 
    draw_image(imgEM, scale = 1) +
    draw_label(paste("1", "\u00B5", "m", sep = ""), 
               x = 0.75, y = 0.12, size = Fontsize, color = "black") +
    draw_label(expression(italic(paste("c-ops-",1^{"∆8/∆8"}))),
               angle = 90, x = -0.025, y = 0.5, size = Fontsize,color = "black") +
    draw_grob(Rect1)
    
  layout <- "
  AAAAAA
  AAAAAA
  BBCCDD
  "
  
  FigSupp8 <- panel_EMWTvsCops +
    PlotCiliaLength +
    LengthBase +
    Ex2IntPlot +
    plot_layout(design = layout, heights = c(1, 1, 1, 1)) +
    plot_annotation(tag_levels = list(
      c("A", "B", "C", "D"))) &
    theme(plot.tag = element_text(size = 12, face = "plain"))
  
  
    
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_8.pdf", 
    FigSupp8, width = 2000, height = 2000,
    units = "px"
  )
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_8.png", 
    FigSupp8, width = 2000, height = 2000,
    units = "px"
  )



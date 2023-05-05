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
library(grid)
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
setwd(ProjectDir)

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


# #cPRC EM----
# 
# ##Loading skeletons
# 
#   Cilia_cPRCs_WT <- nlapply(read.neurons.catmaid("^Cilia_cPRC$", pid=21),
#                             function(x) smooth_neuron(x, sigma=200))
#   CableLWT <- summary(Cilia_cPRCs_WT)$cable.length
#   
#   cellWT <- nlapply(read.neurons.catmaid("^cell$", pid=21),
#                     function(x) smooth_neuron(x, sigma=200))
#   
#   Cilia_cPRCs_Copsmut <- nlapply(read.neurons.catmaid("^Cilia_cPRC$", pid=31),
#                                  function(x) smooth_neuron(x, sigma=200))
#   CableLmut <- summary(Cilia_cPRCs_Copsmut)$cable.length
#   cellmut <- nlapply(read.neurons.catmaid("^cell$", pid=31),
#                      function(x) smooth_neuron(x, sigma=200))
#   
#   #scale_bar_250nm = read.neurons.catmaid("^scale_bar_250nm$", pid=31)
#   
# 
# 
# 
# ##Tibble structure to store branch lengths for each segment type (end,root, or internal segment).
# 
#   AllCilia <- c(Cilia_cPRCs_WT,Cilia_cPRCs_Copsmut)
#   
#   GtypeNames <- c(rep( "WT",length(Cilia_cPRCs_WT[,"skid"])),
#                   rep( '"c-ops-1"^"∆8/∆8"',length(Cilia_cPRCs_Copsmut[,"skid"])
#                        )
#                   )
#   tags <- c("ends","soma")
#   SkeTaggedLenghtTib <- tibble()
#   for(i in seq_along(AllCilia)){ #getting average length of segments for each Strahler order for each cilium.
#     if (i == 1) {
#       SkeTaggedLenghtTib <- SegLengthwTags(AllCilia[[1]],tags)
#     } else {
#       SkeTaggedLenghtTib <- bind_rows(SkeTaggedLenghtTib,
#                                     SegLengthwTags(AllCilia[[i]],
#                                                    tags)
#                                     )
#     }
#   }
#   SkeTaggedLenghtTib <- SkeTaggedLenghtTib %>%
#     mutate(Pc_average_length = (100*(Seglength/CableLen)))
#   
#   SkeTaggedLenghtTib <- SkeTaggedLenghtTib %>%
#     nest(values = - skids) %>%
#     mutate(Genotype = GtypeNames,
#            sknames = AllCilia[,"name"]
#            )
#   SkeTaggedLenghtTib$Genotype <- factor(SkeTaggedLenghtTib$Genotype,levels =  c("WT", '"c-ops-1"^"∆8/∆8"'))
#   
#    

#
#   ##External to internal length comparison
#
#
#
#     SkeTaggedLenghtTib <-  SkeTaggedLenghtTib %>%
#       mutate(
#         E2Is = (
#           (SummarySegLength %>%
#              group_by(skids,Tag) %>%
#              filter(Tag %in% "ends"))$sumSegLength/
#             (SummarySegLength %>%
#                group_by(skids,Tag) %>%
#                filter(Tag %in% "internal"))$sumSegLength
#           )
#         )
#
#      ## Statistical test Ext. vs internal ratio
#   ##Comparing normalised length of main branch.
#   ggplot(SkeTaggedLenghtTib,aes(x =E2Is)) + geom_histogram()
#
#   ##### Testing differences ratios External to internal branch lengths (sums or average values) for each pressure level(paired one tail t-test)
#   ##sum
#   stat.Ex2IntS <- SkeTaggedLenghtTib %>%
#     ungroup() %>%
#     wilcox_test(E2Is ~ Genotype, alternative = "g", paired = F) %>%
#     #adjust_pvalue(method = "bonferroni") %>%
#     add_significance()
#   stat.Ex2IntS
#
#   stat.Ex2IntS <- stat.Ex2IntS %>%
#     add_y_position()
#   stat.Ex2IntS$p <- round(stat.Ex2IntS$p,3)
#
#
#
#   ##Plotting Ext. to Int. branch length bet. genotype
#
#   SkeTaggedLenghtTib$Genotype <- factor(SkeTaggedLenghtTib$Genotype,levels =  c("WT", '"c-ops-1"^"∆8/∆8"'))
#
#   Glabels <-  (parse(text=unique(as.character(SkeTaggedLenghtTib$Genotype))))
#
#   ###sum
#   Ex2IntSPlot <- (
#     ggplot(SkeTaggedLenghtTib,
#            aes(x=Genotype,y = E2Im, col = Genotype)) +
#       theme_minimal() +
#       theme_plot +
#       background_grid(major = "none", minor = "none") +
#       theme(legend.position = "none",
#             axis.text.x = element_text(size = 10, angle = 0 , colour="black")) +
#       geom_violin() +
#       geom_point(position=position_jitterdodge(dodge.width = 1)) +
#       scale_color_manual(values =  c("#000000", "#D55E00")) +
#       stat_pvalue_manual(
#         stat.Ex2IntS,
#         bracket.nudge.y = 0,
#         tip.length = 0,
#         step.increase = 0.05,
#         label = "p") +
#       scale_y_continuous(breaks = seq(0,100,20),limits = c(0,100)) +
#       scale_x_discrete(labels= Glabels) +
#       geom_hline(yintercept = 0) +
#       coord_cartesian(ylim = c(0,100)) +
#       labs(
#         x = "",
#         y = "(Σ terminal branch length) / \n (Σ internal branch length)",
#         color = "genotype"
#       )
#   )
#   Ex2IntSPlot
#   
#   

# generate figure composite panel grid ------------------------------------


imgEM <- readPNG("Manuscript/pictures/EMcPRC_suppnolabs.png")
Fontsize = 10
panel_EMWTvsCops <-ggdraw() + 
    draw_image(imgEM, scale = 1) +
    draw_label(paste("1", "\u00B5", "m", sep = ""), 
               x = 0.31, y = 0.2, size = Fontsize, color = "black") +
    draw_label(paste("1", "\u00B5", "m", sep = ""), 
               x = 0.86, y = 0.2, size = Fontsize, color = "black") +
    draw_label(expression(italic(paste("c-ops-",1^{"∆8/∆8"}))),
               x = 0.75, y = 0.84, size = Fontsize,color = "black") +
    draw_label(expression(italic(paste("WT"))),
               x = 0.2, y = 0.84, size = Fontsize,color = "black") 
    
  layout <- "
  AAAAAA
  AAAAAA
  "
  
  FigSupp8 <- panel_EMWTvsCops +
    plot_layout(design = layout) +
    plot_annotation(tag_levels = list(
      c("A"))) &
    theme(plot.tag = element_text(size = 12, face = "plain"))
  
  
    
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_8.pdf", 
    FigSupp8, width = 1850, height = 1000,
    units = "px"
  )
  ggsave(
    filename = "Manuscript/Figures/FigureSupplement_8.png", 
    FigSupp8, width = 1850, height = 1000,
    units = "px"
  )



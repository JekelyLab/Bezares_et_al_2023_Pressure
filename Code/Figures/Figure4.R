# Code to generate Figure 4 of pressure sensation paper
#Initialize----
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

# Loading libraries--------------------------------------------------
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
  library(rgl)
  library(rstatix)
  library(stringr)
  library(tidyverse)
  library(viridis)
  library(igraph)
  library(visNetwork)
  library(networkD3)
  library(zoo)
  options(nat.plotengine = 'rgl')
  #Sourcing functions
  source("Code/Figures/Functions_figures.R")
  
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
  

# circuit plot

# load and plot neurons --------------------------------------------------------

#catmaid server connectivity
#for the public server:
conn_http1 <- catmaid_login(
  server="https://catmaid.jekelylab.ex.ac.uk/", 
  authname="AnonymousUser",
  config=httr::config(ssl_verifypeer=0, http_version=1)
)

cPRC = nlapply(read.neurons.catmaid("^celltype5$", pid=11),
               function(x) smooth_neuron(x, sigma=20))
INNOS = nlapply(read.neurons.catmaid("^celltype7$", pid=11),
                function(x) smooth_neuron(x, sigma=2000))
INRGW = nlapply(read.neurons.catmaid("^celltype6$", pid=11),
                function(x) smooth_neuron(x, sigma=2000))
Ser_h1 = nlapply(read.neurons.catmaid("^celltype8$", pid=11),
                 function(x) smooth_neuron(x, sigma=6000))
MC = nlapply(read.neurons.catmaid("^celltype9$", pid=11),
             function(x) smooth_neuron(x, sigma=6000))
prototroch = nlapply(read.neurons.catmaid("^celltype_non_neuronal3$", pid=11),
                     function(x) smooth_neuron(x, sigma=6000))
yolk <- catmaid_get_volume(4, rval = c("mesh3d", "catmaidmesh", "raw"),
                           invertFaces = T, conn = NULL, pid = 11)
outline <- catmaid_get_volume(1, rval = c("mesh3d", "catmaidmesh", "raw"),
                              invertFaces = T, conn = NULL, pid = 11)
scalebar_50um_anterior = read.neurons.catmaid("^scalebar_50um_anterior$", pid=11)


plot_background()
#adjust zoom
par3d(zoom=0.67)

#plot neurons
plot3d(cPRC, soma = T, lwd=c(4,3,2,3), alpha = 1, col = "#D55E00")
plot3d(INNOS, soma = T, lwd=2, alpha = 0.3, col = Okabe_Ito[7])
plot3d(INRGW, soma = T, lwd=2, alpha = 0.4, col = "#56B4E9")
plot3d(Ser_h1, soma = T, lwd=c(3,5), alpha = c(0.5, 1), col = c('#009E73','#009E73'))
plot3d(MC, soma = T, lwd = 3, alpha = 0.8, col = Okabe_Ito[2])
plot3d(prototroch, soma = T, lwd = 2, alpha = 0.4, col = 'grey80')
plot3d(scalebar_50um_anterior, lwd = 2, col = 'black')

#add text labels
texts3d(54500,32000,15000, "cPRC", cex = 3, col = "#D55E00")
texts3d(35500,51500,11000, "Ser-h1", cex = 3, col = "#009E73")
texts3d(71500,36500,5000, "INNOS", cex = 2, col = Okabe_Ito[7])
texts3d(102000,48000,5000, "INRGW", cex = 2, col = "#56B4E9")
texts3d(76800,48600,5000, "MC", cex = 3, color = Okabe_Ito[2])
#adjust clipping
clipplanes3d(0, -1, 0, 130000)
#make snapshot
rgl.snapshot("Manuscript/pictures/cPRC_circuit_Catmaid.png",top = T)
# rgl.postscript("Manuscript/pictures/cPRC_circuit_Catmaid.eps")
close3d()
# get connectivity from CATMAID and plot network
cell_groups <- list(cPRC, INNOS, INRGW, Ser_h1, MC, prototroch)
N_cell_groups <- length(cell_groups)
  
cell_group_attr <- data.frame(
    cell_group_names  = c('cPRCs', 'IN^NOS', 'INRGW', 'Ser_h1', 'MC', 'prototroch'),
    type = c('SN', 'INNOS', 'INRGW', 'MN_ser', 'MN_ach', 'effector'),
    level = c('1',  '2', '2', '3', '3', '4')
  )
  
#iterate through cell group neuron lists and get connectivity for all against all
{
    #define empty synapse list with the right dimensions
    synapse_list <- vector("list", N_cell_groups*N_cell_groups)
    for (i in 1:N_cell_groups) {
      for (j in 1:N_cell_groups){
        #get connectors between two cell groups
        presyn_skids <- attr(cell_groups[i][[1]],"df")$skid
        postsyn_skids <- attr(cell_groups[j][[1]],"df")$skid
        connectivity <- catmaid_get_connectors_between(pre=presyn_skids, 
                                                       post=postsyn_skids, pid=11)
        #check the number of synapses from group1 -> group2
        N_synapses <-  dim(connectivity)[1]
        #change "NULL" to 0
        if(is.null(N_synapses)){N_synapses = 0}
        print ((i*N_cell_groups-N_cell_groups)+j)
        print (N_synapses)
        #add value to synapse list
        synapse_list [[(i*N_cell_groups-N_cell_groups)+j]] <- N_synapses
      }
    }
  }
  
#convert synapse list into a matrix of appropriate dimensions
synapse_matrix = matrix(unlist(synapse_list), byrow=TRUE, nrow=N_cell_groups)
rownames(synapse_matrix) <- cell_group_attr$cell_group_names
colnames(synapse_matrix) <- cell_group_attr$cell_group_names
synapse_matrix
  
  
# graph conversion
  
#edge weight filtering on the matrix to remove weak edges
synapse_matrix[synapse_matrix < 0] <- 0
max(synapse_matrix)
  
#with the make_graph function of igraph we turn it into a graph (input is the list of edge pairs)
Conn_graph <- graph_from_adjacency_matrix(
    synapse_matrix,
    mode = c("directed"),
    weighted = TRUE,
    diag = TRUE
  )
  
#calculate node weighted degree
weighted_degree=strength(Conn_graph, v = V(Conn_graph), mode = c("all"), 
                           loops = TRUE)
  
  
# use visNetwork to plot the network
  
## convert to VisNetwork-list
Conn_graph.visn <- toVisNetworkData(Conn_graph)
## copy column "weight" to new column "value" in list "edges"
Conn_graph.visn$edges$value <- sqrt(Conn_graph.visn$edges$weight)
#Conn_graph.visn$nodes$value = weighted_degree
Conn_graph.visn$nodes$group <- cell_group_attr$type
Conn_graph.visn$nodes$value <- c()
#hierarchical layout
#level	: Number. Default to undefined. When using the hierarchical layout, the level determines where the node is going to be positioned.
Conn_graph.visn$nodes$level <- cell_group_attr$level
#hierarchical layout
#From Color Universal Design (CUD): https://jfly.uni-koeln.de/color/
Okabe_Ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
               "#CC79A7", "#000000")
{
    visNet <- visNetwork(Conn_graph.visn$nodes,
                         Conn_graph.visn$edges) %>% 
      visHierarchicalLayout(levelSeparation=240, 
                            direction='UD',
                            nodeSpacing = 1,
                            sortMethod = "directed"
                            ) %>%
      visPhysics(hierarchicalRepulsion = list(nodeDistance = 200)) %>%
      visEdges(smooth = list(type = 'curvedCW', roundness=0),
               scaling=list(min=4, max=12),
               color = list(inherit=TRUE, opacity=0.8),
               arrows = list(to = list(enabled = TRUE, 
                                       scaleFactor = 1.2, type = 'arrow'))) %>%
      visNodes(borderWidth=0.3, 
               color = list(background=Conn_graph.visn$nodes$color, border='black'),
               opacity=0.9,
               shape='dot', 
               font=list(color='black', size=44),
               scaling = list(label=list(enabled=TRUE, min=50, max=65)),
               level= Conn_graph.visn$nodes$level) %>%
      visOptions(width = 1000, height = 800,
                 highlightNearest = list(enabled=TRUE, degree=1, 
                                         algorithm='hierarchical',
                                         labelOnly=FALSE)) %>%
      visInteraction(dragNodes = TRUE, dragView = TRUE,
                     zoomView = TRUE, hover=TRUE,
                     multiselect=TRUE) %>%
      visGroups(groupname = "INRGW", shape = "circle", 
                opacity=1, color="#56B4E9") %>%
      visGroups(groupname = "INNOS", shape = "circle", 
                opacity=1, color=Okabe_Ito[7]) %>%
      visGroups(groupname = "SN", shape = "circle", 
                opacity=1, color="#D55E00") %>%
      visGroups(groupname = "MN_ser", shape = "circle", 
                opacity=1, color="#009E73")  %>%
    visGroups(groupname = "MN_ach", shape = "circle", 
              opacity=1, color="#cccccc")  %>%
      visGroups(groupname = "effector", shape = "circle", 
                opacity=1, color="#cccccc")  %>%
      addFontAwesome()
    visNet
}
  
  
# save network diagram as html
saveNetwork(visNet, "Manuscript/pictures/visNetwork_INNOS.html")
webshot2::webshot(url="Manuscript/pictures/visNetwork_INNOS.html",
                    file="Manuscript/pictures/visNetwork_INNOS.png",
                    cliprect = c(0, 0, 1000, 1000), zoom=1)
  


# CBF--------------------------------------------------

### read data
  TableCBFTetAndCont <- read_csv("Data/TablesResults/CBF-Closure_CiliaryDynamics2dpf_TeTxLC-pLB253_nonbinned.csv")

  ###define pressure levels
  TableCBFTetAndCont$Pressure_Level <- factor(TableCBFTetAndCont$Pressure_Level, 
                                     levels = c("0", "3.125", "32.5",
                                                "85", "237.5",
                                                "556", "988")
  )
  
  TableCBFTetAndCont$Period <- factor(TableCBFTetAndCont$Period, 
                             levels = c("Before", "During_1","During_2", "After"),
                             labels = c("Before", "Stimulus","During_2", "After")
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
    mutate(CBF_sma3 = rollmean(CBF, k = 3, na.pad = T),
           CBF_sta3 = rollmean(rollmean(CBF, k =3, na.pad = T),
                               k = 3, na.pad = T))
  
  
  ####dCBF value
  
  PriorCBFMean <- 
    TableCBFTetAndCont %>%
    ungroup() %>%
    filter(Period %in% "Before") %>%
    group_by(Trial_ID,Larva_ID, Genotype) %>%
    summarise(MeanPrior_staCBF= mean(CBF_sta3, na.rm = TRUE)) %>%
    arrange(Trial_ID) %>%
    group_by(Trial_ID) 
  
  
  TableCBFTetAndCont <- 
    TableCBFTetAndCont %>%
    group_by(Trial_ID) %>% 
    mutate(dstaCBF= CBF - PriorCBFMean$MeanPrior_staCBF[cur_group_id()]) %>%
    relocate(dstaCBF, CBF_sma3, CBF_sta3, .after = CBF)
  

  ####max.CBFs
  MxCBF_Tet <- (
    TableCBFTetAndCont %>% 
      group_by(State,
               Pressure_Level,
               Genotype,
               Trial_ID,
               Period,
               Plasmid) %>% 
      # filter(RelTime > 60 | RelTime <= 30) %>% (in case comparing same size intervals)
      summarise(across(CBF:CBF_sta3, ~max(.x,na.rm = TRUE),.names = "max_{.col}")) %>%
      arrange(Trial_ID)
  )
  
  MxCBF_Tet["max_CBF"][MxCBF_Tet["max_CBF"] == -Inf] <- NA
  MxCBF_Tet["max_dstaCBF"][MxCBF_Tet["max_dstaCBF"] == -Inf] <- NA
  MxCBF_Tet["max_CBF_sma3"][MxCBF_Tet["max_CBF_sma3"] == -Inf] <- NA
  MxCBF_Tet["max_CBF_sta3"][MxCBF_Tet["max_CBF_sta3"] == -Inf] <- NA
  
  #### CBF relation to pressure stimulus
  
  Ref_period = "Stimulus"
  metric = "max_CBF_sta3"
  MxCBF_Tet <-(MxCBF_Tet %>% 
             group_by(Trial_ID,
                      Pressure_Level,
                      Genotype) %>% 
             filter(all(!is.na(max_CBF_sta3))) %>% 
             group_modify(~DirectionCBF(.,Ref_period,metric))
  )
  
  


### Statistical test
ggplot(MxCBF_Tet,aes(x =max_CBF_sta3)) + geom_histogram()
##### Testing differences between Periods for each pressure level(paired one tail wilcox)
stat.testTet <- MxCBF_Tet %>% 
  filter(State %in% c("positive") & 
           Period %in% c("Stimulus","Before") &
           Pressure_Level %in% c("0","85","237.5","556","988")) %>%
  group_by(Pressure_Level, Plasmid) %>%
  t_test(max_CBF_sta3 ~ Period, alternative = "less", paired = T) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
  stat.testTet
print(stat.testTet, n = 100)

stat.testTet <- stat.testTet %>% 
  add_xy_position(x = "Period")
stat.testTet$p.adj <- round(stat.testTet$p.adj,2)

### Plotting with P-values
  
  MaxPlot_tet <- (
    ggplot(
      MxCBF_Tet %>%
        filter(State %in% c("positive") &
                 Period %in% c("Before","Stimulus") &
                 Pressure_Level %in% c("0","85","237.5","556","988")),
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
        stat.testTet %>% filter(group1 %in% c("Before") &
                                  group2 %in% c("Stimulus") &
                                  Pressure_Level %in% c("0","85","237.5","556","988")),
        bracket.nudge.y = 0, 
        tip.length = 0, 
        hide.ns = TRUE, 
        step.increase = 0, 
        #position = position_jitterdodge(dodge.width = 1), 
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
      guides(color = "none") +
      scale_y_continuous(
        breaks = seq(0, 30, 5), 
        limits = c(0, 30), 
        expand = expansion(mult = c(0, 0.1))
      ) +
      facet_grid(vars(Plasmid),vars(Pressure_Level)) + 
      labs(color = str_wrap("CBF", width = 15)) +
      guides(color = guide_legend(keyheight = 0.3)) 
  )
MaxPlot_tet
#### Saving PNG of plot-

ggsave(
  "Manuscript/pictures/PanelPeriodvsmax.CBF_TetxvsControl2dpf.png",
  width = 2000, height = 600, units = "px", device = "png", bg = "white"
)

# generate figure composite panel grid ------------------------------------
Fontsize = 10
img_CellCircuit <- readPNG("Manuscript/pictures/cPRCcircCells.png") 
img_network <- readPNG("Manuscript/pictures/visNetwork_INNOS-crPanel.png")
imgTPHlabel <- readPNG("Manuscript/pictures/TPH-TeTxLC_noventralnolabsNoAc.png")
imgTPHcons <- readPNG("Manuscript/pictures/Construct_TPH-TomTetxnolabs.png")

X1 = 0.1
panel_cells <- ggdraw() + draw_image(img_CellCircuit) +
  draw_label(paste("50 ", "\u00B5", "m", sep = ""), 
             x = 0.7, y = 0.06, size = Fontsize, color = "black") +
  geom_segment(aes(x = X1,
                   y = 0.92,
                   xend = X1,
                   yend = 0.84,
                   color = "black"),
               arrow = arrow(type = 'closed', length = unit(2, "mm")),color = "black") +
  geom_segment(aes(x = X1,
                   y = 0.84,
                   xend = X1,
                   yend = 0.92,
                   color = "white"),
               arrow = arrow(type = 'closed', length = unit(2, "mm")),color = "black") + 
  draw_label("d", x = X1, y = 0.94, size = Fontsize ,color = "black") +
  draw_label("v", x = X1, y = 0.82, size = Fontsize,color = "black") 
  

panel_network <- ggdraw() + draw_image(img_network)
Xcoord1 = 0.09
Xcoord2 = 0.8
panel_TetxLC_immuno <- ggdraw() + 
  draw_image(imgTPHlabel) +
  geom_segment(aes(x = Xcoord1,
                   y = 0.94,
                   xend = Xcoord1,
                   yend = 0.86,
                   color = "white"),
               arrow = arrow(type = 'closed', length = unit(2, "mm")),color = "white") +
  geom_segment(aes(x = Xcoord1,
                   y = 0.86,
                   xend = Xcoord1,
                   yend = 0.94,
                   color = "white"),
               arrow = arrow(type = 'closed', length = unit(2, "mm")),color = "white") + 
  draw_label("d", x = Xcoord1, y = 0.96, size = Fontsize ,color = "white") +
  draw_label("v", x = Xcoord1, y = 0.84, size = Fontsize,color = "white") +
  draw_label(expression(paste("Ser-h",1^r)), x = 0.25, y = 0.59, size = Fontsize,color = "white") +
  draw_label("ciliary band", x = 0.17, y = 0.45, size = Fontsize,color = "white") +
  draw_label("α-HA", x = Xcoord2, y = 0.97, size = Fontsize,color = "white") +
  draw_label(paste("50 ", "\u00B5", "m", sep = ""), 
             x = 0.75, y = 0.08, size = Fontsize, color = "white") 
  

panel_TetxLC_immuno
panel_TetxLC_const <- ggdraw() + 
  draw_image(imgTPHcons) +
  draw_label("TPHp::tdT-P2A-TeTxLC", x = 0.2, y = 0.4, size = Fontsize,color = "black",hjust = 0) +
  draw_label("TPHp::tdT", x = 0.2, y = 0.65, size = Fontsize,color = "black",hjust = 0) 
  
panel_TetCBF <- ggdraw(MaxPlot_tet)  +
  draw_label("set pressure (mb)", x = 0.45, y = 0.99, size = Fontsize,color ="black", fontface = "plain") 


layout <- "
AAAB#C
######
DDDEEE
"
  
Fig4 <-
  panel_cells +
  panel_network +  
  panel_TetxLC_const + 
  panel_TetxLC_immuno +
  panel_TetCBF +
  plot_layout(design = layout, heights = c(1,0.02), widths = c(1, 0.05,1)) +
  plot_annotation(tag_levels = list(
      c("A", "B", "C", "D", "E"))) &
  theme(plot.tag = element_text(size = 12, face = "plain"))
  
  
ggsave(
    filename = "Manuscript/Figures/Figure4.pdf", 
    Fig4, width = 2800, height = 2200,
    units = "px"
  )
  
ggsave(
  filename = "Manuscript/Figures/Figure4.png", 
  Fig4, width = 2800, height = 2500,
  units = "px"
)


    




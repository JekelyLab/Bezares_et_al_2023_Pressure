# Functions Figures manuscript: Pressure sensation in Zooplankton-------

##Define libraries-------
library(gtable)
library(cowplot)
library(nat)
library(rgl)

##Functions----
DirectionCBF <- function(set,Rf,m){    
  ind = which(set$Period == Rf)
  testm = m
  for(y in 1:length(set$Period)){
    if(set[y,testm] > set[ind,testm]){
      set[y,"CBFrel"] = "Decrease"
    }
    else if (set[y,testm] == set[ind,testm]){
      set[y,"CBFrel"] = "Equal"
    }
    else{
      set[y,"CBFrel"] = "Increase"
    }
    if(y == ind){
      set[y,"CBFrel"] = set[ind - 1,"CBFrel"] 
    }
  }
 
  return(set)
}


DirectionArr <- function(set,Rf){    
  ind = which(set$Period == Rf)
  for(y in 1:length(set$Period)){
    if(set[y,"Pc_arrests"] > set[ind,"Pc_arrests"]){
      set[y,"ArrRel"] = "Decrease"
    }
    else if (set[y,"Pc_arrests"] == set[ind,"Pc_arrests"]){
      set[y,"ArrRel"] = "Equal"
    }
    else{
      set[y,"ArrRel"] = "Increase"
    }
    if(y == ind){
      set[y,"ArrRel"] = set[ind - 1,"ArrRel"] 
    }
  }
  #print(set)
  return(set)
}


##Source: https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
##Credit:user:10607772 (https://stackoverflow.com/users/10607772/double-beep)
shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}

#define background plotting function
plot_background <- function(x){
  nopen3d() # opens a pannable 3d window
  plot3d(yolk, WithConnectors = F, WithNodes = F, soma=F, lwd=2,
         rev = FALSE, fixup = F, add=T, forceClipregion = F, alpha=0.1,
         col="#E2E2E2") 
  #we define a z clipping plane for the frontal view
  par3d(zoom=0.52)
  nview3d("frontal", extramat=rotationMatrix(0.2, 1, 0.1, 0.5))
  #z-axis clip
  clipplanes3d(0, 0, -1, 65000)
  #y-axis clip
  clipplanes3d(1, 0, 0.16, 7000)
  #x-axis clip
  clipplanes3d(0, -1, 0.16, 130000)
  par3d(windowRect = c(0, 0, 800, 800)) #resize for frontal view
}

#From Color Universal Design (CUD): https://jfly.uni-koeln.de/color/
Okabe_Ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
               "#CC79A7", "#000000")

Strahler_seg_avglength <- function(Neuron,Str_num){
  IndexStrOrderSeg <- which(strahler_order(Neuron)$segments == Str_num)
  Aver = mean(seglengths(Neuron)[IndexStrOrderSeg])
}

SegLengthbyTag<- function(Neuron,SearchTag){
  
  TaggedNodes <- as.character(Neuron$tags[[grep(SearchTag,names(Neuron$tags))]])
  Points <- as.character(Neuron$d$PointNo)
  
  NeuroTib <- tibble(SegNumber = rep(1:length(Neuron$SegList)), Nodes = Neuron$SegList, Seglength = seglengths(Neuron))
  RowsTaggedNodes <- as_tibble(Neuron$d) %>% mutate(row_id=row_number()) %>%  filter(PointNo %in% TaggedNodes) %>% select(row_id)
  TaggedSegments <- NeuroTib %>% unnest(Nodes) %>% filter(Nodes %in% RowsTaggedNodes$row_id)
  return(TaggedSegments$Seglength)
}

SegLengthwTags<- function(Neuron,SearchTags){
  
  Points <- as.character(Neuron$d$PointNo)
  NeuroTib <- tibble(skids = Neuron$skid,
                     CableLen = summary(Neuron)$cable.length,
                     SegNumber = rep(1:length(Neuron$SegList)),
                     Nodes = Neuron$SegList,
                     Seglength = seglengths(Neuron),
                     Tag = NA)
  for(j in seq_along(SearchTags)){
    TaggedNodes <- as.character(Neuron$tags[[grep(SearchTags[j],names(Neuron$tags))]])
    RowsTaggedNodes <- as_tibble(Neuron$d) %>%
      mutate(row_id=row_number()) %>%
      filter(PointNo %in% TaggedNodes) %>%
      select(row_id)
    TaggedSegments <- NeuroTib %>% unnest(Nodes) %>% filter(Nodes %in% RowsTaggedNodes$row_id)
    NeuroTib[TaggedSegments$SegNumber,"Tag"] <- SearchTags[j]
  }
  NeuroTib <- mutate(NeuroTib,Tag = case_when(is.na(Tag) ~ "internal", TRUE ~ Tag))
  return(NeuroTib)
}
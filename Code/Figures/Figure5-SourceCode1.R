###Code to generate Figure Supplement 8 of pressure sensation paper
#Initialize----
{
  rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
  gc() #free up memory and report the memory usage.
}

# Loading libraries--------------------------------------------------
{
  library(cowplot)
  library(ggpubr)
  library(here)
  library(png)
}

#switch to project directory
project_dir <- here()
setwd(project_dir)



# generate figure composite panel grid ------------------------------------

{  
  img1 <- readPNG("Manuscript/pictures/SummaryPressureMech.png")
  panel_summary <-ggdraw() + draw_image(img1)
  
  Fig5 <- panel_summary 
  ggsave(
    filename = "Manuscript/Figures/Figure5.pdf", 
    Fig5, width = 4047, height = 2956,
    units = "px"
  )
  ggsave(
    filename = "Manuscript/Figures/Figure5.png", 
    Fig5, width = 4047, height = 2956,
    units = "px"
  )
}

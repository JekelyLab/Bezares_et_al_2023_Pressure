#Function to save text images.

saveTxtimgs <- function(TIDs,XYsource,OutD,Imgpx){
  # TIDs <- TrackIDTibbl
  # XYsource <- TabXYcoord
  # OutD <- ODir
  # Imgpx <- ImgPxs
  
  Imgpx <- str_split(Imgpx,pattern = "x")
  CatsTibl<- TIDs %>% ungroup  %>% distinct(TypeD,value)
  for(k in seq_along(CatsTibl$TypeD)){
    CurrTypeDirection <- CatsTibl[k,"TypeD"]
    CurrDir <- CatsTibl[k,"value"]
    TV <- (TIDs %>%
             filter(TypeD %in% CurrTypeDirection &
                      value %in% CurrDir) %>%
             select(TrackID))
    XYcoordinate <- unique(ceiling(XYsource %>%
                                     filter(TrackID %in% TV$TrackID) %>%
                                     select(X,Y)))
    ImgMat <- matrix(0,nrow = as.integer(Imgpx[[1]][1]), ncol = as.integer(Imgpx[[1]][2]))
      for (j in seq_along(XYcoordinate$X)) {
        ImgMat[as.integer(XYcoordinate[j,"X"]),
               as.integer(XYcoordinate[j,"Y"])] <- 255
      }
      write.table(t(ImgMat), file= paste(OutD,
                                         CurrTypeDirection,
                                         "_",
                                         CurrDir,
                                         "_",
                                         unique(XYsource$Trial),
                                         "_Time-",
                                         unique(XYsource$TimeSec),
                                         sep = ""),
                  row.names=FALSE, 
                  col.names=FALSE)
  }
  }


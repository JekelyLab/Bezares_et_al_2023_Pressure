#------Functions for calculating metrics----####


MetricCalc <- function(TabXYcoord,mp,fr){
  
  NumFrames <- length(unique(TabXYcoord$Frame)) #Obtaining the number of frames in the second analysed..
  NumTracks <- length(unique(TabXYcoord$TrackID)) #Obtaining the number of tracks in the second analysed.
  tablTotalDispPxFr <- TabXYcoord %>% group_by(TrackID) %>% group_modify(TotalDispPerTrack) # Calculating the total XY displacement per track for the  
  Speed = (sum(tablTotalDispPxFr$X)/(NumFrames*NumTracks))*mp*fr
  
  StraightnessYperTrack <- TabXYcoord %>% group_by(TrackID) %>% group_modify(StraightnessY)
  MeanStraightY = (sum(StraightnessYperTrack$Y)/(NumFrames*NumTracks))  
  StraightnessXperTrack <- TabXYcoord %>% group_by(TrackID) %>% group_modify(StraightnessX)
  MeanStraightX = (sum(StraightnessXperTrack$X)/(NumFrames*NumTracks))
  
  
  tablNetDispPxFr <- TabXYcoord %>% group_by(TrackID) %>% group_modify(NetDispPerTrack) 
  
  tableTortuosPerTrack <- tablTotalDispPxFr %>% rowwise() %>% summarise(across(X,~ tablNetDispPxFr[[cur_column()]][cur_group_id()]/.x))  #Accoriding to  Codling et al, 2008
  MeanTortuos =  mean(tableTortuosPerTrack$X)
  
  tablVertDispPxFr <- TabXYcoord %>% group_by(TrackID) %>% group_modify(YdispPerTrack) 
  tablVertDispPxFr <- tablVertDispPxFr %>% mutate(DirectionTrack_vert =  case_when(Y>0 ~ "Downward",Y<0 ~ "Upward",Y == 0 ~ "Quiet"))
  
  tablHoriDispPxFr <- TabXYcoord %>% group_by(TrackID) %>% group_modify(XdispPerTrack) 
  tablHoriDispPxFr <- tablHoriDispPxFr %>% mutate(DirectionTrack_Hori =  case_when(X>0 ~ "Rightward",X<0 ~ "Leftward",X == 0 ~ "Quiet"))
  
  #VertDisplMetrics
  AllTracksVertDisp = (sum(tablVertDispPxFr$Y)/(NumFrames*NumTracks))*mp*fr
  AllTracksVertDisp = AllTracksVertDisp*-1 #0 is at the top of cuvette.
  UpTracksVD <- filter(tablVertDispPxFr,grepl("Upward",DirectionTrack_vert))
  DownTracksVD <- filter(tablVertDispPxFr,grepl("Downward",DirectionTrack_vert))
  NumUpTracks <- length(UpTracksVD$TrackID)
  NumDownTracks <- length(DownTracksVD$TrackID)
  UpTracksVertDisp = (sum(UpTracksVD$Y)/(NumFrames*NumUpTracks))*mp*fr
  DownTracksVertDisp = (sum(DownTracksVD$Y)/(NumFrames*NumDownTracks))*mp*fr
  UpTracksVertDisp = UpTracksVertDisp*-1
  DownTracksVertDisp = DownTracksVertDisp*-1
  
  tablVertMov <- TabXYcoord %>% group_by(TrackID) %>% group_modify(YMovPerTrack) 
  VertMov = (sum(tablVertMov$Y)/(NumTracks))*mp*fr
  VertMov = VertMov*-1
  
  #Horizontal DisplMetrics
  AllTracksHoriDisp = (sum(tablHoriDispPxFr$X)/(NumFrames*NumTracks))*mp*fr
  AllTracksHoriDisp = AllTracksHoriDisp #0 is at the left of cuvette.
  LeftTracksHD <- filter(tablHoriDispPxFr,grepl("Leftward",DirectionTrack_Hori))
  RightTracksHD <- filter(tablHoriDispPxFr,grepl("Rightward",DirectionTrack_Hori))
  NumLeftTracks <- length(LeftTracksHD$TrackID)
  NumRightTracks <- length(RightTracksHD$TrackID)
  LeftTracksHoriDisp = (sum(LeftTracksHD$X)/(NumFrames*NumLeftTracks))*mp*fr
  RightTracksHoriDisp = (sum(RightTracksHD$X)/(NumFrames*NumRightTracks))*mp*fr
  LeftTracksHoriDisp = LeftTracksHoriDisp
  RightTracksHoriDisp = RightTracksHoriDisp
  
  tablHoriMov <- TabXYcoord %>% group_by(TrackID) %>% group_modify(XMovPerTrack) 
  HoriMov = (sum(tablHoriMov$X)/(NumTracks))*mp*fr
  HoriMov = HoriMov
  
  
  return(c(Speed,
           AllTracksVertDisp,
           UpTracksVertDisp,
           DownTracksVertDisp,
           VertMov,
           NumUpTracks,
           NumDownTracks,
           AllTracksHoriDisp,
           LeftTracksHoriDisp,
           RightTracksHoriDisp,
           HoriMov,
           NumLeftTracks,
           NumRightTracks,
           MeanTortuos,
           MeanStraightX,
           MeanStraightY))
}  

TotalDispPerTrack <- function(set,...){    
  
  SumFrame_xy = 0
  for(y in 1:(length(set$Frame)-1)){
    Xval <- (set[y+1,"X"]-set[y,"X"])^2
    Yval <- (set[y+1,"Y"]-set[y,"Y"])^2
    SumFrame_xy = SumFrame_xy+sqrt(Xval+Yval)
    
  }
  return(SumFrame_xy)
}

StraightnessY <- function(set,...){
  
  SumStraightY = 0
  for(k in 1:(length(set$Frame)-1)){
    Ymov <- set[k+1,"Y"]-set[k,"Y"]
    Xval <- (set[k+1,"X"]-set[k,"X"])^2
    Yval <- (set[k+1,"Y"]-set[k,"Y"])^2
    Quad = sqrt(Xval+Yval)
    if (Quad>0){
      SumStraightY = SumStraightY-Ymov/Quad
    }
    else{
      SumStraightY = SumStraightY-Ymov
    }
  }
  return(SumStraightY)
}


StraightnessX <- function(set,...){
  
  SumStraightX = 0
  for(k in 1:(length(set$Frame)-1)){
    Xmov <- set[k+1,"X"]-set[k,"X"]
    Xval <- (set[k+1,"X"]-set[k,"X"])^2
    Yval <- (set[k+1,"Y"]-set[k,"Y"])^2
    Quad = sqrt(Xval+Yval)
    if (Quad>0){
      SumStraightX = SumStraightX+Xmov/Quad
    }
    else{
      SumStraightX = SumStraightX+Xmov
    }
  }
  return(SumStraightX)
}

NetDispPerTrack <- function(set,...){
  
  Yval <- (set[length(set$Frame),"Y"]-set[1,"Y"])^2
  Xval <- (set[length(set$Frame),"X"]-set[1,"X"])^2
  EuclNetD = sqrt(Xval+Yval)
  return(EuclNetD)
}

YdispPerTrack <- function(set,...){    
  
  SumFrame_y = 0
  for(y in 1:(length(set$Frame)-1)){
    Yval <- (set[y+1,"Y"]-set[y,"Y"])
    SumFrame_y = SumFrame_y+Yval
    
  }
  return(SumFrame_y)
}

XdispPerTrack <- function(set,...){    
  
  SumFrame_x = 0
  for(x in 1:(length(set$Frame)-1)){
    Xval <- (set[x+1,"X"]-set[x,"X"])
    SumFrame_x = SumFrame_x+Xval
    
  }
  return(SumFrame_x)
}

YMovPerTrack <- function(set,...){    
  
  Yval <- (set[length(set$Frame),"Y"]-set[1,"Y"])
  Yval <- Yval/length(set$Frame)
  
  return(Yval)
}

XMovPerTrack <- function(set,...){    
  
  Xval <- (set[length(set$Frame),"X"]-set[1,"X"])
  Xval <- Xval/length(set$Frame)
  
  return(Xval)
}

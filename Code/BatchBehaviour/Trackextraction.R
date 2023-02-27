#------Function to extract tracks per .res file----####

Trackextraction<-function(tabnum,tables){
  totalnumTracks=0
  for(k in 1:length(tables)){
    numTracks<-(ncol(tables[[k]])-2)/2
    numTracksCols<-(ncol(tables[[k]])-1)
    #XYpos<-paste(unlist(tables[[1]][,seq(2,numTracksCols,by=2)]),unlist(tables[[1]][,seq(3,numTracksCols,by=2)]),sep=",")
    nummerged=0
    for( z in 1: numTracks){
      name<-as.character(paste(tabnum,"-XYPos-tr",totalnumTracks+z,sep=""))
      MergedT<-tables[[k]] %>% unite(!!as.name(name),colnames(tables[[k]])[2+nummerged],colnames(tables[[k]])[3+nummerged],sep=",")
      if(z==1){
        column<-tables[[k]] %>% 
          unite(!!as.name(name),colnames(tables[[k]])[2+nummerged],colnames(tables[[k]])[3+nummerged],sep=",") %>%
          select(name)
      }
      else{
        new<-tables[[k]] %>% 
          unite(!!as.name(name),colnames(tables[[k]])[2+nummerged],colnames(tables[[k]])[3+nummerged],sep=",") %>%
          select(name)
        column<-add_column(column,new)
      }
      nummerged=nummerged+2
    }
    if(k==1){
      AllColumn<-column
    }
    else{
      AllColumn<-add_column(AllColumn,column)
    }
    totalnumTracks<-totalnumTracks+numTracks
  }
  AllColumn$Frame<-tables[[k]]$Frame
  return(AllColumn)  
}
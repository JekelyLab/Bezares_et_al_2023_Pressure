WriteSplittabs<-function(od,tables){
  for(z in 1:length(tables)){
    Trial<-unique(tables[[z]]$Trial)
    write.csv(tables[[z]],paste(od,paste(Trial,".txt",sep=""),sep=""),row.names = F)
  } 
}
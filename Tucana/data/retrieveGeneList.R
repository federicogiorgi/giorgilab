setwd("/home/luca/Documents/uniBO/serverFiles/Tucana/data/NBL/")
partPath<-"/home/luca/Documents/uniBO/serverFiles/Tucana/data/NBL/"
DIrrpath<-dir(path = "/home/luca/Documents/uniBO/serverFiles/Tucana/data/NBL/")
for (i in 1:length(DIrrpath))
{
  #print(DIrrpath[i])
  el<-DIrrpath[i]
  if (grepl("rda", el) & grepl("expmat", el))
  {
    print(el)
    DS<-get(load(paste0(partPath,el)))
    rownDS<-rownames(DS)
    write.csv(rownDS,file = paste0(partPath,"geneList",substr(el,1,nchar(el)-4),".csv"),row.names = FALSE,col.names = FALSE)
  }
}
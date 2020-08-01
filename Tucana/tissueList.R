setwd("/srv/shiny-server/DATA")
i <- 1
for(el in dir("/srv/shiny-server/DATA")) {
  if(grepl("expmat", el) & !(grepl("geneList", el)) & !(grepl("kocak", el)) 
     & !(grepl("nrc", el)) & !(grepl("target", el)))
  {
    #print(i)
    print(el)
    #i <- i+1
    load(el)
    print(ncol(expmat))
  }
}
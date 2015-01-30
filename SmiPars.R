

s<-"C:/Users/Nezdeshni/Desktop/RGNF/BinokularExamle/Experiment038_S03_054_Trial003 Samples.txt"


s<-"C:/Users/Nezdeshni/Desktop/RGNF/BinokularExamle/"


dir("C:/Users/Nezdeshni/Desktop/RGNF/BinokularExamle/", pattern = "*.txt",full.names=TRUE)




ReadFiles(s)
ReadFiles<-function(s){
  d<-dir(s, pattern = "*.txt",full.names=TRUE)
  i<-1
  v<-c(1:length(d))
  for(i in 1:length(d)){
    v[i]<-FindTable(d[i])
    #print(names(read.delim(d[i], dec=",", skip=v[i])))
  }
  return(v)
}



FindTable(s)
FindTable<-function(s){
  d<-readLines(s)
  fl<-TRUE
  z<-1
  while(fl){
    if (gregexpr(pattern ='#',d[z])[[1]][1]==1){
      z<-z+1
      if(gregexpr(pattern ='## Version:',d[z])[[1]][1]>=0){print(substr(d[z], 13 , nchar(d[z])) )  }
      
      if(gregexpr(pattern ='## Sample Rate:',d[z])[[1]][1]>=0){print(substr(d[z], 17 , nchar(d[z]))  ) }
      
      if(gregexpr(pattern ='## Separator Type:',d[z])[[1]][1]>=0){print(substr(d[z], 20 , nchar(d[z])) )  }
      
      if(gregexpr(pattern ='## Trial Count:',d[z])[[1]][1]>=0){print(substr(d[z], 17 , nchar(d[z]))  ) }
      
      if(gregexpr(pattern ='## Stimulus:',d[z])[[1]][1]>=0){print(substr(d[z], 14 , nchar(d[z])) )  }
      
      if(gregexpr(pattern ='## Subject:',d[z])[[1]][1]>=0){print(substr(d[z], 13 , nchar(d[z])) )  }
      
      if(gregexpr(pattern ='## Description:',d[z])[[1]][1]>=0){print(substr(d[z], 17 , nchar(d[z])) )  }
      
      if(gregexpr(pattern ='## Stimulus Dimension [mm]:',fixed = TRUE,d[z])[[1]][1]>=0){print(substr(d[z], 29 , nchar(d[z]))  ) }
      
      if(gregexpr(pattern ='## Head Distance [mm]:',fixed = TRUE,d[z])[[1]][1]>=0){print(substr(d[z], 24 , nchar(d[z])) )  }
      
    } else { fl<-FALSE}
  }
  return(z-1)
}



smi<-read.delim("C:/Users/Nezdeshni/Desktop/RGNF/BinokularExamle/Experiment038_S03_054_Trial003 Samples.txt", dec=",", skip=43)
names(smi)




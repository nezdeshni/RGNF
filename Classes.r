library("XLConnect")
setGeneric("Fill_Ts", function(object){standardGeneric("Fill_Ts")})
setGeneric("Xs", function(object){standardGeneric("Xs")})
setGeneric("FillSaccades", function(object){standardGeneric("FillSaccades")})
setGeneric("FillFixations", function(object){standardGeneric("FillFixations")})
setGeneric("FillWinks", function(object){standardGeneric("FillWinks")})
setGeneric("FillGlissades", function(object){standardGeneric("FillGlissades")})
setGeneric("ExcludePointByBound", function(object, rect){standardGeneric("ExcludePointByBound")})
setGeneric("FillROI",function(object, matrix){standardGeneric("FillROI")})
setGeneric("AddTrajectory",function(object, tr){standardGeneric("AddTrajectory")})
setGeneric("AddStimul",function(object, inpP, ROImat){standardGeneric("AddStimul")})
setGeneric("LoadSMI", function(object, path){standardGeneric("LoadSMI")})

#add trail-stimul
#add type of ROI
#add person list with factors
#Events? separately from traj

#p<-new("experimentData")
#x<-new("binocular")
#p@trajData<-list(1:5)
#p@trajData[[2]]<-x
#p@trajData

setMethod("LoadSMI",                             
          "experimentData",                                   
          function(object,path){                                                                   
            
            v<-SmiReadFiles(path)
            object@trajData<-list(1:length(v))
            
            for (i in 1:length(v)){
              object@trajData[i]<-new("monocular")
              
              d<-readLines(v[i])
              fl<-TRUE
              z<-1
              
              
              while(fl){
                if (gregexpr(pattern ='#',d[z])[[1]][1]==1){
                  z<-z+1
                  #if(gregexpr(pattern ='## Version:',d[z])[[1]][1]>=0){object@trajData[i]@<-substr(d[z], 13 , nchar(d[z]))  }
                  
                  if(gregexpr(pattern ='## Sample Rate:',d[z])[[1]][1]>=0){object@trajData[[i]]@situationParam$SampleRate<-substr(d[z], 17 , nchar(d[z]))  }
                  
                  #if(gregexpr(pattern ='## Separator Type:',d[z])[[1]][1]>=0){substr(d[z], 20 , nchar(d[z]))   }
                  
                  if(gregexpr(pattern ='## Trial Count:',d[z])[[1]][1]>=0){object@trajData[[i]]@situationParam$TrialCount<-substr(d[z], 17 , nchar(d[z]))   }
                  
                  #if(gregexpr(pattern ='## Stimulus:',d[z])[[1]][1]>=0){substr(d[z], 14 , nchar(d[z]))   }
                  
                  if(gregexpr(pattern ='## Subject:',d[z])[[1]][1]>=0){object@trajData[[i]]@situationParam$Subject<-substr(d[z], 13 , nchar(d[z]))   }
                  
                  if(gregexpr(pattern ='## Description:',d[z])[[1]][1]>=0){object@trajData[[i]]@situationParam$Description<-substr(d[z], 17 , nchar(d[z]))   }
                  
                  if(gregexpr(pattern ='## Stimulus Dimension [mm]:',fixed = TRUE,d[z])[[1]][1]>=0){object@trajData[[i]]@situationParam$StimulusDimension<-strsplit(substr(d[z], 29 , nchar(d[z])),"\t")[[1]]   }
                  
                  if(gregexpr(pattern ='## Head Distance [mm]:',fixed = TRUE,d[z])[[1]][1]>=0){object@trajData[[i]]@situationParam$HeadDistance<-substr(d[z], 24 , nchar(d[z]))   }
            
                } else { fl<-FALSE}
              } 
              z<-z-1
              object@trajData[[i]]@points<-read.delim(v[i], dec=",", skip=z)[1:3,1:2]
            } 
            return(object)
          }
)  





SmiReadFiles<-function(s){
  d<-dir(s, pattern = "*.txt",full.names=TRUE)#I think we should extend regexp here, with somthing more special then *.txt
  return(d)
}


SmiFindTable<-function(s){
  d<-readLines(s)
  fl<-TRUE
  z<-1
  while(fl){
    if (gregexpr(pattern ='#',d[z])[[1]][1]==1){
      z<-z+1
      if(gregexpr(pattern ='',d[z])[[1]][1]>=0){}
      if(gregexpr(pattern ='',d[z])[[1]][1]>=0){}
      if(gregexpr(pattern ='',d[z])[[1]][1]>=0){}
      if(gregexpr(pattern ='',d[z])[[1]][1]>=0){}
      if(gregexpr(pattern ='',d[z])[[1]][1]>=0){}
      if(gregexpr(pattern ='',d[z])[[1]][1]>=0){}
      if(gregexpr(pattern ='',d[z])[[1]][1]>=0){}
      if(gregexpr(pattern ='',d[z])[[1]][1]>=0){}
      if(gregexpr(pattern ='',d[z])[[1]][1]>=0){}
    } else { fl<-FALSE}
  }
  return(z-1)
}




mem <- function() {
  bit <- 8L * .Machine$sizeof.pointer
  if (!(bit == 32L || bit == 64L)) {
    stop("Unknown architecture", call. = FALSE)
  }
  
  node_size <- if (bit == 32L) 28L else 56L
  
  usage <- gc()
  sum(usage[, 1] * c(node_size, 8)) / (1024 ^ 2)
}




#This function make list (which obviously present rectangle through two corners on "main diagonal")
#and return it as result. It's quite simpe but could safe time while workig with ROIs and make
#code more understandable
setRect<-function(l,t,r,b){                                     
              r<-list(left=l, top=t, right=r, bottom=b)
              return(r)
         }
#############################################################################################


#This function (which may be should be presented like method, but I don't like external pakage
#calling from it) load data of experiment (only simple example) from XLS document (it's structure 
#presented in the end of code under "Excel example" header). This is very close to loading from
#db, and it was created mostly for debuging
LoadFromXLS<-function(path, ExperData){                              #the input is path to *.xls(xlsx) and object of experimentData class
  wb <- loadWorkbook(path)                                           #we load Excel book(external calling, library("XLConnect") should be included)
  Tables <- readWorksheet(wb, sheet = getSheets(wb))                 #and load this book to Tables, by seets( smth like list of data.frame)
  i<-2                                                               #set start value of i to number 2 because of in first column of sheet-Traject
                                                                     #of our table is Time (only in our fictional universe ;) ) so it's static
                                                                     #here we assume that time is equal for all subjects, but
                                                                     #this assumption influente only structure of xls, class experimentData is
                                                                     #totaly free from such bounds. Next columns repeats x,y,sm(stimul material ID)
                                                                     #sequently for our imagine guys 
 #!   This index aripmetics is very suspicious 
  while (i <= length(Tables$Traject[1,])){                         #here we found number of above mentioned guys (so if any of them take 3 colums) 
                                                                     #their amount is (colcount number/3) -  1(1 column for TimeLine) and repeat load 
                                                                     #operations (filling slots of trajectoryes included in experimentData slot trajData
                                                                     #which is actualy list of monoculars, which is child of trajectory)
    
    ExperData<-AddTrajectory(ExperData,                              #on each iterration we call AddTrajectory method defined for experimentData class
                             data.frame(time=Tables$Traject[1],      #input is object (in tipical languge it would be "self") and data.frame(time,x,y,sm) 
                                        x=Tables$Traject[i],         #so it stores one trajectory from one expriment. Method it self is defined later
                                        y=Tables$Traject[i+1],       #in few words it's append list function
                                        sm=Tables$Traject[i+2]))
    i<-i+3                                                           #in the end of itteration we proceed to next subject
  }
#!  
  t<-Tables$GeneralInfo[which(Tables$GeneralInfo[,1]=="name"),2]    #loading information from xls sheet GeneralInfo which stores
  if(length(t)>0){ExperData@name<-t[1]}                             #general information about experiment. Restriction is one slot - one simple type( 
                                                                    #vector with 1 component)
                   
  t<-Tables$GeneralInfo[which(Tables$GeneralInfo[,1]=="author"),2]  
  if(length(t)>0){ExperData@author<-t[1]}
  
  t<-Tables$GeneralInfo[which(Tables$GeneralInfo[,1]=="description"),2]
  if(length(t)>0){ExperData@description<-t[1]}
  
  #!  I violently ignore loading situationParam in previous block, the reason is my lazyness and to be clear
  #!  it actualy I don’t give a fuck
  
  t<-1

#!   This index aripmetics is very suspicious
  for(i in 1:length(Tables$Stimuls[,1]-1)){                                                #loads information about stimuls from XLS sheet Stimuls
    ExperData<-AddStimul(ExperData,                                                        #loading proceeded row-by-row, and provided by method AddStimul                                                       
                         data.frame(ID=Tables$Stimuls[i,1],                                #defined for experimentData class
                                    description=Tables$Stimuls[i,2], 
                                    path=Tables$Stimuls[i,3]),                          
                         ROImat=as.matrix(Tables$StROI[t:(t+Tables$Stimuls[i,4]-1),1:4]))  #Single one complicated data type passed
    t<-t+Tables$Stimuls[i,4]                                                               #to this method is ROI matrix which describes set of
                                                                                           #rectanglesrelated to this stimul
  }
  
  return(ExperData)
}
#####################################################




#stimul is a class which present one atomic stimulus through it's description,
#path to it(tipicly picture), ID, and ROI(regions of interest) 
setClass("stimul",
         representation(description = "character",
                        path = "character",
                        ID = "numeric",
                        ROI = "list"),                #I think about ROI like about list of rectangles
                                                     #!  may be we should provide extra characteristics of each rectangle                              
                                                     #!  example is regions with big boobs and regions with small one ( structuraly like flag or slot)
         prototype(description = "not defined",
                   path = "not defined",
                   ROI = list(),
                   ID = 0                            #!  ID is preseted and it is not safe if called new(stimul) from other 
           )                                         #!  classes declaration, it could cause phantom object creation
)                                                    #!  we should think about prototypes very VERY SERIOUSLY
###############################################



# It is smth like abstract class (not formaly), this class present experiment
# meta data we could expand slots if think that experiment include other
# characteristics (differs from current). In other words it is a base class for
# all crap related to experiment description
setClass("metaExperiment",
         representation(name = "character",
                        description = "character",
                        author = "character",
                        eyetracker = "list",       #could be class but methods not exist so list is much simpler
                        situationParam = "list",   #could be class but methods not exist so list is much simpler
                        stimulesSet = "list"       #could be class wich include some convertations and so on
           ),
         prototype(     name = "Not defined",
                        description = "Not defined",
                        author = "Not defined",
                        eyetracker = list(name = "not defined", 
                                          type="not defined"),
                        situationParam = list(stimTrackerDist=-1,
                                              light ="not defined"
                                              ),   
                        stimulesSet = list()      
          )
         )

setClass("ROIL",
         representation(RectROI = "list",
                        ElipceROI = "list")
         )
############################################################


#Class which present simple experiment structure, it is child of meta experiment( so got equal slots)
#and extended by trajData(list of monocular).
#! Certainly weak typing provide extreme freedom of operating with slots
#! so we could define trajData slot of our object like list of whore islads, and R will have no problems with this shit
#! It is true for all our list slots, don't forget it! 
setClass("experimentData",
         representation(trajData="list"
                        ),
         prototype(trajData=list()
           ),
         contains="metaExperiment"
         )

####################################################

setClass("Person",                                  #class Person - created for 
         representation(name = "character",         #for experiments with dark magic ;)
                        age = "numeric")
)
#DataFrame with factors

setClass("trajectory",                              #class trajectory - represent the trajectory through
         representation(points = "data.frame")      #points - table aka df with 'x' 'y' 'time' columns
         )     
                                           


setClass("monocular",                               #class monocular - represent the gaze trajectory and include
         representation(saccades = "list",          #saccades - whitch would be found on demand
                        fixations = "list",         #fixations -//////
                        winks = "list",             #///////////////////////////////////////////
                        glissades = "list",
                        situationParam = "list"
         ), 
         #may be I should use C() instead of c(0) in prototype
         prototype(saccades = list(StartPoint=matrix(nrow=0,ncol=2),     #Saccade [1]
                                   EndPoint=matrix(nrow=0,ncol=2),       #Saccade [2]
                                   Way=c(0),                             #Saccade [4]
                                   Distance=c(0),                        #Saccade [3]
                                   TimeStart=c(0),                       #Saccade [5]
                                   TimeEnd=c(0),                         #Saccade [6]
                                   Duration=c(0),                        #Saccade [7]
                                   ADirection=c(0),                      #Saccade [8]
                                   RDirection=c(0),                      #Saccade [9]
                                   MaxSpeed=c(0),                        #Saccade [10]
                                   MaxAcceleration=c(0),                 #Saccade [11]
                                   MaxSpeedOfAccelerationCh=c(0),        #Saccade [12]
                                   ASimmetry=c(0),                       #Saccade [13]
                                   Curvature=c(0),                       #Saccade [14]
                                   MinPupilRad=c(0),                     #Saccade [15]
                                   MedPupilRad=c(0),                     #Saccade [17]
                                   MaxPupilRad=c(0)                      #Saccade [16]
                                ),
                   
                   glissades = list(StartPoint=matrix(nrow=0,ncol=2),    #Glissade [1] 
                                    EndPoint=matrix(nrow=0,ncol=2),      #Glissade [2]
                                    TimeStart=c(0),                      #Glissade [3]
                                    TimeEnd=c(0),                        #Glissade [4]
                                    Duration=c(0),                       #Glissade [5]
                                    Curvature=c(0)                       #Glissade [6]
                                ),
                   
                   fixations = list(FixPoint=matrix(nrow=0,ncol=2),  #Fixation [1]
                                    TimeStart=c(0),                  #Fixation [2]
                                    TimeEnd=c(0),                    #Fixation [3]
                                    Duration=c(0),                   #Fixation [4]
                                    Distance=c(0),                   #Fixation [5]
                                    MinPupilRad=c(0),                #Fixation [6]
                                    MedPupilRad=c(0),                #Fixation [8]
                                    MaxPupilRad=c(0)                 #Fixation [7]
                                ),
                   
                   winks = list(PointBefore=matrix(nrow=0,ncol=2),   #Wink [1]
                                PointAfter=matrix(nrow=0,ncol=2),    #Wink [2]
                                TimeStart=c(0),                      #wink [5]
                                TimeEnd=c(0),                        #Wink [6]
                                Duration=c(0),                       #Wink [7]
                                PupilRadBefore=c(0),                 #Wink [3]
                                PupilRadAfter=c(0)                   #Wink [4]
                           ),
                   
                   situationParam = list(                             #it could be seted like in experiment( cloned from it) or individualy( for each trajectory), and should describe
                                         light = "not defined",        #distance between Eye Tracker and stimul, light(IR, natural so on... and other meaningless shit)
                                         SampleRate = 0,
                                         TrialCount = 0,
                                         Subject = "not defined",
                                         Description = "not defined",
                                         StimulusDimension = c(0,0),
                                         HeadDistance = 0
                                         )                                #it inherits slots from trajectory class
         ),                                                          #adding slots saccades, fixations and so on
                                                                     #specify monocular as gaze based thing whereas
                                                                     #trajectory is simple 2d coordinates timeline
         contains="trajectory"
) 


setClass("binocular",                              #class binocular - use monocular class to define
         representation(left="monocular",          #left eye trajectory and
                        right="monocular"          #right eye trajectory (we mean gaze rutine uder trajectory here)
           )
         )


#Method defined for stimul, input is object of stimul class and matrix with rectangles
#left top right bottom by cols, values by rows
setMethod("FillROI",                             
          "stimul",                                   
          function(object, matrix){                         
            for (i in 1:length(matrix[,1])){             
                 object@ROI[[i]]<-setRect(matrix[i,1],matrix[i,2],matrix[i,3],matrix[i,4]) #we fill list of rectangles
          }                                                                                #with rows of matrix
          return(object)                        #!  two things about this method first - may be there are a way to pass                                          
          }                                                                             #!  params with out of dirty matrix[i,1],matrix[i,2],matrix[i,3],matrix[i,4]
)                                                                                       #!  redeclare setRect could be the best way 
                                                                                        #!  second - we should remember it is no appending
                                                                                        #!  it is refilling from first element      
setMethod("AddTrajectory",                             
          "experimentData",                                   
          function(object, tr){                         
            object@trajData[[length(object@trajData)+1]]<-new("monocular", points=tr)
            colnames(object@trajData[[length(object@trajData)]]@points)<-c("Time","x","y","sm")
            return(object)
          }
)


#Method from experimentData which add new stimul to related object it include ROI filing, from passed matrix
setMethod("AddStimul",                             
          "experimentData",                                   
          function(object, inpP, ROImat){                         
            object@stimulesSet[[length(object@stimulesSet)+1]]<-new("stimul", 
                                                                    ID=inpP$ID, 
                                                                    description=as.character(inpP$description), 
                                                                    path=as.character(inpP$path))
            
            object@stimulesSet[[length(object@stimulesSet)]]<-FillROI(object@stimulesSet[[length(object@stimulesSet)]],
                                                                      ROImat) 

            return(object)
          }
)




#An example of cuting trajectory by rectangle, method defined for trajectory class

setMethod("ExcludePointByBound",                          #method name    
          "trajectory",                                   #related class
          function(object, rect){                         #input is 2 variables I mean object
                                                          #wich part is trajectory and rectangle
                                                          #because of soft typization you could pass anithing else and wait for error
            i<-1
            while (i <= length(object@points$time)){      #we would exclude rows from points until end of table
              if((object@points$x[i]>rect$right)|         #if x lower then right bound
                 (object@points$x[i]<rect$left)|          #or....
                 (object@points$y[i]>rect$bottom)|        #or y ....
                 (object@points$y[i]<rect$top)            #or.... so it's quite simple
                 )
                 {
                object@points<-object@points[-i:-i,]      #then we exclude i row
              }else{i<-i+1}                               #else increment index
            } 
            return(object)                                #return passed object( smth wich part is trajectory)
          }                                               #as usual, because of pass-by-value shit( crap, anything...)
)



############################
#Next two methods is sandbox, so Xs - polymorphic method which behave extreamly differently
#for trajectory and Person signatures
#in case of trajectory it rename columns in points slot of passed object(object@points) and refill x,y, slots by sequence 1,2,3,...
setMethod("Xs",                                        
          "trajectory",                                
          function(object){    
            colnames(object@points)<-c("time","x","y","sm")    
            object@points$x<-c(1:length(object@points$x))
            object@points$y<-c(1:length(object@points$y))
            return(object)                    
          }
)

#in case of Person it fills passed object slots name and age by values "Valera" and 5 respectively
setMethod("Xs",
          "Person",
          function(object){
            object@name<-"Valera"
            object@age<-5
            return(object)
          }
  )
 
##############################


#Next four methods is caps for futher alhorithms, absolutely meaningless operations in all 4 functions, don't sped your time
#More information at right side of next lines
setMethod("FillGlissades",                                                    #all things related to gaze representation
          "monocular",                                                        #is lists
          function(object){
            count<-length(object@points$time)                                 #here is a cap, in future we would develop and
            object@glissades<-list(StartPoint=matrix(nrow=count,ncol=2),      #implement saccade, wink (etc) recognition alhorithms 
                                   EndPoint=matrix(nrow=count,ncol=2),         #but now we have determined lists filling
                                   TimeStart=c(1:count),                       #may be we should use dataframe here??????????
                                   TimeEnd=c(1:count),                         #in any case we could use "as.data.frame" routine
                                   Duration=c(1:count),                        #until length of all list component are equal
                                   Curvature=c(1:count)
            )
            
            for (p in 1:count){
              object@glissades$StartPoint[p,]=as.matrix(p,p)
              object@glissades$EndPoint[p,]=as.matrix(p,p)
              object@glissades$TimeStart[p]=p
              object@glissades$TimeEnd[p]=p
              object@glissades$Duration[p]=p
              object@glissades$Curvature[p]=p
            }
            return(object)                      
          }
          
)


setMethod("FillSaccades",
          "monocular",
          function(object){
            count<-length(object@points$time)
            object@saccades<-list(StartPoint=matrix(nrow=count,ncol=2),       #Saccade [1]
                                        EndPoint=matrix(nrow=count,ncol=2),   #Saccade [2]
                                        Way=c(1:count),                       #Saccade [4]
                                        Distance=c(1:count),                  #Saccade [3]
                                        TimeStart=c(1:count),                 #Saccade [5]
                                        TimeEnd=c(1:count),                   #Saccade [6]
                                        Duration=c(1:count),                  #Saccade [7]
                                        ADirection=c(1:count),                #Saccade [8]
                                        RDirection=c(1:count),                #Saccade [9]
                                        MaxSpeed=c(1:count),                  #Saccade [10]
                                        MaxAcceleration=c(1:count),           #Saccade [11]
                                        MaxSpeedOfAccelerationCh=c(1:count),  #Saccade [12]
                                        ASimmetry=c(1:count),                 #Saccade [13]
                                        Curvature=c(1:count),                 #Saccade [14]
                                        MinPupilRad=c(1:count),               #Saccade [15]
                                        MedPupilRad=c(1:count),               #Saccade [17]
                                        MaxPupilRad=c(1:count)                #Saccade [16]
                                        )
            
            for (p in 1:count){
              object@saccades$StartPoint[p,]=as.matrix(p,p)
              object@saccades$EndPoint[p,]=as.matrix(p,p)
              object@saccades$Way[p]=p
              object@saccades$Distance[p]=p
              object@saccades$TimeStart[p]=p
              object@saccades$TimeEnd[p]=p
              object@saccades$Duration[p]=p
              object@saccades$Direction[p]=p
              object@saccades$MaxSpeed[p]=p
              object@saccades$MaxAcceleration[p]=p
              object@saccades$MaxSpeedOfAccelerationCh[p]=p
              object@saccades$ASimmetry[p]=p
              object@saccades$Curvature[p]=p
              object@saccades$MinPupilRad[p]=p
              object@saccades$MedPupilRad[p]=p
              object@saccades$MaxPupilRad[p]=p              
            }
          return(object)                      
          }
)


setMethod("FillFixations",
          "monocular",
          function(object){
            count<-length(object@points$time)
            object@fixations<-list(FixPoint=matrix(nrow=count,ncol=2),  #Fixation [1]
                                  TimeStart=c(1:count),                 #Fixation [2]
                                  TimeEnd=c(1:count),                   #Fixation [3]
                                  Duration=c(1:count),                  #Fixation [4]
                                  Distance=c(1:count),                  #Fixation [5]
                                  MinPupilRad=c(1:count),               #Fixation [6]
                                  MedPupilRad=c(1:count),               #Fixation [8]
                                  MaxPupilRad=c(1:count)                #Fixation [7]
            )
            
            for(p in 1:count){
              object@fixations$FixPoint[p,]=as.matrix(p,p)
              object@fixations$TimeStart[p]=p
              object@fixations$TimeEnd[p]=p
              object@fixations$Duration[p]=p
              object@fixations$Distance[p]=p
              object@fixations$MinPupilRad[p]=p
              object@fixations$MedPupilRad[p]=p
              object@fixations$MaxPupilRad[p]=p              
            }
            return(object)                      
          }
)



setMethod("FillWinks",
          "monocular",
          function(object){
            count<-length(object@points$time)
            object@winks<-list(PointBefore=matrix(nrow=count,ncol=2),        #Wink [1]
                                  PointAfter=matrix(nrow=count,ncol=2),      #Wink [2]
                                  TimeStart=c(1:count),                      #wink [5]
                                  TimeEnd=c(1:count),                        #Wink [6]
                                  Duration=c(1:count),                       #Wink [7]
                                  PupilRadBefore=c(1:count),                 #Wink [3]
                                  PupilRadAfter=c(1:count)                   #Wink [4]
            )
            
            for(p in 1:count){
              object@winks$PointBefore[p,]=as.matrix(p,p)
              object@winks$PointAfter[p,]=as.matrix(p,p)
              object@winks$TimeStart[p]=p
              object@winks$TimeEnd[p]=p
              object@winks$Duration[p]=p
              object@winks$PupilRadBefore[p]=p
              object@winks$PupilRadAfter[p]=p              
            }
            return(object)                      
          }
          
)




#######################################################
#Example of positive effects wich could polymorphism(ad hoc) give to us
#Fill_Ts is polymorphic function(method) defined for class monocular
#and for class binocular, corresponded functions are different
#but in futher code we could call it from our functions independently from class(monocular or binocular)
#of passed object.
#In fact this method fill all slots which related to gaze crap frequently mentioned above

setMethod("Fill_Ts",
          "monocular",
          function(object){
            object<-FillSaccades(object)
            object<-FillFixations(object)
            object<-FillWinks(object)
            object<-FillGlissades(object)  
            return(object)
          }
  )

setMethod("Fill_Ts",
          "binocular",
          function(object){
            object@left<-FillSaccades(object@left)
            object@left<-FillFixations(object@left)
            object@left<-FillWinks(object@left)
            object@left<-FillGlissades(object@left)  
            
            object@right<-FillSaccades(object@right)
            object@right<-FillFixations(object@right)
            object@right<-FillWinks(object@right)
            object@right<-FillGlissades(object@right)  
            
            return(object)
          }
)





############example part###########################
### 1 ###
#example of filling single stimul ROIs for metaexperiment class
z<-new("metaExperiment")                              #create a new metaExperiment( you could use any of it's child ofcourse) in z
m<-matrix(1:20, (5:4))                                #set matrix value to m 
m                                                     #show m
z                                                     #show z

z@stimulesSet[[1]]<-new("stimul")
z@stimulesSet[[1]]<-FillROI(z@stimulesSet[[1]],m)     #assigne to first "stimul" from "stimulSet" "list" value obtained from "FillROI" method 
                                                      #"FillRoi" method return "stimul" object? wich ROI "list" filled by matrix m rows
                                                      #row[anth,1]=left, row[anth,2]= top... etc
z                                                     #show z after manipulations
z@stimulesSet[[1]]@ROI                                #if you want to see recently added ROIs execute this line 
#########


### 2 ###
#example of cuting trajectory points by one ROI( rectangle)
testt <- new("monocular", points=data.frame(x=1:5, y=1:5, time=c(1:5), mark=(1:5)))  #create new "monocular" with 5 timepoints, and 5 points
testt@points                                                                         #show points of "monocular"( show "trajectory")
r<-setRect(2, 2, 4, 4)                                                               #use user defined function setRect, it return list, with compponents
                                                                                     #left, top... etc. Other words set rectangle(2,2,4,4) to variable r
testt<-ExcludePointByBound(testt,r)                                                  #Exclude points, wich is out of r bounds
testt@points                                                                         #show points of testt trajectory
#########



### 3 ###
#filling event(saccades,fixations... etc) slots of trajectory
testt <- new("monocular", points=data.frame(x=1:5, y=1:5, time=c(1:5)))              #create new "monocular" with 5 timepoints, and 5 points
testt                                                                                #show monocular testt
testt<-Fill_Ts(testt)                                                                #fill slots by cap functions
testt                                                                                #show monocular testt
#########



### 4 ###
#more compicated example of №3example for binocular trajectory and it's characteristic
#the difference is in range of saccades(glissedes, fixations, etc) for left and right eye
bintest<-new("binocular")
bintest@left<-new("monocular", points=data.frame(x=1:5, y=1:5, time=c(1:5), sm=c(1:5)))
bintest@right<-new("monocular", points=data.frame(x=1:6, y=1:6, time=c(1:6), sm=c(1:6)))
bintest<-Fill_Ts(bintest)
bintest
#########


### 5 ###
#Example of loading experimentData object from XLS file

print(mem())
p<-"C:\\DataSeet_exp_1_duckdick.xlsx"  #define path(not nessesary, we could use constant path futher)
ExperData<-new("experimentData")       #create experimentData object
ExperData<-LoadFromXLS(p, ExperData)   #call for loadFromXLS
ExperData                              #show result
print(mem())


#########

### 6 ###
#Memory allocation and "stack" test, compile StackTest and Recive, then execute last 2 lines 
#ATTENTION  !!!RStudio would die!!!
#This example created to show how unpredictable( but all the same very reliable) is memory management
#in R-core, it could matters while working with big data 

#You need ~34mb of real memory to store list(1:9000000)
#So ~64 would be needed for list(list(1:9000000),list(1:9000000))
#In this example first printed text is memory allocated by R-core before filling the list
#All other results would show total memory allocated by R includung above mentioned memory


StackTest<-function(s){                                     
  i<-1
  z<-1
  while(i<4){  
    #s<-list(s,1:90000000)       #This is near a maximum allocation
    print(mem())                 #show "total" memory usage in mb
    s[[z]]<-list(1:9000000)      #Z-indexed ellement is list(1:900....)
    s[[z+1]]<-list(1:9000000)    #Z+1-indexed ellement is the same list
    z<-z+2                       #z<-z+2 to write new lists to new indexes
    print(mem())                 #show "total" memory usage in mb after append s            
    s<-Recive(s)                 #call for Recive function( we use this thing to try pass-by-value, pass-by-reference)
  }
  
  return(s)                      #it would never be called because of infinite loop
}


Recive<-function(e){        
  e<-e                                    #Let's try to make a stupid thing, write e to e adress( e is equal to s passed from StackTest)
                                          #actualy it is only adress(pass-by-reference) of s in memory
  print(mem())                            #Let's look to the total memory, it seems memory used before call Recive and after
                                          #is equal because of pass-by-reference mechanic 
  e[1]<-e[1]                              #Let's try smth equal to above mentioned e<-e, but with rewriting only e[1]<-e[1]
  print(mem())                            #This simple action would increace memory usage to ~2 times (for real list members memory need)
                                          #The reason is pass-by-value "paranoic things", R-core could not predict
                                          #result of e[1]<-e[1]( would it be equal to e<-e?), and create copy of s
                                          #in memory( e comes to value instead of being reference)
  #print(object.size(e)/1073741824)       #e size in Gb
  return(e)                               #return e value, after passing e value memory allocated to e would be free
}                                         #we can see it in the next iterration

p<-0
p<-StackTest(p)
gc()
#########

### 7 ###
#Loading all trajectory data from specified folder

g<-new("experimentData")
s<-"C:/Users/Nezdeshni/Desktop/RGNF/BinokularExamle/"
g<-LoadSMI(g,s)


g@trajData[[1]]



#########

######################################################




################# sandbox #####################
sizes <- sapply(0:100, function(n) object.size(seq_len(n)))
plot(0:100, sizes, xlab = "Length", ylab = "Bytes", type = "s")
sizes

sizes<-c(1:214748364)

s<-1
s[[1]]<-list(1:3)      #Z-indexed ellement is list(1:900....)
s[[2]]<-list(3:5) 
s[[1]]
s[[2]]


print(mem())
p<-"C:\\DataSeet_exp_1_duckdick.xlsx"  #define path(not nessesary, we could use constant path futher)

e<-new.env()
e$a<-c(1,2,3)
e$b<-23
e$ExperData<-new("experimentData")
as.list(e)
e$ExperData<-LoadFromXLS(p, e$ExperData)
as.list(e)


as.list(e)
c<-e
as.list(c)
e$b<-1
c$a<-c(3,2,1)

as.list(R_GlobalEnv())

print(mem())




##################################################



################# comment part ################


################# saccade #####################
#Координаты начала (X,Y)[1], 
#Координаты конца (X,Y)[2]. 
#Амплитуда (как расстояние между начальной и конечной позициями)[3]. 
#Амплитуда (как общая длинна пути, пройденного за время саккады)[4]. 
#Время начала (относительно начала экспериментальной ситуации)[5]. 
#Время завершения (относительно начала экспериментальной ситуации)[6]. Продолжительность[7]. 
#Направление (абсолютное в системе координат, связанной с изображением)[8]. Направление (относительно предыдущей саккады)[9]. 
#Максимальная пиковая скорость[10]. 
#Максимальное пиковое ускорение[11]. 
#Максимальная пиковая скорость изменения ускорения[12]. 
#Асимметрия (соотношение фаз ускорения и замедления во время саккады)[13]. 
#Кривизна как максимальное угловое отклонение от направления саккады[14]. Минимальная[15], 
#максимальная [16]
#и средняя[17] величина раскрытия зрачка во время саккады. 
################################################################################################

################### fixation ###################################################################
#Координаты (X,Y)[1]. 
#Время начала (относительно начала экспериментальной ситуации)[2]. 
#Время окончания (относительно начала экспериментальной ситуации)[3]. Продолжительность[4]. 
#Максимальная дистанция от центра фиксации до отсчета данных, входящего в состав фиксации (возможно в дальнейшем следует ввести другие показатели, характеризующие меру разброса отсчетов данных, входящих в фиксацию)[5]. 
#Минимальная[6], 
#максимальная[7]
#и средняя величина раскрытия зрачка во время фиксации[8].
################################################################################################

################## Wink ########################################################################
#Координаты (X,Y), предшествующие началу моргания[1]. 
#Координаты (X,Y) по завершении моргания[2]. 
#Величина раскрытия зрачка до начала моргания[3],
#Величина раскрытия зрачка после начала моргания[4]. 
#Время начала (относительно начала экспериментальной ситуации)[5]. 
#Время завершения (относительно начала экспериментальной ситуации)[6]. Продолжительность[7].
################################################################################################

################# Glissade #####################################################################
#Координаты начала (X,Y)[1], 
#Координаты конца (X,Y)[2]. 
#Время начала (относительно начала экспериментальной ситуации)[3]. 
#Время окончания (относительно начала экспериментальной ситуации)[4]. 
#Продолжительность[5].
#Кривизна[6].
################################################################################################

################ Excel example #################################################################
#Тут могла бы быть Ваша реклама!
################################################################################################
################################################################################################
rm(list=ls())
source('../src/Rasterise_dev_68akj.r')
debugValue=0
pkglist=c('data.table','raster','maptools','parallel','doParallel','nnet','maptools','rgeos','rgdal','randomForest','expm','Matrix','png')
CheckInstallPackage(pkglist)
T1File="../examples/LULC/1985.tif"
#raster(T1File) ;#sum(!is.na(raster(T1File)[]),na.rm=TRUE) #Data Connsitency Check
T2File="../examples/LULC/1995.tif";
#raster(T2File) ;#sum(!is.na(raster(T2File)[]),na.rm=TRUE) #Data Connsitency Check
T3File="../examples/LULC/2005.tif";
#raster(T3File) ;#sum(!is.na(raster(T3File)[]),na.rm=TRUE) #Data Connsitency Check
#PredictedFile="../examples/outputdata/senario-4-neighwith3-allocorder-134596782-demand-as2005-randomForest-2005.tif";
#PredictedFile="../examples/outputdata/senario-6-neighwithNA-allocNA-134596782-randomForest-2005-re.tif";
#PredictedFile="../examples/outputdata/senario-3-neighwithNA-allocorder-134596782-demand-as2005-randomForest-2005.tif";
#PredictedFile="../examples/outputdata/senario-2-neighwith3-allocorder-134596782-demand-businessAsUsual-logistics-2005.tif";
#PredictedFile="../examples/outputdata/senario-1-neighwithNA-allocorder-134596782-demand-businessAsUsual-logistics-2005.tif";
PredictedFile="../examples/outputdata/senario-5-neighwith3-allocorder-134596782-demand-as2005-mixregression-rrrrrrlrl-2005.tif";
#raster(PredictedFile) ;#sum(!is.na(raster(PredictedFile)[]),na.rm=TRUE)
#PredictedFile="../outputdata/Test-neighwithNA-allocNA-134596782-randomForest-2005-re.tif";

drvs85<-c(
DistanceToDrainage="../examples/Drivers/drivers_85/dist_stream.img",
DistanceToBuiltup="../examples/Drivers/drivers_85/Dist_urban.img",
DistanceToRoad="../examples/Drivers/drivers_85/road_final.img",
Elevation="../examples/Drivers/commonDrivers/elevation.img"
)
#stack(drvs85) ;sum(!is.na(stack(drvs85)[]),na.rm=TRUE) #Detail Data Connsitency Check
drvs95<-c(
DistanceToDrainage="../examples/Drivers/drivers_95/dist_stream.img",
DistanceToBuiltup="../examples/Drivers/drivers_95/Dist_urban.img",
DistanceToRoad="../examples/Drivers/drivers_95/road_final.img",
Elevation="../examples/Drivers/commonDrivers/elevation.img"
)
##stack(drvs95) ;sum(!is.na(stack(drvs95)[]),na.rm=TRUE) #Detail Data Connsitency Check
T1drivers=drvs85
T2drivers=drvs95
#colSums(!is.na(stack(c(drvs85,drvs95,T1File,T3File))[]),na.rm=TRUE)/sum(!is.na(raster(T2File)[]),na.rm=TRUE) #Detail Data Connsitency Check

#isDataSetCorrect(T1drivers,T2drivers,T1File,T2File,T3File) #Data Connsitency Check

restrictSpatial=c(1 ,0.98, 0.99, 0.98, 1, 1, 0.98, 0.93, 0.76)
clsName<-c("BuildUp","Agriculture","DenseForest","FallowLand","GrassLand","MixedForest","Plantation","ScrubLand","WaterBody")
#clsName<-paste("p",clsName,sep="")
mydemand=NA
#mydemand=as.numeric(c(2316,35245,11306,1762,5,71,3735,1562,3594)) #Envisaged
mydemand=as.numeric(c(1331,35634,11357,1722,5,61,3872,1622,3992))  #Actual
#getNewTM(TM,as.numeric(mydemand))
#na.value=128
na.value=NA
neighbourl<-list()
neighbourl[[1]]<-3  #window Size
neighbourl[[2]]<-2 #No of steps
neighbourl[[3]]<-1  #Is all the output files are required for everytime steps
#neighbourl=NA
myallocationorder=NA
#myallocationorder<-c(1,2,4,5,9,6,7,8,3)
#myallocationorder<-c(1,3,4,5,9,6,7,8,2)
#myallocationorder<-c(1,3,4,5,9,6,7,8,2)
########################
#mydemand=NA
#neighbourl=NA
myallocationorder<-c(1,3,4,5,9,6,7,8,2)
#model.type=c('logistic','logistic','logistic','logistic','logistic','logistic','logistic','logistic','logistic')
#model.type=c('randomForest','randomForest','randomForest','randomForest','randomForest','randomForest','randomForest','randomForest','randomForest')
#model.type=c('regression','regression','regression','regression','regression','regression','regression','regression','regression')
#model.type=c('nnet','nnet','nnet','nnet','nnet','nnet','nnet','nnet','nnet')
model.type=c('randomForest','randomForest','randomForest','randomForest','randomForest','randomForest','logistic','randomForest','logistic')

###############
model.formula=c("T1.BuildUp ~ TD1.DistanceToDrainage+TD1.DistanceToBuiltup+TD1.DistanceToRoad+TD1.Elevation",
                  "T1.Agriculture ~ TD1.DistanceToDrainage+TD1.DistanceToBuiltup+TD1.DistanceToRoad+TD1.Elevation",
                  "T1.DenseForest ~ TD1.DistanceToDrainage+TD1.DistanceToBuiltup+TD1.DistanceToRoad+TD1.Elevation",
                  "T1.FallowLand ~ TD1.DistanceToDrainage+TD1.DistanceToBuiltup+TD1.DistanceToRoad+TD1.Elevation",
                  "T1.GrassLand ~ TD1.DistanceToDrainage+TD1.DistanceToBuiltup+TD1.DistanceToRoad+TD1.Elevation",
                  "T1.MixedForest ~ TD1.DistanceToDrainage+TD1.DistanceToBuiltup+TD1.DistanceToRoad+TD1.Elevation",
                  "T1.Plantation ~ TD1.DistanceToDrainage+TD1.DistanceToBuiltup+TD1.DistanceToRoad+TD1.Elevation",
                  "T1.ScrubLand ~ TD1.DistanceToDrainage+TD1.DistanceToBuiltup+TD1.DistanceToRoad+TD1.Elevation",
                  "T1.WaterBody ~ TD1.DistanceToDrainage+TD1.DistanceToBuiltup+TD1.DistanceToRoad+TD1.Elevation")
#model.formula=NA

suitabilityDirectory='../examples/outputdata/'
myconversion<-matrix(
  c(1,5,3,2,4,6,7,8,9,
    2,1,4,3,5,6,7,8,9,
    2,5,1,3,4,6,7,8,9,
    2,4,3,1,5,6,7,8,9,
    2,5,4,3,1,6,7,8,9,
    2,5,4,3,6,1,7,8,9,
    2,5,4,3,6,7,1,8,9,
    2,5,4,3,6,7,8,1,9,
    4,6,5,3,7,8,9,2,1
  )
  ,nrow=length(myallocationorder),byrow=TRUE)
myconversion='TP'

getModelFitSummary(T1File,T2File,T1drivers,modelType='randomForest',withNAvalue=na.value,method="NotIncludeCurrentClass")
####nw<-ParallelComputeNearByWeight(T2File,withNA=na.value)
####createNeighbourMap(nw,T2File,suitabilityDirectory,clsName)
#plot(stack(paste(rep(suitabilityDirectory,length(clsName)),paste(clsName,"NW.tif",sep=""),sep="")))

print(format(Sys.time(), "%a %b %d %X %Y %Z"))
result<-isDataSetCorrect(T1drivers,T2drivers,T1File,T2File,T3File)
#result=FALSE
print(format(Sys.time(), "%a %b %d %X %Y %Z"))
if(result){
  genratePredictedMap(modelType=model.type,T1File,T2File,withClassName=clsName,T1drivers,T2drivers,na.value,
                      demand=mydemand,restrictSpatialMigration=restrictSpatial,neighbour=neighbourl,outputfile=PredictedFile,
                      conversionOrder=myconversion,classAllocationOrder=myallocationorder,maskFile=NA,aoiFile=NA,
                      modelformula=model.formula,suitabilityFileDirectory=suitabilityDirectory)
}
print(format(Sys.time(), "%a %b %d %X %Y %Z"))

getKappaSummary(T3File,PredictedFile,na.value,clsName)

#nohup R -f RunSteps1.R  &
############################USERCOMMAND ENDS#############################
#Show Suitability Map
#plot(stack(paste(rep(suitabilityDirectory,length(clsName)),paste(clsName,"SM.tif",sep=""),sep="")))
#Genrate and Show NeighbourMap Map
#nw<-ComputeNearByWeight(T2File,withNA=na.value)
#createNeighbourMap(nw,T2File,suitabilityDirectory,clsName)
#plot(stack(paste(rep(suitabilityDirectory,length(clsName)),paste(clsName,"NW.tif",sep=""),sep="")))
#st<-stack(T3File,PredictedFile);plot(st)
#colSums(!is.na(stack(c(drvs85,drvs95,T1File,T3File,PredictedFile))[]),na.rm=TRUE)/sum(!is.na(raster(T2File)[]),na.rm=TRUE)

#s<-stack("../examples/LULC/2005.tif","../examples/outputdata/senario-4-neigh-allocorder-124596783-randomForest-2005.tif") 
#s<-stack("../examples/LULC/2005.tif","../examples/outputdata/senario-4-neigh-allocorder-134596782-randomForest-2005.tif")
#s<-stack("../examples/outputdata/senario-4-neigh-allocorder-124596783-randomForest-2005.tif","../examples/outputdata/senario-4-neigh-allocorder-134596782-randomForest-2005.tif")

#s<-stack("../examples/outputdata/senario-4-neigh-randomForest-2005.tif","../examples/outputdata/Senario-3.tif") 
s<-stack(T3File,PredictedFile)

#plot(s)
r2<-s[[1]]*10+s[[2]]
table(r2[])
r3<-r2
r3[r2[]==11]=0
r3[r2[]==22]=0
r3[r2[]==33]=0
r3[r2[]==44]=0
r3[r2[]==55]=0
r3[r2[]==66]=0
r3[r2[]==77]=0
r3[r2[]==88]=0
r3[r2[]==99]=0
r4<-r3
r4[r3[]>=11 & r3[]<=19]=1
r4[r3[]>=21 & r3[]<=29]=2
r4[r3[]>=31 & r3[]<=39]=3
r4[r3[]>=41 & r3[]<=49]=4
r4[r3[]>=51 & r3[]<=59]=5
r4[r3[]>=61 & r3[]<=69]=6
r4[r3[]>=71 & r3[]<=79]=7
r4[r3[]>=81 & r3[]<=89]=8
r4[r3[]>=91 & r3[]<=99]=9
plot(stack(s,r3,r4))
# 

#Input: Vector Map At T0 and grid size of analysis.
#    Param1: Shape File Name   Rasterise_dev_6.R
#    Param2: Gridsize
#rm(list=ls())

status="Currently Processing"

setRStatus<-function(message){
  status=message;
}

getRStatus<-function(message){
  return(status);
} 

debug_getAllocatedCell<-function(dt1)
{
  return(sum(rowSums(dt1,na.rm=TRUE)))
}

debug_getSuitLength<-function(sui)
{
  for ( i in 1:length(sui)){
    print(dim(sui[[i]])[1])
  }
}

getAllocatedDT<-function(suitablityMat,transitionMat,fromClass,neighbourWt=NA,demand=NA,restrictSpatialMigration=NA,AllowedClassMigration=NA
                         ,conversionOrder='TP',classAllocationOrder=NA,method='text'){
  
  # 
  # assign("gsuitablityMat", suitablityMat, envir = globalenv())
  # assign("gtransitionMat", transitionMat, envir = globalenv())
  # assign("gfromClass", fromClass, envir = globalenv())
  # assign("gneighbourWt", neighbourWt, envir = globalenv())
  
  
  #SuitablityMat<-gives the fitted values as per drivres sorted in probablity order
  #transitionMat<-gives the trasition distribution to different class
  #fromClass<-represents the data table with the coloumn for each class representing the currently allocated class from which new prediction needs  to be achived
  #demand<-Demand is matrix representing the total allocatin which needs to be done to each class. The class order needs to be maintained in order of column order in "fromClass"
  #restrictSpatialMigration<-represents the vulnaribility which states that a class in matrix will remain in the same state work as policy {1 represents no change possible 0 means change possible}. It represents class inertia
  #AllowedMigration<-represents the conversion matrix[numberofClass,numberofClass] which is a kind of policy to state that which class migration is possible
  #conversionOrder<-givers the priority of conversion from class to class . low values means less liekly change and high means more likely change.In absense it will be genrated from TP
  #classAllocationOrder<-gives the priority of allocation to different class.By default it allocates is class name order 
  #method<-gives the progress report for the processing
  #                                             
  #                 }
  
  classNames<-names(fromClass)
  
  outputAllocation<-fromClass   #Creating output grid to be allocated as per the current grids 
  outputAllocation[]<-NA          #Initialising it to be non allocated
  noOfAllocationGrid<-dim(fromClass)[1]
  noOfClass<-dim(fromClass)[2]
  outputAllocation<-as.data.table(cbind(id=seq(1,noOfAllocationGrid),outputAllocation))
  if(sum(is.na(classAllocationOrder))!=0){
    classAllocationOrderMat<-seq(1:noOfClass)
  } else {
    classAllocationOrderMat<-classAllocationOrder
  }
  #Class allocation order is either based on the TP or by the modeller given order.
  if(sum(conversionOrder=='TP')!=0){
    conversionOrderMat<-matrix(NA,noOfClass,noOfClass)   #Create a allocatin Order
    for ( i in 1:noOfClass){
      x<-rbind(as.numeric(rownames(table(sort(transitionMat[i,])))),table(sort(transitionMat[i,]))) 
      s<-1;
      #print(x)
      for (k in length(x[1,]):1){ #Reverse order of transition
        p<-as.numeric(x[1,k])
        #print(transitionMat[i,])
        conversionOrderMat[i,][transitionMat[i,]==p]<-seq(s,length.out=x[2,k])
        s<-s+as.numeric(x[2,k])
      }    
    }
    
    #conversionOrderMat=apply(transitionMat,2,function(x){ seq(1,noOfClass)[rank(x,ties.method="first")]})
    #conversionOrderMat=noOfClass+1-conversionOrderMat
    tmpconversionOrderMat=conversionOrderMat
      for (i in seq(1,noOfClass)){tmpconversionOrderMat[i,]=sort(conversionOrderMat[i,],index.return=TRUE)$ix}
      conversionOrderMat=tmpconversionOrderMat
  }else {
    conversionOrderMat<-conversionOrder
  }

  #Restricting any allowed class migrtion fromclass to toclass. 0 is restricted 1 is not resetricted.
  if(is.na(AllowedClassMigration)){ 
    AllowedClassMigrationMat<-matrix(1,nrow=noOfClass,ncol=noOfClass) #All Migration are allowed
  } else {
    AllowedClassMigrationMat<-AllowedClassMigration
  }
  
  #Modify the conversion Order based on the allowed migration 
  conversionOrderMat[which(AllowedClassMigrationMat!=1)]<-0 #Replace all the convesion matrix with  0 to indicate these conversion are not possible
  
  #Modify the Allocation which is restricated due to inertia
  if(sum(is.na(restrictSpatialMigration))!=0){ 
    restrictSpatialMigrationTM=rep(0,noOfClass)
  }else{
    restrictSpatialMigrationTM<-restrictSpatialMigration
  }
  
  if(sum(is.na(demand))!=0){
    demandVec<-colSums(transitionMat)
  }else {
    demandVec<-demand
  }
  
  newTM<-transitionMat
  if(exists("debugValue") && debugValue>=1 ){
    print("getAllocatedDT:Step1:restrictSpatialMigrationTM")
  }
  ###1 Remove all the restricted or inertia based migration. 
  for( i in 1:noOfClass) {
    tmp<-fromClass[,classNames[i],with=F] #Step1: Select the column from which allocation needs to be done. 
    #Step2: Find out all currently allocated grid and order them in increasing order of suitability
    probableGridToAllocate<-neighbourWt[[i]][weight==0]#[order(weight,decreasing = FALSE)]
    probableGridToAllocate<-suitablityMat[[i]][id %in% probableGridToAllocate[,id]][order(weight,decreasing = FALSE)]
    
    if(restrictSpatialMigrationTM[i]>0) 
    {    #1 Means no spatial migration is possible
      #Find out the based on inertia how much will be retained
      noOfGridRestrictedBasedOnInertia=floor(restrictSpatialMigrationTM[i]*newTM[i,i]);
      #Check do we have sufficient grid to allocated and meet the allocatin requirement as per the new demand distribution
      if(noOfGridRestrictedBasedOnInertia>0) #Atleast one grid has to be there which is not migrated
      { 
        #Check do we have sufficient grid to allocated and meet the allocatin requirement as per the new demand distribution
        if(dim(probableGridToAllocate)[1]<noOfGridRestrictedBasedOnInertia) { #if demand to reatain is less than available grid 
          noOfGridRestrictedBasedOnInertia=dim(probableGridToAllocate)[1]#allocate all of them to retain maximum possible retention grid based on previously allocated no grid 
        }
        gridId<-probableGridToAllocate[1:noOfGridRestrictedBasedOnInertia]$id
        suitablityMat<-removeAllocateGrid(suitablityMat,gridId,paste("SM",i))
        neighbourWt<-removeAllocateGrid(neighbourWt,gridId,paste("WT",i))
        demandVec[i]<-demandVec[i]-noOfGridRestrictedBasedOnInertia
        newTM[i,i]<-newTM[i,i]-noOfGridRestrictedBasedOnInertia
        outputAllocation[gridId,classNames[i]]<-1
        outputAllocation[gridId,classNames[-i]]<-0
        
        #if(newTM[i,i]==0) {
        # conversionOrderMat[i,i]<-0
        #}
      }
    }
  }
  
  if(exists("debugValue") && debugValue>=1 ){
    print("getAllocatedDT:Step2:Competitive Allocation")
    print(conversionOrderMat)
    print(classAllocationOrderMat)
  }
  ####2 Start allocation from to the class.
  #print(classAllocationOrderMat)

  for(i in 1:noOfClass){
    classFromAllocate<-classAllocationOrderMat[i]
    for(j in 1:noOfClass){
      classToAllocate<-conversionOrderMat[classFromAllocate,j]
      if(newTM[classFromAllocate,classToAllocate]!=0)   #Is there any allocation needed
      {
        gridId=NA;
        #print(paste("classFromAllocate:",classFromAllocate,"classToAllocate:",classToAllocate))
        if(newTM[classFromAllocate,classToAllocate]>0)
        {
          if(exists("debugValue") && debugValue>=1 ){  
            print(paste("Allocating:FromClass:ToClass:",classFromAllocate,classToAllocate,sep=":",collapse = "X"))
          }
          #get Previously allocated location of classFromAllocate class and classToAllocate class
          gridToClass=neighbourWt[[classToAllocate]][weight!=0]#[order(id,decreasing = FALSE)]
          gridFromClass=neighbourWt[[classFromAllocate]][weight==0]#[order(id,decreasing = FALSE)]
          
          #Filtere Out already allocated Grids     
          outputstatus<-cbind(outputAllocation[,1],rowSums(outputAllocation[,-1]))[V2==1]
          
          #Filter Grids which are not occupied and which can be taken the fromclass
          probableGridToAllocate<-gridToClass[!(id %in% outputstatus$id)]#[order(weight,decreasing = FALSE)]
          

          #Filter Grids which are not occupied and which are demanded by the toclass
          probableGridFromToAllocate<-gridFromClass[!(id %in% outputstatus$id)]#[order(weight,decreasing = FALSE)]
          
          if(classToAllocate!=classFromAllocate){
            
            probableGridToAllocate<-probableGridToAllocate[(id %in% gridFromClass$id)]
            probableGridFromToAllocate<-probableGridFromToAllocate[(id %in% gridToClass$id)]
            
            probableGridToAllocate<-suitablityMat[[classToAllocate]][id %in% probableGridToAllocate[,id]][order(weight,decreasing = TRUE)]
            probableGridFromToAllocate<-suitablityMat[[classFromAllocate]][id %in% probableGridFromToAllocate[,id]][order(weight,decreasing = TRUE)]
            
            #The previously allocated fromclass grid are the probable grid where allocation of toclass will take place 
            #If there is more than the grid fullfilling the criteria do the proritisation either based on suitability or neighbour weight
            if((dim(probableGridFromToAllocate)[1]> newTM[classFromAllocate,classToAllocate]) & 
               (dim(probableGridToAllocate)[1]> newTM[classFromAllocate,classToAllocate])){ #If there are more grid available for allocation than prorities it on the basis of
                if((newTM[classFromAllocate,classToAllocate]/sum(newTM[classToAllocate,])>.05) ){           
                cutoffSuitablity<-suitablityMat[[classToAllocate]][suitablityMat[[classToAllocate]]$id %in% probableGridToAllocate[,id]][order(weight,decreasing = TRUE)][newTM[classFromAllocate,classToAllocate]]
                cutoffSuitablity<-cutoffSuitablity$weight
                
                probableGridToAllocate1=dim(probableGridToAllocate)
                probableGridToAllocate<-suitablityMat[[classToAllocate]][ suitablityMat[[classToAllocate]]$weight >= cutoffSuitablity & id %in% probableGridToAllocate[,id]][order(weight,decreasing = TRUE)]    
    
                probableGridToAllocate2=dim(probableGridToAllocate)
 
                probableGridToAllocate<-neighbourWt[[classToAllocate]][neighbourWt[[classToAllocate]]$id %in% probableGridToAllocate[,id]][order(weight,decreasing = FALSE)]
                probableGridToAllocate3=dim(probableGridToAllocate)
                #print(paste(probableGridToAllocate1,probableGridToAllocate2,probableGridToAllocate3,sep = "XXSuit"))
                }else{
                cutoffSuitablity<-neighbourWt[[classToAllocate]][neighbourWt[[classToAllocate]]$id %in% probableGridToAllocate$id][order(weight,decreasing = FALSE)][newTM[classFromAllocate,classToAllocate]]
                cutoffSuitablity<-cutoffSuitablity$weight
                
                probableGridToAllocate1=dim(probableGridToAllocate)
                probableGridToAllocate<-neighbourWt[[classToAllocate]][neighbourWt[[classToAllocate]]$weight >= cutoffSuitablity & neighbourWt[[classToAllocate]]$id %in% probableGridToAllocate$id][order(weight,decreasing = TRUE)]
                probableGridToAllocate2=dim(probableGridToAllocate)
     
                probableGridToAllocate<-suitablityMat[[classToAllocate]][suitablityMat[[classToAllocate]]$id %in% probableGridToAllocate][order(weight,decreasing = TRUE)]
                probableGridToAllocate3=dim(probableGridToAllocate)
                #print(paste(probableGridToAllocate1,probableGridToAllocate2,probableGridToAllocate3,sep = "XXNeigh"))
                }
            } else { #There is not enough fromgrid
                  if(newTM[classFromAllocate,classToAllocate] !=0 & dim(probableGridFromToAllocate)[1]< newTM[classFromAllocate,classToAllocate]){
                    probableGridToAllocate<-probableGridFromToAllocate #Whatever is there do allocation accordingly
                    warning(paste("Not Enough Probable Grid from Class",classFromAllocate,"for to Class",classToAllocate))
                  }else if(newTM[classFromAllocate,classToAllocate] !=0 & (dim(probableGridToAllocate)[1]<= newTM[classFromAllocate,classToAllocate])){
                    probableGridToAllocate<-probableGridToAllocate
                    warning(paste("Not Enough Probable Grid to Class",classFromAllocate,"for to Class",classToAllocate))
                  }
            }
          } else { #If class for allocation from and to both are same
            probableGridFromToAllocate<-suitablityMat[[classFromAllocate]][id %in% probableGridFromToAllocate[,id]][order(weight,decreasing = TRUE)]
            probableGridToAllocate<-probableGridFromToAllocate
          }
          
          if(dim(probableGridToAllocate)[1]>=newTM[classFromAllocate,classToAllocate]){
            noOfAvailablegrid=newTM[classFromAllocate,classToAllocate]
          }else{
            noOfAvailablegrid=dim(probableGridToAllocate)[1]
          }
          
          gridId<-probableGridToAllocate[1:noOfAvailablegrid,]$id  #XXX 
          
          # Allocates the most eligible grid
          newTM[classFromAllocate,classToAllocate]<-newTM[classFromAllocate,classToAllocate]-noOfAvailablegrid
          outputAllocation[gridId,classNames[classToAllocate]]<-1
          outputAllocation[gridId,classNames[-classToAllocate]]<-0
          #conversionOrderMat[classFromAllocate,classToAllocate]<-0
          demandVec[classToAllocate]<-demandVec[classToAllocate]-newTM[classFromAllocate,classToAllocate]                         
          
          
          #Remove All allocate grid is removed from
          #suitability matrix and weight matrix removal
          suitablityMat<-removeAllocateGrid(suitablityMat,gridId,paste("SM",classFromAllocate))
          neighbourWt<-removeAllocateGrid(neighbourWt,gridId,paste("NW",classFromAllocate))
        }
      }
    }
  }

  if(sum(newTM)!=0){
    #outputstatus<-cbind(outputAllocation[,1],rowSums(outputAllocation[,-1]))[V2==1]
    #Filter Grids which are not occupied and which can be taken the fromclass
    if(exists("debugValue") && debugValue>=1 ){
      print("getAllocatedDT:Step3: Trying Optimum Competitive Allocation")
    }  
    #print(newTM)
    outputstatus<-cbind(outputAllocation[,1],rowSums(outputAllocation[,-1]))[V2==1]
    for(classFromAllocate in 1:noOfClass){
    gridFromClass=neighbourWt[[classFromAllocate]][weight==0]#[order(id,decreasing = FALSE)]
    probableGridFromToAllocate<-gridFromClass[!(id %in% outputstatus$id)]#[order(weight,decreasing = FALSE)]
    #print(dim(probableGridFromToAllocate))
    if(rowSums(newTM)[classFromAllocate]>0 && dim(probableGridFromToAllocate)[1]==rowSums(newTM)[classFromAllocate]){
      gridId<-probableGridFromToAllocate$id  
      outputAllocation[gridId,classNames[classFromAllocate]]<-1
      outputAllocation[gridId,classNames[-classFromAllocate]]<-0
      warning("allocation cost  Increasing")
    }
    }

    if(exists("debugValue") && debugValue>=1 ){
      print("getAllocatedDT:Step4:Retreat Competitive Allocation")
      outputstatus<-cbind(outputAllocation[,1],rowSums(outputAllocation[,-1]))[V2==1]
      print(dim(outputstatus))
    } 
    #print(classAllocationOrderMat)
    for(i in 1:noOfClass){
      classFromAllocate<-classAllocationOrderMat[i]#which(classAllocationOrderMat==i) 
      for(j in 1:noOfClass){
        classToAllocate<-j#conversionOrderMat[classFromAllocate,j]#which(conversionOrderMat[classFromAllocate,]==j)  #Get the current highest order of allocation which needs to be done    
        if(newTM[classFromAllocate,classToAllocate]!=0)   #Is there any allocation needed
        {
          gridId=NA;
          subsetl<-c("id",classNames[classFromAllocate])
          outputstatus<-outputAllocation[,..subsetl]
          names(outputstatus)<-c("id","allocated")
          outputstatus<-outputstatus[allocated==1,]
          gridId=outputstatus$id;
          
          #print(paste("classFromAllocate:",classFromAllocate,"classToAllocate:",classToAllocate))
          if(newTM[classFromAllocate,classToAllocate]>0)
          {
            if(exists("debugValue") && debugValue>=1 ){  
              print(paste("Allocating:FromClass:ToClass:",classFromAllocate,classToAllocate,sep=":",collapse = "X"))
            }
            #get Previously allocated location of classFromAllocate class and classToAllocate class
            gridToClass=neighbourWt[[classToAllocate]][weight!=0]#[order(id,decreasing = FALSE)]
            gridFromClass=neighbourWt[[classFromAllocate]][weight==0]#[order(id,decreasing = FALSE)]
            
            
            #Filter Grids which are not occupied and which can be taken the fromclass
            probableGridToAllocate<-gridToClass[(id %in% gridId)]#[order(weight,decreasing = FALSE)]
            
            
            #Filter Grids which are not occupied and which are demanded by the toclass
            probableGridFromToAllocate<-gridFromClass[(id %in% gridId)]#[order(weight,decreasing = FALSE)]
            
            if(classToAllocate!=classFromAllocate){
              
              #probableGridToAllocate<-probableGridToAllocate[(id %in% gridFromClass$id)]
              #probableGridFromToAllocate<-probableGridFromToAllocate[(id %in% gridToClass$id)]
              
              probableGridToAllocate<-suitablityMat[[classToAllocate]][id %in% probableGridToAllocate[,id]][order(weight,decreasing = TRUE)]
              probableGridFromToAllocate<-suitablityMat[[classFromAllocate]][id %in% probableGridFromToAllocate[,id]][order(weight,decreasing = TRUE)]
              
              #The previously allocated fromclass grid are the probable grid where allocation of toclass will take place 
              #If there is more than the grid fullfilling the criteria do the proritisation either based on suitability or neighbour weight
              if((dim(probableGridFromToAllocate)[1]> newTM[classFromAllocate,classToAllocate]) & 
                 (newTM[classFromAllocate,classToAllocate]!=0&&dim(probableGridToAllocate)[1]> newTM[classFromAllocate,classToAllocate])){ #If there are more grid available for allocation than prorities it on the basis of
          
                  cutoffSuitablity<-suitablityMat[[classToAllocate]][suitablityMat[[classToAllocate]]$id %in% probableGridToAllocate[,id]][order(weight,decreasing = TRUE)][newTM[classFromAllocate,classToAllocate]]
                  cutoffSuitablity<-cutoffSuitablity$weight
                  
                  #probableGridToAllocate1=dim(probableGridToAllocate)
                  probableGridToAllocate<-suitablityMat[[classToAllocate]][ suitablityMat[[classToAllocate]]$weight >= cutoffSuitablity & id %in% probableGridToAllocate[,id]][order(weight,decreasing = TRUE)]    
                  
                  probableGridToAllocate2=dim(probableGridToAllocate)
                  
                  probableGridToAllocate<-neighbourWt[[classToAllocate]][neighbourWt[[classToAllocate]]$id %in% probableGridToAllocate[,id]][order(weight,decreasing = FALSE)]
                  #probableGridToAllocate3=dim(probableGridToAllocate)
                  #print(paste(probableGridToAllocate1,probableGridToAllocate2,probableGridToAllocate3,sep = "XXSuit"))
              } else { #There is not enough fromgrid
                  warning(paste("Increasing Cost no help",classFromAllocate,"for to Class",classToAllocate))
              }
            } else { #If class for allocation from and to both are same
              probableGridFromToAllocate<-suitablityMat[[classFromAllocate]][id %in% probableGridFromToAllocate[,id]][order(weight,decreasing = TRUE)]
              probableGridToAllocate<-probableGridFromToAllocate
            }
            
            if(dim(probableGridToAllocate)[1]>=newTM[classFromAllocate,classToAllocate]){
              noOfAvailablegrid=newTM[classFromAllocate,classToAllocate]
            }else{
              noOfAvailablegrid=dim(probableGridToAllocate)[1]
            }
            
            gridId<-probableGridToAllocate[1:noOfAvailablegrid,]$id  #XXX 
            
            # Allocates the most eligible grid
            newTM[classFromAllocate,classToAllocate]<-newTM[classFromAllocate,classToAllocate]-noOfAvailablegrid
            outputAllocation[gridId,classNames[classToAllocate]]<-1
            outputAllocation[gridId,classNames[-classToAllocate]]<-0
            #conversionOrderMat[classFromAllocate,classToAllocate]<-0
            demandVec[classToAllocate]<-demandVec[classToAllocate]-newTM[classFromAllocate,classToAllocate]                         
            
            #Remove All allocate grid is removed from
            #suitability matrix and weight matrix removal
            suitablityMat<-removeAllocateGrid(suitablityMat,gridId,paste("SM",i))
            neighbourWt<-removeAllocateGrid(neighbourWt,gridId,paste("WT",i))
          }
        }
      }
    }
  }
  # assign("goutputAllocation",outputAllocation,envir = globalenv())
  #print('HELL')
  #print(sum(newTM))
  outputAllocation<-outputAllocation[,classNames,with=F]
  return(outputAllocation)
}




removeAllocateGrid<-function(suitablity,grids,info,decreasing=TRUE){
  
  for (i in 1:length(suitablity)){
    #print(paste("Line 172","Removing",info,length(suitablity[[i]]$id)))
    suitablity[[i]]<-suitablity[[i]][!(id %in% grids)][order(weight,decreasing = decreasing)]
  }
  
  return (suitablity)
}

constructSuitablity<-function(model,driverForWhichToPredict,dt2=NA,method="NotIncludeCurrentClass"){
  library(data.table)    
  suitablity=list()
  if(method=="NotIncludeCurrentClass"){
    drvWithId<-as.data.table(cbind(id=seq(1,dim(driverForWhichToPredict)[1]),TD1=driverForWhichToPredict))
  }else{
    drvWithId<-as.data.table(cbind(id=seq(1,dim(driverForWhichToPredict)[1]),T1=dt2,TD1=driverForWhichToPredict))
  }
  drvWithId<-drvWithId[rowSums(driverForWhichToPredict,na.rm=TRUE)!=0]
  for(i in 1:length(model)){
    model.class<-class(model[[i]])
    if(is.element(model.class[1],"lm")){
      print("OK LM")
      suitablity[[i]]<-as.data.table(cbind(drvWithId[,c("id"),with=F],weight=predict(model[[i]],data=drvWithId,type='response', progress='text')))[order(weight,decreasing = TRUE)]
    }else if(is.element(model.class[1],"glm")) {
      print("OK GLM")
      suitablity[[i]]<-as.data.table(cbind(drvWithId[,c("id"),with=F],weight=predict(model[[i]],data=drvWithId,type='response', progress='text')))[order(weight,decreasing = TRUE)]    
      #Here predict is usning the argument to get the probability (response)
    }else if(is.element(model.class[2],"nnet")){
      print("OK NNET")
      suitablity[[i]]<-as.data.table(cbind(drvWithId[,c("id"),with=F],weight=predict(model[[i]],data=drvWithId,type='probs', progress='text')))[order(weight,decreasing = TRUE)]    
    }else if(is.element(model.class[2],"randomForest")){
      print("OK RF")
      predictedValue=predict(model[[i]],newdata=drvWithId,type='prob', progress='text')[,2] #Second column probablity 
      suitablity<-as.data.table(cbind(drvWithId[,c("id"),with=F],weight=predictedValue))[order(weight,decreasing = TRUE)]    
    }else{
      print("Error in modeling")
    }
  }
  
  return(suitablity)
}

constructSM<-function(i,model,drvWithId){
  library(data.table)  
  print(paste("Start-constructSM",i,sep="-"))
  model.class<-class(model[[i]])
  if(is.element(model.class[1],"lm")){
    print("OK LM")
    suitablity<-as.data.table(cbind(drvWithId[,c("id"),with=F],weight=predict(model[[i]],data=drvWithId,type='response', progress='text')))[order(weight,decreasing = TRUE)]
  }else if(is.element(model.class[1],"glm")) {
    print("OK GLM")
    suitablity<-as.data.table(cbind(drvWithId[,c("id"),with=F],weight=predict(model[[i]],data=drvWithId,type='response', progress='text')))[order(weight,decreasing = TRUE)]    
    #Here predict is usning the argument to get the probability (response)
  }else if(is.element(model.class[2],"nnet")){
    print("OK NNET")
    suitablity<-as.data.table(cbind(drvWithId[,c("id"),with=F],weight=predict(model[[i]],data=drvWithId,type='prob', progress='text')))[order(weight,decreasing = TRUE)]    
  }else if(is.element(model.class[2],"randomForest")){
    print("OK RF")
    predictedValue=predict(model[[i]],newdata=drvWithId,type='prob', progress='text')[,2] #Second column probablity 
    suitablity<-as.data.table(cbind(drvWithId[,c("id"),with=F],weight=predictedValue))[order(weight,decreasing = TRUE)]    
  }else if(is.element(model.class[2],"svm")){
    print("OK SVM")
    predictedValue=predict(model[[i]],newdata=drvWithId,probability = TRUE)
    predictedValue=attr(predictedValue, "probabilities")[,2] #Second column probablity of presence
    suitablity<-as.data.table(cbind(drvWithId[,c("id"),with=F],weight=predictedValue))[order(weight,decreasing = TRUE)]    
  }else{
    print("Error in modeling")
  }
  print(paste("constructSM:forClass",i,sep=":"))
  suitablity
}

unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

ParallelconstructSuitablity<-function(model,driverForWhichToPredict,dt2=NA,method="NotIncludeCurrentClass"){
  library(data.table)    
  suitability=list()
  library(parallel)#library(multicore)
  library(doParallel)
  numberofClass=length(model)
  if(method=="NotIncludeCurrentClass"){
    drvWithId<-as.data.table(cbind(id=seq(1,dim(driverForWhichToPredict)[1]),TD1=driverForWhichToPredict))
  }else{
    drvWithId<-as.data.table(cbind(id=seq(1,dim(driverForWhichToPredict)[1]),T1=dt2,TD1=driverForWhichToPredict))
  }
  drvWithId<-drvWithId[rowSums(driverForWhichToPredict,na.rm=TRUE)!=0]
  withCore=detectCores();
  if( withCore >numberofClass){
    withCore=numberofClass
  }
  myCluster=registerDoParallel(core=withCore)
  #for(i in 1:length(model)){
  if(exists("debugValue") && debugValue>=2){      
    print("ParallelconstructSuitablity::constructSM:Called")
  }
  suitability<-foreach ( i = 1:length(model),.export=c("constructSM"),.packages = c("data.table","randomForest")) %dopar%{
    constructSM(i,model,drvWithId)
  }
  unregister()
  #stopCluster(myCluster)
  
  
  return(suitability)
}


getMasked<-function(rasterFile,maskFile=NA,aoiFile=NA)
{
  #rm(list=ls())
  library(raster)
  #library(maptools)
  if(exists("debugValue") && debugValue>=2){      
    print("getMasked:Entry");
  }
  if(is.na(maskFile) && is.na(aoiFile)){
    return(stack(rasterFile))
  }
  r1<-stack(rasterFile)
  if(!is.na(aoiFile))
  {
    if(tolower(extension(aoiFile))==".shp"){
      roi<-readShapePoly(aoiFile)
    }
    if(tolower(extension(aoiFile))!=".shp"){
      roi<-raster(aoiFile)
    }
    
    if(is(roi,"SpatialPolygonsDataFrame")){
      q<-mask(r1,roi)
      r1[is.na(q)]<-NA
      rm(q)
    }
    if(is(roi,'RasterLayer')){
      tmp<-raster()
      tmp[]<-NA
      q<-merge(tmp,roi,overlap=FALSE )      
      tmp<-raster(r1)
      tmp[!is.na(q)]<-NA
      r1[is.na(q)]<-NA
      #r1[!is.na(tmp)]<-NA
      rm(tmp)
    }
    rm(roi)
  }
  if(!is.na(maskFile))
  {
    if(tolower(extension(maskFile))==".shp"){
      msk<-readShapePoly(maskFile)
    }
    if(tolower(extension(maskFile))!=".shp"){
      msk<-raster(maskFile)
    }
    
    #roi<-readShapePoly(aoiFile)
    #readOGR(maskFile,layer='mask')
    
    if(is(msk,"SpatialPolygonsDataFrame")){
      q<-mask(r1,msk)
      r1[!is.na(q)]<-NA
    }
    if(is(msk,'RasterLayer')){
      tmp<-raster(r1)
      tmp[]<-NA
      q<-merge(tmp,msk)
      rm(tmp)
      r1[!is.na(q)]<-NA
      
    }
    rm(q,msk)
  }
  if(exists("debugValue") && debugValue>=2){      
    print("getMasked:Exit");
  }
  return(r1)
}

getRaster<-function(file,with.single.layer=FALSE,with.na.value=NA,maskFile=NA,aoiFile=NA){
  library(raster);
  if(exists("debugValue") && debugValue>=2){      
    print("getRaster:Entry");
  }
  if(with.single.layer){
    rst<-getMasked(file,maskFile,aoiFile)#raster(file)
    rst<-singleToMultiBand(rst,with.na.value)
    
  } else {
    rst<-getMasked(file,maskFile,aoiFile)#stack(file)    
  }
  if(exists("debugValue") && debugValue>=2){      
    print("getRaster:Exit");
  }
  return(rst)
}

getDataTable<-function(file,with.single.layer=FALSE,withNAvalue=NA,withClassName=NA,maskFile=NA,aoiFile=NA){
  library(raster);
  library(data.table)
  if(exists("debugValue") && debugValue>=2){      
    print("getDataTable:Entry");
  }
  rst<-getRaster(file,with.single.layer,withNAvalue,maskFile,aoiFile)
  if(!with.single.layer) 
  {
    classlayers<-c(paste("class",seq(1:length(names(rst))),sep="_"))
  } else {
    classlayers=names(rst)
  }
  if(sum(is.na(withClassName))==0) {classlayers<-withClassName;}    
  names(rst)<-classlayers;
  #print(names(rst))
  if(exists("debugValue") && debugValue>=2){      
    print("getDataTable:Exit");
  }
  return(data.table(rst[]))
}


getNumberOfClass<-function(File,with.single.layer=NA,withNAvalue=NA){
  t1<-getRaster(File,with.single.layer,withNAvalue)    
  return(dim(t1)[3])
}

getnoOfCell<-function(File,with.single.layer=NA,withNAvalue=NA){
  t1<-getRaster(File,with.single.layer,withNAvalue)    
  return(dim(t1)[1]*dim(t1)[2])
}


prepareData<-function(T1File,T2File,driverfiles,classFile.with.single.layer=FALSE,withNAvalue=NA,withClassName=NA,with.driver.name=NA,maskFile=NA,aoiFile=NA){
  library(raster);library(data.table);#library(pscl);library('memisc')
  
  if(sum(is.na(withClassName))!=0) {classlayers<-c(paste("class",seq(1:length(driverfiles)),sep="_"))}  
  if(! classFile.with.single.layer) {classlayers=withClassName}    
  
  if(sum(is.na(with.driver.name))!=0) {driverlayers<-c(paste("driver",seq(1:length(driverfiles)),sep="_"))} else { driverlayers<-with.driver.name}
  
  numberofClass<-getNumberOfClass(T1File,classFile.with.single.layer,withNAvalue)
  noOfCell<-getnoOfCell(T1File,classFile.with.single.layer,withNAvalue)
  datatableT1<-getDataTable(T1File,classFile.with.single.layer,withNAvalue,withClassName,maskFile,aoiFile)
  datatableT2<-getDataTable(T2File,classFile.with.single.layer,withNAvalue,withClassName,maskFile,aoiFile)
  driverdatatable<-getDataTable(driverfiles,FALSE,withNAvalue,driverlayers,maskFile,aoiFile)
  #driverdatatable<-getDataTable(driverfiles,FALSE,withNAvalue)
  
  data<-as.data.table(cbind(id=seq(1,noOfCell),time1=datatableT1,time2=datatableT2,driver=driverdatatable))
  return(data)
}

fitModel<-function(dt1,drvt1,dt2,modelType,method="NotIncludeCurrentClass"){
  library(data.table)
  if(exists("debugValue") && debugValue>=2 ){      
    print("fitModel:Entry");
  }
  #Here assumtion is the first column is grid number. then all the columnwise class values for T1 then columnwise class values of T2 then all the drivers
  data<-data.table(cbind(id=seq(1:dim(dt1)[1]),T1=dt1,TD1=drvt1)) #adding the grid number along with all class and drivers.
  model.fit<-list()
  dataColumnNames<-colnames(data)
  numberofClass<-dim(dt1)[2]
  numberofDrivers<-dim(drvt1)[2]
  currentClassesToFit<-dataColumnNames[seq(2,length.out=numberofClass)]
  driversToChoose<-dataColumnNames[seq(2+numberofClass,length.out=numberofDrivers)]
  numberofRows<-dim(dt1)[1]
  futureClassesToFit=NA
  ####
  dta<-subset(data[rowSums(subset(data,select=c(classesToFit)),na.rm=TRUE)!=0],select=c(dataColumnNames[1],classesToFit,driversToChoose))  
  #select those grid which has been allocated to any class and then get all the class allocation with the drivers.
  
  #as.data.table(cbind(subset(dta,select=c(dataColumnNames[1])),as.data.frame(predict.model)))
  
  if(modelType=="logistic"){
    dta[is.na(dta)]<-0  #Replace non allocated grid for particular class to indicate absent
    for( i in 1:numberofClass){
      frml<-as.formula(paste(classesToFit[i]," ~ ", paste(driversToChoose, collapse= "+")))
      model.fit[[i]]<-glm(frml,family = binomial(), data = dta,na.action='na.omit')
      #model.fit[[i]]<-glm(formula=as.formula(paste(classesToFit[i]," ~ ", paste(driversToChoose, collapse= "+"))),family = binomial, data = dta,na.action='na.omit')
      
    }
    
    #model1.fit<-glm(time1.class_1 ~ driver.slope + driver.distoueb + driver.disttostream, family = binomial, data = dta,na.action='na.omit')
  } else if (modelType=="nn") {
    
  } else {
    
    for( i in 1:numberofClass){
      frml<-as.formula(paste(classesToFit[i]," ~ ", paste(driversToChoose, collapse= "+")))
      model.fit[[i]]<-lm(frml,family = binomial(), data = dta,na.action='na.omit')
      
    }
    
    #model1.fit<-lm(time1.class_1 ~driver.slope + driver.distoueb + driver.disttostream,  data = dta,na.action='na.omit')
    
  }
  names(model.fit)<-classesToFit
  if(exists("debugValue") && debugValue>=2 ){      
    print("fitModel:Exit");
  }
  return(model.fit)
  
}

biglmupdate<-function(dataset,inmodel,frml,start,end) {   #TO be used by bigglm fit inside fitModelSeparately
  if (start==1) {
    model <<- bigglm(formula(frml),family = binomial(), data = dataset[start:end,],na.action='na.omit')
  }
  else {
    model <<- update(inmodel, data = dataset[start:end,],na.action='na.omit')
  }
  return (model)
}

doLogisticFit<-function(dta,currentClassesToFit,currentDriversToChoose,method="NotIncludeCurrentClass")
{
  withCore=1;#detectCores();
  model.fit<-list()
  numberofClass=length(currentClassesToFit)
  if(method=="NotIncludeCurrentClass"){
    dta[is.na(dta)]<-0  #Replace non allocated grid for particular class to indicate absent
    if(withCore==1){
      for( i in 1:numberofClass){
        frml<-paste(currentClassesToFit[i],"~", paste(currentDriversToChoose, collapse= "+"))
        model.fit[[i]]<-glm(formula(frml),family = binomial, data = dta,na.action='na.omit')
      }
    } else {
      library(parallel)#library(multicore)
      library(doParallel)
      myCluster=registerDoParallel(core=withCore)#RegisterDoMC(withCore) #registerDoParallel(c1)
      model.fit<-foreach ( i = 1:numberofClass) %dopar%{
        frml<-paste(currentClassesToFit[i]," ~ ", paste(currentDriversToChoose, collapse= "+"))
        glm(formula(frml),family = binomial, data = dta,na.action='na.omit')
      }
      unregister()
      #stopCluster(myCluster)
    }
  }else{
    
  }
  names(model.fit)<-currentClassesToFit
  return(model.fit)
}

doLinearFit<-function(dta,currentClassesToFit,currentDriversToChoose,method="NotIncludeCurrentClass"){
  numberofClass=length(currentClassesToFit)
  model.fit<-list()
  dta[is.na(dta)]<-0  #Replace non allocated grid for particular class to indicate absent
  if(method=="NotIncludeCurrentClass"){
    for( i in 1:numberofClass){
      frml<-as.formula(paste(currentClassesToFit[i]," ~ ", paste(currentDriversToChoose, collapse= "+")))
      model.fit[[i]]<-lm(frml, data = dta,na.action='na.omit')
    }
  }else{
    library(parallel)#library(multicore)
    library(doParallel)
    myCluster=registerDoParallel(core=withCore)#RegisterDoMC(withCore) #registerDoParallel(c1)
    model.fit<-foreach ( i = 1:numberofClass) %dopar%{
      frml<-paste(currentClassesToFit[i]," ~ ", paste(currentDriversToChoose, collapse= "+"))
      frml<-as.formula(paste(currentClassesToFit[i]," ~ ", paste(currentDriversToChoose, collapse= "+")))
      lm(formula(frml),data = dta,na.action='na.omit')
    }
    unregister()
    #stopCluster(myCluster)
  }
  names(model.fit)<-currentClassesToFit
  return(model.fit)
}

doRandomFit<-function(dta,currentClassesToFit,currentDriversToChoose,futureClassesToChoose,method="NotIncludeCurrentClass"){
  print("doRandomFit:735")
  #memory.limit(100000)
  library(randomForest)
  withCore=1#detectCores();
  numberofClass=length(currentClassesToFit)
  numberofDrivers=length(currentDriversToChoose)
  model.fit=list()
  if(method=="NotIncludeCurrentClass"){
    dta[is.na(dta)]<-0 #Replace non allocated grid for particular class to indicate absent
    if(withCore==1){
      for( i in 1:numberofClass){
        #print("doRandomFit:602")
        library(randomForest)
        frml<-paste(currentClassesToFit[i],"~", paste(currentDriversToChoose, collapse= "+"))
        #model.fit[[i]]<-randomForest(as.formula(frml), data = dta, ntree=2000, nodesize=50,na.action=na.omit,proximity=TRUE,importance=TRUE,type="classification")
        model.fit[[i]]<-randomForest(as.formula(frml), data = dta, nodesize=1,na.action=na.omit,proximity=TRUE,importance=TRUE,type="classification")
        
      }
    } else {
      library(parallel)#library(multicore)
      library(doParallel)
      myCluster=registerDoParallel(core=withCore)#RegisterDoMC(withCore) #registerDoParallel(c1)
      model.fit<-foreach ( i = 1:numberofClass) %dopar%{
        library(randomForest)
        frml<-paste(currentClassesToFit[i],"~", paste(currentDriversToChoose, collapse= "+"))
        #randomForest(as.formula(frml), data = dta, nodesize=1,na.action=na.omit,proximity=TRUE,importance=TRUE,type="classification")
        randomForest(as.formula(frml), data = dta, nodesize=1,na.action=na.omit,proximity=TRUE,importance=TRUE,type="classification")
        
      }
      unregister()
      #stopCluster(myCluster)
    }
  }else{
    dta[is.na(dta)]<-0  
    if(withCore==1){
      #print("doRandomFit:770")
      for( i in 1:numberofClass){
        frml<-paste(futureClassesToChoose[i],"~", paste(paste(currentClassesToFit,collapse= "+"),paste(currentDriversToChoose, collapse= "+"), sep= "+"))
        
        #model.fit[[i]]=neuralnet(as.formula(frml),data = dta, hidden=numberofDrivers+numberofClass,err.fct="sse",algorithm="backprop" ,learningrate=0.01, linear.output=FALSE,likelihood=T)
        model.fit[[i]]<-randomForest(as.formula(frml), data = dta, nodesize=1,na.action=na.omit,proximity=TRUE,importance=TRUE,type="classification")
      }
    } else {
      myCluster=registerDoParallel(core=withCore)#RegisterDoMC(withCore) #registerDoParallel(c1)
      model.fit<-foreach ( i = 1:numberofClass) %dopar%{
        frml<-paste(futureClassesToChoose[i],"~", paste(paste(currentClassesToFit,collapse= "+"),paste(currentDriversToChoose, collapse= "+"), sep= "+"))
        randomForest(as.formula(frml), data = dta, nodesize=1,na.action=na.omit,proximity=TRUE,importance=TRUE,type="classification")
      }
      unregister()
      #stopCluster(myCluster)
    }
  }
  names(model.fit)<-currentClassesToFit
  return(model.fit)
}

doNeuralFit<-function(dta,currentClassesToFit,currentDriversToChoose,futureClassesToChoose,method="NotIncludeCurrentClass"){
  
  #library(neuralnet)
  library(nnet)
  withCore=1#detectCores();
  numberofClass=length(currentClassesToFit)
  numberofDrivers=length(currentDriversToChoose)
  model.fit=list()
  if(method=="NotIncludeCurrentClass"){
    dta[is.na(dta)]<-0 #Replace non allocated grid for particular class to indicate absent
    if(withCore==1){
      for( i in 1:numberofClass){
        library(nnet)
        frml<-paste(currentClassesToFit[i],"~", paste(currentDriversToChoose, collapse= "+"))
        #model.fit[[i]]=neuralnet(as.formula(frml),data = dta, hidden=numberofDrivers+numberofClass,err.fct="sse",algorithm="backprop" ,learningrate=0.01, linear.output=FALSE,likelihood=T)
        model.fit[[i]]<-multinom(as.formula(frml),data = dta)
      }
    } else {
      library(parallel)#library(multicore)
      library(doParallel)
      myCluster=registerDoParallel(core=withCore)#RegisterDoMC(withCore) #registerDoParallel(c1)
      model.fit<-foreach ( i = 1:numberofClass) %dopar%{
        library(nnet)
        frml<-paste(currentClassesToFit[i],"~", paste(currentDriversToChoose, collapse= "+"))
        #neuralnet(as.formula(frml),data = dta, hidden=numberofDrivers+numberofClass,err.fct="sse",algorithm="backprop" ,learningrate=0.01, linear.output=FALSE,likelihood=T)
        multinom(as.formula(frml),data = dta)
      }
      unregister()
      #stopCluster(myCluster)
    }
  }else{
    dta[is.na(dta)]<-0  #Replace non allocated grid for particular class to indicate absent
    if(withCore==1){
      for( i in 1:numberofClass){
        frml<-paste(futureClassesToChoose[i],"~", paste(paste(currentClassesToFit,collapse= "+"),paste(currentDriversToChoose, collapse= "+"), sep= "+"))
        
        #model.fit[[i]]=neuralnet(as.formula(frml),data = dta, hidden=numberofDrivers+numberofClass,err.fct="sse",algorithm="backprop" ,learningrate=0.01, linear.output=FALSE,likelihood=T)
        model.fit[[i]]<-multinom(as.formula(frml),data = dta)
      }
    } else {
      myCluster=registerDoParallel(core=withCore)#RegisterDoMC(withCore) #registerDoParallel(c1)
      model.fit<-foreach ( i = 1:numberofClass) %dopar%{
        frml<-paste(futureClassesToChoose[i],"~", paste(paste(currentClassesToFit,collapse= "+"),paste(currentDriversToChoose, collapse= "+"), sep= "+"))
        #neuralnet(as.formula(frml),data = dta, hidden=numberofDrivers+numberofClass,err.fct="sse",algorithm="backprop" ,learningrate=0.01, linear.output=FALSE,likelihood=T)
        multinom(as.formula(frml),data = dta)
      }
      unregister()
      #stopCluster(myCluster)
    }
  }
  names(model.fit)<-currentClassesToFit
  return(model.fit)
}

doSVMFit<-function(dta,currentClassesToFit,currentDriversToChoose,futureClassesToChoose,method="NotIncludeCurrentClass"){
  
  #library(neuralnet)
  library(e1071)
  withCore=1#detectCores();
  numberofClass=length(currentClassesToFit)
  numberofDrivers=length(currentDriversToChoose)
  model.fit=list()
  if(method=="NotIncludeCurrentClass"){
    dta[is.na(dta)]<-0 #Replace non allocated grid for particular class to indicate absent
    if(withCore==1){
      for( i in 1:numberofClass){
        library(e1071)
        frml<-paste(currentClassesToFit[i],"~", paste(currentDriversToChoose, collapse= "+"))
        #model.fit[[i]]=neuralnet(as.formula(frml),data = dta, hidden=numberofDrivers+numberofClass,err.fct="sse",algorithm="backprop" ,learningrate=0.01, linear.output=FALSE,likelihood=T)
        model.fit[[i]]<-svm(as.formula(frml),data = dta,probability=TRUE)
      }
    } else {
      library(parallel)#library(multicore)
      library(doParallel)
      myCluster=registerDoParallel(core=withCore)#RegisterDoMC(withCore) #registerDoParallel(c1)
      model.fit<-foreach ( i = 1:numberofClass) %dopar%{
        library(e1071)
        frml<-paste(currentClassesToFit[i],"~", paste(currentDriversToChoose, collapse= "+"))
        #neuralnet(as.formula(frml),data = dta, hidden=numberofDrivers+numberofClass,err.fct="sse",algorithm="backprop" ,learningrate=0.01, linear.output=FALSE,likelihood=T)
        svm(as.formula(frml),data = dta,,probability=TRUE)
      }
      unregister()
      #stopCluster(myCluster)
    }
  }else{
    dta[is.na(dta)]<-0  #Replace non allocated grid for particular class to indicate absent
    if(withCore==1){
      for( i in 1:numberofClass){
        frml<-paste(futureClassesToChoose[i],"~", paste(paste(currentClassesToFit,collapse= "+"),paste(currentDriversToChoose, collapse= "+"), sep= "+"))
        
        #model.fit[[i]]=neuralnet(as.formula(frml),data = dta, hidden=numberofDrivers+numberofClass,err.fct="sse",algorithm="backprop" ,learningrate=0.01, linear.output=FALSE,likelihood=T)
        model.fit[[i]]<-svm(as.formula(frml),data = dta)
      }
    } else {
      myCluster=registerDoParallel(core=withCore)#RegisterDoMC(withCore) #registerDoParallel(c1)
      model.fit<-foreach ( i = 1:numberofClass) %dopar%{
        frml<-paste(futureClassesToChoose[i],"~", paste(paste(currentClassesToFit,collapse= "+"),paste(currentDriversToChoose, collapse= "+"), sep= "+"))
        #neuralnet(as.formula(frml),data = dta, hidden=numberofDrivers+numberofClass,err.fct="sse",algorithm="backprop" ,learningrate=0.01, linear.output=FALSE,likelihood=T)
        svm(as.formula(frml),data = dta,probability=TRUE)
      }
      unregister()
      #stopCluster(myCluster)
    }
  }
  names(model.fit)<-currentClassesToFit
  return(model.fit)
}


# doNeuralFit<-function(dta,classToFit,driversToChoose,dt2,method="Change")
# {
# numberofRows=dim(dta)[1]
# dataColumnNames<-colnames(dta)
# numberofClass=dim(dt2)[2]
# numberofDrivers<-length(driversToChoose)
# numberofColums<-length(dataColumnNames)
# currentClassesToFit<-dataColumnNames[seq(1,numberofClass)];
# futureClassesToFit+numberofDrivers),
# print(modelType)
# if(method=="Change"){
# dt1=subset(dta,select=c(classesToFit))
# dt2=subset(dta,select=c(classesToFit2))
# tmpDt1<-rowSums(dt1*matrix(rep(seq(1,numberofClass),numberofRows),nrow=numberofRows,ncol=numberofClass,byrow=T),na.rm=T)
# tmpDt1[tmpDt1==0]<-NA
# tmpDt2<-rowSums(dt2*matrix(rep(seq(1,numberofClass),numberofRows),nrow=numberofRows,ncol=numberofClass,byrow=T),na.rm=T)
# tmpDt2[tmpDt2==0]<-NA
# ChangeDt<-tmpDt1*(numberofClass+1)+tmpDt2
# ChangeDt<-data.table(dat,Change=ChangeDt[])
# rm(tmpDt1)
# rm(tmpDt2)
# print (names(dta))
# print ((dta))
# #Idx<-ChangeDt[ChangeDt <= (i+1)*(numberofClass+1)] >= i*(numberofClass+1)
# #Idx[is.na(Idx)]<-FALSE
# for(i in 1:numberofClass){
# #ChangeDt[Change==i*(numberofClass+1)+i,Change]<-i*(numberofClass+1)
# ChangeDt[ChangeDt$Change==(i*(numberofClass+1)+i),"Change"]<-i
# Idx1<-ChangeDt[ChangeDt$Change > i*(numberofClass+1)]$id
# #Idx[is.na(Idx)]<-FALSE
# Idx2<-ChangeDt[ChangeDt$Change < (i+1)*(numberofClass+1)]$id
# Id=Idx1[Idx1 %in% Idx2]
# ChangeDt[Id,"Change"]<-i*(numberofClass+1)
# }
# ChangeDt<-na.omit(ChangeDt)
# dataColumnNames<-colnames(ChangeDt)
# classesToFit<-dataColumnNames[2]
# driversToChoose<-dataColumnNames[seq(3,length(dataColumnNames)-1)]
# frml<-paste(classesToFit," ~ ", paste(driversToChoose, collapse= "+"))
# model.fit<-neuralnet(formula(frml),data = ChangeDt, hidden=10,err.fct="sse",algorithm="backprop" ,learningrate=0.01, linear.output=FALSE,likelihood=T)
# }
# }

fitSameModelToAll<-function(dt1,drvt1,dt2,modelType='regression',method="NotIncludeCurrentClass"){
  library(data.table)
  #Here assumtion is the first column is grid number. then all the columnwise class values for T1 then columnwise class values of T2 then all the drivers
  data<-data.table(cbind(id=seq(1:dim(dt1)[1]),T1=dt1,TD1=drvt1)) #adding the grid number along with all class and drivers.
  if(exists("debugValue") && debugValue>=2 ){      
    print("fitSameModelToAll:Entry");
  }
  model.fit<-list()
  dataColumnNames<-colnames(data)
  numberofClass<-dim(dt1)[2]
  numberofDrivers<-dim(drvt1)[2]
  currentClassesToFit<-dataColumnNames[seq(2,length.out=numberofClass)]
  driversToChoose<-dataColumnNames[seq(2+numberofClass,length.out=numberofDrivers)]
  numberofRows<-dim(dt1)[1]
  futureClassesToFit=NA
  if(method=="IncludeCurrentClass"){  #If current classes needs to be weighted against the future class
    data<-data.table(data,TD2=dt2)
    dataColumnNames<-colnames(data)
    futureClassesToFit<-dataColumnNames[seq(2+numberofClass+numberofDrivers,length.out=numberofClass)]
    dta<-subset(data[rowSums(subset(data,select=c(currentClassesToFit)),na.rm=TRUE)!=0],select=c(dataColumnNames[1],currentClassesToFit,driversToChoose,futureClassesToFit))  
  }else{
    dta<-subset(data[rowSums(subset(data,select=c(currentClassesToFit)),na.rm=TRUE)!=0],select=c(dataColumnNames[1],currentClassesToFit,driversToChoose))  
  }
  #select those grid which has been allocated to any class and then get all the class allocation with the drivers.
  if(exists("debugValue") && debugValue>=1 ){      
    print(paste("fitSameModelToAll:modelType",modelType,collapse = ";",sep = ":"));
  }
  
  if(modelType=="logistic"){
    model.fit<-doLogisticFit(dta,currentClassesToFit,driversToChoose,method)    
  } else if (modelType=="nnet") {
    model.fit<-doNeuralFit(dta,currentClassesToFit,driversToChoose,futureClassesToFit,method)
  } else if (modelType=="randomForest") {
    model.fit<-doRandomFit(dta,currentClassesToFit,driversToChoose,futureClassesToFit,method)
  }  else if (modelType=="svm") {
    model.fit<-doSVMFit(dta,currentClassesToFit,driversToChoose,futureClassesToFit,method)
  } else {
    model.fit<-doLinearFit(dta,currentClassesToFit,driversToChoose,method)
  }
  names(model.fit)<-currentClassesToFit
  return(model.fit)
}

fitModelSeparately<-function(dt1,drvt1,dt2,modelType,method="NotIncludeCurrentClass",modelformula=NA)
{
  library(data.table)
  #Here assumtion is the first column is grid number. then all the columnwise class values for T1 then columnwise class values of T2 then all the drivers
  data<-data.table(cbind(id=seq(1:dim(dt1)[1]),T1=dt1,TD1=drvt1)) #adding the grid number along with all class and drivers.
  if(exists("debugValue") && debugValue>=1 ){      
    print("fitModelSeparately:Entry");
  }
  withCore=1#detectCores();
  model.fit<-list()
  dataColumnNames<-colnames(data)
  numberofClass<-dim(dt1)[2]
  numberofDrivers<-dim(drvt1)[2]
  currentClassesToFit<-dataColumnNames[seq(2,length.out=numberofClass)]
  driversToChoose<-dataColumnNames[seq(2+numberofClass,length.out=numberofDrivers)]
  numberofRows<-dim(dt1)[1]
  futureClassesToFit=NA
  if(method=="IncludeCurrentClass"){  #If current classes needs to be weighted against the future class
    data<-data.table(data,TD2=dt2)
    dataColumnNames<-colnames(data)
    futureClassesToFit<-dataColumnNames[seq(2+numberofClass+numberofDrivers,length.out=numberofClass)]
    dta<-subset(data[rowSums(subset(data,select=c(currentClassesToFit)),na.rm=TRUE)!=0],select=c(dataColumnNames[1],currentClassesToFit,driversToChoose,futureClassesToFit))  
  }else{
    dta<-subset(data[rowSums(subset(data,select=c(currentClassesToFit)),na.rm=TRUE)!=0],select=c(dataColumnNames[1],currentClassesToFit,driversToChoose))  
  }
  #select those grid which has been allocated to any class and then get all the class allocation with the drivers.
  if(exists("debugValue") && debugValue>=1 ){      
    print(paste("fitModelSeparately:modelFormula",modelformula,collapse = ";",sep = ":"));
  }
  if(method=="NotIncludeCurrentClass"){
    dta[is.na(dta)]<-0  #Replace non allocated grid for particular class to indicate absent
    if(withCore==1){
      for( i in 1:numberofClass){
        frml<-paste(currentClassesToFit[i],"~", paste(driversToChoose, collapse= "+"))
        if(!sum(is.na(modelformula))){
        frml<-modelformula[[i]]  
        }
        if(modelType[i]=="logistic") {
          print("fitModelSeparately:Seriallogistic")
          model.fit[[i]]<-glm(formula(frml),family = binomial, data = dta,na.action='na.omit')
        }else if (modelType[i]=="nnet") {
          library(nnet)
          print("fitModelSeparately:Serialnnet")
          model.fit[[i]]<-multinom(as.formula(frml),data = dta)
        }else if(modelType[i]=="regression") {
          print("fitModelSeparately:Serialregression")
          model.fit[[i]]<-lm(frml, data = dta,na.action='na.omit')
        }else if(modelType[i]=="randomForest"){
          print("fitModelSeparately:SerialrandomForest")
          library(randomForest)
          cols<-trim(gsub('(.*)~(.*)','\\1',frml))
          frml<-gsub('(.*)~(.*)','factor(\\1)~\\2',frml)
          dta[, (cols) := lapply(.SD, factor), .SDcols = cols]
          #model.fit[[i]]<-randomForest(as.formula(frml), data = dta, ntree=100, proximity=T,na.action=na.omit,importance=TRUE,type="classification")
          #model.fit[[i]]<-randomForest(as.formula(frml), data = dta, nodesize=1,na.action=na.omit,importance=TRUE,type="classification")
          model.fit[[i]]<-randomForest(as.formula(frml), data = dta, na.action=na.omit,importance=TRUE,type="regression")
          #print(model.fit[[i]])                    
        }else if(modelType[i]=="svm"){
          print("fitModelSeparately:SerialSVM")
          library(e1071)
          cols<-trim(gsub('(.*)~(.*)','\\1',frml))
          frml<-gsub('(.*)~(.*)','factor(\\1)~\\2',frml)
          dta[, (cols) := lapply(.SD, factor), .SDcols = cols]
          #svm(as.formula(frml), data = dta, nodesize=1,na.action=na.omit,proximity=TRUE,importance=TRUE,type="classification")
          print(frml)
          model.fit[[i]]<-svm(as.formula(frml), data = dta,probability=TRUE)
        }
      }
    } else {
      library(parallel)#library(multicore)
      library(doParallel)
      myCluster=registerDoParallel(core=withCore)#RegisterDoMC(withCore) #registerDoParallel(c1)
      model.fit<-foreach ( i = 1:numberofClass) %dopar%{
        frml<-paste(currentClassesToFit[i],"~", paste(driversToChoose, collapse= "+"))
        if(!sum(is.na(modelformula))){
        frml<-modelformula[[i]]  
        }
        
        if(modelType[i]=="logistic") {
          print("fitModelSeparately:Parallellogistic")
          glm(formula(frml),family = binomial, data = dta,na.action='na.omit')
        }else if (modelType[i]=="nnet") {
          print("fitModelSeparately:Parallelnnet")
          library(nnet)
          multinom(as.formula(frml),data = dta)
        }else if(modelType[i]=="regression") {
          print("fitModelSeparately:Parallellregression")
          lm(frml, data = dta,na.action='na.omit')
        }else if(modelType[i]=="randomForest"){
          print("fitModelSeparately:ParallelrandomForest")
          library(randomForest)
          cols<-trim(gsub('(.*)~(.*)','\\1',frml))
          frml<-gsub('(.*)~(.*)','factor(\\1)~\\2',frml)
          dta[, (cols) := lapply(.SD, factor), .SDcols = cols]
          randomForest(as.formula(frml), data = dta, nodesize=1,na.action=na.omit,proximity=TRUE,importance=TRUE,type="classification")
        }else if(modelType[i]=="svm"){
          print("fitModelSeparately:ParallelSVM")
          library(e1071)
          cols<-trim(gsub('(.*)~(.*)','\\1',frml))
          frml<-gsub('(.*)~(.*)','factor(\\1)~\\2',frml)
          dta[, (cols) := lapply(.SD, factor), .SDcols = cols]
          #svm(as.formula(frml), data = dta, nodesize=1,na.action=na.omit,proximity=TRUE,importance=TRUE,type="classification")
          svm(as.formula(frml), data = dta,probability=TRUE)
        }
      }
      unregister()
      #stopCluster(myCluster)
    }
  }else{
    print("fitModelSeparately:No Model Exists yet")
    abort("fitModelSeparately:No Model Exists yet")
  } 
  if(exists("debugValue") && debugValue>=1 ){ 
    print("fitModelSeparately:Done");
  }
  print("1111:fitModelSeparately")
  print(currentClassesToFit)
  names(model.fit)<-currentClassesToFit
  return(model.fit)
}

getTp<-function(t){  #T is transition matrix 
  return(t/sum(rowSums(t)))
}

getYearlyMatrix<-function(FCM,Steps=1,foryear=1){
  FPM=round(FCM/rowSums(FCM),6)
  FPM=removeTruncitionErrorFromMatrix(rep(1,dim(FPM)[1]),FPM,"row")
  noOfClass=dim(FPM)[1]
  
  error=matrix(1,noOfClass,noOfClass)
  eps=matrix(.0000000000000001,noOfClass,noOfClass)
  iteration=1
  Qhat=matrix(0,noOfClass,noOfClass)
  Phat=FPM
  diag(Phat)=0
  diag(Phat)=-rowSums(Phat)
  Identity=diag(1,noOfClass,noOfClass)
  while(sum(error>eps) && iteration<100){
    oldQhat=Qhat
    if((iteration %% 2)==1) {
      Qhat=Qhat+Phat/iteration
    }
    else {
      Qhat=Qhat-Phat/iteration
    }
    
    Qhat<-removeTruncitionErrorFromMatrix(rep(0,dim(Qhat)[1]),Qhat,"row")
    Phat = Phat %*% Phat
    iteration=iteration+1
    error=oldQhat-Qhat
  }
  
    if(exists("debugValue") && debugValue>=2 ){
      if(iteration<100){
      print(paste("getYearlyMatrix:GeneratorMatrixonverged:iteration",iteration,"eps",sum(error),sep=":"))
      }else{
        
      }
    }

  Qhat<-Qhat/Steps
  Qhat<-removeTruncitionErrorFromMatrix(rep(0,dim(Qhat)[1]),Qhat,"row")
  nontransientIndex=which(diag(Qhat)==0)
  G_i=abs(diag(Qhat))+rowSums(maxMAB(Qhat-diag(Qhat),matrix(0,noOfClass,noOfClass)))
  B_i=rowSums(maxMAB(-1*(Qhat-diag(Qhat)),matrix(0,noOfClass,noOfClass)))
  QM<-Qhat
  QM[QM<0]=0
  diag(QM)=diag(Qhat)
  #for(i in 1:noOfClass){
  for(i in 1:noOfClass){
    if(G_i[i]>0){
      QM[i,i]=QM[i,i]-B_i[i]*abs(QM[i,i])/G_i[i]
    }
  }
  library(expm)
  FinalYearlyFCM<-expm(QM*foryear)*rowSums(FCM)
  FinalYearlyFCM<-round(colSums(FCM)*FCM/rowSums(FCM),digits=0)
  FinalYearlyFCM<-removeTruncitionErrorFromMatrix(rowSums(FCM),FinalYearlyFCM,"row")
  return(FinalYearlyFCM)
}

createTM<-function(dft1,dft2,withNAvalue='NA'){
  if(typeof(dft1)=="character"){
    library(data.table)
    dft1<-getDataTable(dft1,TRUE,withNAvalue)
  }
  
  if(typeof(dft2)=="character"){
    library(data.table)
    dft2<-getDataTable(dft2,TRUE,withNAvalue)
  }
  
  numberofClass<-dim(dft1)[2]
  numberofRows<-dim(dft1)[1]
  
  dft1[is.na(dft1)]<-0
  dft2[is.na(dft2)]<-0
  #Assuming both are of same dimension
  tp<-dft1*matrix(rep(seq(1,numberofClass),numberofRows),nrow=numberofRows,ncol=numberofClass,byrow=T)*(numberofClass+1)+dft2*matrix(rep(seq        
                                                                                                                                         (1,numberofClass),numberofRows),nrow=numberofRows,ncol=numberofClass,byrow=T)
  tp<-rowSums(tp)
  tp<-table(tp)
  dummytp<-matrix(rep(seq(1,numberofClass),numberofClass),nrow=numberofClass,ncol=numberofClass,byrow=F)*(numberofClass+1)+matrix(rep(seq
                                                                                                                                      (1,numberofClass),numberofClass),nrow=numberofClass,ncol=numberofClass,byrow=T)
  actualtp<-matrix(0,nrow=numberofClass,ncol=numberofClass)
  tpindex<-as.numeric(rownames(as.matrix(tp))) 
  for (k in 1:length(tp)){
    for( i in 1:numberofClass){
      for (j in 1:numberofClass) {
        if(dummytp[i,j]==tpindex[k])
          actualtp[i,j]=tp[k]                
      }
    }
  }
  
  if(exists("debugValue") && debugValue>=1){   
    print("getNewTM:futureTM")
    print("createTM:CurrentTM")
    print(actualtp)
  }
  
  return(actualtp)   
}

removeTruncitionErrorFromMatrix<-function(referenceVector,M,by){
  noOfCols=dim(M)[1]
  if(by=="row"){
    trunkDifference=referenceVector-rowSums(M)
    for(i in 1:noOfCols){
      maxVectLoc<-ifelse(sum(M[,i]>=0)!=0,which.max(M[i,]),which.min(M[i,]))
      if(trunkDifference[i]==referenceVector[i]) {maxVectLoc=i}
      M[i,maxVectLoc]=M[i,maxVectLoc]+trunkDifference[i]
    }
  } 
  if(by=="col"){
    trunkDifference=referenceVector-colSums(M)
    for(i in 1:noOfCols){
      maxVectLoc<-ifelse(sum(M[,i]>=0)!=0,which.max(M[,i]),which.min(M[,i]))
      if(trunkDifference[i]==referenceVector[i]) {maxVectLoc=i}
      M[maxVectLoc,i]=M[maxVectLoc,i]+trunkDifference[i]
      
    }
  }
  
  #print(sum(M))
  #print(sum(referenceVector))
  M[which.max(M)]=M[which.max(M)]+sum(referenceVector)-sum(M)
  return(M)
}

minMAB<-function(A,B){
  C=matrix(0,dim(A)[1],dim(B)[2])
  for(i in 1:length(A)){C[i]=min(A[i],B[i])}
  return(C)
}

maxMAB<-function(A,B){
  C=matrix(0,dim(A)[1],dim(B)[2])
  for(i in 1:length(A)){C[i]=max(A[i],B[i])}
  return(C)
}

getNewTM<-function(TM,demand=NA,SpatialMigrationRestriction=NA){
 
  #print(demand)
  noOfClass<-sqrt(length(TM))
  if(sum(is.na(SpatialMigrationRestriction))!=0){SpatialMigrationRestriction=rep(0,noOfClass)}
  if(sum(is.na(demand))!=0)
  {futureTM<-(round(colSums(TM)*TM/rowSums(TM),digits=0))
  #futureTM<-removeTruncitionErrorFromMatrix(colSums(TM),futureTM,"row")
  }
  else 
  {futureTM<-getNewTMwithDemand(TM,demand,SpatialMigrationRestriction)}
  futureTM<-removeTruncitionErrorFromMatrix(colSums(TM),futureTM,"row")
  PixelDistributionAtT1=colSums(TM)
  PixelDistributionAtT2=rowSums(futureTM)
  trunkDifference=PixelDistributionAtT1-PixelDistributionAtT2
  for(i in 1:noOfClass){
    futureTM[i,i]=futureTM[i,i]+trunkDifference[i]
  }
  
  if(exists("debugValue") && debugValue>=1){   
    print("getNewTM:futureTM")
    print("futureTM")
    print(futureTM)
    print("futuredemand")
    print(colSums(futureTM))
    print("TotalPixel")
    print(sum(futureTM))
  }
  
  # futureTM<-matrix(
  #   c(1227,0,0,0,0,0,0,0,16,
  #     84,34674,0,337,0,0,24,0,99,
  #     0,51,11275,10,0,0,0,0,2,
  #     0,30,0,1259,0,0,0,0,0,
  #     0,0,0,0,5,0,0,0,0,
  #     0,0,0,0,0,61,0,0,0,
  #     4,36,0,40,0,0,3808,0,0,
  #     0,60,0,0,0,0,0,1458,57,
  #     1,894,31,116,0,10,3,104,3820)
  #   ,nrow=9,byrow=TRUE)
  return(round(futureTM,digits=0))
}

getNewTMwithDemand<-function(TM,demand=NA,SpatialMigrationRestriction=NA){
  if(sum(is.na(demand))!=0){
    futureTM<-(round(colSums(TM)*TM/rowSums(TM),digits=0))
    FEM<-futureTM
  }else{
    goal<-t((t(TM)/colSums(TM)*demand))
    FEM<-(round(colSums(TM)*TM/rowSums(TM),digits=0))
    
    currOM<-demand-colSums(FEM)
    #print(currOM!=0)
    while(sum(currOM!=0)){
      FEM=round(FEM/rowSums(FEM)*colSums(TM),digits=0)
      FEM=removeTruncitionErrorFromMatrix(colSums(TM),FEM,"row")
      FEM=round(t(t(FEM)/colSums(FEM)*demand),digits=0)
      FEM=removeTruncitionErrorFromMatrix(demand,FEM,"col")
      currOM<-demand-colSums(FEM)
      #print(currOM)
    }
  }
  #print(FEM)
  return(FEM)
}


singleToMultiBand<-function(rasterVar,with.na='NA'){
  library(raster)
  library(data.table);
  if(exists("debugValue") && debugValue>=2){      
    print("singleToMultiBand:Entry");
  }
  tmpraster<-raster(rasterVar);
  tmpraster[]<- 1:ncell(tmpraster)
  #plot(rasterVar,main="Line 733")
  tmp<-data.table(rasterVar[])
  tmpClassCode<-rownames(table(tmp))
  if(!is.na(with.na)){
    tmpClassCode[tmpClassCode==with.na]<-NA
  }
  
  tmpClassCode<-tmpClassCode[!is.na(tmpClassCode)]
  setnames(tmp,'cls')
  tmp<-as.data.table(cbind(cell=1:ncell(tmpraster),tmp,value=1))
  
  st<-stack(tmpraster)
  st<-subs(st,subset(tmp[cls==tmpClassCode[1]],select =  c('cell','value')))
  #layerNames(st)[1]<-classCode[1]
  
  len<-length(tmpClassCode)
  
  for (x in c(2:len)) {
    r<-raster(tmpraster)
    r[]<- 1:ncell(tmpraster)
    r<-subs(r,subset(tmp[cls==tmpClassCode[x]],select = c('cell','value')))
    st<-stack(st,r)                 #?? Can we do memory optimisation here
  }
  #layerNames(st)[x]<-classCode[x]
  
  names(st)<-c(paste("class",tmpClassCode,sep="_")) 
  if(exists("debugValue") && debugValue>=2){      
    print("singleToMultiBand:Entry");
  }
  return(st)
}

createMutliRaster<-function(classCode,inputRaster,inputDataTable)
{
  inputRaster[]<- 1:ncell(inputRaster)
  st<-stack(inputRaster)
  st<-subs(st,subset(inputDataTable[luClass==classCode[1]],select = names(inputDataTable)[-2]))
  #layerNames(st)[1]<-classCode[1]
  
  len<-length(classCode)
  
  for (x in c(2:len)) {
    r<-raster(inputRaster)
    r[]<- 1:ncell(inputRaster)
    r<-subs(r,subset(inputDataTable[luClass==classCode[x]],select = names(inputDataTable)[-2]))
    st<-stack(st,r)                 #?? Can we do memory optimisation here
    #layerNames(st)[x]<-classCode[x]
    
  }
  names(st)<-classCode
  return(st)
}


rasterise<- function(shpFile,layerName,Gridsize=1000,option="fraction")
{    
  #library(maptools);
  library(rgeos);library(rgdal);library(raster);library(data.table)
  #library(rgdal) # Loads SP package by default
  #Debug
  inputShape<-readOGR(shpFile, layerName) # Creates a SpatialPolygonsDataFrame class (sp)
  #inputShape<-readShapeSpatial(system.file(shpFile))  #read shape file as spatial object
  inputClass<-as.character(inputShape@data$IGBP_CODE)  #TODO Can it be done without hardcode
  #DEBUG
  debug<-as.character(inputShape@data$IGBP_POLY)
  inputClassCode<-as.character(levels(inputShape@data$IGBP_CODE) )
  #create raster grid
  outputRaster<-raster()
  projection(outputRaster)<-projection(inputShape)
  extent(outputRaster)<-extent(inputShape)
  res(outputRaster)<-Gridsize
  names(outputRaster)="grid"
  outputRaster[]<- 1:ncell(outputRaster)
  if(option=="fraction") {
    system.time(ex<-(extract(outputRaster,inputShape,weight=TRUE))) #TODO can we do speedup here
  } else {
    system.time(ex<-(extract(outputRaster,inputShape,weight=FALSE))) 
  }
  
  
  #function to be used to append the LULC class at the last of the column after converting it into dataframe
  
  makeDtbl<- function(x,y,z) {
    if(!is.null(x))
      if(option=="fraction"){
        cbind(x,y,z)
      } else {
        cbind(value=x,weight=1,y,z)
      }
  }
  
  system.time(kc<-mapply(makeDtbl,ex[1:length(ex)],inputClass[1:length(ex)],debug[1:length(ex)]))
  #DEBUG
  #lapply(ex, write, "ex.txt", append=T)
  
  system.time(dtbl <- data.table(do.call("rbind", kc)))
  
  d=transform(dtbl, value = as.numeric(levels(value)[value]), weight= as.numeric(levels(weight)[weight]),y=as.character(y),z=as.character(z)); dtbl=d;rm(d) #TODO get rid of this code
  tableCol<-c(c1="cell",c3="weight",c2="luClass",c4="IGBP_POLY") ;setnames(dtbl,tableCol);setkey(dtbl,cell,luClass,IGBP_POLY)
  #Datatable will be of header with (cell,weight,luClass) corresponding to the (gridnumber,fraction,LULC Class)
  #Aggregate for the cases where two similar class boundary may intersect to a cell
  #DEBUG
  #write.table(dtbl,file="dtbl1.csv",sep=",")
  
  dtblagg<-dtbl[,sum(weight),by="cell,luClass"] #TODO Can it be done without hardcode
  #rm(dtbl) 
  tableCol<-c(c1="cell",c2="luClass",c3="weight") ;setnames(dtblagg,tableCol)
  setkey(dtblagg,cell,luClass)  #TODO Can it be done without hardcode 
  #DEBUG
  #write.table(dtblagg,file="dtblagg2.csv",sep=",")
  
  errorpoly=unique(dtbl[subset(dtblagg[weight>1],select='cell')]$IGBP_POLY)
  write.table(errorpoly,file=paste(layerName,"errorpoly.csv",sep="-"),sep=",")
  mr<-createMutliRaster(inputClassCode,outputRaster,dtblagg)    
  return(mr)
  
  
}

kappa <- function(CM) {
  #Pass the Change Matrix or Transition Matrix.  
  cmx<-as.matrix(CM)
  #try to convert a vector to a square matrix
  if (ncol(cmx) == 1)
    cmx<-matrix(cmx, byrow=TRUE, nrow=sqrt(nrow(cmx)))
  nr<-nrow(cmx); nc<-ncol(cmx)
  if (nr != nc)
  { print("Error: matrix is not square"); break }
  n<-sum(cmx)
  d<-diag(cmx); dsum<-sum(d); th1<-dsum/n
  th1v<-((th1*(1-th1))/n)
  csum<-apply(cmx,2,sum); rsum<-apply(cmx,1,sum)
  ua<-d/rsum; pa<-d/csum
  th2 <- sum(rsum*csum) / n^2; kh <- (th1-th2)/(1-th2)
  th3 <- sum( (csum + rsum) * d ) / n^2;
  th4 <- 0; for (i in 1:nr) for (j in 1:nc)
    th4 <- th4 + (cmx[i,j] * ((csum[i] + rsum[j])^2));
  th4 <- th4 / n^3;
  th1c <- 1 - th1; th2c <- 1 - th2;
  khv <- 1/n *
    (
      ( ( th1 * th1c ) / th2c^2 )
      + ( ( 2 * th1c * ((2*th1*th2) - th3) ) / th2c^3 )
      + ( ( th1c^2 * ( th4 - (4 * th2^2 ) ) ) / th2c^4 )
    )
  #per-class kappa, user's accuracy...
  p <- cmx/n; uap <- apply(p,1,sum); pap <- apply(p,2,sum); dp<-diag(p);
  kpu <- (dp/uap - pap)/(1 - pap);
  #...and its variance
  t1 <- uap-dp; t2 <- (pap*uap)-dp; t3 <- dp*(1 - uap - pap + dp);
  kpuv <- ( (t1/(uap^3 * (1-pap)^3)) * ((t1*t2) + t3) )/n;
  #per-class kappa, producers reliability...
  kpp <- (dp/pap - uap)/(1 - uap);
  #...and its variance
  t1 <- (pap-dp);
  kppv <- ( (t1/(pap^3 * (1-uap)^3)) * ((t1*t2) + t3) )/n;
  #return all statistics as a list
  list(sum.n=n, sum.naive=th1, sum.var=th1v, sum.kappa=kh, sum.kvar=khv,
       user.naive=ua, prod.naive=pa,
       user.kappa=kpu, user.kvar=kpuv, prod.kappa=kpp, prod.kvar=kppv)
}


getUnbiasedEstimatePropertionMatrix<-function(cm){
  #un biased estimate of propertion estimation from confusion matrix.
  rsum<-rowSums(cm);
  tsum<-sum(cm)
  rsum<-round(rsum/tsum,7)
  rsum[which.max(rsum)]=rsum[which.max(rsum)]+round(1-sum(rsum),7)
  
  csum<-colSums(cm);
  csum<-round(csum/tsum,7)
  csum[which.max(csum)]= csum[which.max(csum)]+round(1-sum(csum),7)
  
  
  pmx<-t(round(t(cm)/csum,7))
  deltacsum<-round(1-rowSums(t(cm)/csum),7) #Truncation correction
  maxrowindex<-sort(apply(pmx, 2, which.max))
  maxindex<-(maxrowindex-1)*noLC+maxrowindex
  pmx[maxindex]<- pmx[maxindex]+deltacsum
  return(list(pmx=pmx,cmargin=csum,rowmargin=rsum))
  #maxpmxid<-which.max(pmx)
  #tpsum<-sum(colSums(pmx)); if(tpsum!=noLC) pmx[maxpmxid]=max(pmx)+1-tpsum #Truncation correction
  #rsumpmx<-rowSums(pmx);csumpmx<-colSums(pmx);
}

genratePraportionMatrix<-function(CM){
  
  
  #Pass the Change Matrix or Transition Matrix.  
  cmx1<-as.matrix(CM)
  #try to convert a vector to a square matrix
  if (ncol(cmx1) == 1)
    cmx1<-matrix(cmx1, byrow=TRUE, nrow=sqrt(nrow(cmx1)))
  nr<-nrow(cmx1); nc<-ncol(cmx1)
  if (nr != nc)
  { print("Error: matrix is not square"); break }
  noLC<-nc
  
  pmx<-cmx1/sum(cmx1)
  pmx<-round(pmx,7)
  #Trunction Error Correction 
  pmx[which.max(pmx)]=pmx[which.max(pmx)]+round(1-sum(pmx),7) 
  return(pmx)
}

nMatrixto2Class<-function(m,index){
  p=m[index,index]
  rsum<-sum(m[index,])
  csum<-sum(m[,index])
  pdash=sum(m[-index,-index])
  return(matrix(c(p,csum-p,rsum-p,pdash),ncol=2,nrow=2))  
}
getSAGivenOTrans<-function(simulationFile,actualFile,baseFile){
  library(raster)
  baseLC<-raster(baseFile)
  actualLC<-raster(actualFile)
  simulatedLC<-raster(simulationFile)
  
  
  baseFrequecy= freq(baseLC,useNA='no')
  numberofClass<-dim(baseFrequecy)[1]
  classmaxID=baseFrequecy[,1]
  
  SAGivenOLCpe<-array(0, dim = c(numberofClass, numberofClass),dimnames = list(classmaxID,classmaxID))
  cSAGivenOLCpe<-array(0, dim = c(numberofClass, numberofClass),dimnames = list(classmaxID,classmaxID))
  SAGivenOLCpmax<-array(0, dim = c(numberofClass, numberofClass),dimnames = list(classmaxID,classmaxID))
  cSAGivenOLCpmax<-array(0, dim = c(numberofClass, numberofClass),dimnames = list(classmaxID,classmaxID))
  SAGivenOLCcsum<-array(0, dim = c(numberofClass, numberofClass),dimnames = list(classmaxID,classmaxID))
  SAGivenOLCprop<-vector("numeric",length=numberofClass)
  #cSAGivenOcorrectprop=array(0, dim = c(numberofClass, numberofClass),dimnames = list(classmaxID,classmaxID))
  numericBase<-max(classmaxID)+1
  #Assuming both are of same dimension
  conditionChangeRaster<-baseLC*numericBase^2+simulatedLC*numericBase+actualLC
  conditionChangeFrequency<-freq(conditionChangeRaster,useNA='no')
  conditionChangeFrequency<-as.matrix(conditionChangeFrequency)
  conditionChangeFrequency=cbind(conditionChangeFrequency[,1],conditionChangeFrequency[,1],conditionChangeFrequency[,1],conditionChangeFrequency)
  colnames(conditionChangeFrequency)<-c("MatrixID","rowID","colID","value","count")
  conditionChangeFrequency[,c("MatrixID","rowID","colID")]=NA
  #conditionChangeFrequency["MatrixID","rowID","colID"]
  conditionChangeFrequency[,"colID"]=conditionChangeFrequency[,"value"]%%numericBase
  conditionChangeFrequency[,"rowID"]=((conditionChangeFrequency[,"value"]-conditionChangeFrequency[,"colID"])/numericBase)%%numericBase
  conditionChangeFrequency[,"MatrixID"]=((conditionChangeFrequency[,"value"]-conditionChangeFrequency[,"rowID"]*numericBase - conditionChangeFrequency[,"colID"])/numericBase^2)
  #Create Transition Matrix
  conditionalTP<-array(0, dim = c(numberofClass, numberofClass, numberofClass),
                       dimnames = list(classmaxID,classmaxID,classmaxID))
  #dimnames = list(row.names, column.names,matrix.names))
  for (j in seq(1,dim(conditionChangeFrequency)[1])) {
    conditionalTP[conditionChangeFrequency[j,"rowID"],conditionChangeFrequency[j,c("colID")],conditionChangeFrequency[j,"MatrixID"]]=conditionChangeFrequency[j,"count"]             
  }
  #Create Proportion Transition Matrix
  estimatedconditionalTP<-array(0, dim = c(numberofClass, numberofClass, numberofClass),
                                dimnames = list(classmaxID,classmaxID,paste0("MatrixID",classmaxID)))
  
  for (o in 1:numberofClass){
    SAGivenOLCprop[o]=sum(conditionalTP[,,o])/sum(conditionalTP)
    estimatedconditionalTP[,,o]=conditionalTP[,,o]/sum(conditionalTP[,,o])
    #Overall Computation
    SAGivenOLCpe[o,]=rowSums(estimatedconditionalTP[,,o])*colSums(estimatedconditionalTP[,,o])
    SAGivenOLCpmax[o,]=pmin(rowSums(estimatedconditionalTP[,,o]),colSums(estimatedconditionalTP[,,o]))
  }

  #Classwise Computation
  for (o in 1:numberofClass){
    cSAGivenOLCpe[o,]=apply(estimatedconditionalTP,3,lamdba<-function(x){mm<-nMatrixto2Class(x,o)
      sum(rowSums(mm)*colSums(mm))
      })
    cSAGivenOLCpmax[o,]=apply(estimatedconditionalTP,3,lamdba<-function(x){mm<-nMatrixto2Class(x,o)
      sum(pmin(rowSums(mm),colSums(mm)))
    })
  }

  
  peOverall=sum(rowSums(SAGivenOLCpe)*SAGivenOLCprop)
  pMaxOverll=sum(rowSums(SAGivenOLCpmax)*SAGivenOLCprop)
  
  peClasswise=colSums(t(cSAGivenOLCpe)*SAGivenOLCprop)
  pMaxClasswise=colSums(t(cSAGivenOLCpmax)*SAGivenOLCprop)
  
  return(list(
    pe.classwise=peClasswise,
    pe.overall=peOverall,
    pmax.classwise=pMaxClasswise,
    pmax.overall=pMaxOverll,
    TP.classwise=conditionalTP))
}


kappa.agreementindex <- function(simulationFile,actualFile,baseFile) {
  print(paste("kappa.pontius:actualFile",actualFile,sep=":"))
  print(paste("kappa.pontius:predictedFile",simulationFile,sep=":"))
  print(paste("kappa.pontius:baseFile",baseFile,sep=":"))
  CM1<-createTM(simulationFile,actualFile)
  
  pmx<-genratePraportionMatrix(CM1)
  noLC<-ncol(pmx)
  csumpmx<-colSums(pmx)
  rsumpmx<-rowSums(pmx)
  
  cp<-diag(pmx) #Correct Propertion #MQNL #Checked or Expected Agreement Pontius2011 
  qc<-abs(csumpmx-rsumpmx) #Quantitative Disagreement Error #Checked Pontius2011
  ac<-2*(pmin(csumpmx,rsumpmx)-cp) #Maximum Allocation Disagreement  Error Pontius2011
  dc<-csumpmx+rsumpmx-2*cp #cumulative Disagreement Classwise #Checked Pontius2014-eq1
  
  ec<-2*(colSums(minMAB(pmx,t(pmx)))-cp) #Exchange Proportion # Checked Pontius2014-eq4
  sc<-dc-qc-ec #Shorting Propertion # Checked Pontius2014-eq5
  A<-sum(ac)/2 #Total Allocation Disagreemnt  # Checked Pontius2011-eq5
  C<-sum(cp) #Total Correct Propertion # Checked Pontius2011-eq6
  Q<-sum(qc)/2 #Total Quantitative disagrrement # Checked Pontius2014-eq6
  E<-sum(ec)/2 #Total Exchange # Checked Pontius2014-eq7
  S<-sum(sc)/2 #Total Shift # Checked Pontius2014-eq8
  D<-sum(dc)/2 #Total Disagreement due to Allocation # Checked Pontius2014-eq9  & Checked Pontius2011-eq7
  
  ehatnqnl<-rep(1/noLC,noLC)        #ehatnqnl<-rep(1/noLC,noLC)*csumpmx  Idea dropped as not consistent with formuala
  
  ehatnqpl<-pmin(1/noLC,csumpmx) # Checked Pontius2000 Table-2
  ehatmqnl<-csumpmx*rsumpmx # Checked Pontius2000 Table-2 Bracket represents dot product
  ehatmqpl<-pmin(csumpmx,rsumpmx)   # Checked Pontius2000 Table-2 Bracket represents dot product
  ehatpqnl<-csumpmx*csumpmx  # Checked Pontius2000 Table-2 Bracket represents dot product
  ehatmqml<-cp # # Checked Pontius2011-eq8 && 
  
  #kappa standard Correct
  cehatmqml=1-dc  #It must be equal to cp for two class
  cehatnqpl<-pmin(1/noLC,csumpmx) + pmin(1/noLC,1-csumpmx)
  cehatmqnl=csumpmx*rsumpmx+(1-csumpmx)*(1-rsumpmx) #New for two class
  cehatpqnl<-csumpmx*csumpmx+(1-csumpmx)*(1-csumpmx)  
  kstandard<-(cehatmqml-cehatmqnl)/(1-cehatmqnl) #New for two class
  Kstandard<-(C-sum(ehatmqnl))/(1-sum(ehatmqnl)) #Checked Pontius2011 equ11
  
  #kappa location Correct
  klocation<-(ehatmqml-ehatmqnl)/(ehatmqpl-ehatmqnl) #  #Seems Wrong for two class Checked New for two class  consistent VanVliet2011 eq 5 
  Klocation<-(C-sum(ehatmqnl))/(sum(ehatmqpl)-sum(ehatmqnl)) # Checked VanVliet2011 eq-6 & Pontius2000 Table3
  
  #kappa Histogram Correct
  khistogram<-(1-qc-cehatmqnl)/(1-cehatmqnl)
  Khistogram<-(1-Q-sum(ehatmqnl))/(1-sum(ehatmqnl))  
  
  #kappa no To Check
  #kno<-(ehatmqml-ehatnqnl)/(1-ehatnqnl) #Seems Wrong
  kno<-(cehatmqml-ehatnqnl)/(1-ehatnqnl) # Checked
  Kno<-(C-1/noLC)/(1-1/noLC)
  
  #kappa allocation To Check
  kallocation<-(cehatmqml-cehatmqnl)/(1-qc-cehatmqnl) # Checked
  #kallocation<-(ehatmqml-ehatmqnl)/(ehatmqpl-ehatmqnl)
  Kallocation<-(C-sum(ehatmqnl))/(1-Q-sum(ehatmqnl))
  
  cehatpqml<-cehatpqnl+kallocation*(1-cehatpqnl) #done
  cehatnqml<-ehatnqnl+kallocation*(cehatnqpl-ehatnqnl) #done
  
  #kappa Quantity To Check
  y<-cehatpqml  #done Pontius2000 table 3
  z<-cehatnqml #done Pontius2000 table 3
  Y<-sum(ehatpqnl)+Kallocation*(1-sum(ehatpqnl)) #done Pontius2000 table 3
  Z<-1/noLC+Kallocation*(sum(ehatnqpl)-1/noLC) #done Pontius2000 table 3
  kquantity<-(cehatmqml-z)/(y-z) #done Pontius2000 Pontius2014
  Kquantity<-(C-Z)/(Y-Z) #done Pontius2000 Pontius2014
  
  #Metric For Dependence
  SAGivenOTrans<-getSAGivenOTrans(simulationFile,actualFile,baseFile)
  pmxSAGivenOTrans<-SAGivenOTrans$pe.overall
  cpmxSAGivenOTrans<-SAGivenOTrans$pe.classwise
  pmxSAGivenOMax<-SAGivenOTrans$pmax.overall
  cpmxSAGivenOMax<-SAGivenOTrans$pmax.classwise

  #kapp Simulation Correct
  ksimulation<-(cehatmqml-cpmxSAGivenOTrans)/(1-cpmxSAGivenOTrans)
  ksimulation[is.na(ksimulation)]=1
  ksimulation[is.infinite(ksimulation)]=NA
  
  Ksimulation<-(C-pmxSAGivenOTrans)/(1-pmxSAGivenOTrans)
  if(is.na(Ksimulation))Ksimulation=1
  if(is.infinite(Ksimulation))Ksimulation=NA
  
  #kapp Transition Correct
  ktransition<-(cpmxSAGivenOMax-cpmxSAGivenOTrans)/(1-cpmxSAGivenOTrans)
  ktransition[is.na(ktransition)]=1
  ktransition[is.infinite(ktransition)]=NA

  Ktransition<-(pmxSAGivenOMax-pmxSAGivenOTrans)/(1-pmxSAGivenOTrans)
  if(is.na(Ktransition))Ktransition=1
  if(is.infinite(Ktransition))Ktransition=NA

  #kapp Translocation Correct
  ktranslocation<-(cehatmqml-cpmxSAGivenOTrans)/(cpmxSAGivenOMax-cpmxSAGivenOTrans)
  ktranslocation[is.na(ktranslocation)]=1
  ktranslocation[is.infinite(ktranslocation)]=NA
  
  Ktranslocation<-(C-pmxSAGivenOTrans)/(pmxSAGivenOMax-pmxSAGivenOTrans)
  if(is.na(Ktranslocation))Ktranslocation=1
  if(is.infinite(Ktranslocation))Ktranslocation=NA
 
  
 
  return (
    list(noOfClass=noLC,
         kstandard.classwise=kstandard,kstandard.overall=Kstandard,kno.classwise=kno,kno.overall=Kno, 
         klocation.classwise=klocation,klocation.overall=Klocation,kallocation.classwise=kallocation,kallocation.overall=Kallocation,
         khistogram.classwise=khistogram, khistogram.overall=Khistogram,kquantity.classwise=kquantity,kquantity.overall=Kquantity,
         ksimulation.classwise=ksimulation, ksimulation.overall=Ksimulation, ktransition.classwise=ktransition, ktransition.overall=Ktransition,
         ktranslocation.classwise=ktranslocation,ktranslocation.overall=Ktranslocation, 
         allocationdisagreemnt.overall=A,allocationagreemnt.overall=C,shift.overall=S, 
         exchange.overall=E, quantitydisagreemnt.overall=Q,cumulativedisagreemnt.overall=D,
         allocationdisagreemnt.classwise=ac,allocationagreemnt.classwise=cp,shift.classwise=sc,
         exchange.classwise=ec,quantitydisagreemnt.classwise=qc,cumulativedisagreemnt.classwise=dc,
         propertionmatrix=pmx  
    ))
}

summary.kappa.agreementindex<-function(kappaindex,namesLC=NA){
  
  if(sum(is.na(namesLC)>0)){
    namesLC<-paste0("Class",seq(1,kappaindex$noOfClass))
  }
  
  names(kappaindex$kstandard.classwise)<-namesLC
  names(kappaindex$kno.classwise)<-namesLC
  names(kappaindex$klocation.classwise)<-namesLC
  names(kappaindex$kallocation.classwise)<-namesLC
  names(kappaindex$khistogram.classwise)<-namesLC
  names(kappaindex$kquantity.classwise)<-namesLC
  names(kappaindex$ksimulation.classwise)<-namesLC
  names(kappaindex$ktransition.classwise)<-namesLC
  names(kappaindex$ktranslocation.classwise)<-namesLC
  names(kappaindex$allocationdisagreemnt.classwise)<-namesLC
  names(kappaindex$allocationagreemnt.classwise)<-namesLC
  names(kappaindex$shift.classwise)<-namesLC
  names(kappaindex$exchange.classwise)<-namesLC
  names(kappaindex$quantitydisagreemnt.classwise)<-namesLC
  names(kappaindex$cumulativedisagreemnt.classwise)<-namesLC
  rownames(kappaindex$propertionmatrix)<-namesLC
  colnames(kappaindex$propertionmatrix)<-namesLC
  
  
  
  print("Propertionmatrix:",quote=F)
  print(round(kappaindex$propertionmatrix,7), quote=F)
  print("classwise-Allocation Disagreement:", quote=F); 
  print(t(round(kappaindex$allocationdisagreemnt.classwise,7)));
  print("classwise-Quantitative Disagreement:", quote=F); 
  print(round(kappaindex$quantitydisagreemnt.classwise,7));
  print("classwise-Agreement::", quote=F); 
  print(round(kappaindex$allocationagreemnt.classwise,7));
  print("classwise-Cumulative Disagreement:", quote=F); 
  print(round(kappaindex$cumulativedisagreemnt.classwise,7));
  print("classwise-Shift:", quote=F); 
  print(round(kappaindex$shift.classwise,7));
  print("classwise-Exchance:", quote=F); 
  print(round(kappaindex$exchange.classwise,7));
  
  ##################Overall-Start###################
  print("Overall-Allocation Disagreement:", quote=F); 
  print(round(kappaindex$allocationdisagreemnt.overall,7));
  print("Overall-Quantitative Disagreement:", quote=F); 
  print(round(kappaindex$quantitydisagreemnt.overall,7));
  print("Overall-Agreement::", quote=F); 
  print(round(kappaindex$allocationagreemnt.overall,7));
  print("Overall-Cumulative Disagreement:", quote=F); 
  print(round(kappaindex$cumulativedisagreemnt.overall,7));
  print("Overall-kstandard:", quote=F); 
  print(round(kappaindex$kstandard.overall,7));
  print("Overall-kno:", quote=F); 
  print(round(kappaindex$kno.overall,7));
  print("Overall-kallocation:", quote=F); 
  print(round(kappaindex$kallocation.overall,7));
  print("Overall-khistogram:", quote=F); 
  print(round(kappaindex$khistogram.overall,7));
  print("Overall-kquantity:", quote=F); 
  print(round(kappaindex$kquantity.overall,7));
  print("Overall-ksimulation:", quote=F); 
  print(round(kappaindex$ksimulation.overall,7));
  print("Overall-ktransition:", quote=F); 
  print(round(kappaindex$ktransition.overall,7));
  print("Overall-ktranslocation:", quote=F); 
  print(round(kappaindex$ktranslocation.overall,7));
  ##################Overall-Stop####################
 
  ##################Calsswise-Start###################
  print("classwise-kstandard:", quote=F); 
  print(round(kappaindex$kstandard.classwise,7));
  print("classwise-kno:", quote=F); 
  print(round(kappaindex$kno.classwise,7));
  print("classwise-kallocation:", quote=F); 
  print(round(kappaindex$kallocation.classwise,7));
  print("classwise-khistogram:", quote=F); 
  print(round(kappaindex$khistogram.classwise,7));
  print("classwise-kquantity:", quote=F); 
  print(round(kappaindex$kquantity.classwise,7));
  print("classwise-ksimulation:", quote=F); 
  print(round(kappaindex$ksimulation.classwise,7));
  print("classwise-ktransition:", quote=F); 
  print(round(kappaindex$ktransition.classwise,7));
  print("classwise-ktranslocation:", quote=F); 
  print(round(kappaindex$ktranslocation.classwise,7));
  ##################Calsswise-Ends###################
  
}

summary.kappa.agreementindex.tabel<-function(kappaindex,namesLC=NA){
  if(sum(is.na(namesLC)>0)){
    namesLC<-paste0("Class",seq(1,kappaindex$noOfClass))
  }
  
  names(kappaindex$kstandard.classwise)<-namesLC
  names(kappaindex$kno.classwise)<-namesLC
  names(kappaindex$klocation.classwise)<-namesLC
  names(kappaindex$kallocation.classwise)<-namesLC
  names(kappaindex$khistogram.classwise)<-namesLC
  names(kappaindex$kquantity.classwise)<-namesLC
  names(kappaindex$ksimulation.classwise)<-namesLC
  names(kappaindex$ktransition.classwise)<-namesLC
  names(kappaindex$ktranslocation.classwise)<-namesLC
  names(kappaindex$allocationdisagreemnt.classwise)<-namesLC
  names(kappaindex$allocationagreemnt.classwise)<-namesLC
  names(kappaindex$shift.classwise)<-namesLC
  names(kappaindex$exchange.classwise)<-namesLC
  names(kappaindex$quantitydisagreemnt.classwise)<-namesLC
  names(kappaindex$cumulativedisagreemnt.classwise)<-namesLC
  rownames(kappaindex$propertionmatrix)<-namesLC
  colnames(kappaindex$propertionmatrix)<-namesLC
  
  metric<-c("Kstandard","Kno","Kallocation","Khistogram","Kquantity","Ksimulation","Ktransition","Ktranslocation")
  consolidate.overall<-c(kappaindex$kstandard.overall,kappaindex$kno.overall,
                         kappaindex$kallocation.overall,kappaindex$khistogram.overall,
                         kappaindex$kquantity.overall,kappaindex$ksimulation.overall,
                         kappaindex$ktransition.overall,kappaindex$ktranslocation.overall)
  
  #consolidate.overall<-matrix(consolidate.overall,ncol = 1)
  consolidate.overall<-cbind(Kappa=round(consolidate.overall,7),"Kappa Type"=metric)
  rownames(consolidate.overall)<-rep("OA",length(metric))
  
  consolidate.overall<-rbind(consolidate.overall,
        cbind(Kappa=round(kappaindex$kstandard.classwise,7),"Kappa Type"=rep("Kstandard",kappaindex$noOfClass)),
        cbind(Kappa=round(kappaindex$kno.classwise,7),"Kappa Type"=rep("Kno",kappaindex$noOfClass)),
        cbind(Kappa=round(kappaindex$kallocation.classwise,7),"Kappa Type"=rep("Kallocation",kappaindex$noOfClass)),
        cbind(Kappa=round(kappaindex$khistogram.classwise,7),"Kappa Type"=rep("Khistogram",kappaindex$noOfClass)),
        cbind(Kappa=round(kappaindex$kquantity.classwise,7),"Kappa Type"=rep("Kquantity",kappaindex$noOfClass)),
        cbind(Kappa=round(kappaindex$ksimulation.classwise,7),"Kappa Type"=rep("Ksimulation",kappaindex$noOfClass)),
        cbind(Kappa=round(kappaindex$ktransition.classwise,7),"Kappa Type"=rep("Ktransition",kappaindex$noOfClass)),
        cbind(Kappa=round(kappaindex$ktranslocation.classwise,7),"Kappa Type"=rep("Ktranslocation",kappaindex$noOfClass))
  )
  return(consolidate.overall)
}

summary.kappa <- function(kappa, alpha=0.05) {
  ciw<-function(var, n) {
    qnorm(1-(alpha/2))*sqrt(var) + (1/(2*n))
  }
  print(paste("Number of observations:", kappa$sum.n), quote=F)
  print("Summary of naive statistics", quote=F)
  print(paste(
    "Overall accuracy, stdev, CV%:",
    round(kappa$sum.naive, 4), ",",
    round(sqrt(kappa$sum.var), 4), ",",
    round((sqrt(kappa$sum.var)/kappa$sum.naive)*1000,0)/10),
    quote=F)
  w<-ciw(kappa$sum.var, kappa$sum.n)
  print(paste(
    round((1-alpha)*100,0),"% confidence limits for accuracy:",
    round((kappa$sum.naive-w),4),"...",
    round((kappa$sum.naive+w),4)), quote=F, sep="")
  print("User's accuracy", quote=F); print(round(kappa$user.naive,4));
  print("Producer's reliability:", quote=F); print(round(kappa$prod.naive,4));
  print("Summary of kappa statistics", quote=F)
  print(paste("Overall kappa, stdev, & CV%:",
              round(kappa$sum.kappa,4), ",",
              round(sqrt(kappa$sum.kvar),4), ",",
              round((sqrt(kappa$sum.kvar)/kappa$sum.kappa)*1000,0)/10), quote=F)
  w<-ciw(kappa$sum.kvar, kappa$sum.n)
  print(paste(
    round((1-alpha)*100,0),"% confidence limits for kappa:",
    round((kappa$sum.kappa-w),4),"...",
    round((kappa$sum.kappa+w),4)), quote=F, sep="")
  print("Per-class kappa, stdev, & CV%, for user's accuracy:", quote=F)
  print(round(kappa$user.kappa,4), quote=F);
  print(round(sqrt(kappa$user.kvar),4), quote=F);
  print(round((sqrt(kappa$user.kvar)/kappa$user.kappa)*1000,0)/10, quote=F);
  print("Per-class kappa, stdev, & CV%, for producer's reliability:", quote=F)
  print(round(kappa$prod.kappa,4), quote=F);
  print(round(sqrt(kappa$prod.kvar),4), quote=F);
  print(round((sqrt(kappa$prod.kvar)/kappa$prod.kappa)*1000,0)/10, quote=F);
}

PyKappasummary<-function(kappa){
  ciw<-function(var, n) {
    qnorm(1-(alpha/2))*sqrt(var) + (1/(2*n))
  }
  #kappa(tm)
  retstring<-""
  uac<-paste(round(kappa$user.naive,4), collapse= ",")
  pac<-paste(round(kappa$prod.naive,4), collapse= ",")
  alpha=0.05
  w1<-ciw(kappa$sum.var, kappa$sum.n)
  alpha=0.01
  w2<-ciw(kappa$sum.var, kappa$sum.n)
  retstring<-sprintf("No Of Observation:%.0f\nOverall Of Accuracy: %.4f \n %.0f%% CI :%.4f-%.4f,%.0f%% CI: %.4f-%.4f\nUser’s accuracy : %s\nProducer’s reliability: %s\nOverall kappa: %.4f %.0f%% CI :%.4f-%.4f, %.0f%% CI :%.4f-%.4f",
                     kappa$sum.n,
                     kappa$sum.naive,
                     round((1-0.05)*100,0),
                     kappa$sum.naive-w1,
                     kappa$sum.naive+w1,
                     round((1-0.01)*100,0),
                     kappa$sum.naive-w2,
                     kappa$sum.naive+w2,
                     uac,
                     pac,
                     kappa$sum.kappa,
                     round((1-0.05)*100,0),
                     kappa$sum.kappa-w1,
                     kappa$sum.kappa+w1,
                     round((1-0.01)*100,0),
                     kappa$sum.kappa-w2,
                     kappa$sum.kappa+w2
  )
  retstring1<-sprintf("NoOfObservation:%.0f~~OverallOfAccuracy:%.4f~~95CI:%.4f-%.4f~~99CI:%.4f-%.4f~~UserAccuracy:%s~~ProducerReliability:%s~~Overallkappa:%.4f~~95CI:%.4f-%.4f~~95CI:%.4f-%.4f~~",
                      kappa$sum.n,
                      kappa$sum.naive,
                      kappa$sum.naive-w1,
                      kappa$sum.naive+w1,
                      kappa$sum.naive-w2,
                      kappa$sum.naive+w2,
                      uac,
                      pac,
                      kappa$sum.kappa,
                      kappa$sum.kappa-w1,
                      kappa$sum.kappa+w1,
                      kappa$sum.kappa-w2,
                      kappa$sum.kappa+w2
  )
  #print(retstring)
  return(retstring1)
}

ConvertMultDimDTToMapUsingFile<-function(dT,File,withNAvalue=NA)
{
  numberofClass<-dim(dT)[2]
  numberofRows<-dim(dT)[1]
  predictedMapSingle<-raster(File)
  predictedMapSingle[]<-rowSums(dT*matrix(rep(seq(1,numberofClass),numberofRows),nrow=numberofRows,ncol=numberofClass,byrow=T),na.rm=T)
  predictedMapSingle[rowSums(dT,na.rm=T)==0]<-NA
  return(predictedMapSingle)
}

getClassNumber<-function(T1file){
  library(raster)
  return(names(table(raster(T1file)[])))
}

getModelFitSummary<-function(T1File,T2File,T1drivers,modelType,withNAvalue=NA,method="NotIncludeCurrentClass",maskFile=NA,aoiFile=NA){
  if(exists("debugValue") && debugValue>=2){      
    print('DebugLevel=1:IN getModelFitSummary :1932:Start')
  }
  #modeldetail<-list()
  dT1<-getDataTable(T1File,TRUE,withNAvalue,NA,maskFile,aoiFile)
  dT2<-getDataTable(T2File,TRUE,withNAvalue,NA,maskFile,aoiFile)
  driverT<-getDataTable(T1drivers,FALSE,withNAvalue,names(T1drivers),maskFile,aoiFile)
  numberofClass<-dim(dT1)[2]  ##ADDED
  
  #numberofRows<-dim(dT1)[1]
  #numberofcellWithoutNA<-sum(rowSums(dT2,na.rm=TRUE))
  modelType<-rep(modelType,numberofClass) ##ADDED
  modelfit<-fitModelSeparately(dT1,driverT,dT2,modelType,method="NotIncludeCurrentClass",modelformula=NA) ##ADDED
  #modelfit<-fitSameModelToAll(dT1,driverT,dT2,modelType,method) ##Removed
  #for( i in 1:length(modelfit)){
  #    modeldetail[[i]]<-summary(modelfit[[i]])
  #}
  #print(modeldetail)
  return(getModelSummary(modelfit))
}

getModelSummary<-function(modelfit){
  modeldetail<-list()
  for( i in 1:length(modelfit)){
    if(!is.na(class(modelfit[[i]])[2]) && class(modelfit[[i]])[2]=="randomForest")
    { modeldetail[[i]]<-paste0(sapply(c(paste0(modelfit[[i]]$call,collapse=" "),"Variable\t%IncMSE\tIncNodePurity",#"(Intercept)\tNA",
                                        paste0(apply(cbind(rownames(importance(modelfit[[i]])),importance(modelfit[[i]])),1,paste,collapse="\t"),collapse = "\n"),
                                        #paste0(apply(importance(modelfit[[i]]),1,paste,collapse="\t:"),collapse = "\n"),
                                        "---","\nError\nFP\tTN",paste(sprintf("%.4f",modelfit[[i]]$confusion[,3]),collapse = " "),collapse="\n" ),"paste0",collapse="\n"),collapse = "\n")
    }
    else{
      modeldetail[[i]]<-summary(modelfit[[i]])
      #print(summary(modelfit[[i]]))
    }
  }
  if(exists("debugValue") && debugValue>=2){      
    print('DebugLevel=1:IN getModelFitSummary :1375:Exit')
  }
  return((modeldetail))
}

printParameterDetails<-function(modelType,T1File,T2File,withClassName,T1drivers,T2drivers,withNAvalue,demand,restrictSpatialMigration,AllowedClassMigration,
                                conversionOrder,classAllocationOrder,neighbour,method,modelformula,outputfile,maskFile,aoiFile,suitabilityFileDirectory){
  print("********************BegningOfParameter************************")        
  print(paste("{{ T1File= ",paste(T1File,collapse = ""),"}}"))
  print(paste("{{ T2File= ",paste(T2File,collapse = ";"),"}}"))
  print(paste("{{ withClassName= ",paste(withClassName,collapse = ";"),"}}"))
  print(paste("{{ T1drivers= ",paste(T1drivers,collapse = ";"),"}}"))
  print(paste("{{ T2drivers= ",paste(T2drivers,collapse = ";"),"}}"))
  print(paste("{{ withNAvalue= ",withNAvalue,"}}"))
  print(paste("{{ modelType  ",paste(modelType,collapse = ";"),"}}"))
  print(paste("{{ modelformula ",paste(modelformula,collapse = ";"),"}}"))
  print(paste("{{ demand= ",paste(demand,collapse = ";"),"}}"))
  print(paste("{{ restrictSpatialMigration= ",paste(restrictSpatialMigration,collapse = ";"),"}}"))
  print(paste("{{ AllowedClassMigration= ",paste(AllowedClassMigration,collapse = ";"),"}}"))
  print(paste("{{ conversionOrder= ",paste(t(conversionOrder),collapse = ";"),"}}(Row Wise)"))
  print(paste("{{ classAllocationOrderr= ",paste(classAllocationOrder,collapse = ";"),"}}"))
  print(paste("{{ neighbour= ",paste(neighbour,collapse = ";"),"}}"))
  print(paste("{{ method ",method,"}}"))
  print(paste("{{ outputfile= ",outputfile,"}}"))
  print(paste("{{ maskFile= ",maskFile,"}}"))
  print(paste("{{ aoiFile= ",aoiFile,"}}"))
  print(paste("{{ suitabilityFileDirectory= ",suitabilityFileDirectory,"}}"))
  print("*********************EndOfParameter***********************************")    
}

createSuitabilityMap<-function(suit,TFile,suitabilityFDirectory,fileNames,withNA=NA){
  library(raster)
  suitability<-raster(TFile)
  suitability[]=NA
  if(exists("debugValue") && debugValue>=1){      
    print("createSuitabilityMap:Entry")
  }
  for(i in 1:length(fileNames)){
    
    suitability[suit[[i]]$id]<-suit[[i]]$weight
    writeRaster(suitability,paste( suitabilityFDirectory,"/",fileNames[i],"SM.tif",sep=""),format="GTiff",options="INTERLEAVE=BAND",overwrite=TRUE)
  }
  if(exists("debugValue") && debugValue>=1){      
    print("createSuitabilityMap:Exit")
  }
}

ComputeNearByWeight<-function(TFile,withNA=NA,wSize=NA){
  if(exists("debugValue") && debugValue>=2){      
    print("ComputeNearByWeight:Entry")
  }
  library(raster)
  library(data.table)
  tmp<-as.factor(raster(TFile))
  
  if(!is.na(withNA)){
    tmp[tmp[]==withNA]=NA
  }
  gridID=which(!is.na(tmp[]))
  #print(head(gridID))
  tbl<-as.matrix(levels(tmp)[[1]])#as.numeric(table(tmp[],stringsAsFactors=TRUE))
  #names(tbl)<-c("DN","FREQ")
  #tbl<-tbl[with(tbl,order(DN)),]
  neighbourwt=list()
  
  for(i in 1:length(tbl)){
    tmp[tmp[]!=tbl[i]]=NA
    dst<-distance(tmp)
    #if(!is.na(wSize))
    #    dst[dst[]>wSize]=NA
    
    dst[]<-as.integer(dst[]/res(tmp)[1])    
    #dst[tmp[]!=tbl[,1][i]]=NA
    neighbourwt[[i]]<-as.data.table(cbind(id=gridID,weight=dst[gridID]))
    setkey(neighbourwt[[i]],id)  
    tmp<-raster(TFile)
  }
  if(exists("debugValue") && debugValue>=2){      
    print("ComputeNearByWeight:Exit")
  }
  return(neighbourwt)
}

ComputeNW<-function(i,TFile,withNA=NA,wSize=NA){
  library(data.table)
  if(exists("debugValue") && debugValue>=2){      
    print("ComputeNW:Entry")
  }
  library(raster)
  tmp<-as.factor(raster(TFile))
  if(!is.na(withNA)){
    tmp[tmp[]==withNA]=NA
  }
  gridID=which(!is.na(tmp[]))
  tbl<-as.matrix(levels(tmp)[[1]])
  tmp[tmp[]!=tbl[i]]=NA
  dst<-distance(tmp)
  if(!is.na(wSize)){
    lardist=sqrt(res(dst)[1]*res(dst)[2])*wSize
    dst[dst[]>lardist]=lardist
  }
  #dst[]<-as.integer(dst[]/lardist)
  dst[]<-as.integer(dst[]/min(dst[dst[]>0]))    
  #dst[tmp[]!=tbl[,1][i]]=NA
  neighbourwt<-as.data.table(cbind(id=gridID,weight=dst[gridID]))
  setkey(neighbourwt,id)  
  if(exists("debugValue") && debugValue>=2){      
    print("ComputeNW:Exit")
  }
  neighbourwt
}

ParallelComputeNearByWeight<-function(TFile,withNA=NA,wSize=NA){

  if(exists("debugValue") && debugValue>=2){      
    print("ParallelComputeNearByWeight:Entry")
  }
  library(raster)
  tmp<-as.factor(raster(TFile))
  
  if(!is.na(withNA)){
    tmp[tmp[]==withNA]=NA
  }
  
  if(!is.na(wSize)){
    wSize=3
  }
  
  tbl<-as.matrix(levels(tmp)[[1]])
  numberofClass=length(tbl)
  neighbourwt.lst=list()
  library(parallel)#library(multicore)
  library(doParallel)
  withCore=detectCores()
  if(withCore > numberofClass){
    withCore=numberofClass
  }
  myCluster=registerDoParallel(core=withCore)
  if(exists("debugValue") && debugValue>=2){      
    print("ParallelComputeNearByWeight:ComputeNW:Called")
  }
  
  neighbourwt.lst<-foreach ( i = 1:numberofClass,.export=c("ComputeNW",'TFile','withNA','wSize'),.packages = c("raster","data.table")) %dopar%{
    ComputeNW(i,TFile,withNA=withNA,wSize=wSize)
  }
  #registerDoSEQ()
  #stopCluster(myCluster)
  unregister()
  if(exists("debugValue") && debugValue>=2){      
    print("ParallelComputeNearByWeight:Exit")
  }
  return(neighbourwt.lst)
  
}


createNeighbourMap<-function(nearbyWeight,TFile,suitabilityFDirectory,fileNames,withNA=NA){
  library(raster)
  tmp<-raster(TFile)
  tmp[]=withNA
  for(i in 1:length(fileNames)){
    tmp[nearbyWeight[[i]]$id]<-nearbyWeight[[i]]$weight
    writeRaster(tmp,paste( suitabilityFDirectory,"/",fileNames[i],"NW.tif",sep=""),format="GTiff",options="INTERLEAVE=BAND",overwrite=TRUE)
  }
  
}


genratePredictedMap<-function(modelType='logistic',T1File,T2File,withClassName=NA,T1drivers,T2drivers,withNAvalue,demand=NA,restrictSpatialMigration=NA,AllowedClassMigration=NA,
                              conversionOrder='TP',classAllocationOrder=NA,neighbour=NA,method='NotIncludeCurrentClass',modelformula=NA,outputfile,maskFile=NA,aoiFile=NA,suitabilityFileDirectory=NA)
{
  print(format(Sys.time(), "%a %b %d %X %Y %Z"))
  if(exists("debugValue") && debugValue>=1 ){
    print('DebugLevel=1:IN genratePredictedMap :1514:Start')
  }
  printParameterDetails(modelType,T1File,T2File,withClassName,T1drivers,T2drivers,withNAvalue,demand,restrictSpatialMigration,AllowedClassMigration,
                        conversionOrder,classAllocationOrder,neighbour,method,modelformula,outputfile,maskFile,aoiFile,suitabilityFileDirectory)
  
  
  dT1<-getDataTable(T1File,TRUE,withNAvalue,withClassName,maskFile,aoiFile)
  dT2<-getDataTable(T2File,TRUE,withNAvalue,withClassName,maskFile,aoiFile)
  driverT1<-getDataTable(T1drivers,FALSE,withNAvalue,names(T1drivers),maskFile,aoiFile)
  if(exists("debugValue") && debugValue>=1 ){
    print('genratePredictedMap:getDataTable:Done')
  }
  numberofClass<-dim(dT1)[2]
  numberofRows<-dim(dT1)[1]
  numberofcellWithoutNA<-sum(rowSums(dT2,na.rm=TRUE))
  modelfit<-fitModelSeparately(dT1,driverT1,dT2,modelType,method,modelformula)
  print(getModelSummary(modelfit))

  if(exists("debugValue") && debugValue>=1 ){
    print('genratePredictedMap:fitModelSeparately:Done')
  }
  driverT2<-getDataTable(T2drivers,FALSE,withNAvalue,names(T2drivers),maskFile,aoiFile)
  TM<-createTM(dT1,dT2)
  if(exists("debugValue") && debugValue>=1 ){
    print('genratePredictedMap:createTM:Done')
  }
  #Look for future allocation matrix either as per the markovian based demand or based on the demand.
  if(sum(is.na(demand))!=0){
    newTM<-getNewTM(TM)
  }else {
    newTM<-getNewTM(TM,demand,restrictSpatialMigration)
  }
  if(exists("debugValue") && debugValue>=1 ){
    print('genratePredictedMap:getNewTM:Done')
  }
  suit<-ParallelconstructSuitablity(modelfit,driverT2,dT2,method)
  #suit<-constructSuitablity(modelfit,driverT2,dT2,method)
  if(exists("debugValue") && debugValue>=1 ){
    print(paste("genratePredictedMap:ParallelconstructSuitablity:Done"))
  }
  if(!is.na(suitabilityFileDirectory))
  {
    createSuitabilityMap(suit,T2File,suitabilityFileDirectory,names(dT1),withNAvalue)
    if(exists("debugValue") && debugValue>=1 ){
      print(paste("genratePredictedMap:createSuitabilityMap:Done"))
    }
  }
  nearbyWeight<-ParallelComputeNearByWeight(T2File,withNA=withNAvalue)
  if(!is.na(suitabilityFileDirectory))
  {
    createNeighbourMap(nearbyWeight,T2File,suitabilityFileDirectory,names(dT1),withNAvalue)
    if(exists("debugValue") && debugValue>=1 ){
      print(paste("genratePredictedMap:createNeighbourMap:Done"))
    }
  }
  if(exists("debugValue") && debugValue>=1 ){
    print(paste("genratePredictedMap:ParallelComputeNearByWeight:Done"))
  }
  if(sum(is.na(neighbour))!=0)
  {
    if(exists("debugValue") && debugValue>=1 ){
      print(paste("genratePredictedMap::NoNeighbourhoodProcess:Selected"))
    }
    predictedTB<-getAllocatedDT(suit,newTM,dT2,nearbyWeight,demand,restrictSpatialMigration,AllowedClassMigration,
                                conversionOrder,classAllocationOrder,method)
    if(exists("debugValue") && debugValue>=1 ){
      print(paste("genratePredictedMap:getAllocatedDT:Done"))
    }
    predictedMapSingle=ConvertMultDimDTToMapUsingFile(predictedTB,T2File)
    if(exists("debugValue") && debugValue>=1 ){
      print(paste("genratePredictedMap:predictedMapSingle:Done"))
    }
  }else{
    windowSize<-neighbour[[1]]
    NoOfsteps<-neighbour[[2]]
    if(neighbour[[3]]==0){
      writeStepFile=FALSE
    }else{
      writeStepFile=TRUE
    }
    
    nearbyWeight<-ParallelComputeNearByWeight(T2File,withNA=withNAvalue,windowSize)
    if(!is.na(suitabilityFileDirectory))
    {
      createNeighbourMap(nearbyWeight,T2File,suitabilityFileDirectory,names(dT2),withNAvalue)
      if(exists("debugValue") && debugValue>=1 ){
        print(paste("genratePredictedMap:createNeighbourMap:Done"))
      }
    }
    
    library(raster)
    t2<-as.factor(raster(T2File))
    if(!is.na(withNAvalue))
      t2[t2[]==withNAvalue]<-NA
    if(sum(is.na(demand))!=0){
      FinalFCM<-getNewTM(TM)
    }else {
      FinalFCM<-getNewTM(TM,demand,restrictSpatialMigration)
    }
    
    #nearbyWeight<-ParallelComputeNearByWeight(T2File,withNA=withNAvalue,windowSize)
    if(exists("debugValue") && debugValue>=1 ){
      print('genratePredictedMap:ParallelComputeNearByWeight:done')
    }
    
    windowMatrix<-matrix(rep(1,windowSize*windowSize),nc=windowSize,nr=windowSize)
    for(deltat in 1:NoOfsteps){
      weightedsuit<-list()
      yearlyFCM<-getYearlyMatrix(FinalFCM,NoOfsteps,deltat)
      if(exists("debugValue") && debugValue>=1 ){
        print(paste("genratePredictedMap:getYearlyMatrix:Done"))
      }
      for(i in 1:numberofClass){
        tmp<-t2
        tmp[tmp!=i]<-0
        tmp[tmp==i]<-1
        tmp[is.na(t2)]<-0
        if(windowSize!=1){        
        f <-focal(tmp,w=windowMatrix,pad=TRUE,padValue=0)
        f[is.na(t2[])]<-NA
        f[f==0]<-1 #Where ever value neighbour are less than 1 they will have weight 1 
        tmp[][suit[[i]]$id]<-suit[[i]]$weight
        tmp<-tmp*f
        }
        #plot(f)
        weightedsuit[[i]]<-as.data.table(cbind(id=suit[[1]]$id,weight=tmp[][suit[[1]]$id]))[order(weight,decreasing = TRUE)]            
      }
      #nearbyWeight<-ComputeNearByWeight(t2,withNA=withNAvalue)
      
      predictedTB<-getAllocatedDT(weightedsuit,yearlyFCM,dT2,nearbyWeight,demand,restrictSpatialMigration,AllowedClassMigration,
                                  conversionOrder,classAllocationOrder,method)
      if(exists("debugValue") && debugValue>=1 ){
        print(sum(yearlyFCM))
        print('genratePredictedMap:getAllocatedDT:done')
      }
      predictedMapSingle=ConvertMultDimDTToMapUsingFile(predictedTB,T2File)
      if(writeStepFile){
        t2=writeRaster(predictedMapSingle,paste( substr(outputfile,1,nchar(outputfile)-4),"-Step",deltat,".tif",sep=""),format="GTiff",options="INTERLEAVE=BAND",overwrite=TRUE)
      }
      t2<-predictedMapSingle
      if(exists("debugValue") && debugValue>=2 ){
        print("LINE:1349");print(sum(rowSums(TM)));print(sum(!is.na(predictedMapSingle)[],na.rm=T)) #TODO Check why one extra is coming
      }
    }
    #There will be slight difference between the map stepswise and one step
    predictedTB<-getAllocatedDT(weightedsuit,FinalFCM,dT2,nearbyWeight,demand,restrictSpatialMigration,AllowedClassMigration,conversionOrder,classAllocationOrder,method)
    predictedMapSingle=ConvertMultDimDTToMapUsingFile(predictedTB,T2File)
    
    
  }
  writeRaster(predictedMapSingle,outputfile,format="GTiff",options="INTERLEAVE=BAND",overwrite=TRUE)
  print(format(Sys.time(), "%a %b %d %X %Y %Z"))
  return(predictedMapSingle)
}

ConvertMultDimDTToMapUsingRaster<-function(dT=NA,input.raster,withNAvalue=NA)
{
  #Not Tested
  numberofClass<-dim(dT)[2]
  numberofRows<-dim(dT)[1]
  predictedMap<-getRaster(input.raster,TRUE,withNAvalue);
  #predictedMap<-input.raster
  predictedMap[]<-NA
  predictedMap[]<-as.matrix(dT)
  predictedMapSingleTB<-rowSums(dT*matrix(rep(seq(0,numberofClass-1),numberofRows),nrow=numberofRows,ncol=numberofClass,byrow=T),na.rm=T)
  predictedMapSingleTB[rowSums(dT,na.rm=T)==0]<-NA
  predictedMapSingle<-raster(input.raster)
  predictedMapSingle[]<-predictedMapSingleTB
}

ConvertSingleDimDTToMapUsingRaster<-function(dT=NA,input.raster,withNAvalue=NA)
{
  #Not Tested
  predictedMap<-input.raster
  predictedMap[]<-NA
  predictedMap[]<-as.matrix(dT)
  predictedMap[is.na(dT)]<-NA
  return(predictedMap)
}

getKappaSummary<-function(acualFile,predictedFile,withNAvalue=NA,withClassName=NA)
{
  print(paste("getKappaSummary:actualFile",acualFile,sep=":"))
  print(paste("getKappaSummary:predictedFile",predictedFile,sep=":"))
  actualdT<-getDataTable(acualFile,TRUE,NA,withClassName)
  predicteddT<-getDataTable(PredictedFile,TRUE,NA,withClassName)
  
  CM<-createTM(predicteddT,actualdT)
  kl<-kappa(CM)
  details<-summary.kappa(kl)
  print(details)
  return(details)
}
makePalette <- function(colourvector) {
  cmat = cbind(t(col2rgb(colourvector)), 255)
  res = apply(cmat, 1, function(x) {
    sprintf("<Entry c1=\"%s\" c2=\"%s\" c3=\"%s\" c4=\"%s\"/>", x[1], x[2], 
            x[3], x[4])
  })
  res = paste(res, collapse = "\n")
  res
}

makePaletteVRT <- function(raster, colourvector) {
  s = sprintf("<VRTDataset rasterXSize=\"%s\" rasterYSize=\"%s\">\n<VRTRasterBand dataType=\"Byte\" band=\"1\">\n<ColorInterp>Palette</ColorInterp>\n<ColorTable>\n", 
              ncol(raster), nrow(raster))
  p = makePalette(colourvector)
  s = paste0(s, p, "\n</ColorTable>\n</VRTRasterBand>\n</VRTDataset>\n")
  s
}

writePaletteVRT <- function(out, raster, colourvector) {
  s = makePaletteVRT(raster, colourvector)
  cat(s, file = out)
}
randomizeFileName <- function(n = 8) {
  random.string <- rep(NA, n)
  randomizeString <- function(x) {
    a <-sample(letters, 1, replace = TRUE)
    return(a)
  }
  paste(c(sapply(random.string, randomizeString, simplify = TRUE)), collapse = "")
}

genrateMap<-function(plotFile,plotTitle,plotLegendTitle,classNumber,className,classColour,whereToPlot){
  library(png)
  library(grid)
  library(raster)
  r<-raster(plotFile)
  
  boundingBox<- as.matrix(extent(r)) #Get the extent of raster to create a bounding box
  xExtentRange=boundingBox[1,2]-boundingBox[1,1]   #Subtract the xmax-xmin
  yExtentRange=boundingBox[2,2]-boundingBox[2,1]   #Subtract the xmax-xmin
  breaks=sort(classNumber)
  z<-sort(classNumber,index.return=TRUE)   #sort
  className<-className[z[[2]]]  
  
  xcoor.scale = boundingBox[1,2] - (0.10 *xExtentRange )
  ycoor.scale =  boundingBox[2,1] + (0.05 *yExtentRange)
  
  xcoor.arrow=boundingBox[1,2] - (0.05 *xExtentRange )
  if (whereToPlot=="A4"){
    xcoor.arrow=boundingBox[1,2] - (0.10 *xExtentRange )
    ycoor.arrow=boundingBox[2,2] - (0.05 *yExtentRange)
  }else{
    xcoor.arrow=boundingBox[1,2] - (0.05 *xExtentRange )
    ycoor.arrow=boundingBox[2,2] - (0.15 *yExtentRange)
  }
  plot(r, col=classColour, legend=FALSE, axes=TRUE)
  plot(r, legend.only=TRUE, col=classColour,
       legend.width=1, legend.shrink=0.75,
       axis.args=list(at=breaks,
                      labels=className, 
                      cex.axis=0.6),
       legend.args=list(text=plotLegendTitle, side=4, font=2, line=2.5, cex=0.8))
  title(plotTitle)
  scaledistance=10000
  scalebar(scaledistance, xy=c(xcoor.scale-scaledistance/2,ycoor.scale ), type='bar', below = "km",divs=2,label=c('0','5','10'))
  scale <- list("SpatialPolygonsRescale", layout.scale.bar(),offset = c(xcoor.arrow, ycoor.arrow), scale = 0.4, fill = c("transparent","black"))
  SpatialPolygonsRescale(layout.north.arrow(),offset = c(xcoor.arrow, ycoor.arrow), scale = 5000,plot.grid=FALSE)
  
  copyright.draw("ILULC V1.0 ", .02, .04, .5, fontsize = 8)
}


copyright.draw <- function(label,  x, y, size, ...) {
  library(png)
  lab <- textGrob(label = label,
                  x = unit(x, "npc"), y = unit(y, "npc"),
                  just = c("left", "centre"), gp = gpar(...))
  
  grid.draw(lab)
  if(!file.access("./ilulc/icon.png",4)){
    image<-readPNG("./ilulc/icon.png")
    logo <- rasterGrob(image = image,
                       x = unit(x, "npc") + unit(1, "grobwidth", lab), y = unit(y, "npc"),
                       width = unit(size, "cm"), height = unit(size, "cm"),
                       just = c("left", "centre"), gp = gpar(...))
    grid.draw(logo)
  }
}

#
#type=getOption("pkgType")                           
CheckInstallPackage <- function(packages, repos="https://cran.r-project.org",
                                depend=c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"), ...) {
  installed=as.data.frame(installed.packages())
  for(p in packages) {
    if(is.na(charmatch(p, installed[,1]))) {
      print(paste("Installing: ",installed[,1]))
      #install.packages(p, repos=repos, dependencies=depend, ...) 
      install.packages(p, repos=repos, dependencies=NA, ...) 
    }
  }
} 

isDataSetCorrect<-function(T1drivers,T2drivers,T1File,T2File,T3File=NA){
  library(raster)
  if(is.na(T3File)){
    checkSums=colSums(!is.na(stack(c(T1Driver=T1drivers,T2Driver=T2drivers,T1File=T1File,T2File=T2File))[]),na.rm=TRUE)/sum(!is.na(raster(T2File)[]),na.rm=TRUE)
  }else{
    checkSums=colSums(!is.na(stack(c(T1Driver=T1drivers,T2Driver=T2drivers,T1File=T1File,T2File=T2File,T3File=T3File))[]),na.rm=TRUE)/sum(!is.na(raster(T2File)[]),na.rm=TRUE)
  }
  #totalsum=sum(checkSums)
  if(is.na(T3File) && sum(checkSums)==(2*length(T1drivers)+2)) 
  { 
    print("No Error in Data Sets") 
    return (TRUE)
  }
  
  if(!is.na(T3File) && sum(checkSums)==(2*length(T1drivers)+3)) 
  { 
    print("No Error in Data Sets") 
    return (TRUE)
  }
  
  for(i in 1:length(checkSums)){
    if(checkSums[i]!=1){
      if(checkSums[i] <1) 
        print(paste ( toupper(names(checkSums)[i]) , 'has "LESS" Number of NA Compare to T2LULC'))
      if(checkSums[i] >1) 
        print(paste (  toupper(names(checkSums)[i]) , 'has "MORE" Number of NA Compare to T2LULC'))    
      
      #if(i<=length(T1drivers) && checkSums[i] <1) 
      #    print(paste (names(checkSums)[i] , 'has "LESS" Number of NA Compare to T2LULC'))
      #if(i<=length(T1drivers) && checkSums[i] >1) 
      #    print(paste ( names(checkSums)[i] , 'has "MORE" Number of NA Compare to T2LULC'))    
      
      #if(i>length(T1drivers) && i<=(2*length(T1drivers)) && checkSums[i] <1)
      #    print(paste ( names(checkSums)[i] , 'has "LESS" Number of NA Compare to T2LULC'))
      #if(i>length(T1drivers) && i<=(2*length(T1drivers)) && checkSums[i] >1)
      #    print(paste ( names(checkSums)[i] , 'has "MORE" Number of NA Compare to T2LULC'))
      
      #if(i>(2*length(T1drivers)) && checkSums[i] >1) 
      #    print(paste (paste("T",(i-length(checkSums)),"LULC",sep=""), names(checkSums)[i] , 'has "MORE" Number of NA Compare to T2LULC'))
      #if(i>(2*length(T1drivers)) && checkSums[i] <1)
      #    print(paste (paste("T",(i-length(checkSums)),"LULC",sep=""), names(checkSums)[i] , 'has "LESS" Number of NA Compare to T2LULC'))
    }
    
  }
  
}
#######################################################################################################
# Gui supporting Functions
# progressConnection
# creatProgressBar<-function(min = 0, max = 1, initial = 0, char = "=",
# width = NA, title, label, style = 1, file = ""){
# progressConnection<-txtProgressBar(min = 0, max = 1, initial = 0, char = "=",
# width = NA, title, label, style = 1, file = "")

# getTxtProgressBar(pb)
# setTxtProgressBar(pb, value, title = NULL, label = NULL)
# ## S3 method for class 'txtProgressBar'
# close(con, ...)
# }

# R <- 1000
# progress_bar_text <- create_progress_bar("text")
# progress_bar_text$init(R)

# for (i in 1:R/2) {
# j <- i
# progress_bar_text$step()
# }

# progress_bar_tk <- create_progress_bar("tk",label="HI",title="This is test")
# progress_bar_tk$init(R)
# for (i in 1:R/4) {
# j <- se
# #Sys.sleep(1)
# progress_bar_tk$step()
# }
# print(j)


# createProgressBar()
# { 

# progress_bar_text <- create_progress_bar("text")
# progress_bar_text$init(R)
# for (i in 1:R) {
# j <- i
# progress_bar_text$step()
# }
# }


library(rattle)  
library(data.table)  
library(e1071)
library(randomForest)


getwd();
setwd("E://Kaggle//Mining-Accelerometer-Data")
getwd()

#train<- read.table(unz("train.csv.zip", "train.csv"),skip=0, nrows=1000000, header=T, quote="\"", sep=",")
#train<-fread("train.csv", sep="auto", sep2="auto", nrows=-1, header="auto", na.strings="NA", stringsAsFactors=FALSE)


#test<- read.table(unz("test.csv.zip", "test.csv"),skip=0, nrows=1000000, header=T, quote="\"", sep=",")
#test<-fread("test.csv", sep="auto", sep2="auto", nrows=-1, header="auto", na.strings="NA", stringsAsFactors=FALSE)


question<-fread("questions.csv", sep="auto", sep2="auto", nrows=-1, header="auto", na.strings="NA", stringsAsFactors=FALSE)



d=list()
lastDev=0
Dev=7
filename="train.csv"
nrows <- 1000000
con <- file(filename,open="r")    
train <- read.table(con, nrows=nrows,skip=0,header=T, sep=",")
firstFlage=1
repeat {
 
   if (firstFlage==1) { Devt=unique(train$Device); firstFlage=0;} 
   else 
	Devt=unique(train[,ncol(train)])
   Dev = c(Dev,Devt)
   for(i in Devt){
      if(lastDev==last(Devt)) 
	 d[[i]]=c(d[[i]],train[train[,ncol(train)]==i,])
      else
         d[[i]]=train[train[,ncol(train)]==i,]
   }
   lastDev=last(Devt)


    if (nrow(train) == 0)
        break
    ## process chunk 'data' here, then...
    ## ...read next chunk
    if (nrow(train) != nrows)   # last chunk was final chunk
        break
    train <- tryCatch({
        read.table(con, nrows=nrows, skip=0, header=F, sep=",")
    }, error=function(err) {
       ## matching condition message only works when message is not translated
       if (identical(conditionMessage(err), "no lines available in input"))
          data.frame()
       else stop(err)
    })


}
close(con)  

rm(train)

Dev=unique(Dev)  

save(d,file="Devicelist.RData")


#generate 1000 starting points 
id=list()
for(i in Dev){
  id[[i]]=sample(nrow(d[[i]])-300,1000,replace= TRUE)
}

#save(id,file="id.RData")

featurename=c("MeanX","MeanY","MeanZ","MeanXY","MeanYZ","MeanXZ","MeanXYZ"
          "VarX","VarY","VarZ","VarXY","VarYZ","VarXZ","VarXYZ"
          "MinX","MinY","MinZ","MinXY","MinYZ","MinXZ","MinXYZ"
          "quan25_X","quan25_Y","quan25_Z","quan25_XY","quan25_YZ","quan25_XZ","quan25_XYZ"
          "MedianX","MedianY","MedianZ","MedianXY","MedianYZ","MedianXZ","MedianXYZ"
          "quan75_X","quan75_Y","quan75_Z","quan75_XY","quan75_YZ","quan75_XZ","quan75_XYZ"
          "MaxX","MaxY","MaxZ","MaxXY","MaxYZ","MaxXZ","MaxXYZ"
          "RangeX","RangeY","RangeZ","RangeXY","RangeYZ","RangeXZ","RangeXYZ"
          "SkewnessX","SkewnessY","SkewnessZ","SkewnessXY","SkewnessYZ","SkewnessXZ","SkewnessXYZ"
          "KurtosisX","KurtosisY","KurtosisZ","KurtosisXY","KurtosisYZ","KurtosisXZ","KurtosisXYZ"
          "CorrXY","CorrXZ","CorrYZ",
          "CorrTX","CorrTY","CorrTZ",
          "Timerange"
          )


getrange<-function(value){
  max(value)-min(value)
}  

replaceNA<-function(value){
  value[is.na(value)]<-median(value,na.rm=T)
  value
}

sample=data.frame()


for(k in Dev){ 
   
feature=matrix(NA,nrow=1000,ncol=85)
  
for(i in 1:1000){
tmp=d[[k]][id[[k]][i]:(id[[k]][i]+299),]
tmp=cbind(tmp[,1:4],tmp[,2]^2+tmp[,3]^2,tmp[,3]^2+tmp[,4]^2,tmp[,2]^2+tmp[,4]^2,tmp[,2]^2+tmp[,3]^2+tmp[,4]^2,tmp[,5])

colnames(tmp)=c("T","X","Y","Z","XY","YZ","XZ","XYZ","Device");
numoffeature=0
feature[i,1:7]=as.numeric(colMeans(tmp[,2:8]))        #mean
numoffeature=numoffeature+7;
feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,var))     #Variance
numoffeature=numoffeature+7;
feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,min))
numoffeature=numoffeature+7;
feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,function(x) quantile(x,prob=0.25)))
numoffeature=numoffeature+7;
feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,median))
numoffeature=numoffeature+7;
feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,function(x) quantile(x,prob=0.75)))
numoffeature=numoffeature+7;
feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,max))
numoffeature=numoffeature+7;
feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,getrange))
numoffeature=numoffeature+7;
feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,skewness))
numoffeature=numoffeature+7;
feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,kurtosis))
numoffeature=numoffeature+8;

feature[i,numoffeature]=cor(tmp$X,tmp$Y); numoffeature=numoffeature+1;
feature[i,numoffeature]=cor(tmp$X,tmp$Z); numoffeature=numoffeature+1;
feature[i,numoffeature]=cor(tmp$Y,tmp$Z); numoffeature=numoffeature+1;
feature[i,numoffeature]=cor(tmp$T,tmp$X); numoffeature=numoffeature+1;
feature[i,numoffeature]=cor(tmp$T,tmp$Y); numoffeature=numoffeature+1;
feature[i,numoffeature]=cor(tmp$T,tmp$Z); numoffeature=numoffeature+1;

feature[i,numoffeature]=tmp$T[300]-tmp$T[1]

}

extract=data.frame(feature)

extract$Device=k
  
sample=rbind(sample,extract)

}



#deal with missing in unscaled sample
nomissing=apply(sample[,1:numoffeature-1],2,replaceNA)
nomissing=data.frame(cbind(nomissing,sample$Device))
sample=nomissing
names(sample)[numoffeature]="Device"

#save(sample,file="unscaled sample without NA.RData")


##prepare the factor variable for data mining analysis
for(k in Dev){
  t1=sample[sample$Device==k,]
  t2=sample[!sample$Device==k,]
  ID=sample(nrow(t2),1000)
  t3=sample[ID,]
  t3$Device=-k
  train=rbind(t1,t3)
  train$Device=as.factor(train$Device)
  
 # save RData in folder "traindata" 
  save(train,file=paste("traindata/train",k,".RData",sep=""))
}


################################################################
# get ready testing set
##################################################################
Seq=unique(test$SequenceId)

extract=matrix(NA,nrow=length(Seq),ncol=71)

for(i in 1:3000){ 
  tmp=test[((i-1)*300+1):(i*300),]
  
  tmp=cbind(tmp[,1:4],tmp[,2]^2+tmp[,3]^2,tmp[,3]^2+tmp[,4]^2,tmp[,2]^2+tmp[,4]^2,tmp[,2]^2+tmp[,3]^2+tmp[,4]^2,tmp[,5])

  colnames(tmp)=c("T","X","Y","Z","XY","YZ","XZ","XYZ","Device");
  numoffeature=0
  feature[i,1:7]=as.numeric(colMeans(tmp[,2:8]))        #mean
  numoffeature=numoffeature+7;
  feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,var))     #Variance
  numoffeature=numoffeature+7;
  feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,min))
  numoffeature=numoffeature+7;
  feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,firstquantile))
  numoffeature=numoffeature+7;
  feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,median))
  numoffeature=numoffeature+7;
  feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,thirdquantile))
  numoffeature=numoffeature+7;
  feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,max))
  numoffeature=numoffeature+7;
  feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,getrange))
  numoffeature=numoffeature+7;
  feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,skewness))
  numoffeature=numoffeature+7;
  feature[i,numoffeature+1:numoffeature+7]=as.numeric(apply(tmp[,2:8],2,kurtosis))
  numoffeature=numoffeature+8;
  feature[i,numoffeature+1]=cor(tmp$X,tmp$Y)
  numoffeature=numoffeature+1;
  feature[i,numoffeature]=cor(tmp$X,tmp$Z)
  numoffeature=numoffeature+1;
  feature[i,numoffeature]=cor(tmp$Y,tmp$Z)
  numoffeature=numoffeature+1;
  feature[i,numoffeature]=cor(tmp$T,tmp$X); numoffeature=numoffeature+1;
  feature[i,numoffeature]=cor(tmp$T,tmp$Y); numoffeature=numoffeature+1;
  feature[i,numoffeature]=cor(tmp$T,tmp$Z); numoffeature=numoffeature+1;

  feature[i,numoffeature]=tmp$T[300]-tmp$T[1]
}

te=data.frame(feature)

names(te)[numoffeature]="SequenceId"

# Merge question with transformed test
merge_test_question=merge(te,question,by.x="SequenceId",by.y="SequenceId")
te=merge_test_question[,c(2:71,1,72,73)]


#save(te,file="unscaled transformtest.RData")


#############################################################
# Train with random Forests
#############################################################
rf=list()
for(k in Dev){

load(paste("traindata/train",k,".RData",sep=""))
  
rf[[k]]<-randomForest(as.factor(Device)~.,data=train,importance=T,mtry=3,ntree=100,na.action=na.omit)
}

#save(rf,file="rf.RData")

######################################
# Predict with  Random Forests
#####################################
rf.result=rep(NA,3000)#90024

for(s in 1:3000){#90024
k=te$Device[s]
pred=as.numeric(as.character(predict(rf[[k]],te)))
rf.result[s]=ifelse(pred>0,1,0)
}

rfsubmit=cbind(question$QuestionId, rf.result)
rfsubmit=data.frame(rfsubmit)
names(rfsubmit)=c("QuestionId","IsTrue")
write.csv(rfsubmit,"sampleSubmission.csv")


